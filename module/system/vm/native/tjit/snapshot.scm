;;;; Snapshot and other data to restore frame locals

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA
;;;;

;;; Commentary:
;;;
;;; This module contains snapshot related codes and record type used when
;;; recovering frame locals. Snapshot data are used when restoring locals in
;;; frame, so that the VM interpreter can continue from where the native code
;;; has returned.
;;;
;;; Code:

(define-module (system vm native tjit snapshot)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (language cps types)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:export ($snapshot
            make-snapshot
            %make-snapshot
            snapshot?
            snapshot-id
            snapshot-sp-offset
            snapshot-fp-offset
            snapshot-nlocals
            snapshot-locals
            set-snapshot-locals!
            snapshot-variables
            set-snapshot-variables!
            set-snapshot-code!
            snapshot-ip
            snapshot-outline-types
            snapshot-read-indices
            set-snapshot-read-indices!

            snapshot-link?
            snapshot-downrec?
            snapshot-uprec?
            *ip-key-link*
            *ip-key-downrec*
            *ip-key-uprec*

            $outline
            make-outline
            outline-locals
            outline-local-indices
            set-outline-local-indices!
            outline-local-ref
            outline-type-ref
            outline-sp-offsets
            set-outline-sp-offsets!
            outline-fp-offsets
            set-outline-fp-offsets!
            outline-types
            set-outline-types!
            outline-dls
            outline-ras
            outline-sp-offset
            set-outline-sp-offset!
            outline-fp-offset
            set-outline-fp-offset!
            outline-ret-types
            set-outline-ret-types!
            outline-write-indices
            set-outline-write-indices!
            outline-read-indices
            set-outline-read-indices!

            pop-outline!
            push-outline!
            arrange-outline
            expand-outline
            set-outline-previous-dl-and-ra!
            merge-outline-types!

            $return-address
            make-return-address
            return-address?
            return-address-ip

            $dynamic-link
            make-dynamic-link
            dynamic-link?
            dynamic-link-offset

            &undefined
            &port

            fixnum?
            flonum?
            unbound?
            true?
            false?
            undefined?
            eof?
            unbound?
            type-of
            pretty-type
            *unbound*

            %tc2-int
            %tc3-imm24
            %tc3-cons
            %tc8-char
            %tc3-struct
            %tc7-symbol
            %tc7-variable
            %tc7-vector
            %tc7-wvect
            %tc7-string
            %tc7-number
            %tc7-hashtable
            %tc7-pointer
            %tc7-fluid
            %tc7-stringbuf
            %tc7-keyword
            %tc7-program
            %tc7-bytevector
            %tc7-array
            %tc7-bitvector
            %tc7-port
            %tc16-real

            stack-element
            type->stack-element-type)
  #:re-export (&exact-integer
               &flonum
               &complex
               &fraction
               &char
               &unspecified
               &unbound
               &false
               &true
               &nil
               &null
               &symbol
               &keyword
               &procedure
               &pointer
               &pair
               &fluid
               &vector
               &box
               &struct
               &string
               &bytevector
               &bitvector
               &array
               &hash-table
               &f64
               &u64
               &s64))


;;;
;;; Record types
;;;

;; Record type to represent return address in frame.
(define-record-type $return-address
  (make-return-address ip)
  return-address?
  (ip return-address-ip))

;; Record type to represent dynamic link in frame.
(define-record-type $dynamic-link
  (make-dynamic-link offset)
  dynamic-link?
  (offset dynamic-link-offset))

;; Data type to contain outline of recorded traces.
;;
;; Stores various information used for compiling IR, such as dynamic links and
;; return addresses, past frame locals, locals of caller procedure in inlined
;; procedure ... etc.
(define-record-type $outline
  (%make-outline dls ras locals local-indices sp-offsets fp-offsets types
                 sp-offset fp-offset ret-types write-indices read-indices)
  outline?

  ;; Association list for dynamic link: (local . pointer to fp).
  (dls outline-dls set-outline-dls!)

  ;; Association list for return address: (local . pointer to ra).
  (ras outline-ras set-outline-ras!)

  ;; Vector containing locals.
  (locals outline-locals)

  ;; All local indices found in trace.
  (local-indices outline-local-indices set-outline-local-indices!)

  ;; Vector containing SP offset per bytecode operation.
  (sp-offsets outline-sp-offsets set-outline-sp-offsets!)

  ;; Vector containing FP offset per bytecode operation.
  (fp-offsets outline-fp-offsets set-outline-fp-offsets!)

  ;; Stack element types.
  (types outline-types set-outline-types!)

  ;; Current SP offset.
  (sp-offset outline-sp-offset set-outline-sp-offset!)

  ;; Current FP offset.
  (fp-offset outline-fp-offset set-outline-fp-offset!)

  ;; Returned types from C functions.
  (ret-types outline-ret-types set-outline-ret-types!)

  ;; Local indices for write.
  (write-indices outline-write-indices set-outline-write-indices!)

  ;; Local indices for read.
  (read-indices outline-read-indices set-outline-read-indices!))

(define (make-outline types sp-offset fp-offset write-indices)
  ;; Using hash-table to contain locals, since local index could take negative
  ;; value.
  (%make-outline '() '() (make-hash-table) '() '() '() types
                 sp-offset fp-offset
                 '() write-indices '()))

(define (arrange-outline outline)
  (let ((locals/list (sort (map car (outline-local-indices outline)) >))
        (sp-offsets/vec (list->vector (reverse! (outline-sp-offsets outline))))
        (fp-offsets/vec (list->vector (reverse! (outline-fp-offsets outline))))
        (ret-types/vec (list->vector (reverse! (outline-ret-types outline))))
        (writes/list (sort (outline-write-indices outline) <)))
    (set-outline-local-indices! outline locals/list)
    (set-outline-sp-offsets! outline sp-offsets/vec)
    (set-outline-fp-offsets! outline fp-offsets/vec)
    (set-outline-ret-types! outline ret-types/vec)
    (set-outline-write-indices! outline writes/list)
    outline))

(define (push-outline! outline dl ra sp-offset locals)
  (set-outline-dls! outline (cons dl (outline-dls outline)))
  (set-outline-ras! outline (cons ra (outline-ras outline)))
  (let lp ((i 0)
           (end (vector-length locals))
           (to-update (outline-locals outline)))
    (when (< i end)
      (hashq-set! to-update (+ i sp-offset) (vector-ref locals i))
      (lp (+ i 1) end to-update)))
  outline)

(define (pop-outline! outline sp-offset locals)
  (let ((old-dls (outline-dls outline))
        (old-ras (outline-ras outline)))
    (when (not (null? old-dls))
      (set-outline-dls! outline (cdr old-dls)))
    (when (not (null? old-ras))
      (set-outline-ras! outline (cdr old-ras)))
    (let lp ((i 0)
             (end (vector-length locals))
             (t (outline-locals outline)))
      (when (< i end)
        (hashq-set! t (+ i sp-offset) (vector-ref locals i))
        (lp (+ i 1) end t)))
    outline))

(define (expand-outline outline offset nlocals)
  (let ((locals (outline-locals outline))
        (undefined (make-pointer #x904)))
    (let lp ((n 0))
      (when (< n nlocals)
        (hashq-set! locals (+ offset n) undefined)
        (lp (+ n 1))))))

(define (set-outline-previous-dl-and-ra! outline stack-size ra dl)
  (let ((locals (outline-locals outline)))
    (hashq-set! locals (- stack-size 1) ra)
    (hashq-set! locals stack-size dl)))

(define (outline-local-ref outline i)
  (hashq-ref (outline-locals outline) i))

(define (outline-type-ref outline i)
  (assq-ref (outline-types outline) i))

(define (merge-outline-types! outline local-x-types)
  (let lp ((locals local-x-types)
           (types (outline-types outline)))
    (match locals
      (((local . type) . locals)
       (let* ((etype (if (symbol? type)
                         type
                         (type->stack-element-type type)))
              (types (assq-set! types local etype)))
         (lp locals types)))
      (_
       (set-outline-types! outline types)))))

;; Record type for snapshot.
(define-record-type $snapshot
  (%make-snapshot id sp-offset fp-offset nlocals locals variables code ip
                  outline-types read-indices)
  snapshot?

  ;; ID number of this snapshot.
  (id snapshot-id)

  ;; Integer number to shift SP after returning with this snapshot.
  (sp-offset snapshot-sp-offset)

  ;; Integer number to shift vp->fp after returning with this snapshot.
  (fp-offset snapshot-fp-offset)

  ;; Number of locals at the time of snapshot.
  (nlocals snapshot-nlocals)

  ;; Association list of (local . type).
  (locals snapshot-locals set-snapshot-locals!)

  ;; Variables used at the time of taking exit.
  (variables snapshot-variables set-snapshot-variables!)

  ;; Native code of bailout with this snapshot.
  (code snapshot-code set-snapshot-code!)

  ;; Bytecode IP of this snapshot to return.
  (ip snapshot-ip)

  ;; Types from outline.
  (outline-types snapshot-outline-types)

  ;; Read indices
  (read-indices snapshot-read-indices set-snapshot-read-indices!))

;;;
;;; Tags
;;;

(define-syntax define-tcx
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define name
         (@@ (system base types) name))
       ...))))

(define-tcx
  %tc2-int
  %tc3-imm24
  %tc3-cons
  %tc8-char
  %tc3-struct
  %tc7-symbol
  %tc7-variable
  %tc7-vector
  %tc7-wvect
  %tc7-string
  %tc7-number
  %tc7-hashtable
  %tc7-pointer
  %tc7-fluid
  %tc7-stringbuf
  %tc7-keyword
  %tc7-program
  %tc7-bytevector
  %tc7-array
  %tc7-bitvector
  %tc7-port
  %tc16-real)


;;;
;;; Extra types
;;;

;; XXX: Any better number to use ...?
(define &undefined 0)

(define &port %tc7-port)


;;;
;;; Type checker based on runtime values
;;;

(define (fixnum? val)
  (and (exact-integer? val)
       (<= most-negative-fixnum val most-positive-fixnum)))

(define (flonum? val)
  (and (real? val) (inexact? val)))

(define (undefined? x)
  (= (pointer-address (scm->pointer x)) #x904))

(define (eof? x)
  (= (pointer-address (scm->pointer x)) #xa04))

(define (unbound? x)
  (= (pointer-address (scm->pointer x)) #xb04))

(define (false? x)
  (not x))

(define (true? x)
  (eq? x #t))

(define *unbound*
  (pointer->scm (make-pointer #xb04)))

(define (type-of obj)
  (cond
   ;; From (@ language cps types)
   ((fixnum? obj) &exact-integer)
   ((flonum? obj) &flonum)
   ((char? obj) &char)
   ((unspecified? obj) &unspecified)
   ((false? obj) &false)
   ((true? obj) &true)
   ((null? obj) &null)
   ((symbol? obj) &symbol)
   ((keyword? obj) &keyword)
   ((procedure? obj) &procedure)
   ((pointer? obj) &pointer)
   ((fluid? obj) &fluid)
   ((pair? obj) &pair)
   ((vector? obj) &vector)
   ((variable? obj) &box)
   ((struct? obj) &struct)
   ((string? obj) &string)
   ((bytevector? obj) &bytevector)
   ((bitvector? obj) &bitvector)
   ((array? obj) &array)
   ((hash-table? obj) &hash-table)
   ;; Not from (@ language cps types)
   ((undefined? obj) &undefined)
   ((port? obj) &port)
   (else
    (tjitc-error 'type-of "~s" obj))))

(define (pretty-type type)
  "Show string representation of TYPE."
  (cond
   ;; From (@ language cps types)
   ((eq? type &exact-integer) (blue "snum"))
   ((eq? type &flonum) (magenta "fnum"))
   ((eq? type &char) (blue "char"))
   ((eq? type &unspecified) (green "uspc"))
   ((eq? type &unbound) (green "ubnd"))
   ((eq? type &false) (green "fals"))
   ((eq? type &true) (green "true"))
   ((eq? type &nil) (green "nil"))
   ((eq? type &null) (green "null"))
   ((eq? type &symbol) (blue "symb"))
   ((eq? type &keyword) (blue "keyw"))
   ((eq? type &procedure) (red "proc"))
   ((eq? type &pointer) (yellow "ptr"))
   ((eq? type &pair) (yellow "pair"))
   ((eq? type &vector) (yellow "vect"))
   ((eq? type &box) (yellow "box"))
   ((eq? type &struct) (yellow "strc"))
   ((eq? type &string) (yellow "strn"))
   ((eq? type &bytevector) (yellow "bytv"))
   ((eq? type &bitvector) (yellow "bitv"))
   ((eq? type &array) (yellow "arry"))
   ((eq? type &hash-table) (yellow "htbl"))
   ((eq? type &f64) (magenta "f64"))
   ((eq? type &u64) (blue "u64"))
   ((eq? type &s64) (blue "s64"))
   ;; Not from (@ language cps types)
   ((eq? type &undefined) (green "udef"))
   ((eq? type &port) (yellow "port"))
   ((dynamic-link? type)
    (let ((diff (number->string (dynamic-link-offset type))))
      (string-append "dl:" (cyan diff))))
   ((return-address? type)
    (let* ((addr (pointer-address (return-address-ip type)))
           (hex-ip (number->string addr 16)))
      (string-append "ra:" (cyan hex-ip))))
   (else type)))


;;;
;;; Stack element
;;;

(define (stack-element locals n type)
  (let ((elem (vector-ref locals n)))
    (debug 1 ";;; stack-element ~s ~s => ~s~%" n type elem)
    (cond
     ((eq? 'u64 type)
      (pointer-address elem))
     ((eq? 's64 type)
      (tjitc-error 'stack-element "got s64"))
     ((eq? 'f64 type)
      (tjitc-error 'stack-element "got f64"))
     ((eq? 'scm type)
      (resolve-stack-element elem))
     (else
      (tjitc-error 'stack-element "~s ~s ~s" type n elem)))))

(define (resolve-stack-element ptr)
  (let ((scm? (and (pointer? ptr)
                   (let ((addr (pointer-address ptr)))
                     (and (zero? (logand 1 addr))
                          ;; XXX: Workaround to avoid segmentation fault.
                          (not (and (zero? (modulo addr 8))
                                    (<= addr 800))))))))
    (if scm?
        (pointer->scm ptr)
        (tjitc-error 'resolve-stack-element "non-SCM ~s" ptr))))

(define (type->stack-element-type type)
  (cond
   ((eq? type &f64) 'f64)
   ((eq? type &u64) 'u64)
   ((eq? type &s64) 's64)
   (else 'scm)))


;;;
;;; Snapshot
;;;

(define (make-snapshot id sp-offset fp-offset nlocals locals
                       parent-snapshot write-indices outline ip)
  (define initial-offset
    (or (and=> parent-snapshot snapshot-sp-offset)))
  (define parent-locals
    (and=> parent-snapshot snapshot-locals))
  (define (parent-snapshot-local-ref i)
    (and parent-snapshot (assq-ref parent-locals i)))
  (define-syntax-rule (local-ref i)
    (let ((type (outline-type-ref outline i)))
      (debug 1 ";;; [ms] local-ref ~s => " i)
      (cond
       ((eq? 'f64 type) (debug 1 "~s~%" 'f64) &f64)
       ((eq? 'u64 type) (debug 1 "~s~%" 'u64) &u64)
       ((eq? 's64 type) (debug 1 "~s~%" 's64) &s64)
       ((eq? 'scm type)
        (debug 1 "~s~%" 'scm)
        (type-of (stack-element locals (- i sp-offset) type)))
       (else
        (tjitc-error 'make-snapshot "unknown local ~s ~s" type i)))))
  (let ((car-< (lambda (a b) (< (car a) (car b)))))
    (debug 1 ";;; [ms] id:~s sp:~s fp:~s nlocals:~s~%"
           id sp-offset fp-offset nlocals)
    (debug 1 ";;;      write-indices:~s~%" write-indices)
    (debug 1 ";;;      locals:~a~%"
           (let lp ((i (- (vector-length locals) 1)) (acc '()))
             (if (< i 0)
                 acc
                 (lp (- i 1) (cons (format #f "0x~x"
                                           (pointer-address (vector-ref locals i)))
                                   acc))))))
  (let lp ((is write-indices) (acc '()))
    (match is
      ((i . is)
       (define (dl-or-ra i)
         (or (assq-ref (outline-dls outline) i)
             (assq-ref (outline-ras outline) i)
             (let ((val (parent-snapshot-local-ref i)))
               (and (or (dynamic-link? val)
                        (return-address? val))
                    val))))
       (define (add-local type)
         (lp is (cons `(,i . ,type) acc)))
       (define (add-val val)
         (lp is (cons `(,i . ,val) acc)))
       (cond
        ;; Inlined local in initial frame in root trace. The frame contents
        ;; should be a scheme stack element, not a dynamic link or a return
        ;; address. Only excpetion is return address and dynamic link for last
        ;; snapshot in side trace, which is specified by dedicated IP key.
        ((<= sp-offset i (- (+ sp-offset nlocals) 1))
         (cond
          ((and (= ip *ip-key-link*) (dl-or-ra i))
           => add-val)
          (else
           (add-local (local-ref i)))))

        ;; Dynamic link and return address might need to be passed from parent
        ;; trace. When side trace of inlined procedure takes bailout code,
        ;; recorded trace might not contain bytecode operation to fill in the
        ;; dynamic link and return address of past frame.
        ((dl-or-ra i)
         => add-val)

        ;; Down frame. Taking local from a vector saved at the tme of recording
        ;; the trace.
        ((<= 0 (- i sp-offset) (- (vector-length locals) 1))
         (add-local (local-ref i)))

        ;; When side trace contains inlined procedure and the guard taking this
        ;; snapshot is from the caller of the inlined procedure, saving local in
        ;; upper frame. Looking up locals from newest locals in outline.
        ;;
        ;; Side trace could start from the middle of inlined procedure, locals
        ;; in past frame may not have enough information to recover locals in
        ;; caller of the inlined procedure. In such case, look up locals in the
        ;; snapshot of parent trace.
        ((outline-type-ref outline i)
         => (lambda (se-type)
              (cond
               ((eq? 'f64 se-type) (add-local &f64))
               ((eq? 'u64 se-type) (add-local &u64))
               ((eq? 's64 se-type) (add-local &s64))
               ((eq? 'scm se-type)
                (cond
                 ((outline-local-ref outline i)
                  => (lambda (e)
                       (debug 1 ";;; [ms] outline-local-ref ~s => ~s~%" i e)
                       (cond
                        ((or (dynamic-link? e) (return-address? e))
                         (add-val e))
                        (else
                         (add-local (type-of (resolve-stack-element e)))))))
                 ((parent-snapshot-local-ref i)
                  => add-local)
                 (else
                  (tjitc-error 'make-snapshot "unresolved scm ~s ~s" id i))))
               (else
                (tjitc-error 'make-snapshot "unknown type ~s ~s" id se-type)))))

        ;; Giving up, skip this local.
        (else
         (debug 3 "XXX: local for i=~a not found~%" i)
         (add-local #f))))
      (()
       (let ((copied-types (copy-tree (outline-types outline))))
         (debug 1 ";;; [ms] copied-types=~s~%"
                (sort copied-types (lambda (a b)
                                     (< (car a) (car b)))))
         (%make-snapshot id sp-offset fp-offset (vector-length locals)
                         (reverse! acc) #f #f ip copied-types
                         (outline-read-indices outline)))))))


;;;
;;; IP Keys
;;;

(define *ip-key-link* 0)
(define *ip-key-downrec* 1)
(define *ip-key-uprec* 2)

(define (snapshot-link? snapshot)
  (= (snapshot-ip snapshot) *ip-key-link*))

(define (snapshot-downrec? snapshot)
  (= (snapshot-ip snapshot) *ip-key-downrec*))

(define (snapshot-uprec? snapshot)
  (= (snapshot-ip snapshot) *ip-key-uprec*))
