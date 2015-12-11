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
            snapshot-variables
            set-snapshot-variables!
            set-snapshot-code!
            snapshot-ip

            snapshot-link?
            snapshot-set-loop-info?
            snapshot-downrec?
            snapshot-uprec?
            *ip-key-link*
            *ip-key-set-loop-info!*
            *ip-key-downrec*
            *ip-key-uprec*

            $past-frame
            make-past-frame
            arrange-past-frame
            past-frame-locals
            past-frame-local-indices
            set-past-frame-local-indices!
            past-frame-local-ref
            past-frame-type-ref
            past-frame-sp-offsets
            set-past-frame-sp-offsets!
            past-frame-fp-offsets
            set-past-frame-fp-offsets!
            past-frame-types
            set-past-frame-types!
            past-frame-dls
            past-frame-ras
            past-frame-sp-offset
            set-past-frame-sp-offset!
            past-frame-fp-offset
            set-past-frame-fp-offset!
            pop-past-frame!
            push-past-frame!
            expand-past-frame
            merge-past-frame-types!

            $return-address
            make-return-address
            return-address?
            return-address-ip

            $dynamic-link
            make-dynamic-link
            dynamic-link?
            dynamic-link-offset

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
            &undefined

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
               &fluid
               &vector
               &box
               &struct
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

;; Data type to contain past frame data.
;;
;; Stores dynamic link, return addresses, and locals of caller procedure when
;; inlined procedure exist in trace.
(define-record-type $past-frame
  (%make-past-frame dls ras locals local-indices sp-offsets fp-offsets types
                    sp-offset fp-offset)
  past-frame?

  ;; Association list for dynamic link: (local . pointer to fp).
  (dls past-frame-dls set-past-frame-dls!)

  ;; Association list for return address: (local . pointer to ra).
  (ras past-frame-ras set-past-frame-ras!)

  ;; Vector containing locals.
  (locals past-frame-locals)

  ;; All local indices found in trace.
  (local-indices past-frame-local-indices set-past-frame-local-indices!)

  ;; Vector containing SP offset per bytecode operation.
  (sp-offsets past-frame-sp-offsets set-past-frame-sp-offsets!)

  ;; Vector containing FP offset per bytecode operation.
  (fp-offsets past-frame-fp-offsets set-past-frame-fp-offsets!)

  ;; Stack element types.
  (types past-frame-types set-past-frame-types!)

  ;; Current SP offset.
  (sp-offset past-frame-sp-offset set-past-frame-sp-offset!)

  ;; Current FP offset.
  (fp-offset past-frame-fp-offset set-past-frame-fp-offset!))

(define (make-past-frame types sp-offset fp-offset)
  ;; Using hash-table to contain locals, since local index could take negative
  ;; value.
  (%make-past-frame '() '() (make-hash-table) '() '() '() types
                    sp-offset fp-offset))

(define (arrange-past-frame pf)
  (let ((locals/list (sort (map car (past-frame-local-indices pf)) >))
        (sp-offsets/vec (list->vector (reverse! (past-frame-sp-offsets pf))))
        (fp-offsets/vec (list->vector (reverse! (past-frame-fp-offsets pf)))))
    (set-past-frame-local-indices! pf locals/list)
    (set-past-frame-sp-offsets! pf sp-offsets/vec)
    (set-past-frame-fp-offsets! pf fp-offsets/vec)
    pf))

(define (push-past-frame! past-frame dl ra sp-offset locals)
  (set-past-frame-dls! past-frame (cons dl (past-frame-dls past-frame)))
  (set-past-frame-ras! past-frame (cons ra (past-frame-ras past-frame)))
  (let lp ((i 0)
           (end (vector-length locals))
           (to-update (past-frame-locals past-frame)))
    (when (< i end)
      (hashq-set! to-update (+ i sp-offset) (vector-ref locals i))
      (lp (+ i 1) end to-update)))
  past-frame)

(define (pop-past-frame! past-frame sp-offset locals)
  (let ((old-dls (past-frame-dls past-frame))
        (old-ras (past-frame-ras past-frame)))
    (when (not (null? old-dls))
      (set-past-frame-dls! past-frame (cdr old-dls)))
    (when (not (null? old-ras))
      (set-past-frame-ras! past-frame (cdr old-ras)))
    (let lp ((i 0)
             (end (vector-length locals))
             (t (past-frame-locals past-frame)))
      (when (< i end)
        (hashq-set! t (+ i sp-offset) (vector-ref locals i))
        (lp (+ i 1) end t)))
    past-frame))

(define (expand-past-frame past-frame offset nlocals)
  (let ((locals (past-frame-locals past-frame))
        (undefined (make-pointer #x904)))
    (let lp ((n 0))
      (when (< n nlocals)
        (hashq-set! locals (+ offset n) undefined)
        (lp (+ n 1))))))

(define (past-frame-local-ref past-frame i)
  (hashq-ref (past-frame-locals past-frame) i))

(define (past-frame-type-ref past-frame i)
  (assq-ref (past-frame-types past-frame) i))

(define (merge-past-frame-types! past-frame local-x-types)
  (let lp ((locals local-x-types)
           (types (past-frame-types past-frame)))
    (match locals
      (((local . type) . locals)
       (let* ((etype (type->stack-element-type type))
              (types (assq-set! types local etype)))
         (lp locals types)))
      (_
       (set-past-frame-types! past-frame types)))))



;; Record type for snapshot.
(define-record-type $snapshot
  (%make-snapshot id sp-offset fp-offset nlocals locals variables code ip)
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
  (locals snapshot-locals)

  ;; Variables used at the time of taking exit.
  (variables snapshot-variables set-snapshot-variables!)

  ;; Native code of bailout with this snapshot.
  (code snapshot-code set-snapshot-code!)

  ;; Bytecode IP of this snapshot to return.
  (ip snapshot-ip))


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

;; Extra type for #<undefined> values.
(define &undefined
  ;; XXX: Any better number to use ...?
  0)

(define (type-of obj)
  (cond
   ((fixnum? obj) &exact-integer)
   ((flonum? obj) &flonum)
   ((char? obj) &char)
   ((unspecified? obj) &unspecified)
   ((undefined? obj) &undefined)
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
   (else
    (tjitc-error 'type-of "~s" obj))))

(define (pretty-type type)
  "Show string representation of TYPE."
  (cond
   ((eq? type &exact-integer) (blue "snum"))
   ((eq? type &flonum) (magenta "fnum"))
   ((eq? type &char) (blue "char"))
   ((eq? type &unspecified) (green "uspc"))
   ((eq? type &unbound) (green "ubnd"))
   ((eq? type &undefined) (green "udef"))
   ((eq? type &false) (green "fals"))
   ((eq? type &true) (green "true"))
   ((eq? type &nil) (green "nil"))
   ((eq? type &null) (green "null"))
   ((eq? type &symbol) (blue "symb"))
   ((eq? type &keyword) (blue "keyw"))
   ((eq? type &procedure) (yellow "proc"))
   ((eq? type &pair) (yellow "pair"))
   ((eq? type &vector) (yellow "vect"))
   ((eq? type &box) (yellow "box"))
   ((eq? type &struct) (yellow "strc"))
   ((eq? type &f64) (magenta "f64"))
   ((eq? type &u64) (blue "u64"))
   ((eq? type &s64) (blue "s64"))
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
                       parent-snapshot indices past-frame ip)
  (define initial-offset
    (or (and=> parent-snapshot snapshot-sp-offset)))
  (define parent-locals
    (and=> parent-snapshot snapshot-locals))
  (define (parent-snapshot-local-ref i)
    (and parent-snapshot (assq-ref parent-locals i)))
  (define-syntax-rule (local-ref i)
    (let ((type (past-frame-type-ref past-frame i)))
      (cond
       ((eq? 'f64 type) &f64)
       ((eq? 'u64 type) &u64)
       ((eq? 's64 type) &s64)
       ((eq? 'scm type)
        (type-of (stack-element locals (- i sp-offset) type)))
       (else
        (tjitc-error 'make-snapshot "unknown local ~s ~s" type i)))))

  (let lp ((is indices) (acc '()))
    (match is
      ((i . is)
       (define (dl-or-ra i)
         (or (assq-ref (past-frame-dls past-frame) i)
             (assq-ref (past-frame-ras past-frame) i)
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
        ;; address.
        ((<= sp-offset i (- (+ sp-offset nlocals) 1))
         (add-local (local-ref i)))

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
        ;; upper frame. Looking up locals from newest locals in past-frame.
        ;;
        ;; Side trace could start from the middle of inlined procedure, locals
        ;; in past frame may not have enough information to recover locals in
        ;; caller of the inlined procedure. In such case, look up locals in the
        ;; snapshot of parent trace.
        ((past-frame-type-ref past-frame i)
         => (lambda (se-type)
              (cond
               ((eq? 'f64 se-type) (add-local &f64))
               ((eq? 'u64 se-type) (add-local &u64))
               ((eq? 's64 se-type) (add-local &s64))
               ((eq? 'scm se-type)
                (cond
                 ((past-frame-local-ref past-frame i)
                  => (lambda (ptr)
                       (add-local (type-of (resolve-stack-element ptr)))))
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
       (%make-snapshot id sp-offset fp-offset (vector-length locals)
                       (reverse! acc) #f #f ip)))))


;;;
;;; IP Keys
;;;

(define *ip-key-link* 0)
(define *ip-key-set-loop-info!* 1)
(define *ip-key-downrec* 2)
(define *ip-key-uprec* 3)

(define (snapshot-link? snapshot)
  (= (snapshot-ip snapshot) *ip-key-link*))

(define (snapshot-set-loop-info? snapshot)
  (= (snapshot-ip snapshot) *ip-key-set-loop-info!*))

(define (snapshot-downrec? snapshot)
  (= (snapshot-ip snapshot) *ip-key-downrec*))

(define (snapshot-uprec? snapshot)
  (= (snapshot-ip snapshot) *ip-key-uprec*))
