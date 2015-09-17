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
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit parameters)
  #:export (type-of
            fixnum?
            flonum?
            unbound?
            true?
            false?

            $snapshot
            make-snapshot
            %make-snapshot
            snapshot?
            snapshot-offset
            snapshot-nlocals
            snapshot-locals
            snapshot-variables
            set-snapshot-variables!
            set-snapshot-code!

            $past-frame
            make-past-frame
            past-frame-local-indices
            past-frame-local-ref
            past-frame-lower-ref
            pop-past-frame!
            push-past-frame!

            $return-address
            make-return-address
            return-address?
            return-address-ip

            $dynamic-link
            make-dynamic-link
            dynamic-link?
            dynamic-link-offset

            accumulate-locals))


;;;
;;; Type checker based on runtime values
;;;

(define (fixnum? val)
  (and (exact-integer? val)
       (<= most-negative-fixnum val most-positive-fixnum)))

(define (flonum? val)
  (and (real? val) (inexact? val)))

(define (unbound? x)
  (= (pointer-address (scm->pointer x)) #x904))

(define (false? x)
  (eq? x #f))

(define (true? x)
  (eq? x #t))

(define (type-of obj)
  (cond
   ((fixnum? obj) &exact-integer)
   ((flonum? obj) &flonum)
   ((char? obj) &char)
   ((unspecified? obj) &unspecified)
   ((unbound? obj) &unbound)
   ((false? obj) &false)
   ((true? obj) &true)
   ((procedure? obj) &procedure)
   ((pair? obj) &pair)
   ((variable? obj) &box)
   ((struct? obj) &struct)
   (else
    (debug 3 "*** Type not determined: ~a~%" obj)
    #f)))


;;;
;;; Record types
;;;

(define-record-type $return-address
  (make-return-address ip)
  return-address?
  (ip return-address-ip))

(define-record-type $dynamic-link
  (make-dynamic-link offset)
  dynamic-link?
  (offset dynamic-link-offset))

;; Data type to contain past frame data.
;;
;; Stores dynamic link, return addresses, and locals of caller procedure when
;; inlined procedure exist in trace.
(define-record-type $past-frame
  (%make-past-frame dls ras locals local-indices lowers)
  past-frame?

  ;; Association list for dynamic link: (local . pointer to fp).
  (dls past-frame-dls set-past-frame-dls!)

  ;; Association list for return address: (local . pointer to ra).
  (ras past-frame-ras set-past-frame-ras!)

  ;; Vector containing locals.
  (locals past-frame-locals set-past-frame-locals!)

  ;; All local indices found in trace.
  (local-indices past-frame-local-indices)

  ;; Lower frame data.
  (lowers past-frame-lowers))

(define (make-past-frame dls ras local-offset locals local-indices lowers)
  ;; Using hash-table to contain locals, since local index could take negative
  ;; value.
  (let ((table (make-hash-table))
        (nlocals (vector-length locals)))
    (let lp ((i 0) (end nlocals))
      (when (< i end)
        (let ((elem (and (vector-ref locals i)))
              (j (+ i local-offset)))
          (hashq-set! table j elem))
        (lp (+ i 1) end)))
    (%make-past-frame dls ras table local-indices lowers)))

(define (push-past-frame! past-frame dl ra local-offset locals)
  (set-past-frame-dls! past-frame (cons dl (past-frame-dls past-frame)))
  (set-past-frame-ras! past-frame (cons ra (past-frame-ras past-frame)))
  (let lp ((i 0)
           (end (vector-length locals))
           (to-update (past-frame-locals past-frame)))
    (when (< i end)
      (hashq-set! to-update (+ i local-offset) (vector-ref locals i))
      (lp (+ i 1) end to-update)))
  past-frame)

(define (pop-past-frame! past-frame)
  (let ((old-dls (past-frame-dls past-frame))
        (old-ras (past-frame-ras past-frame)))
    (when (not (null? old-dls))
      (set-past-frame-dls! past-frame (cdr old-dls)))
    (when (not (null? old-ras))
      (set-past-frame-ras! past-frame (cdr old-ras)))
    past-frame))

(define (past-frame-local-ref past-frame i)
  (hashq-get-handle (past-frame-locals past-frame) i))

(define (past-frame-lower-ref past-frame i)
  (let ((frames (past-frame-lowers past-frame)))
    (match frames
      (((offset . locals) . _)
       (let ((j (- i offset)))
         (or (and (<= 0 j)
                  (< j (vector-length locals))
                  (vector-ref locals j))
             #f)))
      (_ #f))))

;; Record type for snapshot.
(define-record-type $snapshot
  (%make-snapshot offset nlocals locals variables code)
  snapshot?

  ;; Integer number to shift vp->fp after returning with this snapshot.
  (offset snapshot-offset)

  ;; Number of locals at the time of snapshot.
  (nlocals snapshot-nlocals)

  ;; Association list of (local . type).
  (locals snapshot-locals)

  ;; Variables used at the time of taking exit.
  (variables snapshot-variables set-snapshot-variables!)

  ;; Native code of bailout with this snapshot.
  (code snapshot-code set-snapshot-code!))


;;;
;;; Locals
;;;

(define (accumulate-locals local-offset ops)
  (let* ((ret (make-hash-table))
         (lowers '())
         (offset local-offset))
    (define (nyi st op)
      (debug 3 "ir:accumulate-locals: NYI ~a~%" op)
      st)
    (define (acc-one st op locals)
      (define-syntax-rule (push-offset! n)
        (set! offset (+ offset n)))
      (define-syntax-rule (pop-offset! n)
        (set! offset (- offset n)))
      (define-syntax add!
        (syntax-rules ()
          ((_ st i j k l)
           (add! (add! st i j) k l))
          ((_ st i j k)
           (add! (add! st i j) k))
          ((_ st i j)
           (add! (add! st i) j))
          ((_ st i)
           (begin
             (hashq-set! st (+ i offset) #t)
             st))))
      (match op
        ((op a1)
         (case op
           ((return)
            ;; Store proc, returned value, VM frame dynamic link, and VM frame
            ;; return address.
            (add! st a1 1 -1 -2))
           ((br tail-call)
            st)
           (else
            (nyi st op))))
        ((op a1 a2)
         (case op
           ((call)
            ;; Store proc, VM frame dynamic link, and VM frame return address.
            (let ((st (add! st a1 (- a1 1) (- a1 2))))
              (push-offset! a1)
              st))
           ((static-ref
             make-short-immediate make-long-immediate make-long-long-immediate)
            (add! st a1))
           ((mov sub1 add1 box-ref box-set!)
            (add! st a1 a2))
           ((assert-nargs-ee/locals)
            st)
           (else
            (nyi st op))))
        ((op a1 a2 a3)
         (case op
           ((add sub mul div quo)
            (add! st a1 a2 a3))
           ((call-label)
            (let ((st (add! st a1)))
              (push-offset! a1)
              st))
           ((receive)
            ;; Modifying locals since procedure taking snapshot uses frame
            ;; lowers to recover the values after this `receive' bytecode
            ;; operation. Making a copy of locals so that later procedure can
            ;; see the original locals.
            (let ((locals-copy (vector-copy locals))
                  (ret-index (+ a2 1)))
              (pop-offset! a2)
              ;; XXX: If this test for `when' is removed, "mandelbrot.scm" with
              ;; `--jit-debug=0' will fail. However, running with `--jit-debug'
              ;; value greater than 0 will work.
              (when (and (< ret-index (vector-length locals))
                         (< a1 (vector-length locals-copy)))
                (vector-set! locals-copy a1 (vector-ref locals ret-index)))
              (set! lowers (acons offset locals-copy lowers))
              (add! st a1 a2)))
           ((receive-values)
            (pop-offset! a1)
            (add! st a1))
           (else
            (nyi st op))))
        ((op a1 a2 a3 a4)
         (case op
           ((br-if-< br-if-= br-if-<=)
            (add! st a1 a2))
           (else
            (nyi st op))))
        ((op a1 a2 a3 a4 a5)
         (case op
           ((toplevel-box)
            (add! st a1))
           (else
            (nyi st op))))
        ((op)
         (nyi st op))
        (_
         (error (format #f "ir:accumulate-locals: ~a" ops)))))
    (define (acc st ops)
      (match ops
        (((op _ _ _ locals) . rest)
         (acc (acc-one st op locals) rest))
        (()
         st)))
    (let ((local-indices (sort (hash-fold (lambda (k v acc)
                                            (cons k acc))
                                          '()
                                          (acc ret ops))
                               >)))
      (let ((verbosity (lightning-verbosity)))
        (when (and verbosity (<= 3 verbosity))
          (format #t ";;; local-indices:~%")
          (format #t ";;;   ~a~%" local-indices)
          (format #t ";;; lowers:~%")
          (when (tjit-dump-locals? (tjit-dump-option))
            (for-each (lambda (lower)
                        (format #t ";;;   ~a~%" lower))
                      lowers))))

      ;; Make past-frame with locals in lower frames.
      ;;
      ;; Lower frame data is saved at the time of accumulation. Otherwise, if
      ;; one of the guard operation appeared soon after bytecode sequence
      ;; `return' and `receive', snapshot does not know the value of locals in
      ;; lower frame. When recorded bytecode contains `return', snapshot will
      ;; recover a frame lower than the one used to enter the native call.
      ;;
      (make-past-frame '() '() local-offset #() local-indices lowers))))


;;;
;;; Snapshot
;;;

(define (make-snapshot local-offset lowest-offset highest-offset
                       locals parent-snapshot-locals indices vars past-frame)
  (define-syntax-rule (local-ref i)
    (vector-ref locals i))
  (define (parent-snapshot-local-ref i)
    (and parent-snapshot-locals
         (assq-ref parent-snapshot-locals i)))
  (define (shift-lowest acc)
    (map (match-lambda
          ((n . local)
           `(,(- n lowest-offset) . ,local)))
         acc))
  (let lp ((is (reverse indices)) (acc '()))
    (match is
      ((i . is)
       (define (dl-or-ra i)
         (or (assq-ref (past-frame-dls past-frame) i)
             (assq-ref (past-frame-ras past-frame) i)
             (let ((val (parent-snapshot-local-ref i)))
               (and (or (dynamic-link? val)
                        (return-address? val))
                    val))))
       (define (add-local local)
         (let ((type (type-of local)))
           (if (and type
                    (<= lowest-offset i highest-offset))
               (begin
                 (debug 3 ";;;   add-local: i=~a, local=~a~%" i local)
                 (lp is (cons `(,i . ,type) acc)))
               (lp is acc))))
       (define (add-val val)
         (debug 3 ";;;   add-val: i=~a, val=~a~%" i val)
         (lp is (cons `(,i . ,val) acc)))
       (cond
        ((= local-offset 0)
         (if (< i 0)
             (let ((frame-val (dl-or-ra i)))
               (if frame-val
                   (add-val frame-val)
                   (add-local (past-frame-lower-ref past-frame i))))
             (add-local (and (< i (vector-length locals))
                             (local-ref i)))))

        ;; Dynamic link and return address might need to be passed from
        ;; parent trace. When side trace of inlined procedure takes bailout
        ;; code, recorded trace might not contain bytecode operation to fill
        ;; in the dynamic link and return address of past frame.
        ((dl-or-ra i)
         => add-val)

        ;; Local in inlined procedure.
        ((<= 0 local-offset i)
         (let ((j (- i local-offset)))
           (if (< -1 j (vector-length locals))
               (add-local (local-ref j))
               (begin
                 (debug 1 ";;;   i=~a, local-offset=~a, skipping~%"
                        i local-offset)
                 (lp is acc)))))

        ;; Local in lower frame.
        ((<= local-offset i 0)
         (let ((j (- i local-offset)))
           (if (< -1 j (vector-length locals))
               (add-local (local-ref j))
               (begin
                 (debug 1 ";;;   i=~a, local-offset=~a, skipping~%"
                        i local-offset)
                 (lp is acc)))))

        ;; When side trace contains inlined procedure and the guard taking
        ;; this snapshot is from the caller of the inlined procedure,
        ;; saving local in upper frame. Looking up locals from newest
        ;; locals in past-frame.
        ((past-frame-local-ref past-frame i)
         => (match-lambda ((_ . local)
                           (add-local local))))

        ;; Side trace could start from the middle of inlined procedure,
        ;; locals in past frame may not have enough information to recover
        ;; locals in caller of the inlined procedure. In such case, look
        ;; up locals in the snapshot of parent trace.
        ((parent-snapshot-local-ref i)
         => add-val)

        ;; Giving up, skip this local.
        (else
         (debug 3 "*** local for i=~a not found~%" i)
         (add-local #f))))
      (()
       (let ((acc (reverse! acc)))
         (%make-snapshot local-offset
                         (vector-length locals)
                         (shift-lowest acc)
                         #f
                         #f))))))
