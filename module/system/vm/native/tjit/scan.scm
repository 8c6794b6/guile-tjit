;;;; Stack element scanner

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
;;; A module containing procedure to scan through stack elements before IR
;;; compilation.
;;;
;;; Code:

(define-module (system vm native tjit scan)
  #:use-module (ice-9 match)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit snapshot)
  #:export (scan-locals))


(define* (scan-locals pf op locals #:optional (type-only? #f))
  ;; Compute local indices and stack element types in op.
  ;;
  ;; Lower frame data is saved at the time of accumulation.  If one of
  ;; the guard operation appeared soon after bytecode sequence
  ;; `return' or `receive', snapshot does not know the value of locals
  ;; in lower frame. When recorded bytecode contains `return' before
  ;; `call', snapshot will recover a frame higher than the one used to
  ;; enter the native call.
  ;;
  ;; The stack grows down.
  ;;
  (define-syntax-rule (push-sp-offset! n)
    (set-past-frame-sp-offset! pf (- (past-frame-sp-offset pf) n)))
  (define-syntax-rule (pop-sp-offset! n)
    (set-past-frame-sp-offset! pf (+ (past-frame-sp-offset pf) n)))
  (define-syntax-rule (push-fp-offset! n)
    (set-past-frame-fp-offset! pf (- (past-frame-fp-offset pf) n)))
  (define-syntax-rule (pop-fp-offset! n)
    (set-past-frame-fp-offset! pf (+ (past-frame-fp-offset pf) n)))
  (define-syntax-rule (save-sp-offset!)
    (when (not type-only?)
      (let ((new-offsets (cons (past-frame-sp-offset pf)
                               (past-frame-sp-offsets pf))))
        (set-past-frame-sp-offsets! pf new-offsets))))
  (define-syntax-rule (save-fp-offset!)
    (when (not type-only?)
      (let ((new-offsets (cons (past-frame-fp-offset pf)
                               (past-frame-fp-offsets pf))))
        (set-past-frame-fp-offsets! pf new-offsets))))
  (define-syntax set-type!
    (syntax-rules ()
      ((_ (i t) ...)
       (let* ((types (past-frame-types pf))
              (types (assq-set! types (+ i (past-frame-sp-offset pf)) t))
              ...)
         (set-past-frame-types! pf types)))))
  (define-syntax add!
    (syntax-rules ()
      ((_ i ...)
       (when (not type-only?)
         (let* ((indices (past-frame-local-indices pf))
                (indices (assq-set! indices (+ i (past-frame-sp-offset pf)) #t))
                ...)
           (set-past-frame-local-indices! pf indices))
         (set-type! (i 'scm) ...)))))
  (define-syntax-rule (ret)
    #t)
  (define-syntax-rule (nyi op)
    (begin
      (debug 1 "XXX: NYI ~a~%" op)
      #f))

  (cond
   ((hashq-ref *element-type-scanners* (car op))
    => (lambda (proc)
         (apply proc pf (past-frame-sp-offset pf) (cdr op))))
   (else
    (values)))

  ;; Lookup accumulating procedure stored in *index-scanners* and apply
  ;; the procedure when found.
  ;;
  ;; VM operations which moves frame pointer and stack pointer are not stored
  ;; in *index-scanners* and treated specially.
  (cond
   ((hashq-ref *index-scanners* (car op))
    => (lambda (proc)
         (if (not type-only?)
             (begin
               (apply proc pf (past-frame-sp-offset pf) (cdr op))
               (save-sp-offset!)
               (save-fp-offset!)
               (ret))
             #f)))
   (else
    (match op
      (('call proc nlocals)
       (let* ((stack-size (vector-length locals))
              (sp-proc (- stack-size proc 1)))
         (add! sp-proc (+ sp-proc 1) (+ sp-proc 2))
         (let lp ((n 1))
           (when (<= n nlocals)
             (set-type! ((- sp-proc n) 'scm))
             (lp (+ n 1))))
         (save-sp-offset!)
         (save-fp-offset!)
         (push-fp-offset! proc)
         (push-sp-offset! (- (+ proc nlocals) stack-size))
         (ret)))
      (('call-label proc nlocals _)
       (let ((stack-size (vector-length locals)))
         (add! proc (- proc 1) (- proc 2))
         (save-sp-offset!)
         (save-fp-offset!)
         (push-fp-offset! proc)
         (push-sp-offset! (- (+ proc nlocals) stack-size))
         (ret)))
      (('tail-call nlocals)
       (let ((stack-size (vector-length locals)))
         (add! (- stack-size 1))
         (save-sp-offset!)
         (save-fp-offset!)
         (ret)))
      (('tail-call-label nlocals label)
       (let ((stack-size (vector-length locals)))
         (add! (- stack-size 1))
         (save-sp-offset!)
         (save-fp-offset!)
         (ret)))
      (('receive dst proc nlocals)
       (let* ((stack-size (vector-length locals))
              (fp (- stack-size proc)))
         (add! (- stack-size dst 1) (- stack-size proc 2))
         (save-sp-offset!)
         (pop-sp-offset! (- stack-size nlocals))
         (pop-fp-offset! proc)
         (save-fp-offset!)
         (ret)))
      ;; XXX: NYI receive-values
      ;; (('receive-values proc _ _)
      ;;  (add! st proc))
      (('return-values nlocals)
       (let ((stack-size (vector-length locals)))
         (add! stack-size (+ stack-size 1))
         (let lp ((n nlocals))
           (when (<= 2 n)
             (add! (- stack-size n))
             (lp (- n 1))))
         (save-sp-offset!)
         (pop-sp-offset! (- stack-size nlocals))
         (save-fp-offset!)
         (ret)))
      (('assert-nargs-ee/locals expected nlocals)
       (push-sp-offset! nlocals)
       (let lp ((n nlocals))
         (when (< 0 n)
           (add! (- n 1))
           (lp (- n 1))))
       (save-sp-offset!)
       (save-fp-offset!)
       (ret))
      (_
       (nyi (car op)))))))
