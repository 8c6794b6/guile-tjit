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


(define* (scan-locals ol op prev-op locals #:optional (type-only? #f))
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
    (set-outline-sp-offset! ol (- (outline-sp-offset ol) n)))
  (define-syntax-rule (pop-sp-offset! n)
    (set-outline-sp-offset! ol (+ (outline-sp-offset ol) n)))
  (define-syntax-rule (push-fp-offset! n)
    (set-outline-fp-offset! ol (- (outline-fp-offset ol) n)))
  (define-syntax-rule (pop-fp-offset! n)
    (set-outline-fp-offset! ol (+ (outline-fp-offset ol) n)))
  (define-syntax-rule (save-sp-offset!)
    (when (not type-only?)
      (let ((new-offsets (cons (outline-sp-offset ol)
                               (outline-sp-offsets ol))))
        (set-outline-sp-offsets! ol new-offsets))))
  (define-syntax-rule (save-fp-offset!)
    (when (not type-only?)
      (let ((new-offsets (cons (outline-fp-offset ol)
                               (outline-fp-offsets ol))))
        (set-outline-fp-offsets! ol new-offsets))))
  (define-syntax set-type!
    (syntax-rules ()
      ((_ (i t) ...)
       (let* ((types (outline-types ol))
              (types (assq-set! types (+ i (outline-sp-offset ol)) t))
              ...)
         (set-outline-types! ol types)))))
  (define-syntax set-index!
    (syntax-rules ()
      ((_ i ...)
       (let* ((indices (outline-local-indices ol))
              (indices (assq-set! indices (+ i (outline-sp-offset ol)) #t))
              ...)
         (set-outline-local-indices! ol indices)))))
  (define-syntax add!
    (syntax-rules ()
      ((_ i ...)
       (when (not type-only?)
         (set-index! i ...)
         (set-type! (i 'scm) ...)))))
  (define-syntax-rule (ret)
    (values #t (car op)))
  (define-syntax-rule (nyi)
    (begin
      (debug 1 "NYI: ~a~%" (car op))
      (values #f (car op))))

  (when (not type-only?)
    (let ((ret-types (outline-ret-types ol)))
      (if (eq? 'subr-call prev-op)
          (begin
            (match op
              (('receive dst proc nlocals)
               (let* ((stack-size (vector-length locals))
                      (idx (- stack-size proc 2))
                      (val (stack-element locals idx 'scm))
                      (type (type-of val)))
                 (set-outline-ret-types! ol (cons type ret-types))))
              (_
               (set-outline-ret-types! ol (cons #f ret-types)))))
          (set-outline-ret-types! ol (cons #f ret-types)))))

  (cond
   ((hashq-ref *element-type-scanners* (car op))
    => (lambda (proc)
         (apply proc ol (outline-sp-offset ol) (cdr op))))
   (else
    (values)))

  ;; Lookup accumulating procedure stored in *index-scanners* and apply
  ;; the procedure when found.
  ;;
  ;; VM operations which moves frame pointer and stack pointer are not stored
  ;; in *index-scanners* and treated specially.
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
    (('subr-call)
     ;; XXX: Multiple value return not supported.
     (let ((stack-size (vector-length locals)))
       (save-sp-offset!)
       (pop-sp-offset! (- stack-size 2))
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
    (('receive-values proc _ _)
     ;; XXX: NYI
     (nyi))
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
     (cond
      ((hashq-ref *index-scanners* (car op))
       => (lambda (proc)
            (if (not type-only?)
                (begin
                  (apply proc ol (outline-sp-offset ol) (cdr op))
                  (save-sp-offset!)
                  (save-fp-offset!)
                  (ret))
                (values #t (car op)))))
      (else
       (nyi))))))
