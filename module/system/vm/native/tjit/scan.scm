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


(define (scan-locals ol op prev-op dl locals initialized? backward?)
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
    (unless initialized?
      (set-outline-sp-offset! ol (- (outline-sp-offset ol) n))))
  (define-syntax-rule (pop-sp-offset! n)
    (unless initialized?
      (set-outline-sp-offset! ol (+ (outline-sp-offset ol) n))))
  (define-syntax-rule (push-fp-offset! n)
    (unless initialized?
      (set-outline-fp-offset! ol (- (outline-fp-offset ol) n))))
  (define-syntax-rule (pop-fp-offset! n)
    (unless initialized?
      (set-outline-fp-offset! ol (+ (outline-fp-offset ol) n))))
  (define-syntax-rule (save-sp-offset!)
    (unless initialized?
      (let ((new-offsets (cons (outline-sp-offset ol)
                               (outline-sp-offsets ol))))
        (set-outline-sp-offsets! ol new-offsets))))
  (define-syntax-rule (save-fp-offset!)
    (unless initialized?
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
              (indices (cons (+ i (outline-sp-offset ol)) indices))
              ...)
         (set-outline-local-indices! ol indices)))))
  (define-syntax set-write!
    (syntax-rules ()
      ((_ i ...)
       (let* ((writes (outline-write-indices ol))
              (writes (if (memq (+ i (outline-sp-offset ol)) writes)
                          writes
                          (cons (+ i (outline-sp-offset ol)) writes)))
              ...)
         (set-outline-write-indices! ol writes)))))
  (define-syntax set-read!
    (syntax-rules ()
      ((_ i ...)
       (let* ((reads (outline-read-indices ol))
              (reads (if (memq (+ i (outline-sp-offset ol)) reads)
                         reads
                         (cons (+ i (outline-sp-offset ol)) reads)))
              ...)
         (set-outline-read-indices! ol reads)))))
  (define-syntax add!
    (syntax-rules ()
      ((_ i ...)
       (begin
         (unless initialized?
           (set-index! i ...))
         (set-type! (i 'scm) ...)))))
  (define-syntax-rule (ret)
    (values #t (car op)))
  (define-syntax-rule (nyi)
    (begin
      (debug 1 "NYI: ~a~%" (car op))
      (values #f (car op))))
  (define-syntax-rule (scan-call proc nlocals)
    (let* ((stack-size (vector-length locals))
           (sp-proc (- stack-size proc 1)))
      (unless initialized?
        (add! sp-proc (+ sp-proc 1) (+ sp-proc 2))
        (set-read! sp-proc (+ sp-proc 1) (+ sp-proc 2))
        (set-write! (+ sp-proc 1) (+ sp-proc 2)))
      (let lp ((n 0))
        (when (< n nlocals)
          (set-type! ((- sp-proc n) 'scm))
          (lp (+ n 1))))
      (save-sp-offset!)
      (save-fp-offset!)
      (push-fp-offset! proc)
      (push-sp-offset! (- (+ proc nlocals) stack-size))
      (ret)))
  (define-syntax-rule (scan-tail-call nlocals)
    (let ((stack-size (vector-length locals)))
      (add! (- stack-size 1))
      (set-write! (- stack-size 1))
      (save-sp-offset!)
      (save-fp-offset!)
      (push-sp-offset! (- nlocals stack-size))
      (ret)))
  (define-syntax-rule (scan-frame nlocals)
    (let* ((stack-size (vector-length locals))
           (diff (- nlocals stack-size)))
      (if (< stack-size nlocals)
          (begin
            (push-sp-offset! diff)
            (let lp ((n 0))
              (when (< n diff)
                (add! n)
                (lp (+ n 1)))))
          (pop-sp-offset! (- diff)))
      (save-sp-offset!)
      (save-fp-offset!)
      (ret)))

  ;; Look for the type of returned value from C function.
  (unless initialized?
    (let* ((ret-types (outline-ret-types ol))
           (fill-false
            (lambda ()
              (set-outline-ret-types! ol (cons #f ret-types)))))
      (if (eq? 'subr-call prev-op)
          (match op
            (('receive dst proc nlocals)
             (let* ((stack-size (vector-length locals))
                    (idx (- stack-size proc 2))
                    (val (stack-element locals idx 'scm))
                    (type (type-of val)))
               (set-outline-ret-types! ol (cons type ret-types))))
            (('receive-values proc _ nvalues)
             (if (= nvalues 1)
                 (let* ((stack-size (vector-length locals))
                        (idx (- stack-size proc 2))
                        (val (stack-element locals idx 'scm))
                        (type (type-of val)))
                   (set-outline-ret-types! ol (cons type ret-types)))
                 (fill-false)))
            (_
             (fill-false)))
          (fill-false))))

  ;; Lookup accumulating procedure stored in *index-scanners* and apply
  ;; the procedure when found.
  ;;
  ;; VM operations which moves frame pointer and stack pointer are not stored
  ;; in *index-scanners* and treated specially.
  (match op
    (('call proc nlocals)
     (scan-call proc nlocals))
    (('call-label proc nlocals _)
     (scan-call proc nlocals))
    (('tail-call nlocals)
     (scan-tail-call nlocals))
    (('tail-call-label nlocals _)
     (scan-tail-call nlocals))
    (('subr-call)
     ;; XXX: Multiple value return not supported.
     (let ((stack-size (vector-length locals)))
       (save-sp-offset!)
       (pop-sp-offset! (- stack-size 2))
       (save-fp-offset!)
       (pop-fp-offset! dl)
       (ret)))
    (('receive dst proc nlocals)
     (let* ((stack-size (vector-length locals))
            (fp (- stack-size proc)))
       (add! (- stack-size dst 1) (- stack-size proc 2))
       (set-read! (- stack-size proc 2))
       (save-sp-offset!)
       (pop-sp-offset! (- stack-size nlocals))
       (unless initialized?
         (set-write! (- nlocals dst 1)))
       (save-fp-offset!)
       (ret)))
    (('receive-values proc _ nvalues)
     ;; XXX: Multiple values NYI
     (if (= nvalues 1)
         (let* ((stack-size (vector-length locals))
                (fp (- stack-size proc 1)))
           (let lp ((n nvalues))
             (when (< 0 n)
               (add! (- fp n))
               (set-read! (- fp n))
               (lp (- n 1))))
           (save-sp-offset!)
           (save-fp-offset!)
           (ret))
         (nyi)))
    (('return-values nlocals)
     (let ((stack-size (vector-length locals)))
       (add! stack-size (+ stack-size 1))
       (set-read! stack-size (+ stack-size 1))
       (set-write! stack-size (+ stack-size 1))
       (let lp ((n nlocals))
         (when (<= 2 n)
           (add! (- stack-size n))
           (set-read! (- stack-size n))
           (lp (- n 1))))
       (save-sp-offset!)
       (pop-sp-offset! (- stack-size nlocals))
       (save-fp-offset!)
       (pop-fp-offset! dl)
       (ret)))
    (('assert-nargs-ee/locals expected nlocals)
     (push-sp-offset! nlocals)
     (let lp ((n nlocals))
       (when (< 0 n)
         (add! (- n 1))
         (set-write! (- n 1))
         (lp (- n 1))))
     (save-sp-offset!)
     (save-fp-offset!)
     (ret))
    (('alloc-frame nlocals)
     (scan-frame nlocals))
    (('reset-frame nlocals)
     (scan-frame nlocals))
    (('mov dst src)
     (unless initialized?
       (set-index! dst src)
       (set-read! src)
       (set-write! dst))
     (let ((sp-offset (outline-sp-offset ol)))
       (if backward?
           (and=> (outline-type-ref ol (+ dst sp-offset))
                  (lambda (type)
                    (set-type! (src type))))
           (and=> (outline-type-ref ol (+ src sp-offset))
                  (lambda (type)
                    (set-type! (dst type))))))
     (save-sp-offset!)
     (save-fp-offset!)
     (ret))
    (_
     (cond
      ((hashq-ref *scan-procedures* (car op))
       => (lambda (proc)
            (apply proc ol initialized? (cdr op))
            (save-sp-offset!)
            (save-fp-offset!)
            (ret)))
      (else
       (nyi))))))
