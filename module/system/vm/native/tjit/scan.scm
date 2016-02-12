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
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit snapshot)
  #:export (scan-locals))

(define (scan-locals ol op prev-op ip dl locals)
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
  (define-syntax-rule (save-write-buf!)
    (unless initialized?
      (let ((writes (outline-write-indices ol))
            (buf (outline-write-buf ol)))
        (set-outline-write-buf! ol (cons writes buf)))))
  (define-syntax set-type!
    (syntax-rules ()
      ((_ (i t) ...)
       (let* ((types (outline-types ol))
              (types (assq-set! types (+ i (outline-sp-offset ol)) t))
              ...)
         (set-outline-types! ol types)))))
  (define-syntax set-write!
    (syntax-rules ()
      ((_ i ...)
       (let* ((writes (outline-write-indices ol))
              (writes (if (memq (+ i (outline-sp-offset ol)) writes)
                          writes
                          (cons (+ i (outline-sp-offset ol)) writes)))
              ...)
         (set-outline-write-indices! ol (sort writes <))))))
  (define-syntax set-read!
    (syntax-rules ()
      ((_ i ...)
       (let* ((reads (outline-read-indices ol))
              (reads (if (memq (+ i (outline-sp-offset ol)) reads)
                         reads
                         (cons (+ i (outline-sp-offset ol)) reads)))
              ...)
         (set-outline-read-indices! ol reads)))))
  (define-syntax set-scm!
    (syntax-rules ()
      ((_ i ...)
       (set-type! (i 'scm) ...))))
  (define-syntax-rule (ret)
    (begin
      (unless initialized?
        (debug 1 "~a"
               (and ((@ (system vm native tjit dump) dump-outline) ol)
                    "")))
      (values #t (car op))))
  (define-syntax-rule (nyi)
    (begin
      (debug 1 "NYI: ~a~%" (car op))
      (values #f (car op))))
  (define-syntax-rule (scan-call proc nlocals label?)
    (let* ((stack-size (vector-length locals))
           (sp-offset (outline-sp-offset ol))
           (sp-proc (- stack-size proc 1)))
      (unless initialized?
        (set-scm! sp-proc (+ sp-proc 1) (+ sp-proc 2))
        (unless label?
          (set-read! sp-proc)
          (set-entry-type! ol sp-proc &procedure))
        (set-read! (+ sp-proc 1) (+ sp-proc 2))
        (set-write! (+ sp-proc 1) (+ sp-proc 2))
        (let lp ((n 1))
          (when (< n nlocals)
            (set-entry-type! ol (- (+ sp-proc sp-offset) n) &scm)
            (lp (+ n 1)))))
      (when infer-type?
        (unless label?
          (set-expected-type! ol (+ sp-proc sp-offset) &procedure))
        (let ((ra-ty (make-return-address
                      (make-pointer (+ ip (* 4 (if label? 3 2))))))
              (dl-ty (make-dynamic-link proc)))
          (set-inferred-type! ol (+ sp-proc sp-offset 1) ra-ty)
          (set-inferred-type! ol (+ sp-proc sp-offset 2) dl-ty))
        (debug 1 ";;; [scan-locals] scan-call: ~s ~s ~s~%" proc nlocals label?)
        (do ((n 0 (+ n 1))) ((= n nlocals))
          (let ((ity (assq-ref (outline-inferred-types ol) n)))
            (match ity
              (('copy . dst)
               (debug 1 ";;; [scan-locals] call arg:~s is copy~%" n)
               (set-inferred-type! ol n &scm)
               (set-entry-type! ol dst &scm)
               (set-expected-type! ol dst &scm))
              (x
               (debug 1 ";;; [scan-locals] call arg:~s is ~s~%" n x)
               (values))))
          (set-expected-type! ol (- (+ sp-proc sp-offset) n) &scm)))
      (let lp ((n 0))
        (when (< n nlocals)
          (set-type! ((- sp-proc n) 'scm))
          (lp (+ n 1))))
      (save-sp-offset!)
      (save-fp-offset!)
      (save-write-buf!)
      (push-fp-offset! proc)
      (push-sp-offset! (- (+ proc nlocals) stack-size))
      (ret)))
  (define-syntax-rule (scan-tail-call nlocals)
    (let ((stack-size (vector-length locals)))
      (set-scm! (- stack-size 1))
      (unless initialized?
        (set-write! (- stack-size 1)))
      (save-sp-offset!)
      (save-fp-offset!)
      (save-write-buf!)
      (push-sp-offset! (- nlocals stack-size))
      (ret)))
  (define initialized? (outline-initialized? ol))
  (define backward? (outline-backward? ol))
  (define infer-type? (outline-infer-type? ol))

  (unless initialized?
    (debug 1 ";;; [scan-locals] op=~s~%" op))

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
     (scan-call proc nlocals #f))
    (('call-label proc nlocals _)
     (scan-call proc nlocals #t))
    (('tail-call nlocals)
     (scan-tail-call nlocals))
    (('tail-call-label nlocals _)
     (scan-tail-call nlocals))
    (('receive dst proc nlocals)
     (let* ((stack-size (vector-length locals))
            (sp-offset (outline-sp-offset ol))
            (fp (- stack-size proc)))
       (set-scm! (- stack-size dst 1) (- stack-size proc 2))
       (set-write! (- stack-size dst 1))
       (set-read! (- stack-size proc 2))
       (save-sp-offset!)
       (pop-sp-offset! (- stack-size nlocals))
       (unless initialized?
         (set-write! (- nlocals dst 1)))
       (when infer-type?
         (let ((inferred (outline-inferred-types ol))
               (expected (outline-expected-types ol))
               (proc/i (+ (- stack-size proc 2) sp-offset))
               (dst/i (+ (- stack-size dst 1) sp-offset)))
           (cond
            ((or (assq-ref inferred proc/i)
                 (assq-ref expected proc/i))
             => (lambda (ty)
                  (set-inferred-type! ol dst/i ty)))
            (else
             (let ((ty `(copy . ,proc/i)))
               (set-inferred-type! ol dst/i ty))))))
       (save-fp-offset!)
       (save-write-buf!)
       (ret)))
    (('receive-values proc _ nvalues)
     ;; XXX: Multiple values NYI
     (if (= nvalues 1)
         (let* ((stack-size (vector-length locals))
                (fp (- stack-size proc 1)))
           (let lp ((n nvalues))
             (when (< 0 n)
               (set-read! (- fp n))
               (lp (- n 1))))
           (save-sp-offset!)
           (save-fp-offset!)
           (save-write-buf!)
           (ret))
         (nyi)))
    (('return-values nlocals)
     (let* ((sp-offset (outline-sp-offset ol))
            (stack-size (vector-length locals))
            (ra-offset stack-size)
            (dl-offset (+ ra-offset 1)))
       (set-scm! ra-offset dl-offset)
       (set-read! ra-offset dl-offset)
       (unless initialized?
         (set-write! ra-offset dl-offset))
       (let lp ((n nlocals))
         (when (<= 2 n)
           (set-scm! (- stack-size n))
           (set-read! (- stack-size n))
           (lp (- n 1))))
       (save-sp-offset!)
       (pop-sp-offset! (- stack-size nlocals))
       (save-fp-offset!)
       (pop-fp-offset! dl)
       (save-write-buf!)
       (ret)))
    (_
     (cond
      ((hashq-ref *scan-procedures* (car op))
       => (lambda (found)
            (cond
             ((procedure? found)
              (apply found ip dl locals ol (cdr op)))
             ((list? found)
              (let lp ((procs found))
                (match procs
                  (((test . work) . procs)
                   (if (apply test (list ol op locals))
                       (apply work ip dl locals ol (cdr op))
                       (lp procs)))
                  (_ (nyi)))))
             (else
              (nyi)))))
      (else
       (nyi))))))
