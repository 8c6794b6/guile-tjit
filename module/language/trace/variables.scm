;;;; Variable resoluation for vm-tjit engine

;;;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.
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
;;; IR variables for register alloocatoion.
;;;
;;; Code:

(define-module (language trace variables)
  #:use-module (ice-9 format)
  #:use-module (system foreign)
  #:use-module (language trace error)
  #:use-module (language trace parameters)
  #:use-module (language trace registers)
  #:use-module (language trace types)
  #:export (make-var make-vars
            ref? ref-value ref-type
            make-con con? con
            register?
            make-gpr gpr? gpr
            make-fpr fpr? fpr
            make-memory memory?
            make-tmpvar make-tmpvar/f make-spill
            argr fargr moffs physical-name))


;;;; Symbol cache table

;; Contains internal hash table to cache variable symbols. Used to obtain symbol
;; from number, instead of calling number->string, string-append, string->symbol
;; ... etc for each time.
(define make-var
  (let ((cache-table (make-hash-table)))
    (lambda (index)
      (or (hashq-ref cache-table index)
          (let ((var (string->symbol
                      (string-append "v" (number->string index)))))
            (hashq-set! cache-table index var)
            var)))))

(define (make-vars locals)
  ;; Might better to use other data structure than alist for variables.
  ;; Number of variables won't change after getting the number of locals from
  ;; `accumulate-locals'.
  (map (lambda (n)
         (cons n (make-var n)))
       locals))


;;;; Variable

;;; XXX: Avoid using cons cell for CON.

(define (ref? x)
  (and (pair? x) (symbol? (car x))))

(define (ref-value x)
  (and (pair? x) (cdr x)))

(define-inlinable (%ref-value x)
  "Inlinable variant of `ref-value' without `pair?' check."
  (cdr x))

(define-syntax-rule (ref-type x)
  (car x))

(define (make-con x)
  (cons 'con x))

(define (con? x)
  (eq? 'con (ref-type x)))

(define (con x)
  (let ((val (ref-value x)))
    (cond
     ((flonum? val) (object-address val))
     ((number? val) val)
     ((not val) *scm-false*)
     ((null? val) *scm-null*)
     ((true? val) *scm-true*)
     (else (error 'con x)))))

(define make-gpr
  ;; Total number of GPR is *num-gpr* plus three scratch registers.
  (let ((cache-vector (make-vector (+ *num-gpr* 3) #f)))
    (lambda (x)
      (or (vector-ref cache-vector (+ x 3))
          (let ((val (cons 'gpr x)))
            (vector-set! cache-vector (+ x 3) val)
            val)))))

(define-inlinable (gpr x)
  (gpr-ref (%ref-value x)))

(define (gpr? x)
  (eq? 'gpr (ref-type x)))

(define make-fpr
  ;; Total number of FPR is *num-fpr* plus three scratch registers.
  (let ((cache-vector (make-vector (+ *num-fpr* 3) #f)))
    (lambda (x)
      (or (vector-ref cache-vector (+ x 3))
          (let ((val (cons 'fpr x)))
            (vector-set! cache-vector (+ x 3) val)
            val)))))

(define-inlinable (fpr x)
  (fpr-ref (%ref-value x)))

(define (fpr? x)
  (eq? 'fpr (ref-type x)))

(define (register? x)
  (or (eq? 'gpr (ref-type x))
      (eq? 'fpr (ref-type x))))

(define make-memory
  (let ((cache-table (make-hash-table)))
    (lambda (x)
      (or (hashq-ref cache-table x)
          (let ((val (cons 'mem x)))
            (hashq-set! cache-table x val)
            val)))))

(define (memory? x)
  (eq? 'mem (ref-type x)))


;;;; Temporary variables

(define *tmpvars*
  #(r0 r1 r2))

(define *tmpvars/f*
  #(f0 f1 f2))

(define (make-tmpvar n)
  (vector-ref *tmpvars* n))

(define (make-tmpvar/f n)
  (vector-ref *tmpvars/f* n))

(define make-spill
  (let ((cache-table (make-hash-table)))
    (lambda (n)
      (or (hashq-ref cache-table n)
          (let ((v (string->symbol (string-append "m" (number->string n)))))
            (hashq-set! cache-table n v)
            v)))))

(define (argr n)
  (if (< *num-arg-gprs* n)
      #f
      (make-gpr (- *num-gpr* n))))

(define (fargr n)
  (if (< *num-arg-fprs* n)
      #f
      (make-fpr (- *num-fpr* n))))

(define-inlinable (moffs mem)
  (let ((n (- (+ 2 1 (%ref-value mem) *num-volatiles* *num-fpr*))))
    (* n %word-size)))

(define (physical-name x)
  (cond
   ((register? x) (register-name x))
   ((memory? x) (format #f "[0x~x]" (- (moffs x))))
   (else x)))
