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


;;;
;;; Symbol cache table
;;;

;;; Hash table to cache variable symbols. Used to obtain symbol from
;;; number, instead of calling number->string, string-append,
;;; string->symbol ... etc for each time.
(define *var-cache-table* (make-hash-table))

(define (make-var index)
  (or (hashq-ref *var-cache-table* index)
      (let ((var (string->symbol
                  (string-append "v" (number->string index)))))
        (hashq-set! *var-cache-table* index var)
        var)))

(define (make-vars locals)
  ;; Might better to use other data structure than alist for variables.
  ;; Number of variables won't change after getting the number of locals from
  ;; `accumulate-locals'.
  (map (lambda (n)
         (cons n (make-var n)))
       locals))


;;;
;;; Variable
;;;

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
     ((and (number? val) (exact? val))
      (if (<= 0 val)
          (make-pointer val)
          (make-negative-pointer val)))
     (else
      (scm->pointer val)))))

(define *gpr-cache-vector*
  ;; Total number is *num-gpr* plus three scratch registers.
  (make-vector (+ *num-gpr* 3) #f))

(define (make-gpr x)
  (or (vector-ref *gpr-cache-vector* (+ x 3))
      (let ((val (cons 'gpr x)))
        (vector-set! *gpr-cache-vector* (+ x 3) val)
        val)))

(define (gpr x)
  (gpr-ref (%ref-value x)))

(define (gpr? x)
  (eq? 'gpr (ref-type x)))

(define *fpr-cache-vector*
  ;; Total number is *num-fpr* plus three scratch registers.
  (make-vector (+ *num-fpr* 3) #f))

(define (make-fpr x)
  (or (vector-ref *fpr-cache-vector* (+ x 3))
      (let ((val (cons 'fpr x)))
        (vector-set! *fpr-cache-vector* (+ x 3) val)
        val)))

(define (fpr x)
  (fpr-ref (%ref-value x)))

(define (fpr? x)
  (eq? 'fpr (ref-type x)))

(define (register? x)
  (or (eq? 'gpr (ref-type x))
      (eq? 'fpr (ref-type x))))

(define *mem-cache-table* (make-hash-table))

(define (make-memory x)
  (or (hashq-ref *mem-cache-table* x)
      (let ((val (cons 'mem x)))
        (hashq-set! *mem-cache-table* x val)
        val)))

(define (memory? x)
  (eq? 'mem (ref-type x)))


;;;
;;; Temporary variables
;;;

(define *tmpvars*
  #(r0 r1 r2))

(define *tmpvars/f*
  #(f0 f1 f2))

(define (make-tmpvar n)
  (vector-ref *tmpvars* n))

(define (make-tmpvar/f n)
  (vector-ref *tmpvars/f* n))

(define *spill-cache-table* (make-hash-table))

(define (make-spill n)
  (or (hashq-ref *spill-cache-table* n)
      (let ((v (string->symbol (string-append "m" (number->string n)))))
        (hashq-set! *spill-cache-table* n v)
        v)))

(define (argr n)
  (if (< *num-arg-gprs* n)
      #f
      (make-gpr (- *num-gpr* n))))

(define (fargr n)
  (if (< *num-arg-fprs* n)
      #f
      (make-fpr (- *num-fpr* n))))

(define-syntax-rule (moffs mem)
  (let ((n (- (+ 2 1 (%ref-value mem) *num-volatiles* *num-fpr*))))
    (make-negative-pointer (* n %word-size))))

(define (physical-name x)
  (cond
   ((register? x)
    (register-name x))
   ((memory? x)
    (format #f "[0x~x]"
            (+ (- (case %word-size
                    ((4) #xffffffff)
                    ((8) #xffffffffffffffff)
                    (else
                     (failure 'physical-name "unknown word-size ~s"
                              %word-size)))
                  (pointer-address (moffs x)))
               1)))
   (else x)))
