;;;; Variable resoluation for vm-tjit engine

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
;;; IR variables for register alloocatoion.
;;;
;;; Code:

(define-module (system vm native tjit variables)
  #:use-module (ice-9 format)
  #:use-module (language cps)
  #:use-module (system foreign)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit registers)
  #:export (ref? ref-value ref-type
            make-constant constant? constant
            register?
            make-gpr gpr? gpr
            make-fpr fpr? fpr
            make-memory memory?
            make-tmpvar make-tmpvar/f
            argr fargr moffs physical-name))

;;;
;;; Variable
;;;

(define (ref? x)
  (and (pair? x) (symbol? (car x))))

(define (ref-value x)
  (and (ref? x) (cdr x)))

(define (ref-type x)
  (and (ref? x) (car x)))

(define (make-constant x)
  (cons 'const x))

(define (constant? x)
  (eq? 'const (ref-type x)))

(define (constant x)
  (let ((val (ref-value x)))
    (cond
     ((and (number? val) (exact? val))
      (make-pointer (ref-value x)))
     (else
      (scm->pointer val)))))

(define (make-gpr x)
  (cons 'gpr x))

(define (gpr x)
  (gpr-ref (ref-value x)))

(define (gpr? x)
  (eq? 'gpr (ref-type x)))

(define (make-fpr x)
  (cons 'fpr x))

(define (fpr x)
  (fpr-ref (ref-value x)))

(define (fpr? x)
  (eq? 'fpr (ref-type x)))

(define (register? x)
  (or (eq? 'gpr (ref-type x))
      (eq? 'fpr (ref-type x))))

(define (make-memory x)
  (cons 'mem x))

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

(define (argr n)
  (if (< *num-arg-gprs* n)
      #f
      (make-gpr (- *num-gpr* n))))

(define (fargr n)
  (if (< *num-arg-fprs* n)
      #f
      (make-fpr (- *num-fpr* n))))

(define-syntax-rule (moffs mem)
  (let ((n (- (+ 2 1 (ref-value mem) *num-volatiles* *num-fpr*))))
    (make-pointer (+ (ash 2 (- (* 8 %word-size) 1))
                     (* n %word-size)))))

(define (physical-name x)
  (cond
   ((register? x)
    (register-name x))
   ((memory? x)
    (format #f "[@ 0x~x]"
            (+ (- (case %word-size
                    ((4) #xffffffff)
                    ((8) #xffffffffffffffff)
                    (else tjitc-error 'physical-name "unknown word-size ~s"
                          %word-size))
                  (pointer-address (moffs x)))
               1)))
   (else x)))
