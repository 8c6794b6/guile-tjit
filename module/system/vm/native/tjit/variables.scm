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
  #:use-module (ice-9 match)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit registers)
  #:export (ref? ref-value ref-type
            make-constant constant? constant
            register?
            make-gpr gpr? gpr
            make-fpr fpr? fpr
            make-memory memory?))

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
