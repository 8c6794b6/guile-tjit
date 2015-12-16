;;;; Error handling during JIT compilation

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
;;; Module containing codes related to error during traacing JIT compilation.
;;; Errors in tracing JIT compilation happens when somethings wrong happened
;;; other than not-yet-implemented (NYI). Which means, malfunctioning code in
;;; known bytecode operation.

;;; Code:

(define-module (system vm native tjit error)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit parameters)
  #:export (tjitc-errors
            tjitc-error
            call-with-tjitc-error-handler
            with-tjitc-error-handler
            nyi
            call-with-nyi-handler
            with-nyi-handler))

(define *tjitc-prompt-tag*
  (make-prompt-tag "tjitc"))

(define *tjitc-errors-table*
  (make-hash-table))

(define (tjitc-prompt-tag)
  *tjitc-prompt-tag*)

(define (tjitc-errors)
  *tjitc-errors-table*)

(define (tjitc-error name fmt . args)
  (apply abort-to-prompt (tjitc-prompt-tag) name fmt args))

(define (call-with-tjitc-error-handler ip thunk)
  (call-with-prompt (tjitc-prompt-tag)
    thunk
    (lambda (k name fmt . args)
      (let ((msg (apply format #f fmt args)))
        (debug 0 "[~a] ~a: ~a~%" (red "TJITC ERROR") name msg)
        (tjit-increment-compilation-failure! ip)
        (hashq-set! (tjitc-errors) ip (cons name msg))))))

(define-syntax with-tjitc-error-handler
  (syntax-rules ()
    ((_ ip exp)
     (call-with-tjitc-error-handler ip
       (lambda () exp)))))

(define *nyi-prompt-tag*
  (make-prompt-tag "nyi"))

(define (nyi-prompt-tag)
  *nyi-prompt-tag*)

(define (nyi fmt . args)
  (apply abort-to-prompt (nyi-prompt-tag) fmt args))

(define (call-with-nyi-handler ip thunk)
  (call-with-prompt (nyi-prompt-tag)
    thunk
    (lambda (k fmt . args)
      (debug 1 "NYI: ~a~%" (apply format #f fmt args))
      (tjit-increment-compilation-failure! ip))))

(define-syntax with-nyi-handler
  (syntax-rules ()
    ((_ ip exp)
     (call-with-nyi-handler ip
       (lambda () exp)))))
