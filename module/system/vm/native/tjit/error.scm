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
;;; other than explicit retry or not-yet-implemented (NYI). Which means,
;;; malfunctioning code in bytecode operation which should be compiled.

;;; Code:

(define-module (system vm native tjit error)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit parameters)
  #:export (tjitc-errors
            call-with-tjitc-error-handler
            with-tjitc-error-handler
            nyi break recompile failure))

;;;
;;; Handler
;;;

(define *tjitc-error-prompt-tag*
  (make-prompt-tag 'tjitc))

(define *tjitc-errors-table*
  (make-hash-table))

(define (tjitc-errors)
  *tjitc-errors-table*)

(define (call-with-tjitc-error-handler env thunk)
  (define (increment! amount)
    (unless (env-parent-fragment env)
      (tjit-increment-compilation-failure! (env-entry-ip env) amount)))
  (call-with-prompt *tjitc-error-prompt-tag*
    thunk
    (lambda (k kind meta fmt . args)
      (case kind
        ((nyi)
         (debug 1 "NYI: ~a~%" (apply format #f fmt args))
         (increment! 3))
        ((break)
         (debug 1 "BREAK: ~a~%" (apply format #f fmt args))
         (increment! meta))
        ((recompile)
         (debug 1 "RECOMPILE: ~a~%" (apply format #f fmt args)))
        ((failure)
         (let ((msg (apply format #f fmt args)))
           (debug 0 "~a: ~a ~a~%" (red "FAILURE") meta msg)
           (increment! 5)
           (hashq-set! (tjitc-errors) (env-entry-ip env)
                       (cons meta msg))))))))

(define-syntax with-tjitc-error-handler
  (syntax-rules ()
    ((_ ip exp)
     (call-with-tjitc-error-handler ip
       (lambda () exp)))))


;;;
;;; Errors
;;;

(define-syntax-rule (make-tjitc-error kind name fmt args)
  (apply abort-to-prompt *tjitc-error-prompt-tag* kind name fmt args))

(define (nyi fmt . args)
  (make-tjitc-error 'nyi #f fmt args))

(define (break increment fmt . args)
  (make-tjitc-error 'break increment fmt args))

(define (recompile fmt . args)
  (make-tjitc-error 'recompile #f fmt args))

(define (failure name fmt . args)
  (make-tjitc-error 'failure name fmt args))
