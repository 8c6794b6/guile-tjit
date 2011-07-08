;;; Guile Emacs Lisp

;;; Copyright (C) 2009 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA

;;; Code:

(define-module (language elisp runtime subrs)
  #:use-module (language elisp runtime)
  #:use-module (system base compile)
  #:export (symbol-value
            symbol-function
            set
            fset
            makunbound
            fmakunbound
            boundp
            fboundp
            apply
            eval
            load))

;;; Accessing symbol bindings for symbols known only at runtime.

(define (symbol-value sym)
  (reference-variable value-slot-module sym))

(define (symbol-function sym)
  (reference-variable function-slot-module sym))

(define (set sym value)
  (set-variable! value-slot-module sym value))

(define (fset sym value)
  (set-variable! function-slot-module sym value))

(define (makunbound sym)
  (if (module-bound? (resolve-interface value-slot-module) sym)
      (let ((var (module-variable (resolve-module value-slot-module)
                                  sym)))
        (if (and (variable-bound? var) (fluid? (variable-ref var)))
            (fluid-unset! (variable-ref var))
            (variable-unset! var))))
  sym)

(define (fmakunbound sym)
  (if (module-bound? (resolve-interface function-slot-module) sym)
      (let ((var (module-variable
                  (resolve-module function-slot-module)
                  sym)))
        (if (and (variable-bound? var) (fluid? (variable-ref var)))
            (fluid-unset! (variable-ref var))
            (variable-unset! var))))
  sym)

(define (boundp sym)
  (elisp-bool
   (and
    (module-bound? (resolve-interface value-slot-module) sym)
    (let ((var (module-variable (resolve-module value-slot-module)
                                sym)))
      (and (variable-bound? var)
           (if (fluid? (variable-ref var))
               (fluid-bound? (variable-ref var))
               #t))))))

(define (fboundp sym)
  (elisp-bool
   (and
    (module-bound? (resolve-interface function-slot-module) sym)
    (let* ((var (module-variable (resolve-module function-slot-module)
                                 sym)))
      (and (variable-bound? var)
           (if (fluid? (variable-ref var))
               (fluid-bound? (variable-ref var))
               #t))))))

;;; Function calls. These must take care of special cases, like using
;;; symbols or raw lambda-lists as functions!

(define (apply func . args)
  (let ((real-func (cond
                    ((symbol? func)
                     (reference-variable function-slot-module func))
                    ((list? func)
                     (if (and (prim not (null? func))
                              (eq? (prim car func) 'lambda))
                         (compile func #:from 'elisp #:to 'value)
                         (runtime-error "list is not a function"
                                        func)))
                    (else func))))
    (prim apply (@ (guile) apply) real-func args)))

;;; Miscellaneous.

(define (eval form)
  (compile form #:from 'elisp #:to 'value))

(define (load file)
  (compile-file file #:from 'elisp #:to 'value)
  #t)
