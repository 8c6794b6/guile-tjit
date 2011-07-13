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
  #:export (apply
            eval
            load))

;;; Function calls. These must take care of special cases, like using
;;; symbols or raw lambda-lists as functions!

(define (apply func . args)
  (let ((real-func (cond
                    ((symbol? func)
                     (symbol-function func))
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
