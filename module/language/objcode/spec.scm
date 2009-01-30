;;; Guile Lowlevel Intermediate Language

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (language objcode spec)
  #:use-module (system base language)
  #:use-module (system vm objcode)
  #:use-module (system vm program)
  #:export (objcode make-objcode-env))

(define (make-objcode-env module externals)
  (cons module externals))

(define (objcode-env-module env)
  (if env (car env) (current-module)))

(define (objcode-env-externals env)
  (if env (cdr env) '()))

(define (objcode->value x e opts)
  (let ((thunk (make-program x #f (objcode-env-externals e))))
    (if e
        (save-module-excursion
         (lambda ()
           (set-current-module (objcode-env-module e))
           (values (thunk) #f)))
        (values (thunk) #f))))

(define-language objcode
  #:title	"Guile Object Code"
  #:version	"0.3"
  #:reader	#f
  #:printer	write-objcode
  #:compilers   `((value . ,objcode->value))
  )
