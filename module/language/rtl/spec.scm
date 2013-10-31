;;; Register Transfer Language (RTL)

;; Copyright (C) 2013 Free Software Foundation, Inc.

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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language rtl spec)
  #:use-module (system base language)
  #:use-module (system vm objcode)
  #:use-module (ice-9 binary-ports)
  #:export (rtl))

(define (rtl->value x e opts)
  (let ((thunk (load-thunk-from-memory x)))
    (if (eq? e (current-module))
        ;; save a cons in this case
        (values (thunk) e e)
        (save-module-excursion
         (lambda ()
           (set-current-module e)
           (values (thunk) e e))))))

(define-language rtl
  #:title	"Register Transfer Language"
  #:compilers   `((value . ,rtl->value))
  #:printer	(lambda (rtl port) (put-bytevector port rtl))
  #:reader      get-bytevector-all
  #:for-humans? #f)
