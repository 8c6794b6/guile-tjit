;;; Continuation-passing style (CPS) intermediate language (IL)

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

(define-module (language cps spec)
  #:use-module (system base language)
  #:use-module (language cps)
  #:export (cps))

(define* (write-cps exp #:optional (port (current-output-port)))
  (write (unparse-cps exp) port))

(define-language cps
  #:title	"CPS Intermediate Language"
  #:reader	(lambda (port env) (read port))
  #:printer	write-cps
  #:parser      parse-cps
  #:compilers   '()
  #:for-humans? #f
  )
