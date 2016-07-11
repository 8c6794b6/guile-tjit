;;;; Traced bytecode instructions

;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; Language definition for compiling traced bytecode instruction to fragment
;;; data used by tracing JIT VM engine.
;;;
;;; Code:

(define-module (language trace spec)
  #:use-module (system base language)
  #:use-module (language trace compile-fragment)
  #:export (trace))

(define-language trace
  #:title "Traced Bytecode Instructions"
  #:reader #f
  #:compilers `((value . ,compile-fragment))
  #:printer write
  #:for-humans? #f)
