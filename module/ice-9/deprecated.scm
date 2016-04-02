;;;; Copyright (C) 2003, 2005, 2006, 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;

(define-module (ice-9 deprecated)
  #:export (_IONBF _IOLBF _IOFBF))

(define-syntax-rule (define-deprecated var msg exp)
  (define-syntax var
    (lambda (x)
      (issue-deprecation-warning msg)
      (syntax-case x ()
        (id (identifier? #'id) #'exp)))))

(define-deprecated _IONBF
  "`_IONBF' is deprecated.  Use the symbol 'none instead."
  'none)
(define-deprecated _IOLBF
  "`_IOLBF' is deprecated.  Use the symbol 'line instead."
  'line)
(define-deprecated _IOFBF
  "`_IOFBF' is deprecated.  Use the symbol 'block instead."
  'block)
