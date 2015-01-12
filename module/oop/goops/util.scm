;;;; 	Copyright (C) 1999, 2000, 2001, 2003, 2006, 2008, 2012, 2015 Free Software Foundation, Inc.
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


(define-module (oop goops util)
  #:export (map* for-each* length*))

;;;
;;; {Utilities}
;;;

(define (map* fn . l) 		; A map which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (cons (apply fn      (map car l))
			  (apply map* fn (map cdr l))))
   (else            (apply fn l))))

(define (for-each* fn . l) 	; A for-each which accepts dotted lists (arg lists  
  (cond 			; must be "isomorph"
   ((null? (car l)) '())
   ((pair? (car l)) (apply fn (map car l)) (apply for-each* fn (map cdr l)))
   (else            (apply fn l))))

(define (length* ls)
  (do ((n 0 (+ 1 n))
       (ls ls (cdr ls)))
      ((not (pair? ls)) n)))
