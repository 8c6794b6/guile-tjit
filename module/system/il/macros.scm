;;; GHIL macros

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

(define-module (system il macros)
  :use-module (srfi srfi-16))

(define (make-label) (gensym ":L"))
(define (make-sym) (gensym "_"))


;;;
;;; Syntax
;;;

;; (@and X Y...) =>
;; 
;; (@if X (@and Y...) #f)
(define @and
  (case-lambda
    (() #t)
    ((x) x)
    ((x . rest) `(@if ,x (@and ,@rest) #f))))

;; (@or X Y...) =>
;; 
;; (@let ((@_ X)) (@if @_ @_ (@or Y...)))
(define @or
  (case-lambda
    (() #f)
    ((x) x)
    ((x . rest)
     (let ((sym (make-sym)))
       `(@let ((,sym ,x)) (@if ,sym ,sym (@or ,@rest)))))))

(define (@let* binds . body)
  (if (null? binds)
      `(@begin ,@body)
      `(@let (,(car binds)) (@let* ,(cdr binds) ,@body))))


;;;
;;; R5RS Procedures
;;;

;; 6. Standard procedures

;;; 6.1 Equivalence predicates

(define (@eq? x y)	`(@@ eq? ,x ,y))
;(define (@eqv? x y)	`(@@ eqv? ,x ,y))
;(define (@equal? x y)	`(@@ equal? ,x ,y))

;;; 6.2 Numbers

(define (@number? x)	`((@ Core::number?) ,x))
(define (@complex? x)	`((@ Core::complex?) ,x))
(define (@real? x)	`((@ Core::real?) ,x))
(define (@rational? x)	`((@ Core::rational?) ,x))
(define (@integer? x)	`((@ Core::integer?) ,x))

(define (@exact? x)	`((@ Core::exact?) ,x))
(define (@inexact? x)	`((@ Core::inexact?) ,x))

;(define (@= x y)	`(@@ ee? ,x ,y))
;(define (@< x y)	`(@@ lt? ,x ,y))
;(define (@> x y)	`(@@ gt? ,x ,y))
;(define (@<= x y)	`(@@ le? ,x ,y))
;(define (@>= x y)	`(@@ ge? ,x ,y))

(define @+
  (case-lambda
    (() 0)
    ((x) x)
    ((x y) `(@@ add ,x ,y))
    ((x y . rest) `(@@ add ,x (@+ ,y ,@rest)))))

(define @*
  (case-lambda
    (() 1)
    ((x) x)
    ((x y) `(@@ mul ,x ,y))
    ((x y . rest) `(@@ mul ,x (@* ,y ,@rest)))))

(define @-
  (case-lambda
    ((x) `(@@ sub 0 ,x))
    ((x y) `(@@ sub ,x ,y))
    ((x y . rest) `(@@ sub ,x (@+ ,y ,@rest)))))

(define @/
  (case-lambda
    ((x) `(@@ div 1 ,x))
    ((x y) `(@@ div ,x ,y))
    ((x y . rest) `(@@ div ,x (@* ,y ,@rest)))))

(define (@quotient x y) `(@@ quo ,x ,y))
(define (@remainder x y) `(@@ rem ,x ,y))
(define (@modulo x y) `(@@ mod ,x ,y))

;;; numerator
;;; denominator
;;; 
;;; floor
;;; ceiling
;;; truncate
;;; round
;;; 
;;; exp
;;; log
;;; sin
;;; cos
;;; tan
;;; asin
;;; acos
;;; atan
;;; 
;;; sqrt
;;; expt
;;; 
;;; make-rectangular
;;; make-polar
;;; real-part
;;; imag-part
;;; magnitude
;;; angle
;;; 
;;; exact->inexact
;;; inexact->exact
;;; 
;;; number->string
;;; string->number

;;; 6.3 Other data types

;;;; 6.3.1 Booleans

(define (@not x) `(@@ not ,x))
(define (@boolean? x) `((@ Core::boolean?) ,x))

;;;; 6.3.2 Pairs and lists

(define (@pair? x) `(@@ pair? ,x))
(define (@cons x y) `(@@ cons ,x ,y))

(define (@car x) `(@@ car ,x))
(define (@cdr x) `(@@ cdr ,x))
(define (@set-car! x y) `(@@ set-car! ,x ,y))
(define (@set-cdr! x y) `(@@ set-cdr! ,x ,y))

(define (@caar x) `(@@ car (@@ car ,x)))
(define (@cadr x) `(@@ car (@@ cdr ,x)))
(define (@cdar x) `(@@ cdr (@@ car ,x)))
(define (@cddr x) `(@@ cdr (@@ cdr ,x)))
(define (@caaar x) `(@@ car (@@ car (@@ car ,x))))
(define (@caadr x) `(@@ car (@@ car (@@ cdr ,x))))
(define (@cadar x) `(@@ car (@@ cdr (@@ car ,x))))
(define (@caddr x) `(@@ car (@@ cdr (@@ cdr ,x))))
(define (@cdaar x) `(@@ cdr (@@ car (@@ car ,x))))
(define (@cdadr x) `(@@ cdr (@@ car (@@ cdr ,x))))
(define (@cddar x) `(@@ cdr (@@ cdr (@@ car ,x))))
(define (@cdddr x) `(@@ cdr (@@ cdr (@@ cdr ,x))))
(define (@caaaar x) `(@@ car (@@ car (@@ car (@@ car ,x)))))
(define (@caaadr x) `(@@ car (@@ car (@@ car (@@ cdr ,x)))))
(define (@caadar x) `(@@ car (@@ car (@@ cdr (@@ car ,x)))))
(define (@caaddr x) `(@@ car (@@ car (@@ cdr (@@ cdr ,x)))))
(define (@cadaar x) `(@@ car (@@ cdr (@@ car (@@ car ,x)))))
(define (@cadadr x) `(@@ car (@@ cdr (@@ car (@@ cdr ,x)))))
(define (@caddar x) `(@@ car (@@ cdr (@@ cdr (@@ car ,x)))))
(define (@cadddr x) `(@@ car (@@ cdr (@@ cdr (@@ cdr ,x)))))
(define (@cdaaar x) `(@@ cdr (@@ car (@@ car (@@ car ,x)))))
(define (@cdaadr x) `(@@ cdr (@@ car (@@ car (@@ cdr ,x)))))
(define (@cdadar x) `(@@ cdr (@@ car (@@ cdr (@@ car ,x)))))
(define (@cdaddr x) `(@@ cdr (@@ car (@@ cdr (@@ cdr ,x)))))
(define (@cddaar x) `(@@ cdr (@@ cdr (@@ car (@@ car ,x)))))
(define (@cddadr x) `(@@ cdr (@@ cdr (@@ car (@@ cdr ,x)))))
(define (@cdddar x) `(@@ cdr (@@ cdr (@@ cdr (@@ car ,x)))))
(define (@cddddr x) `(@@ cdr (@@ cdr (@@ cdr (@@ cdr ,x)))))

(define (@null? x) `(@@ null? ,x))
(define (@list? x) `(@@ list? ,x))
(define (@list . args) `(@@ list ,@args))

;;; length
;;; append
;;; reverse

(define (@memq x l) `((@ Core::memq) ,x ,l))
(define (@memv x l) `((@ Core::memv) ,x ,l))
(define (@member x l) `((@ Core::member) ,x ,l))

(define (@assq x l) `((@ Core::assq) ,x ,l))
(define (@assv x l) `((@ Core::assv) ,x ,l))
(define (@assber x l) `((@ Core::assber) ,x ,l))

;;;; 6.3.3 Symbols

;;; symbol?
;;; symbol->string
;;; string->symbol

;;;; 6.3.4 Characters

;;; char?
;;; char=?
;;; char<?
;;; char>?
;;; char<=?
;;; char>=?
;;; char->integer
;;; integer->char

;;;; 6.3.5 Strings

;;; string?
;;; make-string
;;; string-length
;;; string-ref
;;; string-set!

;;;; 6.3.6 Vectors

;;; vector?
;;; make-vector
;;; vector-length
;;; vector-ref
;;; vector-set!

;;;; 6.4 Control features

(define (@procedure? x) `((@ Core::procedure?) ,x))

(define (@apply proc . args) `(@@ apply ,proc ,@args))

;;; (define (@force promise) `(@@ force promise))

;;; (define (@call/cc proc) `(@@ call/cc proc))

;;; values
;;; call-with-values
;;; dynamic-wind

;;; 6.5 Eval

;;; 6.6 Input and output

;;;; 6.6.1 Ports

;;; input-port?
;;; output-port?
;;; current-input-port
;;; current-output-port
;;; 
;;; open-input-file
;;; open-output-file
;;; close-input-port
;;; close-output-port

;;;; 6.6.2 Input

;;; read
;;; read-char
;;; peek-char
;;; eof-object?
;;; char-ready?

;;;; 6.6.3 Output

;;; write
;;; display
;;; newline
;;; write-char

;;;; 6.6.4 System interface


;;;
;;; Non-R5RS Procedures
;;;

(define @cons*
  (case-lambda
    ((x) x)
    ((x y) `(@cons ,x ,y))
    ((x y . rest) `(@cons ,x (@cons* ,y ,@rest)))))
