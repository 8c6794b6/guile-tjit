;;; Guile Emacs Lisp

;;; Copyright (C) 2011 Free Software Foundation, Inc.

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

(defmacro @ (module symbol)
  `(guile-ref ,module ,symbol))

(defun funcall (function &rest arguments)
  (apply function arguments))

(defun fset (symbol definition)
  (funcall (@ (language elisp runtime subrs) fset) symbol definition))

(fset 'symbol-value (@ (language elisp runtime subrs) symbol-value))
(fset 'symbol-function (@ (language elisp runtime subrs) symbol-function))
(fset 'set (@ (language elisp runtime subrs) set))
(fset 'makunbound (@ (language elisp runtime subrs) makunbound))
(fset 'fmakunbound (@ (language elisp runtime subrs) fmakunbound))
(fset 'boundp (@ (language elisp runtime subrs) boundp))
(fset 'fboundp (@ (language elisp runtime subrs) fboundp))
(fset 'throw (@ (language elisp runtime subrs) throw))
(fset 'eval (@ (language elisp runtime subrs) eval))
(fset' load (@ (language elisp runtime subrs) load))

;;; Equality predicates

(fset 'eq (@ (guile) eq?))
(fset 'equal (@ (guile) equal?))

;;; Numerical type predicates

(defun floatp (object)
  (and (funcall (@ (guile) real?) object)
       (or (funcall (@ (guile) inexact?) object)
           (null (funcall (@ (guile) integer?) object)))))

(defun integerp (object)
  (and (funcall (@ (guile) exact?) object)
       (funcall (@ (guile) integer?) object)))

(defun numberp (object)
  (funcall (@ (guile) real?) object))

(defun wholenump (object)
  (and (funcall (@ (guile) exact?) object)
       (funcall (@ (guile) integer?) object)
       (>= object 0)))

(defun zerop (object)
  (= object 0))

;;; Numerical comparisons

(fset '= (@ (guile) =))

(defun /= (num1 num2)
  (null (= num1 num2)))

(fset '< (@ (guile) <))
(fset '<= (@ (guile) <=))
(fset '> (@ (guile) >))
(fset '>= (@ (guile) >=))

(defun max (&rest numbers)
  (apply (@ (guile) max) numbers))

(defun min (&rest numbers)
  (apply (@ (guile) min) numbers))

;;; Arithmetic functions

(fset '1+ (@ (guile) 1+))
(fset '1- (@ (guile) 1-))
(fset '+ (@ (guile) +))
(fset '- (@ (guile) -))
(fset '* (@ (guile) *))
(fset '% (@ (guile) modulo))
(fset 'abs (@ (guile) abs))

;;; Floating-point rounding

(fset 'ffloor (@ (guile) floor))
(fset 'fceiling (@ (guile) ceiling))
(fset 'ftruncate (@ (guile) truncate))
(fset 'fround (@ (guile) round))

;;; Numeric conversion

(defun float (arg)
  (if (numberp arg)
      (funcall (@ (guile) exact->inexact) arg)
    (signal 'wrong-type-argument `(numberp ,arg))))

;;; List predicates

(fset 'consp (@ (guile) pair?))

(defun null (object)
  (if object nil t))

(fset 'not #'null)

(defun atom (object)
  (null (consp object)))

(defun listp (object)
  (or (consp object) (null object)))

(defun nlistp (object)
  (null (listp object)))

;;; Lists

(fset 'cons (@ (guile) cons))
(fset 'list (@ (guile) list))
(fset 'make-list (@ (guile) make-list))
(fset 'append (@ (guile) append))
(fset 'reverse (@ (guile) reverse))

(defun car (list)
  (if (null list)
      nil
    (funcall (@ (guile) car) list)))

(defun cdr (list)
  (if (null list)
      nil
    (funcall (@ (guile) cdr) list)))

(defun car-safe (object)
  (if (consp object)
      (car object)
    nil))

(defun cdr-safe (object)
  (if (consp object)
      (cdr object)
    nil))

(defun setcar (cell newcar)
  (if (consp cell)
      (progn
        (funcall (@ (guile) set-car!) cell newcar)
        newcar)
    (signal 'wrong-type-argument `(consp ,cell))))

(defun setcdr (cell newcdr)
  (if (consp cell)
      (progn
        (funcall (@ (guile) set-cdr!) cell newcdr)
        newcdr)
    (signal 'wrong-type-argument `(consp ,cell))))

(defun nthcdr (n list)
  (let ((i 0))
    (while (< i n)
      (setq list (cdr list)
            i (+ i 1)))
    list))

(defun nth (n list)
  (car (nthcdr n list)))

;;; Strings

(defun string (&rest characters)
  (funcall (@ (guile) list->string)
           (mapcar (@ (guile) integer->char) characters)))

;;; Sequences

(fset 'length (@ (guile) length))

(defun mapcar (function sequence)
  (funcall (@ (guile) map) function sequence))
