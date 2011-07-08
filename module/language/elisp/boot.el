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

(defmacro eval-and-compile (&rest body)
  `(progn
     (eval-when-compile ,@body)
     (progn ,@body)))

(eval-and-compile
  (defun funcall (function &rest arguments)
    (apply function arguments))
  (defun fset (symbol definition)
    (funcall (@ (language elisp runtime subrs) fset) symbol definition))
  (defun null (object)
    (if object nil t))
  (fset 'consp (@ (guile) pair?))
  (defun listp (object)
    (if object (consp object) t))
  (defun car (list)
    (if list (funcall (@ (guile) car) list) nil))
  (defun cdr (list)
    (if list (funcall (@ (guile) cdr) list) nil))
  (fset 'make-symbol (@ (guile) make-symbol))
  (defun signal (&rest args)
    (funcall (@ (guile) throw) 'elisp-error args)))

(defmacro lambda (&rest cdr)
  `#'(lambda ,@cdr))

(defmacro prog1 (first &rest body)
  (let ((temp (make-symbol "prog1-temp")))
    `(lexical-let ((,temp ,first))
       ,@body
       ,temp)))

(defmacro prog2 (form1 form2 &rest body)
  `(progn ,form1 (prog1 ,form2 ,@body)))

(defmacro cond (&rest clauses)
  (if (null clauses)
      nil
    (let ((first (car clauses))
          (rest (cdr clauses)))
     (if (listp first)
         (let ((condition (car first))
               (body (cdr first)))
           (if (null body)
               (let ((temp (make-symbol "cond-temp")))
                 `(lexical-let ((,temp ,condition))
                    (if ,temp
                        ,temp
                      (cond ,@rest))))
             `(if ,condition
                  (progn ,@body)
                (cond ,@rest))))
       (signal 'wrong-type-argument `(listp ,first))))))

(defmacro and (&rest conditions)
  (cond ((null conditions) t)
        ((null (cdr conditions)) (car conditions))
        (t `(if ,(car conditions)
                (and ,@(cdr conditions))
              nil))))

(defmacro or (&rest conditions)
  (cond ((null conditions) nil)
        ((null (cdr conditions)) (car conditions))
        (t (let ((temp (make-symbol "or-temp")))
             `(lexical-let ((,temp ,(car conditions)))
                (if ,temp
                    ,temp
                  (or ,@(cdr conditions))))))))

(defmacro catch (tag &rest body)
  (let* ((temp (make-symbol "catch-temp"))
         (elisp-key (make-symbol "catch-elisp-key"))
         (key (make-symbol "catch-key"))
         (value (make-symbol "catch-value")))
    `(lexical-let ((,temp ,tag))
       (funcall (@ (guile) catch)
                'elisp-exception
                #'(lambda () ,@body)
                #'(lambda (,key ,elisp-key ,value)
                    (if (eq ,elisp-key ,temp)
                        ,value
                      (funcall (@ (guile) throw)
                               ,key
                               ,elisp-key
                               ,value)))))))

(defmacro unwind-protect (bodyform &rest unwindforms)
  `(funcall (@ (guile) dynamic-wind)
            #'(lambda () nil)
            #'(lambda () ,bodyform)
            #'(lambda () ,@unwindforms)))

(fset 'symbol-value (@ (language elisp runtime subrs) symbol-value))
(fset 'symbol-function (@ (language elisp runtime subrs) symbol-function))
(fset 'set (@ (language elisp runtime subrs) set))
(fset 'makunbound (@ (language elisp runtime subrs) makunbound))
(fset 'fmakunbound (@ (language elisp runtime subrs) fmakunbound))
(fset 'boundp (@ (language elisp runtime subrs) boundp))
(fset 'fboundp (@ (language elisp runtime subrs) fboundp))
(fset 'eval (@ (language elisp runtime subrs) eval))
(fset' load (@ (language elisp runtime subrs) load))

(defun throw (tag value)
  (funcall (@ (guile) throw) 'elisp-exception tag value))

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

(fset 'not #'null)

(defun atom (object)
  (null (consp object)))

(defun nlistp (object)
  (null (listp object)))

;;; Lists

(fset 'cons (@ (guile) cons))
(fset 'list (@ (guile) list))
(fset 'make-list (@ (guile) make-list))
(fset 'append (@ (guile) append))
(fset 'reverse (@ (guile) reverse))

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
