;;; Guile Emacs Lisp

;;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language elisp runtime macros)
  #:use-module (language elisp runtime))

;;; This module contains the macro definitions of elisp symbols.  In
;;; contrast to the other runtime modules, those are used directly
;;; during compilation, of course, so not really in runtime.  But I
;;; think it fits well to the others here.
 
(built-in-macro lambda
  (lambda cdr
    `(function (lambda ,@cdr))))

;;; The prog1 and prog2 constructs can easily be defined as macros using
;;; progn and some lexical-let's to save the intermediate value to
;;; return at the end.

(built-in-macro prog1
  (lambda (form1 . rest)
    (let ((temp (gensym)))
      `(lexical-let ((,temp ,form1))
         ,@rest
         ,temp))))

(built-in-macro prog2
  (lambda (form1 form2 . rest)
    `(progn ,form1 (prog1 ,form2 ,@rest))))

;;; Impement the cond form as nested if's.  A special case is a
;;; (condition) subform, in which case we need to return the condition
;;; itself if it is true and thus save it in a local variable before
;;; testing it.

(built-in-macro cond
  (lambda (. clauses)
    (let iterate ((tail clauses))
      (if (null? tail)
          'nil
          (let ((cur (car tail))
                (rest (iterate (cdr tail))))
            (prim cond
                  ((prim or (not (list? cur)) (null? cur))
                   (macro-error "invalid clause in cond" cur))
                  ((null? (cdr cur))
                   (let ((var (gensym)))
                     `(lexical-let ((,var ,(car cur)))
                        (if ,var
                            ,var
                            ,rest))))
                  (else
                   `(if ,(car cur)
                        (progn ,@(cdr cur))
                        ,rest))))))))

;;; The and and or forms can also be easily defined with macros.

(built-in-macro and
  (case-lambda
    (() 't)
    ((x) x)
    ((x . args)
     (let iterate ((x x) (tail args))
       (if (null? tail)
           x
           `(if ,x
                ,(iterate (car tail) (cdr tail))
                nil))))))

(built-in-macro or
  (case-lambda
    (() 'nil)
    ((x) x)
    ((x . args)
     (let iterate ((x x) (tail args))
       (if (null? tail)
           x
           (let ((var (gensym)))
             `(lexical-let ((,var ,x))
                (if ,var
                    ,var
                    ,(iterate (car tail) (cdr tail))))))))))

;;; Exception handling.  unwind-protect and catch are implemented as
;;; macros (throw is a built-in function).

;;; catch and throw can mainly be implemented directly using Guile's
;;; primitives for exceptions, the only difficulty is that the keys used
;;; within Guile must be symbols, while elisp allows any value and
;;; checks for matches using eq (eq?).  We handle this by using always #t
;;; as key for the Guile primitives and check for matches inside the
;;; handler; if the elisp keys are not eq?, we rethrow the exception.

(built-in-macro catch
  (lambda (tag . body)
    (if (null? body)
        (macro-error "catch with empty body"))
    (let ((tagsym (gensym)))
      `(lexical-let ((,tagsym ,tag))
         ((guile-primitive catch)
          #t
          (lambda () ,@body)
          ,(let* ((dummy-key (gensym))
                  (elisp-key (gensym))
                  (value (gensym))
                  (arglist `(,dummy-key ,elisp-key ,value)))
             `(with-always-lexical
               ,arglist
               (lambda ,arglist
                 (if (eq ,elisp-key ,tagsym)
                     ,value
                     ((guile-primitive throw) ,dummy-key ,elisp-key
                      ,value))))))))))

;;; unwind-protect is just some weaker construct as dynamic-wind, so
;;; straight-forward to implement.

(built-in-macro unwind-protect
  (lambda (body . clean-ups)
    (if (null? clean-ups)
        (macro-error "unwind-protect without cleanup code"))
    `((guile-primitive dynamic-wind)
      (lambda () nil)
      (lambda () ,body)
      (lambda () ,@clean-ups))))

