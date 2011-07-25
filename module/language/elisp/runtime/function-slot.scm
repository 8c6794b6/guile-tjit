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

(define-module (language elisp runtime function-slot)
  #:use-module ((language elisp runtime subrs)
                #:select (apply))
  #:use-module ((language elisp compile-tree-il)
                #:select
                ((compile-progn . progn)
                 (compile-eval-when-compile . eval-when-compile)
                 (compile-if . if)
                 (compile-defconst . defconst)
                 (compile-defvar . defvar)
                 (compile-setq . setq)
                 (compile-let . let)
                 (compile-lexical-let . lexical-let)
                 (compile-flet . flet)
                 (compile-let* . let*)
                 (compile-lexical-let* . lexical-let*)
                 (compile-guile-ref . guile-ref)
                 (compile-guile-primitive . guile-primitive)
                 (compile-while . while)
                 (compile-function . function)
                 (compile-defun . defun)
                 (compile-defmacro . defmacro)
                 (#{compile-`}# . #{`}#)
                 (compile-quote . quote)
                 (compile-%funcall . %funcall)
                 (compile-%set-lexical-binding-mode
                  . %set-lexical-binding-mode)))
  #:duplicates (last)
  ;; special operators
  #:re-export (progn
               eval-when-compile
               if
               defconst
               defvar
               setq
               let
               lexical-let
               flet
               let*
               lexical-let*
               guile-ref
               guile-primitive
               while
               function
               defun
               defmacro
               #{`}#
               quote
               %funcall
               %set-lexical-binding-mode)
  ;; functions
  #:re-export (apply)
  #:pure)
