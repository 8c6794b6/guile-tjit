;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA
;;;;

;;; Commentary:

;;; Primitives used for vm-tjit engine.  Primitives defined here are used during
;;; compilation from traced data to native code only, and could not be called as
;;; ordinary scheme procedure.

;;; Code:

(define-module (system vm native tjit primitives)
  #:use-module (ice-9 match)
  #:use-module (language tree-il primitives)
  #:use-module (language cps2)
  #:export (initialize-tjit-primitives))

(define *native-primitives*
  '((%frame-set! . (0 . 2))
    (%fxadd . (1 . 2))
    (%fxadd1 . (1 . 1))
    (%fxsub . (1 . 2))
    (%fxsub1 . (1 . 1))))

(define (initialize-tjit-primitives)
  (define (add-cps-primitive! name arity)
    (module-add! the-root-module name (make-variable #f))
    (add-interesting-primitive! name)
    (hashq-set! (force (@@ (language cps primitives) *prim-instructions*))
                name name)
    (hashq-set! (@@ (language cps primitives) *prim-arities*)
                name arity))
  (for-each (match-lambda
             ((name . arity)
              (add-cps-primitive! name arity)))
            *native-primitives*))
