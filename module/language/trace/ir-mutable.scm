;;;; ANF IR for mutable top-level bindings

;;;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.
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
;;;
;;; Module containing ANF IR definitions for mutable top-level binding
;;; operations.
;;;
;;; There is no need to worry about resovling variable in `toplevel-box' and
;;; `module-box', since the variables are already resolved at the time of
;;; recording the trace.
;;;
;;; Code:

(define-module (language trace ir-mutable)
  #:use-module (system foreign)
  #:use-module (language trace error)
  #:use-module (language trace ir)
  #:use-module (language trace env)
  #:use-module (language trace primitives)
  #:use-module (language trace snapshot)
  #:use-module (language trace types))


;; XXX: current-module
;; XXX: resolve
;; XXX: define!

(define-constant (toplevel-box var-offset mod-offset sym-offset bound?)
  (let ((var (dereference-scm (+ ip (* var-offset 4)))))
    (object-address var)))

(define-constant (module-box var-offset mod-offset sym-offset bound?)
  (let ((var (dereference-scm (+ ip (* var-offset 4)))))
    (object-address var)))
