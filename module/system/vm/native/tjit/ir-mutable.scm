;;; ANF IR for mutable top-level bindings

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
;;; Code:

(define-module (system vm native tjit ir-mutable)
  #:use-module (system foreign)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot))

;; XXX: current-module
;; XXX: resolve
;; XXX: define!

(define-ir (toplevel-box (box! dst) (const var-offset) (const mod-offset)
                         (const sym-offset) (const bound?))
  (let ((vdst (var-ref dst))
        (var (dereference-scm (+ ip (* var-offset 4))))
        (dst/i+sp-offset (+ dst (current-sp-offset)))
        (live-indices (env-live-indices env)))
    (if (variable? var)
        `(let ((,vdst ,(pointer-address (scm->pointer var))))
           ,(next))
        (nyi "toplevel-box: not a variable ~s" var))))

(define-ir (module-box (box! dst) (const var-offset) (const mod-offset)
                       (const sym-offset) (const bound?))
  (let ((vdst (var-ref dst))
        (var (dereference-scm (+ ip (* var-offset 4))))
        (dst/i+sp-offset (+ dst (current-sp-offset)))
        (live-indices (env-live-indices env)))
    (if (variable? var)
        `(let ((,vdst ,(pointer-address (scm->pointer var))))
           ,(next))
        (nyi "module-box: not a variable ~s" var))))
