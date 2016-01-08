;;; ANF IR for function prologues

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
;;; Module containing ANF IR definitions for function prologue operations.
;;;
;;; Code:

(define-module (system vm native tjit ir-prologue)
  #:use-module (system foreign)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables))

(define-syntax-rule (expand-stack nlocals)
  (expand-outline (ir-outline ir) (current-sp-offset) nlocals))

;; XXX: br-if-nargs-ne
;; XXX: br-if-nargs-lt
;; XXX; br-if-nargs-gt

(define-ir (assert-nargs-ee (const expected))
  ;; XXX: Unless this op was found at entry of down recursion, nothing to do.
  ;; Detect entry of down recursion, emit assertion in native code.
  (next))

(define-ir (assert-nargs-ge (const expected))
  ;; XXX: Same as assert-nargs-ee
  (next))

(define-ir (assert-nargs-le (const expected))
  ;; XXX: Same as assert-nargs-ee
  (next))

(define-ir (alloc-frame nlocals)
  (let ((stack-size (vector-length locals))
        (undefined (pointer->scm (make-pointer #x904))))
    (if (< stack-size nlocals)
        (begin
          (expand-stack (- nlocals stack-size))
          (let lp ((n 0))
            (if (< n (- nlocals stack-size))
                `(let ((,(var-ref (- n)) ,undefined))
                   ,(lp (+ n 1)))
                (next))))
        (next))))

(define-ir (reset-frame nlocals)
  (next))

(define-ir (assert-nargs-ee/locals expected nlocals)
  (let* ((stack-size (vector-length locals))
         (undefined (pointer->scm (make-pointer #x904))))
    (expand-stack nlocals)
    (let lp ((n nlocals))
      (if (< 0 n)
          `(let ((,(var-ref (- n 1)) ,undefined))
             ,(lp (- n 1)))
          (next)))))

;; XXX: br-if-npos-gt
;; XXX: bind-kw-args
;; XXX: bind-rest
