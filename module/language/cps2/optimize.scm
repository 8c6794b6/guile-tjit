;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; Optimizations on CPS2.
;;;
;;; Code:

(define-module (language cps2 optimize)
  #:use-module (ice-9 match)
  #:use-module (language cps2 simplify)
  #:export (optimize))

(define (kw-arg-ref args kw default)
  (match (memq kw args)
    ((_ val . _) val)
    (_ default)))

(define (optimize program opts)
  (define (run-pass! pass kw default)
    (set! program
          (if (kw-arg-ref opts kw default)
              (pass program)
              program)))

  ;; This series of assignments to `env' used to be a series of let*
  ;; bindings of `env', as you would imagine.  In compiled code this is
  ;; fine because the compiler is able to allocate all let*-bound
  ;; variable to the same slot, which also means that the garbage
  ;; collector doesn't have to retain so many copies of the term being
  ;; optimized.  However during bootstrap, the interpreter doesn't do
  ;; this optimization, leading to excessive data retention as the terms
  ;; are rewritten.  To marginally improve bootstrap memory usage, here
  ;; we use set! instead.  The compiler should produce the same code in
  ;; any case, though currently it does not because it doesn't do escape
  ;; analysis on the box created for the set!.

  (run-pass! simplify #:simplify? #t)

  program)