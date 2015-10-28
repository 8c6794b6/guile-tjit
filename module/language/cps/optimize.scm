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
;;; Optimizations on CPS.
;;;
;;; Code:

(define-module (language cps optimize)
  #:use-module (ice-9 match)
  #:use-module (language cps verify)
  #:export (optimize-higher-order-cps
            optimize-first-order-cps
            cps-default-optimization-options))

(define (kw-arg-ref args kw default)
  (match (memq kw args)
    ((_ val . _) val)
    (_ default)))

(define *debug?* #f)

(define (maybe-verify program)
  (if *debug?*
      (verify program)
      program))

(define-syntax-rule (define-optimizer optimize ((@ mod pass) kw default) ...)
  (define* (optimize program #:optional (opts '()))
    ;; This series of assignments to `program' used to be a series of
    ;; let* bindings of `program', as you would imagine.  In compiled
    ;; code this is fine because the compiler is able to allocate all
    ;; let*-bound variable to the same slot, which also means that the
    ;; garbage collector doesn't have to retain so many copies of the
    ;; term being optimized.  However during bootstrap, the interpreter
    ;; doesn't do this optimization, leading to excessive data retention
    ;; as the terms are rewritten.  To marginally improve bootstrap
    ;; memory usage, here we use set! instead.  The compiler should
    ;; produce the same code in any case, though currently it does not
    ;; because it doesn't do escape analysis on the box created for the
    ;; set!.
    (maybe-verify program)
    (set! program
      (if (kw-arg-ref opts kw default)
          (maybe-verify ((module-ref (resolve-interface 'mod) 'pass) program))
          program))
    ...
    (maybe-verify program)))

;; Passes that are needed:
;;
;;  * Abort contification: turning abort primcalls into continuation
;;    calls, and eliding prompts if possible.
;;
(define-optimizer optimize-higher-order-cps
  ;; FIXME: split-rec call temporarily moved to compile-bytecode and run
  ;; unconditionally, because closure conversion requires it.  Move the
  ;; pass back here when that's fixed.
  ;;
  ;; (split-rec #:split-rec? #t)
  ((@ (language cps dce) eliminate-dead-code)
   #:eliminate-dead-code? #t)
  ((@ (language cps prune-top-level-scopes) prune-top-level-scopes)
   #:prune-top-level-scopes? #t)
  ((@ (language cps simplify) simplify)
   #:simplify? #t)
  ((@ (language cps contification) contify)
   #:contify? #t)
  ((@ (language cps constructors) inline-constructors)
   #:inline-constructors? #t)
  ((@ (language cps specialize-primcalls) specialize-primcalls)
   #:specialize-primcalls? #t)
  ((@ (language cps elide-values) elide-values)
   #:elide-values? #t)
  ((@ (language cps prune-bailouts) prune-bailouts)
   #:prune-bailouts? #t)
  ((@ (language cps peel-loops) peel-loops)
   #:peel-loops? #t)
  ((@ (language cps cse) eliminate-common-subexpressions)
   #:cse? #t)
  ((@ (language cps type-fold) type-fold)
   #:type-fold? #t)
  ((@ (language cps self-references) resolve-self-references)
   #:resolve-self-references? #t)
  ((@ (language cps dce) eliminate-dead-code)
   #:eliminate-dead-code? #t)
  ((@ (language cps simplify) simplify)
   #:simplify? #t))

(define-optimizer optimize-first-order-cps
  ((@ (language cps licm) hoist-loop-invariant-code)
   #:licm? #t)
  ;; FIXME: CSE here to eliminate duplicate free-ref terms.
  ((@ (language cps dce) eliminate-dead-code)
   #:eliminate-dead-code? #t)
  ((@ (language cps rotate-loops) rotate-loops)
   #:rotate-loops? #t)
  ((@ (language cps simplify) simplify)
   #:simplify? #t))

(define (cps-default-optimization-options)
  (list ;; #:split-rec? #t
   #:eliminate-dead-code? #t
   #:prune-top-level-scopes? #t
   #:contify? #t
   #:inline-constructors? #t
   #:specialize-primcalls? #t
   #:elide-values? #t
   #:prune-bailouts? #t
   #:peel-loops? #t
   #:cse? #t
   #:type-fold? #t
   #:resolve-self-references? #t
   #:licm? #t
   #:rotate-loops? #t))
