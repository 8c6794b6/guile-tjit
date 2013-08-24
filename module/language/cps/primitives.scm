;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013 Free Software Foundation, Inc.

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
;;; Information about named primitives, as they appear in $prim and $primcall.
;;;
;;; Code:

(define-module (language cps primitives)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-26)
  #:use-module (language rtl)
  #:export (prim-rtl-instruction
            branching-primitive?
            prim-arity
            ))

(define *rtl-instruction-aliases*
  '((+ . add) (1+ . add1)
    (- . sub) (1- . sub1)
    (* . mul) (/ . div)
    (quotient . quo) (remainder . rem)
    (modulo . mod)
    (define! . define)
    (vector-set! . vector-set)))

(define *macro-instruction-arities*
  '((cache-current-module! . (0 . 2))
    (cached-toplevel-box . (1 . 3))
    (cached-module-box . (1 . 4))))

(define *branching-primcall-arities*
  '((null? . (1 . 1))
    (nil? . (1 . 1))
    (pair? . (1 . 1))
    (struct? . (1 . 1))
    (char? . (1 . 1))
    (eq? . (1 . 2))
    (eqv? . (1 . 2))
    (equal? . (1 . 2))
    (= . (1 . 2))
    (< . (1 . 2))
    (> . (1 . 2))
    (<= . (1 . 2))
    (>= . (1 . 2))))

(define (compute-prim-rtl-instructions)
  (let ((table (make-hash-table)))
    (for-each
     (match-lambda ((inst . _) (hashq-set! table inst inst)))
     (rtl-instruction-list))
    (for-each
     (match-lambda ((prim . inst) (hashq-set! table prim inst)))
     *rtl-instruction-aliases*)
    (for-each
     (match-lambda ((inst . arity) (hashq-set! table inst inst)))
     *macro-instruction-arities*)
    table))

(define *prim-rtl-instructions* (delay (compute-prim-rtl-instructions)))

;; prim -> rtl-instruction | #f
(define (prim-rtl-instruction name)
  (hashq-ref (force *prim-rtl-instructions*) name))

(define (branching-primitive? name)
  (and (assq name *branching-primcall-arities*) #t))

(define *prim-arities* (make-hash-table))

(define (prim-arity name)
  (or (hashq-ref *prim-arities* name)
      (let ((arity (cond
                    ((prim-rtl-instruction name) => rtl-instruction-arity)
                    ((assq name *branching-primcall-arities*) => cdr)
                    (else
                     (error "Primitive of unknown arity" name)))))
        (hashq-set! *prim-arities* name arity)
        arity)))
