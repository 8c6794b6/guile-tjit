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
;;; Primcalls that don't correspond to VM instructions are treated as if
;;; they are calls, and indeed the later reify-primitives pass turns
;;; them into calls.  Because no return arity checking is done for these
;;; primitives, if a later optimization pass simplifies the primcall to
;;; an RTL operation, the tail of the simplification has to be a
;;; primcall to 'values.  Most of these primcalls can be elided, and
;;; that is the job of this pass.
;;;
;;; Code:

(define-module (language cps elide-values)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:export (elide-values))

(define (elide-values fun)
  (let ((conts (build-local-cont-table
                (match fun (($ $fun meta free body) body)))))
    (define (visit-cont cont)
      (rewrite-cps-cont cont
        (($ $cont sym src ($ $kargs names syms body))
         (sym src ($kargs names syms ,(visit-term body))))
        (($ $cont sym src ($ $kentry self tail clauses))
         (sym src ($kentry self ,tail ,(map visit-cont clauses))))
        (($ $cont sym src ($ $kclause arity body))
         (sym src ($kclause ,arity ,(visit-cont body))))
        (($ $cont)
         ,cont)))
    (define (visit-term term)
      (rewrite-cps-term term
        (($ $letk conts body)
         ($letk ,(map visit-cont conts)
           ,(visit-term body)))
        (($ $letrec names syms funs body)
         ($letrec names syms (map elide-values funs)
                  ,(visit-term body)))
        (($ $continue k ($ $primcall 'values vals))
         ,(rewrite-cps-term (lookup-cont k conts)
            (($ $ktail)
             ($continue k ($values vals)))
            (($ $ktrunc ($ $arity req () rest () #f) kargs)
             ,(if (or rest (< (length vals) (length req)))
                  term
                  (let ((vals (list-head vals (length req))))
                    (build-cps-term
                      ($continue kargs ($values vals))))))
            (($ $kargs args)
             ,(if (< (length vals) (length args))
                  term
                  (let ((vals (list-head vals (length args))))
                    (build-cps-term
                      ($continue k ($values vals))))))))
        (($ $continue k (and fun ($ $fun)))
         ($continue k ,(elide-values fun)))
        (($ $continue)
         ,term)))

    (rewrite-cps-exp fun
      (($ $fun meta free body)
       ($fun meta free ,(visit-cont body))))))
