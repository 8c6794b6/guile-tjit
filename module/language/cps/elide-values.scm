;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

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
;;; a VM operation, the tail of the simplification has to be a
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
                (match fun (($ $fun src meta free body) body)))))
    (define (visit-cont cont)
      (rewrite-cps-cont cont
        (($ $cont sym ($ $kargs names syms body))
         (sym ($kargs names syms ,(visit-term body))))
        (($ $cont sym ($ $kentry self tail clauses))
         (sym ($kentry self ,tail ,(map visit-cont clauses))))
        (($ $cont sym ($ $kclause arity body))
         (sym ($kclause ,arity ,(visit-cont body))))
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
        (($ $continue k src ($ $primcall 'values vals))
         ,(rewrite-cps-term (lookup-cont k conts)
            (($ $ktail)
             ($continue k src ($values vals)))
            (($ $kreceive ($ $arity req () rest () #f) kargs)
             ,(cond
               ((and (not rest) (= (length vals) (length req)))
                (build-cps-term
                 ($continue kargs src ($values vals))))
               ((and rest (>= (length vals) (length req)))
                (let-gensyms (krest rest)
                  (let ((vals* (append (list-head vals (length req))
                                       (list rest))))
                    (build-cps-term
                      ($letk ((krest ($kargs ('rest) (rest)
                                       ($continue kargs src
                                         ($values vals*)))))
                        ,(let lp ((tail (list-tail vals (length req)))
                                  (k krest))
                           (match tail
                             (()
                              (build-cps-term ($continue k src
                                                ($const '()))))
                             ((v . tail)
                              (let-gensyms (krest rest)
                                (build-cps-term
                                  ($letk ((krest ($kargs ('rest) (rest)
                                                   ($continue k src
                                                     ($primcall 'cons
                                                                (v rest))))))
                                    ,(lp tail krest))))))))))))
               (else term)))
            (($ $kargs args)
             ,(if (< (length vals) (length args))
                  term
                  (let ((vals (list-head vals (length args))))
                    (build-cps-term
                      ($continue k src ($values vals))))))))
        (($ $continue k src (and fun ($ $fun)))
         ($continue k src ,(elide-values fun)))
        (($ $continue)
         ,term)))

    (rewrite-cps-exp fun
      (($ $fun src meta free body)
       ($fun src meta free ,(visit-cont body))))))
