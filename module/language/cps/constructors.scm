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
;;; Constructor inlining turns "list" primcalls into a series of conses,
;;; and does similar transformations for "vector".
;;;
;;; Code:

(define-module (language cps constructors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:export (inline-constructors))

(define (inline-constructors fun)
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
       ($letrec names syms (map inline-constructors funs)
                ,(visit-term body)))
      (($ $continue k ($ $primcall 'list args))
       ,(let-gensyms (kvalues val)
          (build-cps-term
            ($letk ((kvalues #f ($kargs ('val) (val)
                                  ($continue k
                                    ($primcall 'values (val))))))
              ,(let lp ((args args) (k kvalues))
                 (match args
                   (()
                    (build-cps-term
                      ($continue k ($const '()))))
                   ((arg . args)
                    (let-gensyms (ktail tail)
                      (build-cps-term
                        ($letk ((ktail #f ($kargs ('tail) (tail)
                                            ($continue k
                                              ($primcall 'cons (arg tail))))))
                          ,(lp args ktail)))))))))))
      (($ $continue k ($ $primcall 'vector args))
       ,(let-gensyms (kalloc vec len init)
          (define (initialize args n)
            (match args
              (()
               (build-cps-term
                 ($continue k ($primcall 'values (vec)))))
              ((arg . args)
               (let-gensyms (knext idx)
                 (build-cps-term
                   ($letk ((knext #f ($kargs () ()
                                       ,(initialize args (1+ n)))))
                     ($letconst (('idx idx n))
                       ($continue knext
                         ($primcall 'vector-set! (vec idx arg))))))))))
          (build-cps-term
            ($letk ((kalloc #f ($kargs ('vec) (vec)
                                 ,(initialize args 0))))
              ($letconst (('len len (length args))
                          ('init init #f))
                ($continue kalloc
                  ($primcall 'make-vector (len init))))))))
      (($ $continue k (and fun ($ $fun)))
       ($continue k ,(inline-constructors fun)))
      (($ $continue)
       ,term)))

  (rewrite-cps-exp fun
    (($ $fun meta free body)
     ($fun meta free ,(visit-cont body)))))
