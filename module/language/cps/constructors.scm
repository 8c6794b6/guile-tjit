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
;;; Constructor inlining turns "list" primcalls into a series of conses,
;;; and does similar transformations for "vector".
;;;
;;; Code:

(define-module (language cps constructors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:export (inline-constructors))

(define (inline-constructors* fun)
  (define (visit-cont cont)
    (rewrite-cps-cont cont
      (($ $cont sym ($ $kargs names syms body))
       (sym ($kargs names syms ,(visit-term body))))
      (($ $cont sym ($ $kentry self tail clause))
       (sym ($kentry self ,tail ,(and clause (visit-cont clause)))))
      (($ $cont sym ($ $kclause arity body alternate))
       (sym ($kclause ,arity ,(visit-cont body)
                      ,(and alternate (visit-cont alternate)))))
      (($ $cont)
       ,cont)))
  (define (visit-term term)
    (rewrite-cps-term term
      (($ $letk conts body)
       ($letk ,(map visit-cont conts)
         ,(visit-term body)))
      (($ $letrec names syms funs body)
       ($letrec names syms (map inline-constructors* funs)
                ,(visit-term body)))
      (($ $continue k src ($ $primcall 'list args))
       ,(let-fresh (kvalues) (val)
          (build-cps-term
            ($letk ((kvalues ($kargs ('val) (val)
                               ($continue k src
                                 ($primcall 'values (val))))))
              ,(let lp ((args args) (k kvalues))
                 (match args
                   (()
                    (build-cps-term
                      ($continue k src ($const '()))))
                   ((arg . args)
                    (let-fresh (ktail) (tail)
                      (build-cps-term
                        ($letk ((ktail ($kargs ('tail) (tail)
                                         ($continue k src
                                           ($primcall 'cons (arg tail))))))
                          ,(lp args ktail)))))))))))
      (($ $continue k src ($ $primcall 'vector args))
       ,(let-fresh (kalloc) (vec len init)
          (define (initialize args n)
            (match args
              (()
               (build-cps-term
                 ($continue k src ($primcall 'values (vec)))))
              ((arg . args)
               (let-fresh (knext) (idx)
                 (build-cps-term
                   ($letk ((knext ($kargs () ()
                                    ,(initialize args (1+ n)))))
                     ($letconst (('idx idx n))
                       ($continue knext src
                         ($primcall 'vector-set! (vec idx arg))))))))))
          (build-cps-term
            ($letk ((kalloc ($kargs ('vec) (vec)
                              ,(initialize args 0))))
              ($letconst (('len len (length args))
                          ('init init #f))
                ($continue kalloc src
                  ($primcall 'make-vector (len init))))))))
      (($ $continue k src (and fun ($ $fun)))
       ($continue k src ,(inline-constructors* fun)))
      (($ $continue)
       ,term)))

  (rewrite-cps-exp fun
    (($ $fun src meta free body)
     ($fun src meta free ,(visit-cont body)))))

(define (inline-constructors fun)
  (with-fresh-name-state fun
    (inline-constructors* fun)))
