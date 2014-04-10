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
;;; The fundamental lambda calculus reductions, like beta and eta
;;; reduction and so on.  Pretty lame currently.
;;;
;;; Code:

(define-module (language cps simplify)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (language cps renumber)
  #:export (simplify))

(define (compute-eta-reductions fun)
  (let ((table (make-hash-table)))
    (define (visit-cont cont)
      (match cont
        (($ $cont sym ($ $kargs names syms body))
         (visit-term body sym syms))
        (($ $cont sym ($ $kfun src meta self tail clause))
         (when clause (visit-cont clause)))
        (($ $cont sym ($ $kclause arity body alternate))
         (visit-cont body)
         (when alternate (visit-cont alternate)))
        (($ $cont sym _) #f)))
    (define (visit-term term term-k term-args)
      (match term
        (($ $letk conts body)
         (for-each visit-cont conts)
         (visit-term body term-k term-args))
        (($ $letrec names syms funs body)
         (for-each visit-fun funs)
         (visit-term body term-k term-args))
        (($ $continue k src ($ $values args))
         (when (and (equal? term-args args) (not (eq? k term-k)))
           (hashq-set! table term-k k)))
        (($ $continue k src (and fun ($ $fun)))
         (visit-fun fun))
        (($ $continue k src _)
         #f)))
    (define (visit-fun fun)
      (match fun
        (($ $fun free body)
         (visit-cont body))))
    (visit-fun fun)
    table))

(define (eta-reduce fun)
  (let ((table (compute-eta-reductions fun))
        (dfg (compute-dfg fun)))
    (define (reduce* k scope values?)
      (match (hashq-ref table k)
        (#f k)
        (k* 
         (if (and (continuation-bound-in? k* scope dfg)
                  (or values?
                      (match (lookup-cont k* dfg)
                        (($ $kargs) #t)
                        (_ #f))))
             (reduce* k* scope values?)
             k))))
    (define (reduce k scope)
      (reduce* k scope #f))
    (define (reduce-values k scope)
      (reduce* k scope #t))
    (define (visit-cont cont scope)
      (rewrite-cps-cont cont
        (($ $cont sym ($ $kargs names syms body))
         (sym ($kargs names syms ,(visit-term body sym))))
        (($ $cont sym ($ $kfun src meta self tail clause))
         (sym ($kfun src meta self ,tail
                ,(and clause (visit-cont clause sym)))))
        (($ $cont sym ($ $kclause arity body alternate))
         (sym ($kclause ,arity ,(visit-cont body sym)
                        ,(and alternate (visit-cont alternate sym)))))
        (($ $cont sym ($ $kreceive ($ $arity req () rest () #f) kargs))
         (sym ($kreceive req rest (reduce kargs scope))))
        (($ $cont sym ($ $kif kt kf))
         (sym ($kif (reduce kt scope) (reduce kf scope))))))
    (define (visit-term term scope)
      (rewrite-cps-term term
        (($ $letk conts body)
         ($letk ,(map (cut visit-cont <> scope) conts)
           ,(visit-term body scope)))
        (($ $letrec names syms funs body)
         ($letrec names syms (map visit-fun funs)
                  ,(visit-term body scope)))
        (($ $continue k src ($ $values args))
         ($continue (reduce-values k scope) src ($values args)))
        (($ $continue k src (and fun ($ $fun)))
         ($continue (reduce k scope) src ,(visit-fun fun)))
        (($ $continue k src exp)
         ($continue (reduce k scope) src ,exp))))
    (define (visit-fun fun)
      (rewrite-cps-exp fun
        (($ $fun free body)
         ($fun free ,(visit-cont body #f)))))
    (visit-fun fun)))

(define (compute-beta-reductions fun)
  ;; A continuation's body can be inlined in place of a $values
  ;; expression if the continuation is a $kargs.  It should only be
  ;; inlined if it is used only once, and not recursively.
  (let ((var-table (make-hash-table))
        (k-table (make-hash-table))
        (dfg (compute-dfg fun)))
    (define (visit-cont cont)
      (match cont
        (($ $cont sym ($ $kargs names syms body))
         (visit-term body))
        (($ $cont sym ($ $kfun src meta self tail clause))
         (when clause (visit-cont clause)))
        (($ $cont sym ($ $kclause arity body alternate))
         (visit-cont body)
         (when alternate (visit-cont alternate)))
        (($ $cont sym (or ($ $ktail) ($ $kreceive) ($ $kif)))
         #f)))
    (define (visit-term term)
      (match term
        (($ $letk conts body)
         (for-each visit-cont conts)
         (visit-term body))
        (($ $letrec names syms funs body)
         (for-each visit-fun funs)
         (visit-term body))
        (($ $continue k src ($ $values args))
         (match (lookup-cont k dfg)
           (($ $kargs names syms body)
            (match (lookup-predecessors k dfg)
              ((_)
               ;; There is only one use, and it is this use.  We assume
               ;; it's not recursive, as there would to be some other
               ;; use for control flow to reach this loop.  Store the k
               ;; -> body mapping in the table.  Also store the
               ;; substitutions for the variables bound by the inlined
               ;; continuation.
               (for-each (cut hashq-set! var-table <> <>) syms args)
               (hashq-set! k-table k body))
              (_ #f)))
           (_ #f)))
        (($ $continue k src (and fun ($ $fun)))
         (visit-fun fun))
        (($ $continue k src _)
         #f)))
    (define (visit-fun fun)
      (match fun
        (($ $fun free body)
         (visit-cont body))))
    (visit-fun fun)
    (values var-table k-table)))

(define (beta-reduce fun)
  (let-values (((var-table k-table) (compute-beta-reductions fun)))
    (define (subst var)
      (cond ((hashq-ref var-table var) => subst)
            (else var)))
    (define (must-visit-cont cont)
      (or (visit-cont cont)
          (error "continuation must not be inlined" cont)))
    (define (visit-cont cont)
      (match cont
        (($ $cont sym cont)
         (and (not (hashq-ref k-table sym))
              (rewrite-cps-cont cont
                (($ $kargs names syms body)
                 (sym ($kargs names syms ,(visit-term body))))
                (($ $kfun src meta self tail clause)
                 (sym ($kfun src meta self ,tail
                        ,(and clause (must-visit-cont clause)))))
                (($ $kclause arity body alternate)
                 (sym ($kclause ,arity ,(must-visit-cont body)
                                ,(and alternate (must-visit-cont alternate)))))
                ((or ($ $kreceive) ($ $kif))
                 (sym ,cont)))))))
    (define (visit-term term)
      (match term
        (($ $letk conts body)
         (match (filter-map visit-cont conts)
           (() (visit-term body))
           (conts (build-cps-term
                    ($letk ,conts ,(visit-term body))))))
        (($ $letrec names syms funs body)
         (build-cps-term
           ($letrec names syms (map visit-fun funs)
                    ,(visit-term body))))
        (($ $continue k src exp)
         (cond
          ((hashq-ref k-table k) => visit-term)
          (else
           (build-cps-term
             ($continue k src
               ,(match exp
                  ((or ($ $void) ($ $const) ($ $prim)) exp)
                  (($ $fun) (visit-fun exp))
                  (($ $call proc args)
                   (let ((args (map subst args)))
                     (build-cps-exp ($call (subst proc) args))))
                  (($ $callk k proc args)
                   (let ((args (map subst args)))
                     (build-cps-exp ($callk k (subst proc) args))))
                  (($ $primcall name args)
                   (let ((args (map subst args)))
                     (build-cps-exp ($primcall name args))))
                  (($ $values args)
                   (let ((args (map subst args)))
                     (build-cps-exp ($values args))))
                  (($ $prompt escape? tag handler)
                   (build-cps-exp ($prompt escape? (subst tag) handler)))))))))))
    (define (visit-fun fun)
      (rewrite-cps-exp fun
        (($ $fun free body)
         ($fun (map subst free) ,(must-visit-cont body)))))
    (visit-fun fun)))

(define (simplify fun)
  ;; Renumbering prunes continuations that are made unreachable by
  ;; eta/beta reductions.
  (renumber (eta-reduce (beta-reduce fun))))
