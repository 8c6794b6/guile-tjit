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
;;; Contification is a pass that turns $fun instances into $cont
;;; instances if all calls to the $fun return to the same continuation.
;;; This is a more rigorous variant of our old "fixpoint labels
;;; allocation" optimization.
;;;
;;; See Kennedy's "Compiling with Continuations, Continued", and Fluet
;;; and Weeks's "Contification using Dominators".
;;;
;;; Code:

(define-module (language cps contification)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (concatenate))
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (language cps primitives)
  #:use-module (language rtl)
  #:export (contify))

(define (contify fun)
  (let* ((dfg (compute-dfg fun))
         (cont-table (dfg-cont-table dfg))
         (call-substs '())
         (cont-substs '())
         (pending-contifications (make-hash-table)))
    (define (subst-call! sym arities body-ks)
      (set! call-substs (acons sym (map cons arities body-ks) call-substs)))
    (define (subst-return! old-tail new-tail)
      (set! cont-substs (acons old-tail new-tail cont-substs)))
    (define (lookup-return-cont k)
      (or (assq-ref cont-substs k) k))

    (define (add-pending-contifications! scope conts)
      (for-each (match-lambda
                 (($ $cont k)
                  (lift-definition! k scope dfg)))
                conts)
      (hashq-set! pending-contifications scope
                  (append conts
                          (hashq-ref pending-contifications scope '()))))
    (define (finish-pending-contifications call term-k)
      (match (hashq-ref pending-contifications term-k)
        (#f call)
        ((cont ...)
         ;; Catch any possible double-contification bug.
         (hashq-set! pending-contifications term-k 'poison)
         (build-cps-term
           ($letk ,(map visit-cont cont)
             ,call)))))

    (define (contify-call proc args)
      (and=> (assq-ref call-substs proc)
             (lambda (clauses)
               (let lp ((clauses clauses))
                 (match clauses
                   (() (error "invalid contification"))
                   (((($ $arity req () #f () #f) . k) . clauses)
                    (if (= (length req) (length args))
                        (build-cps-term
                          ($continue k ($values args)))
                        (lp clauses)))
                   ((_ . clauses) (lp clauses)))))))

    ;; If K is a continuation that binds one variable, and it has only
    ;; one predecessor, return that variable.
    (define (bound-symbol k)
      (match (lookup-cont k cont-table)
        (($ $kargs (_) (sym))
         (match (lookup-uses k dfg)
           ((_)
            ;; K has one predecessor, the one that defined SYM.
            sym)
           (_ #f)))
        (_ #f)))

    (define (contify-fun term-k sym self tail arities bodies)
      (contify-funs term-k
                    (list sym) (list self) (list tail)
                    (list arities) (list bodies)))

    ;; Given a set of mutually recursive functions bound to local
    ;; variables SYMS, with self symbols SELFS, tail continuations
    ;; TAILS, arities ARITIES, and bodies BODIES, all bound in TERM-K,
    ;; contify them if we can prove that they all return to the same
    ;; continuation.  Returns a true value on success, and false
    ;; otherwise.
    (define (contify-funs term-k syms selfs tails arities bodies)
      ;; Are the given args compatible with any of the arities?
      (define (applicable? proc args)
        (or-map (match-lambda
                 (($ $arity req () #f () #f)
                  (= (length args) (length req)))
                 (_ #f))
                (assq-ref (map cons syms arities) proc)))

      ;; If the use of PROC in continuation USE is a call to PROC that
      ;; is compatible with one of the procedure's arities, return the
      ;; target continuation.  Otherwise return #f.
      (define (call-target use proc)
        (match (find-call (lookup-cont use cont-table))
          (($ $continue k ($ $call proc* args))
           (and (eq? proc proc*) (not (memq proc args)) (applicable? proc args)
                k))
          (_ #f)))

      (and
       (and-map null? (map (cut lookup-uses <> dfg) selfs))
       (and=> (let visit-syms ((syms syms) (k #f))
                (match syms
                  (() k)
                  ((sym . syms)
                   (let visit-uses ((uses (lookup-uses sym dfg)) (k k))
                     (match uses
                       (() (visit-syms syms k))
                       ((use . uses)
                        (and=> (call-target use sym)
                               (lambda (k*)
                                 (cond
                                  ((memq k* tails) (visit-uses uses k))
                                  ((not k) (visit-uses uses k*))
                                  ((eq? k k*) (visit-uses uses k))
                                  (else #f))))))))))
              (lambda (k)
                ;; We have a common continuation.  High fives!
                ;;
                ;; (1) Find the scope at which to contify.
                (let ((scope (if (variable-bound-in? k term-k dfg)
                                 term-k
                                 (lookup-def k dfg))))
                  ;; (2) Mark all SYMs for replacement in calls, and
                  ;; mark the tail continuations for replacement by K.
                  (for-each (lambda (sym tail arities bodies)
                              (match bodies
                                ((($ $cont body-k) ...)
                                 (subst-call! sym arities body-k)))
                              (subst-return! tail k))
                            syms tails arities bodies)
                  ;; (3) Mutate the DFG to reflect the new scope of the
                  ;; continuations, and arrange for the continuations to
                  ;; be spliced into their new scope.
                  (add-pending-contifications! scope (concatenate bodies))
                  k)))))

    (define (visit-fun term)
      (rewrite-cps-exp term
        (($ $fun meta free body)
         ($fun meta free ,(visit-cont body)))))
    (define (visit-cont cont)
      (rewrite-cps-cont cont
        (($ $cont sym src
            ($ $kargs (name) (and sym (? (cut assq <> call-substs)))
               body))
         (sym src ($kargs () () ,(visit-term body sym))))
        (($ $cont sym src ($ $kargs names syms body))
         (sym src ($kargs names syms ,(visit-term body sym))))
        (($ $cont sym src ($ $kentry self tail clauses))
         (sym src ($kentry self ,tail ,(map visit-cont clauses))))
        (($ $cont sym src ($ $kclause arity body))
         (sym src ($kclause ,arity ,(visit-cont body))))
        (($ $cont)
         ,cont)))
    (define (visit-term term term-k)
      (match term
        (($ $letk conts body)
         ;; Visit the body first, so we visit depth-first.
         (let lp ((body (visit-term body term-k)))
           ;; Because we attach contified functions on a particular
           ;; term-k, and one term-k can correspond to an arbitrarily
           ;; nested sequence of $letrec and $letk instances, normalize
           ;; so that all continuations are bound by one $letk --
           ;; guaranteeing that they are in the same scope.
           (rewrite-cps-term body
             (($ $letrec names syms funs body)
              ($letrec names syms funs ,(lp body)))
             (($ $letk conts* body)
              ($letk ,(append conts* (map visit-cont conts))
                ,body))
             (body
              ($letk ,(map visit-cont conts)
                ,body)))))
        (($ $letrec names syms funs body)
         (define (split-components nsf)
           ;; FIXME: Compute strongly-connected components.  Currently
           ;; we just put non-recursive functions in their own
           ;; components, and lump everything else in the remaining
           ;; component.
           (define (recursive? k)
             (or-map (cut variable-free-in? <> k dfg) syms))
           (let lp ((nsf nsf) (rec '()))
             (match nsf
               (()
                (if (null? rec)
                    '()
                    (list rec)))
               (((and elt (n s ($ $fun meta free ($ $cont kentry))))
                 . nsf)
                (if (recursive? kentry)
                    (lp nsf (cons elt rec))
                    (cons (list elt) (lp nsf rec)))))))
         (define (visit-components components)
           (match components
             (() (visit-term body term-k))
             ((((name sym fun) ...) . components)
              (match fun
                ((($ $fun meta free
                     ($ $cont fun-k _
                        ($ $kentry self
                           ($ $cont tail-k _ ($ $ktail))
                           (($ $cont _ _ ($ $kclause arity body))
                            ...))))
                  ...)
                 (if (contify-funs term-k sym self tail-k arity body)
                     (visit-components components)
                     (build-cps-term
                       ($letrec name sym (map visit-fun fun)
                                ,(visit-components components)))))))))
         (visit-components (split-components (map list names syms funs))))
        (($ $continue k exp)
         (let ((k* (lookup-return-cont k)))
           (define (default)
             (rewrite-cps-term exp
               (($ $fun) ($continue k* ,(visit-fun exp)))
               (($ $primcall 'return (val))
                ,(if (eq? k k*)
                     (build-cps-term ($continue k* ,exp))
                     (build-cps-term ($continue k* ($values (val))))))
               (($ $primcall 'return-values vals)
                ,(if (eq? k k*)
                     (build-cps-term ($continue k* ,exp))
                     (build-cps-term ($continue k* ($values vals)))))
               (_ ($continue k* ,exp))))
           (finish-pending-contifications
            (match exp
              (($ $fun meta free
                  ($ $cont fun-k _
                     ($ $kentry self
                        ($ $cont tail-k _ ($ $ktail))
                        (($ $cont _ _ ($ $kclause arity body)) ...))))
               (if (and=> (bound-symbol k*)
                          (lambda (sym)
                            (contify-fun term-k sym self tail-k arity body)))
                   (build-cps-term
                     ($continue k* ($values ())))
                   (default)))
              (($ $call proc args)
               (or (contify-call proc args)
                   (default)))
              (_ (default)))
            term-k)))))

    (let ((fun (visit-fun fun)))
      (if (null? call-substs)
          fun
          ;; Iterate to fixed point.
          (contify fun)))))
