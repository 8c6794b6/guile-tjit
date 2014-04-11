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
  #:use-module ((srfi srfi-1) #:select (concatenate filter-map))
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (language cps primitives)
  #:use-module (language bytecode)
  #:export (contify))

(define (compute-contification fun)
  (let* ((dfg (compute-dfg fun))
         (scope-table (make-hash-table))
         (call-substs '())
         (cont-substs '())
         (fun-elisions '())
         (cont-splices (make-hash-table)))
    (define (subst-call! sym arities body-ks)
      (set! call-substs (acons sym (map cons arities body-ks) call-substs)))
    (define (subst-return! old-tail new-tail)
      (set! cont-substs (acons old-tail new-tail cont-substs)))
    (define (elide-function! k cont)
      (set! fun-elisions (acons k cont fun-elisions)))
    (define (splice-conts! scope conts)
      (for-each (match-lambda
                 (($ $cont k) (hashq-set! scope-table k scope)))
                conts)
      (hashq-set! cont-splices scope
                  (append conts (hashq-ref cont-splices scope '()))))

    (define (lookup-return-cont k)
      (match (assq-ref cont-substs k)
        (#f k)
        (k (lookup-return-cont k))))

    ;; If K is a continuation that binds one variable, and it has only
    ;; one predecessor, return that variable.
    (define (bound-symbol k)
      (match (lookup-cont k dfg)
        (($ $kargs (_) (sym))
         (match (lookup-predecessors k dfg)
           ((_)
            ;; K has one predecessor, the one that defined SYM.
            sym)
           (_ #f)))
        (_ #f)))

    (define (extract-arities clause)
      (match clause
        (($ $cont _ ($ $kclause arity body alternate))
         (cons arity (extract-arities alternate)))
        (#f '())))
    (define (extract-bodies clause)
      (match clause
        (($ $cont _ ($ $kclause arity body alternate))
         (cons body (extract-bodies alternate)))
        (#f '())))

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
      (define (unused? sym)
        (null? (lookup-uses sym dfg)))

      ;; Are the given args compatible with any of the arities?
      (define (applicable? proc args)
        (let lp ((arities (assq-ref (map cons syms arities) proc)))
          (match arities
            ((($ $arity req () #f () #f) . arities)
             (or (= (length args) (length req))
                 (lp arities)))
            ;; If we reached the end of the arities, fail.  Also fail if
            ;; the next arity in the list has optional, keyword, or rest
            ;; arguments.
            (_ #f))))

      ;; If the use of PROC in continuation USE is a call to PROC that
      ;; is compatible with one of the procedure's arities, return the
      ;; target continuation.  Otherwise return #f.
      (define (call-target use proc)
        (match (find-call (lookup-cont use dfg))
          (($ $continue k src ($ $call proc* args))
           (and (eq? proc proc*) (not (memq proc args)) (applicable? proc args)
                ;; Converge more quickly by resolving already-contified
                ;; call targets.
                (lookup-return-cont k)))
          (_ #f)))

      ;; If this set of functions is always called with one
      ;; continuation, not counting tail calls between the functions,
      ;; return that continuation.
      (define (find-common-continuation)
        (let visit-syms ((syms syms) (k #f))
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
                            (else #f)))))))))))

      ;; Given that the functions are called with the common
      ;; continuation K, determine the scope at which to contify the
      ;; functions.  If K is in scope in the term, we go ahead and
      ;; contify them there.  Otherwise the scope is inside the letrec
      ;; body, and so choose the scope in which the continuation is
      ;; defined, whose free variables are a superset of the free
      ;; variables of the functions.
      ;;
      ;; There is some slight trickiness here.  Call-target already uses
      ;; the information we compute within this pass.  Previous
      ;; contifications may cause functions to be contified not at their
      ;; point of definition but at their point of non-recursive use.
      ;; That will cause the scope nesting to change.  (It may
      ;; effectively push a function deeper down the tree -- the second
      ;; case above, a call within the letrec body.)  What if we contify
      ;; to the tail of a previously contified function?  We have to
      ;; track what the new scope tree will be when asking whether K
      ;; will be bound in TERM-K's scope, not the scope tree that
      ;; existed when we started the pass.
      ;;
      ;; FIXME: Does this choose the right scope for contified let-bound
      ;; functions?
      (define (find-contification-scope k)
        (define (scope-contains? scope k)
          (let ((k-scope (or (hashq-ref scope-table k)
                             (let ((k-scope (lookup-block-scope k dfg)))
                               (hashq-set! scope-table k k-scope)
                               k-scope))))
            (or (eq? scope k-scope)
                (and k-scope (scope-contains? scope k-scope)))))

        ;; Find the scope of K.
        (define (continuation-scope k)
          (or (hashq-ref scope-table k)
              (let ((scope (lookup-block-scope k dfg)))
                (hashq-set! scope-table k scope)
                scope)))

        (let ((k-scope (continuation-scope k)))
          (if (scope-contains? k-scope term-k)
              term-k
              (match (lookup-cont k-scope dfg)
                (($ $kfun src meta self tail clause)
                 ;; K is the tail of some function.  If that function
                 ;; has just one clause, return that clause.  Otherwise
                 ;; bail.
                 (match clause
                   (($ $cont _ ($ $kclause arity ($ $cont kargs) #f))
                    kargs)
                   (_ #f)))
                (_ k-scope)))))

      ;; We are going to contify.  Mark all SYMs for replacement in
      ;; calls, and mark the tail continuations for replacement by K.
      ;; Arrange for the continuations to be spliced into SCOPE.
      (define (enqueue-contification! k scope)
        (for-each (lambda (sym tail arities bodies)
                    (match bodies
                      ((($ $cont body-k) ...)
                       (subst-call! sym arities body-k)))
                    (subst-return! tail k))
                  syms tails arities bodies)
        (splice-conts! scope (concatenate bodies))
        #t)

      ;; "Call me maybe"
      (and (and-map unused? selfs)
           (and=> (find-common-continuation)
                  (lambda (k)
                    (and=> (find-contification-scope k)
                           (cut enqueue-contification! k <>))))))

    (define (visit-fun term)
      (match term
        (($ $fun free body)
         (visit-cont body))))
    (define (visit-cont cont)
      (match cont
        (($ $cont sym ($ $kargs _ _ body))
         (visit-term body sym))
        (($ $cont sym ($ $kfun src meta self tail clause))
         (when clause (visit-cont clause)))
        (($ $cont sym ($ $kclause arity body alternate))
         (visit-cont body)
         (when alternate (visit-cont alternate)))
        (($ $cont)
         #t)))
    (define (visit-term term term-k)
      (match term
        (($ $letk conts body)
         (for-each visit-cont conts)
         (visit-term body term-k))
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
               (((and elt (n s ($ $fun free ($ $cont kfun))))
                 . nsf)
                (if (recursive? kfun)
                    (lp nsf (cons elt rec))
                    (cons (list elt) (lp nsf rec)))))))
         (define (extract-arities+bodies clauses)
           (values (map extract-arities clauses)
                   (map extract-bodies clauses)))
         (define (visit-component component)
           (match component
             (((name sym fun) ...)
              (match fun
                ((($ $fun free
                     ($ $cont fun-k
                        ($ $kfun src meta self ($ $cont tail-k ($ $ktail))
                           clause)))
                  ...)
                 (call-with-values (lambda () (extract-arities+bodies clause))
                   (lambda (arities bodies)
                     (if (contify-funs term-k sym self tail-k arities bodies)
                         (for-each (cut for-each visit-cont <>) bodies)
                         (for-each visit-fun fun)))))))))
         (visit-term body term-k)
         (for-each visit-component
                   (split-components (map list names syms funs))))
        (($ $continue k src exp)
         (match exp
           (($ $fun free
               ($ $cont fun-k
                  ($ $kfun src meta self ($ $cont tail-k ($ $ktail)) clause)))
            (if (and=> (bound-symbol k)
                       (lambda (sym)
                         (contify-fun term-k sym self tail-k
                                      (extract-arities clause)
                                      (extract-bodies clause))))
                (begin
                  (elide-function! k (lookup-cont k dfg))
                  (for-each visit-cont (extract-bodies clause)))
                (visit-fun exp)))
           (_ #t)))))

    (visit-cont fun)
    (values call-substs cont-substs fun-elisions cont-splices)))

(define (apply-contification fun call-substs cont-substs fun-elisions cont-splices)
  (define (contify-call src proc args)
    (and=> (assq-ref call-substs proc)
           (lambda (clauses)
             (let lp ((clauses clauses))
               (match clauses
                 (() (error "invalid contification"))
                 (((($ $arity req () #f () #f) . k) . clauses)
                  (if (= (length req) (length args))
                      (build-cps-term
                        ($continue k src
                          ($values args)))
                      (lp clauses)))
                 ((_ . clauses) (lp clauses)))))))
  (define (continue k src exp)
    (define (lookup-return-cont k)
      (match (assq-ref cont-substs k)
        (#f k)
        (k (lookup-return-cont k))))
    (let ((k* (lookup-return-cont k)))
      ;; We are contifying this return.  It must be a call or a
      ;; primcall to values, return, or return-values.
      (if (eq? k k*)
          (build-cps-term ($continue k src ,exp))
          (rewrite-cps-term exp
            (($ $primcall 'return (val))
             ($continue k* src ($primcall 'values (val))))
            (($ $values vals)
             ($continue k* src ($primcall 'values vals)))
            (_ ($continue k* src ,exp))))))
  (define (splice-continuations term-k term)
    (match (hashq-ref cont-splices term-k)
      (#f term)
      ((cont ...)
       (let lp ((term term))
         (rewrite-cps-term term
           (($ $letrec names syms funs body)
            ($letrec names syms funs ,(lp body)))
           (($ $letk conts* body)
            ($letk ,(append conts* (filter-map visit-cont cont))
              ,body))
           (body
            ($letk ,(filter-map visit-cont cont)
              ,body)))))))
  (define (visit-fun term)
    (rewrite-cps-exp term
      (($ $fun free body)
       ($fun free ,(visit-cont body)))))
  (define (visit-cont cont)
    (rewrite-cps-cont cont
      (($ $cont (? (cut assq <> fun-elisions)))
       ;; This cont gets inlined in place of the $fun.
       ,#f)
      (($ $cont sym ($ $kargs names syms body))
       (sym ($kargs names syms ,(visit-term body sym))))
      (($ $cont sym ($ $kfun src meta self tail clause))
       (sym ($kfun src meta self ,tail ,(and clause (visit-cont clause)))))
      (($ $cont sym ($ $kclause arity body alternate))
       (sym ($kclause ,arity ,(visit-cont body)
                      ,(and alternate (visit-cont alternate)))))
      (($ $cont)
       ,cont)))
  (define (visit-term term term-k)
    (match term
      (($ $letk conts body)
       ;; Visit the body first, so we rewrite depth-first.
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
            ($letk ,(append conts* (filter-map visit-cont conts))
              ,body))
           (body
            ($letk ,(filter-map visit-cont conts)
              ,body)))))
      (($ $letrec names syms funs body)
       (rewrite-cps-term (filter (match-lambda
                                  ((n s f) (not (assq s call-substs))))
                                 (map list names syms funs))
         (((names syms funs) ...)
          ($letrec names syms (map visit-fun funs)
                   ,(visit-term body term-k)))))
      (($ $continue k src exp)
       (splice-continuations
        term-k
        (match exp
          (($ $fun)
           (cond
            ((assq-ref fun-elisions k)
             => (match-lambda
                 (($ $kargs (_) (_) body)
                  (visit-term body k))))
            (else
             (continue k src (visit-fun exp)))))
          (($ $call proc args)
           (or (contify-call src proc args)
               (continue k src exp)))
          (_ (continue k src exp)))))))
  (visit-cont fun))

(define (contify fun)
  (call-with-values (lambda () (compute-contification fun))
    (lambda (call-substs cont-substs fun-elisions cont-splices)
      (if (null? call-substs)
          fun
          ;; Iterate to fixed point.
          (contify
           (apply-contification fun call-substs cont-substs fun-elisions cont-splices))))))
