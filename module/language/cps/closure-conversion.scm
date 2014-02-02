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
;;; This pass converts a CPS term in such a way that no function has any
;;; free variables.  Instead, closures are built explicitly with
;;; make-closure primcalls, and free variables are referenced through
;;; the closure.
;;;
;;; Closure conversion also removes any $letrec forms that contification
;;; did not handle.  See (language cps) for a further discussion of
;;; $letrec.
;;;
;;; Code:

(define-module (language cps closure-conversion)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold
                                        lset-union lset-difference
                                        list-index))
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:export (convert-closures))

(define (union s1 s2)
  (lset-union eq? s1 s2))

(define (difference s1 s2)
  (lset-difference eq? s1 s2))

;; bound := sym ...
;; free := sym ...

(define (convert-free-var sym self bound k)
  "Convert one possibly free variable reference to a bound reference.

If @var{sym} is free (i.e., not present in @var{bound},), it is replaced
by a closure reference via a @code{free-ref} primcall, and @var{k} is
called with the new var.  Otherwise @var{sym} is bound, so @var{k} is
called with @var{sym}.

@var{k} should return two values: a term and a list of additional free
values in the term."
  (if (memq sym bound)
      (k sym)
      (let-gensyms (k* sym*)
        (receive (exp free) (k sym*)
          (values (build-cps-term
                    ($letk ((k* ($kargs (sym*) (sym*) ,exp)))
                      ($continue k* #f ($primcall 'free-ref (self sym)))))
                  (cons sym free))))))
  
(define (convert-free-vars syms self bound k)
  "Convert a number of possibly free references to bound references.
@var{k} is called with the bound references, and should return two
values: the term and a list of additional free variables in the term."
  (match syms
    (() (k '()))
    ((sym . syms)
     (convert-free-var sym self bound
                       (lambda (sym)
                         (convert-free-vars syms self bound
                                            (lambda (syms)
                                              (k (cons sym syms)))))))))
  
(define (init-closure src v free outer-self outer-bound body)
  "Initialize the free variables @var{free} in a closure bound to
@var{v}, and continue with @var{body}.  @var{outer-self} must be the
label of the outer procedure, where the initialization will be
performed, and @var{outer-bound} is the list of bound variables there."
  (fold (lambda (free idx body)
          (let-gensyms (k idxsym)
            (build-cps-term
              ($letk ((k ($kargs () () ,body)))
                ,(convert-free-var
                  free outer-self outer-bound
                  (lambda (free)
                    (values (build-cps-term
                              ($letconst (('idx idxsym idx))
                                ($continue k src
                                  ($primcall 'free-set! (v idxsym free)))))
                            '())))))))
        body
        free
        (iota (length free))))

(define (cc* exps self bound)
  "Convert all free references in the list of expressions @var{exps} to
bound references, and convert functions to flat closures.  Returns two
values: the transformed list, and a cumulative set of free variables."
  (let lp ((exps exps) (exps* '()) (free '()))
    (match exps
      (() (values (reverse exps*) free))
      ((exp . exps)
       (receive (exp* free*) (cc exp self bound)
         (lp exps (cons exp* exps*) (union free free*)))))))

;; Closure conversion.
(define (cc exp self bound)
  "Convert all free references in @var{exp} to bound references, and
convert functions to flat closures."
  (match exp
    (($ $letk conts body)
     (receive (conts free) (cc* conts self bound)
       (receive (body free*) (cc body self bound)
         (values (build-cps-term ($letk ,conts ,body))
                 (union free free*)))))

    (($ $cont sym ($ $kargs names syms body))
     (receive (body free) (cc body self (append syms bound))
       (values (build-cps-cont (sym ($kargs names syms ,body)))
               free)))

    (($ $cont sym ($ $kentry self tail clauses))
     (receive (clauses free) (cc* clauses self (list self))
       (values (build-cps-cont (sym ($kentry self ,tail ,clauses)))
               free)))

    (($ $cont sym ($ $kclause arity body))
     (receive (body free) (cc body self bound)
       (values (build-cps-cont (sym ($kclause ,arity ,body)))
               free)))

    (($ $cont)
     ;; Other kinds of continuations don't bind values and don't have
     ;; bodies.
     (values exp '()))

    ;; Remove letrec.
    (($ $letrec names syms funs body)
     (let ((bound (append bound syms)))
       (receive (body free) (cc body self bound)
         (let lp ((in (map list names syms funs))
                  (bindings (lambda (body) body))
                  (body body)
                  (free free))
           (match in
             (() (values (bindings body) free))
             (((name sym ($ $fun src meta () fun-body)) . in)
              (receive (fun-body fun-free) (cc fun-body #f '())
                (lp in
                    (lambda (body)
                      (let-gensyms (k)
                        (build-cps-term
                          ($letk ((k ($kargs (name) (sym) ,(bindings body))))
                            ($continue k src
                              ($fun src meta fun-free ,fun-body))))))
                    (init-closure src sym fun-free self bound body)
                    (union free (difference fun-free bound))))))))))

    (($ $continue k src
        (or ($ $void)
            ($ $const)
            ($ $prim)))
     (values exp '()))

    (($ $continue k src ($ $fun src* meta () body))
     (receive (body free) (cc body #f '())
       (match free
         (()
          (values (build-cps-term
                    ($continue k src ($fun src* meta free ,body)))
                  free))
         (_
          (values
           (let-gensyms (kinit v)
             (build-cps-term
               ($letk ((kinit ($kargs (v) (v)
                                ,(init-closure
                                  src v free self bound
                                  (build-cps-term
                                    ($continue k src ($values (v))))))))
                 ($continue kinit src ($fun src* meta free ,body)))))
           (difference free bound))))))

    (($ $continue k src ($ $call proc args))
     (convert-free-vars (cons proc args) self bound
                        (match-lambda
                         ((proc . args)
                          (values (build-cps-term
                                    ($continue k src ($call proc args)))
                                  '())))))

    (($ $continue k src ($ $callk k* proc args))
     (convert-free-vars (cons proc args) self bound
                        (match-lambda
                         ((proc . args)
                          (values (build-cps-term
                                    ($continue k src ($callk k* proc args)))
                                  '())))))

    (($ $continue k src ($ $primcall name args))
     (convert-free-vars args self bound
                        (lambda (args)
                          (values (build-cps-term
                                    ($continue k src ($primcall name args)))
                                  '()))))

    (($ $continue k src ($ $values args))
     (convert-free-vars args self bound
                        (lambda (args)
                          (values (build-cps-term
                                    ($continue k src ($values args)))
                                  '()))))

    (($ $continue k src ($ $prompt escape? tag handler))
     (convert-free-var
      tag self bound
      (lambda (tag)
        (values (build-cps-term
                  ($continue k src ($prompt escape? tag handler)))
                '()))))

    (_ (error "what" exp))))

;; Convert the slot arguments of 'free-ref' primcalls from symbols to
;; indices.
(define (convert-to-indices body free)
  (define (free-index sym)
    (or (list-index (cut eq? <> sym) free)
        (error "free variable not found!" sym free)))
  (define (visit-term term)
    (rewrite-cps-term term
      (($ $letk conts body)
       ($letk ,(map visit-cont conts) ,(visit-term body)))
      (($ $continue k src ($ $primcall 'free-ref (closure sym)))
       ,(let-gensyms (idx)
          (build-cps-term
            ($letconst (('idx idx (free-index sym)))
              ($continue k src ($primcall 'free-ref (closure idx)))))))
      (($ $continue k src ($ $fun src* meta free body))
       ($continue k src
         ($fun src* meta free ,(convert-to-indices body free))))
      (($ $continue)
       ,term)))
  (define (visit-cont cont)
    (rewrite-cps-cont cont
      (($ $cont sym ($ $kargs names syms body))
       (sym ($kargs names syms ,(visit-term body))))
      (($ $cont sym ($ $kclause arity body))
       (sym ($kclause ,arity ,(visit-cont body))))
      ;; Other kinds of continuations don't bind values and don't have
      ;; bodies.
      (($ $cont)
       ,cont)))

  (rewrite-cps-cont body
    (($ $cont sym ($ $kentry self tail clauses))
     (sym ($kentry self ,tail ,(map visit-cont clauses))))))

(define (convert-closures exp)
  "Convert free reference in @var{exp} to primcalls to @code{free-ref},
and allocate and initialize flat closures."
  (match exp
    (($ $fun src meta () body)
     (receive (body free) (cc body #f '())
       (unless (null? free)
         (error "Expected no free vars in toplevel thunk" exp body free))
       (build-cps-exp
         ($fun src meta free ,(convert-to-indices body free)))))))
