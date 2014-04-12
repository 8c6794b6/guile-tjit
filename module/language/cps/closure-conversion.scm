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
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:export (convert-closures))

;; free := var ...

(define (convert-free-var var self free k)
  "Convert one possibly free variable reference to a bound reference.

If @var{var} is free (i.e., present in @var{free},), it is replaced
by a closure reference via a @code{free-ref} primcall, and @var{k} is
called with the new var.  Otherwise @var{var} is bound, so @var{k} is
called with @var{var}."
  (cond
   ((list-index (cut eq? <> var) free)
    => (lambda (free-idx)
         (let-fresh (k* kidx) (idx var*)
           (build-cps-term
             ($letk ((kidx ($kargs ('idx) (idx)
                             ($letk ((k* ($kargs (var*) (var*) ,(k var*))))
                               ($continue k* #f
                                 ($primcall 'free-ref (self idx)))))))
               ($continue kidx #f ($const free-idx)))))))
   (else (k var))))
  
(define (convert-free-vars vars self free k)
  "Convert a number of possibly free references to bound references.
@var{k} is called with the bound references, and should return the
term."
  (match vars
    (() (k '()))
    ((var . vars)
     (convert-free-var var self free
                       (lambda (var)
                         (convert-free-vars vars self free
                                            (lambda (vars)
                                              (k (cons var vars)))))))))
  
(define (init-closure src v free outer-self outer-free body)
  "Initialize the free variables @var{free} in a closure bound to
@var{v}, and continue with @var{body}.  @var{outer-self} must be the
label of the outer procedure, where the initialization will be
performed, and @var{outer-free} is the list of free variables there."
  (fold (lambda (free idx body)
          (let-fresh (k) (idxvar)
            (build-cps-term
              ($letk ((k ($kargs () () ,body)))
                ,(convert-free-var
                  free outer-self outer-free
                  (lambda (free)
                    (values (build-cps-term
                              ($letconst (('idx idxvar idx))
                                ($continue k src
                                  ($primcall 'free-set! (v idxvar free)))))
                            '())))))))
        body
        free
        (iota (length free))))

(define (compute-free-vars exp)
  "Compute the set of free variables for all $fun instances in
@var{exp}."
  (let ((table (make-hash-table)))
    (define (union a b)
      (lset-union eq? a b))
    (define (difference a b)
      (lset-difference eq? a b))
    (define (visit-cont cont bound)
      (match cont
        (($ $cont label ($ $kargs names vars body))
         (visit-term body (append vars bound)))
        (($ $cont label ($ $kfun src meta self tail clause))
         (let ((free (if clause
                         (visit-cont clause (list self))
                         '())))
           (hashq-set! table label (cons free cont))
           (difference free bound)))
        (($ $cont label ($ $kclause arity body alternate))
         (let ((free (visit-cont body bound)))
           (if alternate
               (union (visit-cont alternate bound) free)
               free)))
        (($ $cont) '())))
    (define (visit-term term bound)
      (match term
        (($ $letk conts body)
         (fold (lambda (cont free)
                 (union (visit-cont cont bound) free))
               (visit-term body bound)
               conts))
        (($ $letrec names vars (($ $fun () cont) ...) body)
         (let ((bound (append vars bound)))
           (fold (lambda (cont free)
                   (union (visit-cont cont bound) free))
                 (visit-term body bound)
                 cont)))
        (($ $continue k src ($ $fun () body))
         (visit-cont body bound))
        (($ $continue k src exp)
         (visit-exp exp bound))))
    (define (visit-exp exp bound)
      (define (adjoin var free)
        (if (or (memq var bound) (memq var free))
            free
            (cons var free)))
      (match exp
        ((or ($ $void) ($ $const) ($ $prim)) '())
        (($ $call proc args)
         (fold adjoin (adjoin proc '()) args))
        (($ $callk k* proc args)
         (fold adjoin (adjoin proc '()) args))
        (($ $primcall name args)
         (fold adjoin '() args))
        (($ $values args)
         (fold adjoin '() args))
        (($ $prompt escape? tag handler)
         (adjoin tag '()))))

    (let ((free (visit-cont exp '())))
      (unless (null? free)
        (error "Expected no free vars in toplevel thunk" free exp))
      table)))

(define (convert-one label table)
  (match (hashq-ref table label)
    ((free . (and fun ($ $cont _ ($ $kfun _ _ self))))
     (define (visit-cont cont)
       (rewrite-cps-cont cont
         (($ $cont label ($ $kargs names vars body))
          (label ($kargs names vars ,(visit-term body))))
         (($ $cont label ($ $kfun src meta self tail clause))
          (label ($kfun src meta self ,tail ,(and clause (visit-cont clause)))))
         (($ $cont label ($ $kclause arity body alternate))
          (label ($kclause ,arity ,(visit-cont body)
                         ,(and alternate (visit-cont alternate)))))
         (($ $cont) ,cont)))
     (define (visit-term term)
       (match term
         (($ $letk conts body)
          (build-cps-term
            ($letk ,(map visit-cont conts) ,(visit-term body))))

         ;; Remove letrec.
         (($ $letrec names vars funs body)
          (let lp ((in (map list names vars funs))
                   (bindings (lambda (body) body))
                   (body (visit-term body)))
            (match in
              (() (bindings body))
              (((name var ($ $fun ()
                             (and fun-body
                                  ($ $cont kfun ($ $kfun src))))) . in)
               (match (hashq-ref table kfun)
                 ((fun-free . _)
                  (lp in
                      (lambda (body)
                        (let-fresh (k) ()
                          (build-cps-term
                            ($letk ((k ($kargs (name) (var) ,(bindings body))))
                              ($continue k src
                                ($closure kfun (length fun-free)))))))
                      (init-closure src var fun-free self free body))))))))

         (($ $continue k src (or ($ $void) ($ $const) ($ $prim)))
          term)

         (($ $continue k src ($ $fun () ($ $cont kfun)))
          (match (hashq-ref table kfun)
            ((() . _)
             (build-cps-term ($continue k src ($closure kfun 0))))
            ((fun-free . _)
             (let-fresh (kinit) (v)
               (build-cps-term
                 ($letk ((kinit ($kargs (v) (v)
                                  ,(init-closure
                                    src v fun-free self free
                                    (build-cps-term
                                      ($continue k src ($values (v))))))))
                   ($continue kinit src
                     ($closure kfun (length fun-free)))))))))

         (($ $continue k src ($ $call proc args))
          (convert-free-vars (cons proc args) self free
                             (match-lambda
                              ((proc . args)
                               (build-cps-term
                                 ($continue k src ($call proc args)))))))

         (($ $continue k src ($ $callk k* proc args))
          (convert-free-vars (cons proc args) self free
                             (match-lambda
                              ((proc . args)
                               (build-cps-term
                                 ($continue k src ($callk k* proc args)))))))

         (($ $continue k src ($ $primcall name args))
          (convert-free-vars args self free
                             (lambda (args)
                               (build-cps-term
                                 ($continue k src ($primcall name args))))))

         (($ $continue k src ($ $values args))
          (convert-free-vars args self free
                             (lambda (args)
                               (build-cps-term
                                 ($continue k src ($values args))))))

         (($ $continue k src ($ $prompt escape? tag handler))
          (convert-free-var tag self free
                            (lambda (tag)
                              (build-cps-term
                                ($continue k src
                                  ($prompt escape? tag handler))))))))
     (visit-cont fun))))

(define (convert-closures fun)
  "Convert free reference in @var{exp} to primcalls to @code{free-ref},
and allocate and initialize flat closures."
  (with-fresh-name-state fun
    (let* ((table (compute-free-vars fun))
           (labels (sort (hash-map->list (lambda (k v) k) table) <)))
      (build-cps-term
        ($program ,(map (cut convert-one <> table) labels))))))
