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
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:export (convert-closures))

;; free := var ...

(define (convert-free-var var self self-known? free k)
  "Convert one possibly free variable reference to a bound reference.

If @var{var} is free (i.e., present in @var{free},), it is replaced
by a closure reference via a @code{free-ref} primcall, and @var{k} is
called with the new var.  Otherwise @var{var} is bound, so @var{k} is
called with @var{var}."
  (cond
   ((list-index (cut eq? <> var) free)
    => (lambda (free-idx)
         (match (cons self-known? free)
           ;; A reference to one of the two free vars in a well-known
           ;; function.
           ((#t _ _)
            (let-fresh (k*) (var*)
              (build-cps-term
                ($letk ((k* ($kargs (var*) (var*) ,(k var*))))
                  ($continue k* #f
                    ($primcall (match free-idx (0 'car) (1 'cdr)) (self)))))))
           (_
            (let-fresh (k* kidx) (idx var*)
              (build-cps-term
                ($letk ((kidx ($kargs ('idx) (idx)
                                ($letk ((k* ($kargs (var*) (var*) ,(k var*))))
                                  ($continue k* #f
                                    ($primcall
                                     (cond
                                      ((not self-known?) 'free-ref)
                                      ((<= free-idx #xff) 'vector-ref/immediate)
                                      (else 'vector-ref))
                                     (self idx)))))))
                  ($continue kidx #f ($const free-idx)))))))))
   (else (k var))))
  
(define (convert-free-vars vars self self-known? free k)
  "Convert a number of possibly free references to bound references.
@var{k} is called with the bound references, and should return the
term."
  (match vars
    (() (k '()))
    ((var . vars)
     (convert-free-var var self self-known? free
                       (lambda (var)
                         (convert-free-vars vars self self-known? free
                                            (lambda (vars)
                                              (k (cons var vars)))))))))
  
(define (allocate-closure src name var label known? free body)
  "Allocate a new closure."
  (match (cons known? free)
    ((#f . _)
     (let-fresh (k*) ()
       (build-cps-term
         ($letk ((k* ($kargs (name) (var) ,body)))
           ($continue k* src
             ($closure label (length free)))))))
    ((#t)
     ;; Well-known callee with no free variables; elide the
     ;; binding entirely.
     body)
    ;; FIXME: Single-var case here.
    ((#t _ _)
     ;; Well-known callee with two free variables; the closure is a
     ;; pair.
     (let-fresh (kinit kfalse) (false)
       (build-cps-term
         ($letk ((kinit ($kargs (name) (var)
                          ,body))
                 (kfalse ($kargs ('false) (false)
                           ($continue kinit src
                             ($primcall 'cons (false false))))))
           ($continue kfalse src ($const #f))))))
    ;; Well-known callee with more than two free variables; the closure
    ;; is a vector.
    ((#t . _)
     (let ((nfree (length free)))
       (let-fresh (kinit klen kfalse) (false len-var)
         (build-cps-term
           ($letk ((kinit ($kargs (name) (var) ,body))
                   (kfalse ($kargs ('false) (false)
                             ($letk ((klen
                                      ($kargs ('len) (len-var)
                                        ($continue kinit src
                                          ($primcall (if (<= nfree #xff)
                                                         'make-vector/immediate
                                                         'make-vector)
                                                     (len-var false))))))
                               ($continue klen src ($const nfree))))))
             ($continue kfalse src ($const #f)))))))))

(define (init-closure src var known? free
                      outer-self outer-known? outer-free body)
  "Initialize the free variables @var{free} in a closure bound to
@var{var}, and continue with @var{body}.  @var{outer-self} must be the
label of the outer procedure, where the initialization will be
performed, and @var{outer-free} is the list of free variables there."
  (match (cons known? free)
    ;; Well-known callee with no free variables; no initialization
    ;; necessary.
    ((#t) body)
    ;; Well-known callee with two free variables; do a set-car! and
    ;; set-cdr!.
    ((#t v0 v1)
     (let-fresh (kcar kcdr) ()
       (convert-free-var
        v0 outer-self outer-known? outer-free
        (lambda (v0)
          (build-cps-term
            ($letk ((kcar ($kargs () ()
                            ,(convert-free-var
                              v1 outer-self outer-known? outer-free
                              (lambda (v1)
                                (build-cps-term
                                  ($letk ((kcdr ($kargs () () ,body)))
                                    ($continue kcdr src
                                      ($primcall 'set-cdr! (var v1))))))))))
              ($continue kcar src
                ($primcall 'set-car! (var v0)))))))))
    ;; Otherwise residualize a sequence of vector-set! or free-set!,
    ;; depending on whether the callee is well-known or not.
    (_
     (fold (lambda (free idx body)
             (let-fresh (k) (idxvar)
               (build-cps-term
                 ($letk ((k ($kargs () () ,body)))
                   ,(convert-free-var
                     free outer-self outer-known? outer-free
                     (lambda (free)
                       (build-cps-term
                         ($letconst (('idx idxvar idx))
                           ($continue k src
                             ($primcall (cond
                                         ((not known?) 'free-set!)
                                         ((<= idx #xff) 'vector-set!/immediate)
                                         (else 'vector-set!))
                                        (var idxvar free)))))))))))
           body
           free
           (iota (length free))))))

(define (analyze-closures exp dfg)
  "Compute the set of free variables for all $fun instances in
@var{exp}."
  (let ((free-vars (make-hash-table))
        (named-funs (make-hash-table))
        (well-known-vars (make-bitvector (var-counter) #t)))
    (define (add-named-fun! var cont)
      (hashq-set! named-funs var cont))
    (define (clear-well-known! var)
      (bitvector-set! well-known-vars var #f))
    (define (compute-well-known-labels)
      (let ((bv (make-bitvector (label-counter) #f)))
        (hash-for-each
         (lambda (var cont)
           (match cont
             (($ $cont label ($ $kfun src meta self))
              (unless (equal? var self)
                (bitvector-set! bv label
                                (and (bitvector-ref well-known-vars var)
                                     (bitvector-ref well-known-vars self)))))))
         named-funs)
        bv))
    (define (union a b)
      (lset-union eq? a b))
    (define (difference a b)
      (lset-difference eq? a b))
    (define (visit-cont cont bound)
      (match cont
        (($ $cont label ($ $kargs names vars body))
         (visit-term body (append vars bound)))
        (($ $cont label ($ $kfun src meta self tail clause))
         (add-named-fun! self cont)
         (let ((free (if clause
                         (visit-cont clause (list self))
                         '())))
           (hashq-set! free-vars label free)
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
           (for-each add-named-fun! vars cont)
           (fold (lambda (cont free)
                   (union (visit-cont cont bound) free))
                 (visit-term body bound)
                 cont)))
        (($ $continue k src ($ $fun () body))
         (match (lookup-predecessors k dfg)
           ((_) (match (lookup-cont k dfg)
                  (($ $kargs (name) (var))
                   (add-named-fun! var body))))
           (_ #f))
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
         (for-each clear-well-known! args)
         (fold adjoin (adjoin proc '()) args))
        (($ $primcall name args)
         (for-each clear-well-known! args)
         (fold adjoin '() args))
        (($ $values args)
         (for-each clear-well-known! args)
         (fold adjoin '() args))
        (($ $prompt escape? tag handler)
         (clear-well-known! tag)
         (adjoin tag '()))))

    (let ((free (visit-cont exp '())))
      (unless (null? free)
        (error "Expected no free vars in toplevel thunk" free exp))
      (values free-vars named-funs (compute-well-known-labels)))))

(define (prune-free-vars free-vars named-funs well-known)
  (define (well-known? label)
    (bitvector-ref well-known label))
  (let ((eliminated (make-bitvector (label-counter) #f)))
    (define (filter-out-eliminated free)
      (match free
        (() '())
        ((var . free)
         (match (hashq-ref named-funs var)
           (($ $cont (? (cut bitvector-ref eliminated <>) label))
            (filter-out-eliminated free))
           (_ (cons var (filter-out-eliminated free)))))))
    (let lp ((label 0))
      (let ((label (bit-position #t well-known label)))
        (when label
          (match (hashq-ref free-vars label)
            ;; Eliminate all well-known closures that have no free
            ;; variables.
            (() (bitvector-set! eliminated label #t))
            (_ #f))
          (lp (1+ label)))))
    (let lp ()
      (let ((recurse? #f))
        (hash-for-each-handle
         (lambda (pair)
           (match pair
             ((label . ()) #t)
             ((label . free)
              ;; We could be more precise and eliminate elements of
              ;; `free' that are well-known closures within this
              ;; function, even if they aren't globally well known.  Not
              ;; implemented.
              (let ((free (filter-out-eliminated free)))
                (set-cdr! pair free)
                (when (and (null? free) (well-known? label))
                  (bitvector-set! eliminated label #t)
                  (set! recurse? #t))))))
         free-vars)
        ;; Iterate to fixed point.
        (when recurse? (lp))))))

(define (convert-one label fun free-vars named-funs well-known)
  (define (well-known? label)
    (bitvector-ref well-known label))

  ;; Load the closure for a known call.  The callee may or may not be
  ;; known at all call sites.
  (define (convert-known-proc-call var label self self-known? free k)
    (match (cons (well-known? label)
                 (hashq-ref free-vars label))
      ((#t)
       ;; Calling a well-known procedure with no free variables; pass #f
       ;; as the closure.
       (let-fresh (k*) (v*)
         (build-cps-term
           ($letk ((k* ($kargs (v*) (v*) ,(k v*))))
             ($continue k* #f ($const #f))))))
      (_
       (convert-free-var var self self-known? free k))))

  (let ((free (hashq-ref free-vars label))
        (self-known? (well-known? label))
        (self (match fun (($ $kfun _ _ self) self))))
    (define (visit-cont cont)
      (rewrite-cps-cont cont
        (($ $cont label ($ $kargs names vars body))
         (label ($kargs names vars ,(visit-term body))))
        (($ $cont label ($ $kfun src meta self tail clause))
         (label ($kfun src meta self ,tail
                  ,(and clause (visit-cont clause)))))
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
              (let ((fun-free (hashq-ref free-vars kfun)))
                (lp in
                    (lambda (body)
                      (allocate-closure
                       src name var kfun (well-known? kfun) fun-free
                       (bindings body)))
                    (init-closure
                     src var (well-known? kfun) fun-free self self-known? free
                     body)))))))

        (($ $continue k src (or ($ $void) ($ $const) ($ $prim)))
         term)

        (($ $continue k src ($ $fun () ($ $cont kfun)))
         (let ((fun-free (hashq-ref free-vars kfun)))
           (match fun-free
             (()
              (build-cps-term
                ($continue k src ,(if (well-known? kfun)
                                      (build-cps-exp ($const #f))
                                      (build-cps-exp ($closure kfun 0))))))
             (_
              (let-fresh () (var)
                (allocate-closure
                 src #f var kfun (well-known? kfun) fun-free
                 (init-closure
                  src var (well-known? kfun) fun-free self self-known? free
                  (build-cps-term ($continue k src ($values (var)))))))))))

        (($ $continue k src ($ $call proc args))
         (match (hashq-ref named-funs proc)
           (($ $cont kfun)
            (convert-known-proc-call
             proc kfun self self-known? free
             (lambda (proc)
               (convert-free-vars args self self-known? free
                                  (lambda (args)
                                    (build-cps-term
                                      ($continue k src
                                        ($callk kfun proc args))))))))
           (#f
            (convert-free-vars (cons proc args) self self-known? free
                               (match-lambda
                                ((proc . args)
                                 (build-cps-term
                                   ($continue k src
                                     ($call proc args)))))))))

        (($ $continue k src ($ $primcall name args))
         (convert-free-vars args self self-known? free
                            (lambda (args)
                              (build-cps-term
                                ($continue k src ($primcall name args))))))

        (($ $continue k src ($ $values args))
         (convert-free-vars args self self-known? free
                            (lambda (args)
                              (build-cps-term
                                ($continue k src ($values args))))))

        (($ $continue k src ($ $prompt escape? tag handler))
         (convert-free-var tag self self-known? free
                           (lambda (tag)
                             (build-cps-term
                               ($continue k src
                                 ($prompt escape? tag handler))))))))
    (visit-cont (build-cps-cont (label ,fun)))))

(define (convert-closures fun)
  "Convert free reference in @var{exp} to primcalls to @code{free-ref},
and allocate and initialize flat closures."
  (let ((dfg (compute-dfg fun)))
    (with-fresh-name-state-from-dfg dfg
      (call-with-values (lambda () (analyze-closures fun dfg))
        (lambda (free-vars named-funs well-known)
          (prune-free-vars free-vars named-funs well-known)
          (let ((labels (sort (hash-map->list (lambda (k v) k) free-vars) <)))
            (build-cps-term
              ($program
               ,(map (lambda (label)
                       (convert-one label (lookup-cont label dfg)
                                    free-vars named-funs well-known))
                     labels)))))))))
