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
;;; Common subexpression elimination for CPS.
;;;
;;; Code:

(define-module (language cps cse)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (language cps effects-analysis)
  #:use-module (language cps renumber)
  #:export (eliminate-common-subexpressions))

(define (compute-always-available-expressions effects)
  "Return the set of continuations whose values are always available
within their dominance frontier.  This is the case for effects that do
not allocate, read, or write mutable memory."
  (let ((out (make-bitvector (vector-length effects) #f)))
    (let lp ((n 0))
      (cond
       ((< n (vector-length effects))
        (unless (causes-effect? (vector-ref effects n)
                                (logior &allocation &read &write))
          (bitvector-set! out n #t))
        (lp (1+ n)))
       (else out)))))

(define (compute-available-expressions dfg min-label label-count)
  "Compute and return the continuations that may be reached if flow
reaches a continuation N.  Returns a vector of bitvectors, whose first
index corresponds to MIN-LABEL, and so on."
  (let* ((effects (compute-effects dfg min-label label-count))
         (always-avail (compute-always-available-expressions effects))
         ;; Vector of bitvectors, indicating that at a continuation N,
         ;; the values from continuations M... are available.
         (avail-in (make-vector label-count #f))
         (avail-out (make-vector label-count #f)))

    (define (label->idx label) (- label min-label))
    (define (idx->label idx) (+ idx min-label))

    (synthesize-definition-effects! effects dfg min-label label-count)

    (let lp ((n 0))
      (when (< n label-count)
        (vector-set! avail-in n (make-bitvector label-count #f))
        (vector-set! avail-out n (make-bitvector label-count #f))
        (lp (1+ n))))

    (let ((tmp (make-bitvector label-count #f)))
      (define (bitvector-copy! dst src)
        (bitvector-fill! dst #f)
        (bit-set*! dst src #t))
      (define (intersect! dst src)
        (bitvector-copy! tmp src)
        (bit-invert! tmp)
        (bit-set*! dst tmp #f))
      (let lp ((n 0) (first? #t) (changed? #f))
        (cond
         ((< n label-count)
          (let* ((in (vector-ref avail-in n))
                 (prev-count (bit-count #t in))
                 (out (vector-ref avail-out n))
                 (fx (vector-ref effects n)))
            ;; Intersect avail-out from predecessors into "in".
            (let lp ((preds (lookup-predecessors (idx->label n) dfg))
                     (initialized? #f))
              (match preds
                (() #t)
                ((pred . preds)
                 (let ((pred (label->idx pred)))
                   (cond
                    ((and first? (<= n pred))
                     ;; Avoid intersecting back-edges and cross-edges on
                     ;; the first iteration.
                     (lp preds initialized?))
                    (else
                     (if initialized?
                         (intersect! in (vector-ref avail-out pred))
                         (bitvector-copy! in (vector-ref avail-out pred)))
                     (lp preds #t)))))))
            (let ((new-count (bit-count #t in)))
              (unless (= prev-count new-count)
                ;; Copy "in" to "out".
                (bitvector-copy! out in)
                ;; Kill expressions that don't commute.
                (cond
                 ((causes-all-effects? fx)
                  ;; Fast-path if this expression clobbers the world.
                  (intersect! out always-avail))
                 ((not (causes-effect? fx &write))
                  ;; Fast-path if this expression clobbers nothing.
                  #t)
                 (else
                  ;; Loop of sadness.
                  (bitvector-copy! tmp out)
                  (bit-set*! tmp always-avail #f)
                  (let lp ((i 0))
                    (let ((i (bit-position #t tmp i)))
                      (when i
                        (when (effect-clobbers? fx (vector-ref effects i))
                          (bitvector-set! out i #f))
                        (lp (1+ i))))))))
              (bitvector-set! out n #t)
              (lp (1+ n) first? (or changed? (not (= prev-count new-count)))))))
         (else
          (if (or first? changed?)
              (lp 0 #f #f)
              (values avail-in effects))))))))

(define (compute-truthy-expressions dfg min-label label-count)
  "Compute a \"truth map\", indicating which expressions can be shown to
be true and/or false at each of LABEL-COUNT expressions in DFG, starting
from MIN-LABEL.  Returns a vector of bitvectors, each bitvector twice as
long as LABEL-COUNT.  The first half of the bitvector indicates labels
that may be true, and the second half those that may be false.  It could
be that both true and false proofs are available."
  (let ((boolv (make-vector label-count #f)))
    (define (label->idx label) (- label min-label))
    (define (idx->label idx) (+ idx min-label))
    (define (true-idx idx) idx)
    (define (false-idx idx) (+ idx label-count))

    (let lp ((n 0))
      (when (< n label-count)
        (let ((bool (make-bitvector (* label-count 2) #f)))
          (vector-set! boolv n bool)
          (lp (1+ n)))))

    (let ((tmp (make-bitvector (* label-count 2) #f)))
      (define (bitvector-copy! dst src)
        (bitvector-fill! dst #f)
        (bit-set*! dst src #t))
      (define (intersect! dst src)
        (bitvector-copy! tmp src)
        (bit-invert! tmp)
        (bit-set*! dst tmp #f))
      (let lp ((n 0) (first? #t) (changed? #f))
        (cond
         ((< n label-count)
          (let* ((label (idx->label n))
                 (bool (vector-ref boolv n))
                 (prev-count (bit-count #t bool)))
            ;; Intersect truthiness from all predecessors.
            (let lp ((preds (lookup-predecessors label dfg))
                     (initialized? #f))
              (match preds
                (() #t)
                ((pred . preds)
                 (let ((pidx (label->idx pred)))
                   (cond
                    ((and first? (<= n pidx))
                     ;; Avoid intersecting back-edges and cross-edges on
                     ;; the first iteration.
                     (lp preds initialized?))
                    (else
                     (if initialized?
                         (intersect! bool (vector-ref boolv pidx))
                         (bitvector-copy! bool (vector-ref boolv pidx)))
                     (match (lookup-cont pred dfg)
                       (($ $kargs _ _ term)
                        (match (find-call term)
                          (($ $continue kf ($ $branch kt exp))
                           (when (eqv? kt label)
                             (bitvector-set! bool (true-idx pidx) #t))
                           (when (eqv? kf label)
                             (bitvector-set! bool (false-idx pidx) #t)))
                          (_ #t)))
                       (_ #t))
                     (lp preds #t)))))))
            (lp (1+ n) first?
                (or changed?
                    (not (= prev-count (bit-count #t bool)))))))
         (else
          (if (or first? changed?)
              (lp 0 #f #f)
              boolv)))))))

;; Returns a map of label-idx -> (var-idx ...) indicating the variables
;; defined by a given labelled expression.
(define (compute-defs dfg min-label label-count)
  (define (cont-defs k)
    (match (lookup-cont k dfg)
      (($ $kargs names vars) vars)
      (_ '())))
  (define (idx->label idx) (+ idx min-label))
  (let ((defs (make-vector label-count '())))
    (let lp ((n 0))
      (when (< n label-count)
        (vector-set!
         defs
         n
         (match (lookup-cont (idx->label n) dfg)
           (($ $kargs _ _ body)
            (match (find-call body)
              (($ $continue k) (cont-defs k))))
           (($ $kreceive arity kargs)
            (cont-defs kargs))
           (($ $kclause arity ($ $cont kargs ($ $kargs names syms)))
            syms)
           (($ $kfun src meta self) (list self))
           (($ $ktail) '())))
        (lp (1+ n))))
    defs))

(define (compute-label-and-var-ranges fun)
  (match fun
    (($ $cont kfun ($ $kfun src meta self))
     ((make-local-cont-folder min-label label-count min-var var-count)
      (lambda (k cont min-label label-count min-var var-count)
        (let ((min-label (min k min-label))
              (label-count (1+ label-count)))
          (match cont
            (($ $kargs names vars body)
             (let lp ((body body)
                      (min-var (fold min min-var vars))
                      (var-count (+ var-count (length vars))))
               (match body
                 (($ $letrec names vars funs body)
                  (lp body
                      (fold min min-var vars)
                      (+ var-count (length vars))))
                 (($ $letk conts body) (lp body min-var var-count))
                 (_ (values min-label label-count min-var var-count)))))
            (($ $kfun src meta self)
             (values min-label label-count (min self min-var) (1+ var-count)))
            (_
             (values min-label label-count min-var var-count)))))
      fun kfun 0 self 0))))

;; Compute a vector containing, for each node, a list of the nodes that
;; it immediately dominates.  These are the "D" edges in the DJ tree.

(define (compute-equivalent-subexpressions fun dfg)
  (define (compute min-label label-count min-var var-count avail effects)
    (let ((idoms (compute-idoms dfg min-label label-count))
          (defs (compute-defs dfg min-label label-count))
          (var-substs (make-vector var-count #f))
          (equiv-labels (make-vector label-count #f))
          (equiv-set (make-hash-table)))
      (define (idx->label idx) (+ idx min-label))
      (define (label->idx label) (- label min-label))
      (define (idx->var idx) (+ idx min-var))
      (define (var->idx var) (- var min-var))

      (define (for-each/2 f l1 l2)
        (unless (= (length l1) (length l2))
          (error "bad lengths" l1 l2))
        (let lp ((l1 l1) (l2 l2))
          (when (pair? l1)
            (f (car l1) (car l2))
            (lp (cdr l1) (cdr l2)))))

      (define (subst-var var)
        ;; It could be that the var is free in this function; if so, its
        ;; name will be less than min-var.
        (let ((idx (var->idx var)))
          (if (<= 0 idx)
              (vector-ref var-substs idx)
              var)))

      (define (compute-exp-key exp)
        (match exp
          (($ $void) 'void)
          (($ $const val) (cons 'const val))
          (($ $prim name) (cons 'prim name))
          (($ $fun free body) #f)
          (($ $call proc args) #f)
          (($ $callk k proc args) #f)
          (($ $primcall name args)
           (cons* 'primcall name (map subst-var args)))
          (($ $branch _ ($ $primcall name args))
           (cons* 'primcall name (map subst-var args)))
          (($ $branch) #f)
          (($ $values args) #f)
          (($ $prompt escape? tag handler) #f)))

      (define (add-auxiliary-definitions! label exp-key)
        (let ((defs (vector-ref defs (label->idx label))))
          (define (add-def! aux-key var)
            (let ((equiv (hash-ref equiv-set aux-key '())))
              (hash-set! equiv-set aux-key
                         (acons label (list var) equiv))))
          (match exp-key
            (('primcall 'box val)
             (match defs
               ((box)
                (add-def! `(primcall box-ref ,(subst-var box)) val))))
            (('primcall 'box-set! box val)
             (add-def! `(primcall box-ref ,box) val))
            (('primcall 'cons car cdr)
             (match defs
               ((pair)
                (add-def! `(primcall car ,(subst-var pair)) car)
                (add-def! `(primcall cdr ,(subst-var pair)) cdr))))
            (('primcall 'set-car! pair car)
             (add-def! `(primcall car ,pair) car))
            (('primcall 'set-cdr! pair cdr)
             (add-def! `(primcall cdr ,pair) cdr))
            (('primcall (or 'make-vector 'make-vector/immediate) len fill)
             (match defs
               ((vec)
                (add-def! `(primcall vector-length ,(subst-var vec)) len))))
            (('primcall 'vector-set! vec idx val)
             (add-def! `(primcall vector-ref ,vec ,idx) val))
            (('primcall 'vector-set!/immediate vec idx val)
             (add-def! `(primcall vector-ref/immediate ,vec ,idx) val))
            (('primcall (or 'allocate-struct 'allocate-struct/immediate)
                        vtable size)
             (match defs
               ((struct)
                (add-def! `(primcall struct-vtable ,(subst-var struct))
                          vtable))))
            (('primcall 'struct-set! struct n val)
             (add-def! `(primcall struct-ref ,struct ,n) val))
            (('primcall 'struct-set!/immediate struct n val)
             (add-def! `(primcall struct-ref/immediate ,struct ,n) val))
            (_ #t))))

      ;; The initial substs vector is the identity map.
      (let lp ((var min-var))
        (when (< (var->idx var) var-count)
          (vector-set! var-substs (var->idx var) var)
          (lp (1+ var))))

      ;; Traverse the labels in fun in forward order, which will visit
      ;; dominators first.
      (let lp ((label min-label))
        (when (< (label->idx label) label-count)
          (match (lookup-cont label dfg)
            (($ $kargs names vars body)
             (match (find-call body)
               (($ $continue k src exp)
                (let* ((exp-key (compute-exp-key exp))
                       (equiv (hash-ref equiv-set exp-key '()))
                       (lidx (label->idx label))
                       (fx (vector-ref effects lidx))
                       (avail (vector-ref avail lidx)))
                  (let lp ((candidates equiv))
                    (match candidates
                      (()
                       ;; No matching expressions.  Add our expression
                       ;; to the equivalence set, if appropriate.  Note
                       ;; that expressions that allocate a fresh object
                       ;; or change the current fluid environment can't
                       ;; be eliminated by CSE (though DCE might do it
                       ;; if the value proves to be unused, in the
                       ;; allocation case).
                       (when (and exp-key
                                  (not (causes-effect? fx &allocation))
                                  (not (effect-clobbers?
                                        fx
                                        (&read-object &fluid))))
                         (hash-set! equiv-set exp-key
                                    (acons label (vector-ref defs lidx)
                                           equiv))))
                      (((and head (candidate . vars)) . candidates)
                       (cond
                        ((not (bitvector-ref avail (label->idx candidate)))
                         ;; This expression isn't available here; try
                         ;; the next one.
                         (lp candidates))
                        (else
                         ;; Yay, a match.  Mark expression as equivalent.
                         (vector-set! equiv-labels lidx head)
                         ;; If we dominate the successor, mark vars
                         ;; for substitution.
                         (when (= label (vector-ref idoms (label->idx k)))
                           (for-each/2
                            (lambda (var subst-var)
                              (vector-set! var-substs (var->idx var) subst-var))
                            (vector-ref defs lidx)
                            vars)))))))
                  ;; If this expression defines auxiliary definitions,
                  ;; as `cons' does for the results of `car' and `cdr',
                  ;; define those.  Do so after finding equivalent
                  ;; expressions, so that we can take advantage of
                  ;; subst'd output vars.
                  (add-auxiliary-definitions! label exp-key)))))
            (_ #f))
          (lp (1+ label))))
      (values (compute-dom-edges idoms min-label)
              equiv-labels min-label var-substs min-var)))

  (call-with-values (lambda () (compute-label-and-var-ranges fun))
    (lambda (min-label label-count min-var var-count)
      (call-with-values
          (lambda ()
            (compute-available-expressions dfg min-label label-count))
        (lambda (avail effects)
          (compute min-label label-count min-var var-count avail effects))))))

(define (apply-cse fun dfg
                   doms equiv-labels min-label var-substs min-var boolv)
  (define (idx->label idx) (+ idx min-label))
  (define (label->idx label) (- label min-label))
  (define (idx->var idx) (+ idx min-var))
  (define (var->idx var) (- var min-var))
  (define (true-idx idx) idx)
  (define (false-idx idx) (+ idx (vector-length equiv-labels)))

  (define (subst-var var)
    ;; It could be that the var is free in this function; if so,
    ;; its name will be less than min-var.
    (let ((idx (var->idx var)))
      (if (<= 0 idx)
          (vector-ref var-substs idx)
          var)))

  (define (visit-fun-cont cont)
    (rewrite-cps-cont cont
      (($ $cont label ($ $kargs names vars body))
       (label ($kargs names vars ,(visit-term body label))))
      (($ $cont label ($ $kfun src meta self tail clause))
       (label ($kfun src meta self ,tail
                ,(and clause (visit-fun-cont clause)))))
      (($ $cont label ($ $kclause arity ($ $cont kbody body) alternate))
       (label ($kclause ,arity ,(visit-cont kbody body)
                        ,(and alternate (visit-fun-cont alternate)))))))

  (define (visit-cont label cont)
    (rewrite-cps-cont cont
      (($ $kargs names vars body)
       (label ($kargs names vars ,(visit-term body label))))
      (_ (label ,cont))))

  (define (visit-term term label)
    (define (visit-exp exp)
      ;; We shouldn't see $fun here.
      (rewrite-cps-exp exp
        ((or ($ $void) ($ $const) ($ $prim)) ,exp)
        (($ $call proc args)
         ($call (subst-var proc) ,(map subst-var args)))
        (($ $callk k proc args)
         ($callk k (subst-var proc) ,(map subst-var args)))
        (($ $primcall name args)
         ($primcall name ,(map subst-var args)))
        (($ $branch k exp)
         ($branch k ,(visit-exp exp)))
        (($ $values args)
         ($values ,(map subst-var args)))
        (($ $prompt escape? tag handler)
         ($prompt escape? (subst-var tag) handler))))

    (define (visit-exp* k src exp)
      (match exp
        (($ $fun free body)
         (build-cps-term
           ($continue k src
             ($fun (map subst-var free) ,(cse body dfg)))))
        (_
         (cond
          ((vector-ref equiv-labels (label->idx label))
           => (match-lambda
               ((equiv . vars)
                (let* ((eidx (label->idx equiv)))
                  (match exp
                    (($ $branch kt exp)
                     (let* ((bool (vector-ref boolv (label->idx label)))
                            (t (bitvector-ref bool (true-idx eidx)))
                            (f (bitvector-ref bool (false-idx eidx))))
                       (if (eqv? t f)
                           (build-cps-term
                             ($continue k src
                               ($branch kt ,(visit-exp exp))))
                           (build-cps-term
                             ($continue (if t kt k) src ($values ()))))))
                    (_
                     ;; FIXME: can we always continue with $values?  why
                     ;; or why not?
                     (rewrite-cps-term (lookup-cont k dfg)
                       (($ $kargs)
                        ($continue k src ($values vars)))
                       (_
                        ($continue k src ,(visit-exp exp))))))))))
          (else
           (build-cps-term
             ($continue k src ,(visit-exp exp))))))))

    (define (visit-dom-conts label)
      (let ((cont (lookup-cont label dfg)))
        (match cont
          (($ $ktail) '())
          (($ $kargs) (list (visit-cont label cont)))
          (else
           (cons (visit-cont label cont)
                 (append-map visit-dom-conts
                             (vector-ref doms (label->idx label))))))))

    (rewrite-cps-term term
      (($ $letk conts body)
       ,(visit-term body label))
      (($ $letrec names syms funs body)
       ($letrec names syms
                (map (lambda (fun)
                       (rewrite-cps-exp fun
                         (($ $fun free body)
                          ($fun (map subst-var free) ,(cse body dfg)))))
                     funs)
         ,(visit-term body label)))
      (($ $continue k src exp)
       ,(let ((conts (append-map visit-dom-conts
                                 (vector-ref doms (label->idx label)))))
          (if (null? conts)
              (visit-exp* k src exp)
              (build-cps-term
                ($letk ,conts ,(visit-exp* k src exp))))))))

  (visit-fun-cont fun))

(define (cse fun dfg)
  (call-with-values (lambda () (compute-equivalent-subexpressions fun dfg))
    (lambda (doms equiv-labels min-label var-substs min-var)
      (apply-cse fun dfg doms equiv-labels min-label var-substs min-var
                 (compute-truthy-expressions dfg
                                             min-label (vector-length doms))))))

(define (eliminate-common-subexpressions fun)
  (call-with-values (lambda () (renumber fun))
    (lambda (fun nlabels nvars)
      (cse fun (compute-dfg fun)))))
