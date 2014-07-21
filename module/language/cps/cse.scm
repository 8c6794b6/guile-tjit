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
  #:use-module (language cps intset)
  #:use-module (rnrs bytevectors)
  #:export (eliminate-common-subexpressions))

(define (cont-successors cont)
  (match cont
    (($ $kargs names syms body)
     (let lp ((body body))
       (match body
         (($ $letk conts body) (lp body))
         (($ $letrec names vars funs body) (lp body))
         (($ $continue k src exp)
          (match exp
            (($ $prompt escape? tag handler) (list k handler))
            (($ $branch kt) (list k kt))
            (_ (list k)))))))

    (($ $kreceive arity k) (list k))

    (($ $kclause arity ($ $cont kbody)) (list kbody))

    (($ $kfun src meta self tail clause)
     (let lp ((clause clause))
       (match clause
         (($ $cont kclause ($ $kclause _ _ alt))
          (cons kclause (lp alt)))
         (#f '()))))

    (($ $kfun src meta self tail #f) '())

    (($ $ktail) '())))

(define (compute-available-expressions dfg min-label label-count idoms)
  "Compute and return the continuations that may be reached if flow
reaches a continuation N.  Returns a vector of intsets, whose first
index corresponds to MIN-LABEL, and so on."
  (let* ((effects (compute-effects dfg min-label label-count))
         ;; Vector of intsets, indicating that at a continuation N, the
         ;; values from continuations M... are available.
         (avail (make-vector label-count #f))
         (revisit-label #f))

    (define (label->idx label) (- label min-label))
    (define (idx->label idx) (+ idx min-label))
    (define (get-effects label) (vector-ref effects (label->idx label)))

    (define (propagate! pred succ out)
      (let* ((succ-idx (label->idx succ))
             (in (match (lookup-predecessors succ dfg)
                   ;; Fast path: normal control flow.
                   ((_) out)
                   ;; Slow path: control-flow join.
                   (_ (cond
                       ((vector-ref avail succ-idx)
                        => (lambda (in)
                             (intset-intersect in out)))
                       (else out))))))
        (when (and (<= succ pred)
                   (or (not revisit-label) (< succ revisit-label))
                   (not (eq? in (vector-ref avail succ-idx))))
          ;; Arrange to revisit if this is not a forward edge and the
          ;; available set changed.
          (set! revisit-label succ))
        (vector-set! avail succ-idx in)))

    (define (clobber label in)
      (let ((fx (get-effects label)))
        (cond
         ((not (causes-effect? fx &write))
          ;; Fast-path if this expression clobbers nothing.
          in)
         (else
          ;; Kill clobbered expressions.  There is no need to check on
          ;; any label before than the last dominating label that
          ;; clobbered everything.
          (let ((first (let lp ((dom label))
                         (let* ((dom (vector-ref idoms (label->idx dom))))
                           (and (< min-label dom)
                                (let ((fx (vector-ref effects (label->idx dom))))
                                  (if (causes-all-effects? fx)
                                      dom
                                      (lp dom))))))))
            (let lp ((i first) (in in))
              (cond
               ((intset-next in i)
                => (lambda (i)
                     (if (effect-clobbers? fx (vector-ref effects (label->idx i)))
                         (lp (1+ i) (intset-remove in i))
                         (lp (1+ i) in))))
               (else in))))))))

    (synthesize-definition-effects! effects dfg min-label label-count)

    (vector-set! avail 0 empty-intset)

    (let lp ((n 0))
      (cond
       ((< n label-count)
        (let* ((label (idx->label n))
               ;; It's possible for "in" to be #f if it has no
               ;; predecessors, as is the case for the ktail of a
               ;; function with an iloop.
               (in (or (vector-ref avail n) empty-intset))
               (out (intset-add (clobber label in) label)))
          (lookup-predecessors label dfg)
          (let visit-succs ((succs (cont-successors (lookup-cont label dfg))))
            (match succs
              (() (lp (1+ n)))
              ((succ . succs)
               (propagate! label succ out)
               (visit-succs succs))))))
       (revisit-label
        (let ((n (label->idx revisit-label)))
          (set! revisit-label #f)
          (lp n)))
       (else
        (values avail effects))))))

(define (compute-truthy-expressions dfg min-label label-count)
  "Compute a \"truth map\", indicating which expressions can be shown to
be true and/or false at each of LABEL-COUNT expressions in DFG, starting
from MIN-LABEL.  Returns a vector of intsets, each intset twice as long
as LABEL-COUNT.  The even elements of the intset indicate labels that
may be true, and the odd ones indicate those that may be false.  It
could be that both true and false proofs are available."
  (let ((boolv (make-vector label-count #f))
        (revisit-label #f))
    (define (label->idx label) (- label min-label))
    (define (idx->label idx) (+ idx min-label))
    (define (true-idx idx) (ash idx 1))
    (define (false-idx idx) (1+ (ash idx 1)))

    (define (propagate! pred succ out)
      (let* ((succ-idx (label->idx succ))
             (in (match (lookup-predecessors succ dfg)
                   ;; Fast path: normal control flow.
                   ((_) out)
                   ;; Slow path: control-flow join.
                   (_ (cond
                       ((vector-ref boolv succ-idx)
                        => (lambda (in)
                             (intset-intersect in out)))
                       (else out))))))
        (when (and (<= succ pred)
                   (or (not revisit-label) (< succ revisit-label))
                   (not (eq? in (vector-ref boolv succ-idx))))
          (set! revisit-label succ))
        (vector-set! boolv succ-idx in)))

    (vector-set! boolv 0 empty-intset)

    (let lp ((n 0))
      (cond
       ((< n label-count)
        (let* ((label (idx->label n))
               ;; It's possible for "in" to be #f if it has no
               ;; predecessors, as is the case for the ktail of a
               ;; function with an iloop.
               (in (or (vector-ref boolv n) empty-intset)))
          (define (default-propagate)
            (let visit-succs ((succs (cont-successors (lookup-cont label dfg))))
              (match succs
                (() (lp (1+ n)))
                ((succ . succs)
                 (propagate! label succ in)
                 (visit-succs succs)))))
          (match (lookup-cont label dfg)
            (($ $kargs names syms body)
             (match (find-call body)
               (($ $continue k src ($ $branch kt))
                (propagate! label k (intset-add in (false-idx n)))
                (propagate! label kt (intset-add in (true-idx n)))
                (lp (1+ n)))
               (_ (default-propagate))))
            (_ (default-propagate)))))
       (revisit-label
        (let ((n (label->idx revisit-label)))
          (set! revisit-label #f)
          (lp n)))
       (else boolv)))))

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
  (define (compute min-label label-count min-var var-count idoms avail effects)
    (let ((defs (compute-defs dfg min-label label-count))
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
               (() #f) ;; allocate-struct in tail or kreceive position.
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
                        ((not (intset-ref avail candidate))
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
      (let ((idoms (compute-idoms dfg min-label label-count)))
        (call-with-values
            (lambda ()
              (compute-available-expressions dfg min-label label-count idoms))
          (lambda (avail effects)
            (compute min-label label-count min-var var-count
                     idoms avail effects)))))))

(define (apply-cse fun dfg
                   doms equiv-labels min-label var-substs min-var boolv)
  (define (idx->label idx) (+ idx min-label))
  (define (label->idx label) (- label min-label))
  (define (idx->var idx) (+ idx min-var))
  (define (var->idx var) (- var min-var))
  (define (true-idx idx) (ash idx 1))
  (define (false-idx idx) (1+ (ash idx 1)))

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
                            (t (intset-ref bool (true-idx eidx)))
                            (f (intset-ref bool (false-idx eidx))))
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
