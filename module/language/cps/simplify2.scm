;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

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

(define-module (language cps simplify2)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:export (simplify2))

;; advantages of cps2: little recursion so evaluator doesn't consume too
;; much stack at boot time, rewriting can share more state for conts
;; that don't need rewrites, transformations can use dominators instead
;; of scoping approximation, redomination isn't a thing that needs to
;; happen, more functional techniques... easy detection of when no
;; transformation is necessary, transform-conts...

(define-syntax build-term
  (syntax-rules (unquote $rec $continue)
    ((_ (unquote exp))
     exp)
    ((_ ($continue k src exp))
     (build-cps-term ($continue k src exp)))))

(define-syntax-rule (build-cont-body cont)
  (match (build-cps-cont (#f cont))
    (($ $cont k x) x)))

(define-syntax build-cont
  (syntax-rules (unquote $kreceive $kargs $kfun $ktail $kclause)
    ((_ (unquote exp))
     exp)
    ((_ ($kreceive req rest kargs))
     (build-cont-body ($kreceive req rest kargs)))
    ((_ ($kargs (name ...) (unquote syms) body))
     (build-cont-body ($kargs (name ...) (unquote syms)
                        ,(build-term body))))
    ((_ ($kargs (name ...) (sym ...) body))
     (build-cont-body ($kargs (name ...) (sym ...) ,(build-term body))))
    ((_ ($kargs names syms body))
     (build-cont-body ($kargs names syms ,(build-term body))))
    ((_ ($kfun src meta self ktail kclause))
     (build-cont-body ($kfun src meta self ,ktail ,kclause)))
    ((_ ($ktail))
     (build-cont-body ($ktail)))
    ((_ ($kclause arity cont alternate))
     (build-cont-body ($kclause arity ,cont ,alternate)))))

(define-syntax-rule (rewrite-term x (pat term) ...)
  (match x
    (pat (build-term term))
    ...))

(define-syntax-rule (rewrite-cont x (pat cont) ...)
  (match x
    (pat (build-cont cont))
    ...))

(define (fun->conts fun)
  (define conts empty-intmap)
  (define (visit-cont-body cont)
    (rewrite-cont cont
      (($ $kargs names syms body)
       ($kargs names syms ,(visit-term body)))
      (($ $kfun src meta self tail clause)
       ($kfun src meta self (visit-cont tail)
         (and clause (visit-cont clause))))
      (($ $kclause arity body alternate)
       ($kclause ,arity (visit-cont body)
                 (and alternate (visit-cont alternate))))
      (($ $kreceive)
       ,cont)
      (($ $ktail)
       ,cont)))
  (define (visit-cont cont)
    (match cont
      (($ $cont label cont)
       (let ((cont (visit-cont-body cont)))
         (set! conts (intmap-add! conts label cont)))
       label)))
  (define (visit-term term)
    (match term
      (($ $letk conts body)
       (for-each visit-cont conts)
       (visit-term body))
      (($ $continue k src (and ($ $fun) fun))
       (build-term ($continue k src ,(visit-fun fun))))
      (($ $continue k src ($ $rec names syms funs))
       (build-term ($continue k src ($rec names syms (map visit-fun funs)))))
      (($ $continue k src exp)
       term)))
  (define (visit-fun fun)
    (rewrite-cps-exp fun
      (($ $fun body)
       ($fun ,(visit-cont body)))))
  (let ((kfun (visit-cont fun)))
    (values (persistent-intmap conts) kfun)))

(define (compute-function-body conts kfun)
  (persistent-intset
   (let visit-cont ((label kfun) (labels empty-intset))
     (cond
      ((intset-ref labels label) labels)
      (else
       (let ((labels (intset-add! labels label)))
         (match (intmap-ref conts label)
           (($ $kreceive arity k) (visit-cont k labels))
           (($ $kfun src meta self ktail kclause)
            (let ((labels (visit-cont ktail labels)))
              (if kclause
                  (visit-cont kclause labels)
                  labels)))
           (($ $ktail) labels)
           (($ $kclause arity kbody kalt)
            (if kalt
                (visit-cont kalt (visit-cont kbody labels))
                (visit-cont kbody labels)))
           (($ $kargs names syms ($ $continue k src exp))
            (visit-cont k (match exp
                            (($ $branch k)
                             (visit-cont k labels))
                            (($ $callk k)
                             (visit-cont k labels))
                            (($ $prompt escape? tag k)
                             (visit-cont k labels))
                            (_ labels)))))))))))

(define-inlinable (fold1 f l s0)
  (let lp ((l l) (s0 s0))
    (match l
      (() s0)
      ((elt . l) (lp l (f elt s0))))))

(define-inlinable (fold2 f l s0 s1)
  (let lp ((l l) (s0 s0) (s1 s1))
    (match l
      (() (values s0 s1))
      ((elt . l)
       (call-with-values (lambda () (f elt s0 s1))
         (lambda (s0 s1)
           (lp l s0 s1)))))))

#;
(define (intset-fold f set seed)
  (let lp ((i 0) (seed seed))
    (match (intset-next set i)
      (#f seed)
      (i (lp (1+ i) (f i seed))))))

#;
(define (intset-fold2 f set s0 s1)
  (let lp ((i 0) (s0 s0) (s1 s1))
    (match (intset-next set i)
      (#f (values s0 s1))
      (i (call-with-values (lambda () (f i s0 s1))
           (lambda (s0 s1)
             (lp (1+ i) s0 s1)))))))

(define (intset->intmap f set)
  (persistent-intmap
   (intset-fold (lambda (label preds)
                  (intmap-add! preds label (f label)))
                set empty-intmap)))

#;
(define (intmap-fold f map seed)
  (let lp ((i 0) (seed seed))
    (match (intmap-next map i)
      (#f seed)
      (i (lp (1+ i) (f i (intmap-ref map i) seed))))))

(define* (compute-predecessors conts kfun #:key
                               (labels (compute-function-body conts kfun)))
  (define (meet cdr car)
    (cons car cdr))
  (define (add-preds label preds)
    (define (add-pred k preds)
      (intmap-add! preds k label meet))
    (match (intmap-ref conts label)
      (($ $kreceive arity k)
       (add-pred k preds))
      (($ $kfun src meta self ktail kclause)
       (add-pred ktail (if kclause (add-pred kclause preds) preds)))
      (($ $ktail)
       preds)
      (($ $kclause arity kbody kalt)
       (add-pred kbody (if kalt (add-pred kalt preds) preds)))
      (($ $kargs names syms ($ $continue k src exp))
       (add-pred k
                 (match exp
                   (($ $branch k) (add-pred k preds))
                   (($ $prompt _ _ k) (add-pred k preds))
                   (_ preds))))))
  (persistent-intmap
   (intset-fold add-preds labels
                (intset->intmap (lambda (label) '()) labels))))

(define (worklist-fold f in out)
  (if (eq? in empty-intset)
      out
      (call-with-values (lambda () (f in out))
        (lambda (in out)
          (worklist-fold f in out)))))

(define (worklist-fold2 f in out0 out1)
  (if (eq? in empty-intset)
      (values out0 out1)
      (call-with-values (lambda () (f in out0 out1))
        (lambda (in out0 out1)
          (worklist-fold2 f in out0 out1)))))

(define* (compute-tail-path-lengths conts kfun preds)
  (define (add-lengths labels lengths length)
    (intset-fold (lambda (label lengths)
                   (intmap-add! lengths label length))
                 labels
                 lengths))
  (define (compute-next labels lengths)
    (intset-fold (lambda (label labels)
                   (fold1 (lambda (pred labels)
                            (if (intmap-ref lengths pred)
                                labels
                                (intset-add! labels pred)))
                          (intmap-ref preds label)
                          labels))
                 labels
                 empty-intset))
  (define (visit labels lengths length)
    (let ((lengths (add-lengths labels lengths length)))
      (values (compute-next labels lengths) lengths (1+ length))))
  (match (intmap-ref conts kfun)
    (($ $kfun src meta self tail clause)
     (worklist-fold2 visit (intset-add empty-intset tail) empty-intmap 0))))


;; Topologically sort the continuation tree starting at k0, using
;; reverse post-order numbering.
(define (sort-labels-locally conts k0 path-lengths)
  (let ((order '())
        (visited empty-intset))
    (define (visit k)
      (define (maybe-visit k)
        (unless (intset-ref visited k)
          (visit k)))
      (define (visit-successors k)
        (match (intmap-ref conts k)
          (($ $kargs names syms ($ $continue k src exp))
           (match exp
             (($ $prompt escape? tag handler)
              (maybe-visit handler)
              (maybe-visit k))
             (($ $branch kt)
              ;; Visit the successor with the shortest path length
              ;; to the tail first, so that if the branches are
              ;; unsorted, the longer path length will appear
              ;; first.  This will move a loop exit out of a loop.
              (let ((k-len (intmap-ref path-lengths k))
                    (kt-len (intmap-ref path-lengths kt)))
                (cond
                 ((if kt-len
                      (or (not k-len)
                          (< k-len kt-len)
                          ;; If the path lengths are the
                          ;; same, preserve original order
                          ;; to avoid squirreliness.
                          (and (= k-len kt-len) (< kt k)))
                      (if k-len #f (< kt k)))
                  (maybe-visit k)
                  (maybe-visit kt))
                 (else
                  (maybe-visit kt)
                  (maybe-visit k)))))
             (_
              (maybe-visit k))))
          (($ $kreceive arity k) (maybe-visit k))
          (($ $kclause arity kbody kalt)
           (when kalt (visit kalt))
           (maybe-visit kbody))
          (($ $kfun src meta self tail clause)
           (visit tail)
           (when clause (visit clause)))
          (_ #f)))

      ;; Mark this continuation as visited.
      (set! visited (intset-add! visited k))

      ;; Visit unvisited successors.
      (visit-successors k)

      ;; Add k to the reverse post-order.
      (set! order (cons k order)))

    ;; Recursively visit all continuations reachable from k0.
    (visit k0)

    ;; Return the sorted order.
    order))

(define (compute-renaming conts kfun)
  ;; labels := old -> new
  ;; vars := old -> new
  (define *next-label* -1)
  (define *next-var* -1)
  (define (rename-label label labels)
    (set! *next-label* (1+ *next-label*))
    (intmap-add! labels label *next-label*))
  (define (rename-var sym vars)
    (set! *next-var* (1+ *next-var*))
    (intmap-add! vars sym *next-var*))
  (define (rename label labels vars)
    (values (rename-label label labels)
            (match (intmap-ref conts label)
              (($ $kargs names syms exp)
               (fold1 rename-var syms vars))
              (($ $kfun src meta self tail clause)
               (rename-var self vars))
              (_ vars))))
  (define (visit-nested-funs k labels vars)
    (match (intmap-ref conts k)
      (($ $kargs names syms ($ $continue k src ($ $fun kfun)))
       (visit-fun kfun labels vars))
      (($ $kargs names syms ($ $continue k src ($ $rec names* syms*
                                                  (($ $fun kfun) ...))))
       (fold2 visit-fun kfun labels vars))
      (_ (values labels vars))))
  (define (visit-fun kfun labels vars)
    (let* ((preds (compute-predecessors conts kfun))
           (path-lengths (compute-tail-path-lengths conts kfun preds))
           (order (sort-labels-locally conts kfun path-lengths)))
      ;; First rename locally, then recurse on nested functions.
      (let-values (((labels vars) (fold2 rename order labels vars)))
        (fold2 visit-nested-funs order labels vars))))
  (let-values (((labels vars) (visit-fun kfun empty-intmap empty-intmap)))
    (values (persistent-intmap labels) (persistent-intmap vars))))

(define (renumber conts kfun)
  (let-values (((label-map var-map) (compute-renaming conts kfun)))
    (define (rename-label label)
      (or (intmap-ref label-map label) (error "what" label)))
    (define (rename-var var)
      (or (intmap-ref var-map var) (error "what2" var)))
    (define (rename-exp exp)
      (rewrite-cps-exp exp
        ((or ($ $const) ($ $prim)) ,exp)
        (($ $closure k nfree)
         ($closure (rename-label k) nfree))
        (($ $fun body)
         ($fun ,(rename-label body)))
        (($ $rec names vars funs)
         ($rec names (map rename-var vars) (map rename-exp funs)))
        (($ $values args)
         ($values ,(map rename-var args)))
        (($ $call proc args)
         ($call (rename-var proc) ,(map rename-var args)))
        (($ $callk k proc args)
         ($callk (rename-label k) (rename-var proc) ,(map rename-var args)))
        (($ $branch kt exp)
         ($branch (rename-label kt) ,(rename-exp exp)))
        (($ $primcall name args)
         ($primcall name ,(map rename-var args)))
        (($ $prompt escape? tag handler)
         ($prompt escape? (rename-var tag) (rename-label handler)))))
    (define (rename-arity arity)
      (match arity
        (($ $arity req opt rest () aok?)
         arity)
        (($ $arity req opt rest kw aok?)
         (match kw
           (() arity)
           (((kw kw-name kw-var) ...)
            (let ((kw (map list kw kw-name (map rename-var kw-var))))
              (make-$arity req opt rest kw aok?)))))))
    (persistent-intmap
     (intmap-fold
      (lambda (old-k new-k out)
        (intmap-add!
         out
         new-k
         (rewrite-cont (intmap-ref conts old-k)
                       (($ $kargs names syms ($ $continue k src exp))
                        ($kargs names (map rename-var syms)
                          ($continue (rename-label k) src ,(rename-exp exp))))
                       (($ $kreceive ($ $arity req () rest () #f) k)
                        ($kreceive req rest (rename-label k)))
                       (($ $ktail)
                        ($ktail))
                       (($ $kfun src meta self tail clause)
                        ($kfun src meta (rename-var self) (rename-label tail)
                          (and clause (rename-label clause))))
                       (($ $kclause arity body alternate)
                        ($kclause ,(rename-arity arity) (rename-label body)
                                  (and alternate (rename-label alternate)))))))
      label-map
      empty-intmap))))

(define (fixpoint f x)
  (let ((x* (f x)))
    (if (eq? x x*) x* (f x*))))

;; Precondition: For each function in CONTS, the continuation names are
;; topologically sorted.
(define* (compute-idoms* conts kfun)
  ;; This is the iterative O(n^2) fixpoint algorithm, originally from
  ;; Allen and Cocke ("Graph-theoretic constructs for program flow
  ;; analysis", 1972).  See the discussion in Cooper, Harvey, and
  ;; Kennedy's "A Simple, Fast Dominance Algorithm", 2001.
  (let ((preds-map (compute-predecessors conts kfun)))
    (define (compute-idom idoms preds)
      (match preds
        (() -1)
        ((pred) pred)                   ; Shortcut.
        ((pred . preds)
         (define (common-idom d0 d1)
           ;; We exploit the fact that a reverse post-order is a
           ;; topological sort, and so the idom of a node is always
           ;; numerically less than the node itself.
           (let lp ((d0 d0) (d1 d1))
             (cond
              ;; d0 or d1 can be false on the first iteration.
              ((not d0) d1)
              ((not d1) d0)
              ((= d0 d1) d0)
              ((< d0 d1) (lp d0 (intmap-ref idoms d1)))
              (else (lp (intmap-ref idoms d0) d1)))))
         (fold1 common-idom preds pred))))
    (define (adjoin-idom label preds idoms)
      (let ((idom (compute-idom idoms preds)))
        ;; Don't use intmap-add! here.
        (intmap-add idoms label idom (lambda (old new) new))))
    (fixpoint (lambda (idoms)
                (intmap-fold adjoin-idom preds-map idoms))
              empty-intmap)))

;; Compute a vector containing, for each node, a list of the nodes that
;; it immediately dominates.  These are the "D" edges in the DJ tree.
(define (compute-dom-edges* idoms)
  (define (snoc cdr car) (cons car cdr))
  (intmap-fold (lambda (label idom doms)
                 (let ((doms (intmap-add! doms label '())))
                   (cond
                    ((< idom 0) doms) ;; No edge to entry.
                    (else (intmap-add! doms idom label snoc)))))
               idoms
               empty-intmap))

;; Precondition: For each function in CONTS, the continuation names are
;; topologically sorted.
(define (conts->fun conts kentry)
  (define (convert-fun kfun)
    (let ((doms (compute-dom-edges* (compute-idoms* conts kfun))))
      (define (visit-cont label)
        (rewrite-cps-cont (intmap-ref conts label)
          (($ $kargs names syms body)
           (label ($kargs names syms ,(redominate label (visit-term body)))))
          ((and cont (or ($ $ktail) ($ $kreceive)))
           (label ,cont))))
      (define (visit-clause label)
        (and label
             (rewrite-cps-cont (intmap-ref conts label)
               (($ $kclause arity body alternate)
                (label ($kclause ,arity ,(visit-cont body)
                                 ,(visit-clause alternate)))))))
      (define (redominate label term)
        (define (visit-dom-conts label)
          (match (intmap-ref conts label)
            (($ $ktail) '())
            (($ $kargs) (list (visit-cont label)))
            (else
             (cons (visit-cont label)
                   (visit-dom-conts* (intmap-ref doms label))))))
        (define (visit-dom-conts* labels)
          (match labels
            (() '())
            ((label . labels)
             (append (visit-dom-conts label)
                     (visit-dom-conts* labels)))))
        (rewrite-cps-term (visit-dom-conts* (intmap-ref doms label))
          (() ,term)
          (conts ($letk ,conts ,term))))
      (define (visit-term term)
        (rewrite-cps-term term
          (($ $continue k src (and ($ $fun) fun))
           ($continue k src ,(visit-fun fun)))
          (($ $continue k src ($ $rec names syms funs))
           ($continue k src ($rec names syms (map visit-fun funs))))
          (($ $continue k src exp)
           ,term)))
      (define (visit-fun fun)
        (rewrite-cps-exp fun
          (($ $fun body)
           ($fun ,(convert-fun body)))))

      (rewrite-cps-cont (intmap-ref conts kfun)
        (($ $kfun src meta self tail clause)
         (kfun ($kfun src meta self (tail ($ktail))
                 ,(visit-clause clause)))))))
  (convert-fun kentry))

;;; Continuations that simply forward their values to another may be
;;; elided via eta reduction over labels.
;;;
;;; There is an exception however: we must exclude strongly-connected
;;; components (SCCs).  The only kind of SCC we can build out of $values
;;; expressions are infinite loops.
;;;
;;; Condition A below excludes single-node SCCs.  Single-node SCCs
;;; cannot be reduced.
;;;
;;; Condition B conservatively excludes edges to labels already marked
;;; as candidates.  This prevents back-edges and so breaks SCCs, and is
;;; optimal if labels are sorted.  If the labels aren't sorted it's
;;; suboptimal but cheap.
(define (compute-eta-reductions conts kfun)
  (define (visit-fun kfun nested-funs eta)
    (let ((body (compute-function-body conts kfun)))
      (define (visit-cont label nested-funs eta)
        (match (intmap-ref conts label)
          (($ $kargs names vars ($ $continue k src ($ $values vars)))
           (values nested-funs
                   (intset-maybe-add! eta label
                                      (match (intmap-ref conts k)
                                        (($ $kargs)
                                         (and (not (eqv? label k)) ; A
                                              (not (intset-ref eta label)) ; B
                                              ))
                                        (_ #f)))))
          (($ $kargs _ _ ($ $continue _ _ ($ $fun kfun)))
           (values (intset-add! nested-funs kfun) eta))
          (($ $kargs _ _ ($ $continue _ _ ($ $rec _ _ (($ $fun kfun) ...))))
           (values (intset-add*! nested-funs kfun) eta))
          (_
           (values nested-funs eta))))
      (intset-fold2 visit-cont body nested-funs eta)))
  (define (visit-funs worklist eta)
    (intset-fold2 visit-fun worklist empty-intset eta))
  (persistent-intset
   (worklist-fold visit-funs (intset-add empty-intset kfun) empty-intset)))

(define (eta-reduce conts kfun)
  (let ((label-set (compute-eta-reductions conts kfun)))
    ;; Replace any continuation to a label in LABEL-SET with the label's
    ;; continuation.  The label will denote a $kargs continuation, so
    ;; only terms that can continue to $kargs need be taken into
    ;; account.
    (define (subst label)
      (if (intset-ref label-set label)
          (match (intmap-ref conts label)
            (($ $kargs _ _ ($ $continue k)) (subst k)))
          label))
    (transform-conts
     (lambda (label cont)
       (and (not (intset-ref label-set label))
            (rewrite-cont cont
              (($ $kargs names syms ($ $continue kf src ($ $branch kt exp)))
               ($kargs names syms
                 ($continue (subst kf) src ($branch (subst kt) ,exp))))
              (($ $kargs names syms ($ $continue k src exp))
               ($kargs names syms
                 ($continue (subst k) src ,exp)))
              (($ $kreceive ($ $arity req () rest () #f) k)
               ($kreceive req rest (subst k)))
              (($ $kclause arity body alt)
               ($kclause ,arity (subst body) alt))
              (_ ,cont))))
     conts)))

(define (compute-singly-referenced-labels conts body)
  (define (add-ref label single multiple)
    (define (ref k single multiple)
      (if (intset-ref single k)
          (values single (intset-add! multiple k))
          (values (intset-add! single k) multiple)))
    (define (ref0) (values single multiple))
    (define (ref1 k) (ref k single multiple))
    (define (ref2 k k*)
      (if k*
          (let-values (((single multiple) (ref k single multiple)))
            (ref k* single multiple))
          (ref1 k)))
    (match (intmap-ref conts label)
      (($ $kreceive arity k) (ref1 k))
      (($ $kfun src meta self ktail kclause) (ref2 ktail kclause))
      (($ $ktail) (ref0))
      (($ $kclause arity kbody kalt) (ref2 kbody kalt))
      (($ $kargs names syms ($ $continue k src exp))
       (ref2 k (match exp (($ $branch k) k) (($ $prompt _ _ k) k) (_ #f))))))
  (let*-values (((single multiple) (values empty-intset empty-intset))
                ((single multiple) (intset-fold2 add-ref body single multiple)))
    (intset-subtract (persistent-intset single)
                     (persistent-intset multiple))))

#;
(define (compute-singly-referenced-labels conts body)
  (define (add-ref label counts)
    (define (ref k counts) (intmap-add counts k 1 +))
    (define (ref0) counts)
    (define (ref1 k) (ref k counts))
    (define (ref2 k k*) (ref k (if k* (ref k* counts) counts)))
    (match (intmap-ref conts label)
      (($ $kreceive arity k) (ref1 k))
      (($ $kfun src meta self ktail kclause) (ref2 ktail kclause))
      (($ $ktail) (ref0))
      (($ $kclause arity kbody kalt) (ref2 kbody kalt))
      (($ $kargs names syms ($ $continue k src exp))
       (ref2 k (match exp (($ $branch k) k) (($ $prompt _ _ k) k) (_ #f))))))
  (intmap-fold (lambda (label count single)
                 (if (= count 1)
                     (intset-add single label)
                     single))
               (pk (intset-fold add-ref body empty-intmap))
               empty-intset))

(define (intset-maybe-add! set k add?)
  (if add? (intset-add! set k) set))
(define (intset-add* set k*)
  (let lp ((set set) (k* k*))
    (match k*
      ((k . k*) (lp (intset-add set k) k*))
      (() set))))
(define (intset-add*! set k*)
  (fold1 (lambda (k set) (intset-add! set k)) k* set))

(define (compute-beta-reductions conts kfun)
  (define (visit-fun kfun nested-funs beta)
    (let* ((body (compute-function-body conts kfun))
           (single (compute-singly-referenced-labels conts body)))
      (define (visit-cont label nested-funs beta)
        (match (intmap-ref conts label)
          ;; A continuation's body can be inlined in place of a $values
          ;; expression if the continuation is a $kargs.  It should only
          ;; be inlined if it is used only once, and not recursively.
          (($ $kargs _ _ ($ $continue k src ($ $values)))
           (values nested-funs
                   (intset-maybe-add! beta label
                                      (and (intset-ref single k)
                                           (match (intmap-ref conts k)
                                             (($ $kargs) #t)
                                             (_ #f))))))
          (($ $kargs _ _ ($ $continue _ _ ($ $fun kfun)))
           (values (intset-add nested-funs kfun) beta))
          (($ $kargs _ _ ($ $continue _ _ ($ $rec _ _ (($ $fun kfun) ...))))
           (values (intset-add* nested-funs kfun) beta))
          (_
           (values nested-funs beta))))
      (intset-fold2 visit-cont body nested-funs beta)))
  (define (visit-funs worklist beta)
    (intset-fold2 visit-fun worklist empty-intset beta))
  (persistent-intset
   (worklist-fold visit-funs (intset-add empty-intset kfun) empty-intset)))

(define (fold2* f l1 l2 seed)
  (let lp ((l1 l1) (l2 l2) (seed seed))
    (match (cons l1 l2)
      ((() . ()) seed)
      (((x1 . l1) . (x2 . l2)) (lp l1 l2 (f x1 x2 seed))))))

(define (compute-beta-var-substitutions conts label-set)
  (define (add-var-substs label var-map)
    (match (intmap-ref conts label)
      (($ $kargs _ _ ($ $continue k _ ($ $values vals)))
       (match (intmap-ref conts k)
         (($ $kargs names vars)
          (fold2* (lambda (var val var-map)
                    (intmap-add! var-map var val))
                  vars vals var-map))))))
  (intset-fold add-var-substs label-set empty-intmap))

(define (transform-conts f conts)
  (intmap-fold (lambda (k v out)
                 (let ((v* (f k v)))
                   (if (equal? v v*)
                       out
                       (intmap-add! out k v* (lambda (old new) new)))))
               conts
               conts))

(define (beta-reduce conts kfun)
  (let* ((label-set (compute-beta-reductions conts kfun))
         (var-map (compute-beta-var-substitutions conts label-set)))
    (define (subst var)
      (match (intmap-ref var-map var)
        (#f var)
        (val (subst val))))
    (define (transform-exp label k src exp)
      (if (intset-ref label-set label)
          (match (intmap-ref conts k)
            (($ $kargs _ _ ($ $continue k* src* exp*))
             (transform-exp k k* src* exp*)))
          (build-term
           ($continue k src
             ,(rewrite-cps-exp exp
                ((or ($ $const) ($ $prim) ($ $fun) ($ $rec))
                 ,exp)
                (($ $call proc args)
                 ($call (subst proc) ,(map subst args)))
                (($ $callk k proc args)
                 ($callk k (subst proc) ,(map subst args)))
                (($ $primcall name args)
                 ($primcall name ,(map subst args)))
                (($ $values args)
                 ($values ,(map subst args)))
                (($ $branch kt ($ $values (var)))
                 ($branch kt ($values ((subst var)))))
                (($ $branch kt ($ $primcall name args))
                 ($branch kt ($primcall name ,(map subst args))))
                (($ $prompt escape? tag handler)
                 ($prompt escape? (subst tag) handler)))))))
    (transform-conts
     (lambda (label cont)
       (match cont
         (($ $kargs names syms ($ $continue k src exp))
          (build-cont
           ($kargs names syms ,(transform-exp label k src exp))))
         (_ cont)))
     conts)))

(define (simplify2 fun)
  (let-values (((conts kfun) (fun->conts fun)))
    (let* ((conts (beta-reduce conts kfun))
           (conts (eta-reduce conts kfun)))
      ;; Renumbering prunes unreachable continuations.
      (conts->fun (renumber conts kfun) 0))))
