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
;;; Helper facilities for working with CPS.
;;;
;;; Code:

(define-module (language cps2 utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (language cps2)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:export (;; Fresh names.
            label-counter var-counter
            fresh-label fresh-var
            with-fresh-name-state compute-max-label-and-var
            let-fresh

            ;; Various utilities.
            fold1 fold2
            trivial-intset
            intmap-map
            intmap-keys
            invert-bijection invert-partition
            intset->intmap
            worklist-fold
            fixpoint

            ;; Flow analysis.
            compute-constant-values
            compute-function-body
            compute-reachable-functions
            compute-successors
            invert-graph
            compute-predecessors
            compute-reverse-post-order
            compute-strongly-connected-components
            compute-idoms
            compute-dom-edges
            ))

(define label-counter (make-parameter #f))
(define var-counter (make-parameter #f))

(define (fresh-label)
  (let ((count (or (label-counter)
                   (error "fresh-label outside with-fresh-name-state"))))
    (label-counter (1+ count))
    count))

(define (fresh-var)
  (let ((count (or (var-counter)
                   (error "fresh-var outside with-fresh-name-state"))))
    (var-counter (1+ count))
    count))

(define-syntax-rule (let-fresh (label ...) (var ...) body ...)
  (let* ((label (fresh-label)) ...
         (var (fresh-var)) ...)
    body ...))

(define-syntax-rule (with-fresh-name-state fun body ...)
  (call-with-values (lambda () (compute-max-label-and-var fun))
    (lambda (max-label max-var)
      (parameterize ((label-counter (1+ max-label))
                     (var-counter (1+ max-var)))
        body ...))))

(define (compute-max-label-and-var conts)
  (values (or (intmap-prev conts) -1)
          (intmap-fold (lambda (k cont max-var)
                         (match cont
                           (($ $kargs names syms body)
                            (apply max max-var syms))
                           (($ $kfun src meta self)
                            (max max-var self))
                           (_ max-var)))
                       conts
                       -1)))

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

(define (trivial-intset set)
  "Returns the sole member of @var{set}, if @var{set} has exactly one
member, or @code{#f} otherwise."
  (let ((first (intset-next set)))
    (and first
         (not (intset-next set (1+ first)))
         first)))

(define (intmap-map proc map)
  (persistent-intmap
   (intmap-fold (lambda (k v out) (intmap-replace! out k (proc k v)))
                map
                map)))

(define (intmap-keys map)
  "Return an intset of the keys in @var{map}."
  (persistent-intset
   (intmap-fold (lambda (k v keys) (intset-add! keys k)) map empty-intset)))

(define (invert-bijection map)
  "Assuming the values of @var{map} are integers and are unique, compute
a map in which each value maps to its key.  If the values are not
unique, an error will be signalled."
  (intmap-fold (lambda (k v out) (intmap-add out v k)) map empty-intmap))

(define (invert-partition map)
  "Assuming the values of @var{map} are disjoint intsets, compute a map
in which each member of each set maps to its key.  If the values are not
disjoint, an error will be signalled."
  (intmap-fold (lambda (k v* out)
                 (intset-fold (lambda (v out) (intmap-add out v k)) v* out))
               map empty-intmap))

(define (intset->intmap f set)
  (persistent-intmap
   (intset-fold (lambda (label preds)
                  (intmap-add! preds label (f label)))
                set empty-intmap)))

(define worklist-fold
  (case-lambda
    ((f in out)
     (let lp ((in in) (out out))
       (if (eq? in empty-intset)
           out
           (call-with-values (lambda () (f in out)) lp))))
    ((f in out0 out1)
     (let lp ((in in) (out0 out0) (out1 out1))
       (if (eq? in empty-intset)
           (values out0 out1)
           (call-with-values (lambda () (f in out0 out1)) lp))))))

(define fixpoint
  (case-lambda
    ((f x)
     (let lp ((x x))
       (let ((x* (f x)))
         (if (eq? x x*) x* (lp x*)))))
    ((f x0 x1)
     (let lp ((x0 x0) (x1 x1))
       (call-with-values (lambda () (f x0 x1))
         (lambda (x0* x1*)
           (if (and (eq? x0 x0*) (eq? x1 x1*))
               (values x0* x1*)
               (lp x0* x1*))))))))

(define (compute-defining-expressions conts)
  (define (meet-defining-expressions old new)
    ;; If there are multiple definitions, punt and
    ;; record #f.
    #f)
  (persistent-intmap
   (intmap-fold (lambda (label cont defs)
                  (match cont
                    (($ $kargs _ _ ($ $continue k src exp))
                     (match (intmap-ref conts k)
                       (($ $kargs (_) (var))
                        (intmap-add! defs var exp meet-defining-expressions))
                       (_ defs)))
                    (_ defs)))
                conts
                empty-intmap)))

(define (compute-constant-values conts)
  (persistent-intmap
   (intmap-fold (lambda (var exp out)
                  (match exp
                    (($ $const val)
                     (intmap-add! out var val))
                    (_ out)))
                (compute-defining-expressions conts)
                empty-intmap)))

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
                            (($ $prompt escape? tag k)
                             (visit-cont k labels))
                            (_ labels)))))))))))

(define (compute-reachable-functions conts kfun)
  "Compute a mapping LABEL->LABEL..., where each key is a reachable
$kfun and each associated value is the body of the function, as an
intset."
  (define (intset-cons i set) (intset-add set i))
  (define (visit-fun kfun body to-visit)
    (intset-fold
     (lambda (label to-visit)
       (define (return kfun*) (fold intset-cons to-visit kfun*))
       (define (return1 kfun) (intset-add to-visit kfun))
       (define (return0) to-visit)
       (match (intmap-ref conts label)
         (($ $kargs _ _ ($ $continue _ _ exp))
          (match exp
            (($ $fun label) (return1 label))
            (($ $rec _ _ (($ $fun labels) ...)) (return labels))
            (($ $closure label nfree) (return1 label))
            (($ $callk label) (return1 label))
            (_ (return0))))
         (_ (return0))))
     body
     to-visit))
  (let lp ((to-visit (intset kfun)) (visited empty-intmap))
    (let ((to-visit (intset-subtract to-visit (intmap-keys visited))))
      (if (eq? to-visit empty-intset)
          visited
          (call-with-values
              (lambda ()
                (intset-fold
                 (lambda (kfun to-visit visited)
                   (let ((body (compute-function-body conts kfun)))
                     (values (visit-fun kfun body to-visit)
                             (intmap-add visited kfun body))))
                 to-visit
                 empty-intset
                 visited))
            lp)))))

(define (compute-successors conts kfun)
  (define (visit label succs)
    (let visit ((label kfun) (succs empty-intmap))
      (define (propagate0)
        (intmap-add! succs label empty-intset))
      (define (propagate1 succ)
        (visit succ (intmap-add! succs label (intset succ))))
      (define (propagate2 succ0 succ1)
        (let ((succs (intmap-add! succs label (intset succ0 succ1))))
          (visit succ1 (visit succ0 succs))))
      (if (intmap-ref succs label (lambda (_) #f))
          succs
          (match (intmap-ref conts label)
            (($ $kargs names vars ($ $continue k src exp))
             (match exp
               (($ $branch kt) (propagate2 k kt))
               (($ $prompt escape? tag handler) (propagate2 k handler))
               (_ (propagate1 k))))
            (($ $kreceive arity k)
             (propagate1 k))
            (($ $kfun src meta self tail clause)
             (if clause
                 (propagate1 clause)
                 (propagate0)))
            (($ $kclause arity kbody kalt)
             (if kalt
                 (propagate2 kbody kalt)
                 (propagate1 kbody)))
            (($ $ktail) (propagate0))))))
  (persistent-intmap (visit kfun empty-intmap)))

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

(define (compute-reverse-post-order succs start)
  "Compute a reverse post-order numbering for a depth-first walk over
nodes reachable from the start node."
  (let visit ((label start) (order '()) (visited empty-intset))
    (call-with-values
        (lambda ()
          (intset-fold (lambda (succ order visited)
                         (if (intset-ref visited succ)
                             (values order visited)
                             (visit succ order visited)))
                       (intmap-ref succs label)
                       order
                       (intset-add! visited label)))
      (lambda (order visited)
        ;; After visiting successors, add label to the reverse post-order.
        (values (cons label order) visited)))))

(define (invert-graph succs)
  "Given a graph PRED->SUCC..., where PRED is a label and SUCC... is an
intset of successors, return a graph SUCC->PRED...."
  (intmap-fold (lambda (pred succs preds)
                 (intset-fold
                  (lambda (succ preds)
                    (intmap-add preds succ pred intset-add))
                  succs
                  preds))
               succs
               (intmap-map (lambda (label _) empty-intset) succs)))

(define (compute-strongly-connected-components succs start)
  "Given a LABEL->SUCCESSOR... graph, compute a SCC->LABEL... map
partitioning the labels into strongly connected components (SCCs)."
  (let ((preds (invert-graph succs)))
    (define (visit-scc scc sccs-by-label)
      (let visit ((label scc) (sccs-by-label sccs-by-label))
        (if (intmap-ref sccs-by-label label (lambda (_) #f))
            sccs-by-label
            (intset-fold visit
                         (intmap-ref preds label)
                         (intmap-add sccs-by-label label scc)))))
    (intmap-fold
     (lambda (label scc sccs)
       (let ((labels (intset-add empty-intset label)))
         (intmap-add sccs scc labels intset-union)))
     (fold visit-scc empty-intmap (compute-reverse-post-order succs start))
     empty-intmap)))

;; Precondition: For each function in CONTS, the continuation names are
;; topologically sorted.
(define (compute-idoms conts kfun)
  ;; This is the iterative O(n^2) fixpoint algorithm, originally from
  ;; Allen and Cocke ("Graph-theoretic constructs for program flow
  ;; analysis", 1972).  See the discussion in Cooper, Harvey, and
  ;; Kennedy's "A Simple, Fast Dominance Algorithm", 2001.
  (let ((preds-map (compute-predecessors conts kfun)))
    (define (compute-idom idoms preds)
      (define (idom-ref label)
        (intmap-ref idoms label (lambda (_) #f)))
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
              ((< d0 d1) (lp d0 (idom-ref d1)))
              (else (lp (idom-ref d0) d1)))))
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
(define (compute-dom-edges idoms)
  (define (snoc cdr car) (cons car cdr))
  (persistent-intmap
   (intmap-fold (lambda (label idom doms)
                  (let ((doms (intmap-add! doms label '())))
                    (cond
                     ((< idom 0) doms) ;; No edge to entry.
                     (else (intmap-add! doms idom label snoc)))))
                idoms
                empty-intmap)))
