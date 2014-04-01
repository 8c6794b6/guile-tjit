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
;;; Many passes rely on a local or global static analysis of a function.
;;; This module implements a simple data-flow graph (DFG) analysis,
;;; tracking the definitions and uses of variables and continuations.
;;; It also builds a table of continuations and scope links, to be able
;;; to easily determine if one continuation is in the scope of another,
;;; and to get to the expression inside a continuation.
;;;
;;; Note that the data-flow graph of continuation labels is a
;;; control-flow graph.
;;;
;;; We currently don't expose details of the DFG type outside this
;;; module, preferring to only expose accessors.  That may change in the
;;; future but it seems to work for now.
;;;
;;; Code:

(define-module (language cps dfg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:export (build-cont-table
            lookup-cont

            compute-dfg
            dfg-cont-table
            dfg-min-label
            dfg-label-count
            dfg-min-var
            dfg-var-count
            lookup-def
            lookup-uses
            lookup-predecessors
            lookup-successors
            lookup-block-scope
            find-call
            call-expression
            find-expression
            find-defining-expression
            find-constant-value
            continuation-bound-in?
            variable-free-in?
            constant-needs-allocation?
            control-point?
            lookup-bound-syms

            ;; Control flow analysis.
            analyze-control-flow
            cfa-k-idx cfa-k-count cfa-k-sym cfa-predecessors

            ;; Data flow analysis.
            compute-live-variables
            dfa-k-idx dfa-k-sym dfa-k-count dfa-k-in dfa-k-out
            dfa-var-idx dfa-var-sym dfa-var-count
            print-dfa))

;; These definitions are here because currently we don't do cross-module
;; inlining.  They can be removed once that restriction is gone.
(define-inlinable (for-each f l)
  (unless (list? l)
    (scm-error 'wrong-type-arg "for-each" "Not a list: ~S" (list l) #f))
  (let for-each1 ((l l))
    (unless (null? l)
      (f (car l))
      (for-each1 (cdr l)))))

(define-inlinable (for-each/2 f l1 l2)
  (unless (= (length l1) (length l2))
    (scm-error 'wrong-type-arg "for-each" "List of wrong length: ~S"
               (list l2) #f))
  (let for-each2 ((l1 l1) (l2 l2))
    (unless (null? l1)
      (f (car l1) (car l2))
      (for-each2 (cdr l1) (cdr l2)))))

(define (build-cont-table fun)
  (let ((max-k (fold-conts (lambda (k cont max-k) (max k max-k))
                           -1 fun)))
    (fold-conts (lambda (k cont table)
                  (vector-set! table k cont)
                  table)
                (make-vector (1+ max-k) #f)
                fun)))

;; Data-flow graph for CPS: both for values and continuations.
(define-record-type $dfg
  (make-dfg conts preds defs uses scopes scope-levels
            min-label label-count min-var var-count)
  dfg?
  ;; vector of label -> $kif, $kargs, etc
  (conts dfg-cont-table)
  ;; vector of label -> (pred-label ...)
  (preds dfg-preds)
  ;; vector of var -> def-label
  (defs dfg-defs)
  ;; vector of var -> (use-label ...)
  (uses dfg-uses)
  ;; vector of label -> label
  (scopes dfg-scopes)
  ;; vector of label -> int
  (scope-levels dfg-scope-levels)

  (min-label dfg-min-label)
  (label-count dfg-label-count)
  (min-var dfg-min-var)
  (var-count dfg-var-count))

;; Some analyses assume that the only relevant set of nodes is the set
;; that is reachable from some start node.  Others need to include nodes
;; that are reachable from an end node as well, or all nodes in a
;; function.  In that case pass an appropriate implementation of
;; fold-all-conts, as analyze-control-flow does.
(define (reverse-post-order k0 get-successors fold-all-conts)
  (let ((order '())
        (visited? (make-hash-table)))
    (let visit ((k k0))
      (hashq-set! visited? k #t)
      (for-each (lambda (k)
                  (unless (hashq-ref visited? k)
                    (visit k)))
                (get-successors k))
      (set! order (cons k order)))
    (list->vector (fold-all-conts
                   (lambda (k seed)
                     (if (hashq-ref visited? k)
                         seed
                         (begin
                           (hashq-set! visited? k #t)
                           (cons k seed))))
                   order))))

(define (make-block-mapping order)
  (let ((mapping (make-hash-table)))
    (let lp ((n 0))
      (when (< n (vector-length order))
        (hashq-set! mapping (vector-ref order n) n)
        (lp (1+ n))))
    mapping))

(define (convert-predecessors order get-predecessors)
  (let ((preds-vec (make-vector (vector-length order) #f)))
    (let lp ((n 0))
      (when (< n (vector-length order))
        (vector-set! preds-vec n (get-predecessors (vector-ref order n)))
        (lp (1+ n))))
    preds-vec))

;; Control-flow analysis.
(define-record-type $cfa
  (make-cfa k-map order preds)
  cfa?
  ;; Hash table mapping k-sym -> k-idx
  (k-map cfa-k-map)
  ;; Vector of k-idx -> k-sym, in reverse post order
  (order cfa-order)
  ;; Vector of k-idx -> list of k-idx
  (preds cfa-preds))

(define* (cfa-k-idx cfa k
                    #:key (default (lambda (k)
                                     (error "unknown k" k))))
  (or (hashq-ref (cfa-k-map cfa) k)
      (default k)))

(define (cfa-k-count cfa)
  (vector-length (cfa-order cfa)))

(define (cfa-k-sym cfa n)
  (vector-ref (cfa-order cfa) n))

(define (cfa-predecessors cfa n)
  (vector-ref (cfa-preds cfa) n))

(define-inlinable (vector-push! vec idx val)
  (let ((v vec) (i idx))
    (vector-set! v i (cons val (vector-ref v i)))))

(define (compute-reachable cfa dfg)
  "Given the forward control-flow analysis in CFA, compute and return
the continuations that may be reached if flow reaches a continuation N.
Returns a vector of bitvectors.  The given CFA should be a forward CFA,
for quickest convergence."
  (let* ((k-count (cfa-k-count cfa))
         ;; Vector of bitvectors, indicating that continuation N can
         ;; reach a set M...
         (reachable (make-vector k-count #f))
         ;; Vector of lists, indicating that continuation N can directly
         ;; reach continuations M...
         (succs (make-vector k-count '())))

    ;; All continuations are reachable from themselves.
    (let lp ((n 0))
      (when (< n k-count)
        (let ((bv (make-bitvector k-count #f)))
          (bitvector-set! bv n #t)
          (vector-set! reachable n bv)
          (lp (1+ n)))))

    ;; Initialize successor lists.
    (let lp ((n 0))
      (when (< n k-count)
        (for-each (lambda (succ)
                    (vector-push! succs n (cfa-k-idx cfa succ)))
                  (lookup-successors (cfa-k-sym cfa n) dfg))
        (lp (1+ n))))

    ;; Iterate cfa backwards, to converge quickly.
    (let ((tmp (make-bitvector k-count #f)))
      (let lp ((n k-count) (changed? #f))
        (cond
         ((zero? n)
          (if changed?
              (lp 0 #f)
              reachable))
         (else
          (let ((n (1- n)))
            (bitvector-fill! tmp #f)
            (for-each (lambda (succ)
                        (bit-set*! tmp (vector-ref reachable succ) #t))
                      (vector-ref succs n))
            (bitvector-set! tmp n #t)
            (bit-set*! tmp (vector-ref reachable n) #f)
            (cond
             ((bit-position #t tmp 0)
              (bit-set*! (vector-ref reachable n) tmp #t)
              (lp n #t))
             (else
              (lp n changed?))))))))))

(define (find-prompts cfa dfg)
  "Find the prompts in CFA, and return them as a list of PROMPT-INDEX,
HANDLER-INDEX pairs."
  (let lp ((n 0) (prompts '()))
    (cond
     ((= n (cfa-k-count cfa))
      (reverse prompts))
     (else
      (match (lookup-cont (cfa-k-sym cfa n) dfg)
        (($ $kargs names syms body)
         (match (find-expression body)
           (($ $prompt escape? tag handler)
            (lp (1+ n) (acons n (cfa-k-idx cfa handler) prompts)))
           (_ (lp (1+ n) prompts))))
        (_ (lp (1+ n) prompts)))))))

(define (compute-interval cfa dfg reachable start end)
  "Compute and return the set of continuations that may be reached from
START, inclusive, but not reached by END, exclusive.  Returns a
bitvector."
  (let ((body (make-bitvector (cfa-k-count cfa) #f)))
    (bit-set*! body (vector-ref reachable start) #t)
    (bit-set*! body (vector-ref reachable end) #f)
    body))

(define (find-prompt-bodies cfa dfg)
  "Find all the prompts in CFA, and compute the set of continuations
that is reachable from the prompt bodies but not from the corresponding
handler.  Returns a list of PROMPT, HANDLER, BODY lists, where the BODY
is a bitvector."
  (match (find-prompts cfa dfg)
    (() '())
    (((prompt . handler) ...)
     (let ((reachable (compute-reachable cfa dfg)))
       (map (lambda (prompt handler)
              ;; FIXME: It isn't correct to use all continuations
              ;; reachable from the prompt, because that includes
              ;; continuations outside the prompt body.  This point is
              ;; moot if the handler's control flow joins with the the
              ;; body, as is usually but not always the case.
              ;;
              ;; One counter-example is when the handler contifies an
              ;; infinite loop; in that case we compute a too-large
              ;; prompt body.  This error is currently innocuous, but
              ;; we should fix it at some point.
              ;;
              ;; The fix is to end the body at the corresponding "pop"
              ;; primcall, if any.
              (let ((body (compute-interval cfa dfg reachable prompt handler)))
                (list prompt handler body)))
            prompt handler)))))

(define* (visit-prompt-control-flow cfa dfg f #:key complete?)
  "For all prompts in CFA, invoke F with arguments PROMPT, HANDLER, and
BODY for each body continuation in the prompt."
  (for-each
   (match-lambda
    ((prompt handler body)
     (define (out-or-back-edge? n)
       ;; Most uses of visit-prompt-control-flow don't need every body
       ;; continuation, and would be happy getting called only for
       ;; continuations that postdominate the rest of the body.  Unless
       ;; you pass #:complete? #t, we only invoke F on continuations
       ;; that can leave the body, or on back-edges in loops.
       ;;
       ;; You would think that looking for the final "pop" primcall
       ;; would be sufficient, but that is incorrect; it's possible for
       ;; a loop in the prompt body to be contified, and that loop need
       ;; not continue to the pop if it never terminates.  The pop could
       ;; even be removed by DCE, in that case.
       (or-map (lambda (succ)
                 (let ((succ (cfa-k-idx cfa succ)))
                   (or (not (bitvector-ref body succ))
                       (<= succ n))))
               (lookup-successors (cfa-k-sym cfa n) dfg)))
     (let lp ((n 0))
       (let ((n (bit-position #t body n)))
         (when n
           (when (or complete? (out-or-back-edge? n))
             (f prompt handler n))
           (lp (1+ n)))))))
   (find-prompt-bodies cfa dfg)))

(define* (analyze-control-flow fun dfg #:key reverse? add-handler-preds?)
  (define (build-cfa kentry lookup-succs lookup-preds forward-cfa)
    (define (reachable-preds mapping)
      ;; It's possible for a predecessor to not be in the mapping, if
      ;; the predecessor is not reachable from the entry node.
      (lambda (k)
        (filter-map (cut hashq-ref mapping <>) (lookup-preds k dfg))))
    (let* ((order (reverse-post-order
                   kentry
                   (lambda (k)
                     ;; RPO numbering is going to visit this list of
                     ;; successors in the order that we give it.  Sort
                     ;; it so that all things being equal, we preserve
                     ;; the existing numbering order.  This also has the
                     ;; effect of preserving clause order.
                     (let ((succs (lookup-succs k dfg)))
                       (if (or (null? succs) (null? (cdr succs)))
                           succs
                           (sort succs >))))
                   (if forward-cfa
                       (lambda (f seed)
                         (let lp ((n (cfa-k-count forward-cfa)) (seed seed))
                           (if (zero? n)
                               seed
                               (lp (1- n)
                                   (f (cfa-k-sym forward-cfa (1- n)) seed)))))
                       (lambda (f seed) seed))))
           (k-map (make-block-mapping order))
           (preds (convert-predecessors order (reachable-preds k-map)))
           (cfa (make-cfa k-map order preds)))
      (when add-handler-preds?
        ;; Any expression in the prompt body could cause an abort to the
        ;; handler.  This code adds links from every block in the prompt
        ;; body to the handler.  This causes all values used by the
        ;; handler to be seen as live in the prompt body, as indeed they
        ;; are.
        (let ((forward-cfa (or forward-cfa cfa)))
          (visit-prompt-control-flow
           forward-cfa dfg
           (lambda (prompt handler body)
             (define (renumber n)
               (if (eq? forward-cfa cfa)
                   n
                   (cfa-k-idx cfa (cfa-k-sym forward-cfa n))))
             (let ((handler (renumber handler))
                   (body (renumber body)))
               (if reverse?
                   (vector-push! preds body handler)
                   (vector-push! preds handler body)))))))
      cfa))
  (match fun
    (($ $fun src meta free
        ($ $cont kentry
           (and entry ($ $kentry self ($ $cont ktail tail)))))
     (if reverse?
         (build-cfa ktail lookup-predecessors lookup-successors
                    (analyze-control-flow fun dfg #:reverse? #f
                                          #:add-handler-preds? #f))
         (build-cfa kentry lookup-successors lookup-predecessors #f)))))

;; Dominator analysis.
(define-record-type $dominator-analysis
  (make-dominator-analysis cfa idoms dom-levels loop-header irreducible)
  dominator-analysis?
  ;; The corresponding $cfa
  (cfa dominator-analysis-cfa)
  ;; Vector of k-idx -> k-idx
  (idoms dominator-analysis-idoms)
  ;; Vector of k-idx -> dom-level
  (dom-levels dominator-analysis-dom-levels)
  ;; Vector of k-idx -> k-idx or -1
  (loop-header dominator-analysis-loop-header)
  ;; Vector of k-idx -> true or false value
  (irreducible dominator-analysis-irreducible))

(define (compute-dom-levels idoms)
  (let ((dom-levels (make-vector (vector-length idoms) #f)))
    (define (compute-dom-level n)
      (or (vector-ref dom-levels n)
          (let ((dom-level (1+ (compute-dom-level (vector-ref idoms n)))))
            (vector-set! dom-levels n dom-level)
            dom-level)))
    (vector-set! dom-levels 0 0)
    (let lp ((n 0))
      (when (< n (vector-length idoms))
        (compute-dom-level n)
        (lp (1+ n))))
    dom-levels))

(define (compute-idoms preds)
  (let ((idoms (make-vector (vector-length preds) 0)))
    (define (common-idom d0 d1)
      ;; We exploit the fact that a reverse post-order is a topological
      ;; sort, and so the idom of a node is always numerically less than
      ;; the node itself.
      (cond
       ((= d0 d1) d0)
       ((< d0 d1) (common-idom d0 (vector-ref idoms d1)))
       (else (common-idom (vector-ref idoms d0) d1))))
    (define (compute-idom preds)
      (match preds
        (() 0)
        ((pred . preds)
         (let lp ((idom pred) (preds preds))
           (match preds
             (() idom)
             ((pred . preds)
              (lp (common-idom idom pred) preds)))))))
    ;; This is the iterative O(n^2) fixpoint algorithm, originally from
    ;; Allen and Cocke ("Graph-theoretic constructs for program flow
    ;; analysis", 1972).  See the discussion in Cooper, Harvey, and
    ;; Kennedy's "A Simple, Fast Dominance Algorithm", 2001.
    (let iterate ((n 0) (changed? #f))
      (cond
       ((< n (vector-length preds))
        (let ((idom (vector-ref idoms n))
              (idom* (compute-idom (vector-ref preds n))))
          (cond
           ((eqv? idom idom*)
            (iterate (1+ n) changed?))
           (else
            (vector-set! idoms n idom*)
            (iterate (1+ n) #t)))))
       (changed?
        (iterate 0 #f))
       (else idoms)))))

;; Compute a vector containing, for each node, a list of the nodes that
;; it immediately dominates.  These are the "D" edges in the DJ tree.
(define (compute-dom-edges idoms)
  (let ((doms (make-vector (vector-length idoms) '())))
    (let lp ((n 0))
      (when (< n (vector-length idoms))
        (let ((idom (vector-ref idoms n)))
          (vector-push! doms idom n))
        (lp (1+ n))))
    doms))

;; Compute a vector containing, for each node, a list of the successors
;; of that node that are not dominated by that node.  These are the "J"
;; edges in the DJ tree.
(define (compute-join-edges preds idoms)
  (define (dominates? n1 n2)
    (or (= n1 n2)
        (and (< n1 n2)
             (dominates? n1 (vector-ref idoms n2)))))
  (let ((joins (make-vector (vector-length idoms) '())))
    (let lp ((n 0))
      (when (< n (vector-length preds))
        (for-each (lambda (pred)
                    (unless (dominates? pred n)
                      (vector-push! joins pred n)))
                  (vector-ref preds n))
        (lp (1+ n))))
    joins))

;; Compute a vector containing, for each node, a list of the back edges
;; to that node.  If a node is not the entry of a reducible loop, that
;; list is empty.
(define (compute-reducible-back-edges joins idoms)
  (define (dominates? n1 n2)
    (or (= n1 n2)
        (and (< n1 n2)
             (dominates? n1 (vector-ref idoms n2)))))
  (let ((back-edges (make-vector (vector-length idoms) '())))
    (let lp ((n 0))
      (when (< n (vector-length joins))
        (for-each (lambda (succ)
                    (when (dominates? succ n)
                      (vector-push! back-edges succ n)))
                  (vector-ref joins n))
        (lp (1+ n))))
    back-edges))

;; Compute the levels in the dominator tree at which there are
;; irreducible loops, as an integer.  If a bit N is set in the integer,
;; that indicates that at level N in the dominator tree, there is at
;; least one irreducible loop.
(define (compute-irreducible-dom-levels doms joins idoms dom-levels)
  (define (dominates? n1 n2)
    (or (= n1 n2)
        (and (< n1 n2)
             (dominates? n1 (vector-ref idoms n2)))))
  (let ((pre-order (make-vector (vector-length doms) #f))
        (last-pre-order (make-vector (vector-length doms) #f))
        (res 0)
        (count 0))
    ;; Is MAYBE-PARENT an ancestor of N on the depth-first spanning tree
    ;; computed from the DJ graph?  See Havlak 1997, "Nesting of
    ;; Reducible and Irreducible Loops".
    (define (ancestor? a b)
      (let ((w (vector-ref pre-order a))
            (v (vector-ref pre-order b)))
        (and (<= w v)
             (<= v (vector-ref last-pre-order w)))))
    ;; Compute depth-first spanning tree of DJ graph.
    (define (recurse n)
      (unless (vector-ref pre-order n)
        (visit n)))
    (define (visit n)
      ;; Pre-order visitation index.
      (vector-set! pre-order n count)
      (set! count (1+ count))
      (for-each recurse (vector-ref doms n))
      (for-each recurse (vector-ref joins n))
      ;; Pre-order visitation index of last descendant.
      (vector-set! last-pre-order (vector-ref pre-order n) (1- count)))

    (visit 0)

    (let lp ((n 0))
      (when (< n (vector-length joins))
        (for-each (lambda (succ)
                    ;; If this join edge is not a loop back edge but it
                    ;; does go to an ancestor on the DFST of the DJ
                    ;; graph, then we have an irreducible loop.
                    (when (and (not (dominates? succ n))
                               (ancestor? succ n))
                      (set! res (logior (ash 1 (vector-ref dom-levels succ))))))
                  (vector-ref joins n))
        (lp (1+ n))))

    res))

(define (compute-nodes-by-level dom-levels)
  (let* ((max-level (let lp ((n 0) (max-level 0))
                      (if (< n (vector-length dom-levels))
                          (lp (1+ n) (max (vector-ref dom-levels n) max-level))
                          max-level)))
         (nodes-by-level (make-vector (1+ max-level) '())))
    (let lp ((n (1- (vector-length dom-levels))))
      (when (>= n 0)
        (vector-push! nodes-by-level (vector-ref dom-levels n) n)
        (lp (1- n))))
    nodes-by-level))

;; Collect all predecessors to the back-nodes that are strictly
;; dominated by the loop header, and mark them as belonging to the loop.
;; If they already have a loop header, that means they are either in a
;; nested loop, or they have already been visited already.
(define (mark-loop-body header back-nodes preds idoms loop-headers)
  (define (strictly-dominates? n1 n2)
    (and (< n1 n2)
         (let ((idom (vector-ref idoms n2)))
           (or (= n1 idom)
               (strictly-dominates? n1 idom)))))
  (define (visit node)
    (when (strictly-dominates? header node)
      (cond
       ((vector-ref loop-headers node) => visit)
       (else
        (vector-set! loop-headers node header)
        (for-each visit (vector-ref preds node))))))
  (for-each visit back-nodes))

(define (mark-irreducible-loops level idoms dom-levels loop-headers)
  ;; FIXME: Identify strongly-connected components that are >= LEVEL in
  ;; the dominator tree, and somehow mark them as irreducible.
  (warn 'irreducible-loops-at-level level))

;; "Identifying Loops Using DJ Graphs" by Sreedhar, Gao, and Lee, ACAPS
;; Technical Memo 98, 1995.
(define (identify-loops preds idoms dom-levels)
  (let* ((doms (compute-dom-edges idoms))
         (joins (compute-join-edges preds idoms))
         (back-edges (compute-reducible-back-edges joins idoms))
         (irreducible-levels
          (compute-irreducible-dom-levels doms joins idoms dom-levels))
         (loop-headers (make-vector (vector-length preds) #f))
         (nodes-by-level (compute-nodes-by-level dom-levels)))
    (let lp ((level (1- (vector-length nodes-by-level))))
      (when (>= level 0)
        (for-each (lambda (n)
                    (let ((edges (vector-ref back-edges n)))
                      (unless (null? edges)
                        (mark-loop-body n edges preds idoms loop-headers))))
                  (vector-ref nodes-by-level level))
        (when (logbit? level irreducible-levels)
          (mark-irreducible-loops level idoms dom-levels loop-headers))
        (lp (1- level))))
    loop-headers))

(define (analyze-dominators cfa)
  (match cfa
    (($ $cfa k-map order preds)
     (let* ((idoms (compute-idoms preds))
            (dom-levels (compute-dom-levels idoms))
            (loop-headers (identify-loops preds idoms dom-levels)))
       (make-dominator-analysis cfa idoms dom-levels loop-headers #f)))))


;; Compute the maximum fixed point of the data-flow constraint problem.
;;
;; This always completes, as the graph is finite and the in and out sets
;; are complete semi-lattices.  If the graph is reducible and the blocks
;; are sorted in reverse post-order, this completes in a maximum of LC +
;; 2 iterations, where LC is the loop connectedness number.  See Hecht
;; and Ullman, "Analysis of a simple algorithm for global flow
;; problems", POPL 1973, or the recent summary in "Notes on graph
;; algorithms used in optimizing compilers", Offner 2013.
(define (compute-maximum-fixed-point preds inv outv killv genv union?)
  (define (bitvector-copy! dst src)
    (bitvector-fill! dst #f)
    (bit-set*! dst src #t))
  (define (bitvector-meet! accum src)
    (bit-set*! accum src union?))
  (let lp ((n 0) (changed? #f))
    (cond
     ((< n (vector-length preds))
      (let ((in (vector-ref inv n))
            (out (vector-ref outv n))
            (kill (vector-ref killv n))
            (gen (vector-ref genv n)))
        (let ((out-count (or changed? (bit-count #t out))))
          (for-each
           (lambda (pred)
             (bitvector-meet! in (vector-ref outv pred)))
           (vector-ref preds n))
          (bitvector-copy! out in)
          (for-each (cut bitvector-set! out <> #f) kill)
          (for-each (cut bitvector-set! out <> #t) gen)
          (lp (1+ n)
              (or changed? (not (eqv? out-count (bit-count #t out))))))))
     (changed?
      (lp 0 #f)))))

;; Data-flow analysis.
(define-record-type $dfa
  (make-dfa cfa min-var var-count in out)
  dfa?
  ;; CFA, for its reverse-post-order numbering
  (cfa dfa-cfa)
  ;; Minimum var in this function.
  (min-var dfa-min-var)
  ;; Minimum var in this function.
  (var-count dfa-var-count)
  ;; Vector of k-idx -> bitvector
  (in dfa-in)
  ;; Vector of k-idx -> bitvector
  (out dfa-out))

(define (dfa-k-idx dfa k)
  (cfa-k-idx (dfa-cfa dfa) k))

(define (dfa-k-sym dfa idx)
  (cfa-k-sym (dfa-cfa dfa) idx))

(define (dfa-k-count dfa)
  (cfa-k-count (dfa-cfa dfa)))

(define (dfa-var-idx dfa var)
  (let ((idx (- var (dfa-min-var dfa))))
    (unless (< -1 idx (dfa-var-count dfa))
      (error "var out of range" var))
    idx))

(define (dfa-var-sym dfa idx)
  (unless (< -1 idx (dfa-var-count dfa))
    (error "idx out of range" idx))
  (+ idx (dfa-min-var dfa)))

(define (dfa-k-in dfa idx)
  (vector-ref (dfa-in dfa) idx))

(define (dfa-k-out dfa idx)
  (vector-ref (dfa-out dfa) idx))

(define (compute-live-variables fun dfg)
  (unless (and (= (vector-length (dfg-uses dfg)) (dfg-var-count dfg))
               (= (vector-length (dfg-cont-table dfg)) (dfg-label-count dfg)))
    (error "function needs renumbering"))
  (let* ((min-var (dfg-min-var dfg))
         (nvars (dfg-var-count dfg))
         (cfa (analyze-control-flow fun dfg #:reverse? #t
                                    #:add-handler-preds? #t))
         (usev (make-vector (cfa-k-count cfa) '()))
         (defv (make-vector (cfa-k-count cfa) '()))
         (live-in (make-vector (cfa-k-count cfa) #f))
         (live-out (make-vector (cfa-k-count cfa) #f)))
    (define (var->idx var) (- var min-var))
    (define (idx->var idx) (+ idx min-var))

    ;; Initialize defv and usev.
    (let ((defs (dfg-defs dfg))
          (uses (dfg-uses dfg)))
      (let lp ((n 0))
        (when (< n (vector-length defs))
          (let ((def (vector-ref defs n)))
            (unless def
              (error "internal error -- var array not packed"))
            (for-each (lambda (def)
                        (vector-push! defv (cfa-k-idx cfa def) n))
                      (lookup-predecessors def dfg))
            (for-each (lambda (use)
                        (vector-push! usev (cfa-k-idx cfa use) n))
                      (vector-ref uses n))
            (lp (1+ n))))))

    ;; Initialize live-in and live-out sets.
    (let lp ((n 0))
      (when (< n (vector-length live-out))
        (vector-set! live-in n (make-bitvector nvars #f))
        (vector-set! live-out n (make-bitvector nvars #f))
        (lp (1+ n))))

    ;; Liveness is a reverse data-flow problem, so we give
    ;; compute-maximum-fixed-point a reversed graph, swapping in
    ;; for out, and usev for defv.  Note that since we are using
    ;; a reverse CFA, cfa-preds are actually successors, and
    ;; continuation 0 is ktail.
    (compute-maximum-fixed-point (cfa-preds cfa)
                                 live-out live-in defv usev #t)

    (make-dfa cfa min-var nvars live-in live-out)))

(define (print-dfa dfa)
  (match dfa
    (($ $dfa cfa min-var in out)
     (define (print-var-set bv)
       (let lp ((n 0))
         (let ((n (bit-position #t bv n)))
           (when n
             (format #t " ~A" (+ n min-var))
             (lp (1+ n))))))
     (let lp ((n 0))
       (when (< n (cfa-k-count cfa))
         (format #t "~A:\n" (cfa-k-sym cfa n))
         (format #t "  in:")
         (print-var-set (vector-ref in n))
         (newline)
         (format #t "  out:")
         (print-var-set (vector-ref out n))
         (newline)
         (lp (1+ n)))))))

(define (visit-fun fun conts preds defs uses scopes scope-levels
                   min-label min-var global?)
  (define (add-def! var def-k)
    (vector-set! defs (- var min-var) def-k))

  (define (add-use! var use-k)
    (vector-push! uses (- var min-var) use-k))

  (define* (declare-block! label cont parent
                           #:optional (level
                                       (1+ (vector-ref
                                            scope-levels
                                            (- parent min-label)))))
    (vector-set! conts (- label min-label) cont)
    (vector-set! scopes (- label min-label) parent)
    (vector-set! scope-levels (- label min-label) level))

  (define (link-blocks! pred succ)
    (vector-push! preds (- succ min-label) pred))

  (define (visit exp exp-k)
    (define (def! sym)
      (add-def! sym exp-k))
    (define (use! sym)
      (add-use! sym exp-k))
    (define (use-k! k)
      (link-blocks! exp-k k))
    (define (recur exp)
      (visit exp exp-k))
    (match exp
      (($ $letk (($ $cont k cont) ...) body)
       ;; Set up recursive environment before visiting cont bodies.
       (for-each/2 (lambda (cont k)
                     (declare-block! k cont exp-k))
                   cont k)
       (for-each/2 visit cont k)
       (recur body))

      (($ $kargs names syms body)
       (for-each def! syms)
       (recur body))

      (($ $kif kt kf)
       (use-k! kt)
       (use-k! kf))

      (($ $kreceive arity k)
       (use-k! k))

      (($ $letrec names syms funs body)
       (unless global?
         (error "$letrec should not be present when building a local DFG"))
       (for-each def! syms)
       (for-each
        (cut visit-fun <> conts preds defs uses scopes scope-levels
             min-label min-var global?)
        funs)
       (visit body exp-k))

      (($ $continue k src exp)
       (use-k! k)
       (match exp
         (($ $call proc args)
          (use! proc)
          (for-each use! args))

         (($ $callk k proc args)
          (use! proc)
          (for-each use! args))

         (($ $primcall name args)
          (for-each use! args))

         (($ $values args)
          (for-each use! args))

         (($ $prompt escape? tag handler)
          (use! tag)
          (use-k! handler))

         (($ $fun)
          (when global?
            (visit-fun exp conts preds defs uses scopes scope-levels
                       min-label min-var global?)))

         (_ #f)))))

  (match fun
    (($ $fun src meta free
        ($ $cont kentry
           (and entry
                ($ $kentry self ($ $cont ktail tail) clause))))
     (declare-block! kentry entry #f 0)
     (add-def! self kentry)

     (declare-block! ktail tail kentry)

     (let lp ((clause clause))
       (match clause
         (#f #t)
         (($ $cont kclause
             (and clause ($ $kclause arity ($ $cont kbody body)
                            alternate)))
          (declare-block! kclause clause kentry)
          (link-blocks! kentry kclause)

          (declare-block! kbody body kclause)
          (link-blocks! kclause kbody)

          (visit body kbody)
          (lp alternate)))))))

(define (compute-label-and-var-ranges fun global?)
  (define (min* a b)
    (if b (min a b) a))
  (define-syntax-rule (do-fold global?)
    ((make-cont-folder global?
                       min-label max-label label-count
                       min-var max-var var-count)
     (lambda (label cont
                    min-label max-label label-count
                    min-var max-var var-count)
       (let ((min-label (min* label min-label))
             (max-label (max label max-label)))
         (define (visit-letrec body min-var max-var var-count)
           (match body
             (($ $letk conts body)
              (visit-letrec body min-var max-var var-count))
             (($ $letrec names vars funs body)
              (visit-letrec body
                            (cond (min-var (fold min min-var vars))
                                  ((pair? vars) (fold min (car vars) (cdr vars)))
                                  (else min-var))
                            (fold max max-var vars)
                            (+ var-count (length vars))))
             (($ $continue) (values min-var max-var var-count))))
         (match cont
           (($ $kargs names vars body)
            (call-with-values
                (lambda ()
                  (if global?
                      (visit-letrec body min-var max-var var-count)
                      (values min-var max-var var-count)))
              (lambda (min-var max-var var-count)
                (values min-label max-label (1+ label-count)
                        (cond (min-var (fold min min-var vars))
                              ((pair? vars) (fold min (car vars) (cdr vars)))
                              (else min-var))
                        (fold max max-var vars)
                        (+ var-count (length vars))))))
           (($ $kentry self)
            (values min-label max-label (1+ label-count)
                    (min* self min-var) (max self max-var) (1+ var-count)))
           (_ (values min-label max-label (1+ label-count)
                      min-var max-var var-count)))))
     fun
     #f -1 0 #f -1 0))
  (if global?
      (do-fold #t)
      (do-fold #f)))

(define* (compute-dfg fun #:key (global? #t))
  (call-with-values (lambda () (compute-label-and-var-ranges fun global?))
    (lambda (min-label max-label label-count min-var max-var var-count)
      (when (or (zero? label-count) (zero? var-count))
        (error "internal error (no vars or labels for fun?)"))
      (let* ((nlabels (- (1+ max-label) min-label))
             (nvars (- (1+ max-var) min-var))
             (conts (make-vector nlabels #f))
             (preds (make-vector nlabels '()))
             (defs (make-vector nvars #f))
             (uses (make-vector nvars '()))
             (scopes (make-vector nlabels #f))
             (scope-levels (make-vector nlabels #f)))
        (visit-fun fun conts preds defs uses scopes scope-levels
                   min-label min-var global?)
        (make-dfg conts preds defs uses scopes scope-levels
                  min-label label-count min-var var-count)))))

(define (lookup-cont label dfg)
  (let ((res (vector-ref (dfg-cont-table dfg) (- label (dfg-min-label dfg)))))
    (unless res
      (error "Unknown continuation!" label))
    res))

(define (lookup-predecessors k dfg)
  (vector-ref (dfg-preds dfg) (- k (dfg-min-label dfg))))

(define (lookup-successors k dfg)
  (let ((cont (vector-ref (dfg-cont-table dfg) (- k (dfg-min-label dfg)))))
    (visit-cont-successors list cont)))

(define (lookup-def var dfg)
  (vector-ref (dfg-defs dfg) (- var (dfg-min-var dfg))))

(define (lookup-uses var dfg)
  (vector-ref (dfg-uses dfg) (- var (dfg-min-var dfg))))

(define (lookup-block-scope k dfg)
  (vector-ref (dfg-scopes dfg) (- k (dfg-min-label dfg))))

(define (lookup-scope-level k dfg)
  (vector-ref (dfg-scope-levels dfg) (- k (dfg-min-label dfg))))

(define (find-defining-term sym dfg)
  (match (lookup-predecessors (lookup-def sym dfg) dfg)
    ((def-exp-k)
     (lookup-cont def-exp-k dfg))
    (else #f)))

(define (find-call term)
  (match term
    (($ $kargs names syms body) (find-call body))
    (($ $letk conts body) (find-call body))
    (($ $letrec names syms funs body) (find-call body))
    (($ $continue) term)))

(define (call-expression call)
  (match call
    (($ $continue k src exp) exp)))

(define (find-expression term)
  (call-expression (find-call term)))

(define (find-defining-expression sym dfg)
  (match (find-defining-term sym dfg)
    (#f #f)
    (($ $kreceive) #f)
    (($ $kclause) #f)
    (term (find-expression term))))

(define (find-constant-value sym dfg)
  (match (find-defining-expression sym dfg)
    (($ $const val)
     (values #t val))
    (($ $continue k src ($ $void))
     (values #t *unspecified*))
    (else
     (values #f #f))))

(define (constant-needs-allocation? sym val dfg)
  (define (immediate-u8? val)
    (and (integer? val) (exact? val) (<= 0 val 255)))

  (define (find-exp term)
    (match term
      (($ $kargs names syms body) (find-exp body))
      (($ $letk conts body) (find-exp body))
      (else term)))

  (or-map
   (lambda (use)
     (match (find-expression (lookup-cont use dfg))
       (($ $call) #f)
       (($ $callk) #f)
       (($ $values) #f)
       (($ $primcall 'free-ref (closure slot))
        (not (eq? sym slot)))
       (($ $primcall 'free-set! (closure slot value))
        (not (eq? sym slot)))
       (($ $primcall 'cache-current-module! (mod . _))
        (eq? sym mod))
       (($ $primcall 'cached-toplevel-box _)
        #f)
       (($ $primcall 'cached-module-box _)
        #f)
       (($ $primcall 'resolve (name bound?))
        (eq? sym name))
       (($ $primcall 'make-vector/immediate (len init))
        (not (eq? sym len)))
       (($ $primcall 'vector-ref/immediate (v i))
        (not (eq? sym i)))
       (($ $primcall 'vector-set!/immediate (v i x))
        (not (eq? sym i)))
       (($ $primcall 'allocate-struct/immediate (vtable nfields))
        (not (eq? sym nfields)))
       (($ $primcall 'struct-ref/immediate (s n))
        (not (eq? sym n)))
       (($ $primcall 'struct-set!/immediate (s n x))
        (not (eq? sym n)))
       (($ $primcall 'builtin-ref (idx))
        #f)
       (_ #t)))
   (vector-ref (dfg-uses dfg) (- sym (dfg-min-var dfg)))))

(define (continuation-scope-contains? scope-k k dfg)
  (let ((scope-level (lookup-scope-level scope-k dfg)))
    (let lp ((k k))
      (or (eq? scope-k k)
          (and (< scope-level (lookup-scope-level k dfg))
               (lp (lookup-block-scope k dfg)))))))

(define (continuation-bound-in? k use-k dfg)
  (continuation-scope-contains? (lookup-block-scope k dfg) use-k dfg))

(define (variable-free-in? var k dfg)
  (or-map (lambda (use)
            (continuation-scope-contains? k use dfg))
          (lookup-uses var dfg)))

;; A continuation is a control point if it has multiple predecessors, or
;; if its single predecessor does not have a single successor.
(define (control-point? k dfg)
  (match (lookup-predecessors k dfg)
    ((pred)
     (let ((cont (vector-ref (dfg-cont-table dfg)
                             (- pred (dfg-min-label dfg)))))
       (visit-cont-successors (case-lambda
                                (() #t)
                                ((succ0) #f)
                                ((succ1 succ2) #t))
                              cont)))
    (_ #t)))

(define (lookup-bound-syms k dfg)
  (match (lookup-cont k dfg)
    (($ $kargs names syms body)
     syms)))
