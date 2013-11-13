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
            build-local-cont-table
            lookup-cont

            compute-dfg
            dfg-cont-table
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
            dfa-var-idx dfa-var-name dfa-var-sym dfa-var-count
            print-dfa))

(define (build-cont-table fun)
  (fold-conts (lambda (k cont table)
                (hashq-set! table k cont)
                table)
              (make-hash-table)
              fun))

(define (build-local-cont-table cont)
  (fold-local-conts (lambda (k cont table)
                      (hashq-set! table k cont)
                      table)
                    (make-hash-table)
                    cont))

(define (lookup-cont sym conts)
  (let ((res (hashq-ref conts sym)))
    (unless res
      (error "Unknown continuation!" sym (hash-fold acons '() conts)))
    res))

;; Data-flow graph for CPS: both for values and continuations.
(define-record-type $dfg
  (make-dfg conts blocks use-maps)
  dfg?
  ;; hash table of sym -> $kif, $kargs, etc
  (conts dfg-cont-table)
  ;; hash table of sym -> $block
  (blocks dfg-blocks)
  ;; hash table of sym -> $use-map
  (use-maps dfg-use-maps))

(define-record-type $use-map
  (make-use-map name sym def uses)
  use-map?
  (name use-map-name)
  (sym use-map-sym)
  (def use-map-def)
  (uses use-map-uses set-use-map-uses!))

(define-record-type $block
  (%make-block scope scope-level preds succs)
  block?
  (scope block-scope set-block-scope!)
  (scope-level block-scope-level set-block-scope-level!)
  (preds block-preds set-block-preds!)
  (succs block-succs set-block-succs!))

(define (make-block scope scope-level)
  (%make-block scope scope-level '() '()))

;; Some analyses assume that the only relevant set of nodes is the set
;; that is reachable from some start node.  Others need to include nodes
;; that are reachable from an end node as well, or all nodes in a
;; function.  In that case pass an appropriate implementation of
;; fold-all-conts, as compute-live-variables does.
(define* (reverse-post-order k0 get-successors #:optional
                             (fold-all-conts (lambda (f seed) seed)))
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

(define* (analyze-control-flow fun dfg #:key reverse?)
  (define (build-cfa kentry block-succs block-preds)
    (define (block-accessor accessor)
      (lambda (k)
        (accessor (lookup-block k (dfg-blocks dfg)))))
    (define (reachable-preds mapping accessor)
      ;; It's possible for a predecessor to not be in the mapping, if
      ;; the predecessor is not reachable from the entry node.
      (lambda (k)
        (filter-map (cut hashq-ref mapping <>)
                    ((block-accessor accessor) k))))
    (let* ((order (reverse-post-order kentry (block-accessor block-succs)))
           (k-map (make-block-mapping order))
           (preds (convert-predecessors order
                                        (reachable-preds k-map block-preds))))
      (make-cfa k-map order preds)))
  (match fun
    (($ $fun src meta free
        ($ $cont kentry
           (and entry
                ($ $kentry self ($ $cont ktail tail) clauses))))
     (if reverse?
         (build-cfa ktail block-preds block-succs)
         (build-cfa kentry block-succs block-preds)))))

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

(define-inlinable (vector-push! vec idx val)
  (let ((v vec) (i idx))
    (vector-set! v i (cons val (vector-ref v i)))))

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
  (make-dfa k-map order var-map names syms in out)
  dfa?
  ;; Hash table mapping k-sym -> k-idx
  (k-map dfa-k-map)
  ;; Vector of k-idx -> k-sym
  (order dfa-order)
  ;; Hash table mapping var-sym -> var-idx
  (var-map dfa-var-map)
  ;; Vector of var-idx -> name
  (names dfa-names)
  ;; Vector of var-idx -> var-sym
  (syms dfa-syms)
  ;; Vector of k-idx -> bitvector
  (in dfa-in)
  ;; Vector of k-idx -> bitvector
  (out dfa-out))

(define (dfa-k-idx dfa k)
  (or (hashq-ref (dfa-k-map dfa) k)
      (error "unknown k" k)))

(define (dfa-k-sym dfa idx)
  (vector-ref (dfa-order dfa) idx))

(define (dfa-k-count dfa)
  (vector-length (dfa-order dfa)))

(define (dfa-var-idx dfa var)
  (or (hashq-ref (dfa-var-map dfa) var)
      (error "unknown var" var)))

(define (dfa-var-name dfa idx)
  (vector-ref (dfa-names dfa) idx))

(define (dfa-var-sym dfa idx)
  (vector-ref (dfa-syms dfa) idx))

(define (dfa-var-count dfa)
  (vector-length (dfa-syms dfa)))

(define (dfa-k-in dfa idx)
  (vector-ref (dfa-in dfa) idx))

(define (dfa-k-out dfa idx)
  (vector-ref (dfa-out dfa) idx))

(define (compute-live-variables fun dfg)
  (define (make-variable-mapping use-maps)
    (let ((mapping (make-hash-table))
          (n 0))
      (hash-for-each (lambda (sym use-map)
                       (hashq-set! mapping sym n)
                       (set! n (1+ n)))
                     use-maps)
      (values mapping n)))
  (define (block-accessor blocks accessor)
    (lambda (k)
      (accessor (lookup-block k blocks))))
  (define (renumbering-accessor mapping blocks accessor)
    (lambda (k)
      (map (cut hashq-ref mapping <>)
           ((block-accessor blocks accessor) k))))
  (match fun
    (($ $fun src meta free
        (and entry
             ($ $cont kentry ($ $kentry self ($ $cont ktail tail)))))
     (call-with-values (lambda () (make-variable-mapping (dfg-use-maps dfg)))
       (lambda (var-map nvars)
         (define (fold-all-conts f seed)
           (fold-local-conts (lambda (k cont seed) (f k seed))
                             seed entry))
         (let* ((blocks (dfg-blocks dfg))
                (order (reverse-post-order ktail
                                           (block-accessor blocks block-preds)
                                           fold-all-conts))
                (k-map (make-block-mapping order))
                (succs (convert-predecessors
                        order
                        (renumbering-accessor k-map blocks block-succs)))
                (syms (make-vector nvars #f))
                (names (make-vector nvars #f))
                (usev (make-vector (vector-length order) '()))
                (defv (make-vector (vector-length order) '()))
                (live-in (make-vector (vector-length order) #f))
                (live-out (make-vector (vector-length order) #f)))
           (define (k->idx k)
             (or (hashq-ref k-map k) (error "unknown k" k)))
           ;; Initialize syms, names, defv, and usev.
           (hash-for-each
            (lambda (sym use-map)
              (match use-map
                (($ $use-map name sym def uses)
                 (let ((v (or (hashq-ref var-map sym)
                              (error "unknown var" sym))))
                   (vector-set! syms v sym)
                   (vector-set! names v name)
                   (for-each (lambda (def)
                               (vector-push! defv (k->idx def) v))
                             ((block-accessor blocks block-preds) def))
                   (for-each (lambda (use)
                               (vector-push! usev (k->idx use) v))
                             uses)))))
            (dfg-use-maps dfg))

           ;; Initialize live-in and live-out sets.
           (let lp ((n 0))
             (when (< n (vector-length live-out))
               (vector-set! live-in n (make-bitvector nvars #f))
               (vector-set! live-out n (make-bitvector nvars #f))
               (lp (1+ n))))

           ;; Liveness is a reverse data-flow problem, so we give
           ;; compute-maximum-fixed-point a reversed graph, swapping in
           ;; and out, usev and defv, using successors instead of
           ;; predecessors, and starting with ktail instead of the
           ;; entry.
           (compute-maximum-fixed-point succs live-out live-in defv usev #t)

           (make-dfa k-map order var-map names syms live-in live-out)))))))

(define (print-dfa dfa)
  (match dfa
    (($ $dfa k-map order var-map names syms in out)
     (define (print-var-set bv)
       (let lp ((n 0))
         (let ((n (bit-position #t bv n)))
           (when n
             (format #t " ~A" (vector-ref syms n))
             (lp (1+ n))))))
     (let lp ((n 0))
       (when (< n (vector-length order))
         (format #t "~A:\n" (vector-ref order n))
         (format #t "  in:")
         (print-var-set (vector-ref in n))
         (newline)
         (format #t "  out:")
         (print-var-set (vector-ref out n))
         (newline)
         (lp (1+ n)))))))

(define (visit-fun fun conts blocks use-maps global?)
  (define (add-def! name sym def-k)
    (unless def-k
      (error "Term outside labelled continuation?"))
    (hashq-set! use-maps sym (make-use-map name sym def-k '())))

  (define (add-use! sym use-k)
    (match (hashq-ref use-maps sym)
      (#f (error "Symbol out of scope?" sym))
      ((and use-map ($ $use-map name sym def uses))
       (set-use-map-uses! use-map (cons use-k uses)))))

  (define* (declare-block! label cont parent
                           #:optional (level
                                       (1+ (lookup-scope-level parent blocks))))
    (hashq-set! conts label cont)
    (hashq-set! blocks label (make-block parent level)))

  (define (link-blocks! pred succ)
    (let ((pred-block (hashq-ref blocks pred))
          (succ-block (hashq-ref blocks succ)))
      (unless (and pred-block succ-block)
        (error "internal error" pred-block succ-block))
      (set-block-succs! pred-block (cons succ (block-succs pred-block)))
      (set-block-preds! succ-block (cons pred (block-preds succ-block)))))

  (define (visit exp exp-k)
    (define (def! name sym)
      (add-def! name sym exp-k))
    (define (use! sym)
      (add-use! sym exp-k))
    (define (use-k! k)
      (link-blocks! exp-k k))
    (define (recur exp)
      (visit exp exp-k))
    (match exp
      (($ $letk (($ $cont k cont) ...) body)
       ;; Set up recursive environment before visiting cont bodies.
       (for-each (lambda (cont k)
                   (declare-block! k cont exp-k))
                 cont k)
       (for-each visit cont k)
       (recur body))

      (($ $kargs names syms body)
       (for-each def! names syms)
       (recur body))

      (($ $kif kt kf)
       (use-k! kt)
       (use-k! kf))

      (($ $ktrunc arity k)
       (use-k! k))

      (($ $letrec names syms funs body)
       (unless global?
         (error "$letrec should not be present when building a local DFG"))
       (for-each def! names syms)
       (for-each (cut visit-fun <> conts blocks use-maps global?) funs)
       (visit body exp-k))

      (($ $continue k src exp)
       (use-k! k)
       (match exp
         (($ $call proc args)
          (use! proc)
          (for-each use! args))

         (($ $primcall name args)
          (for-each use! args))

         (($ $values args)
          (for-each use! args))

         (($ $prompt escape? tag handler pop)
          (use! tag)
          (use-k! handler)
          ;; Any continuation in the prompt body could cause an abort to
          ;; the handler, so in theory we could register the handler as
          ;; a successor of any block in the prompt body.  That would be
          ;; inefficient, though, besides being a hack.  Instead we take
          ;; advantage of the fact that pop continuation post-dominates
          ;; the prompt body, so we add a link from there to the
          ;; handler.  This creates a primcall node with multiple
          ;; successors, which is not quite correct, but it does reflect
          ;; control flow.  It is necessary to ensure that the live
          ;; variables in the handler are seen as live in the body.
          (link-blocks! pop handler))

         (($ $fun)
          (when global?
            (visit-fun exp conts blocks use-maps global?)))

         (_ #f)))))

  (match fun
    (($ $fun src meta free
        ($ $cont kentry
           (and entry
                ($ $kentry self ($ $cont ktail tail) clauses))))
     (declare-block! kentry entry #f 0)
     (add-def! #f self kentry)

     (declare-block! ktail tail kentry)

     (for-each
      (match-lambda
       (($ $cont kclause
           (and clause ($ $kclause arity ($ $cont kbody body))))
        (declare-block! kclause clause kentry)
        (link-blocks! kentry kclause)

        (declare-block! kbody body kclause)
        (link-blocks! kclause kbody)

        (visit body kbody)))
      clauses))))

(define* (compute-dfg fun #:key (global? #t))
  (let* ((conts (make-hash-table))
         (blocks (make-hash-table))
         (use-maps (make-hash-table)))
    (visit-fun fun conts blocks use-maps global?)
    (make-dfg conts blocks use-maps)))

(define (lookup-block k blocks)
  (let ((res (hashq-ref blocks k)))
    (unless res
      (error "Unknown continuation!" k (hash-fold acons '() blocks)))
    res))

(define (lookup-scope-level k blocks)
  (match (lookup-block k blocks)
    (($ $block _ scope-level) scope-level)))

(define (lookup-use-map sym use-maps)
  (let ((res (hashq-ref use-maps sym)))
    (unless res
      (error "Unknown lexical!" sym (hash-fold acons '() use-maps)))
    res))

(define (lookup-def sym dfg)
  (match dfg
    (($ $dfg conts blocks use-maps)
     (match (lookup-use-map sym use-maps)
       (($ $use-map name sym def uses)
        def)))))

(define (lookup-uses sym dfg)
  (match dfg
    (($ $dfg conts blocks use-maps)
     (match (lookup-use-map sym use-maps)
       (($ $use-map name sym def uses)
        uses)))))

(define (lookup-block-scope k dfg)
  (block-scope (lookup-block k (dfg-blocks dfg))))

(define (lookup-predecessors k dfg)
  (match (lookup-block k (dfg-blocks dfg))
    (($ $block _ _ preds succs) preds)))

(define (lookup-successors k dfg)
  (match (lookup-block k (dfg-blocks dfg))
    (($ $block _ _ preds succs) succs)))

(define (find-defining-term sym dfg)
  (match (lookup-predecessors (lookup-def sym dfg) dfg)
    ((def-exp-k)
     (lookup-cont def-exp-k (dfg-cont-table dfg)))
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
    (($ $ktrunc) #f)
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
  (match dfg
    (($ $dfg conts blocks use-maps)
     (match (lookup-use-map sym use-maps)
       (($ $use-map _ _ def uses)
        (or-map
         (lambda (use)
           (match (find-expression (lookup-cont use conts))
             (($ $call) #f)
             (($ $values (_ _ . _)) #f)
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
         uses))))))

(define (continuation-scope-contains? scope-k k blocks)
  (let ((scope-level (lookup-scope-level scope-k blocks)))
    (let lp ((k k))
      (or (eq? scope-k k)
          (match (lookup-block k blocks)
            (($ $block scope level)
             (and (< scope-level level)
                  (lp scope))))))))

(define (continuation-bound-in? k use-k dfg)
  (match dfg
    (($ $dfg conts blocks use-maps)
     (match (lookup-block k blocks)
       (($ $block def-k)
        (continuation-scope-contains? def-k use-k blocks))))))

(define (variable-free-in? var k dfg)
  (match dfg
    (($ $dfg conts blocks use-maps)
     (or-map (lambda (use)
               (continuation-scope-contains? k use blocks))
             (match (lookup-use-map var use-maps)
               (($ $use-map name sym def uses)
                uses))))))

;; A continuation is a control point if it has multiple predecessors, or
;; if its single predecessor has multiple successors.
(define (control-point? k dfg)
  (match (lookup-predecessors k dfg)
    ((pred)
     (match (lookup-successors pred dfg)
       ((_) #f)
       (_ #t)))
    (_ #t)))

(define (lookup-bound-syms k dfg)
  (match dfg
    (($ $dfg conts blocks use-maps)
     (match (lookup-cont k conts)
       (($ $kargs names syms body)
        syms)))))
