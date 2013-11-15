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
;;; A module to assign stack slots to variables in a CPS term.
;;;
;;; Code:

(define-module (language cps slot-allocation)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:export (allocate-slots
            lookup-slot
            lookup-maybe-slot
            lookup-constant-value
            lookup-maybe-constant-value
            lookup-nlocals
            lookup-call-proc-slot
            lookup-parallel-moves))

(define-record-type $allocation
  (make-allocation dfa slots
                   has-constv constant-values
                   call-allocations
                   nlocals)
  allocation?

  ;; A DFA records all variables bound in a function, and assigns them
  ;; indices.  The slot in which a variable is stored at runtime can be
  ;; had by indexing into the SLOTS vector with the variable's index.
  ;;
  (dfa allocation-dfa)
  (slots allocation-slots)

  ;; Not all variables have slots allocated.  Variables that are
  ;; constant and that are only used by primcalls that can accept
  ;; constants directly are not allocated to slots, and their SLOT value
  ;; is false.  Likewise constants that are only used by calls are not
  ;; allocated into slots, to avoid needless copying.  If a variable is
  ;; constant, its constant value is set in the CONSTANT-VALUES vector
  ;; and the corresponding bit in the HAS-CONSTV bitvector is set.
  ;;
  (has-constv allocation-has-constv)
  (constant-values allocation-constant-values)

  ;; Some continuations have additional associated information.  This
  ;; addition information is a /call allocation/.  Call allocations
  ;; record the way that functions are passed values, and how their
  ;; return values are rebound to local variables.
  ;;
  ;; A call allocation contains two pieces of information: the call's
  ;; /proc slot/, and a set of /parallel moves/.  The proc slot
  ;; indicates the slot of a procedure in a procedure call, or where the
  ;; procedure would be in a multiple-value return.  The parallel moves
  ;; shuffle locals into position for a call, or shuffle returned values
  ;; back into place.  Though they use the same slot, moves for a call
  ;; are called "call moves", and moves to handle a return are "return
  ;; moves".
  ;;
  ;; $ktrunc continuations record a proc slot and a set of return moves
  ;; to adapt multiple values from the stack to local variables.
  ;;
  ;; Tail calls record arg moves, but no proc slot.
  ;;
  ;; Non-tail calls record arg moves and a call slot.  Multiple-valued
  ;; returns will have an associated $ktrunc continuation, which records
  ;; the same proc slot, but has return moves.
  ;;
  ;; $prompt handlers are $ktrunc continuations like any other.
  ;;
  ;; $values expressions with more than 1 value record moves but have no
  ;; proc slot.
  ;;
  ;; A set of moves is expressed as an ordered list of (SRC . DST)
  ;; moves, where SRC and DST are slots.  This may involve a temporary
  ;; variable.
  ;;
  (call-allocations allocation-call-allocations)

  ;; The number of locals for a $kclause.
  ;;
  (nlocals allocation-nlocals))

(define-record-type $call-allocation
  (make-call-allocation proc-slot moves)
  call-allocation?
  (proc-slot call-allocation-proc-slot)
  (moves call-allocation-moves))

(define (find-first-zero n)
  ;; Naive implementation.
  (let lp ((slot 0))
    (if (logbit? slot n)
        (lp (1+ slot))
        slot)))

(define (find-first-trailing-zero n)
  (let lp ((slot (let lp ((count 2))
                   (if (< n (ash 1 (1- count)))
                       count
                       ;; Grow upper bound slower than factor 2 to avoid
                       ;; needless bignum allocation on 32-bit systems
                       ;; when there are more than 16 locals.
                       (lp (+ count (ash count -1)))))))
    (if (or (zero? slot) (logbit? (1- slot) n))
        slot
        (lp (1- slot)))))

(define (lookup-maybe-slot sym allocation)
  (match allocation
    (($ $allocation dfa slots)
     (vector-ref slots (dfa-var-idx dfa sym)))))

(define (lookup-slot sym allocation)
  (or (lookup-maybe-slot sym allocation)
      (error "Variable not allocated to a slot" sym)))

(define (lookup-constant-value sym allocation)
  (match allocation
    (($ $allocation dfa slots has-constv constant-values)
     (let ((idx (dfa-var-idx dfa sym)))
       (if (bitvector-ref has-constv idx)
           (vector-ref constant-values idx)
           (error "Variable does not have constant value" sym))))))

(define (lookup-maybe-constant-value sym allocation)
  (match allocation
    (($ $allocation dfa slots has-constv constant-values)
     (let ((idx (dfa-var-idx dfa sym)))
       (values (bitvector-ref has-constv idx)
               (vector-ref constant-values idx))))))

(define (lookup-call-allocation k allocation)
  (or (hashq-ref (allocation-call-allocations allocation) k)
      (error "Continuation not a call" k)))

(define (lookup-call-proc-slot k allocation)
  (or (call-allocation-proc-slot (lookup-call-allocation k allocation))
      (error "Call has no proc slot" k)))

(define (lookup-parallel-moves k allocation)
  (or (call-allocation-moves (lookup-call-allocation k allocation))
      (error "Call has no use parallel moves slot" k)))

(define (lookup-nlocals k allocation)
  (or (hashq-ref (allocation-nlocals allocation) k)
      (error "Not a clause continuation" k)))

(define (solve-parallel-move src dst tmp)
  "Solve the parallel move problem between src and dst slot lists, which
are comparable with eqv?.  A tmp slot may be used."

  ;; This algorithm is taken from: "Tilting at windmills with Coq:
  ;; formal verification of a compilation algorithm for parallel moves"
  ;; by Laurence Rideau, Bernard Paul Serpette, and Xavier Leroy
  ;; <http://gallium.inria.fr/~xleroy/publi/parallel-move.pdf>

  (define (split-move moves reg)
    (let loop ((revhead '()) (tail moves))
      (match tail
        (((and s+d (s . d)) . rest)
         (if (eqv? s reg)
             (cons d (append-reverse revhead rest))
             (loop (cons s+d revhead) rest)))
        (_ #f))))

  (define (replace-last-source reg moves)
    (match moves
      ((moves ... (s . d))
       (append moves (list (cons reg d))))))

  (let loop ((to-move (map cons src dst))
             (being-moved '())
             (moved '())
             (last-source #f))
    ;; 'last-source' should always be equivalent to:
    ;; (and (pair? being-moved) (car (last being-moved)))
    (match being-moved
      (() (match to-move
            (() (reverse moved))
            (((and s+d (s . d)) . t1)
             (if (or (eqv? s d) ; idempotent
                     (not s))   ; src is a constant and can be loaded directly
                 (loop t1 '() moved #f)
                 (loop t1 (list s+d) moved s)))))
      (((and s+d (s . d)) . b)
       (match (split-move to-move d)
         ((r . t1) (loop t1 (acons d r being-moved) moved last-source))
         (#f (match b
               (() (loop to-move '() (cons s+d moved) #f))
               (_ (if (eqv? d last-source)
                      (loop to-move
                            (replace-last-source tmp b)
                            (cons s+d (acons d tmp moved))
                            tmp)
                      (loop to-move b (cons s+d moved) last-source))))))))))

(define (dead-after-def? def-k v-idx dfa)
  (let ((l (dfa-k-idx dfa def-k)))
    (not (bitvector-ref (dfa-k-in dfa l) v-idx))))

(define (dead-after-use? use-k v-idx dfa)
  (let ((l (dfa-k-idx dfa use-k)))
    (not (bitvector-ref (dfa-k-out dfa l) v-idx))))

(define (allocate-slots fun dfg)
  (let* ((dfa (compute-live-variables fun dfg))
         (cfa (analyze-control-flow fun dfg))
         (usev (make-vector (cfa-k-count cfa) '()))
         (defv (make-vector (cfa-k-count cfa) '()))
         (cont-table (dfg-cont-table dfg))
         (slots (make-vector (dfa-var-count dfa) #f))
         (constant-values (make-vector (dfa-var-count dfa) #f))
         (has-constv (make-bitvector (dfa-var-count dfa) #f))
         (has-slotv (make-bitvector (dfa-var-count dfa) #t))
         (needs-slotv (make-bitvector (dfa-var-count dfa) #t))
         (call-allocations (make-hash-table))
         (nlocals 0)                    ; Mutable.  It pains me.
         (nlocals-table (make-hash-table)))

    (define (bump-nlocals! nlocals*)
      (when (< nlocals nlocals*)
        (set! nlocals nlocals*)))

    (define (empty-live-slots)
      #b0)

    (define (add-live-slot slot live-slots)
      (logior live-slots (ash 1 slot)))

    (define (kill-dead-slot slot live-slots)
      (logand live-slots (lognot (ash 1 slot))))

    (define (compute-slot live-slots hint)
      (if (and hint (not (logbit? hint live-slots)))
          hint
          (find-first-zero live-slots)))

    (define (compute-call-proc-slot live-slots)
      (+ 3 (find-first-trailing-zero live-slots)))

    (define (compute-prompt-handler-proc-slot live-slots)
      (1- (find-first-trailing-zero live-slots)))

    (define (recompute-live-slots k nargs)
      (let ((in (dfa-k-in dfa (dfa-k-idx dfa k))))
        (let lp ((v 0) (live-slots (1- (ash 1 (1+ nargs)))))
          (let ((v (bit-position #t in v)))
            (if v
                (let ((slot (vector-ref slots v)))
                  (lp (1+ v)
                      (if slot
                          (add-live-slot slot live-slots)
                          live-slots)))
                live-slots)))))

    (define* (allocate! var-idx hint live)
      (cond
       ((not (bitvector-ref needs-slotv var-idx)) live)
       ((vector-ref slots var-idx) => (cut add-live-slot <> live))
       (else
        (let ((slot (compute-slot live hint)))
          (bump-nlocals! (1+ slot))
          (vector-set! slots var-idx slot)
          (add-live-slot slot live)))))

    ;; Although some parallel moves may proceed without a temporary
    ;; slot, in general one is needed.  That temporary slot must not be
    ;; part of the source or destination sets, and that slot should not
    ;; correspond to a live variable.  Usually the source and
    ;; destination sets are a subset of the union of the live sets
    ;; before and after the move.  However for stack slots that don't
    ;; have names -- those slots that correspond to function arguments
    ;; or to function return values -- it could be that they are out of
    ;; the computed live set.  In that case they need to be adjoined to
    ;; the live set, used when choosing a temporary slot.
    (define (compute-tmp-slot live stack-slots)
      (find-first-zero (fold add-live-slot live stack-slots)))

    (define (parallel-move src-slots dst-slots tmp-slot)
      (let ((moves (solve-parallel-move src-slots dst-slots tmp-slot)))
        (when (assv tmp-slot moves)
          (bump-nlocals! (1+ tmp-slot)))
        moves))

    ;; Find variables that are actually constant, and determine which
    ;; of those can avoid slot allocation.
    (define (compute-constants!)
      (let lp ((n 0))
        (when (< n (vector-length constant-values))
          (let ((sym (dfa-var-sym dfa n)))
            (call-with-values (lambda () (find-constant-value sym dfg))
              (lambda (has-const? const)
                (when has-const?
                  (bitvector-set! has-constv n has-const?)
                  (vector-set! constant-values n const)
                  (when (not (constant-needs-allocation? sym const dfg))
                    (bitvector-set! needs-slotv n #f)))
                (lp (1+ n))))))))

    ;; Record uses and defs, as lists of variable indexes, indexed by
    ;; CFA continuation index.
    (define (compute-uses-and-defs!)
      (let lp ((n 0))
        (when (< n (vector-length usev))
          (match (lookup-cont (cfa-k-sym cfa n) cont-table)
            (($ $kentry self)
             (vector-set! defv n (list (dfa-var-idx dfa self))))
            (($ $kargs names syms body)
             (vector-set! defv n (map (cut dfa-var-idx dfa <>) syms))
             (vector-set! usev n
                          (map (cut dfa-var-idx dfa <>)
                               (match (find-expression body)
                                 (($ $call proc args)
                                  (cons proc args))
                                 (($ $primcall name args)
                                  args)
                                 (($ $values args)
                                  args)
                                 (($ $prompt escape? tag handler pop)
                                  (list tag))
                                 (_ '())))))
            (_ #f))
          (lp (1+ n)))))

    (define (allocate-call label k uses pre-live post-live)
      (match (lookup-cont k cont-table)
        (($ $ktail)
         (let* ((tail-nlocals (length uses))
                (tail-slots (iota tail-nlocals))
                (moves (parallel-move (map (cut vector-ref slots <>) uses)
                                      tail-slots
                                      (compute-tmp-slot pre-live tail-slots))))
           (bump-nlocals! tail-nlocals)
           (hashq-set! call-allocations label
                       (make-call-allocation #f moves))))
        (($ $ktrunc arity kargs)
         (let* ((proc-slot (compute-call-proc-slot post-live))
                (call-slots (map (cut + proc-slot <>) (iota (length uses))))
                (arg-moves (parallel-move (map (cut vector-ref slots <>) uses)
                                          call-slots
                                          (compute-tmp-slot pre-live
                                                            call-slots)))
                (result-vars (vector-ref defv (cfa-k-idx cfa kargs)))
                (value-slots (map (cut + proc-slot 1 <>)
                                  (iota (length result-vars))))
                (result-live (fold allocate!
                                   post-live result-vars value-slots))
                (result-slots (map (cut vector-ref slots <>) result-vars))
                (result-moves (parallel-move value-slots
                                             result-slots
                                             (compute-tmp-slot result-live
                                                               value-slots))))
           (bump-nlocals! (+ proc-slot (length uses)))
           (hashq-set! call-allocations label
                       (make-call-allocation proc-slot arg-moves))
           (hashq-set! call-allocations k
                       (make-call-allocation proc-slot result-moves))))

        (_
         (let* ((proc-slot (compute-call-proc-slot post-live))
                (call-slots (map (cut + proc-slot <>) (iota (length uses))))
                (arg-moves (parallel-move (map (cut vector-ref slots <>) uses)
                                          call-slots
                                          (compute-tmp-slot pre-live
                                                            call-slots))))
           (bump-nlocals! (+ proc-slot (length uses)))
           (hashq-set! call-allocations label
                       (make-call-allocation proc-slot arg-moves))))))
                         
    (define (allocate-values label k uses pre-live post-live)
      (let* ((src-slots (map (cut vector-ref slots <>) uses))
             (dst-slots (match (lookup-cont k cont-table)
                          (($ $ktail)
                           (let ((tail-nlocals (1+ (length uses))))
                             (bump-nlocals! tail-nlocals)
                             (cdr (iota tail-nlocals))))
                          (_
                           (let ((dst-vars (vector-ref defv (cfa-k-idx cfa k))))
                             (fold allocate! post-live dst-vars src-slots)
                             (map (cut vector-ref slots <>) dst-vars)))))
             (moves (parallel-move src-slots
                                   dst-slots
                                   (compute-tmp-slot pre-live dst-slots))))
        (hashq-set! call-allocations label
                    (make-call-allocation #f moves))))

    (define (allocate-prompt label k handler nargs)
      (match (lookup-cont handler cont-table)
        (($ $ktrunc arity kargs)
         (let* ((handler-live (recompute-live-slots handler nargs))
                (proc-slot (compute-prompt-handler-proc-slot handler-live))
                (result-vars (vector-ref defv (cfa-k-idx cfa kargs)))
                (value-slots (map (cut + proc-slot 1 <>)
                                  (iota (length result-vars))))
                (result-live (fold allocate!
                                   handler-live result-vars value-slots))
                (result-slots (map (cut vector-ref slots <>) result-vars))
                (moves (parallel-move value-slots
                                      result-slots
                                      (compute-tmp-slot result-live
                                                        value-slots))))
           (bump-nlocals! (+ proc-slot 1 (length result-vars)))
           (hashq-set! call-allocations handler
                       (make-call-allocation proc-slot moves))))))

    (define (allocate-defs! n live)
      (fold (cut allocate! <> #f <>) live (vector-ref defv n)))

    ;; This traversal will visit definitions before uses, as
    ;; definitions dominate uses and a block's dominator will appear
    ;; before it, in reverse post-order.
    (define (visit-clause n nargs live)
      (let lp ((n n) (live live))
        (define (kill-dead live vars-by-cfa-idx pred)
          (fold (lambda (v live)
                  (let ((slot (vector-ref slots v)))
                    (if (and slot
                             (> slot nargs)
                             (pred (cfa-k-sym cfa n) v dfa))
                        (kill-dead-slot slot live)
                        live)))
                live
                (vector-ref vars-by-cfa-idx n)))
        (define (kill-dead-defs live)
          (kill-dead live defv dead-after-def?))
        (define (kill-dead-uses live)
          (kill-dead live usev dead-after-use?))
        (if (= n (cfa-k-count cfa))
            n
            (let* ((label (cfa-k-sym cfa n))
                   (live (if (control-point? label dfg)
                             (recompute-live-slots label nargs)
                             live))
                   (live (kill-dead-defs (allocate-defs! n live)))
                   (post-live (kill-dead-uses live)))
              ;; LIVE are the live slots coming into the term.
              ;; POST-LIVE is the subset that is still live after the
              ;; term uses its inputs.
              (match (lookup-cont label cont-table)
                (($ $kclause) n)
                (($ $kargs names syms body)
                 (let ((uses (vector-ref usev n)))
                   (match (find-call body)
                     (($ $continue k src ($ $call))
                      (allocate-call label k uses live post-live))
                     (($ $continue k src ($ $primcall)) #t)
                     ;; We only need to make a call allocation if there
                     ;; are two or more values.
                     (($ $continue k src ($ $values (_ _ . _)))
                      (allocate-values label k uses live post-live))
                     (($ $continue k src ($ $values)) #t)
                     (($ $continue k src ($ $prompt escape? tag handler pop))
                      (allocate-prompt label k handler nargs))
                     (_ #f)))
                 (lp (1+ n) post-live))
                ((or ($ $ktrunc) ($ $kif) ($ $ktail))
                 (lp (1+ n) post-live)))))))

    (define (visit-entry)
      (define (visit-clauses n live)
        (unless (eqv? live (add-live-slot 0 (empty-live-slots)))
          (error "Unexpected clause live set"))
        (set! nlocals 1)
        (let ((k (cfa-k-sym cfa n)))
          (match (lookup-cont k cont-table)
            (($ $kclause arity ($ $cont kbody ($ $kargs names)))
             (unless (eq? (cfa-k-sym cfa (1+ n)) kbody)
               (error "Unexpected CFA order"))
             (let ((next (visit-clause (1+ n) (length names) live)))
               (hashq-set! nlocals-table k nlocals)
               (when (< next (cfa-k-count cfa))
                 (visit-clauses next live)))))))
      (match (lookup-cont (cfa-k-sym cfa 0) cont-table)
        (($ $kentry self)
         (visit-clauses 1 (allocate-defs! 0 (empty-live-slots))))))

    (compute-constants!)
    (compute-uses-and-defs!)
    (visit-entry)

    (make-allocation dfa slots
                     has-constv constant-values
                     call-allocations
                     nlocals-table)))
