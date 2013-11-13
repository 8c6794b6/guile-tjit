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
            lookup-constant-value
            lookup-maybe-constant-value
            lookup-nlocals
            lookup-call-proc-slot
            lookup-parallel-moves))

;; Continuations can bind variables.  The $allocation structure
;; represents the slot in which a variable is stored.
;;
;; Not all variables have slots allocated.  Variables that are constant
;; and that are only used by primcalls that can accept constants
;; directly are not allocated to slots, and their SLOT value is false.
;; Likewise constants that are only used by calls are not allocated into
;; slots, to avoid needless copying.  If a variable is constant, its
;; constant value is set to the CONST slot and HAS-CONST? is set to a
;; true value.
;;
(define-record-type $allocation
  (make-allocation slot has-const? const)
  allocation?
  (slot allocation-slot)
  (has-const? allocation-has-const?)
  (const allocation-const))

;; Continuations can also have associated allocation data.  For example,
;; when a call happens in a labelled continuation, we need to know what
;; slot the procedure goes in.  Likewise before branching to the target
;; continuation, we might need to shuffle values into the right place: a
;; parallel move.  $cont-allocation stores allocation data keyed on the
;; continuation label.
(define-record-type $cont-allocation
  (make-cont-allocation call-proc-slot parallel-moves)
  cont-allocation?

  ;; Currently calls are allocated in the caller frame, above all locals
  ;; that are live at the time of the call.  Therefore there is no
  ;; parallel move problem.  We could be more clever here.
  ;;
  ;; $prompt expressions also use this call slot to indicate where the
  ;; handler's arguments are expected, but without reserving space for a
  ;; frame or for the procedure slot.
  (call-proc-slot cont-call-proc-slot)

  ;; Tail calls, multiple-value returns, and jumps to continuations with
  ;; multiple arguments are forms of parallel assignment.  A
  ;; $parallel-move represents a specific solution to the parallel
  ;; assignment problem, with an ordered list of (SRC . DST) moves.  This
  ;; may involve a temporary variable.
  ;;
  ;; ((src . dst) ...)
  (parallel-moves cont-parallel-moves))

(define (find-first-zero n)
  ;; Naive implementation.
  (let lp ((slot 0))
    (if (logbit? slot n)
        (lp (1+ slot))
        slot)))

(define (find-first-trailing-zero n count)
  (let lp ((slot count))
    (if (or (zero? slot) (logbit? (1- slot) n))
        slot
        (lp (1- slot)))))

(define (lookup-allocation sym allocation)
  (let ((res (hashq-ref allocation sym)))
    (unless res
      (error "Variable or continuation not defined" sym))
    res))

(define (lookup-slot sym allocation)
  (match (lookup-allocation sym allocation)
    (($ $allocation slot has-const? const) slot)))

(define (lookup-constant-value sym allocation)
  (match (lookup-allocation sym allocation)
    (($ $allocation slot #t const) const)
    (_
     (error "Variable does not have constant value" sym))))

(define (lookup-maybe-constant-value sym allocation)
  (match (lookup-allocation sym allocation)
    (($ $allocation slot has-const? const)
     (values has-const? const))))

(define (lookup-call-proc-slot k allocation)
  (match (lookup-allocation k allocation)
    (($ $cont-allocation proc-slot parallel-moves)
     (unless proc-slot
       (error "Continuation not a call" k))
     proc-slot)
    (_
     (error "Continuation not a call" k))))

(define (lookup-nlocals k allocation)
  (match (lookup-allocation k allocation)
    ((? number? nlocals) nlocals)
    (_
     (error "Not a clause continuation" k))))

(define (lookup-parallel-moves k allocation)
  (match (lookup-allocation k allocation)
    (($ $cont-allocation proc-slot parallel-moves)
     (unless parallel-moves
       (error "Continuation does not have parallel moves" k))
     parallel-moves)
    (_
     (error "Continuation not a call" k))))

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

  (define (compute-call-proc-slot live-slots nlocals)
    (+ 3 (find-first-trailing-zero live-slots nlocals)))

  (define (compute-prompt-handler-proc-slot live-slots nlocals)
    (1- (find-first-trailing-zero live-slots nlocals)))

  (define (recompute-live-slots k slots nargs dfa)
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

  (define (visit-clause clause dfa allocation slots live-slots)
    (define nlocals (compute-slot live-slots #f))
    (define nargs
      (match clause
        (($ $cont _ ($ $kclause _ ($ $cont _ ($ $kargs names syms))))
         (length syms))))

    (define (allocate! sym k hint live-slots)
      (match (hashq-ref allocation sym)
        (($ $allocation slot)
         ;; Parallel move already allocated this one.
         (if slot
             (add-live-slot slot live-slots)
             live-slots))
        (_
         (call-with-values (lambda () (find-constant-value sym dfg))
           (lambda (has-const? const)
             (cond
              ((and has-const? (not (constant-needs-allocation? sym const dfg)))
               (hashq-set! allocation sym
                           (make-allocation #f has-const? const))
               live-slots)
              (else
               (let ((slot (compute-slot live-slots hint)))
                 (when (>= slot nlocals)
                   (set! nlocals (+ slot 1)))
                 (vector-set! slots (dfa-var-idx dfa sym) slot)
                 (hashq-set! allocation sym
                             (make-allocation slot has-const? const))
                 (add-live-slot slot live-slots)))))))))

    (define (allocate-prompt-handler! k live-slots)
      (let ((proc-slot (compute-prompt-handler-proc-slot live-slots nlocals)))
        (hashq-set! allocation k
                    (make-cont-allocation
                     proc-slot
                     (match (hashq-ref allocation k)
                       (($ $cont-allocation #f moves) moves)
                       (#f #f))))
        live-slots))

    (define (allocate-frame! k nargs live-slots)
      (let ((proc-slot (compute-call-proc-slot live-slots nlocals)))
        (set! nlocals (max nlocals (+ proc-slot 1 nargs)))
        (hashq-set! allocation k
                    (make-cont-allocation
                     proc-slot
                     (match (hashq-ref allocation k)
                       (($ $cont-allocation #f moves) moves)
                       (#f #f))))
        live-slots))

    (define (parallel-move! src-k src-slots pre-live-slots post-live-slots dst-slots)
      (let* ((tmp-slot (find-first-zero (logior pre-live-slots post-live-slots)))
             (moves (solve-parallel-move src-slots dst-slots tmp-slot)))
        (when (and (>= tmp-slot nlocals) (assv tmp-slot moves))
          (set! nlocals (+ tmp-slot 1)))
        (hashq-set! allocation src-k
                    (make-cont-allocation
                     (match (hashq-ref allocation src-k)
                       (($ $cont-allocation proc-slot #f) proc-slot)
                       (#f #f))
                     moves))
        post-live-slots))

    (define (visit-cont cont label live-slots)
      (define (maybe-kill-definition sym live-slots)
        (let* ((v (dfa-var-idx dfa sym))
               (slot (vector-ref slots v)))
          (if (and slot (> slot nargs) (dead-after-def? label v dfa))
              (kill-dead-slot slot live-slots)
              live-slots)))

      (define (maybe-recompute-live-slots live-slots)
        (if (control-point? label dfg)
            (recompute-live-slots label slots nargs dfa)
            live-slots))

      (match cont
        (($ $kclause arity ($ $cont k body))
         (visit-cont body k live-slots))

        (($ $kargs names syms body)
         (visit-term body label
                     (maybe-recompute-live-slots
                      (fold maybe-kill-definition
                            (fold (cut allocate! <> label #f <>) live-slots syms)
                            syms))))

        (($ $ktrunc) live-slots)
        (($ $kif) live-slots)))

    (define (visit-term term label live-slots)
      (match term
        (($ $letk conts body)
         (let ((live-slots (visit-term body label live-slots)))
           (for-each (match-lambda
                      (($ $cont k cont)
                       (visit-cont cont k live-slots)))
                     conts))
         live-slots)

        (($ $continue k src exp)
         (visit-exp exp label k live-slots))))

    (define (visit-exp exp label k live-slots)
      (define (use sym live-slots)
        (let* ((v (dfa-var-idx dfa sym))
               (l (dfa-k-idx dfa label))
               (slot (vector-ref slots v)))
          (if (and slot (> slot nargs) (dead-after-use? label v dfa))
              (kill-dead-slot slot live-slots)
              live-slots)))

      (match exp
        (($ $call proc args)
         (match (lookup-cont k (dfg-cont-table dfg))
           (($ $ktail)
            (let ((tail-nlocals (1+ (length args))))
              (set! nlocals (max nlocals tail-nlocals))
              (parallel-move! label
                              (map (cut lookup-slot <> allocation)
                                   (cons proc args))
                              live-slots (fold use live-slots (cons proc args))
                              (iota tail-nlocals))))
           (($ $ktrunc arity kargs)
            (let* ((live-slots
                    (fold use
                          (use proc
                               (allocate-frame! label (length args) live-slots))
                          args))
                   (proc-slot (lookup-call-proc-slot label allocation))
                   (dst-syms (lookup-bound-syms kargs dfg))
                   (nvals (length dst-syms))
                   (src-slots (map (cut + proc-slot 1 <>) (iota nvals)))
                   (live-slots* (fold (cut allocate! <> kargs <> <>)
                                      live-slots dst-syms src-slots))
                   (dst-slots (map (cut lookup-slot <> allocation)
                                   dst-syms)))
              (parallel-move! label src-slots live-slots live-slots* dst-slots)))
           (else
            (fold use
                  (use proc (allocate-frame! label (length args) live-slots))
                  args))))

        (($ $primcall name args)
         (fold use live-slots args))

        (($ $values (arg))
         (use arg live-slots))

        (($ $values args)
         (let ((live-slots* (fold use live-slots args)))
           (define (compute-dst-slots)
             (match (lookup-cont k (dfg-cont-table dfg))
               (($ $ktail)
                (let ((tail-nlocals (1+ (length args))))
                  (set! nlocals (max nlocals tail-nlocals))
                  (cdr (iota tail-nlocals))))
               (_
                (let* ((src-slots (map (cut lookup-slot <> allocation) args))
                       (dst-syms (lookup-bound-syms k dfg))
                       (dst-live-slots (fold (cut allocate! <> k <> <>)
                                             live-slots* dst-syms src-slots)))
                  (map (cut lookup-slot <> allocation) dst-syms)))))

           (parallel-move! label
                           (map (cut lookup-slot <> allocation) args)
                           live-slots live-slots*
                           (compute-dst-slots))))

        (($ $values args)
         (let ((live-slots* (fold use live-slots args)))
           (define (compute-dst-slots)
             (match (lookup-cont k (dfg-cont-table dfg))
               (($ $ktail)
                (let ((tail-nlocals (1+ (length args))))
                  (set! nlocals (max nlocals tail-nlocals))
                  (cdr (iota tail-nlocals))))
               (_
                (let* ((src-slots (map (cut lookup-slot <> allocation) args))
                       (dst-syms (lookup-bound-syms k dfg))
                       (dst-live-slots (fold (cut allocate! <> k <> <>)
                                             live-slots* dst-syms src-slots)))
                  (map (cut lookup-slot <> allocation) dst-syms)))))

           (parallel-move! label
                           (map (cut lookup-slot <> allocation) args)
                           live-slots live-slots*
                           (compute-dst-slots))))

        (($ $prompt escape? tag handler pop)
         (match (lookup-cont handler (dfg-cont-table dfg))
           (($ $ktrunc arity kargs)
            (let* ((live-slots (allocate-prompt-handler! label live-slots))
                   (proc-slot (lookup-call-proc-slot label allocation))
                   (dst-syms (lookup-bound-syms kargs dfg))
                   (nvals (length dst-syms))
                   (src-slots (map (cut + proc-slot 1 <>) (iota nvals)))
                   (live-slots* (fold (cut allocate! <> kargs <> <>)
                                      live-slots dst-syms src-slots))
                   (dst-slots (map (cut lookup-slot <> allocation)
                                   dst-syms)))
              (parallel-move! handler src-slots live-slots live-slots* dst-slots))))
         (use tag live-slots))

        (_ live-slots)))

    (match clause
      (($ $cont k body)
       (visit-cont body k live-slots)
       (hashq-set! allocation k nlocals))))

  (match fun
    (($ $fun src meta free ($ $cont k ($ $kentry self tail clauses)))
     (let* ((dfa (compute-live-variables fun dfg))
            (allocation (make-hash-table))
            (slots (make-vector (dfa-var-count dfa) #f))
            (live-slots (add-live-slot 0 (empty-live-slots))))
       (vector-set! slots (dfa-var-idx dfa self) 0)
       (hashq-set! allocation self (make-allocation 0 #f #f))
       (for-each (cut visit-clause <> dfa allocation slots live-slots)
                 clauses)
       allocation))))
