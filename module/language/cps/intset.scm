;;; Functional name maps
;;; Copyright (C) 2014 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; A persistent, functional data structure representing a set of
;;; integers as a tree whose branches are vectors and whose leaves are
;;; fixnums.  Intsets are careful to preserve sub-structure, in the
;;; sense of eq?, whereever possible.
;;;
;;; Code:

(define-module (language cps intset)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (empty-intset
            intset?
            intset-add
            intset-remove
            intset-ref
            intset-next
            intset-union
            intset-intersect))

(define-syntax-rule (define-inline name val)
  (define-syntax name (identifier-syntax val)))

(eval-when (expand)
  (use-modules (system base target))
  (define-syntax compile-time-cond
    (lambda (x)
      (syntax-case x (else)
        ((_ (test body ...) rest ...)
         (if (primitive-eval (syntax->datum #'test))
             #'(begin body ...)
             #'(begin (compile-time-cond rest ...))))
        ((_ (else body ...))
         #'(begin body ...))
        ((_)
         (error "no compile-time-cond expression matched"))))))

(compile-time-cond
 ((eqv? (target-word-size) 4)
  (define-inline *leaf-bits* 4))
 ((eqv? (target-word-size) 8)
  (define-inline *leaf-bits* 5)))

(define-inline *leaf-size* (ash 1 *leaf-bits*))
(define-inline *leaf-mask* (1- *leaf-size*))
(define-inline *branch-bits* 3)
(define-inline *branch-size* (ash 1 *branch-bits*))
(define-inline *branch-mask* (1- *branch-size*))

(define-record-type <intset>
  (make-intset min shift root)
  intset?
  (min intset-min)
  (shift intset-shift)
  (root intset-root))

(define (new-leaf) 0)
(define-inlinable (clone-leaf-and-set leaf i val)
  (if val
      (if leaf
          (logior leaf (ash 1 i))
          (ash 1 i))
      (if leaf
          (logand leaf (lognot (ash 1 i)))
          #f)))
(define (leaf-empty? leaf)
  (zero? leaf))

(define (new-branch)
  (make-vector *branch-size* #f))
(define (clone-branch-and-set branch i elt)
  (let ((new (new-branch)))
    (when branch (vector-move-left! branch 0 *branch-size* new 0))
    (vector-set! new i elt)
    new))
(define (branch-empty? branch)
  (let lp ((i 0))
    (or (= i *branch-size*)
        (and (not (vector-ref branch i))
             (lp (1+ i))))))

(define (round-down min shift)
  (logand min (lognot (1- (ash 1 shift)))))

(define empty-intset (make-intset 0 *leaf-bits* #f))

(define (add-level min shift root)
  (let* ((shift* (+ shift *branch-bits*))
         (min* (round-down min shift*))
         (idx (logand (ash (- min min*) (- shift)) *branch-mask*)))
    (make-intset min* shift* (clone-branch-and-set #f idx root))))

(define (make-intset/prune min shift root)
  (cond
   ((not root)
    empty-intset)
   ((= shift *leaf-bits*)
    (make-intset min shift root))
   (else
    (let lp ((i 0) (elt #f))
      (cond
       ((< i *branch-size*)
        (if (vector-ref root i)
            (if elt
                (make-intset min shift root)
                (lp (1+ i) i))
            (lp (1+ i) elt)))
       (elt
        (let ((shift (- shift *branch-bits*)))
          (make-intset/prune (+ min (ash elt shift))
                             shift
                             (vector-ref root elt))))
       ;; Shouldn't be reached...
       (else empty-intset))))))

(define (intset-add bs i)
  (define (adjoin i shift root)
    (cond
     ((= shift *leaf-bits*)
      (let ((idx (logand i *leaf-mask*)))
        (if (and root (logbit? idx root))
            root
            (clone-leaf-and-set root idx #t))))
     (else
      (let* ((shift (- shift *branch-bits*))
             (idx (logand (ash i (- shift)) *branch-mask*))
             (node (and root (vector-ref root idx)))
             (new-node (adjoin i shift node)))
        (if (eq? node new-node)
            root
            (clone-branch-and-set root idx new-node))))))
  (match bs
    (($ <intset> min shift root)
     (cond
      ((not root)
       ;; Add first element.
       (let ((min (round-down i shift)))
         (make-intset min *leaf-bits*
                      (adjoin (- i min) *leaf-bits* root))))
      ((and (<= min i) (< i (+ min (ash 1 shift))))
       ;; Add element to set; level will not change.
       (let ((old-root root)
             (root (adjoin (- i min) shift root)))
         (if (eq? root old-root)
             bs
             (make-intset min shift root))))
      ((< i min)
       ;; Rebuild the tree by unioning two intsets.
       (intset-union (intset-add empty-intset i) bs))
      (else
       ;; Add a new level and try again.
       (intset-add (add-level min shift root) i))))))

(define (intset-remove bs i)
  (define (remove i shift root)
    (cond
     ((= shift *leaf-bits*)
      (let ((idx (logand i *leaf-mask*)))
        (if (logbit? idx root)
            (let ((root (clone-leaf-and-set root idx #f)))
              (and (not (leaf-empty? root)) root))
            root)))
     (else
      (let* ((shift (- shift *branch-bits*))
             (idx (logand (ash i (- shift)) *branch-mask*)))
        (cond
         ((vector-ref root idx)
          => (lambda (node)
               (let ((new-node (remove i shift node)))
                 (if (eq? node new-node)
                     root
                     (let ((root (clone-branch-and-set root idx new-node)))
                       (and (or new-node (not (branch-empty? root)))
                            root))))))
         (else root))))))
  (match bs
    (($ <intset> min shift root)
     (cond
      ((not root) bs)
      ((and (<= min i) (< i (+ min (ash 1 shift))))
       ;; Add element to set; level will not change.
       (let ((old-root root)
             (root (remove (- i min) shift root)))
         (if (eq? root old-root)
             bs
             (make-intset/prune min shift root))))
      (else bs)))))

(define (intset-ref bs i)
  (match bs
    (($ <intset> min shift root)
     (and (<= min i) (< i (+ min (ash 1 shift)))
          (let ((i (- i min)))
            (let lp ((node root) (shift shift))
              (and node
                   (if (= shift *leaf-bits*)
                       (logbit? (logand i *leaf-mask*) node)
                       (let* ((shift (- shift *branch-bits*))
                              (idx (logand (ash i (- shift)) *branch-mask*)))
                         (lp (vector-ref node idx) shift))))))))))

(define (intset-next bs i)
  (define (visit-leaf node i)
    (let lp ((idx (logand i *leaf-mask*)))
      (if (logbit? idx node)
          (logior (logand i (lognot *leaf-mask*)) idx)
          (let ((idx (1+ idx)))
            (and (< idx *leaf-size*)
                 (lp idx))))))
  (define (visit-branch node shift i)
    (let lp ((i i) (idx (logand (ash i (- shift)) *branch-mask*)))
      (and (< idx *branch-size*)
           (or (visit-node (vector-ref node idx) shift i)
               (let ((inc (ash 1 shift)))
                 (lp (+ (round-down i shift) inc) (1+ idx)))))))
  (define (visit-node node shift i)
    (and node
         (if (= shift *leaf-bits*)
             (visit-leaf node i)
             (visit-branch node (- shift *branch-bits*) i))))
  (match bs
    (($ <intset> min shift root)
     (let ((i (if (and i (< min i))
                  (- i min)
                  0)))
       (and (< i (ash 1 shift))
            (let ((i (visit-node root shift i)))
              (and i (+ min i))))))))

(define (intset-size shift root)
  (cond
   ((not root) 0)
   ((= *leaf-bits* shift) *leaf-size*)
   (else
    (let lp ((i (1- *branch-size*)))
      (let ((node (vector-ref root i)))
        (if node
            (let ((shift (- shift *branch-bits*)))
              (+ (intset-size shift node)
                 (* i (ash 1 shift))))
            (lp (1- i))))))))

(define (intset-union a b)
  ;; Union leaves.
  (define (union-leaves a b)
    (logior (or a 0) (or b 0)))
  ;; Union A and B from index I; the result will be fresh.
  (define (union-branches/fresh shift a b i fresh)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (vector-set! fresh i (union shift a-child b-child))
          (lp (1+ i))))
       (else fresh))))
  ;; Union A and B from index I; the result may be eq? to A.
  (define (union-branches/a shift a b i)
    (let lp ((i i))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (if (eq? a-child b-child)
              (lp (1+ i))
              (let ((child (union shift a-child b-child)))
                (cond
                 ((eq? a-child child)
                  (lp (1+ i)))
                 (else
                  (let ((result (clone-branch-and-set a i child)))
                    (union-branches/fresh shift a b (1+ i) result))))))))
       (else a))))
  ;; Union A and B; the may could be eq? to either.
  (define (union-branches shift a b)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (if (eq? a-child b-child)
              (lp (1+ i))
              (let ((child (union shift a-child b-child)))
                (cond
                 ((eq? a-child child)
                  (union-branches/a shift a b (1+ i)))
                 ((eq? b-child child)
                  (union-branches/a shift b a (1+ i)))
                 (else
                  (let ((result (clone-branch-and-set a i child)))
                    (union-branches/fresh shift a b (1+ i) result))))))))
       ;; Seems they are the same but not eq?.  Odd.
       (else a))))
  (define (union shift a-node b-node)
    (cond
     ((not a-node) b-node)
     ((not b-node) a-node)
     ((eq? a-node b-node) a-node)
     ((= shift *leaf-bits*) (union-leaves a-node b-node))
     (else (union-branches (- shift *branch-bits*) a-node b-node))))
  (match (cons a b)
    ((($ <intset> a-min a-shift a-root) . ($ <intset> b-min b-shift b-root))
     (cond
      ((not (= b-shift a-shift))
       ;; Hoist the set with the lowest shift to meet the one with the
       ;; higher shift.
       (if (< b-shift a-shift)
           (intset-union a (add-level b-min b-shift b-root))
           (intset-union (add-level a-min a-shift a-root) b)))
      ((not (= b-min a-min))
       ;; Nodes at the same shift but different minimums will cover
       ;; disjoint ranges (due to the round-down call on min).  Hoist
       ;; both until they cover the same range.
       (intset-union (add-level a-min a-shift a-root)
                     (add-level b-min b-shift b-root)))
      (else
       ;; At this point, A and B cover the same range.
       (let ((root (union a-shift a-root b-root)))
         (cond
          ((eq? root a-root) a)
          ((eq? root b-root) b)
          (else (make-intset a-min a-shift root)))))))))

(define (intset-intersect a b)
  (define tmp (new-leaf))
  ;; Intersect leaves.
  (define (intersect-leaves a b)
    (logand a b))
  ;; Intersect A and B from index I; the result will be fresh.
  (define (intersect-branches/fresh shift a b i fresh)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (vector-set! fresh i (intersect shift a-child b-child))
          (lp (1+ i))))
       ((branch-empty? fresh) #f)
       (else fresh))))
  ;; Intersect A and B from index I; the result may be eq? to A.
  (define (intersect-branches/a shift a b i)
    (let lp ((i i))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (if (eq? a-child b-child)
              (lp (1+ i))
              (let ((child (intersect shift a-child b-child)))
                (cond
                 ((eq? a-child child)
                  (lp (1+ i)))
                 (else
                  (let ((result (clone-branch-and-set a i child)))
                    (intersect-branches/fresh shift a b (1+ i) result))))))))
       (else a))))
  ;; Intersect A and B; the may could be eq? to either.
  (define (intersect-branches shift a b)
    (let lp ((i 0))
      (cond
       ((< i *branch-size*)
        (let* ((a-child (vector-ref a i))
               (b-child (vector-ref b i)))
          (if (eq? a-child b-child)
              (lp (1+ i))
              (let ((child (intersect shift a-child b-child)))
                (cond
                 ((eq? a-child child)
                  (intersect-branches/a shift a b (1+ i)))
                 ((eq? b-child child)
                  (intersect-branches/a shift b a (1+ i)))
                 (else
                  (let ((result (clone-branch-and-set a i child)))
                    (intersect-branches/fresh shift a b (1+ i) result))))))))
       ;; Seems they are the same but not eq?.  Odd.
       (else a))))
  (define (intersect shift a-node b-node)
    (cond
     ((or (not a-node) (not b-node)) #f)
     ((eq? a-node b-node) a-node)
     ((= shift *leaf-bits*) (intersect-leaves a-node b-node))
     (else (intersect-branches (- shift *branch-bits*) a-node b-node))))

  (define (different-mins lo-min lo-shift lo-root hi-min hi-shift hi lo-is-a?)
    (cond
     ((<= lo-shift hi-shift)
      ;; If LO has a lower shift and a lower min, it is disjoint.  If
      ;; it has the same shift and a different min, it is also
      ;; disjoint.
      empty-intset)
     (else
      (let* ((lo-shift (- lo-shift *branch-bits*))
             (lo-idx (ash (- hi-min lo-min) (- lo-shift))))
        (cond
         ((>= lo-idx *branch-size*)
          ;; HI has a lower shift, but it not within LO.
          empty-intset)
         ((vector-ref lo-root lo-idx)
          => (lambda (lo-root)
               (let ((lo (make-intset (+ lo-min (ash lo-idx lo-shift))
                                      lo-shift
                                      lo-root)))
                 (if lo-is-a?
                     (intset-intersect lo hi)
                     (intset-intersect hi lo)))))
         (else empty-intset))))))

  (define (different-shifts-same-min min hi-shift hi-root lo lo-is-a?)
    (cond
     ((vector-ref hi-root 0)
      => (lambda (hi-root)
           (let ((hi (make-intset min
                                  (- hi-shift *branch-bits*)
                                  hi-root)))
             (if lo-is-a?
                 (intset-intersect lo hi)
                 (intset-intersect hi lo)))))
     (else empty-intset)))

  (match (cons a b)
    ((($ <intset> a-min a-shift a-root) . ($ <intset> b-min b-shift b-root))
     (cond
      ((< a-min b-min)
       (different-mins a-min a-shift a-root b-min b-shift b #t))
      ((< b-min a-min)
       (different-mins b-min b-shift b-root a-min a-shift a #f))
      ((< a-shift b-shift)
       (different-shifts-same-min b-min b-shift b-root a #t))
      ((< b-shift a-shift)
       (different-shifts-same-min a-min a-shift a-root b #f))
      (else
       ;; At this point, A and B cover the same range.
       (let ((root (intersect a-shift a-root b-root)))
         (cond
          ((eq? root a-root) a)
          ((eq? root b-root) b)
          (else (make-intset/prune a-min a-shift root)))))))))
