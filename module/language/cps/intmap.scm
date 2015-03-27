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
;;; Some CPS passes need to perform a flow analysis in which every
;;; program point has an associated map over some set of labels or
;;; variables.  The naive way to implement this is with an array of
;;; arrays, but this has N^2 complexity, and it really can hurt us.
;;;
;;; Instead, this module provides a functional map that can share space
;;; between program points, reducing the amortized space complexity of
;;; the representations down to O(n log n).  Adding entries to the
;;; mapping and lookup are O(log n).  Intersection and union between
;;; intmaps that share state are fast, too. 
;;;
;;; Code:

(define-module (language cps intmap)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (empty-intmap
            intmap?
            intmap-add
            intmap-remove
            intmap-ref
            intmap-next
            intmap-union
            intmap-intersect))

;; Persistent sparse intmaps.

(define-syntax-rule (define-inline name val)
  (define-syntax name (identifier-syntax val)))

(define-inline *branch-bits* 4)
(define-inline *branch-size* (ash 1 *branch-bits*))
(define-inline *branch-mask* (1- *branch-size*))

(define-record-type <intmap>
  (make-intmap min shift root)
  intmap?
  (min intmap-min)
  (shift intmap-shift)
  (root intmap-root))

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

(define empty-intmap (make-intmap 0 0 #f))

(define (add-level min shift root)
  (let* ((shift* (+ shift *branch-bits*))
         (min* (round-down min shift*))
         (idx (logand (ash (- min min*) (- shift))
                      *branch-mask*)))
    (make-intmap min* shift* (clone-branch-and-set #f idx root))))

(define (make-intmap/prune min shift root)
  (if (zero? shift)
      (make-intmap min shift root)
      (let lp ((i 0) (elt #f))
        (cond
         ((< i *branch-size*)
          (if (vector-ref root i)
              (if elt
                  (make-intmap min shift root)
                  (lp (1+ i) i))
              (lp (1+ i) elt)))
         (elt
          (let ((shift (- shift *branch-bits*)))
            (make-intmap/prune (+ min (ash elt shift))
                               shift
                               (vector-ref root elt))))
         ;; Shouldn't be reached...
         (else empty-intmap)))))

(define (intmap-add bs i val meet)
  (define (adjoin i shift root)
    (cond
     ((zero? shift)
      (cond
       ((eq? root val) root)
       ((not root) val)
       (else (meet root val))))
     (else
      (let* ((shift (- shift *branch-bits*))
             (idx (logand (ash i (- shift)) *branch-mask*))
             (node (and root (vector-ref root idx)))
             (new-node (adjoin i shift node)))
        (if (eq? node new-node)
            root
            (clone-branch-and-set root idx new-node))))))
  (match bs
    (($ <intmap> min shift root)
     (cond
      ((< i 0)
       ;; The power-of-two spanning trick doesn't work across 0.
       (error "Intmaps can only map non-negative integers." i))
      ((not val) (intmap-remove bs i))
      ((not root)
       ;; Add first element.
       (make-intmap i 0 val))
      ((and (<= min i) (< i (+ min (ash 1 shift))))
       ;; Add element to map; level will not change.
       (let ((old-root root)
             (root (adjoin (- i min) shift root)))
         (if (eq? root old-root)
             bs
             (make-intmap min shift root))))
      ((< i min)
       ;; Rebuild the tree by unioning two intmaps.
       (intmap-union (intmap-add empty-intmap i val error) bs error))
      (else
       ;; Add a new level and try again.
       (intmap-add (add-level min shift root) i val error))))))

(define (intmap-remove bs i)
  (define (remove i shift root)
    (cond
     ((zero? shift) #f)
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
    (($ <intmap> min shift root)
     (cond
      ((not root) bs)
      ((and (<= min i) (< i (+ min (ash 1 shift))))
       ;; Add element to map; level will not change.
       (let ((old-root root)
             (root (remove (- i min) shift root)))
         (if (eq? root old-root)
             bs
             (make-intmap/prune min shift root))))
      (else bs)))))

(define (intmap-ref bs i)
  (match bs
    (($ <intmap> min shift root)
     (if (zero? shift)
         (and (= i min) root)
         (and (<= min i) (< i (+ min (ash 1 shift)))
              (let ((i (- i min)))
                (let lp ((node root) (shift shift))
                  (and node
                       (if (= shift *branch-bits*)
                           (vector-ref node (logand i *branch-mask*))
                           (let* ((shift (- shift *branch-bits*))
                                  (idx (logand (ash i (- shift))
                                               *branch-mask*)))
                             (lp (vector-ref node idx) shift)))))))))))

(define (intmap-next bs i)
  (define (visit-branch node shift i)
    (let lp ((i i) (idx (logand (ash i (- shift)) *branch-mask*)))
      (and (< idx *branch-size*)
           (or (visit-node (vector-ref node idx) shift i)
               (let ((inc (ash 1 shift)))
                 (lp (+ (round-down i shift) inc) (1+ idx)))))))
  (define (visit-node node shift i)
    (and node
         (if (zero? shift)
             i
             (visit-branch node (- shift *branch-bits*) i))))
  (match bs
    (($ <intmap> min shift root)
     (let ((i (if (and i (< min i))
                  (- i min)
                  0)))
       (and (< i (ash 1 shift))
            (let ((i (visit-node root shift i)))
              (and i (+ min i))))))))

(define (intmap-union a b meet)
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
     ((zero? shift) (meet a-node b-node))
     (else (union-branches (- shift *branch-bits*) a-node b-node))))
  (match (cons a b)
    ((($ <intmap> a-min a-shift a-root) . ($ <intmap> b-min b-shift b-root))
     (cond
      ((not (= b-shift a-shift))
       ;; Hoist the map with the lowest shift to meet the one with the
       ;; higher shift.
       (if (< b-shift a-shift)
           (intmap-union a (add-level b-min b-shift b-root) meet)
           (intmap-union (add-level a-min a-shift a-root) b meet)))
      ((not (= b-min a-min))
       ;; Nodes at the same shift but different minimums will cover
       ;; disjoint ranges (due to the round-down call on min).  Hoist
       ;; both until they cover the same range.
       (intmap-union (add-level a-min a-shift a-root)
                     (add-level b-min b-shift b-root)
                     meet))
      (else
       ;; At this point, A and B cover the same range.
       (let ((root (union a-shift a-root b-root)))
         (cond
          ((eq? root a-root) a)
          ((eq? root b-root) b)
          (else (make-intmap a-min a-shift root)))))))))

(define (intmap-intersect a b meet)
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
     ((zero? shift) (meet a-node b-node))
     (else (intersect-branches (- shift *branch-bits*) a-node b-node))))

  (define (different-mins lo-min lo-shift lo-root hi-min hi-shift hi lo-is-a?)
    (cond
     ((<= lo-shift hi-shift)
      ;; If LO has a lower shift and a lower min, it is disjoint.  If
      ;; it has the same shift and a different min, it is also
      ;; disjoint.
      empty-intmap)
     (else
      (let* ((lo-shift (- lo-shift *branch-bits*))
             (lo-idx (ash (- hi-min lo-min) (- lo-shift))))
        (cond
         ((>= lo-idx *branch-size*)
          ;; HI has a lower shift, but it not within LO.
          empty-intmap)
         ((vector-ref lo-root lo-idx)
          => (lambda (lo-root)
               (let ((lo (make-intmap (+ lo-min (ash lo-idx lo-shift))
                                      lo-shift
                                      lo-root)))
                 (if lo-is-a?
                     (intmap-intersect lo hi meet)
                     (intmap-intersect hi lo meet)))))
         (else empty-intmap))))))

  (define (different-shifts-same-min min hi-shift hi-root lo lo-is-a?)
    (cond
     ((vector-ref hi-root 0)
      => (lambda (hi-root)
           (let ((hi (make-intmap min
                                  (- hi-shift *branch-bits*)
                                  hi-root)))
             (if lo-is-a?
                 (intmap-intersect lo hi meet)
                 (intmap-intersect hi lo meet)))))
     (else empty-intmap)))

  (match (cons a b)
    ((($ <intmap> a-min a-shift a-root) . ($ <intmap> b-min b-shift b-root))
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
          (else (make-intmap/prune a-min a-shift root)))))))))
