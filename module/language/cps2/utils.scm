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
            intset->intmap
            worklist-fold worklist-fold2
            fixpoint

            ;; Flow analysis.
            compute-predecessors
            compute-function-body
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

(define (intset->intmap f set)
  (persistent-intmap
   (intset-fold (lambda (label preds)
                  (intmap-add! preds label (f label)))
                set empty-intmap)))

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

(define (fixpoint f x)
  (let ((x* (f x)))
    (if (eq? x x*) x* (fixpoint f x*))))

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

;; Precondition: For each function in CONTS, the continuation names are
;; topologically sorted.
(define (compute-idoms conts kfun)
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
