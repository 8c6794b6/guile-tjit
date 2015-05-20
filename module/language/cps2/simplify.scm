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

(define-module (language cps2 simplify)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (language cps2)
  #:use-module (language cps2 utils)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:export (simplify))

(define (intset-maybe-add! set k add?)
  (if add? (intset-add! set k) set))

(define (intset-add* set k*)
  (let lp ((set set) (k* k*))
    (match k*
      ((k . k*) (lp (intset-add set k) k*))
      (() set))))

(define (intset-add*! set k*)
  (fold1 (lambda (k set) (intset-add! set k)) k* set))

(define (fold2* f l1 l2 seed)
  (let lp ((l1 l1) (l2 l2) (seed seed))
    (match (cons l1 l2)
      ((() . ()) seed)
      (((x1 . l1) . (x2 . l2)) (lp l1 l2 (f x1 x2 seed))))))

(define (transform-conts f conts)
  (persistent-intmap
   (intmap-fold (lambda (k v out)
                  (let ((v* (f k v)))
                    (if (equal? v v*)
                        out
                        (intmap-add! out k v* (lambda (old new) new)))))
                conts
                conts)))

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
      (intset-fold visit-cont body nested-funs eta)))
  (define (visit-funs worklist eta)
    (intset-fold visit-fun worklist empty-intset eta))
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
                ((single multiple) (intset-fold add-ref body single multiple)))
    (intset-subtract (persistent-intset single)
                     (persistent-intset multiple))))

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
      (intset-fold visit-cont body nested-funs beta)))
  (define (visit-funs worklist beta)
    (intset-fold visit-fun worklist empty-intset beta))
  (persistent-intset
   (worklist-fold visit-funs (intset-add empty-intset kfun) empty-intset)))

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

(define (beta-reduce conts kfun)
  (let* ((label-set (compute-beta-reductions conts kfun))
         (var-map (compute-beta-var-substitutions conts label-set)))
    (define (subst var)
      (match (intmap-ref var-map var (lambda (_) #f))
        (#f var)
        (val (subst val))))
    (define (transform-exp label k src exp)
      (if (intset-ref label-set label)
          (match (intmap-ref conts k)
            (($ $kargs _ _ ($ $continue k* src* exp*))
             (transform-exp k k* src* exp*)))
          (build-term
           ($continue k src
             ,(rewrite-exp exp
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

(define (simplify conts)
  (eta-reduce (beta-reduce conts 0) 0))
