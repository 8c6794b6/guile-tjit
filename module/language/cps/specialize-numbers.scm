;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2015 Free Software Foundation, Inc.

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
;;; Some arithmetic operations have multiple implementations: one
;;; polymorphic implementation that works on all kinds of numbers, like
;;; `add', and one or more specialized variants for unboxed numbers of
;;; some kind, like `fadd'.  If we can replace a polymorphic
;;; implementation with a monomorphic implementation, we should do so --
;;; it will speed up the runtime and avoid boxing numbers.
;;;
;;; A polymorphic operation can be specialized if its result is
;;; specialized.  To specialize an operation, we manually unbox its
;;; arguments and box its return value, relying on CSE to remove boxes
;;; where possible.
;;;
;;; We also want to specialize phi variables.  A phi variable is bound
;;; by a continuation with more than one predecessor.  For example in
;;; this code:
;;;
;;;   (+ 1.0 (if a 2.0 3.0))
;;;
;;; We want to specialize this code to:
;;;
;;;   (f64->scm (fl+ (scm->f64 1.0) (if a (scm->f64 2.0) (scm->f64 3.0))))
;;;
;;; Hopefully later passes will remove the conversions.  In any case,
;;; specialization will likely result in a lower heap-number allocation
;;; rate, and that cost is higher than the extra opcodes to do
;;; conversions.  This transformation is especially important for loop
;;; variables.
;;;
;;; Code:

(define-module (language cps specialize-numbers)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps renumber)
  #:use-module (language cps types)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (specialize-numbers))

(define (specialize-f64-binop cps k src op a b)
  (let ((fop (match op
               ('add 'fadd)
               ('sub 'fsub)
               ('mul 'fmul)
               ('div 'fdiv))))
    (with-cps cps
      (letv f64-a f64-b result)
      (letk kbox ($kargs ('result) (result)
                   ($continue k src
                     ($primcall 'f64->scm (result)))))
      (letk kop ($kargs ('f64-b) (f64-b)
                  ($continue kbox src
                    ($primcall fop (f64-a f64-b)))))
      (letk kunbox-b ($kargs ('f64-a) (f64-a)
                       ($continue kop src
                         ($primcall 'scm->f64 (b)))))
      (build-term
        ($continue kunbox-b src
          ($primcall 'scm->f64 (a)))))))

(define (specialize-f64-operations cps)
  (define (visit-cont label cont cps types)
    (match cont
      (($ $kfun)
       (values cps (infer-types cps label)))
      (($ $kargs names vars
          ($ $continue k src
             ($ $primcall (and op (or 'add 'sub 'mul 'div)) (a b))))
       (match (intmap-ref cps k)
         (($ $kargs (_) (result))
          (call-with-values (lambda ()
                              (lookup-post-type types label result 0))
            (lambda (type min max)
              (values
               (if (eqv? type &flonum)
                   (with-cps cps
                     (let$ body (specialize-f64-binop k src op a b))
                     (setk label ($kargs names vars ,body)))
                   cps)
               types))))))
      (_ (values cps types))))

  (values (intmap-fold visit-cont cps cps #f)))

;; Compute a map from VAR -> LABEL, where LABEL indicates the cont that
;; binds VAR.
(define (compute-defs conts labels)
  (intset-fold
   (lambda (label defs)
     (match (intmap-ref conts label)
       (($ $kfun src meta self tail clause)
        (intmap-add defs self label))
       (($ $kargs names vars)
        (fold1 (lambda (var defs)
                 (intmap-add defs var label))
               vars defs))
       (_ defs)))
   labels empty-intmap))

;; Compute vars whose definitions are all inexact reals and whose uses
;; include an unbox operation.
(define (compute-specializable-f64-vars cps body preds defs)
  ;; Compute a map of VAR->LABEL... indicating the set of labels that
  ;; define VAR with f64 values, given the set of vars F64-VARS which is
  ;; known already to be f64-valued.
  (define (collect-f64-def-labels f64-vars)
    (define (add-f64-def f64-defs var label)
      (intmap-add f64-defs var (intset label) intset-union))
    (intset-fold (lambda (label f64-defs)
                   (match (intmap-ref cps label)
                     (($ $kargs _ _ ($ $continue k _ exp))
                      (match exp
                        ((or ($ $primcall 'f64->scm (_))
                             ($ $const (and (? number?) (? inexact?) (? real?))))
                         (match (intmap-ref cps k)
                           (($ $kargs (_) (def))
                            (add-f64-def f64-defs def label))))
                        (($ $values vars)
                         (match (intmap-ref cps k)
                           (($ $kargs _ defs)
                            (fold (lambda (var def f64-defs)
                                    (if (intset-ref f64-vars var)
                                        (add-f64-def f64-defs def label)
                                        f64-defs))
                                  f64-defs vars defs))
                           ;; Could be $ktail for $values.
                           (_ f64-defs)))
                        (_ f64-defs)))
                     (_ f64-defs)))
                 body empty-intmap))

  ;; Compute the set of vars which are always f64-valued.
  (define (compute-f64-defs)
    (fixpoint
     (lambda (f64-vars)
       (intmap-fold
        (lambda (def f64-pred-labels f64-vars)
          (if (and (not (intset-ref f64-vars def))
                   ;; Are all defining expressions f64-valued?
                   (and-map (lambda (pred)
                              (intset-ref f64-pred-labels pred))
                            (intmap-ref preds (intmap-ref defs def))))
              (intset-add f64-vars def)
              f64-vars))
        (collect-f64-def-labels f64-vars)
        f64-vars))
     empty-intset))

  ;; Compute the set of vars that may ever be unboxed.
  (define (compute-f64-uses f64-defs)
    (intset-fold
     (lambda (label f64-uses)
       (match (intmap-ref cps label)
         (($ $kargs _ _ ($ $continue k _ exp))
          (match exp
            (($ $primcall 'scm->f64 (var))
             (intset-add f64-uses var))
            (($ $values (var))
             (match (intmap-ref cps k)
               (($ $kargs (_) (def))
                (if (intset-ref f64-defs def)
                    (intset-add f64-uses var)
                    f64-uses))
               ;; Could be $ktail.
               (_ f64-uses)))
            (_ f64-uses)))
         (_ f64-uses)))
     body empty-intset))

  (let ((f64-defs (compute-f64-defs)))
    (intset-intersect f64-defs (compute-f64-uses f64-defs))))

(define (compute-phi-vars cps preds)
  (intmap-fold (lambda (label preds phis)
                 (match preds
                   (() phis)
                   ((_) phis)
                   (_
                    (match (intmap-ref cps label)
                      (($ $kargs names vars)
                       (fold1 (lambda (var phis)
                                (intset-add phis var))
                              vars phis))
                      (_ phis)))))
               preds empty-intset))

;; Compute the set of variables which have more than one definition,
;; whose definitions are always f64-valued, and which have at least one
;; use that is an unbox operation.
(define (compute-specializable-f64-phis cps body preds defs)
  (intset-intersect
   (compute-specializable-f64-vars cps body preds defs)
   (compute-phi-vars cps preds)))

;; Each definition of an f64 variable should unbox that variable.  The
;; cont that binds the variable should re-box it under its original
;; name, and rely on CSE to remove the boxing as appropriate.
(define (apply-f64-specialization cps kfun body preds defs phis)
  (define (compute-unbox-labels)
    (intset-fold (lambda (phi labels)
                   (fold1 (lambda (pred labels)
                            (intset-add labels pred))
                          (intmap-ref preds (intmap-ref defs phi))
                          labels))
                 phis empty-intset))
  (define (unbox-operands)
    (define (unbox-arg cps arg def-var have-arg)
      (if (intset-ref phis def-var)
          (with-cps cps
            (letv f64)
            (let$ body (have-arg f64))
            (letk kunboxed ($kargs ('f64) (f64) ,body))
            (build-term
              ($continue kunboxed #f ($primcall 'scm->f64 (arg)))))
          (have-arg cps arg)))
    (define (unbox-args cps args def-vars have-args)
      (match args
        (() (have-args cps '()))
        ((arg . args)
         (match def-vars
           ((def-var . def-vars)
            (unbox-arg cps arg def-var
                       (lambda (cps arg)
                         (unbox-args cps args def-vars
                                     (lambda (cps args)
                                       (have-args cps (cons arg args)))))))))))
    (intset-fold
     (lambda (label cps)
       (match (intmap-ref cps label)
         (($ $kargs names vars ($ $continue k src exp))
          ;; For expressions that define a single value, we know we need
          ;; to unbox that value.  For $values though we might have to
          ;; unbox just a subset of values.
          (match exp
            (($ $values args)
             (let ((def-vars (match (intmap-ref cps k)
                               (($ $kargs _ defs) defs))))
               (with-cps cps
                 (let$ term (unbox-args
                             args def-vars
                             (lambda (cps args)
                               (with-cps cps
                                 (build-term
                                   ($continue k src ($values args)))))))
                 (setk label ($kargs names vars ,term)))))
            (_
             (with-cps cps
               (letv const)
               (letk kunbox ($kargs ('const) (const)
                              ($continue k src
                                ($primcall 'scm->f64 (const)))))
               (setk label ($kargs names vars
                             ($continue k src ,exp)))))))))
     (compute-unbox-labels)
     cps))
  (define (compute-box-labels)
    (intset-fold (lambda (phi labels)
                   (intset-add labels (intmap-ref defs phi)))
                 phis empty-intset))
  (define (box-results cps)
    (intset-fold
     (lambda (label cps)
       (match (intmap-ref cps label)
         (($ $kargs names vars term)
          (let* ((boxed (fold1 (lambda (var boxed)
                                 (if (intset-ref phis var)
                                     (intmap-add boxed var (fresh-var))
                                     boxed))
                               vars empty-intmap))
                 (bound-vars (map (lambda (var)
                                    (intmap-ref boxed var (lambda (var) var)))
                                  vars)))
            (define (box-var cps name var done)
              (let ((f64 (intmap-ref boxed var (lambda (_) #f))))
                (if f64
                    (with-cps cps
                      (let$ term (done))
                      (letk kboxed ($kargs (name) (var) ,term))
                      (build-term
                        ($continue kboxed #f ($primcall 'f64->scm (f64)))))
                    (done cps))))
            (define (box-vars cps names vars done)
              (match vars
                (() (done cps))
                ((var . vars)
                 (match names
                   ((name . names)
                    (box-var cps name var
                             (lambda (cps)
                               (box-vars cps names vars done))))))))
            (with-cps cps
              (let$ box-term (box-vars names vars
                                       (lambda (cps)
                                         (with-cps cps term))))
              (setk label ($kargs names bound-vars ,box-term)))))))
     (compute-box-labels)
     cps))
  (pk 'specializing phis)
  (box-results (unbox-operands)))

(define (specialize-f64-phis cps)
  (intmap-fold
   (lambda (kfun body cps)
     (let* ((preds (compute-predecessors cps kfun #:labels body))
            (defs (compute-defs cps body))
            (phis (compute-specializable-f64-phis cps body preds defs)))
       (if (eq? phis empty-intset)
           cps
           (apply-f64-specialization cps kfun body preds defs phis))))
   (compute-reachable-functions cps)
   cps))

(define (specialize-numbers cps)
  ;; Type inference wants a renumbered graph; OK.
  (let ((cps (renumber cps)))
    (with-fresh-name-state cps
      (specialize-f64-phis (specialize-f64-operations cps)))))
