;;; Abstract constant folding on CPS
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
;;; This pass uses the abstract interpretation provided by type analysis
;;; to fold constant values and type predicates.  It is most profitably
;;; run after CSE, to take advantage of scalar replacement.
;;;
;;; Code:

(define-module (language cps type-fold)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (language cps renumber)
  #:use-module (language cps types)
  #:export (type-fold))

(define &scalar-types
  (logior &exact-integer &flonum &char &unspecified &boolean &nil &null))

(define *branch-folders* (make-hash-table))

(define-syntax-rule (define-branch-folder name f)
  (hashq-set! *branch-folders* 'name f))

(define-syntax-rule (define-branch-folder-alias to from)
  (hashq-set! *branch-folders* 'to (hashq-ref *branch-folders* 'from)))

(define-syntax-rule (define-unary-branch-folder (name arg min max) body ...)
  (define-branch-folder name (lambda (arg min max) body ...)))

(define-syntax-rule (define-binary-branch-folder (name arg0 min0 max0
                                                       arg1 min1 max1)
                      body ...)
  (define-branch-folder name (lambda (arg0 min0 max0 arg1 min1 max1) body ...)))

(define-syntax-rule (define-unary-type-predicate-folder name &type)
  (define-unary-branch-folder (name type min max)
    (let ((type* (logand type &type)))
      (cond
       ((zero? type*) (values #t #f))
       ((eqv? type type*) (values #t #t))
       (else (values #f #f))))))

;; All the cases that are in compile-bytecode.
(define-unary-type-predicate-folder pair? &pair)
(define-unary-type-predicate-folder null? &null)
(define-unary-type-predicate-folder nil? &nil)
(define-unary-type-predicate-folder symbol? &symbol)
(define-unary-type-predicate-folder variable? &box)
(define-unary-type-predicate-folder vector? &vector)
(define-unary-type-predicate-folder struct? &struct)
(define-unary-type-predicate-folder string? &string)
(define-unary-type-predicate-folder number? &number)
(define-unary-type-predicate-folder char? &char)

(define-binary-branch-folder (eq? type0 min0 max0 type1 min1 max1)
  (cond
   ((or (zero? (logand type0 type1)) (< max0 min1) (< max1 min0))
    (values #t #f))
   ((and (eqv? type0 type1)
         (eqv? min0 min1 max0 max1)
         (zero? (logand type0 (1- type0)))
         (not (zero? (logand type0 &scalar-types))))
    (values #t #t))
   (else
    (values #f #f))))
(define-branch-folder-alias eqv? eq?)
(define-branch-folder-alias equal? eq?)

(define (compare-ranges type0 min0 max0 type1 min1 max1)
  (and (zero? (logand (logior type0 type1) (lognot &real)))
       (cond ((< max0 min1) '<)
             ((> min0 max1) '>)
             ((= min0 max0 min1 max1) '=)
             ((<= max0 min1) '<=)
             ((>= min0 max1) '>=)
             (else #f))))

(define-binary-branch-folder (< type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((<) (values #t #t))
    ((= >= >) (values #t #f))
    (else (values #f #f))))

(define-binary-branch-folder (<= type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((< <= =) (values #t #t))
    ((>) (values #t #f))
    (else (values #f #f))))

(define-binary-branch-folder (= type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((=) (values #t #t))
    ((< >) (values #t #f))
    (else (values #f #f))))

(define-binary-branch-folder (>= type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((> >= =) (values #t #t))
    ((<) (values #t #f))
    (else (values #f #f))))

(define-binary-branch-folder (> type0 min0 max0 type1 min1 max1)
  (case (compare-ranges type0 min0 max0 type1 min1 max1)
    ((>) (values #t #t))
    ((= <= <) (values #t #f))
    (else (values #f #f))))

(define (compute-folded fun dfg min-label min-var)
  (define (scalar-value type val)
    (cond
     ((eqv? type &exact-integer) val)
     ((eqv? type &flonum) (exact->inexact val))
     ((eqv? type &char) (integer->char val))
     ((eqv? type &unspecified) *unspecified*)
     ((eqv? type &boolean) (not (zero? val)))
     ((eqv? type &nil) #nil)
     ((eqv? type &null) '())
     (else (error "unhandled type" type val))))
  (let* ((typev (infer-types fun dfg #:max-label-count 3000))
         (folded? (and typev
                       (make-bitvector (/ (vector-length typev) 2) #f)))
         (folded-values (and typev
                             (make-vector (bitvector-length folded?) #f))))
    (define (label->idx label) (- label min-label))
    (define (var->idx var) (- var min-var))
    (define (maybe-fold-value! label name k def)
      (call-with-values (lambda () (lookup-post-type typev label def))
        (lambda (type min max)
          (when (and (not (zero? type))
                     (zero? (logand type (1- type)))
                     (zero? (logand type (lognot &scalar-types)))
                     (eqv? min max))
            (bitvector-set! folded? label #t)
            (vector-set! folded-values label (scalar-value type min))))))
    (define (maybe-fold-unary-branch! label name arg)
      (let* ((folder (hashq-ref *branch-folders* name)))
        (when folder
          (call-with-values (lambda () (lookup-pre-type typev label arg))
            (lambda (type min max)
              (call-with-values (lambda () (folder type min max))
                (lambda (f? v)
                  (bitvector-set! folded? label f?)
                  (vector-set! folded-values label v))))))))
    (define (maybe-fold-binary-branch! label name arg0 arg1)
      (let* ((folder (hashq-ref *branch-folders* name)))
        (when folder
          (call-with-values (lambda () (lookup-pre-type typev label arg0))
            (lambda (type0 min0 max0)
              (call-with-values (lambda () (lookup-pre-type typev label arg1))
                (lambda (type1 min1 max1)
                  (call-with-values (lambda ()
                                      (folder type0 min0 max0 type1 min1 max1))
                    (lambda (f? v)
                      (bitvector-set! folded? label f?)
                      (vector-set! folded-values label v))))))))))
    (define (visit-cont cont)
      (match cont
        (($ $cont label ($ $kargs _ _ body))
         (visit-term body label))
        (($ $cont label ($ $kclause arity body alternate))
         (visit-cont body)
         (visit-cont alternate))
        (_ #f)))
    (define (visit-term term label)
      (match term
        (($ $letk conts body)
         (for-each visit-cont conts)
         (visit-term body label))
        (($ $letrec _ _ _ body)
         (visit-term body label))
        (($ $continue k src ($ $primcall name args))
         ;; We might be able to fold primcalls that define a value or
         ;; that branch.
         (match (lookup-cont k dfg)
           (($ $kargs (_) (def))
            (maybe-fold-value! (label->idx label) name (label->idx k)
                               (var->idx def)))
           (($ $kif kt kf)
            (match args
              ((arg)
               (maybe-fold-unary-branch! (label->idx label) name
                                         (var->idx arg)))
              ((arg0 arg1)
               (maybe-fold-binary-branch! (label->idx label) name
                                          (var->idx arg0) (var->idx arg1)))))
           (_ #f)))
        (($ $continue kf src ($ $branch kt ($ $primcall name args)))
         ;; We might be able to fold primcalls that branch.
         (match args
           ((arg)
            (maybe-fold-unary-branch! (label->idx label) name
                                      (var->idx arg)))
           ((arg0 arg1)
            (maybe-fold-binary-branch! (label->idx label) name
                                       (var->idx arg0) (var->idx arg1)))))
        (_ #f)))
    (when typev
      (match fun
        (($ $cont kfun ($ $kfun src meta self tail clause))
         (visit-cont clause))))
    (values folded? folded-values)))

(define (fold-constants* fun dfg)
  (match fun
    (($ $cont min-label ($ $kfun _ _ min-var))
     (call-with-values (lambda () (compute-folded fun dfg min-label min-var))
       (lambda (folded? folded-values)
         (define (label->idx label) (- label min-label))
         (define (var->idx var) (- var min-var))
         (define (visit-cont cont)
           (rewrite-cps-cont cont
             (($ $cont label ($ $kargs names syms body))
              (label ($kargs names syms ,(visit-term body label))))
             (($ $cont label ($ $kclause arity body alternate))
              (label ($kclause ,arity ,(visit-cont body)
                               ,(and alternate (visit-cont alternate)))))
             (_ ,cont)))
         (define (visit-term term label)
           (rewrite-cps-term term
             (($ $letk conts body)
              ($letk ,(map visit-cont conts)
                ,(visit-term body label)))
             (($ $letrec names vars funs body)
              ($letrec names vars (map visit-fun funs)
                ,(visit-term body label)))
             (($ $continue k src (and fun ($ $fun)))
              ($continue k src ,(visit-fun fun)))
             (($ $continue k src (and primcall ($ $primcall)))
              ,(if (and folded?
                        (bitvector-ref folded? (label->idx label)))
                   (let ((val (vector-ref folded-values (label->idx label))))
                     ;; Uncomment for debugging.
                     ;; (pk 'folded src primcall val)
                     (match (lookup-cont k dfg)
                       (($ $kargs)
                        (let-fresh (k*) (v*)
                          ;; Rely on DCE to elide this expression, if
                          ;; possible.
                          (build-cps-term
                            ($letk ((k* ($kargs (#f) (v*)
                                          ($continue k src ($const val)))))
                              ($continue k* src ,primcall)))))
                       (($ $kif kt kf)
                        ;; Folded branch.
                        (build-cps-term
                          ($continue (if val kt kf) src ($values ()))))))
                   term))
             (($ $continue kf src ($ $branch kt ($ $primcall)))
              ,(if (and folded?
                        (bitvector-ref folded? (label->idx label)))
                   ;; Folded branch.
                   (let ((val (vector-ref folded-values (label->idx label))))
                     (build-cps-term
                       ($continue (if val kt kf) src ($values ()))))
                   term))
             (_ ,term)))
         (define (visit-fun fun)
           (rewrite-cps-exp fun
             (($ $fun free body)
              ($fun free ,(fold-constants* body dfg)))))
         (rewrite-cps-cont fun
           (($ $cont kfun ($ $kfun src meta self tail clause))
            (kfun ($kfun src meta self ,tail ,(visit-cont clause))))))))))

(define (type-fold fun)
  (let* ((fun (renumber fun))
         (dfg (compute-dfg fun)))
    (with-fresh-name-state-from-dfg dfg
      (fold-constants* fun dfg))))
