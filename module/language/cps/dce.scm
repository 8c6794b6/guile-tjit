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
;;; Various optimizations can inline calls from one continuation to some
;;; other continuation, usually in response to information about the
;;; return arity of the call.  That leaves us with dangling
;;; continuations that aren't reachable any more from the procedure
;;; entry.  This pass will remove them.
;;;
;;; This pass also kills dead expressions: code that has no side
;;; effects, and whose value is unused.  It does so by marking all live
;;; values, and then discarding other values as dead.  This happens
;;; recursively through procedures, so it should be possible to elide
;;; dead procedures as well.
;;;
;;; Code:

(define-module (language cps dce)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (language cps effects-analysis)
  #:use-module (language cps renumber)
  #:export (eliminate-dead-code))

(define-record-type $fun-data
  (make-fun-data min-label effects live-conts defs)
  fun-data?
  (min-label fun-data-min-label)
  (effects fun-data-effects)
  (live-conts fun-data-live-conts)
  (defs fun-data-defs))

(define (compute-defs dfg min-label label-count)
  (define (cont-defs k)
    (match (lookup-cont k dfg)
      (($ $kargs names vars) vars)
      (_ #f)))
  (define (idx->label idx) (+ idx min-label))
  (let ((defs (make-vector label-count #f)))
    (let lp ((n 0))
      (when (< n label-count)
        (vector-set!
         defs
         n
         (match (lookup-cont (idx->label n) dfg)
           (($ $kargs _ _ body)
            (match (find-call body)
              (($ $continue k) (cont-defs k))))
           (($ $kreceive arity kargs)
            (cont-defs kargs))
           (($ $kclause arity ($ $cont kargs ($ $kargs names syms)))
            syms)
           (($ $kif) #f)
           (($ $kfun src meta self) (list self))
           (($ $ktail) #f)))
        (lp (1+ n))))
    defs))

(define (constant-type val)
  (cond
   ((and (exact-integer? val) (<= 0 val most-positive-fixnum))
    'size)
   ((number? val) 'number)
   ((vector? val) 'vector)
   ((pair? val) 'pair)
   ((char? val) 'char)
   (else #f)))

(define (lookup-type arg dfg)
  (match (lookup-predecessors (lookup-def arg dfg) dfg)
    ((pred)
     (match (lookup-cont pred dfg)
       (($ $kargs _ _ term)
        (match (find-expression term)
          (($ $const val) (constant-type val))
          (($ $primcall name args)
           (match (check-primcall-arg-types dfg name args)
             ((type) type)
             (_ #f)))
          (($ $values (var)) (lookup-type var dfg))
          (($ $void) 'unspecified)
          (_ #f)))
       (_ #f)))
    (_ #f)))

(define (default-type-checker . _)
  #f)

(define *primcall-type-checkers* (make-hash-table))

(define-syntax-rule (define-primcall-type-checker (name dfg arg ...)
                      body ...)
  (hashq-set! *primcall-type-checkers* 'name
              (lambda (dfg arg ...) body ...)))

(define-syntax-rule (define-simple-primcall-types
                      ((name (arg arg-type) ...) result ...)
                      ...)
  (begin
    (define-primcall-type-checker (name dfg arg ...)
      (define (check-type val type)
        (or (eqv? type #t)
            (eqv? (lookup-type val dfg) type)))
      (and (check-type arg 'arg-type)
           ...
           '(result ...)))
    ...))

(define-simple-primcall-types
  ((cons (car #t) (cdr #t)) pair)
  ((car (pair pair)) #f)
  ((cdr (pair pair)) #f)
  ((set-car! (pair pair) (car #t)))
  ((set-cdr! (pair pair) (car #t)))
  ((make-vector (len size) (fill #t)) vector)
  ((make-vector/immediate (len size) (fill #t)) vector)
  ((vector-length (vector vector)) size)
  ((box (val #t)) box)
  ((box-ref (box box)) #f)
  ((box-set! (box box) (val #t)))
  ((make-struct (vtable vtable) (len size)) struct)
  ((make-struct/immediate (vtable vtable) (len size)) struct))

(define (vector-index-within-range? dfg vec idx)
  (define (constant-value var)
    (call-with-values (lambda () (find-constant-value var dfg))
      (lambda (found? val)
        (unless found?
          (error "should have found value" var))
        val)))
  (let lp ((vec vec))
    (match (find-defining-expression vec dfg)
      (($ $primcall 'make-vector/immediate (len fill))
       (<= 0 (constant-value idx) (1- (constant-value len))))
      (($ $values (vec)) (lp vec))
      (_ #f))))

(define-primcall-type-checker (vector-ref/immediate dfg vec idx)
  (and (vector-index-within-range? dfg vec idx)
       '(#f)))

(define-primcall-type-checker (vector-set!/immediate dfg vec idx val)
  (and (vector-index-within-range? dfg vec idx)
       '()))

(define (check-primcall-arg-types dfg name args)
  (apply (hashq-ref *primcall-type-checkers* name default-type-checker)
         dfg args))

(define (compute-live-code fun)
  (let* ((fun-data-table (make-hash-table))
         (dfg (compute-dfg fun #:global? #t))
         (live-vars (make-bitvector (dfg-var-count dfg) #f))
         (changed? #f))
    (define (mark-live! var)
      (unless (value-live? var)
        (set! changed? #t)
        (bitvector-set! live-vars var #t)))
    (define (value-live? var)
      (bitvector-ref live-vars var))
    (define (ensure-fun-data fun)
      (or (hashq-ref fun-data-table fun)
          (call-with-values (lambda ()
                              ((make-local-cont-folder label-count max-label)
                               (lambda (k cont label-count max-label)
                                 (values (1+ label-count) (max k max-label)))
                               fun 0 -1))
            (lambda (label-count max-label)
              (let* ((min-label (- (1+ max-label) label-count))
                     (effects (compute-effects dfg min-label label-count))
                     (live-conts (make-bitvector label-count #f))
                     (defs (compute-defs dfg min-label label-count))
                     (fun-data (make-fun-data
                                min-label effects live-conts defs)))
                (hashq-set! fun-data-table fun fun-data)
                (set! changed? #t)
                fun-data)))))
    (define (visit-fun fun)
      (match (ensure-fun-data fun)
        (($ $fun-data min-label effects live-conts defs)
         (define (types-check? exp)
           (match exp
             (($ $primcall name args)
              (check-primcall-arg-types dfg name args))))
         (define (visit-grey-exp n exp)
           (let ((defs (vector-ref defs n))
                 (fx (vector-ref effects n)))
             (or
              ;; No defs; perhaps continuation is $ktail.
              (not defs)
              ;; Do we have a live def?
              (or-map value-live? defs)
              ;; Does this expression cause any effects we don't know
              ;; how to elide?
              (not (effect-free?
                    (exclude-effects fx
                                     (logior &allocation &type-check
                                             &car &cdr &vector &struct &box))))
              ;; Does it cause a type check, but we can't prove that the
              ;; types check?
              (and (causes-effects? fx &type-check)
                   (not (types-check? exp)))
              (cond
               ((effect-free?
                 (exclude-effects fx (logior &type-check &allocation)))
                ;; We've already handled type checks.  If allocation is
                ;; the only remaining effect, this expression is still
                ;; dead.
                #f)
               (else
                ;; We might have a setter.  If the object being assigned
                ;; to is live, then this expression is live.
                (match exp
                  (($ $primcall 'vector-set!/immediate (vec idx val))
                   (value-live? vec))
                  (($ $primcall 'set-car! (pair car))
                   (value-live? pair))
                  (($ $primcall 'set-cdr! (pair cdr))
                   (value-live? pair))
                  (($ $primcall 'box-set! (box val))
                   (value-live? box))
                  (_ #t)))))))
         (define (idx->label idx) (+ idx min-label))
         (let lp ((n (1- (vector-length effects))))
           (unless (< n 0)
             (let ((cont (lookup-cont (idx->label n) dfg)))
               (match cont
                 (($ $kargs _ _ body)
                  (let lp ((body body))
                    (match body
                      (($ $letk conts body) (lp body))
                      (($ $letrec names syms funs body)
                       (lp body)
                       (for-each (lambda (sym fun)
                                   (when (value-live? sym)
                                     (match fun
                                       (($ $fun free body)
                                        (visit-fun body)))))
                                 syms funs))
                      (($ $continue k src exp)
                       (unless (bitvector-ref live-conts n)
                         (when (visit-grey-exp n exp)
                           (set! changed? #t)
                           (bitvector-set! live-conts n #t)))
                       (when (bitvector-ref live-conts n)
                         (match exp
                           ((or ($ $void) ($ $const) ($ $prim))
                            #f)
                           (($ $fun free body)
                            (visit-fun body))
                           (($ $prompt escape? tag handler)
                            (mark-live! tag))
                           (($ $call proc args)
                            (mark-live! proc)
                            (for-each mark-live! args))
                           (($ $callk k proc args)
                            (mark-live! proc)
                            (for-each mark-live! args))
                           (($ $primcall name args)
                            (for-each mark-live! args))
                           (($ $values args)
                            (match (vector-ref defs n)
                              (#f (for-each mark-live! args))
                              (defs (for-each (lambda (use def)
                                                (when (value-live? def)
                                                  (mark-live! use)))
                                              args defs))))))))))
                 (($ $kreceive arity kargs) #f)
                 (($ $kif) #f)
                 (($ $kclause arity ($ $cont kargs ($ $kargs names syms body)))
                  (for-each mark-live! syms))
                 (($ $kfun src meta self)
                  (mark-live! self))
                 (($ $ktail) #f))
               (lp (1- n))))))))
    (unless (= (dfg-var-count dfg) (var-counter))
      (error "internal error" (dfg-var-count dfg) (var-counter)))
    (let lp ()
      (set! changed? #f)
      (visit-fun fun)
      (when changed? (lp)))
    (values fun-data-table live-vars)))

(define (process-eliminations fun fun-data-table live-vars)
  (define (value-live? var)
    (bitvector-ref live-vars var))
  (define (make-adaptor name k defs)
    (let* ((names (map (lambda (_) 'tmp) defs))
           (syms (map (lambda (_) (fresh-var)) defs))
           (live (filter-map (lambda (def sym)
                               (and (value-live? def)
                                    sym))
                             defs syms)))
      (build-cps-cont
        (name ($kargs names syms
                ($continue k #f ($values live)))))))
  (define (visit-fun fun)
    (match (hashq-ref fun-data-table fun)
      (($ $fun-data min-label effects live-conts defs)
       (define (label->idx label) (- label min-label))
       (define (visit-cont cont)
         (match (visit-cont* cont)
           ((cont) cont)))
       (define (visit-cont* cont)
         (match cont
           (($ $cont label cont)
            (match cont
              (($ $kargs names syms body)
               (match (filter-map (lambda (name sym)
                                    (and (value-live? sym)
                                         (cons name sym)))
                                  names syms)
                 (((names . syms) ...)
                  (list
                   (build-cps-cont
                     (label ($kargs names syms
                              ,(visit-term body label))))))))
              (($ $kfun src meta self tail clause)
               (list
                (build-cps-cont
                  (label ($kfun src meta self ,tail
                           ,(and clause (visit-cont clause)))))))
              (($ $kclause arity body alternate)
               (list
                (build-cps-cont
                  (label ($kclause ,arity
                           ,(visit-cont body)
                           ,(and alternate
                                 (visit-cont alternate)))))))
              (($ $kreceive ($ $arity req () rest () #f) kargs)
               (let ((defs (vector-ref defs (label->idx label))))
                 (if (and-map value-live? defs)
                     (list (build-cps-cont (label ,cont)))
                     (let-fresh (adapt) ()
                       (list (make-adaptor adapt kargs defs)
                             (build-cps-cont
                               (label ($kreceive req rest adapt))))))))
              (_ (list (build-cps-cont (label ,cont))))))))
       (define (visit-conts conts)
         (append-map visit-cont* conts))
       (define (visit-term term term-k)
         (match term
           (($ $letk conts body)
            (let ((body (visit-term body term-k)))
              (match (visit-conts conts)
                (() body)
                (conts (build-cps-term ($letk ,conts ,body))))))
           (($ $letrec names syms funs body)
            (let ((body (visit-term body term-k)))
              (match (filter-map
                      (lambda (name sym fun)
                        (and (value-live? sym)
                             (match fun
                               (($ $fun free body)
                                (list name
                                      sym
                                      (build-cps-exp
                                        ($fun free ,(visit-fun body))))))))
                      names syms funs)
                (() body)
                (((names syms funs) ...)
                 (build-cps-term
                   ($letrec names syms funs ,body))))))
           (($ $continue k src ($ $values args))
            (match (vector-ref defs (label->idx term-k))
              (#f term)
              (defs
                (let ((args (filter-map (lambda (use def)
                                          (and (value-live? def) use))
                                        args defs)))
                  (build-cps-term
                    ($continue k src ($values args)))))))
           (($ $continue k src exp)
            (if (bitvector-ref live-conts (label->idx term-k))
                (rewrite-cps-term exp
                  (($ $fun free body)
                   ($continue k src ($fun free ,(visit-fun body))))
                  (_
                   ,(match (vector-ref defs (label->idx term-k))
                      ((or #f ((? value-live?) ...))
                       (build-cps-term
                         ($continue k src ,exp)))
                      (syms
                       (let-fresh (adapt) ()
                         (build-cps-term
                           ($letk (,(make-adaptor adapt k syms))
                             ($continue adapt src ,exp))))))))
                (build-cps-term ($continue k src ($values ())))))))
       (visit-cont fun))))
  (visit-fun fun))

(define (eliminate-dead-code fun)
  (call-with-values (lambda () (renumber fun))
    (lambda (fun nlabels nvars)
      (parameterize ((label-counter nlabels)
                     (var-counter nvars))
        (call-with-values (lambda () (compute-live-code fun))
          (lambda (fun-data-table live-vars)
            (process-eliminations fun fun-data-table live-vars)))))))
