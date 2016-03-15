;;;; Definitions for compiling to IR

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
;;;;
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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA
;;;;

;;; Commentary:
;;;
;;; A module containing definition of operations to compile bytecode to ANF IR
;;; used in vm-tjit.
;;;
;;; Code:

(define-module (system vm native tjit ir)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit parse)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables)
  #:export (make-ir
            <ir>
            ir-snapshots
            ir-snapshot-id set-ir-snapshot-id!
            ir-min-sp-offset set-ir-min-sp-offset!
            ir-max-sp-offset set-ir-max-sp-offset!
            ir-bytecode-index set-ir-bytecode-index!
            ir-vars
            ir-return-subr? set-ir-return-subr!
            ir-last-op? set-ir-last-op!

            define-ir
            define-interrupt-ir
            gen-put-element-type
            gen-put-index
            dereference-scm
            take-snapshot!
            scm-ref
            u64-ref
            var-ref
            type-ref
            with-boxing
            with-type-guard
            current-sp-offset
            current-fp-offset
            inline-current-call?
            inline-current-return?
            env ir ip ra dl locals next

            make-var
            make-vars
            get-max-sp-offset
            get-initial-sp-offset
            get-initial-fp-offset
            take-snapshot
            gen-load-thunk
            with-frame-ref

            define-scan
            define-ti
            define-anf

            push-scan-sp-offset!
            pop-scan-sp-offset!
            push-scan-fp-offset!
            pop-scan-fp-offset!
            set-scan-initial-fields!

            *ir-procedures*))

;;;
;;; IR record type
;;;

(define-record-type <ir>
  (make-ir snapshots snapshot-id vars min-sp-offset max-sp-offset
           bytecode-index return-subr? last-op?)
  ir?

  ;; Hash table containing snapshots.
  (snapshots ir-snapshots)

  ;; Current snapshot ID.
  (snapshot-id ir-snapshot-id set-ir-snapshot-id!)

  ;; List of symbols for variables.
  (vars ir-vars)

  ;; Current minimum SP offset.
  (min-sp-offset ir-min-sp-offset set-ir-min-sp-offset!)

  ;; Current maximum SP offset.
  (max-sp-offset ir-max-sp-offset set-ir-max-sp-offset!)

  ;; Current bytecode index.
  (bytecode-index ir-bytecode-index set-ir-bytecode-index!)

  ;; Flag for subr call.
  (return-subr? ir-return-subr? set-ir-return-subr!)

  ;; Flag for last recorded operation.
  (last-op? ir-last-op? set-ir-last-op!))


;;;
;;; Exported hash tablel
;;;

(define *ir-procedures*
  (make-hash-table 255))


;;;
;;; Macros for scan
;;;

(define-syntax-rule (push-scan-sp-offset! env n)
  (set-env-sp-offset! env (- (env-sp-offset env) n)))

(define-syntax-rule (pop-scan-sp-offset! env n)
  (set-env-sp-offset! env (+ (env-sp-offset env) n)))

(define-syntax-rule (push-scan-fp-offset! env n)
  (set-env-fp-offset! env (- (env-fp-offset env) n)))

(define-syntax-rule (pop-scan-fp-offset! env n)
  (set-env-fp-offset! env (+ (env-fp-offset env) n)))

(define-syntax-rule (set-scan-initial-fields! env)
  (let-syntax
      ((update! (syntax-rules ()
                  ((_ setter current olds)
                   (setter env (cons (current env) (olds env)))))))
    (update! set-env-sp-offsets! env-sp-offset env-sp-offsets)
    (update! set-env-fp-offsets! env-fp-offset env-fp-offsets)))


;;;
;;; Macro for ANF
;;;

(define-syntax-rule (define-anf (name arg ...) . body)
  (let ((test-proc (lambda (op locals)
                     #t))
        (anf-proc (lambda (%env %ir %next %ip %ra %dl %locals arg ...)
                    (syntax-parameterize
                        ((env (identifier-syntax %env))
                         (ir (identifier-syntax %ir))
                         (next (identifier-syntax %next))
                         (ip (identifier-syntax %ip))
                         (ra (identifier-syntax %ra))
                         (dl (identifier-syntax %dl))
                         (locals (identifier-syntax %locals)))
                      . body))))
    (hashq-set! *ir-procedures* 'name (list (cons test-proc anf-proc)))))


;;;
;;; Macros for ANF
;;;

(define (make-var index)
  (string->symbol (string-append "v" (number->string index))))

(define (make-vars locals)
  ;; Might better to use other data structure than alist for variables.
  ;; Number of variables won't change after getting the number of locals from
  ;; `accumulate-locals'.
  (map (lambda (n)
         (cons n (make-var n)))
       locals))

(define (get-max-sp-offset sp-offset fp-offset nlocals)
  (max fp-offset
       (- (+ sp-offset nlocals) 1)
       (if (< fp-offset 0)
           (- (+ (- fp-offset) nlocals) 1)
           0)))

(define (get-initial-sp-offset parent-snapshot)
  ;; Initial offset of root trace is constantly 0. Initial offset of side
  ;; trace is where parent trace left, using offset value from SNAPSHOT.
  (match parent-snapshot
    (($ $snapshot _ sp-offset) sp-offset)
    (_ 0)))

(define (get-initial-fp-offset parent-snapshot)
  ;; Initial offset of root trace is constantly 0. Initial offset of side
  ;; trace is where parent trace left, using offset value from SNAPSHOT.
  (match parent-snapshot
    (($ $snapshot _ _ fp-offset) fp-offset)
    (_ 0)))

(define* (take-snapshot ip dst-offset locals vars indices id sp-offset fp-offset
                        min-sp-offset max-sp-offset inline-depth env
                        #:optional (refill? #f))
  (let* ((nlocals (vector-length locals))
         (dst-ip (+ ip (* dst-offset 4)))
         (indices (filter (lambda (i)
                            (<= min-sp-offset i max-sp-offset))
                          indices))
         (args (let lp ((vars vars) (acc '()))
                 (match vars
                   (((n . var) . vars)
                    (if (memq n indices)
                        (lp vars (cons var acc))
                        (lp vars acc)))
                   (()
                    (if refill?
                        (append acc (list (make-var (+ sp-offset nlocals))
                                          (make-var (+ sp-offset nlocals 1))))
                        acc)))))
         (snapshot (make-snapshot id sp-offset fp-offset nlocals indices
                                  env dst-ip inline-depth refill?)))
    (values `(%snap ,id ,@args) snapshot)))

(define-syntax gen-load-thunk
  (syntax-rules ()
    ((_ proc nlocals skip-var?)
     (let* ((return-subr? (ir-return-subr? ir))
            (stack-size (vector-length locals))
            (sp-offset (current-sp-offset))
            (min-local-index (+ (- stack-size proc 1) sp-offset 2))
            (max-local-index (+ nlocals sp-offset))
            (live-indices (env-live-indices env))
            (load-up-frame
             (lambda ()
               (let lp ((vars (reverse (ir-vars ir))) (loaded '()))
                 (match vars
                   (((n . var) . vars)
                    (cond
                     ((or (skip-var? var)
                          (memq n live-indices))
                      (lp vars loaded))
                     ((< min-local-index n max-local-index)
                      (let* ((entries (env-entry-types env))
                             (t (assq-ref entries n)))
                        (with-frame-ref var t n lp vars (cons n loaded))))
                     (else
                      (lp vars loaded))))
                   (()
                    (let ((live-indices
                           (let lp ((loaded loaded) (acc live-indices))
                             (match loaded
                               ((k . loaded)
                                (if (memq k acc)
                                    (lp loaded acc)
                                    (lp loaded (cons k acc))))
                               (() acc)))))
                      (set-env-live-indices! env live-indices)
                      (next))))))))
       (lambda ()
         (when (and (not (env-parent-snapshot env))
                    (< 0 (current-fp-offset)))
           (nyi "root trace with up-frame load"))
         (set-ir-return-subr! ir #f)
         (load-up-frame))))))

(define-syntax-rule (with-frame-ref var type idx next . args)
  (cond
   ((dynamic-link? type)
    `(let ((,var ,(dynamic-link-offset type)))
       ,(next . args)))
   ((return-address? type)
    `(let ((,var ,(pointer-address (return-address-ip type))))
       ,(next . args)))
   ((or (eq? type &flonum)
        (eq? type &f64))
    `(let ((,var (%fref/f ,idx ,type)))
       ,(next . args)))
   (else
    `(let ((,var (%fref ,idx ,type)))
       ,(next . args)))))

(define-syntax define-ir-syntax-parameters
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define-syntax-parameter name
         (lambda (x)
           'name "uninitialized" x))
       ...))))

(define-ir-syntax-parameters env ir ip ra dl locals next)

(define-syntax-rule (gen-entry-type ty arg rest)
  (let ((i (+ arg (env-sp-offset env))))
    (set-entry-type! env i ty)
    (gen-scan-type . rest)))

(define-syntax gen-scan-type
  (syntax-rules (scm fixnum flonum pair vector box bytevector u64 f64)
    ((_) (values))
    ((_ (scm arg) . rest) (gen-entry-type &scm arg rest))
    ((_ (fixnum arg) . rest) (gen-entry-type &fixnum arg rest))
    ((_ (flonum arg) . rest) (gen-entry-type &flonum arg rest))
    ((_ (pair arg) . rest) (gen-entry-type &pair arg rest))
    ((_ (vector arg) . rest) (gen-entry-type &vector arg rest))
    ((_ (box arg) . rest) (gen-entry-type &box arg rest))
    ((_ (bytevector arg) . rest) (gen-entry-type &bytevector arg rest))
    ((_ (u64 arg) . rest) (gen-entry-type &u64 arg rest))
    ((_ (f64 arg) . rest) (gen-entry-type &f64 arg rest))
    ((_ (other arg) . rest) (gen-scan-type . rest))))

(define-syntax gen-infer-type
  (syntax-rules (scm! fixnum! flonum! pair! vector! box! u64! f64!)
    ((_ (scm! arg) . rest)
     (set-inferred-type! env (+ arg (env-sp-offset env)) &scm))
    ((_ (fixnum! arg) . rest)
     (set-inferred-type! env (+ arg (env-sp-offset env)) &fixnum))
    ((_ (flonum! arg) . rest)
     (set-inferred-type! env (+ arg (env-sp-offset env)) &flonum))
    ((_ (pair! arg) . rest)
     (set-inferred-type! env (+ arg (env-sp-offset env)) &pair))
    ((_ (vector! arg) . rest)
     (set-inferred-type! env (+ arg (env-sp-offset env)) &vector))
    ((_ (box! arg) . rest)
     (set-inferred-type! env (+ arg (env-sp-offset env)) &box))
    ((_ (u64! arg) . rest)
     (set-inferred-type! env (+ arg (env-sp-offset env)) &u64))
    ((_ (f64! arg) . rest)
     (set-inferred-type! env (+ arg (env-sp-offset env)) &f64))
    ((_ . other) (values))))

(define-syntax maybe-update-live-indices
  (syntax-rules ()
    ((_ idx)
     (let ((live-indices (env-live-indices env))
           (idx+sp-offset (+ idx (current-sp-offset))))
       (unless (memq idx+sp-offset live-indices)
         (set-env-live-indices! env (cons idx+sp-offset live-indices)))))))

(define-syntax gen-update-live-indices
  (syntax-rules (scm! fixnum! flonum! pair! vector! box! u64! f64!)
    ((_ (scm! arg) . rest) (maybe-update-live-indices arg))
    ((_ (fixnum! arg) . rest) (maybe-update-live-indices arg))
    ((_ (flonum! arg) . rest) (maybe-update-live-indices arg))
    ((_ (pair! arg) . rest) (maybe-update-live-indices arg))
    ((_ (vector! arg) . rest) (maybe-update-live-indices arg))
    ((_ (box! arg) . rest) (maybe-update-live-indices arg))
    ((_ (u64! arg) . rest) (maybe-update-live-indices arg))
    ((_ (f64! arg) . rest) (maybe-update-live-indices arg))
    ((_ . other) (values))))

(define-syntax define-ir
  (syntax-rules ()
    "Defines procedure to compile bytecode operation to IR, and optionally
defines procedure for scanning locals and types. E.g:

  (define-ir (add (fixnum! dst) (fixnum a) (fixnum b))
    ...)

will define two procedures: one for IR compilation taking three arguments, and
another procedure for scanning locals and types. The procedure for scanner saves
index referenced by dst, a, and b values at runtime."
    ((_ (name (flag arg) ...) . body)
     (let ((test-proc
            (lambda (op locals)
              (let lp ((flags '(flag ...)) (ns (cdr op)))
                (match (cons flags ns)
                  (((f . flags) . (n . ns))
                   (if (memq f '(fixnum
                                 flonum procedure pair vector
                                 struct string bytevector array))
                       (let* ((v (vector-ref locals n))
                              (t (type-of v)))
                         (if (eq? t (flag->type f))
                             (lp flags ns)
                             #f))
                       (lp flags ns)))
                  (_ #t)))))
           (scan-proc
            (lambda (%env %ip %dl %locals arg ...)
              (syntax-parameterize ((env (identifier-syntax %env)))
                (gen-scan-type (flag arg) ...))
              (set-scan-initial-fields! %env)
              #t))
           (ti-proc
            (lambda (%env %ip %dl %locals arg ...)
              (syntax-parameterize ((env (identifier-syntax %env)))
                (gen-infer-type (flag arg) ...))))
           (anf-proc
            (lambda (%env %ir %next %ip %ra %dl %locals arg ...)
              (syntax-parameterize
                  ((env (identifier-syntax %env))
                   (ir (identifier-syntax %ir))
                   (next (identifier-syntax %next))
                   (ip (identifier-syntax %ip))
                   (ra (identifier-syntax %ra))
                   (dl (identifier-syntax %dl))
                   (locals (identifier-syntax %locals)))
                ;; Live indices in root traces are constantly same as write
                ;; indices, loaded at the time of entry.
                (when (env-parent-snapshot env)
                  (gen-update-live-indices (flag arg) ...))
                . body))))
       (let ((add-proc! (lambda (tbl proc)
                          (let* ((elem (cons test-proc proc))
                                 (val (cond ((hashq-ref tbl 'name)
                                             => (lambda (found)
                                                  (cons elem found)))
                                            (else (list elem)))))
                            (hashq-set! tbl 'name val)))))
         (add-proc! *scan-procedures* scan-proc)
         (add-proc! *ti-procedures* ti-proc)
         (add-proc! *ir-procedures* anf-proc))))))

(define-syntax define-interrupt-ir
  (syntax-rules ()
    ((_ names-and-args . body)
     (define-ir names-and-args
       (begin
         (set-env-handle-interrupts! env #t)
         . body)))))

(define-syntax-rule (dereference-scm addr)
  (pointer->scm (dereference-pointer (make-pointer addr))))

(define-syntax-rule (current-sp-offset)
  (vector-ref (env-sp-offsets env) (ir-bytecode-index ir)))

(define-syntax-rule (current-fp-offset)
  (vector-ref (env-fp-offsets env) (ir-bytecode-index ir)))

(define-syntax-rule (inline-current-call?)
  (assq-ref (env-calls env) (env-call-num env)))

(define-syntax-rule (inline-current-return?)
  (assq-ref (env-returns env) (env-return-num env)))

(define-syntax-rule (scm-ref n)
  (vector-ref locals n))

(define-syntax-rule (u64-ref n)
  (pointer-address (scm->pointer (vector-ref locals n))))

(define-syntax-rule (var-ref n)
  (assq-ref (ir-vars ir) (+ n (current-sp-offset))))

(define-syntax-rule (type-ref n)
  (assq-ref (env-inferred-types env) (+ n (current-sp-offset))))

(define-syntax take-snapshot!
  (syntax-rules ()
    ((_ ip dst-offset)
     (take-snapshot! ip dst-offset #f))
    ((_ ip dst-offset refill?)
     (let-values (((ret snapshot)
                   (take-snapshot ip dst-offset locals (ir-vars ir)
                                  (if (env-parent-snapshot env)
                                      (vector-ref
                                       (env-write-buf env)
                                       (ir-bytecode-index ir))
                                      (env-write-indices env))
                                  (ir-snapshot-id ir)
                                  (current-sp-offset) (current-fp-offset)
                                  (ir-min-sp-offset ir) (ir-max-sp-offset ir)
                                  (env-current-inline-depth env) env refill?)))
       (let ((old-id (ir-snapshot-id ir)))
         (hashq-set! (ir-snapshots ir) old-id snapshot)
         (set-ir-snapshot-id! ir (+ old-id 1)))
       ret))))

(define-syntax-rule (with-boxing type var tmp proc)
  (cond
   ((eq? type &flonum)
    (set-env-handle-interrupts! env #t)
    `(let ((,tmp (%d2s ,var)))
       ,(proc tmp)))
   (else
    (proc var))))

(define-syntax-rule (with-type-guard type src thunk)
  `(let ((_ ,(take-snapshot! ip 0)))
     (let ((_ (%typeq ,src ,type)))
       ,(thunk))))


;;;
;;; Macro for scan
;;;

(define-syntax-rule (define-scan (name args ...) . body)
  (let ((test-proc (lambda (op locals)
                     #t))
        (scan-proc (lambda (%env %ip %dl %locals args ...)
                     (syntax-parameterize
                         ((ip (identifier-syntax %ip))
                          (dl (identifier-syntax %dl))
                          (locals (identifier-syntax %locals))
                          (env (identifier-syntax %env)))
                       . body)
                     #t)))
    (hashq-set! *scan-procedures* 'name (list (cons test-proc scan-proc)))))

;;;
;;; Macro for Type Inference
;;;

(define-syntax-rule (define-ti (name args ...) . body)
  (let ((test-proc (lambda (op locals)
                     #t))
        (ti-proc (lambda (%env %ip %dl %locals args ...)
                   (syntax-parameterize
                       ((ip (identifier-syntax %ip))
                        (dl (identifier-syntax %dl))
                        (locals (identifier-syntax %locals))
                        (env (identifier-syntax %env)))
                     . body))))
    (hashq-set! *ti-procedures* 'name (list (cons test-proc ti-proc)))))

;;; *** The dynamic environment

;; XXX: prompt
;; XXX: wind
;; XXX: unwind
;; XXX: push-fluid
;; XXX: pop-fluid
;; XXX: fluid-ref
;; XXX: fluid-set

;;; *** Strings, symbols, and keywords

;; XXX: string-length
;; XXX: string-ref
;; XXX: string->number
;; XXX: string->symbol
;; XXX: symbol->keyword

;;; *** Structs and GOOPS

;; XXX: struct-vtable
;; XXX: allocate-struct
;; XXX: struct-ref
;; XXX: struct-set!
;; XXX: allocate-struct/immediate
;; XXX: struct-ref/immediate
;; XXX: struct-set!/immediate
;; XXX: class-of
