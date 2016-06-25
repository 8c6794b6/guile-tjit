;;;; Definitions for compiling to IR

;;;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.
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

(define-module (language trace ir)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (language trace error)
  #:use-module (language trace env)
  #:use-module (language trace parameters)
  #:use-module (language trace parse)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables)
  #:export (make-ir
            <ir>
            ir-snapshots
            ir-snapshot-id set-ir-snapshot-id!
            ir-min-sp-offset set-ir-min-sp-offset!
            ir-max-sp-offset set-ir-max-sp-offset!
            ir-bytecode-index set-ir-bytecode-index!
            ir-vars
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
            src-ref
            dst-ref
            type-ref
            with-boxing
            with-type-guard
            with-type-guard-always
            current-sp-offset
            current-sp-for-ti
            current-fp-offset
            inline-current-call?
            inline-current-return?
            env ir ip ra dl locals next

            make-var
            make-vars
            get-initial-sp-offset
            get-initial-fp-offset
            take-snapshot
            gen-load-thunk
            with-stack-ref

            define-scan
            define-ti
            define-anf
            define-constant

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
           bytecode-index last-op? cached-snapshot)
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

  ;; Flag for last recorded operation.
  (last-op? ir-last-op? set-ir-last-op!)

  ;; Cached snapshot.
  (cached-snapshot ir-cached-snapshot set-ir-cached-snapshot!))


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
  (let-syntax ((update! (syntax-rules ()
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

(define (current-inline-depth env)
  "Compute current inline depth in ENV.

Counts the number of inlined calls which are currently opened, by using calls,
returns, current call-num, and current return-num."
  (let ((call-num (env-call-num env))
        (return-num (env-return-num env)))
    (let lp ((calls (env-calls env)) (acc 0))
      (match calls
        (((_ . #f) . calls)
         (lp calls acc))
        (((c . r) . calls)
         (if (and (< c call-num) (<= return-num r))
             (lp calls (+ acc 1))
             (lp calls acc)))
        (()
         (when (< (tjit-max-inline-depth) acc)
           (break 2 "too many inlined procedures"))
         (+ acc (env-inline-depth env)))))))

(define* (take-snapshot ip dst-offset locals vars indices id sp-offset fp-offset
                        min-sp-offset max-sp-offset inline-depth env
                        #:optional (refill? #f) (arg-nlocals #f))
  (let* ((nlocals (or arg-nlocals (vector-length locals)))
         (dst-ip (+ ip (* dst-offset 4)))
         (test (if (and (env-uprec? env)
                        (not (env-parent-fragment env)))
                   (lambda (i)
                     (<= min-sp-offset i (- max-sp-offset 1)))
                   (lambda (i)
                     (<= min-sp-offset i))))
         (indices (filter test indices))
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
     (let* ((stack-size (vector-length locals))
            (sp-offset (current-sp-offset))
            (min-local-index (+ (- stack-size proc) sp-offset 1))
            (max-local-index (+ nlocals sp-offset 1))
            (live-indices (env-live-indices env))
            (load-up-frame
             (lambda ()
               (let lp ((vars (reverse (ir-vars ir))) (loaded '()))
                 (match vars
                   (((n . var) . vars)
                    (cond
                     ((or (skip-var? var)
                          (memq n live-indices)
                          (not (memq n (env-read-indices env))))
                      (lp vars loaded))
                     ((< min-local-index n max-local-index)
                      (let* ((t (assq-ref (env-entry-types env) n))
                             (t (if (eq? t &flonum)
                                    t
                                    #f)))
                        (with-stack-ref var t n lp vars (cons n loaded))))
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
                    (not (env-uprec? env))
                    (< 0 (current-fp-offset)))
           (break 1 "root trace with up-frame load"))
         (load-up-frame))))))

(define-syntax-rule (with-stack-ref var type idx next . args)
  (cond
   ((dynamic-link? type)
    `(let ((,var ,(dynamic-link-offset type)))
       ,(next . args)))
   ((return-address? type)
    `(let ((,var ,(return-address-ip type)))
       ,(next . args)))
   ((or (eq? type &flonum)
        (eq? type &f64))
    `(let ((,var (%sref/f ,idx ,type)))
       ,(next . args)))
   (else
    `(let ((,var (%sref ,idx ,type)))
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

(eval-when (compile load expand)
  (define (format-id k fmt . args)
    (datum->syntax k (string->symbol (apply format #f fmt args)))))

(define-syntax define-scan-infer-live
  (lambda (x)
    (syntax-case x ()
      ((k scan-name infer-name live-name (type ...))
       (with-syntax (((tag ...)
                      (map (lambda (t)
                             (format-id #'k "&~s" (syntax->datum t)))
                           #'(type ...)))
                     ((punct ...)
                      (map (lambda (t)
                             (format-id #'k "~s!" (syntax->datum t)))
                           #'(type ...))))
         #`(begin
             (define-syntax scan-helper
               (syntax-rules (type ...)
                 ((_ acc (type arg) . rest)
                  (scan-helper (`(,arg . ,tag) . acc) . rest))
                 ...
                 ((_ acc (other arg) . rest)
                  (scan-helper acc . rest))
                 ((_ acc)
                  (set-entry-types! env (list . acc)))))
             (define-syntax-rule (scan-name . expr)
               (scan-helper () . expr))
             (define-syntax infer-name
               (syntax-rules (punct ...)
                 ((_ (punct arg) . rest)
                  (set-inferred-type! env (+ arg (env-sp-offset env)) tag))
                 ...
                 ((_ . other)
                  (values))))
             (define-syntax live-name
               (syntax-rules (punct ...)
                 ((_ (punct arg) . rest)
                  (let ((live-indices (env-live-indices env))
                        (idx+sp-offset (+ arg (current-sp-offset))))
                    (unless (memq idx+sp-offset live-indices)
                      (let ((live-indices (cons idx+sp-offset live-indices)))
                        (set-env-live-indices! env live-indices)))))
                 ...
                 ((_ . other) (values))))))))))

(define-scan-infer-live gen-scan-types gen-infer-type gen-update-live-indices
  (scm fixnum flonum fraction char pair vector box procedure struct string
       bytevector u64 f64 s64))

(define-syntax define-ir
  (syntax-rules ()
    "Defines procedure to compile bytecode operation to IR, and optionally
defines procedure for scanning locals and types. E.g:

  (define-ir (add (fixnum! dst) (fixnum a) (fixnum b))
    ...)

will define three procedures: one for IR compilation taking three arguments,
another for type inference, and one more for scanning locals and types. The
procedure for scanner saves index values referenced by dst, a, and b at
runtime."
    ((_ (name (flag arg) ...) . body)
     (let ((test-proc
            (lambda (op locals)
              (let lp ((flags '(flag ...)) (ns (cdr op)))
                (match (cons flags ns)
                  (((f . flags) . (n . ns))
                   (case f
                     ((fixnum flonum fraction char procedure pair
                       vector box struct string bytevector array)
                      (let ((runtime-value (vector-ref locals n)))
                        (if (eq? (type-of runtime-value) (flag->type f))
                            (lp flags ns)
                            #f)))
                     (else
                      (lp flags ns))))
                  (_
                   #t)))))
           (scan-proc
            (lambda (%env %ip %dl %locals arg ...)
              (syntax-parameterize ((env (identifier-syntax %env)))
                (gen-scan-types (flag arg) ...))
              (set-scan-initial-fields! %env)
              #t))
           (ti-proc
            (lambda (%env %ip %dl %locals arg ...)
              (syntax-parameterize ((env (identifier-syntax %env))
                                    (ip (identifier-syntax %ip))
                                    (dl (identifier-syntax %dl))
                                    (locals (identifier-syntax %locals)))
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
                (debug 2 ";;; ~a~%" (cons 'name '(flag ...)))
                ;; Updating live indices, only for side traces. Live indices in
                ;; root traces are constantly same as write indices, loaded at
                ;; the time of entry.
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

(define-syntax-rule (current-sp-for-ti)
  ;; Type inference procedures are called during initialization and ANF IR
  ;; compilation. Some bytecode operation shift SP during env
  ;; initialization. This macro test whether env is initialized and get
  ;; current SP offset appropriately.
  (if (env-initialized? env)
      (env-sp-offset env)
      (car (env-sp-offsets env))))

(define-syntax-rule (current-fp-offset)
  (vector-ref (env-fp-offsets env) (ir-bytecode-index ir)))

;; `call' and `call-label' at last position are always inlined, no need to emit
;; FP shifting would be done with offsets stored in snapshot.
(define-syntax-rule (inline-current-call?)
  (or (assq-ref (env-calls env) (env-call-num env))
      (< 0 (env-inline-depth env))
      (ir-last-op? ir)))

(define-syntax-rule (inline-current-return?)
  (or (assq-ref (env-returns env) (env-return-num env))
      (< 0 (env-inline-depth env))))

(define-syntax-rule (scm-ref n)
  (vector-ref locals n))

(define-syntax-rule (u64-ref n)
  (object-address (vector-ref locals n)))

(define-syntax-rule (var-ref n)
  (assq-ref (ir-vars ir) (+ n (current-sp-offset))))

(define-syntax-rule (src-ref n)
  (let* ((sp-offset (current-sp-offset))
         (type (assq-ref (env-inferred-types env) (+ n sp-offset))))
    (if (constant? type)
        (constant-value type)
        (assq-ref (ir-vars ir) (+ n sp-offset)))))

(define-syntax-rule (dst-ref n)
  (assq-ref (ir-vars ir) (+ n (current-sp-offset))))

(define-syntax-rule (type-ref n)
  (assq-ref (env-inferred-types env) (+ n (current-sp-offset))))

(define-syntax take-snapshot!
  (syntax-rules ()
    ((_ ip dst-offset)
     (take-snapshot! ip dst-offset #f))
    ((_ ip dst-offset refill?)
     ;; Compare IP of cached snapshot. Reuse it if the IP was same with current
     ;; IP. Take a new snapshot and increment snapshot ID if not.
     (if (eq? ip (and=> (ir-cached-snapshot ir) snapshot-ip))
         '_
         (let-values (((ret snapshot)
                       (take-snapshot ip dst-offset locals (ir-vars ir)
                                      (if (env-parent-snapshot env)
                                          (vector-ref (env-write-buf env)
                                                      (ir-bytecode-index ir))
                                          (env-write-indices env))
                                      (ir-snapshot-id ir)
                                      (current-sp-offset)
                                      (current-fp-offset)
                                      (ir-min-sp-offset ir)
                                      (ir-max-sp-offset ir)
                                      (current-inline-depth env) env
                                      refill?)))
           (let ((old-id (ir-snapshot-id ir)))
             (hashq-set! (ir-snapshots ir) old-id snapshot)
             (set-ir-snapshot-id! ir (+ old-id 1))
             (set-ir-cached-snapshot! ir snapshot))
           ret)))))

(define-syntax-rule (with-boxing type var tmp proc)
  (cond
   ((eq? type &flonum)
    (set-env-handle-interrupts! env #t)
    `(let ((,tmp (%d2s ,var)))
       ,(proc tmp)))
   (else
    (proc var))))

(define-syntax-rule (with-type-guard type src expr)
  (if (let ((src/t (type-ref src)))
        (or (eq? type src/t)
            (constant? src/t)
            (eq? type (applied-guard env (+ src (current-sp-offset))))))
      expr
      (begin
        (set-applied-guard! env (+ src (current-sp-offset)) type)
        `(let ((_ ,(take-snapshot! ip 0)))
           (let ((_ (%typeq ,(var-ref src) ,type)))
             ,expr)))))

(define-syntax-rule (with-type-guard-always type src expr)
  `(let ((_ ,(take-snapshot! ip 0)))
     (let ((_ (%typeq ,(var-ref src) ,type)))
       ,expr)))


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


;;;
;;; Macro for defining constant
;;;

(define-syntax define-constant
  (syntax-rules ()
    ((_ (name . args) expr)
     (begin
       (define-scan (name dst . args)
         (let ((sp-offset (env-sp-offset env)))
           (set-entry-type! env (+ dst sp-offset) &any)
           (set-scan-initial-fields! env)))
       (define-ti (name dst . args)
         (let ((dst/i+sp (+ dst (env-sp-offset env))))
           (set-inferred-type! env dst/i+sp (make-constant expr))))
       (define-anf (name dst . args)
         (let ((dst/i+sp (+ dst (current-sp-offset)))
               (live-indices (env-live-indices env)))
           (unless (memq dst/i+sp live-indices)
             (set-env-live-indices! env (cons dst/i+sp live-indices)))
           (next)))))))
