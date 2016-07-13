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
            ir-vars
            ir-last-op? set-ir-last-op!

            ir-procedure-check-type
            ir-procedure-parse
            ir-procedure-infer-type
            ir-procedure-anf

            ir ip ra dl locals next ; syntax parameters
            gen-load-thunk dereference-scm
            take-snapshot take-snapshot!
            scm-ref u64-ref var-ref src-ref dst-ref type-ref
            with-stack-ref with-boxing
            with-type-guard with-type-guard-always
            inline-current-call? inline-current-return?

            *ir-procedures*
            define-ir define-interrupt-ir
            define-scan define-ti define-anf
            define-constant

            parse-trace
            infer-type))


;;;; IR record type

(define-record-type <ir>
  (make-ir snapshots snapshot-id vars min-sp-offset max-sp-offset
           last-op? cached-snapshot)
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

  ;; Flag for last recorded operation.
  (last-op? ir-last-op? set-ir-last-op!)

  ;; Cached snapshot.
  (cached-snapshot ir-cached-snapshot set-ir-cached-snapshot!))

(define-record-type <ir-procedure>
  (make-ir-procedure check-type parse infer-type anf)
  ir-procedure?
  (check-type ir-procedure-check-type)
  (parse ir-procedure-parse set-ir-procedure-parse!)
  (infer-type ir-procedure-infer-type set-ir-procedure-infer-type!)
  (anf ir-procedure-anf set-ir-procedure-anf!))


;;;; For IR body

(define* (take-snapshot ip dst-offset locals vars indices id
                        sp-offset fp-offset
                        min-sp-offset max-sp-offset
                        inline-depth env
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
                        (append! acc (list (make-var (+ sp-offset nlocals))
                                           (make-var (+ sp-offset nlocals 1))))
                        acc)))))
         (snapshot (make-snapshot id sp-offset fp-offset nlocals indices
                                  env dst-ip inline-depth refill?)))
    (values `(%snap ,id ,@args) snapshot)))

(define-syntax define-ir-syntax-parameters
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define-syntax-parameter name
         (lambda (x)
           'name "uninitialized" x))
       ...))))

(define-ir-syntax-parameters ir ip ra dl locals next)

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
                     (else (lp vars loaded))))
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

(define-syntax-rule (dereference-scm addr)
  (pointer->scm (dereference-pointer (make-pointer addr))))

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
                                          (vector-ref
                                           (env-write-buf env)
                                           (env-bytecode-index env))
                                          (env-write-indices env))
                                      (ir-snapshot-id ir)
                                      (current-sp-offset)
                                      (current-fp-offset)
                                      (ir-min-sp-offset ir)
                                      (ir-max-sp-offset ir)
                                      (current-inline-depth) env
                                      refill? #f)))
           (let ((old-id (ir-snapshot-id ir)))
             (snapshots-set! (ir-snapshots ir) old-id snapshot)
             (set-ir-snapshot-id! ir (+ old-id 1))
             (set-ir-cached-snapshot! ir snapshot))
           ret)))))

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

(define-syntax-rule (with-boxing type var tmp proc)
  (if (eq? type &flonum)
      (begin
        (set-env-handle-interrupts! env #t)
        `(let ((,tmp (%d2s ,var)))
           ,(proc tmp)))
      (proc var)))

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


;;;; Macros for defining IR

(eval-when (compile load expand)
  (define (format-id k fmt . args)
    (datum->syntax k (string->symbol
                      (apply format #f fmt (map syntax->datum args))))))

(define-syntax define-scan-infer-live
  (lambda (x)
    (syntax-case x ()
      ((k scan-name infer-name live-name (type ...))
       (with-syntax (((tag ...) (map (lambda (t)
                                       (format-id #'k "&~s" t))
                                     #'(type ...)))
                     ((punct ...) (map (lambda (t)
                                         (format-id #'k "~s!" t))
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

(define-syntax define-type-checker
  (lambda (x)
    (syntax-case x ()
      ((k name (type ...))
       (with-syntax (((&type ...) (map (lambda (s)
                                         (format-id #'k "&~s" s))
                                       #'(type ...))))
         #`(define-syntax name
             (syntax-rules (type ...)
               ((_ locals n type)
                (eq? &type (type-of (vector-ref locals n))))
               ...
               ((_ _ locals n) #t))))))))

(define-type-checker type-check-one
  (fixnum
   flonum fraction char procedure pair vector box
   struct string bytevector array))

(define-syntax type-check
  (syntax-rules ()
    ((_ locals _ ()) #t)
    ((_ locals ns (flag . flags))
     (and (type-check-one locals (car ns) flag)
          (let ((ns* (cdr ns)))
            (type-check locals ns* flags))))))

(define-syntax-rule (update-ir name setter! proc new)
  (match (ir-procedures-ref 'name)
    ((ir-procedure)
     (setter! ir-procedure proc))
    (_
     (ir-procedures-set! 'name (list new)))))

(define no-test (lambda (op locals) #t))

(define *ir-procedures* (make-hash-table))

(define-inlinable (ir-procedures-ref name)
  (hashq-ref *ir-procedures* name))

(define-inlinable (ir-procedures-set! name val)
  (hashq-set! *ir-procedures* name val))

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
              (type-check locals (cdr op) (flag ...))))
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
                ;; Updating live indices, only for side traces. Live indices in
                ;; root traces are constantly same as write indices, loaded at
                ;; the time of entry.
                (when (env-parent-snapshot env)
                  (gen-update-live-indices (flag arg) ...))
                . body))))

       (let* ((ir-procedure (make-ir-procedure test-proc scan-proc
                                               ti-proc anf-proc))
              (entry (ir-procedures-ref 'name))
              (entry (if entry
                         (cons ir-procedure entry)
                         (list ir-procedure))))
         (ir-procedures-set! 'name entry))))))

(define-syntax define-interrupt-ir
  (syntax-rules ()
    ((_ names-and-args . body)
     (define-ir names-and-args
       (begin
         (set-env-handle-interrupts! env #t)
         . body)))))

(define-syntax-rule (define-anf (name arg ...) . body)
  (let ((anf-proc (lambda (%env %ir %next %ip %ra %dl %locals arg ...)
                    (syntax-parameterize
                        ((env (identifier-syntax %env))
                         (ir (identifier-syntax %ir))
                         (next (identifier-syntax %next))
                         (ip (identifier-syntax %ip))
                         (ra (identifier-syntax %ra))
                         (dl (identifier-syntax %dl))
                         (locals (identifier-syntax %locals)))
                      . body))))
    (update-ir name set-ir-procedure-anf! anf-proc
               (make-ir-procedure no-test #f #f anf-proc))))

(define-syntax-rule (define-scan (name args ...) . body)
  (let ((scan-proc (lambda (%env %ip %dl %locals args ...)
                     (syntax-parameterize
                         ((ip (identifier-syntax %ip))
                          (dl (identifier-syntax %dl))
                          (locals (identifier-syntax %locals))
                          (env (identifier-syntax %env)))
                       . body)
                     #t)))
    (update-ir name set-ir-procedure-parse! scan-proc
               (make-ir-procedure no-test scan-proc #f #f))))

(define-syntax-rule (define-ti (name args ...) . body)
  (let ((ti-proc (lambda (%env %ip %dl %locals args ...)
                   (syntax-parameterize
                       ((ip (identifier-syntax %ip))
                        (dl (identifier-syntax %dl))
                        (locals (identifier-syntax %locals))
                        (env (identifier-syntax %env)))
                     . body))))
    (update-ir name set-ir-procedure-infer-type! ti-proc
               (make-ir-procedure no-test #f ti-proc #f))))

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


;;;; Procedures with IR lookup

(define-inlinable (parse-trace env op ip dl locals)
  ;; Compute local indices and stack element types in op.
  ;;
  ;; The stack used by VM interpreter grows down. Lower frame data is
  ;; saved at the time of accumulation.  If one of the guard operation
  ;; appeared soon after bytecode sequence `return' or `receive',
  ;; snapshot does not know the value of locals in lower frame. When
  ;; recorded bytecode contains `return' before `call', snapshot will
  ;; recover a frame higher than the one used to enter the native
  ;; call.
  ;;
  (let lp ((procs (ir-procedures-ref (car op))))
    (define (%nyi op)
      (let ((verbosity (lightning-verbosity)))
        (if (and (number? verbosity) (<= 1 verbosity))
            (begin
              (debug 1 "NYI: ~a~%" (car op))
              #f)
            (nyi "~a" op))))
    (match procs
      ((proc . procs)
       (if ((ir-procedure-check-type proc) op locals)
           (let* ((args (cdr op))
                  (ret (apply (ir-procedure-parse proc)
                              env ip dl locals args)))
             (apply (ir-procedure-infer-type proc) env ip dl locals args)
             (set-env-ir-procedures! env (cons proc (env-ir-procedures env)))
             ret)
           (lp procs)))
      (_ (%nyi op)))))

(define-inlinable (infer-type env op ip dl locals)
  (let ((proc (current-ir-procedure env)))
    (apply (ir-procedure-infer-type proc) env ip dl locals (cdr op))))
