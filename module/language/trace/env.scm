;;;; Environment data for tracing JIT compilation

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
;;; This module contains record type for environment data, used to hold globally
;;; accessible values during single JIT compilation work.  Environment stores
;;; various information used for compiling, such as parent snapshot, types,
;;; locals ... etc.
;;;
;;; Code:

(define-module (language trace env)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (delete-duplicates))
  #:use-module (srfi srfi-9)
  #:use-module (system vm native debug)
  #:use-module (language trace types)
  #:use-module (language trace parameters)
  #:export (make-env
            env-id
            env-entry-ip
            env-linked-ip
            env-linked-fragment set-env-linked-fragment!
            env-parent-exit-id
            env-parent-fragment
            env-parent-snapshot
            env-loop?
            env-downrec?
            env-uprec?
            env-handle-interrupts? set-env-handle-interrupts!
            env-loop-locals set-env-loop-locals!
            env-loop-vars set-env-loop-vars!
            env-initialized? set-env-initialized!
            env-sp-offsets set-env-sp-offsets!
            env-fp-offsets set-env-fp-offsets!
            env-sp-offset set-env-sp-offset!
            env-fp-offset set-env-fp-offset!
            env-last-sp-offset set-env-last-sp-offset!
            env-write-indices set-env-write-indices!
            env-read-indices set-env-read-indices!
            env-write-buf set-env-write-buf!
            env-live-indices set-env-live-indices!
            env-entry-types set-env-entry-types!
            env-inferred-types set-env-inferred-types!
            env-call-num set-env-call-num!
            env-return-num set-env-return-num!
            env-calls set-env-calls!
            env-returns set-env-returns!
            env-inline-depth set-env-inline-depth!
            env-save-volatiles? set-env-save-volatiles!
            env-applied-guards
            env-min-sp-offset set-env-min-sp-offset!
            env-bytecode-index set-env-bytecode-index!
            env-ir-procedures set-env-ir-procedures!
            env-dynamic-links set-env-dynamic-links!
            env                         ; syntax parameter.

            increment-env-call-return-num!
            add-env-call!
            add-env-return!
            env-local-indices
            set-entry-type!
            set-entry-types!
            set-inferred-type!
            applied-guard
            set-applied-guard!

            current-ir-procedure
            current-sp-offset
            current-sp-for-ti
            current-fp-offset
            current-inline-depth

            push-scan-sp-offset! pop-scan-sp-offset!
            push-scan-fp-offset! pop-scan-fp-offset!
            set-scan-initial-fields!))


;;;; The environment

;; Data type to contain environment.
(define-record-type <env>
  (%make-env id entry-ip linked-ip linked-fragment
             parent-exit-id parent-fragment parent-snapshot
             loop? downrec? uprec?
             handle-interrupts? save-volatiles? initialized?
             loop-locals loop-vars
             sp-offsets fp-offsets sp-offset fp-offset last-sp-offset
             write-indices read-indices write-buf live-indices
             entry-types inferred-types
             call-num return-num calls returns inline-depth
             applied-guards min-sp-offset bytecode-index ir-procedures
             dynamic-links)
  env?

  ;; Trace ID of this compilation.
  (id env-id)

  ;; Entry IP of this trace.
  (entry-ip env-entry-ip)

  ;; Linked IP, if any.
  (linked-ip env-linked-ip)

  ;; Linked fragment.
  (linked-fragment env-linked-fragment set-env-linked-fragment!)

  ;; Parent exit ID of this trace.
  (parent-exit-id env-parent-exit-id)

  ;; Parent fragment of this trace.
  (parent-fragment env-parent-fragment)

  ;; Parent snapshot, snapshot of parent-exit-id in parent fragment.
  (parent-snapshot env-parent-snapshot)

  ;; Flag for loop trace.
  (loop? env-loop?)

  ;; Flag for down recursion trace.
  (downrec? env-downrec?)

  ;; Flag for up recursion trace.
  (uprec? env-uprec?)

  ;; Flag to emit interrupt handler.
  (handle-interrupts? env-handle-interrupts? set-env-handle-interrupts!)

  ;; Flag to save volatile registers at entry.
  (save-volatiles? env-save-volatiles? set-env-save-volatiles!)

  ;; Flag to hold whether initialized.
  (initialized? env-initialized? set-env-initialized!)

  ;; Loop locals for root trace.
  (loop-locals env-loop-locals set-env-loop-locals!)

  ;; Loop vars for root trace.
  (loop-vars env-loop-vars set-env-loop-vars!)

  ;; Vector containing SP offset per bytecode operation.
  (sp-offsets env-sp-offsets set-env-sp-offsets!)

  ;; Vector containing FP offset per bytecode operation.
  (fp-offsets env-fp-offsets set-env-fp-offsets!)

  ;; Current SP offset.
  (sp-offset env-sp-offset set-env-sp-offset!)

  ;; Current FP offset.
  (fp-offset env-fp-offset set-env-fp-offset!)

  ;; Last SP offset.
  ;;
  ;; Note that the last SP offset may differ from the SP offset coupled with
  ;; last recorded bytecode operation, because some bytecode operations modify
  ;; SP offset after saving the SP offset.
  ;;
  (last-sp-offset env-last-sp-offset set-env-last-sp-offset!)

  ;; Local indices for write.
  (write-indices env-write-indices set-env-write-indices!)

  ;; Local indices for read.
  (read-indices env-read-indices set-env-read-indices!)

  ;; Buffer to hold write indices.
  (write-buf env-write-buf set-env-write-buf!)

  ;; Live indices, updated during IR compilation.
  (live-indices env-live-indices set-env-live-indices!)

  ;; Entry types.
  (entry-types env-entry-types set-env-entry-types!)

  ;; Inferred types.
  (inferred-types env-inferred-types set-env-inferred-types!)

  ;; Current call number.
  (call-num env-call-num set-env-call-num!)

  ;; Current return number.
  (return-num env-return-num set-env-return-num!)

  ;; Inline flags for calls.
  (calls env-calls set-env-calls!)

  ;; Inline flags for returns.
  (returns env-returns set-env-returns!)

  ;; Depth of calls and returns.
  (inline-depth env-inline-depth set-env-inline-depth!)

  ;; Applied guards.
  (applied-guards env-applied-guards)

  ;; Minimum SP offset.
  (min-sp-offset env-min-sp-offset set-env-min-sp-offset!)

  ;; Current bytecode index.
  (bytecode-index env-bytecode-index set-env-bytecode-index!)

  ;; Possibly type specified IR procedures per bytecode instruction.
  (ir-procedures env-ir-procedures set-env-ir-procedures!)

  ;; List of dynamic links.
  (dynamic-links env-dynamic-links set-env-dynamic-links!))

(define (make-env id entry-ip linked-ip
                  parent-exit-id parent-fragment parent-snapshot
                  loop? downrec? uprec?
                  sp-offset fp-offset write-indices live-indices
                  types-from-parent inline-depth)
  (let ((linked-fragment #f)
        (handle-interrupts? #f)
        (save-volatiles? #f)
        (initialized? #f)
        (loop-locals #f)
        (loop-vars #f)
        (sp-offsets '())
        (fp-offsets '())
        (last-sp-offset 0)
        (read-indices '())
        (write-buf (list write-indices))
        (entry-types '())
        (inferred-types (copy-tree types-from-parent))
        (call-num 0)
        (return-num 0)
        (calls '())
        (returns '())
        (applied-guards (make-hash-table))
        (min-sp-offset 0)
        (bytecode-index 0)
        (ir-procedures '())
        (dynamic-links '()))
    (%make-env id entry-ip linked-ip linked-fragment
               parent-exit-id parent-fragment parent-snapshot
               loop? downrec? uprec?
               handle-interrupts? save-volatiles? initialized?
               loop-locals loop-vars sp-offsets fp-offsets
               sp-offset fp-offset last-sp-offset
               write-indices read-indices write-buf live-indices
               entry-types inferred-types
               call-num return-num calls returns
               inline-depth applied-guards min-sp-offset
               bytecode-index ir-procedures dynamic-links)))

(define-syntax-parameter env
  (lambda (x) 'env "uninitialized" x))


;;;; Macros for parse

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


;;;; For IR compilation

(define-inlinable (env-local-indices env)
  (sort (delete-duplicates (append (env-write-indices env)
                                   (env-read-indices env)))
        >))

(define (increment-env-call-return-num! env op)
  "Increment call/return number in ENV if OP was call or return.

For return operations, if the current return had no matching call but was
continued as inlined call in parent trace, this procedure will lower the current
inline depth by one."
  (case (car op)
    ((call call-label)
     (set-env-call-num! env (1+ (env-call-num env))))
    ((return-values subr-call foreign-call)
     (let ((return-num (env-return-num env)))
       (when (and=> (assq-ref (env-returns env) return-num)
                    (lambda (paired) (< paired 0)))
         (set-env-inline-depth! env (- (env-inline-depth env) 1)))
       (set-env-return-num! env (1+ return-num))))
    (else (values))))

(define-inlinable (add-env-call! env)
  (let ((new (cons (env-call-num env) #f)))
    (set-env-calls! env (cons new (env-calls env)))))

(define (add-env-return! env)
  (define (add! new)
    (set-env-returns! env (cons new (env-returns env))))
  (let* ((calls (env-calls env))
         (return-num (env-return-num env))
         (last-opened-call (let lp ((calls calls))
                             (match calls
                               (((call-num . #f) . calls)
                                call-num)
                               (((call-num . ret-num) . calls)
                                (lp calls))
                               (() #f))))
         (new (cons return-num last-opened-call)))
    (cond
     (last-opened-call
      (let ((calls (assq-set! calls last-opened-call return-num)))
        (set-env-calls! env calls)
        (add! (cons return-num last-opened-call))))
     ((< 0 (env-inline-depth env))
      ;; Mark as inlined return with no matching call in recorded bytecode.
      ;; This happens when parent trace contain inlined call, and current trace
      ;; was a side trace derived from side exit in the inlined call.
      (add! (cons return-num -1)))
     (else
      (add! (cons return-num #f))))))

(define (set-entry-type! env n t)
  (let* ((inferred (env-inferred-types env))
         (entry (env-entry-types env))
         (entry (if (and (not (assq-ref entry n))
                         (not (assq-ref inferred n)))
                    (assq-set! entry n t)
                    entry))
         (n? (lambda (x) (= x n)))
         (entry (if (eq? t &any)
                    entry
                    (let lp ((current entry) (acc entry))
                      ;; Unless the given type was `&any', update the
                      ;; copied type. If copy was marked as `&any',
                      ;; initial stack load might skip necessary locals.
                      (match current
                        (((i 'copy . (? n?)) . current)
                         (lp current (assq-set! acc i t)))
                        ((_ . current)
                         (lp current acc))
                        (() acc))))))
    (set-env-entry-types! env entry)))

;;; Variant ot `set-entry-type!' for batch update.
(define (set-entry-types! env types)
  (let* ((sp-offset (env-sp-offset env))
         (inferred (env-inferred-types env))
         (found? (lambda (i)
                   (assq-ref (- i sp-offset) types)))
         (entry (let lp ((types types) (entry (env-entry-types env)))
                  (match types
                    (((n . t) . types)
                     (let ((i (+ n sp-offset)))
                       (if (and (not (assq-ref entry i))
                                (not (assq-ref inferred i)))
                           (lp types (assq-set! entry i t))
                           (lp types entry))))
                    (()
                     entry))))
         (entry (let lp ((current entry) (acc entry))
                  (match current
                    (((i 'copy . (? found? t)) . current)
                     ;; Test type with `&any', as in `set-entry-type!'.
                     (lp current (if (eq? t &any)
                                     acc
                                     (assq-set! acc i t))))
                    ((_ . current)
                     (lp current acc))
                    (_ acc)))))
    (set-env-entry-types! env entry)))

(define-inlinable (set-inferred-type! env n t)
  (let* ((inferred (env-inferred-types env))
         (inferred (assq-set! inferred n t)))
    (hashq-remove! (env-applied-guards env) n)
    (set-env-inferred-types! env inferred)))

(define-inlinable (applied-guard env n)
  (hashq-ref (env-applied-guards env) n))

(define-inlinable (set-applied-guard! env n type)
  (hashq-set! (env-applied-guards env) n type))

(define-inlinable (current-ir-procedure env)
  (vector-ref (env-ir-procedures env) (env-bytecode-index env)))


;;;; Definitions using syntax parameter

(define-syntax-rule (current-inline-depth)
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
           ((@ (language trace error) break) 2
            "too many inlined procedures"))
         (+ acc (env-inline-depth env)))))))

(define-syntax-rule (current-sp-offset)
  (vector-ref (env-sp-offsets env) (env-bytecode-index env)))

(define-syntax-rule (current-sp-for-ti)
  ;; Type inference procedures are called during initialization and ANF IR
  ;; compilation. Some bytecode operation shift SP during env
  ;; initialization. This macro test whether env is initialized and get current
  ;; SP offset appropriately.
  (if (env-initialized? env)
      (env-sp-offset env)
      (car (env-sp-offsets env))))

(define-syntax-rule (current-fp-offset)
  (vector-ref (env-fp-offsets env) (env-bytecode-index env)))
