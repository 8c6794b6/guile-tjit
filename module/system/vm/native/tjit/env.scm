;;;; Environment data for tracing JIT compilation

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
;;; This module contains record type for environment data, used to hold globally
;;; accessible values during single JIT compilation work.  Environment stores
;;; various information used for compiling, such as parent snapshot, types,
;;; locals ... etc.
;;;
;;; Code:

(define-module (system vm native tjit env)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit types)
  #:autoload (system vm native tjit snapshot) (snapshot-inline-depth)
  #:export ($env
            make-env
            env-id
            env-entry-ip
            env-linked-ip
            env-parent-exit-id
            env-parent-fragment
            env-parent-snapshot
            env-loop?
            env-downrec?
            env-uprec?
            env-linking-roots?
            env-handle-interrupts? set-env-handle-interrupts!
            env-loop-locals set-env-loop-locals!
            env-loop-vars set-env-loop-vars!
            env-initialized?
            env-sp-offsets set-env-sp-offsets!
            env-fp-offsets set-env-fp-offsets!
            env-sp-offset set-env-sp-offset!
            env-fp-offset set-env-fp-offset!
            env-last-sp-offset
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

            arrange-env!
            increment-env-call-return-num!
            add-env-call!
            add-env-return!
            env-local-indices
            env-current-inline-depth
            expand-env
            set-entry-type!
            set-inferred-type!))


;; Data type to contain environment.
;;
(define-record-type $env
  (%make-env id entry-ip linked-ip
             parent-exit-id parent-fragment parent-snapshot
             loop? downrec? uprec? linking-roots?
             handle-interrupts? initialized?
             loop-locals loop-vars
             sp-offsets fp-offsets sp-offset fp-offset last-sp-offset
             write-indices read-indices write-buf live-indices
             entry-types inferred-types
             call-num return-num calls returns inline-depth)
  env?

  ;; Trace ID of this compilation.
  (id env-id)

  ;; Entry IP of this trace.
  (entry-ip env-entry-ip)

  ;; Linked IP, if any.
  (linked-ip env-linked-ip)

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

  ;; Flag to tell whether the parent trace origin is different from linked
  ;; trace.
  (linking-roots? env-linking-roots?)

  ;; Flag to emit interrupt handler.
  (handle-interrupts? env-handle-interrupts? set-env-handle-interrupts!)

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
  (inline-depth env-inline-depth set-env-inline-depth!))

(define (make-env id entry-ip linked-ip
                  parent-exit-id parent-fragment parent-snapshot
                  loop? downrec? uprec? linking-roots?
                  sp-offset fp-offset write-indices live-indices
                  types-from-parent inline-depth)
  (debug 2 ";;; [make-env] inline-depth=~a~%" inline-depth)
  (%make-env id entry-ip linked-ip
             parent-exit-id parent-fragment parent-snapshot
             loop? downrec? uprec? linking-roots?
             #f #f #f #f '() '() sp-offset fp-offset 0
             write-indices '() (list write-indices) live-indices
             '() (copy-tree types-from-parent)
             0 0 '() '() inline-depth))

(define (env-local-indices env)
  (sort (delete-duplicates (append (env-write-indices env)
                                   (env-read-indices env)))
        >))

(define (env-current-inline-depth env)
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
        (() (+ acc (env-inline-depth env)))))))

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
    (else
     (values))))

(define (add-env-call! env)
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

(define (arrange-env! env)
  (define (resolve-copies dsts srcs)
    (let ((copies (let lp ((dsts dsts) (acc '()))
                    (match dsts
                      (((dst 'copy . src) . dsts)
                       (lp dsts (cons (cons dst src) acc)))
                      ((_ . dsts)
                       (lp dsts acc))
                      (()
                       acc)))))
      (let lp ((copies copies) (dsts dsts))
        (match copies
          (((dst . src) . copies)
           (lp copies (assq-set! dsts dst (assq-ref srcs src))))
          (_
           dsts)))))
  (define (set-reversed-vector! setter getter)
    (setter env (list->vector (reverse! (getter env)))))
  (set-env-last-sp-offset! env (env-sp-offset env))
  (set-env-call-num! env 0)
  (set-env-return-num! env 0)
  (let ((depth (or (and=> (env-parent-snapshot env) snapshot-inline-depth)
                   0)))
    (set-env-inline-depth! env depth))
  (set-reversed-vector! set-env-sp-offsets! env-sp-offsets)
  (set-reversed-vector! set-env-fp-offsets! env-fp-offsets)
  (set-reversed-vector! set-env-write-buf! env-write-buf)
  (let ((entry (env-entry-types env))
        (inferred (env-inferred-types env)))
    (set-env-entry-types! env (resolve-copies entry entry))
    (set-env-inferred-types! env (resolve-copies inferred entry))
    (set-env-read-indices! env (sort (map car entry) <))
    (set-env-write-indices! env (sort (map car inferred) <)))
  (set-env-initialized! env #t))

(define (expand-env env offset nlocals)
  (let lp ((n 0) (acc (env-live-indices env)))
    (if (< n nlocals)
        (lp (+ n 1) (if (memq n acc)
                        acc
                        (cons n acc)))
        (set-env-live-indices! env (sort acc <)))))

(define (set-entry-type! env n t)
  (let* ((inferred (env-inferred-types env))
         (entry (env-entry-types env))
         (entry (if (and (not (assq-ref entry n))
                         (not (assq-ref inferred n)))
                    (assq-set! entry n t)
                    entry))
         (entry (let lp ((current entry) (acc entry))
                  (match current
                    (((i 'copy . (? (lambda (x) (= x n)))) . current)
                     (lp current (assq-set! acc i t)))
                    ((_ . current)
                     (lp current acc))
                    (() acc)))))
    (set-env-entry-types! env entry)))

(define (set-inferred-type! env n t)
  (let* ((inferred (env-inferred-types env))
         (inferred (assq-set! inferred n t)))
    (set-env-inferred-types! env inferred)))
