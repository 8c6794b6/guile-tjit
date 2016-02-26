;;;; Environment data during tracing JIT compilation

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

            arrange-env!
            env-local-indices
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
                 entry-types inferred-types)
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
  (inferred-types env-inferred-types set-env-inferred-types!))

(define (make-env id entry-ip linked-ip
                      parent-exit-id parent-fragment parent-snapshot
                      loop? downrec? uprec? linking-roots?
                      sp-offset fp-offset write-indices live-indices
                      types-from-parent)
  ;; Using hash-table to contain locals, since local index could take negative
  ;; value.
  (%make-env id entry-ip linked-ip
                 parent-exit-id parent-fragment parent-snapshot
                 loop? downrec? uprec? linking-roots?
                 #f #f #f #f '() '() sp-offset fp-offset 0
                 write-indices '() (list write-indices) live-indices
                 '() (copy-tree types-from-parent)))

(define (env-local-indices env)
  (sort (delete-duplicates (append (env-write-indices env)
                                   (env-read-indices env)))
        >))

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
  (let* ((sp-offsets/vec (list->vector (reverse! (env-sp-offsets env))))
         (fp-offsets/vec (list->vector (reverse! (env-fp-offsets env))))
         (writes/list (sort (env-write-indices env) <))
         (write-buf/vec (list->vector (reverse! (env-write-buf env)))))
    (set-env-sp-offsets! env sp-offsets/vec)
    (set-env-fp-offsets! env fp-offsets/vec)
    (set-env-last-sp-offset! env (env-sp-offset env))
    (set-env-write-buf! env write-buf/vec)
    (let ((entry (env-entry-types env))
          (inferred (env-inferred-types env)))
      (set-env-entry-types! env (resolve-copies entry entry))
      (set-env-inferred-types! env (resolve-copies inferred entry))
      (set-env-read-indices! env (sort (map car entry) <))
      (set-env-write-indices! env (sort (map car inferred) <)))
    (set-env-initialized! env #t)))

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
    (debug 2 ";;; [set-entry-type!] ~s => ~a~%" n (pretty-type t))
    (set-env-entry-types! env entry)))

(define (set-inferred-type! env n t)
  (let* ((inferred (env-inferred-types env))
         (inferred (assq-set! inferred n t)))
    (debug 2 ";;; [set-inferred-type!] ~s => ~a~%" n (pretty-type t))
    (set-env-inferred-types! env inferred)))
