;;;; Outline data for recorded trace

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
;;; This module contains data type for outline, created from recorded
;;; information during trace.
;;;
;;; Code:

(define-module (system vm native tjit outline)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit types)
  #:export ($outline
            make-outline
            outline-id
            outline-entry-ip
            outline-linked-ip
            outline-parent-exit-id
            outline-parent-fragment
            outline-parent-snapshot
            outline-loop?
            outline-downrec?
            outline-uprec?
            outline-linking-roots?
            outline-handle-interrupts? set-outline-handle-interrupts!
            outline-loop-locals set-outline-loop-locals!
            outline-loop-vars set-outline-loop-vars!
            outline-initialized?
            outline-sp-offsets set-outline-sp-offsets!
            outline-fp-offsets set-outline-fp-offsets!
            outline-sp-offset set-outline-sp-offset!
            outline-fp-offset set-outline-fp-offset!
            outline-last-sp-offset
            outline-write-indices set-outline-write-indices!
            outline-read-indices set-outline-read-indices!
            outline-write-buf set-outline-write-buf!
            outline-live-indices set-outline-live-indices!
            outline-entry-types set-outline-entry-types!
            outline-inferred-types set-outline-inferred-types!

            arrange-outline!
            outline-local-indices
            expand-outline
            set-entry-type!
            set-inferred-type!))


;; Data type to contain outline of recorded traces.
;;
;; Stores various information used for compiling IR, such as dynamic links and
;; return addresses, past frame locals, locals of caller procedure in inlined
;; procedure ... etc.
(define-record-type $outline
  (%make-outline id entry-ip linked-ip
                 parent-exit-id parent-fragment parent-snapshot
                 loop? downrec? uprec? linking-roots?
                 handle-interrupts? initialized?
                 loop-locals loop-vars
                 sp-offsets fp-offsets sp-offset fp-offset last-sp-offset
                 write-indices read-indices write-buf live-indices
                 entry-types inferred-types)
  outline?

  ;; Trace ID of this compilation.
  (id outline-id)

  ;; Entry IP of this trace.
  (entry-ip outline-entry-ip)

  ;; Linked IP, if any.
  (linked-ip outline-linked-ip)

  ;; Parent exit ID of this trace.
  (parent-exit-id outline-parent-exit-id)

  ;; Parent fragment of this trace.
  (parent-fragment outline-parent-fragment)

  ;; Parent snapshot, snapshot of parent-exit-id in parent fragment.
  (parent-snapshot outline-parent-snapshot)

  ;; Flag for loop trace.
  (loop? outline-loop?)

  ;; Flag for down recursion trace.
  (downrec? outline-downrec?)

  ;; Flag for up recursion trace.
  (uprec? outline-uprec?)

  ;; Flag to tell whether the parent trace origin is different from linked
  ;; trace.
  (linking-roots? outline-linking-roots?)

  ;; Flag to emit interrupt handler.
  (handle-interrupts? outline-handle-interrupts? set-outline-handle-interrupts!)

  ;; Flag to hold whether initialized.
  (initialized? outline-initialized? set-outline-initialized!)

  ;; Loop locals for root trace.
  (loop-locals outline-loop-locals set-outline-loop-locals!)

  ;; Loop vars for root trace.
  (loop-vars outline-loop-vars set-outline-loop-vars!)

  ;; Vector containing SP offset per bytecode operation.
  (sp-offsets outline-sp-offsets set-outline-sp-offsets!)

  ;; Vector containing FP offset per bytecode operation.
  (fp-offsets outline-fp-offsets set-outline-fp-offsets!)

  ;; Current SP offset.
  (sp-offset outline-sp-offset set-outline-sp-offset!)

  ;; Current FP offset.
  (fp-offset outline-fp-offset set-outline-fp-offset!)

  ;; Last SP offset.
  ;;
  ;; Note that the last SP offset may differ from the SP offset coupled with
  ;; last recorded bytecode operation, because some bytecode operations modify
  ;; SP offset after saving the SP offset.
  ;;
  (last-sp-offset outline-last-sp-offset set-outline-last-sp-offset!)

  ;; Local indices for write.
  (write-indices outline-write-indices set-outline-write-indices!)

  ;; Local indices for read.
  (read-indices outline-read-indices set-outline-read-indices!)

  ;; Buffer to hold write indices.
  (write-buf outline-write-buf set-outline-write-buf!)

  ;; Live indices, updated during IR compilation.
  (live-indices outline-live-indices set-outline-live-indices!)

  ;; Entry types.
  (entry-types outline-entry-types set-outline-entry-types!)

  ;; Inferred types.
  (inferred-types outline-inferred-types set-outline-inferred-types!))

(define (make-outline id entry-ip linked-ip
                      parent-exit-id parent-fragment parent-snapshot
                      loop? downrec? uprec? linking-roots?
                      sp-offset fp-offset write-indices live-indices
                      types-from-parent)
  ;; Using hash-table to contain locals, since local index could take negative
  ;; value.
  (%make-outline id entry-ip linked-ip
                 parent-exit-id parent-fragment parent-snapshot
                 loop? downrec? uprec? linking-roots?
                 #f #f #f #f '() '() sp-offset fp-offset 0
                 write-indices '() (list write-indices) live-indices
                 '() (copy-tree types-from-parent)))

(define (outline-local-indices outline)
  (sort (delete-duplicates (append (outline-write-indices outline)
                                   (outline-read-indices outline)))
        >))

(define (arrange-outline! outline)
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
  (let* ((sp-offsets/vec (list->vector (reverse! (outline-sp-offsets outline))))
         (fp-offsets/vec (list->vector (reverse! (outline-fp-offsets outline))))
         (writes/list (sort (outline-write-indices outline) <))
         (write-buf/vec (list->vector (reverse! (outline-write-buf outline)))))
    (set-outline-sp-offsets! outline sp-offsets/vec)
    (set-outline-fp-offsets! outline fp-offsets/vec)
    (set-outline-last-sp-offset! outline (outline-sp-offset outline))
    (set-outline-write-buf! outline write-buf/vec)
    (let ((entry (outline-entry-types outline))
          (inferred (outline-inferred-types outline)))
      (set-outline-entry-types! outline (resolve-copies entry entry))
      (set-outline-inferred-types! outline (resolve-copies inferred entry))
      (set-outline-read-indices! outline (sort (map car entry) <))
      (set-outline-write-indices! outline (sort (map car inferred) <)))
    (set-outline-initialized! outline #t)))

(define (expand-outline outline offset nlocals)
  (let lp ((n 0) (acc (outline-live-indices outline)))
    (if (< n nlocals)
        (lp (+ n 1) (if (memq n acc)
                        acc
                        (cons n acc)))
        (set-outline-live-indices! outline (sort acc <)))))

(define (set-entry-type! outline n t)
  (let* ((inferred (outline-inferred-types outline))
         (entry (outline-entry-types outline))
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
    (set-outline-entry-types! outline entry)))

(define (set-inferred-type! outline n t)
  (let* ((inferred (outline-inferred-types outline))
         (inferred (assq-set! inferred n t)))
    (debug 2 ";;; [set-inferred-type!] ~s => ~a~%" n (pretty-type t))
    (set-outline-inferred-types! outline inferred)))
