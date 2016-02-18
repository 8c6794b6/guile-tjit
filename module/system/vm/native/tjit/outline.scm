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
            outline-locals
            outline-initialized?
            outline-sp-offsets set-outline-sp-offsets!
            outline-fp-offsets set-outline-fp-offsets!
            outline-sp-offset set-outline-sp-offset!
            outline-fp-offset set-outline-fp-offset!
            outline-write-indices set-outline-write-indices!
            outline-read-indices set-outline-read-indices!
            outline-write-buf set-outline-write-buf!
            outline-live-indices set-outline-live-indices!
            outline-entry-types set-outline-entry-types!
            outline-inferred-types set-outline-inferred-types!
            outline-expected-types set-outline-expected-types!

            arrange-outline
            outline-local-indices
            expand-outline
            set-entry-type!
            set-expected-type!
            set-inferred-type!))


;; Data type to contain outline of recorded traces.
;;
;; Stores various information used for compiling IR, such as dynamic links and
;; return addresses, past frame locals, locals of caller procedure in inlined
;; procedure ... etc.
(define-record-type $outline
  (%make-outline initialized? sp-offsets fp-offsets sp-offset fp-offset
                 write-indices read-indices write-buf live-indices
                 entry-types expected-types inferred-types)
  outline?

  ;; Flag to hold whether initialized.
  (initialized? outline-initialized? set-outline-initialized!)

  ;; Vector containing SP offset per bytecode operation.
  (sp-offsets outline-sp-offsets set-outline-sp-offsets!)

  ;; Vector containing FP offset per bytecode operation.
  (fp-offsets outline-fp-offsets set-outline-fp-offsets!)

  ;; Current SP offset.
  (sp-offset outline-sp-offset set-outline-sp-offset!)

  ;; Current FP offset.
  (fp-offset outline-fp-offset set-outline-fp-offset!)

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

  ;; Expected types.
  (expected-types outline-expected-types set-outline-expected-types!)

  ;; Inferred types.
  (inferred-types outline-inferred-types set-outline-inferred-types!))

(define (make-outline sp-offset fp-offset write-indices live-indices
                      types-from-parent)
  ;; Using hash-table to contain locals, since local index could take negative
  ;; value.
  (%make-outline #f '() '() sp-offset fp-offset
                 write-indices '() (list write-indices) live-indices
                 '() '() (copy-tree types-from-parent)))

(define (outline-local-indices ol)
  (sort (delete-duplicates (append (outline-write-indices ol)
                                   (outline-read-indices ol)))
        >))

(define (arrange-outline outline)
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
         (reads/list (sort (outline-read-indices outline) <))
         (writes/list (sort (outline-write-indices outline) <))
         (write-buf/vec (list->vector (reverse! (outline-write-buf outline)))))
    (set-outline-sp-offsets! outline sp-offsets/vec)
    (set-outline-fp-offsets! outline fp-offsets/vec)
    (set-outline-read-indices! outline reads/list)
    (set-outline-write-indices! outline writes/list)
    (set-outline-write-buf! outline write-buf/vec)
    (let ((entry (outline-entry-types outline))
          (expected (outline-expected-types outline))
          (inferred (outline-inferred-types outline)))
      (set-outline-entry-types! outline (resolve-copies entry entry))
      (set-outline-expected-types! outline (resolve-copies expected entry))
      (set-outline-inferred-types! outline (resolve-copies inferred entry)))
    (set-outline-initialized! outline #t)
    outline))

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
    (debug 1 ";;; [set-entry-type!] ~s => ~a~%" n (pretty-type t))
    (set-outline-entry-types! outline entry)))

(define (set-expected-type! outline n t)
  (let* ((expected (outline-expected-types outline))
         (expected (assq-set! expected n t))
         (expected (let lp ((current expected) (acc expected))
                     (match current
                       (((i 'copy . (? (lambda (x) (= x n)))) . current)
                        (lp current (assq-set! acc i t)))
                       ((_ . current)
                        (lp current acc))
                       (() acc)))))
    (debug 1 ";;; [set-expected-type!] ~s => ~a~%" n (pretty-type t))
    (set-outline-expected-types! outline expected)))

(define (set-inferred-type! outline n t)
  (let* ((inferred (outline-inferred-types outline))
         (inferred (assq-set! inferred n t)))
    (debug 1 ";;; [set-inferred-type!] ~s => ~a~%" n (pretty-type t))
    (set-outline-inferred-types! outline inferred)))
