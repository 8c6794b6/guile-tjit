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
            outline-local-indices set-outline-local-indices!
            outline-local-ref
            outline-type-ref
            outline-initialized?
            outline-infer-type? set-outline-infer-type!
            outline-backward? set-outline-backward!
            outline-sp-offsets set-outline-sp-offsets!
            outline-fp-offsets set-outline-fp-offsets!
            outline-types set-outline-types!
            outline-dls
            outline-ras
            outline-sp-offset set-outline-sp-offset!
            outline-fp-offset set-outline-fp-offset!
            outline-ret-types set-outline-ret-types!
            outline-write-indices set-outline-write-indices!
            outline-read-indices set-outline-read-indices!
            outline-write-buf set-outline-write-buf!
            outline-live-indices set-outline-live-indices!
            outline-entry-types set-outline-entry-types!
            outline-inferred-types set-outline-inferred-types!
            outline-expected-types set-outline-expected-types!

            pop-outline!
            push-outline!
            arrange-outline
            expand-outline
            set-outline-previous-dl-and-ra!
            merge-outline-types!
            set-entry-type!
            set-expected-type!
            set-inferred-type!))


;; Data type to contain outline of recorded traces.
;;
;; Stores various information used for compiling IR, such as dynamic links and
;; return addresses, past frame locals, locals of caller procedure in inlined
;; procedure ... etc.
(define-record-type $outline
  (%make-outline initialized? infer-type? backward?
                 dls ras locals local-indices
                 sp-offsets fp-offsets types sp-offset fp-offset ret-types
                 write-indices read-indices write-buf live-indices
                 entry-types expected-types inferred-types)
  outline?

  ;; Flag to hold whether initialized.
  (initialized? outline-initialized? set-outline-initialized!)

  ;; Flag to hold whether infer type.
  (infer-type? outline-infer-type? set-outline-infer-type!)

  ;; Flag to hold whether the operation goes backward.
  (backward? outline-backward? set-outline-backward!)

  ;; Association list for dynamic link: (local . pointer to fp).
  (dls outline-dls set-outline-dls!)

  ;; Association list for return address: (local . pointer to ra).
  (ras outline-ras set-outline-ras!)

  ;; Vector containing locals.
  (locals outline-locals)

  ;; All local indices found in trace.
  (local-indices outline-local-indices set-outline-local-indices!)

  ;; Vector containing SP offset per bytecode operation.
  (sp-offsets outline-sp-offsets set-outline-sp-offsets!)

  ;; Vector containing FP offset per bytecode operation.
  (fp-offsets outline-fp-offsets set-outline-fp-offsets!)

  ;; Stack element types.
  (types outline-types set-outline-types!)

  ;; Current SP offset.
  (sp-offset outline-sp-offset set-outline-sp-offset!)

  ;; Current FP offset.
  (fp-offset outline-fp-offset set-outline-fp-offset!)

  ;; Returned types from C functions.
  (ret-types outline-ret-types set-outline-ret-types!)

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

(define (make-outline types sp-offset fp-offset write-indices live-indices
                      expected-types)
  ;; Using hash-table to contain locals, since local index could take negative
  ;; value.
  (%make-outline #f #t #f '() '() (make-hash-table) '() '() '() types
                 sp-offset fp-offset '() write-indices '()
                 (list write-indices) live-indices
                 '() (copy-tree expected-types) '()))

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
         (ret-types/vec (list->vector (reverse! (outline-ret-types outline))))
         (reads/list (sort (outline-read-indices outline) <))
         (writes/list (sort (outline-write-indices outline) <))
         (write-buf/vec (list->vector (reverse! (outline-write-buf outline))))
         (locals/list
          (sort (delete-duplicates (append writes/list reads/list)) >)))
    (set-outline-local-indices! outline locals/list)
    (set-outline-sp-offsets! outline sp-offsets/vec)
    (set-outline-fp-offsets! outline fp-offsets/vec)
    (set-outline-ret-types! outline ret-types/vec)
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

(define (push-outline! outline dl ra sp-offset locals)
  (set-outline-dls! outline (cons dl (outline-dls outline)))
  (set-outline-ras! outline (cons ra (outline-ras outline)))
  (let lp ((i 0)
           (end (vector-length locals))
           (to-update (outline-locals outline)))
    (when (< i end)
      (hashq-set! to-update (+ i sp-offset) (vector-ref locals i))
      (lp (+ i 1) end to-update)))
  outline)

(define (pop-outline! outline sp-offset locals)
  (let ((old-dls (outline-dls outline))
        (old-ras (outline-ras outline)))
    (when (not (null? old-dls))
      (set-outline-dls! outline (cdr old-dls)))
    (when (not (null? old-ras))
      (set-outline-ras! outline (cdr old-ras)))
    (let lp ((i 0)
             (end (vector-length locals))
             (t (outline-locals outline)))
      (when (< i end)
        (hashq-set! t (+ i sp-offset) (vector-ref locals i))
        (lp (+ i 1) end t)))
    outline))

(define (expand-outline outline offset nlocals)
  (let ((locals (outline-locals outline))
        (undefined (make-pointer #x904)))
    (let lp ((n 0) (acc (outline-live-indices outline)))
      (if (< n nlocals)
          (begin
            (hashq-set! locals (+ offset n) undefined)
            (lp (+ n 1) (if (memq n acc)
                            acc
                            (cons n acc))))
          (set-outline-live-indices! outline (sort acc <))))))

(define (set-outline-previous-dl-and-ra! outline stack-size ra dl)
  (let ((locals (outline-locals outline)))
    (hashq-set! locals (- stack-size 1) ra)
    (hashq-set! locals stack-size dl)))

(define (outline-local-ref outline i)
  (hashq-ref (outline-locals outline) i))

(define (outline-type-ref outline i)
  (assq-ref (outline-types outline) i))

(define (merge-outline-types! outline local-x-types)
  (let lp ((locals local-x-types)
           (types (outline-types outline)))
    (match locals
      (((local . type) . locals)
       (let* ((etype (if (symbol? type)
                         type
                         (type->stack-element-type type)))
              (types (assq-set! types local etype)))
         (lp locals types)))
      (_
       (set-outline-types! outline types)))))

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
