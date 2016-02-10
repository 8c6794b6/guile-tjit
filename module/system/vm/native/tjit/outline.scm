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
  #:use-module (system vm native tjit types)
  #:export ($outline
            make-outline
            outline-locals
            outline-local-indices set-outline-local-indices!
            outline-local-ref
            outline-type-ref
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
            outline-inferred-types set-outline-inferred-types!
            outline-expecting-types set-outline-expecting-types!

            pop-outline!
            push-outline!
            arrange-outline
            expand-outline
            set-outline-previous-dl-and-ra!
            merge-outline-types!
            set-inferred-type!
            set-expecting-type!))


;; Data type to contain outline of recorded traces.
;;
;; Stores various information used for compiling IR, such as dynamic links and
;; return addresses, past frame locals, locals of caller procedure in inlined
;; procedure ... etc.
(define-record-type $outline
  (%make-outline dls ras locals local-indices sp-offsets fp-offsets types
                 sp-offset fp-offset ret-types write-indices read-indices
                 write-buf live-indices expecting-types inferred-types)
  outline?

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

  ;; Expecting types.
  (expecting-types outline-expecting-types set-outline-expecting-types!)

  ;; Inferred types.
  (inferred-types outline-inferred-types set-outline-inferred-types!))

(define (make-outline types sp-offset fp-offset write-indices live-indices
                      expecting-types)
  ;; Using hash-table to contain locals, since local index could take negative
  ;; value.
  (%make-outline '() '() (make-hash-table) '() '() '() types
                 sp-offset fp-offset '() write-indices '()
                 (list write-indices) live-indices expecting-types '()))

(define (arrange-outline outline)
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

(define (set-expecting-type! outline n t)
  (let ((expecting (outline-expecting-types outline))
        (inferred (outline-inferred-types outline)))
    (when (and (not (assq-ref expecting n))
               (not (assq-ref inferred n)))
      (set-outline-expecting-types! outline (assq-set! expecting n t)))))

(define (set-inferred-type! outline n t)
  (let ((inferred (outline-inferred-types outline)))
    (set-outline-inferred-types! outline (assq-set! inferred n t))))
