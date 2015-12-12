;;;; Fragment data type

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
;;; A module defining data types for fragment. Fragment is a log of native
;;; compilation. Fragments are stored in hash table for later use.
;;;
;;; Code:

(define-module (system vm native tjit fragment)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit parameters)
  #:export (<fragment>
            make-fragment
            fragment-id
            fragment-code
            fragment-exit-counts
            fragment-entry-ip
            fragment-parent-id
            fragment-parent-exit-id
            fragment-snapshots
            fragment-exit-codes
            fragment-trampoline
            fragment-loop-address
            fragment-loop-locals
            fragment-loop-vars
            fragment-fp-offset
            fragment-end-address

            put-fragment!
            get-fragment
            get-root-trace

            make-trampoline
            trampoline-ref
            trampoline-set!))


;;;
;;; Trampoline
;;;

;;; `Trampoline' is a chunk of native code containing jump destinations.
;;; Initially contains bailout code returning to VM interpreter.  Later when
;;; certain exit get hot, the bailout code will be replaced by native code of
;;; the side exit.  Updating the contents of bytevector containing naitve code
;;; will not work when the size of new bytevector is larget than the size of old
;;; bytevector.  Each native code entry in trampoline is a fragment of code
;;; containing jump to absolute address, which has same sizes. These fragments
;;; of native code are used as a layer to cope with updating native codes with
;;; different sizes.

(define (emit-to-bytevector!)
  (let* ((size (jit-code-size))
         (bv (make-bytevector size)))
    (jit-set-code (bytevector->pointer bv) (imm size))
    (jit-emit)
    (make-bytevector-executable! bv)
    bv))

(define (get-size-of-jump-to-register)
  (with-jit-state
   (jit-prolog)
   (jit-tramp (imm (* 4 %word-size)))
   (jit-jmpr r0)
   (jit-epilog)
   (jit-realize)
   (jit-emit)
   (jit-code-size)))

(define (get-size-of-move-immediate)
  (with-jit-state
   (jit-prolog)
   (jit-tramp (imm (* 4 %word-size)))
   (jit-movi r0 (imm #xdeadbeaf))
   (jit-epilog)
   (jit-realize)
   (jit-emit)
   (jit-code-size)))

(define size-of-jump-to-register
  (get-size-of-jump-to-register))

(define size-of-move-immediate
  (get-size-of-move-immediate))

(define size-of-trampoline-entry
  (+ size-of-jump-to-register size-of-move-immediate))

(define (make-trampoline size)
  (with-jit-state
   (jit-prolog)
   (jit-tramp (imm (* 4 %word-size)))
   (for-each (lambda _
               ;; Dummy jump destination.
               (jit-movi r0 (imm #xdeadbeaf))
               (jit-jmpr r0))
             (iota size))
   (jit-epilog)
   (jit-realize)
   (emit-to-bytevector!)))

(define (trampoline-ref trampoline i)
  (let ((start (bytevector->pointer trampoline))
        (offset (* i size-of-trampoline-entry)))
    (make-pointer (+ (pointer-address start) offset))))

(define (trampoline-set! trampoline i dest)
  (with-jit-state
   (jit-prolog)
   (jit-tramp (imm (* 4 %word-size)))
   (jit-movi r0 dest)
   (jit-epilog)
   (jit-realize)
   (let ((entry (pointer->bytevector (jit-emit) size-of-move-immediate)))
     (bytevector-copy! entry 0
                       trampoline (* i size-of-trampoline-entry)
                       size-of-move-immediate))))


;;;
;;; The fragment data
;;;

;; Record type to contain various information for compilation of trace to native
;; code.  Information stored in this record type is used when re-entering hot
;; bytecode IP, patching native code from side exit, ... etc.
;;
;; This record type is shared with C code. C macros written in
;; "libguile/vm-tjit.h" with "SCM_FRAGMENT" prefix are referring the fields.
;;
(define-record-type <fragment>
  (%make-fragment id code exit-counts downrec? uprec? entry-ip
                  parent-id parent-exit-id loop-address loop-locals loop-vars
                  snapshots trampoline fp-offset end-address)
  fragment?

  ;; Trace id number.
  (id fragment-id)

  ;; Bytevector of compiled native code.
  (code fragment-code)

  ;; Hash-table containing number of exits taken, per exit-id.
  (exit-counts fragment-exit-counts)

  ;; Flag to tell whether the trace was down-recursion or not.
  (downrec? fragment-downrec?)

  ;; Flag to tell whether the trace was up-recursion or not.
  (uprec? fragment-uprec?)

  ;; Entry bytecode IP.
  (entry-ip fragment-entry-ip)

  ;; Trace id of parent trace, 0 for root trace.
  (parent-id fragment-parent-id)

  ;; Exit id taken by parent, root traces constantly have 0.
  (parent-exit-id fragment-parent-exit-id)

  ;; Address of start of loop.
  (loop-address fragment-loop-address)

  ;; Local header information of loop.
  (loop-locals fragment-loop-locals)

  ;; Variable header information of loop.
  (loop-vars fragment-loop-vars)

  ;; Snapshot locals and types.
  (snapshots fragment-snapshots)

  ;; Trampoline, native code containing jump destinations.
  (trampoline fragment-trampoline)

  ;; FP offset in native code.
  (fp-offset fragment-fp-offset)

  ;; End address.
  (end-address fragment-end-address))

(define make-fragment %make-fragment)

(define (root-trace-fragment? fragment)
  (zero? (fragment-parent-id fragment)))

(define (put-fragment! trace-id fragment)
  (hashq-set! (tjit-fragment) trace-id fragment)
  (when (root-trace-fragment? fragment)
    (tjit-add-root-ip! (fragment-entry-ip fragment))
    (hashq-set! (tjit-root-trace) (fragment-entry-ip fragment) fragment)))

(define (get-fragment fragment-id)
  (hashq-ref (tjit-fragment) fragment-id #f))

(define (get-root-trace ip)
  (hashq-ref (tjit-root-trace) ip #f))
