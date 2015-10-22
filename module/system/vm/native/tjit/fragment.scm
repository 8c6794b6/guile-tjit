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
  #:use-module (system vm native tjit compile-ir)
  #:use-module (system vm native tjit snapshot)
  #:export (<fragment>
            make-fragment
            fragment-id
            fragment-code
            fragment-exit-counts
            fragment-entry-ip
            fragment-snapshots
            fragment-exit-codes
            fragment-trampoline
            fragment-loop-address
            fragment-loop-locals
            fragment-loop-vars
            fragment-fp-offset
            fragment-end-address

            dump-fragment
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
  (%make-fragment id code exit-counts entry-ip parent-id parent-exit-id
                  loop-address loop-locals loop-vars snapshots
                  trampoline fp-offset end-address)
  fragment?

  ;; Trace id number.
  (id fragment-id)

  ;; Bytevector of compiled native code.
  (code fragment-code)

  ;; Hash-table containing number of exits taken, per exit-id.
  (exit-counts fragment-exit-counts)

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

(define-syntax-rule (snapshot-fmt)
  (let ((fields '("id=~a"
                  "sp-offset=~a"
                  "fp-offset=~a"
                  "nlocals=~a"
                  "locals=~a"
                  "variales=~a"
                  "code=~a"
                  "ip=~a")))
    (string-append "~13@a:" (string-join fields " ") "~%")))

(define (dump-fragment fragment)
  (format #t "~20@a~a~%" "*****" " fragment *****")
  (format #t "~19@a: ~a~%" 'id (fragment-id fragment))
  (format #t "~19@a: addr=~a size=~a~%" 'code
          (let ((code (fragment-code fragment)))
            (and (bytevector? code)
                 (bytevector->pointer code)))
          (bytevector-length (fragment-code fragment)))
  (format #t "~19@a: ~{~a ~}~%" 'exit-counts
          (reverse! (hash-fold acons '() (fragment-exit-counts fragment))))
  (format #t "~19@a: ~x~%" 'entry-ip (fragment-entry-ip fragment))
  (format #t "~19@a: ~a~%" 'parent-id (fragment-parent-id fragment))
  (format #t "~19@a: ~a~%" 'parent-exit-id (fragment-parent-exit-id fragment))
  (format #t "~19@a:~%" 'snapshots)
  (for-each
   (match-lambda
    ((i . ($ $snapshot id sp-offset fp-offset nlocals locals variables code ip))
     (format #t (snapshot-fmt)
             i id sp-offset fp-offset nlocals locals variables
             (and (bytevector? code) (bytevector->pointer code))
             ip)))
   (sort (hash-fold acons '() (fragment-snapshots fragment))
         (lambda (a b)
           (< (car a) (car b)))))
  (let ((code (fragment-trampoline fragment)))
    (format #t "~19@a: ~a:~a~%" 'trampoline
            (and (bytevector? code) (bytevector->pointer code))
            (and (bytevector? code) (bytevector-length code))))
  (format #t "~19@a: ~a~%" 'loop-address (fragment-loop-address fragment))
  (format #t "~19@a: ~a~%" 'loop-locals (fragment-loop-locals fragment))
  (format #t "~19@a: ~a~%" 'loop-vars (fragment-loop-vars fragment))
  (format #t "~19@a: ~a~%" 'fp-offset (fragment-fp-offset fragment))
  (format #t "~19@a: ~a~%" 'end-address (fragment-end-address fragment)))

(define (root-trace-fragment? fragment)
  (zero? (fragment-parent-id fragment)))

(define (put-fragment! trace-id fragment)
  (hashq-set! (tjit-fragment-table) trace-id fragment)
  (when (root-trace-fragment? fragment)
    (hashq-set! (tjit-root-trace-table) (fragment-entry-ip fragment) fragment)))

(define (get-fragment fragment-id)
  (hashq-ref (tjit-fragment-table) fragment-id #f))

(define (get-root-trace ip)
  (hashq-ref (tjit-root-trace-table) ip #f))
