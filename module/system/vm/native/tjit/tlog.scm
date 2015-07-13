;;; -*- mode: scheme; coding: utf-8; -*-

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

;;; Log of native compilation.

;;; Code:

(define-module (system vm native tjit tlog)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit parameters)
  #:export (<tlog>
            make-tlog
            tlog-id
            tlog-code
            tlog-entry-ip
            tlog-snapshots
            tlog-side-exit-variables
            tlog-side-exit-codes
            tlog-trampoline
            tlog-loop-address
            tlog-loop-locals
            tlog-loop-vars

            dump-tlog
            put-tlog!
            get-tlog

            make-trampoline
            trampoline-ref
            trampoline-set!))


;;;
;;; Trampoline
;;;

;;; `Trampoline' is a chunk of native code containing jump
;;; destinations. Updating the contents of bytevector containing naitve code
;;; will not work when the size of new bytevector is larget than the size of old
;;; bytevector.  To make things work with different size of native code, using a
;;; layer of native code containing jump destinations.

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
;;; Trace compilation log
;;;

;; Record type to contain various information for compilation of trace to native
;; code.  Information stored in this record type is used when re-entering hot
;; bytecode IP, patching native code from side exit, ... etc.
;;
;; This record type is shared with C code. Macros written in
;; "libguile/vm-tjit.h" with "SCM_TLOG" prefix are referring the
;; contents.
;;
(define-record-type <tlog>
  (%make-tlog id code exit-counts entry-ip
              snapshots side-exit-variables side-exit-codes trampoline
              loop-address loop-locals loop-vars)
  tlog?

  ;; Trace id number.
  (id tlog-id)

  ;; Bytevector of compiled native code.
  (code tlog-code)

  ;; Hash-table containing number of exits taken, per exit-id.
  (exit-counts tlog-exit-counts)

  ;; Entry bytecode IP.
  (entry-ip tlog-entry-ip)

  ;; Snapshot locals and types.
  (snapshots tlog-snapshots)

  ;; ;; Address of start of the loop in native code.
  ;; (loop-start-address tlog-loop-start-address)

  ;; Hash-table containing variables for side exit.
  (side-exit-variables tlog-side-exit-variables)

  ;; Hash-table containing side exit codes.
  (side-exit-codes tlog-side-exit-codes)

  ;; Trampoline, native code containing jump destinations.
  (trampoline tlog-trampoline)

  ;; Address of start of loop.
  (loop-address tlog-loop-address)

  ;; Local header information of loop.
  (loop-locals tlog-loop-locals)

  ;; Variable header information of loop.
  (loop-vars tlog-loop-vars))

(define make-tlog %make-tlog)

(define (dump-tlog tlog)
  (format #t "~20@a~a~%" "*****" " tlog *****")
  (format #t "~19@a: ~a~%" 'id (tlog-id tlog))
  (format #t "~19@a: addr=~a size=~a~%" 'code
          (bytevector->pointer (tlog-code tlog))
          (bytevector-length (tlog-code tlog)))
  (format #t "~19@a: ~{~a ~}~%" 'exit-counts
          (reverse! (hash-fold acons '() (tlog-exit-counts tlog))))
  (format #t "~19@a: ~x~%" 'entry-ip (tlog-entry-ip tlog))
  (format #t "~19@a: ~{~a~^~%                     ~}~%" 'snapshots
          (let ((snapshots (tlog-snapshots tlog)))
            (sort (hash-fold acons '() snapshots)
                  (lambda (a b)
                    (< (car a) (car b))))))
  (format #t "~19@a: ~{~a~^~%                     ~}~%" 'side-exit-vars
          (let ((vars (tlog-side-exit-variables tlog)))
            (sort (hash-fold acons '() vars)
                  (lambda (a b)
                    (< (car a) (car b))))))
  (format #t "~19@a: ~a~%" 'side-exit-codes (tlog-side-exit-codes tlog))
  (let ((code (tlog-trampoline tlog)))
    (format #t "~19@a: ~a:~a~%" 'trampoline
            (and (bytevector? code) (bytevector->pointer code))
            (and (bytevector? code) (bytevector-length code))))
  (format #t "~19@a: ~a~%" 'loop-address (tlog-loop-address tlog))
  (format #t "~19@a: ~a~%" 'loop-locals (tlog-loop-locals tlog))
  (format #t "~19@a: ~a~%" 'loop-vars (tlog-loop-vars tlog)))

(define (put-tlog! key tlog)
  (hashq-set! (tlog-table) key tlog))

(define (get-tlog key)
  (hashq-ref (tlog-table) key #f))
