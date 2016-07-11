;;;; Trampoline data type and interace

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
;;; A module defining interfaces for trampoline.
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
;;;
;;; Code:

(define-module (language trace trampoline)
  #:use-module (rnrs bytevectors)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native lightning)
  #:export (make-trampoline
            trampoline-ref
            trampoline-set!))


;;;; Auxiliary

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

(define-inlinable (emit-to-bytevector!)
  (let* ((size (jit-code-size))
         (bv (make-bytevector size)))
    (jit-set-code (bytevector->pointer bv) (imm size))
    (jit-emit)
    (make-bytevector-executable! bv)
    bv))


;;;; Exported interface

(define *trampoline-cache* '())

(define (make-trampoline num-entries)
  (let* ((size (* num-entries size-of-trampoline-entry))
         (paged-size (* 4096 (+ (quotient size 4096) 1)))
         (cached (assq-ref *trampoline-cache* paged-size)))
    (if cached
        (let ((bv (bytevector-copy cached)))
          (make-bytevector-executable! bv)
          bv)
        (let ((bv (with-jit-state
                   (jit-prolog)
                   (jit-tramp (imm (* 4 %word-size)))
                   (do ((i 0 (+ i 1)))
                       ((<= (/ paged-size size-of-trampoline-entry) i))
                     ;; Dummy jump destination.
                     (jit-movi r0 (imm #xdeadbeaf))
                     (jit-jmpr r0))
                   (jit-epilog)
                   (jit-realize)
                   (emit-to-bytevector!))))
          (set! *trampoline-cache*
            (acons paged-size bv *trampoline-cache*))
          bv))))

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
     (bytevector-copy! entry 0 trampoline (* i size-of-trampoline-entry)
                       size-of-move-immediate))))
