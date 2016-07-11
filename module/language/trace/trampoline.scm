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
  #:use-module (ice-9 format)
  #:use-module (language trace compat)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
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

(define *move-immediate-template* #f)

(define (get-size-of-move-immediate)
  (with-jit-state
   (jit-prolog)
   (jit-tramp (imm (* 4 %word-size)))
   (jit-movi r0 (imm #xdeadbeaf))
   (jit-epilog)
   (jit-realize)
   (let* ((ptr (jit-emit))
          (size (jit-code-size)))
     (set! *move-immediate-template*
       (pointer->bytevector ptr size))
     size)))

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

;; Record type for trampoline. Holds bytevector of native code and the address
;; of the native code.
(define-record-type <trampoline>
  (%make-trampoline code address)
  trampoline?
  (code trampoline-code set-trampoline-code!)
  (address trampoline-address set-trampoline-address!))

(define *trampoline-code-cache* '())


;;;; Exported interface

(define (make-trampoline num-entries)
  (let* ((size (* num-entries size-of-trampoline-entry))
         (paged-size (* 4096 (+ (quotient size 4096) 1)))
         (cached (assq-ref *trampoline-code-cache* paged-size)))
    (if cached
        (let* ((bv (bytevector-copy cached))
               (addr (pointer-address (bytevector->pointer bv))))
          (make-bytevector-executable! bv)
          (%make-trampoline bv addr))
        (with-jit-state
         (jit-prolog)
         (jit-tramp (imm (* 4 %word-size)))
         (do ((i 0 (+ i 1)))
             ((<= (/ paged-size size-of-trampoline-entry) i))
           ;; Dummy jump destination.
           (jit-movi r0 (imm #xdeadbeaf))
           (jit-jmpr r0))
         (jit-epilog)
         (jit-realize)
         (let* ((size (jit-code-size))
                (bv (make-bytevector size))
                (_ (jit-set-code (bytevector->pointer bv) (imm size)))
                (ptr (jit-emit)))
           (make-bytevector-executable! bv)
           (set! *trampoline-code-cache*
             (acons paged-size bv *trampoline-code-cache*))
           (%make-trampoline bv (pointer-address ptr)))))))

(define (trampoline-ref trampoline i)
  (make-pointer (+ (trampoline-address trampoline)
                   (* i size-of-trampoline-entry))))

(define (trampoline-set! trampoline i dest)
  (bytevector-copy! (move-immediate-code dest) 0
                    (trampoline-code trampoline)
                    (* i size-of-trampoline-entry)
                    size-of-move-immediate))
