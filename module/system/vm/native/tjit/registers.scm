;;;; Registers used in vm-tjit engine

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
;;; Module containing register related codes. Currently works under x86-64 only,
;;; using architecture dependent registers.  Lightning has it's own register
;;; management policy, not sure how it works under other architecture than
;;; x86-64 linux.
;;;
;;; Code:

(define-module (system vm native tjit registers)
  #:use-module (system vm native lightning)
  #:export (*num-gpr*
            *num-fpr*
            *num-volatiles*
            *num-non-volatiles*
            *num-arg-gprs*
            *num-arg-fprs*
            gpr-ref
            fpr-ref
            fp
            reg-thread
            reg-retval))

;;;
;;; Internal aliases
;;;

(define r8 (jit-r 8))
(define r9 (jit-r 9))
(define rcx (jit-r 10))
(define rdx (jit-r 11))
(define rsi (jit-r 12))
(define rdi (jit-r 13))

(define xmm7 (jit-f 8))
(define xmm6 (jit-f 9))
(define xmm5 (jit-f 10))
(define xmm4 (jit-f 11))
(define xmm3 (jit-f 12))
(define xmm2 (jit-f 13))
(define xmm1 (jit-f 14))
(define xmm0 (jit-f 15))


;;;
;;; Exported
;;;

;; Non-volatile register to refer FP in native code.
(define fp (jit-fp))

;; Non-volatile register to hold thread.
(define-syntax reg-thread (identifier-syntax v0))

;; Volatile register to hold return value from native code to VM.
(define-syntax reg-retval (identifier-syntax r0))

(define *non-volatile-registers*
  `#(,v1 ,v2 ,v3 ,r3))

;; Ordering is mandatory. The last element in the vector is ARG1 register, next
;; to the last vector element is ARG2 register, and so on. Non-argument volatile
;; registers need to be placed at the beginning of the vector. R0, R1, and R2
;; are used as scratch register.
(define *volatile-registers*
  `#(,r9 ,r8 ,rcx ,rdx ,rsi ,rdi))

(define *gprs*
  (list->vector (append (vector->list *non-volatile-registers*)
                        (vector->list *volatile-registers*))))

(define *fprs*
  `#(,f3 ,f4 ,f5 ,f6 ,f7 ,xmm7 ,xmm6 ,xmm5 ,xmm4 ,xmm3 ,xmm2 ,xmm1 ,xmm0))

(define *num-gpr*
  (vector-length *gprs*))

(define *num-fpr*
  (vector-length *fprs*))

(define *num-volatiles*
  (vector-length *volatile-registers*))

(define *num-non-volatiles*
  (vector-length *non-volatile-registers*))

(define *num-arg-gprs*
  6)

(define *num-arg-fprs*
  8)

;; Using negative number to refer scratch registers.

(define (gpr-ref i)
  (cond
   ((= i -1) r0)
   ((= i -2) r1)
   ((= i -3) r2)
   (else (vector-ref *gprs* i))))

(define (fpr-ref i)
  (cond
   ((= i -1) f0)
   ((= i -2) f1)
   ((= i -3) f2)
   (else
    (vector-ref *fprs* i))))
