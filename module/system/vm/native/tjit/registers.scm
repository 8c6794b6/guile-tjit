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
;;; Module containing register related codes. Currently works under x86-64 Linux
;;; only, using architecture dependent registers. Lightning has it's own
;;; register management policy, not sure how it works under other architecture
;;; than x86-64 Linux.
;;;
;;; Code:

(define-module (system vm native tjit registers)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit error)
  #:export (*num-gpr*
            *num-fpr*
            *num-volatiles*
            *num-non-volatiles*
            *num-arg-gprs*
            *num-arg-fprs*
            gpr-ref
            fpr-ref
            %fp
            %sp
            %thread
            %retval
            register-name))

;;;
;;; Internal aliases
;;;

(define rbx v0)
(define r13 v1)
(define r14 v2)
(define r15 v3)

(define r9 (jit-r 8))
(define r8 (jit-r 9))
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

;; Non-volatile register to refer FP in native code. This FP came from
;; Lightning, not the `vp->fp' passed from VM interpreter.
(define %fp (jit-fp))

;; Non-volatile register to hold vp->sp.
(define-syntax %sp (identifier-syntax rbx))

;; Non-volatile register to hold thread.
(define-syntax %thread (identifier-syntax r13))

;; Volatile register to hold retval.
(define-syntax %retval (identifier-syntax r0))

;; Non-volatile registers. `r3' from Lightning is R12, which is non-volatile in
;; Linux's calling convention sense, but Lightning sometimes internally uses
;; `r3', e.g: when branching instruction `jit-beqi' which takes immediate
;; values, were called and r0, r1, r2 were already in use.
(define *non-volatile-registers*
  `#(,r14 ,r15))

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

;; Number of GPRs used for argument passing.
(define *num-arg-gprs* 6)

;; Number of FPRs used for argument passing.
(define *num-arg-fprs* 8)

;; Using negative numbers to refer scratch GPRs.
(define (gpr-ref i)
  (if (< i 0)
      (cond
       ((= i -1) r0)
       ((= i -2) r1)
       ((= i -3) r2)
       (else (failure 'gpr-ref "~s" i)))
      (vector-ref *gprs* i)))

;; Using negative numbers to refer scratch FPRs.
(define (fpr-ref i)
  (if (< i 0)
      (cond
       ((= i -1) f0)
       ((= i -2) f1)
       ((= i -3) f2)
       (else (failure 'fpr-ref "~s" i)))
      (vector-ref *fprs* i)))

(define (register-name r)
  (let ((gpr-names
         ;; RBX is for %sp, R13 is for %thread, and R12 is used by
         ;; Lightning.
         #(r11 r10 rax r14 r15 r9 r8 rcx rdx rsi rdi))
        (fpr-names
         #(xmm10 xmm9 xmm8 xmm11 xmm12 xmm13 xmm14 xmm15
                 xmm7 xmm6 xmm5 xmm4 xmm3 xmm2 xmm1 xmm0)))
    (when (not (pair? r))
      (failure 'physical-name "unknown argument ~s" r))
    (let ((t (car r))
          (n (cdr r)))
      (when (not (integer? n))
        (failure 'physical-name "cdr not an integer ~s" r))
      (cond
       ((eq? t 'gpr)
        (vector-ref gpr-names (+ 3 n)))
       ((eq? t 'fpr)
        (vector-ref fpr-names (+ 3 n)))
       (else
        (failure 'physical-name "not a register ~s" r))))))
