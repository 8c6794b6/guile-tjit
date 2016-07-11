;;;; Body of compatibility code for Linux x86_64

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
;;; Code specific to Linux x86_64.
;;;
;;; Code:


;;;; Internal aliases

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


;;;; Exported

;; Non-volatile registers. `r3' from Lightning is R12, which is non-volatile in
;; Linux's calling convention, but Lightning sometimes internally uses `r3',
;; e.g: when branching instruction `jit-beqi' which takes immediate values, were
;; called and r0, r1, r2 were already in use.
(define *non-volatile-registers*
  `#(,r14 ,r15))

;; Ordering is mandatory.
(define *volatile-registers*
  `#(,r9 ,r8 ,rcx ,rdx ,rsi ,rdi))

(define *gprs*
  (list->vector (append (vector->list *non-volatile-registers*)
                        (vector->list *volatile-registers*))))

(define *fprs*
  `#(,f3 ,f4 ,f5 ,f6 ,f7 ,xmm7 ,xmm6 ,xmm5 ,xmm4 ,xmm3 ,xmm2 ,xmm1 ,xmm0))

(define *num-arg-gprs* 6)

(define *num-arg-fprs* 8)

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


;;;; Piece of native code

(define-inlinable (move-immediate-code dest)
  (let ((bv (bytevector-copy #vu8(#xb8 0 0 0 0))))
    (bytevector-u32-native-set! bv 1 (pointer-address dest))
    bv))

