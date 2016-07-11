;;;; Registers used in vm-tjit engine

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
;;; Module containing register related codes.
;;;
;;; Code:

(define-module (language trace registers)
  #:use-module (language trace compat)
  #:use-module (language trace error)
  #:use-module (system vm native lightning)
  #:export (gpr-ref
            fpr-ref
            *num-gpr*
            *num-fpr*
            *num-volatiles*
            *num-non-volatiles*
            %fp %sp %thread %retval)
  #:re-export (*gprs*
               *fprs*
               *num-arg-gprs*
               *num-arg-fprs*
               register-name))


;;;; Exported

;; Non-volatile register to refer FP in native code. This FP came from
;; Lightning, not the `vp->fp' passed from VM interpreter.
(define %fp (jit-fp))

;; Non-volatile register to hold vp->sp.
(define-syntax %sp (identifier-syntax v0))

;; Non-volatile register to hold thread.
(define-syntax %thread (identifier-syntax v1))

;; Volatile register to hold retval.
(define-syntax %retval (identifier-syntax r0))

(define *num-gpr* (vector-length *gprs*))

(define *num-fpr* (vector-length *fprs*))

(define *num-volatiles* (vector-length *volatile-registers*))

(define *num-non-volatiles* (vector-length *non-volatile-registers*))

;; `gpr-ref' and `fpr-ref' use negative numbers to refer scratch registers.
(define-inlinable (gpr-ref i)
  (cond
   ((= i -1) r0)
   ((= i -2) r1)
   ((= i -3) r2)
   ((<= 0 i) (vector-ref *gprs* i))
   (else (failure 'gpr-ref "~s" i))))

(define-inlinable (fpr-ref i)
  (cond
   ((= i -1) f0)
   ((= i -2) f1)
   ((= i -3) f2)
   ((<= 0 i) (vector-ref *fprs* i))
   (else (failure 'fpr-ref "~s" i))))
