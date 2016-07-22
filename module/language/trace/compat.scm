;;;; Compatibility management for different targets

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
;;; Module to manage target compatibility, such as register information
;;; and piece of native code used to update jump table.
;;;
;;; This module includes a target specific file and replace the module
;;; contents. Included file is expected to have definitions listed in
;;; export list of this module.
;;;
;;; Code:

(define-module (language trace compat)
  #:use-module (language trace error)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system vm native lightning)
  #:export
  (
   ;; Vector of available GPRs for locals. This vector should not
   ;; contain scratch registers, %fp, %sp, and %thread.
   *gprs*

   ;; Vector of FPRs. This vector should not contain scratch registers.
   *fprs*

   ;; Volatile registers.
   ;;
   ;; Ordering of *volatile-registers* is mandatory. The last element in
   ;; the vector is ARG1 register, next to the last is ARG2 register,
   ;; and so on. Non-argument volatile registers need to be placed at
   ;; the beginning of the vector. R0, R1, and R2 are used as scratch
   ;; register.
   *volatile-registers*

   ;; Vector of non-volatile registers. Should not contain
   *non-volatile-registers*

   ;; Number of GPRs used for passing arguments.
   *num-arg-gprs*

   ;; Number of FPRs used for passing arguments.
   *num-arg-fprs*

   ;; Name of register, for pretty printing. Scratch registers may
   ;; passed to this procedure.
   register-name

   ;; Piece of native code to update an entry in trampoline.
   move-immediate-code))


;;;; Replaced file contents

;; Below will include target specific variant of this module by
;; evaluating syntax when compiling module to bytecode.

(eval-when (compile expand)
  (define-syntax replaced-contents
    (lambda (x)
      (define (the-file)
        (let ((cpu ((@ (system base target) target-cpu)))
              (os ((@ (system base target) target-os))))
          (cond
           ((and (equal? cpu "x86_64") (equal? os "linux-gnu"))
            "compat-x86_64-linux.scm")
           (else
            (error "Unsupported target ~a ~a" cpu os)))))
      (syntax-case x ()
        ((k) #`(include #,(datum->syntax #'k (the-file))))))))

(replaced-contents)
