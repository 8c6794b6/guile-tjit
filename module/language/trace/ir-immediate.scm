;;;; ANF IR for immediates and statically allocated non-immediates

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
;;; Module containing ANF IR definitions for immediates and statically allocated
;;; non-immediates operations.
;;;
;;; Code:

(define-module (language trace ir-immediate)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (language trace error)
  #:use-module (language trace env)
  #:use-module (language trace ir)
  #:use-module (language trace primitives)
  #:use-module (language trace snapshot)
  #:use-module (language trace types))



(define-constant (make-short-immediate low-bits)
  low-bits)

(define-constant (make-long-immediate low-bits)
  low-bits)

(define-constant (make-long-long-immediate high-bits low-bits)
  (logior (ash high-bits 32) low-bits))

(define-constant (make-non-immediate offset)
  (+ ip (* 4 offset)))

(define-constant (static-ref offset)
  (let* ((ref (dereference-pointer (make-pointer (+ ip (* 4 offset)))))
         (src (pointer->scm ref)))
    (if (flonum? src)
        src
        (pointer-address ref))))

;; XXX: static-set!
;; XXX: static-patch!

(define-constant (load-f64 high-bits low-bits)
  ;; Convert bits to SCM flonum by storing to bytevector once.
  (let ((bv (make-bytevector 8)))
    (bytevector-u64-native-set! bv 0 (logior (ash high-bits 32) low-bits))
    (bytevector-ieee-double-native-ref bv 0)))

(define-constant (load-u64 high-bits low-bits)
  (logior (ash high-bits 32) low-bits))

;; XXX: load-s64
