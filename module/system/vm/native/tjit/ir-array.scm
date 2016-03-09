;;; ANF IR for arrays, packed uniform arrays, and bytevectors

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
;;; Module containing ANF IR definitions for arrays, packed uniform arrays, and
;;; bytevectors.
;;;
;;; Code:

(define-module (system vm native tjit ir-array)
  #:use-module (system foreign)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types))

;; XXX: load-typed-array
;; XXX: make-array

;; XXX: bv-u8-ref
;; (define-ir (bv-u8-ref (u64 dst) (scm src) (u64 idx))
;;   (let ((dst/v (var-ref dst))
;;         (src/v (var-ref src))
;;         (idx/v (var-ref idx))
;;         (contents (make-tmpvar 2)))
;;     `(let ((,contents (%add ,src/v 16)))
;;        (let ((,contents (%cref ,contents 0)))
;;          (let ((,dst/v (%cref ,contents ,idx/v)))
;;            (let ((,dst/v (%band ,dst/v #xff)))
;;              ,(next)))))))

;; XXX: bv-s8-ref
;; XXX: bv-u16-ref
;; XXX: bv-s16-ref
;; XXX: bv-u32-ref
;; XXX: bv-s32-ref
;; XXX: bv-u64-ref
;; XXX: bv-s64-ref
;; XXX: bv-f32-ref
;; XXX: bv-f64-ref
;; XXX: bv-u8-set!
;; XXX: bv-s8-set!
;; XXX: bv-u16-set!
;; XXX: bv-s16-set!
;; XXX: bv-u32-set!
;; XXX: bv-s32-set!
;; XXX: bv-u64-set!
;; XXX: bv-s64-set!
;; XXX: bv-f32-set!
;; XXX: bv-f64-set!

(define-ir (bv-length (u64! dst) (scm src))
  (nyi "bv-length: ~a ~a" dst src))

(define-ir (bv-length (u64! dst) (bytevector src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%cref ,src/v 1)))
       ,(next))))
