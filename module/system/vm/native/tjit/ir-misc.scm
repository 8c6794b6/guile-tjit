;;; ANF IR for miscellaneous operations

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
;;; Module containing ANF IR definitions for miscellaneous operations. IR
;;; defined in this module are relatively new in "libguile/vm-engine.c" and
;;; uncategorized. May move to relevant place later.
;;;
;;; Code:

(define-module (system vm native tjit ir-misc)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-ir (scm->f64 (f64! dst) (scm src))
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

(define-ir (f64->scm (scm! dst) (f64 src))
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

;; XXX: apply-non-program

(define-ir (scm->u64 (u64! dst) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%rsh ,src/v 2)))
       ,(next))))

(define-scan (u64->scm dst src)
  (set-entry-type! outline (+ src (outline-sp-offset outline)) &u64)
  (set-scan-initial-fields! outline))

(define-ti (u64->scm dst src)
  (let* ((src/l (u64-ref src))
         (sp-offset (outline-sp-offset outline))
         (type (if (< src/l most-positive-fixnum)
                            &fixnum
                            &scm)))
    (set-inferred-type! outline (+ dst sp-offset) type)))

;; XXX: Overflow check not yet done.
(define-anf (u64->scm dst src)
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%lsh ,src/v 2)))
       (let ((,dst/v (%add ,dst/v 2)))
         ,(next)))))

;; XXX: load-f64

(define-ir (load-u64 (u64! dst) (const high-bits) (const low-bits))
  `(let ((,(var-ref dst) ,(logior (ash high-bits 32) low-bits)))
     ,(next)))

;; XXX: scm->s64
;; XXX: s64->scm
;; XXX: load-s64

;; XXX: current-thread

;; XXX: logsub

;; XXX: scm->u64/truncate
