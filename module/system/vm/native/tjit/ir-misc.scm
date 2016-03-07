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
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-ir (scm->f64 (f64! dst) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%cref/f ,src/v 2)))
       ,(next))))

(define-ir (scm->f64 (f64! dst) (fixnum src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src))
        (r2 (make-tmpvar 2)))
    `(let ((,r2 (%rsh ,src/v 2)))
       (let ((,dst/v (%i2d ,r2)))
         ,(next)))))

(define-ir (scm->f64 (f64! dst) (flonum src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v ,src/v))
       ,(next))))

(define-ir (f64->scm (flonum! dst) (f64 src))
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

;; XXX: apply-non-program

(define-ir (scm->u64 (u64! dst) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%rsh ,src/v 2)))
       ,(next))))

(define-scan (u64->scm dst src)
  (set-entry-type! env (+ src (env-sp-offset env)) &u64)
  (set-scan-initial-fields! env))

;; XXX: Non-fixnum number conversion not yet implemented.
(define-ti (u64->scm dst src)
  (let* ((src/l (u64-ref src))
         (sp-offset (env-sp-offset env))
         (type (if (< src/l most-positive-fixnum)
                            &fixnum
                            (nyi "u64->scm: src=~a" src/l))))
    (set-inferred-type! env (+ dst sp-offset) type)))

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
