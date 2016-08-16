;;;; ANF IR for structs and GOOPS

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
;;; Module containing ANF IR definitions for structs and GOOPS
;;;
;;; Code:

(define-module (language trace ir-struct)
  #:use-module (system foreign)
  #:use-module (language trace error)
  #:use-module (language trace ir)
  #:use-module (language trace env)
  #:use-module (language trace primitives)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables))

;;; XXX: Disabled until the traces without these IRs get stable.

;; XXX: struct-vtable
;; (define-ir (struct-vtable (struct! dst) (struct src))
;;   (let ((src/v (src-ref src))
;;         (dst/v (dst-ref dst))
;;         (r2 (make-tmpvar 2)))
;;     (with-type-guard &struct src
;;       `(let ((,r2 (,%cref ,src/v 0)))
;;          (let ((,r2 (,%sub ,r2 1)))
;;            (let ((,dst/v (,%cref ,r2 2)))
;;              ,(next)))))))

;; XXX: allocate-struct
;; XXX: struct-ref
;; XXX: struct-set!

;; XXX: allocate-struct/immediate
;; (define-ir (allocate-struct/immediate (struct! dst) (struct vtable)
;;                                       (const nfields))
;;   (let ((vt/v (src-ref vtable))
;;         (dst/v (dst-ref dst)))
;;     `(let ((_ (,%carg ,(+ (ash nfields 2) 2))))
;;        (let ((_ (,%carg ,vt/v)))
;;          (let ((,dst/v (,%ccall ,(object-address allocate-struct))))
;;            ,(next))))))

;; XXX: struct-ref/immediate
;; (define-ir (struct-ref/immediate (scm! dst) (struct src) (const idx))
;;   (let ((src/v (src-ref src))
;;         (dst/v (dst-ref dst))
;;         (r2 (make-tmpvar 2)))
;;     (with-type-guard &struct src
;;       `(let ((,r2 (,%cref ,src/v 1)))
;;         (let ((,dst/v (,%cref ,r2 ,idx)))
;;           ,(next))))))

;; XXX: struct-set!/immediate
;; (define-ir (struct-set!/immediate (struct dst) (const idx) (scm src))
;;   (let ((src/v (src-ref src))
;;         (dst/v (src-ref dst))
;;         (r2 (make-tmpvar 2)))
;;     (with-type-guard &struct dst
;;       `(let ((,r2 (,%cref ,dst/v 1)))
;;         (let ((_ (,%cset ,r2 ,idx ,src/v)))
;;           ,(next))))))

;; XXX: class-of
