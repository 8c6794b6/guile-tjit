;;; ANF IR for immediates and statically allocated non-immediates

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

(define-module (system vm native tjit ir-immediate)
  #:use-module (system foreign)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

;; XXX: Could infer type from value of `low-bits'.
(define-ir (make-short-immediate (scm! dst) (const low-bits))
  `(let ((,(var-ref dst) ,low-bits))
     ,(next)))

(define-ir (make-long-immediate (scm! dst) (const low-bits))
  `(let ((,(var-ref dst) ,low-bits))
     ,(next)))

(define-ir (make-long-long-immediate (scm! dst)
                                     (const high-bits)
                                     (const low-bits))
  `(let ((,(var-ref dst) ,(logior (ash high-bits 32) low-bits)))
     ,(next)))

(define-ir (make-non-immediate (scm! dst) (const offset))
  `(let ((,(var-ref dst) ,(+ ip (* 4 offset))))
     ,(next)))

(define-ir (static-ref (scm! dst) (const offset))
  (let ((src/l (dereference-scm (+ ip (* 4 offset)))))
    `(let ((,(var-ref dst)
            ;; XXX: Needs more type check.
            ,(cond
              ((flonum? src/l) src/l)
              (else (pointer-address (scm->pointer src/l))))))
       ,(next))))

;; XXX: static-set!
;; XXX: static-patch!
