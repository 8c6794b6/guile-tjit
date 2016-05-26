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
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types))

(define-constant (make-short-immediate low-bits)
  low-bits)

(define-constant (make-long-immediate low-bits)
  low-bits)

(define-constant (make-long-long-immediate high-bits low-bits)
  (logior (ash high-bits 32) low-bits))

(define-constant (make-non-immediate offset)
  (+ ip (* 4 offset)))

(define-scan (static-ref dst offset)
  (let ((sp-offset (env-sp-offset env)))
    (set-entry-type! env (+ dst sp-offset) &any)
    (set-scan-initial-fields! env)))

(define-ti (static-ref dst offset)
  (let* ((sp-offset (env-sp-offset env))
         (ptr (make-pointer (+ ip (* 4 offset))))
         (ref (dereference-pointer ptr))
         (ty (if (and (zero? (logand (pointer-address ref) 1))
                      (flonum? (pointer->scm ref)))
                 &flonum
                 &scm)))
    (set-inferred-type! env (+ dst sp-offset) ty)))

(define-anf (static-ref dst offset)
  (let* ((ptr (make-pointer (+ ip (* 4 offset))))
         (ref (dereference-pointer ptr))
         (src/l (pointer->scm ref)))
    `(let ((,(dst-ref dst) ,(if (flonum? src/l)
                                src/l
                                (pointer-address ref))))
       ,(next))))

;; XXX: static-set!
;; XXX: static-patch!

(define-ir (load-f64 (f64! dst) (const high-bits) (const low-bits))
  `(let ((,(dst-ref dst) ,(logior (ash high-bits 32) low-bits)))
     ,(next)))

(define-constant (load-u64 high-bits low-bits)
  (logior (ash high-bits 32) low-bits))

;; XXX: load-s64
