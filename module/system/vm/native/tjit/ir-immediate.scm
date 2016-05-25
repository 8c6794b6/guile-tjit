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

(define-scan (make-short-immediate dst low-bits)
  (let ((sp-offset (env-sp-offset env)))
    (set-entry-type! env (+ dst sp-offset) &any)
    (set-scan-initial-fields! env)))

(define-ti (make-short-immediate dst low-bits)
  (let ((sp-offset (env-sp-offset env)))
    (set-inferred-type! env (+ dst sp-offset) (make-constant low-bits))))

(define-anf (make-short-immediate dst low-bits)
  (let ((dst/i+sp (+ dst (current-sp-offset)))
        (live-indices (env-live-indices env)))
    (unless (memq dst/i+sp live-indices)
      (set-env-live-indices! env (cons dst/i+sp live-indices)))
    (next)))

(define-ir (make-long-immediate (scm! dst) (const low-bits))
  `(let ((,(dst-ref dst) ,low-bits))
     ,(next)))

(define-ir (make-long-long-immediate (scm! dst)
                                     (const high-bits)
                                     (const low-bits))
  `(let ((,(dst-ref dst) ,(logior (ash high-bits 32) low-bits)))
     ,(next)))

(define-ir (make-non-immediate (scm! dst) (const offset))
  `(let ((,(dst-ref dst) ,(+ ip (* 4 offset))))
     ,(next)))

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

(define-scan (load-u64 dst high low)
  (let ((sp-offset (env-sp-offset env)))
    (set-entry-type! env (+ dst sp-offset) &any)
    (set-scan-initial-fields! env)))

;; (define-ti (load-u64 dst high low)
;;   (let ((sp-offset (env-sp-offset env))
;;         (v (logior (ash high 32) low)))
;;     (set-inferred-type! env (+ dst sp-offset) &u64)))

;; (define-anf (load-u64 dst high low)
;;   `(let ((,(dst-ref dst) ,(logior (ash high 32) low)))
;;      ,(next)))

(define-ti (load-u64 dst high low)
  (let ((sp-offset (env-sp-offset env))
        (v (logior (ash high 32) low)))
    (set-inferred-type! env (+ dst sp-offset) (make-constant v))))

(define-anf (load-u64 dst high low)
  (let ((dst/i+sp (+ dst (current-sp-offset)))
        (live-indices (env-live-indices env)))
    (unless (memq dst/i+sp live-indices)
      (set-env-live-indices! env (cons dst/i+sp live-indices)))
    (next)))

;; XXX: load-s64
