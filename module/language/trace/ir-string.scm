;;; ANF IR for strings, symbols, and keywords

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
;;; Module containing ANF IR definitions for strings, symbols, and keywords.
;;;
;;; Code:

(define-module (language trace ir-string)
  #:use-module (system foreign)
  #:use-module (language trace error)
  #:use-module (language trace ir)
  #:use-module (language trace env)
  #:use-module (language trace parameters)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables))

(define-ir (string-length (u64! dst) (string src))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst)))
    (with-type-guard &string src
      `(let ((,dst/v (%cref ,src/v 3)))
         ,(next)))))

;; XXX: Inline with `scm_i_string_ref'.
(define-ir (string-ref (char! dst) (string src) (u64 idx))
  (let* ((src/v (src-ref src))
         (idx/v (src-ref idx))
         (dst/v (dst-ref dst))
         (r2 (make-tmpvar 2)))
    (with-type-guard &string src
      `(let ((_ (%carg ,idx/v)))
         (let ((_ (%carg ,src/v)))
           (let ((,dst/v (%ccall ,(object-address scm-do-i-string-ref))))
             ,(next)))))))

(define-ir (string->number (scm! dst) (string src))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst)))
    (with-type-guard &string src
      `(let ((_ (%carg #x904)))
         (let ((_ (%carg ,src/v)))
           (let ((,dst/v (%ccall ,(object-address string->number))))
             ,(next)))))))

(define-ir (string->symbol (scm! dst) (string src))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst)))
    (with-type-guard &string src
      `(let ((_ (%carg ,src/v)))
         (let ((,dst/v (%ccall ,(object-address string->symbol))))
           ,(next))))))

(define-ir (symbol->keyword (scm! dst) (scm src))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst)))
    `(let ((_ (%carg ,src/v)))
       (let ((,dst/v (%ccall ,(object-address symbol->keyword))))
         ,(next)))))
