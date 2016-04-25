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

(define-module (system vm native tjit ir-string)
  #:use-module (system foreign)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-syntax-rule (with-string-guard x x/v expr)
  (if (eq? &string (type-ref x))
      expr
      (with-type-guard &string x/v expr)))

(define-ir (string-length (u64! dst) (string src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    (with-string-guard src src/v
      `(let ((,dst/v (%cref ,src/v 3)))
         ,(next)))))

;; XXX: Inline with `scm_i_string_ref'.
(define-ir (string-ref (char! dst) (string src) (u64 idx))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src))
        (idx/v (var-ref idx))
        (r2 (make-tmpvar 2)))
    (with-string-guard src src/v
      `(let ((,r2 (%lsh ,idx/v 2)))
         (let ((,r2 (%add ,r2 2)))
           (let ((_ (%carg ,r2)))
             (let ((_ (%carg ,src/v)))
               (let ((,dst/v (%ccall ,(object-address string-ref))))
                 ,(next)))))))))

(define-ir (string->number (scm! dst) (string src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    (with-string-guard src src/v
      `(let ((_ (%carg #x904)))
         (let ((_ (%carg ,src/v)))
           (let ((,dst/v (%ccall ,(object-address string->number))))
             ,(next)))))))

(define-ir (string->symbol (scm! dst) (string src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    (with-string-guard src src/v
      `(let ((_ (%carg ,src/v)))
         (let ((,dst/v (%ccall ,(object-address string->symbol))))
           ,(next))))))

(define-ir (symbol->keyword (scm! dst) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((_ (%carg ,src/v)))
       (let ((,dst/v (%ccall ,(object-address symbol->keyword))))
         ,(next)))))
