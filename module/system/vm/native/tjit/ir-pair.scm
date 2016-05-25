;;; ANF IR for pair

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
;;; Module containing ANF IR definitions for pair operations.
;;;
;;; Code:

(define-module (system vm native tjit ir-pair)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

;; Using dedicated IR for `cons'. Uses C function `scm_inline_cell', which
;; expects current thread as first argument. The value of current thread is not
;; stored in frame but in register.
;;
;; When both `x' and `y' are unboxed value, using spilled offset to temporary
;; hold the boxed result of x, since subsequent C function calls could overwrite
;; the contents of x when the boxed value of x were stored in register.
(define-interrupt-ir (cons (pair! dst) (scm x) (scm y))
  (let* ((x/t (type-ref x))
         (y/t (type-ref y))
         (x/v (src-ref x))
         (y/v (src-ref y))
         (dst/v (var-ref dst))
         (r2 (make-tmpvar 2))
         (m0 (make-spill 0)))
    (if (and (eq? &flonum x/t) (eq? &flonum y/t))
        `(let ((,m0 (%d2s ,x/v)))
           (let ((,r2 (%d2s ,y/v)))
             (let ((,dst/v (%cell ,m0 ,r2)))
               ,(next))))
        (with-boxing (type-ref x) x/v r2
          (lambda (x/boxed)
            (with-boxing (type-ref y) y/v r2
              (lambda (y/boxed)
                `(let ((,dst/v (%cell ,x/boxed ,y/boxed)))
                   ,(next)))))))))

(define-ir (car (scm! dst) (pair src))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst)))
    (with-type-guard &pair src
      `(let ((,dst/v (%cref ,src/v 0)))
         ,(next)))))

(define-ir (cdr (scm! dst) (pair src))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst)))
    (with-type-guard &pair src
      `(let ((,dst/v (%cref ,src/v 1)))
         ,(next)))))

(define-ir (set-car! (pair dst) (scm src))
  (let ((src/v (src-ref src))
        (dst/v (src-ref dst)))
    (with-type-guard &pair dst
      `(let ((_ (%cset ,dst/v 0 ,src/v)))
         ,(next)))))

(define-ir (set-cdr! (pair dst) (scm src))
  (let ((src/v (src-ref src))
        (dst/v (src-ref dst)))
    (with-type-guard &pair dst
      `(let ((_ (%cset ,dst/v 1 ,src/v)))
         ,(next)))))
