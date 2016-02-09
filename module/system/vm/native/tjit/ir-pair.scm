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
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

;; Using dedicated IR for `cons'. Uses C function `scm_inline_cons', which
;; expects current thread as first argument. The value of current thread is not
;; stored in frame but in non-volatile register.
(define-interrupt-ir (cons (pair! dst) (scm x) (scm y))
  (let* ((vdst (var-ref dst))
         (vx (var-ref x))
         (vy (var-ref y))
         (r1 (make-tmpvar 1))
         (r2 (make-tmpvar 2))
         (lx (local-ref x))
         (ly (local-ref y))
         (emit-cons (lambda (a)
                      (lambda (b)
                        `(let ((,vdst (%cell ,a ,b)))
                           ,(next)))))
         (emit-y (lambda (a)
                   (with-boxing (type-of ly) vy r1
                     (emit-cons a))))
         (emit-x (lambda ()
                   (with-boxing (type-of lx) vx r2
                     emit-y))))
    (emit-x)))

(define-ir (car (scm! dst) (pair src))
  (let ((dst/l (local-ref dst))
        (src/l (local-ref src))
        (dst/v (var-ref dst))
        (src/v (var-ref src)))
    (when (not (pair? src/l))
      (tjitc-error 'car "~a ~a~%" dst/l src/l))
    (let ((car/l (car src/l))
          (r2 (make-tmpvar 2)))
      `(let ((_ ,(take-snapshot! ip 0)))
         (let ((,r2 (%cref ,src/v 0)))
           ,(with-unboxing (type-of car/l) r2
              (lambda ()
                `(let ((,dst/v ,r2))
                   ,(next)))))))))

(define-ir (cdr (scm! dst) (pair src))
  (let ((dst/l (local-ref dst))
        (src/l (local-ref src))
        (dst/v (var-ref dst))
        (src/v (var-ref src)))
    (when (not (pair? src/l))
      (tjitc-error 'cdr "~a ~a~%" dst/l src/l))
    (let ((cdr/l (cdr src/l))
          (r2 (make-tmpvar 2)))
      `(let ((_ ,(take-snapshot! ip 0)))
         (let ((,r2 (%cref ,src/v 1)))
           ,(with-unboxing (type-of cdr/l) r2
              (lambda ()
                `(let ((,dst/v ,r2))
                   ,(next)))))))))

;; XXX: set-car!
;; XXX: set-cdr!
