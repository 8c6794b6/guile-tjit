;;; ANF IR for lexical binding operations

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
;;; Module containing ANF IR definitions for lexical binding operations.
;;;
;;; Code:

(define-module (system vm native tjit ir-lexical)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables))

;; XXX: Assuming both `dst' and `src' have `scm' stack element type. If not,
;; stack element type resolution may return incorrect result. To properly
;; resolve stack element types, may need to traverse bytecode operations
;; backward.
(define-ir (mov dst src)
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

;; XXX: long-mov
;; XXX: long-fmov
;; XXX: box

(define-interrupt-ir (box (scm! dst) (scm src))
  (let ((r1 (make-tmpvar 1))
        (r2 (make-tmpvar 2))
        (dst/v (var-ref dst))
        (src/v (var-ref src))
        (src/l (local-ref src)))
    `(let ((,r2 ,%tc7-variable))
       ,(with-boxing (type-of src/l) src/v r1
          (lambda (src/v)
            `(let ((,dst/v (%cell ,r2 ,src/v)))
               ,(next)))))))

;; XXX: Reconsider how to manage `box', `box-ref', and `box-set!'.
;; Boxing back tagged value every time will make the loop slow, need
;; more analysis when the storing could be removed from native code loop
;; and delayed to side exit code.
;;
;; XXX: Add test for nested boxes.
;; XXX: Add test for box contents not being other type than scm (no u64, no f64).

(define-ir (box-ref (scm! dst) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src))
        (src/l (and (< src (vector-length locals))
                   (let ((var (local-ref src)))
                     (if (variable? var)
                         (variable-ref var)
                         (tjitc-error 'box-ref "got ~s" var)))))
        (r2 (make-tmpvar 2)))
    (vector-set! locals dst (scm->pointer src/l))
    `(let ((,r2 (%cref ,src/v 1)))
       ,(with-unboxing (type-of src/l) r2
          (lambda ()
            `(let ((,dst/v ,r2))
               ,(next)))))))

(define-ir (box-set! (scm dst) (scm src))
  (let* ((vdst (var-ref dst))
         (vsrc (var-ref src))
         (rdst (and (< dst (vector-length locals))
                    (let ((var (local-ref dst)))
                      (if (variable? var)
                          (variable-ref var)
                          (tjitc-error 'box-set! "got ~s~%" var)))))
         (r2 (make-tmpvar 2))
         (emit-next (lambda (tmp)
                      `(let ((_ (%cset ,vdst 1 ,tmp)))
                         ,(next)))))
    (with-boxing (type-of rdst) vsrc r2
      emit-next)))

;; XXX: make-closure

(define-ir (free-ref (scm! dst) (scm src) (const idx))
  (let* ((dst/v (var-ref dst))
         (src/v (var-ref src))
         (src/l (local-ref src))
         (ref/l (and (program? src/l)
                     (program-free-variable-ref src/l idx)))
         (r2 (make-tmpvar 2)))
    `(let ((,r2 (%add ,src/v ,(* 2 %word-size))))
       (let ((,r2 (%cref ,r2 ,idx)))
         ,(with-unboxing (type-of ref/l) r2
            (lambda ()
              `(let ((,dst/v ,r2))
                 ,(next))))))))

;; XXX: free-set!
