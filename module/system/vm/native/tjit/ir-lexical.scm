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
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-anf (mov dst src)
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

(define-scan (mov ol dst src)
  (let* ((sp-offset (outline-sp-offset ol))
         (dst+sp (+ dst sp-offset))
         (src+sp (+ src sp-offset))
         (entry (outline-entry-types ol)))
    (set-scan-read! ol src)
    (set-scan-write! ol dst)

    ;; Resolving expcting and inferred type for dst and src. There are no SCM
    ;; type clue here, use existing data stored in outline. If src could not
    ;; resolved, a tagged `copy' type with local index are stored, to be
    ;; resolved later .
    (unless (or (assq-ref (outline-inferred-types ol) src+sp)
                (assq-ref (outline-entry-types ol) src+sp))
      (set-entry-type! ol src+sp `(copy . ,dst+sp)))
    (unless (or (assq-ref (outline-inferred-types ol) src+sp)
                (assq-ref (outline-expected-types ol) src+sp))
      (set-expected-type! ol src+sp `(copy . ,dst+sp)))

    (set-scan-initial-fields! ol)))

(define-ti (mov ol dst src)
  (let* ((sp-offset (outline-sp-offset ol))
         (dst+sp (+ dst sp-offset))
         (src+sp (+ src sp-offset)))
    (cond
     ((or (assq-ref (outline-inferred-types ol) src+sp)
          (assq-ref (outline-expected-types ol) src+sp))
      => (lambda (ty)
           (set-inferred-type! ol dst+sp ty)))
     (else
      (set-inferred-type! ol dst+sp `(copy . ,src+sp))))))


;; XXX: long-mov
;; XXX: long-fmov
;; XXX: box

(define-interrupt-ir (box (scm! dst) (scm src))
  (let ((r1 (make-tmpvar 1))
        (r2 (make-tmpvar 2))
        (dst/v (var-ref dst))
        (src/v (var-ref src))
        (src/t (ty-ref src)))
    (if (eq? &flonum src/t)
        `(let ((,r2 ,%tc7-variable))
           ,(with-boxing src/t src/v r1
              (lambda (src/v)
                `(let ((,dst/v (%cell ,r2 ,src/v)))
                   ,(next)))))
        `(let ((,r2 ,%tc7-variable))
           (let ((,dst/v (%cell ,r2 ,src/v)))
             ,(next))))))

;; XXX: Reconsider how to manage `box', `box-ref', and `box-set!'.
;; Boxing back tagged value every time will make the loop slow, need
;; more analysis when the storing could be removed from native code loop
;; and delayed to side exit code.
;;
;; XXX: Add test for nested boxes.
;; XXX: Add test for box contents not being other type than scm (no u64, no f64).

(define-ir (box-ref (scm! dst) (box src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%cref ,src/v 1)))
       ,(next))))

(define-ir (box-set! (box dst) (scm src))
  (let* ((src/t (ty-ref src))
         (vdst (var-ref dst))
         (vsrc (var-ref src)))
    (debug 1 ";;; [IR] box-set! src/t=~a~%" (pretty-type src/t))
    (if (eq? &flonum src/t)
        (let ((r2 (make-tmpvar 2)))
          (with-boxing src/t vsrc r2
            (lambda (tmp)
              `(let ((_ (%cset ,vdst 1 ,tmp)))
                 ,(next)))))
        `(let ((_ (%cset ,vdst 1 ,vsrc)))
           ,(next)))))

;; XXX: make-closure

;; XXX: Remove `with-unboxing'.
(define-ir (free-ref (scm! dst) (scm src) (const idx))
  (let* ((dst/v (var-ref dst))
         (src/v (var-ref src))
         (src/l (scm-ref src))
         (ref/l (and (program? src/l)
                     (program-free-variable-ref src/l idx)))
         (r2 (make-tmpvar 2)))
    `(let ((,r2 (%add ,src/v ,(* 2 %word-size))))
       (let ((,dst/v (%cref ,r2 ,idx)))
         ,(next)))))

;; XXX: free-set!
