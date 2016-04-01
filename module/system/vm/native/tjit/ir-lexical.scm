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
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-scan (mov dst src)
  (let* ((sp-offset (env-sp-offset env))
         (dst+sp (+ dst sp-offset))
         (src+sp (+ src sp-offset))
         (entry (env-entry-types env)))
    ;; Resolving expcting and inferred type for dst and src. There are no SCM
    ;; type clue here, use existing data stored in env. If src could not
    ;; resolved, a tagged `copy' type with local index are stored, to be
    ;; resolved later .
    (unless (or (assq-ref (env-inferred-types env) src+sp)
                (assq-ref (env-entry-types env) src+sp))
      (set-entry-type! env src+sp `(copy . ,dst+sp)))
    (set-scan-initial-fields! env)))

(define-ti (mov dst src)
  (let* ((sp-offset (env-sp-offset env))
         (dst+sp (+ dst sp-offset))
         (src+sp (+ src sp-offset)))
    (let ((type (or (assq-ref (env-inferred-types env) src+sp)
                    (assq-ref (env-entry-types env) src+sp))))
      (if (or (not type)
              (and (pair? type) (eq? 'copy (car type))))
          (set-inferred-type! env dst+sp `(copy . ,src+sp))
          (set-inferred-type! env dst+sp type)))))

(define-anf (mov dst src)
  (let ((dst/i (+ dst (current-sp-offset)))
        (live-indices (env-live-indices env)))
    (unless (memq dst/i live-indices)
      (set-env-live-indices! env (cons dst/i live-indices)))
    `(let ((,(var-ref dst) ,(var-ref src)))
       ,(next))))


;; XXX: long-mov
;; XXX: long-fmov
;; XXX: box

(define-interrupt-ir (box (scm! dst) (scm src))
  (let ((r1 (make-tmpvar 1))
        (r2 (make-tmpvar 2))
        (dst/v (var-ref dst))
        (src/v (var-ref src))
        (src/t (type-ref src)))
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
  (let* ((src/t (type-ref src))
         (vdst (var-ref dst))
         (vsrc (var-ref src))
         (r2 (make-tmpvar 2)))
    (with-boxing src/t vsrc r2
      (lambda (tmp)
        `(let ((_ (%cset ,vdst 1 ,tmp)))
           ,(next))))))

(define-interrupt-ir (make-closure (scm! dst) (const offset) (const nfree))
  (let* ((dst/v (var-ref dst))
         (tag (logior %tc7-program (ash nfree 16)))
         (nwords (+ nfree 2)))
    `(let ((,dst/v (%words ,tag ,nwords)))
       (let ((_ (%cset ,dst/v 1 ,(+ ip (* offset 4)))))
         ,(next)))))

(define-ir (free-ref (scm! dst) (procedure src) (const idx))
  (let* ((dst/v (var-ref dst))
         (src/v (var-ref src)))
    `(let ((,dst/v (%cref ,src/v ,(+ idx 2))))
       ,(next))))

(define-ir (free-set! (procedure dst) (scm src) (const idx))
  (let* ((dst/v (var-ref dst))
         (src/v (var-ref src))
         (src/t (type-ref src))
         (r2 (make-tmpvar 2)))
    (with-boxing src/t src/v r2
      (lambda (tmp)
        `(let ((_ (%cset ,dst/v ,(+ 2 idx) ,tmp)))
           ,(next))))))
