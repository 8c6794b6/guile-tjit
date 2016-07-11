;;;; ANF IR for lexical binding operations

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

(define-module (language trace ir-lexical)
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (language trace error)
  #:use-module (language trace ir)
  #:use-module (language trace env)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables))



(define-scan (mov dst src)
  (let* ((sp-offset (env-sp-offset env))
         (dst+sp (+ dst sp-offset))
         (src+sp (+ src sp-offset))
         (entry (env-entry-types env)))
    ;; Resolving expcting and inferred type for dst and src. There are no SCM
    ;; type clue here, use existing data stored in env. If src could not be
    ;; resolved, a tagged `copy' type with local index are stored, to resolve
    ;; later.
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
  (let* ((dst/i (+ dst (current-sp-offset)))
         (src/v (src-ref src))
         (dst/v (dst-ref dst))
         (live-indices (env-live-indices env)))
    (unless (memq dst/i live-indices)
      (set-env-live-indices! env (cons dst/i live-indices)))
    `(let ((,dst/v ,src/v))
       ,(next))))

;; XXX: long-mov
;; XXX: long-fmov
;; XXX: box

(define-interrupt-ir (box (scm! dst) (scm src))
  (let* ((r1 (make-tmpvar 1))
         (r2 (make-tmpvar 2))
         (src/v (src-ref src))
         (dst/v (dst-ref dst))
         (src/t (type-ref src)))
    (with-boxing src/t src/v r1
      (lambda (boxed)
        `(let ((,dst/v (%cell ,%tc7-variable ,boxed)))
           ,(next))))))

;; XXX: Reconsider how to manage `box', `box-ref', and `box-set!'.
;; Boxing back tagged value every time will make the loop slow, need
;; more analysis when the storing could be removed from native code loop
;; and delayed to side exit code.

(define-ir (box-ref (scm! dst) (box src))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst)))
    `(let ((,dst/v (%cref ,src/v 1)))
       ,(next))))

(define-ir (box-set! (box dst) (scm src))
  (let* ((src/t (type-ref src))
         (src/v (src-ref src))
         (dst/v (src-ref dst))
         (r1 (make-tmpvar 1)))
    (with-boxing src/t src/v r1
      (lambda (tmp)
        `(let ((_ (%cset ,dst/v 1 ,tmp)))
           ,(next))))))

(define-interrupt-ir (make-closure (procedure! dst) (const offset)
                                   (const nfree))
  (let* ((dst/v (dst-ref dst))
         (tag (logior %tc7-program (ash nfree 16)))
         (nwords (+ nfree 2)))
    `(let ((,dst/v (%words ,tag ,nwords)))
       (let ((_ (%cset ,dst/v 1 ,(+ ip (* offset 4)))))
         ,(next)))))

(define-ir (free-ref (scm! dst) (procedure src) (const idx))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst)))
    `(let ((,dst/v (%cref ,src/v ,(+ idx 2))))
       ,(next))))

(define-ir (free-set! (procedure dst) (scm src) (const idx))
  (let* ((src/v (src-ref src))
         (dst/v (src-ref dst))
         (src/t (type-ref src))
         (r1 (make-tmpvar 1)))
    (with-boxing src/t src/v r1
      (lambda (tmp)
        `(let ((_ (%cset ,dst/v ,(+ 2 idx) ,tmp)))
           ,(next))))))
