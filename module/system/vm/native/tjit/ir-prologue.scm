;;; ANF IR for function prologues

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
;;; Module containing ANF IR definitions for function prologue operations.
;;;
;;; Code:

(define-module (system vm native tjit ir-prologue)
  #:use-module (system foreign)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-syntax-rule (expand-stack! nlocals)
  (let ((offset (current-sp-offset)))
    (let lp ((n 0) (acc (env-live-indices env)))
      (if (< n nlocals)
          (lp (+ n 1) (let ((m (- offset n)))
                        (if (memq m acc)
                            acc
                            (cons m acc))))
          (set-env-live-indices! env (sort acc <))))))

(define-syntax-rule (scan-frame nlocals)
  (let* ((stack-size (vector-length locals))
         (diff (- nlocals stack-size)))
    (set-scan-initial-fields! env)
    (push-scan-sp-offset! env diff)))

;; XXX: br-if-nargs-ne
;; (define-ir (br-if-nargs-ne (const expected))
;;   (next))

;; XXX: br-if-nargs-lt
;; (define-ir (br-if-nargs-lt (const expected))
;;   (next))

;; XXX: br-if-nargs-gt
;; (define-ir (br-if-nargs-gt (const expected))
;;   (next))

;;; XXX: Unless this op was found at entry of down recursion, nothing to do.
;;; Detect entry of down recursion, emit assertion in native code.
(define-ir (assert-nargs-ee (const expected))
  (next))

;;; XXX: Same as assert-nargs-ee
(define-ir (assert-nargs-ge (const expected))
  (next))

;;; XXX: Same as assert-nargs-ee
(define-ir (assert-nargs-le (const expected))
  (next))

(define-scan (alloc-frame nlocals)
  (scan-frame nlocals))

(define-ti (alloc-frame nlocals)
  (let* ((stack-size (vector-length locals))
         (diff (- nlocals stack-size))
         (sp-offset (if (env-initialized? env)
                        (env-sp-offset env)
                        (car (env-sp-offsets env)))))
    (when (< 0 diff)
      (do ((n 1 (+ n 1)))
          ((< diff n))
        (set-inferred-type! env (- sp-offset n) &undefined)))))

(define-anf (alloc-frame nlocals)
  (let* ((stack-size (vector-length locals))
         (diff (- nlocals stack-size))
         (undefined (pointer->scm (make-pointer #x904))))
    (if (< 0 diff)
        (begin
          (expand-stack! diff)
          (let lp ((n 1))
            (if (<= n diff)
                `(let ((,(var-ref (- n)) ,undefined))
                   ,(lp (+ n 1)))
                (next))))
        (next))))

(define-scan (reset-frame nlocals)
  (scan-frame nlocals))

(define-ti (reset-frame nlocals)
  (values))

(define-anf (reset-frame nlocals)
  (let ((thunk (gen-load-thunk (- nlocals 2) nlocals (const #f))))
    (thunk)))

;; XXX: push
;; XXX: pop
;; XXX: drop

(define-scan (assert-nargs-ee/locals expected nlocals)
  (set-scan-initial-fields! env)
  (push-scan-sp-offset! env nlocals))

(define-ti (assert-nargs-ee/locals expected nlocals)
  (let ((sp-offset (current-sp-for-ti)))
    (do ((n 1 (+ n 1)))
        ((< nlocals n))
      (set-inferred-type! env (- sp-offset n) &undefined))))

(define-anf (assert-nargs-ee/locals expected nlocals)
  (let ((undefined (pointer->scm (make-pointer #x904))))
    (expand-stack! nlocals)
    (let lp ((n 1))
      (if (<= n nlocals)
          `(let ((,(var-ref (- n)) ,undefined))
             ,(lp (+ n 1)))
          (next)))))

;; XXX: br-if-npos-gt
;; XXX: bind-kw-args

(define-scan (bind-rest dst)
  (let* ((stack-size (vector-length locals))
         (diff (- stack-size (+ dst 1))))
    (set-scan-initial-fields! env)
    (unless (zero? diff)
      (pop-scan-sp-offset! env diff))))

(define-ti (bind-rest dst)
  (let* ((sp-offset (current-sp-for-ti))
         (stack-size (vector-length locals))
         (dst/i (+ (- stack-size dst 1) sp-offset)))
    (set-inferred-type! env dst/i &pair)))

(define-anf (bind-rest dst)
  (let* ((r2 (make-tmpvar 2))
         (stack-size (vector-length locals))
         (dst/i (- stack-size dst 1)))
    (if (< dst/i 0)
        `(let ((,(var-ref dst/i) ()))
           ,(next))
        (let lp ((n 0))
          (if (< dst/i n)
              (next)
              (let ((rest (if (< dst/i (+ n 1))
                              (var-ref dst/i)
                              r2))
                    (tail (if (zero? n)
                              '()
                              r2)))
                `(let ((,rest (%cell ,(var-ref n) ,tail)))
                   ,(lp (+ n 1)))))))))
