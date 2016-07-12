;;;; ANF IR for function prologues

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

(define-module (language trace ir-prologue)
  #:use-module (system foreign)
  #:use-module (language trace ir)
  #:use-module (language trace env)
  #:use-module (language trace error)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables))



(define-syntax-rule (expand-stack! nlocals)
  (let ((offset (current-sp-offset)))
    (let lp ((n 0) (acc (env-live-indices env)))
      (if (< n nlocals)
          (lp (+ n 1) (let ((m (- offset n)))
                        (if (and (not (memq m acc))
                                 (or (memq m (env-read-indices env))
                                     (memq m (env-write-indices env))))
                            (cons m acc)
                            acc)))
          (set-env-live-indices! env (sort acc <))))))

(define-syntax-rule (scan-frame nlocals)
  (let* ((stack-size (vector-length locals))
         (diff (- nlocals stack-size)))
    (set-scan-initial-fields! env)
    (push-scan-sp-offset! env diff)))

;;; XXX: Unless br-if-{ne,lt,gt} and assert-nargs-{ee,ge,le} ops were found at
;;; the entry of down recursion, there's nothing to do.
;;;
;;; Detect entry of down recursion, emit assertion in native code.

(define-ir (br-if-nargs-ne (const expected) (const offset))
  (next))

(define-ir (br-if-nargs-lt (const expected) (const offset))
  (next))

(define-ir (br-if-nargs-gt (const expected) (const offset))
  (next))

(define-ir (assert-nargs-ee (const expected))
  (next))

(define-ir (assert-nargs-ge (const expected))
  (next))

(define-ir (assert-nargs-le (const expected))
  (next))

(define-scan (alloc-frame nlocals)
  (scan-frame nlocals))

(define-ti (alloc-frame nlocals)
  (let* ((stack-size (vector-length locals))
         (diff (- nlocals stack-size))
         (sp-offset (current-sp-for-ti)))
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
                `(let ((,(dst-ref (- n)) ,undefined))
                   ,(lp (+ n 1)))
                (next))))
        (next))))

(define-scan (reset-frame nlocals)
  (scan-frame nlocals))

(define-ti (reset-frame nlocals)
  (values))

(define-anf (reset-frame nlocals)
  (let ((thunk (gen-load-thunk (- nlocals 1) nlocals (const #f))))
    (thunk)))

;; XXX: push
;; XXX: pop
;; XXX: drop

(define-scan (assert-nargs-ee/locals expected nlocals)
  (set-scan-initial-fields! env)
  (push-scan-sp-offset! env nlocals))

(define-ti (assert-nargs-ee/locals expected nlocals)
  ;; Fill in allocated locals with #<undefined>.
  (let ((sp-offset (current-sp-for-ti)))
    (do ((n 1 (+ n 1)))
        ((< nlocals n))
      (set-inferred-type! env (- sp-offset n) &undefined))))

(define-anf (assert-nargs-ee/locals expected nlocals)
  (let ((undefined (pointer->scm (make-pointer #x904))))
    (expand-stack! nlocals)
    (next)))

;; XXX: br-if-npos-gt
;; XXX: bind-kw-args

(define-scan (bind-rest dst)
  (let* ((stack-size (vector-length locals))
         (diff (- stack-size (+ dst 1)))
         (sp-offset (env-sp-offset env)))
    (set-scan-initial-fields! env)
    (let lp ((i dst) (types '()))
      (if (< i 0)
          (set-entry-types! env types)
          (lp (- i 1) (cons (cons i &scm) types))))
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
        `(let ((,(dst-ref dst/i) ()))
           ,(next))
        (let lp ((n 0))
          (if (< dst/i n)
              (next)
              (let ((rest (if (< dst/i (+ n 1))
                              (dst-ref dst/i)
                              r2))
                    (tail (if (zero? n)
                              '()
                              r2)))
                (set-env-handle-interrupts! env #t)
                `(let ((,rest (%cell ,(src-ref n) ,tail)))
                   ,(lp (+ n 1)))))))))
