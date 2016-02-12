;;; ANF IR for specialized call stubs

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
;;; Module containing ANF IR definitions for specialized call stub operations.
;;;
;;; Code:

(define-module (system vm native tjit ir-specialized)
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

;; Helper to get type from runtime value returned from external functions, i.e.:
;; `subr-call' and `foreign-call'.
(define-syntax-rule (current-ret-type)
  (let ((idx (+ (ir-bytecode-index ir) 1))
        (ret-types (outline-ret-types (ir-outline ir))))
    (if (<= 0 idx (- (vector-length ret-types) 1))
        (vector-ref ret-types idx)
        (tjitc-error 'current-ret-type "index ~s out of range ~s"
                     idx ret-types))))

(define-interrupt-ir (subr-call)
  (let* ((stack-size (vector-length locals))
         (dst/v (var-ref (- stack-size 2)))
         (subr/l (local-ref (- stack-size 1)))
         (ccode (and (program? subr/l)
                     (program-code subr/l)))
         (ret-type (current-ret-type))
         (ra/v (var-ref stack-size))
         (dl/v (var-ref (+ stack-size 1)))
         (r2 (make-tmpvar 2))
         (proc-addr (pointer-address (scm->pointer subr/l)))
         (emit-next
          (lambda ()
            `(let ((,ra/v #f))
               (let ((,dl/v #f))
                 ,(next)))))
         (emit-pop
          (lambda ()
            (pop-outline! (ir-outline ir) (current-sp-offset) locals)
            ;; XXX: Any other way to decide emitting `%return' than SP offset
            ;; comparison?
            (if (< (ir-max-sp-offset ir) (+ (current-sp-offset) dl))
                `(let ((_ ,(take-snapshot! ip 0)))
                   (let ((_ (%return ,ra)))
                     ,(emit-next)))
                (emit-next))))
         (emit-ccall
          (lambda ()
            `(let ((,r2 (%ccall ,proc-addr)))
               ,(if ret-type
                    (with-unboxing ret-type r2 r2
                      (lambda ()
                        `(let ((,dst/v ,r2))
                           ,(emit-pop))))
                    (emit-pop))))))
    (debug 1 ";;; subr-call: (~a) (~s ~{~a~^ ~})~%"
           (pretty-type (current-ret-type))
           (procedure-name subr/l)
           (let lp ((n 0) (acc '()))
             (if (< n (- stack-size 1))
                 (begin
                   (let ((arg (local-ref n)))
                     (lp (+ n 1)
                         (cons (pretty-type (type-of arg)) acc))))
                 acc)))
    (set-ir-return-subr! ir #t)
    (if (primitive-code? ccode)
        (let lp ((n 0))
          (if (< n (- stack-size 1))
              (let ((n/v (var-ref n))
                    (n/l (local-ref n)))
                (with-boxing (type-of n/l) n/v n/v
                  (lambda (boxed)
                    `(let ((_ (%carg ,boxed)))
                       ,(lp (+ n 1))))))
              (emit-ccall)))
        (tjitc-error 'subr-call "not a primitive ~s" subr/l))))

(define-scan (subr-call ol)
  (let* ((stack-size (vector-length locals))
         (proc-offset (- stack-size 1))
         (ra-offset stack-size)
         (dl-offset (+ ra-offset 1))
         (initialized (outline-initialized? ol)))
    (set-scan-scm! ol stack-size (+ stack-size 1))
    (when (outline-infer-type? ol)
      (set-expected-type! ol proc-offset &procedure)
      (set-inferred-type! ol ra-offset &false)
      (set-inferred-type! ol dl-offset &false))
    (unless initialized
      (set-scan-write! ol stack-size (+ stack-size 1))
      (set-entry-type! ol proc-offset &procedure)
      (let ((new-offsets (cons (outline-sp-offset ol)
                               (outline-sp-offsets ol))))
        (set-outline-sp-offsets! ol new-offsets)))
    (pop-scan-sp-offset! ol (- stack-size 2))
    (unless initialized
      (let ((new-offsets (cons (outline-fp-offset ol)
                               (outline-fp-offsets ol)))
            (writes (outline-write-indices ol))
            (buf (outline-write-buf ol)))
        (set-outline-fp-offsets! ol new-offsets)
        (set-outline-write-buf! ol (cons writes buf))))
    (pop-scan-fp-offset! ol dl)))

;; XXX: foreign-call
;; XXX: continuation-call
;; XXX: compose-continuation
;; XXX: tail-apply
;; XXX: call/cc
;; XXX: abort
;; XXX: builtin-ref
