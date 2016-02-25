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
  #:use-module (system vm native tjit state)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-scan (subr-call)
  (let* ((stack-size (vector-length locals))
         (proc-offset (- stack-size 1))
         (ra-offset stack-size)
         (dl-offset (+ ra-offset 1)))
    (set-scan-initial-fields! outline)
    (pop-scan-sp-offset! outline (- stack-size 2))
    (pop-scan-fp-offset! outline dl)))

;;; XXX: Multiple values return not yet implemented.
(define-ti (subr-call)
  (let* ((stack-size (vector-length locals))
         (sp-offset (if (outline-initialized? outline)
                        (outline-sp-offset outline)
                        (car (outline-sp-offsets outline))))
         (proc-offset (+ (- stack-size 1) sp-offset))
         (ra-offset (+ proc-offset 1))
         (dl-offset (+ ra-offset 1)))
    (debug 2 ";;; [ti] subr-call proc-offset=~s~%" proc-offset)
    (set-inferred-type! outline ra-offset &false)
    (set-inferred-type! outline dl-offset &false)

    ;; Returned value from C function is stored in (- proc-offset 1). The stack
    ;; item type of the value is always `scm'.
    (set-inferred-type! outline (- proc-offset 1) &scm)))

(define-anf (subr-call)
  (let* ((stack-size (vector-length locals))
         (dst/v (var-ref (- stack-size 2)))
         (subr/l (scm-ref (- stack-size 1)))
         (ccode (and (program? subr/l)
                     (program-code subr/l)))
         (ra/v (var-ref stack-size))
         (dl/v (var-ref (+ stack-size 1)))
         (proc-addr (pointer-address (scm->pointer subr/l)))
         (inlineable (<= (+ (current-sp-offset) dl) (ir-max-sp-offset ir)))
         (emit-next
          (lambda ()
            `(let ((,ra/v #f))
               (let ((,dl/v #f))
                 ,(next)))))
         (emit-ccall
          (lambda ()
            `(let ((,dst/v (%ccall ,proc-addr)))
               ;; XXX: Any other way to decide emitting `%return' than SP
               ;; offset comparison?
               ,(if inlineable
                    (emit-next)
                    `(let ((_ ,(take-snapshot! ip 0)))
                       (let ((_ (%return ,ra)))
                         ,(emit-next))))))))
    (set-ir-return-subr! ir #t)
    (set-tj-handle-interrupts! tj #t)
    (if (primitive-code? ccode)
        (let lp ((n 0))
          (if (< n (- stack-size 1))
              (let ((n/v (var-ref n))
                    (n/l (scm-ref n)))
                (with-boxing (type-of n/l) n/v n/v
                  (lambda (boxed)
                    `(let ((_ (%carg ,boxed)))
                       ,(lp (+ n 1))))))
              (emit-ccall)))
        (tjitc-error 'subr-call "not a primitive ~s" subr/l))))

;; XXX: foreign-call
;; XXX: continuation-call
;; XXX: compose-continuation
;; XXX: tail-apply
;; XXX: call/cc
;; XXX: abort
;; XXX: builtin-ref
