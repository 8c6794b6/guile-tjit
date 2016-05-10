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
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-scan (subr-call)
  (let* ((stack-size (vector-length locals))
         (proc-offset (- stack-size 1))
         (ra-offset stack-size)
         (dl-offset (+ ra-offset 1))
         (sp-offset (env-sp-offset env)))
    (do ((n 0 (+ n 1)))
        ((= n proc-offset))
      (set-entry-type! env (+ n sp-offset) &scm))
    (set-scan-initial-fields! env)
    (add-env-return! env)
    (pop-scan-sp-offset! env (- stack-size 2))
    (pop-scan-fp-offset! env dl)))

;;; XXX: Multiple values return not yet implemented.
(define-ti (subr-call)
  ;; Filling in return address and dynamic link with false.
  (let* ((stack-size (vector-length locals))
         (sp-offset (if (env-initialized? env)
                        (env-sp-offset env)
                        (car (env-sp-offsets env))))
         (proc-offset (+ (- stack-size 1) sp-offset))
         (ra-offset (+ proc-offset 1))
         (dl-offset (+ ra-offset 1)))
    (debug 2 ";;; [ti] subr-call proc-offset=~s~%" proc-offset)
    (set-inferred-type! env ra-offset &false)
    (set-inferred-type! env dl-offset &false)

    ;; Returned value from C function is stored in (- proc-offset 1). The stack
    ;; item type of the returned value is always `scm'.
    (set-inferred-type! env (- proc-offset 1) &scm)))

;; XXX: Inline known subroutines.
;;
;; For instance, C function `scm_cons' could be replaced with `%cell'
;; primitive. Compare the program code at runtime, add guard, and replace with
;; inlined alternative.
(define-anf (subr-call)
  (let* ((stack-size (vector-length locals))
         (dst/v (var-ref (- stack-size 2)))
         (subr/l (scm-ref (- stack-size 1)))
         (ccode (and (program? subr/l)
                     (program-code subr/l)))
         (ra/v (var-ref stack-size))
         (dl/v (var-ref (+ stack-size 1)))
         (proc-addr (object-address subr/l))
         (emit-ccall
          (lambda ()
            (if (inline-current-return?)
                `(let ((,dst/v (%ccall ,proc-addr)))
                   ,(next))
                `(let ((_ ,(take-snapshot! ip 0)))
                   (let ((_ (%return ,ra)))
                     (let ((,dst/v (%ccall ,proc-addr)))
                       ,(next))))))))
    (set-env-handle-interrupts! env #t)
    (if (primitive-code? ccode)
        (let lp ((n 0))
          (if (< n (- stack-size 1))
              (let ((n/v (var-ref n))
                    (n/l (scm-ref n))
                    (r1 (make-tmpvar 2)))
                (with-boxing (type-ref n) n/v r1
                  (lambda (boxed)
                    `(let ((_ (%carg ,boxed)))
                       ,(lp (+ n 1))))))
              (emit-ccall)))
        (failure 'subr-call "not a primitive ~s" subr/l))))

;; XXX: foreign-call
;; XXX: continuation-call
;; XXX: compose-continuation
;; XXX: tail-apply

;; XXX: call/cc
;; (define-ir (call/cc)
;;   (let ((cont/v (var-ref 0))
;;         (dst/v (var-ref 1))
;;         (cont/l (scm-ref 0))
;;         (r2 (make-tmpvar 2)))
;;     (debug 0 ";;; [IR] call/cc, cont/l=~a program-code=~x~%"
;;            cont/l (program-code cont/l))
;;     `(let ((_ ,(take-snapshot! ip 0)))
;;        (let ((,r2 (%cref ,cont/v 1)))
;;          (let ((_ (%eq ,r2 ,(program-code cont/l))))
;;            (let ((,r2 (%cont)))
;;              (let ((_ (%ne ,r2 #x904)))
;;                (let ((,dst/v ,cont/v))
;;                  (let ((,cont/v ,r2))
;;                    ,(next))))))))))

;; XXX: abort

(define-ir (builtin-ref (scm! dst) (const idx))
  (let ((ref (case idx
               ((0) apply)
               ((1) values)
               ((2) abort-to-prompt)
               ((3) call-with-values)
               ((4) call-with-current-continuation)
               (else
                (failure 'builtin-ref "unknown builtin ~a" idx)))))
    `(let ((,(var-ref dst) ,(object-address ref)))
       ,(next))))
