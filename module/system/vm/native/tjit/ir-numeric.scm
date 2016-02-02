;;; ANF IR for numeric operations

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
;;; Module containing ANF IR definitions for numeric operations.
;;;
;;; Code:

(define-module (system vm native tjit ir-numeric)
  #:use-module (system foreign)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-syntax define-binary-arith-scm-scm
  (syntax-rules ()
    ((_ name op-fx1 op-fx2 op-fl)
     (define-ir (name (scm! dst) (scm a) (scm b))
       (let ((a/l (local-ref a))
             (b/l (local-ref b))
             (dst/v (var-ref dst))
             (a/v (var-ref a))
             (b/v (var-ref b)))
         (cond
          ((and (fixnum? a/l) (fixnum? b/l))
           `(let ((,dst/v (op-fx1 ,a/v ,b/v)))
              (let ((,dst/v (op-fx2 ,dst/v 2)))
                ,(next))))
          ((and (fixnum? a/l) (flonum? b/l))
           (let ((r0 (make-tmpvar 0))
                 (f0 (make-tmpvar/f 0)))
             `(let ((,r0 (%rsh ,a/v 2)))
                (let ((,f0 (%i2d ,r0)))
                  (let ((,dst/v (op-fl ,f0 ,b/v)))
                    ,(next))))))
          ((and (flonum? a/l) (flonum? b/l))
           `(let ((,dst/v (op-fl ,a/v ,b/v)))
              ,(next)))
          (else
           (nyi "~s: ~a ~a ~a" 'name (local-ref dst) a/l b/l))))))))

(define-syntax define-binary-arith-scm-imm
  (syntax-rules ()
    ((_ name op-fx)
     (define-ir (name (scm! dst) (scm src) (const imm))
       (let ((src/l (local-ref src))
             (dst/v (var-ref dst))
             (src/v (var-ref src)))
         (cond
          ((fixnum? src/l)
           `(let ((,dst/v (op-fx ,src/v ,(* imm 4))))
              ,(next)))
          (else
           (nyi "~s: ~a ~a" 'name (local-ref dst) src/l))))))))

(define-binary-arith-scm-scm add %add %sub %fadd)
(define-binary-arith-scm-imm add/immediate %add)
(define-binary-arith-scm-scm sub %sub %add %fsub)
(define-binary-arith-scm-imm sub/immediate %sub)

(define-ir (mul (scm! dst) (scm a) (scm b))
  (let ((a/l (local-ref a))
        (b/l (local-ref b))
        (dst/v (var-ref dst))
        (a/v (var-ref a))
        (b/v (var-ref b)))
    (cond
     ((and (flonum? a/l) (flonum? b/l))
      `(let ((,dst/v (%fmul ,a/v ,b/v)))
         ,(next)))
     ((and (flonum? a/l) (fixnum? b/l))
      (let* ((r2 (make-tmpvar 2))
             (f2 (make-tmpvar/f 2))
             (emit-next (lambda ()
                          `(let ((,r2 (%rsh ,b/v 2)))
                             (let ((,f2 (%i2d ,r2)))
                               (let ((,dst/v (%fmul ,a/v ,f2)))
                                 ,(next)))))))
        (with-unboxing &exact-integer b/v emit-next)))
     (else
      (nyi "mul: ~a ~a ~a" (local-ref dst) a/l b/l)))))

(define-ir (div (scm! dst) (scm a) (scm b))
  (let ((a/l (local-ref a))
        (b/l (local-ref b))
        (dst/v (var-ref dst))
        (a/v (var-ref a))
        (b/v (var-ref b)))
    (cond
     ((and (flonum? a/l) (flonum? b/l))
      `(let ((,dst/v (%fdiv ,a/v ,b/v)))
         ,(next)))
     ((and (fixnum? a/l) (flonum? b/l))
      (let* ((r2 (make-tmpvar 2))
             (f2 (make-tmpvar/f 2))
             (emit-next (lambda ()
                          `(let ((,r2 (%rsh ,a/v 2)))
                             (let ((,f2 (%i2d ,r2)))
                               (let ((,dst/v (%fdiv ,f2 ,b/v)))
                                 ,(next)))))))
        (with-unboxing &exact-integer a/v emit-next)))
     (else
      (nyi "div: ~a ~a ~a" (local-ref dst) a/l b/l)))))

;; XXX: quo
;; XXX: rem

(define-ir (mod (scm! dst) (scm a) (scm b))
  (let ((a/l (local-ref a))
        (b/l (local-ref b))
        (dst/v (var-ref dst))
        (a/v (var-ref a))
        (b/v (var-ref b)))
    (cond
     ((and (fixnum? a/l) (fixnum? b/l))
      (let ((r2 (make-tmpvar 2)))
        `(let ((,r2 (%rsh ,a/v 2)))
           (let ((,dst/v (%rsh ,b/v 2)))
             (let ((,dst/v (%mod ,r2 ,dst/v)))
               (let ((,dst/v (%lsh ,dst/v 2)))
                 (let ((,dst/v (%add ,dst/v 2)))
                   ,(next))))))))
     (else
      (nyi "mod: ~a ~a ~a" (local-ref dst) a/l b/l)))))

;; XXX: ash
;; XXX: logand
;; XXX: logior
;; XXX: logxor
;; XXX: make-vector
;; XXX: make-vector/immediate

(define-ir (vector-length (u64! dst) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%cref ,src/v 0)))
       (let ((,dst/v (%rsh ,dst/v 8)))
         ,(next)))))

(define-ir (vector-ref (scm! dst) (scm src) (u64 idx))
  (let ((dst/v (var-ref dst))
        (dst/l (let ((src/l (local-ref src))
                     (idx/l (local-ref idx)))
                 (if (vector? src/l)
                     (vector-ref src/l idx/l)
                     (tjitc-error 'vector-ref "not a vector ~s" src/l))))
        (src/v (var-ref src))
        (idx/v (var-ref idx))
        (r2 (make-tmpvar 2)))
    (vector-set! locals dst (scm->pointer dst/l))
    `(let ((,r2 (%add ,idx/v 1)))
       (let ((,r2 (%cref ,src/v ,r2)))
         ,(with-unboxing (type-of dst/l) r2
            (lambda ()
              `(let ((,dst/v ,r2))
                 ,(next))))))))

(define-ir (vector-ref/immediate (scm! dst) (scm src) (const idx))
  (let ((dst/v (var-ref dst))
        (dst/l (let ((src/l (local-ref src)))
                 (if (vector? src/l)
                     (vector-ref src/l idx)
                     (tjitc-error 'vector-ref "not a vector ~s" src/l))))
        (src/v (var-ref src))
        (r2 (make-tmpvar 2)))
    `(let ((,r2 (%cref ,src/v ,(+ idx 1))))
       ,(with-unboxing (type-of dst/l) r2
          (lambda ()
            `(let ((,dst/v ,r2))
               ,(next)))))))

(define-ir (vector-set! (scm dst) (u64 idx) (scm src))
  (let ((dst/v (var-ref dst))
        (idx/v (var-ref idx))
        (src/v (var-ref src))
        (src/l (local-ref src))
        (r1 (make-tmpvar 1))
        (r2 (make-tmpvar 2)))
    (with-boxing (type-of src/l) src/v r2
      (lambda (boxed)
        `(let ((,r1 (%add ,idx/v 1)))
           (let ((_ (%cset ,dst/v ,r1 ,boxed)))
             ,(next)))))))

(define-ir (vector-set!/immediate (scm dst) (const idx) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src))
        (src/l (local-ref src))
        (r2 (make-tmpvar 2)))
    (with-boxing (type-of src/l) src/v r2
      (lambda (boxed)
        `(let ((_ (%cset ,dst/v ,(+ idx 1) ,boxed)))
           ,(next))))))

(define-syntax define-binary-arith-f64-f64
  (syntax-rules ()
    ((_ name op)
     (define-ir (name (f64! dst) (f64 a) (f64 b))
       `(let ((,(var-ref dst) (op ,(var-ref a) ,(var-ref b))))
          ,(next))))))

(define-binary-arith-f64-f64 fadd %fadd)
(define-binary-arith-f64-f64 fsub %fsub)
(define-binary-arith-f64-f64 fmul %fmul)
(define-binary-arith-f64-f64 fdiv %fdiv)

(define-syntax define-binary-arith-u64-imm
  (syntax-rules ()
    ((_ name op)
     (define-ir (name (u64! dst) (u64 src) (const imm))
       `(let ((,(var-ref dst) (op ,(var-ref src) ,imm)))
          ,(next))))))

(define-binary-arith-u64-imm uadd/immediate %add)
(define-binary-arith-u64-imm usub/immediate %sub)

;; XXX: uadd
;; XXX: usub
;; XXX: umul
;; XXX: umul/immediate

;; XXX: ulogand
;; XXX: ulogior
;; XXX: ulogsub

;; XXX: ursh
;; XXX: ulsh
;; XXX: ursh/immediate
;; XXX: ulsh/immediate
