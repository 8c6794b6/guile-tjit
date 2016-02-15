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
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))


;; Definition order matters, define general types first, then follows
;; more specific types.
(define-syntax define-binary-arith-scm-scm
  (syntax-rules ()
    ((_ name op-fx1 op-fx2 op-fl)
     (begin
       (define-ir (name (scm! dst) (scm a) (scm b))
         (nyi "~s: ~a ~a ~a" 'name (local-ref dst) a b))
       (define-ir (name (fixnum! dst) (fixnum a) (fixnum b))
         (let ((dst/v (var-ref dst))
               (a/v (var-ref a))
               (b/v (var-ref b)))
           `(let ((,dst/v (op-fx1 ,a/v ,b/v)))
              (let ((,dst/v (op-fx2 ,dst/v 2)))
                ,(next)))))
       (define-ir (name (flonum! dst) (fixnum a) (flonum b))
         (let ((dst/v (var-ref dst))
               (a/v (var-ref a))
               (b/v (var-ref b))
               (r2 (make-tmpvar 2))
               (f2 (make-tmpvar/f 2)))
           `(let ((,r2 (%rsh ,a/v 2)))
              (let ((,f2 (%i2d ,r2)))
                (let ((,dst/v (op-fl ,f2 ,b/v)))
                  ,(next))))))
       (define-ir (name (flonum! dst) (flonum a) (flonum b))
         (let* ((dst/v (var-ref dst))
                (a/v (var-ref a))
                (b/v (var-ref b))
                (inferred (outline-inferred-types (ir-outline ir)))
                (a/it (assq-ref inferred a))
                (b/it (assq-ref inferred b)))
           (debug 1 ";;; [IR] ~s: i=(~a ~a)~%" 'name
                  (pretty-type a/it) (pretty-type b/it))
           `(let ((,dst/v (op-fl ,a/v ,b/v)))
              ,(next))
           ;; (cond
           ;;  ((and (eq? &flonum a/it) (eq? &flonum b/it))
           ;;   `(let ((,dst/v (op-fl ,a/v ,b/v)))
           ;;      ,(next)))
           ;;  ((and (eq? &scm a/it) (eq? &flonum b/it))
           ;;   (let ((f2 (make-tmpvar/f 2)))
           ;;     (with-unboxing &flonum f2 a/v
           ;;       (lambda ()
           ;;         `(let ((,dst/v (op-fl ,f2 ,b/v)))
           ;;            ,(next))))))
           ;;  (else
           ;;   (nyi "~s: type mismatch i=(~s ~s)" 'name a/it b/it)))
           ))))))

(define-syntax define-binary-arith-scm-imm
  (syntax-rules ()
    ((_ name op-fx)
     (begin
       (define-ir (name (scm! dst) (scm src) (const imm))
         (nyi "~s: ~a ~a" 'name (local-ref src) imm))
       (define-ir (name (fixnum! dst) (fixnum src) (const imm))
         `(let ((,(var-ref dst) (op-fx ,(var-ref src) ,(* imm 4))))
            ,(next)))))))

(define-binary-arith-scm-scm add %add %sub %fadd)
(define-binary-arith-scm-imm add/immediate %add)
(define-binary-arith-scm-scm sub %sub %add %fsub)
(define-binary-arith-scm-imm sub/immediate %sub)

(define-ir (mul (scm! dst) (scm a) (scm b))
  (nyi "mul: ~a ~a ~a" dst a b))

(define-ir (mul (flonum! dst) (flonum a) (fixnum b))
  (let* ((dst/v (var-ref dst))
         (a/v (var-ref a))
         (b/v (var-ref b))
         (r2 (make-tmpvar 2))
         (f2 (make-tmpvar/f 2))
         (emit-next (lambda ()
                      `(let ((,r2 (%rsh ,b/v 2)))
                         (let ((,f2 (%i2d ,r2)))
                           (let ((,dst/v (%fmul ,a/v ,f2)))
                             ,(next)))))))
    (with-unboxing &exact-integer b/v b/v emit-next)))

(define-ir (mul (flonum! dst) (flonum a) (flonum b))
  `(let ((,(var-ref dst) (%fmul ,(var-ref a) ,(var-ref b))))
     ,(next)))

(define-ir (div (scm! dst) (scm a) (scm b))
  (nyi "div: ~s ~s ~s~%" dst a b))

(define-ir (div (flonum! dst) (fixnum a) (flonum b))
  (let* ((dst/v (var-ref dst))
         (a/v (var-ref a))
         (b/v (var-ref b))
         (r2 (make-tmpvar 2))
         (f2 (make-tmpvar/f 2))
         (emit-next (lambda ()
                      `(let ((,r2 (%rsh ,a/v 2)))
                         (let ((,f2 (%i2d ,r2)))
                           (let ((,dst/v (%fdiv ,f2 ,b/v)))
                             ,(next)))))))
    (with-unboxing &exact-integer a/v a/v emit-next)))

(define-ir (div (flonum! dst) (flonum a) (flonum b))
  `(let ((,(var-ref dst) (%fdiv ,(var-ref a) ,(var-ref b))))
     ,(next)))

(define-syntax define-binary-arith-fx-fx
  (syntax-rules ()
    ((_ name op)
     (begin
       (define-ir (name (scm! dst) (scm a) (scm b))
         (nyi "~s: ~a ~a ~a" 'name dst a b))
       (define-ir (name (fixnum! dst) (fixnum a) (fixnum b))
         (let ((dst/v (var-ref dst))
               (a/v (var-ref a))
               (b/v (var-ref b))
               (r2 (make-tmpvar 2)))
           `(let ((,r2 (%rsh ,a/v 2)))
              (let ((,dst/v (%rsh ,b/v 2)))
                (let ((,dst/v (op ,r2 ,dst/v)))
                  (let ((,dst/v (%lsh ,dst/v 2)))
                    (let ((,dst/v (%add ,dst/v 2)))
                      ,(next))))))))))))

(define-binary-arith-fx-fx mod %mod)

;; XXX: quo
;; XXX: rem

;; XXX: ash
;; XXX: logand
;; XXX: logior
;; XXX: logxor
;; XXX: make-vector
;; XXX: make-vector/immediate

(define-ir (vector-length (u64! dst) (vector src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%cref ,src/v 0)))
       (let ((,dst/v (%rsh ,dst/v 8)))
         ,(next)))))

(define-ir (vector-ref (scm! dst) (vector src) (u64 idx))
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
         ,(with-unboxing (type-of dst/l) r2 r2
            (lambda ()
              `(let ((,dst/v ,r2))
                 ,(next))))))))

(define-ir (vector-ref/immediate (scm! dst) (vector src) (const idx))
  (let ((dst/v (var-ref dst))
        (dst/l (let ((src/l (local-ref src)))
                 (if (vector? src/l)
                     (vector-ref src/l idx)
                     (tjitc-error 'vector-ref "not a vector ~s" src/l))))
        (src/v (var-ref src))
        (r2 (make-tmpvar 2)))
    `(let ((,r2 (%cref ,src/v ,(+ idx 1))))
       ,(with-unboxing (type-of dst/l) r2 r2
          (lambda ()
            `(let ((,dst/v ,r2))
               ,(next)))))))

(define-ir (vector-set! (vector dst) (u64 idx) (scm src))
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

(define-ir (vector-set!/immediate (vector dst) (const idx) (scm src))
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
