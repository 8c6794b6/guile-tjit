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
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-syntax-rule (nyi-unary name t1 a)
  (nyi "~s: et=(~s) args=(~a:~a)" name t1
       (scm-ref a) (pretty-type (type-ref a))))

(define-syntax-rule (nyi-binary name t1 t2 a b)
  (nyi "~s: et=(~s ~s) args=(~a:~a ~a:~a)" name t1 t2
       (scm-ref a) (pretty-type (type-ref a))
       (scm-ref b) (pretty-type (type-ref b))))

;; Definition order matters, define general types first, then follows
;; more specific types.
(define-syntax-rule (define-add-sub-scm-scm name op-fx1 op-fx2 op-fl)
  (begin
    (define-ir (name (scm! dst) (scm a) (scm b))
      (nyi-binary 'name 'scm 'scm a b))
    (define-ir (name (fixnum! dst) (fixnum a) (fixnum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst)))
        (with-type-guard &fixnum a
          (with-type-guard &fixnum b
            `(let ((,dst/v (op-fx1 ,a/v ,b/v)))
               (let ((,dst/v (op-fx2 ,dst/v 2)))
                 ,(next)))))))
    (define-ir (name (flonum! dst) (flonum a) (fraction b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (a/t (type-ref a))
             (b/t (type-ref b))
             (r2 (make-tmpvar 2))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2)))
        (cond
         ((and (eq? &flonum a/t)
               (or (eq? &scm b/t) (eq? &fraction b/t)))
          (with-type-guard-always &fraction b
            `(let ((,r2 (%cref ,b/v 1)))
               (let ((,r2 (%rsh ,r2 2)))
                 (let ((,f1 (%i2d ,r2)))
                   (let ((,r2 (%cref ,b/v 2)))
                     (let ((,r2 (%rsh ,r2 2)))
                       (let ((,f2 (%i2d ,r2)))
                         (let ((,f2 (%fdiv ,f1 ,f2)))
                           (let ((,dst/v (op-fl ,a/v ,f2)))
                             ,(next)))))))))))
         (else
          (nyi-binary 'name 'flonum 'fraction a b)))))
    (define-ir (name (flonum! dst) (fraction a) (flonum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (a/t (type-ref a))
             (b/t (type-ref b))
             (r2 (make-tmpvar 2))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2)))
        (cond
         ((and (or (eq? &scm a/t) (eq? &fraction a/t))
               (eq? &flonum b/t))
          (with-type-guard-always &fraction a
            `(let ((,r2 (%cref ,a/v 1)))
               (let ((,r2 (%rsh ,r2 2)))
                 (let ((,f1 (%i2d ,r2)))
                   (let ((,r2 (%cref ,a/v 2)))
                     (let ((,r2 (%rsh ,r2 2)))
                       (let ((,f2 (%i2d ,r2)))
                         (let ((,f2 (%fdiv ,f1 ,f2)))
                           (let ((,dst/v (op-fl ,f2 ,b/v)))
                             ,(next)))))))))))
         (else
          (nyi-binary 'name 'fraction 'flonum a b)))))
    (define-ir (name (flonum! dst) (fixnum a) (flonum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (b/t (type-ref b))
             (r2 (make-tmpvar 2))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2)))
        (cond
         ((eq? &flonum b/t)
          (with-type-guard &fixnum a
            `(let ((,r2 (%rsh ,a/v 2)))
               (let ((,f2 (%i2d ,r2)))
                 (let ((,dst/v (op-fl ,f2 ,b/v)))
                   ,(next))))))
         ((eq? &scm b/t)
          (with-type-guard &fixnum a
            (with-type-guard-always &flonum b
              `(let ((,r2 (%rsh ,a/v 2)))
                 (let ((,f1 (%i2d ,r2)))
                   (let ((,f2 (%cref/f ,b/v 2)))
                     (let ((,dst/v (op-fl ,f1 ,f2)))
                       ,(next))))))))
         (else
          (nyi-binary 'name 'fixnum 'flonum a b)))))
    (define-ir (name (flonum! dst) (flonum a) (fixnum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (a/t (type-ref a))
             (r2 (make-tmpvar 2))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2)))
        (cond
         ((eq? &flonum a/t)
          (with-type-guard &fixnum b
            `(let ((,r2 (%rsh ,b/v 2)))
               (let ((,f2 (%i2d ,r2)))
                 (let ((,dst/v (op-fl ,a/v ,f2)))
                   ,(next))))))
         ((eq? &scm a/t)
          (with-type-guard-always &flonum a
            (with-type-guard &fixnum b
              `(let ((,f1 (%cref/f ,a/v 2)))
                 (let ((,r2 (%rsh ,b/v 2)))
                   (let ((,f2 (%i2d ,r2)))
                     (let ((,dst/v (op-fl ,f1 ,f2)))
                       ,(next))))))))
         (else
          (nyi-binary 'name 'flonum 'fixnum a b)))))
    (define-ir (name (flonum! dst) (flonum a) (flonum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (a/t (type-ref a))
             (b/t (type-ref b))
             (r2 (make-tmpvar 2))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2)))
        (cond
         ((and (eq? &flonum a/t) (eq? &flonum b/t))
          `(let ((,dst/v (op-fl ,a/v ,b/v)))
             ,(next)))
         ((and (eq? &scm a/t) (eq? &flonum b/t))
          (with-type-guard-always &flonum a
            `(let ((,f2 (%cref/f ,a/v 2)))
               (let ((,dst/v (op-fl ,f2 ,b/v)))
                 ,(next)))))
         ((and (eq? &flonum a/t) (eq? &fixnum b/t))
          (with-type-guard-always &fixnum b
            `(let ((,r2 (%rsh ,b/v 2)))
               (let ((,f2 (%i2d ,r2)))
                 (let ((,dst/v (op-fl ,a/v ,f2)))
                   ,(next))))))
         ((and (eq? &flonum a/t) (eq? &scm b/t))
          (with-type-guard-always &flonum b
            `(let ((,f2 (%cref/f ,b/v 2)))
               (let ((,dst/v (op-fl ,a/v ,f2)))
                 ,(next)))))
         ((and (eq? &scm a/t) (eq? &scm b/t))
          (with-type-guard-always &flonum a
            (with-type-guard-always &flonum b
              `(let ((,f1 (%cref/f ,a/v 2)))
                 (let ((,f2 (%cref/f ,b/v 2)))
                   (let ((,dst/v (op-fl ,f1 ,f2)))
                     ,(next)))))))
         (else
          (nyi-binary 'name 'flonum 'flonum a b)))))))

(define-syntax-rule (define-add-sub-scm-imm name op-fx)
  (begin
    (define-ir (name (scm! dst) (scm src) (const imm))
      (nyi-unary 'name 'scm src))
    (define-ir (name (fixnum! dst) (fixnum src) (const imm))
      (let* ((src/v (src-ref src))
             (dst/v (dst-ref dst)))
        (with-type-guard &fixnum src
          `(let ((,dst/v (op-fx ,src/v ,(* imm 4))))
             ,(next)))))))

(define-add-sub-scm-scm add %add %sub %fadd)
(define-add-sub-scm-imm add/immediate %add)
(define-add-sub-scm-scm sub %sub %add %fsub)
(define-add-sub-scm-imm sub/immediate %sub)

(define-syntax-rule (define-mul-div-scm-scm name op-fx op-fl)
  (begin
    (define-ir (name (scm! dst) (scm a) (scm b))
      (nyi-binary 'name 'scm 'scm a b))
    (define-ir (name (flonum! dst) (flonum a) (fraction b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (a/t (type-ref a))
             (b/t (type-ref b))
             (r1 (make-tmpvar 1))
             (r2 (make-tmpvar 2))
             (f0 (make-tmpvar/f 0))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2))
             (emit-next (lambda (unboxed)
                          `(let ((,r1 (%cref ,b/v 1)))
                             (let ((,r1 (%rsh ,r1 2)))
                               (let ((,r2 (%cref ,b/v 2)))
                                 (let ((,r2 (%rsh ,r2 2)))
                                   (let ((,f1 (%i2d ,r1)))
                                     (let ((,f2 (%i2d ,r2)))
                                       (let ((,f2 (%fdiv ,f1 ,f2)))
                                         (let ((,dst/v (op-fl ,unboxed ,f2)))
                                           ,(next))))))))))))
        (cond
         ((and (eq? &flonum a/t)
               (or (eq? &scm b/t) (eq? &fraction b/t)))
          (with-type-guard-always &fraction b
            (emit-next a/v)))
         ((and (eq? &scm a/t)
               (or (eq? &scm b/t) (eq? &fraction b/t)))
          (with-type-guard-always &fraction b
            (with-type-guard-always &flonum a
              `(let ((,f0 (%cref/f ,a/v 2)))
                 ,(emit-next f0)))))
         (else
          (nyi-binary 'name 'flonum 'fraction a b)))))
    (define-ir (name (flonum! dst) (fraction a) (flonum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (a/t (type-ref a))
             (b/t (type-ref b))
             (r1 (make-tmpvar 1))
             (r2 (make-tmpvar 2))
             (f0 (make-tmpvar/f 0))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2))
             (emit-next (lambda (unboxed)
                          `(let ((,r1 (%cref ,a/v 1)))
                             (let ((,r1 (%rsh ,r1 2)))
                               (let ((,r2 (%cref ,a/v 2)))
                                 (let ((,r2 (%rsh ,r2 2)))
                                   (let ((,f1 (%i2d ,r1)))
                                     (let ((,f2 (%i2d ,r2)))
                                       (let ((,f2 (%fdiv ,f1 ,f2)))
                                         (let ((,dst/v (op-fl ,f2 ,unboxed)))
                                           ,(next))))))))))))
        (cond
         ((and (or (eq? &scm a/t) (eq? &fraction a/t))
               (eq? &flonum b/t))
          (with-type-guard-always &fraction a
            (emit-next b/v)))
         ((and (or (eq? &scm a/t) (eq? &fraction a/t))
               (eq? &scm b/t))
          (with-type-guard-always &fraction a
            (with-type-guard-always &flonum b
              `(let ((,f0 (%cref/f a/v 2)))
                 ,(emit-next f0)))))
         (else
          (nyi-binary 'name 'fraction 'flonum a b)))))
    (define-ir (name (flonum! dst) (flonum a) (fixnum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (r2 (make-tmpvar 2))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2))
             (a/t (type-ref a))
             (b/t (type-ref b)))
        (cond
         ((eq? &flonum a/t)
          (with-type-guard &fixnum b
            `(let ((,r2 (%rsh ,b/v 2)))
               (let ((,f2 (%i2d ,r2)))
                 (let ((,dst/v (op-fl ,a/v ,f2)))
                   ,(next))))))
         ((eq? &scm a/t)
          (with-type-guard &flonum a
            (with-type-guard &fixnum b
              `(let ((,f1 (%cref/f ,a/v 2)))
                 (let ((,r2 (%rsh ,b/v 2)))
                   (let ((,f2 (%i2d ,r2)))
                     (let ((,dst/v (op-fl ,f1 ,f2)))
                       ,(next))))))))
         (else
          (nyi-binary 'name 'flonum 'fixnum a b)))))
    (define-ir (name (flonum! dst) (fixnum a) (flonum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (r2 (make-tmpvar 2))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2))
             (a/t (type-ref a))
             (b/t (type-ref b)))
        (cond
         ((eq? &flonum b/t)
          (with-type-guard &fixnum a
            `(let ((,r2 (%rsh ,a/v 2)))
               (let ((,f2 (%i2d ,r2)))
                 (let ((,dst/v (op-fl ,f2 ,b/v)))
                   ,(next))))))
         ((eq? &scm b/t)
          (with-type-guard &fixnum a
            (with-type-guard &flonum b
              `(let ((,r2 (%rsh ,a/v 2)))
                 (let ((,f1 (%i2d ,r2)))
                   (let ((,f2 (%cref/f ,b/v 2)))
                     (let ((,dst/v (op-fl ,f1 ,f2)))
                       ,(next))))))))
         (else
          (nyi-binary 'name 'fixnum 'flonum a b)))))
    (define-ir (name (flonum! dst) (flonum a) (flonum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst))
             (a/t (type-ref a))
             (b/t (type-ref b))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2)))
        (cond
         ((and (eq? &flonum a/t) (eq? &flonum b/t))
          `(let ((,dst/v (op-fl ,a/v ,b/v)))
             ,(next)))
         ((and (eq? &flonum a/t) (eq? &scm b/t))
          (with-type-guard-always &flonum b
            `(let ((,f2 (%cref/f ,b/v 2)))
               (let ((,dst/v (op-fl ,a/v ,f2)))
                 ,(next)))))
         ((and (eq? &scm a/t) (eq? &flonum b/t))
          (with-type-guard-always &flonum a
            `(let ((,f1 (%cref/f ,a/v 2)))
               (let ((,dst/v (op-fl ,f1 ,b/v)))
                 ,(next)))))
         ((and (eq? &scm a/t) (eq? &scm b/t))
          (with-type-guard-always &flonum a
            (with-type-guard-always &flonum b
              `(let ((,f1 (%cref/f ,a/v 2)))
                 (let ((,f2 (%cref/f ,b/v 2)))
                   (let ((,dst/v (op-fl ,f1 ,f2)))
                     ,(next)))))))
         (else
          (nyi-binary 'name 'flonum 'flonum a b)))))))

(define-mul-div-scm-scm mul %mulov %fmul)
(define-mul-div-scm-scm div %div %fdiv)

(define-ir (div (fraction! dst) (fixnum a) (fixnum b))
  (let* ((a/v (src-ref a))
         (b/v (src-ref b))
         (dst/v (dst-ref dst))
         (a/t (type-ref a))
         (b/t (type-ref b))
         (r1 (make-tmpvar 1))
         (r2 (make-tmpvar 2)))
    (when (env-parent-snapshot env)
      (set-env-save-volatiles! env #t))
    (with-type-guard &fixnum a
      (with-type-guard &fixnum b
        `(let ((_ ,(take-snapshot! ip 0)))
           (let ((,r1 (%rsh ,a/v 2)))
             (let ((,r2 (%rsh ,b/v 2)))
               (let ((,dst/v (%div ,r1 ,r2)))
                 ,(next)))))))))

;; Primitive %mod, %quo, and %rem may use volatile register. Setting
;; `env-save-volatiles?' flag to true.
(define-syntax-rule (define-binary-arith-fx-fx name op save-volatile?)
  (define-ir (name (fixnum! dst) (fixnum a) (fixnum b))
    (let* ((a/v (src-ref a))
           (b/v (src-ref b))
           (dst/v (dst-ref dst))
           (r1 (make-tmpvar 1))
           (r2 (make-tmpvar 2)))
      (when (and save-volatile?
                 (env-parent-snapshot env))
        (set-env-save-volatiles! env #t))
      (with-type-guard &fixnum a
        (with-type-guard &fixnum b
          `(let ((,r2 (%rsh ,a/v 2)))
             (let ((,r1 (%rsh ,b/v 2)))
               (let ((,r2 (op ,r2 ,r1)))
                 (let ((,r2 (%lsh ,r2 2)))
                   (let ((,dst/v (%add ,r2 2)))
                     ,(next)))))))))))

(define-syntax-rule (define-nyi-binary-scm-scm name)
  (define-ir (name (scm! dst) (scm a) (scm b))
    (nyi-binary 'name 'scm 'scm a b)))

(define-nyi-binary-scm-scm mod)
(define-nyi-binary-scm-scm quo)
(define-nyi-binary-scm-scm rem)

(define-binary-arith-fx-fx mul %mulov #f)
(define-binary-arith-fx-fx mod %mod #t)
(define-binary-arith-fx-fx quo %quo #t)
(define-binary-arith-fx-fx rem %rem #t)

;; XXX: ash

(define-syntax-rule (define-binary-bitwise-arith name dst/v a/v b/v exp)
  (begin
    (define-nyi-binary-scm-scm name)
    (define-ir (name (fixnum! dst) (fixnum a) (fixnum b))
      (let* ((a/v (src-ref a))
             (b/v (src-ref b))
             (dst/v (dst-ref dst)))
        (with-type-guard &fixnum a
          (with-type-guard &fixnum b
            exp))))))

(define-binary-bitwise-arith logand dst/v a/v b/v
  `(let ((,dst/v (%band ,a/v ,b/v)))
     ,(next)))

(define-binary-bitwise-arith logior dst/v a/v b/v
  `(let ((,dst/v (%bor ,a/v ,b/v)))
     ,(next)))

;; XXX: logxor
;; XXX: logsub

(define-interrupt-ir (make-vector (vector! dst) (u64 len) (scm ini))
  (let* ((len/v (src-ref len))
         (ini/v (src-ref ini))
         (dst/v (dst-ref dst))
         (r1 (make-tmpvar 1))
         (r2 (make-tmpvar 2)))
    `(let ((,r1 (%lsh ,len/v 8)))
       (let ((,r1 (%bor ,%tc7-vector ,r1)))
         (let ((,r2 (%add ,len/v 1)))
           (let ((,r2 (%words ,r1 ,r2)))
             ,(with-boxing (type-ref ini) ini/v r1
                (lambda (boxed)
                  `(let ((_ (%fill ,r2 ,len/v ,boxed)))
                     (let ((,dst/v ,r2))
                       ,(next)))))))))))

(define-interrupt-ir (make-vector/immediate (vector! dst) (const len)
                                            (scm ini))
  (let ((ini/v (src-ref ini))
        (dst/v (dst-ref dst))
        (r1 (make-tmpvar 1))
        (r2 (make-tmpvar 2))
        (nwords (+ len 1))
        (head (logior %tc7-vector (ash len 8))))
    `(let ((,r2 (%words ,head ,nwords)))
       ,(with-boxing (type-ref ini) ini/v r1
          (lambda (boxed)
            (if (= len 1)
                `(let ((_ (%cset ,r2 1 ,boxed)))
                   (let ((,dst/v ,r2))
                     ,(next)))
                `(let ((_ (%fill ,r2 ,len ,boxed)))
                   (let ((,dst/v ,r2))
                     ,(next)))))))))

(define-ir (vector-length (u64! dst) (vector src))
  (let ((src/v (src-ref src))
        (dst/v (dst-ref dst)))
    (with-type-guard &vector src
      `(let ((,dst/v (%cref ,src/v 0)))
         (let ((,dst/v (%rsh ,dst/v 8)))
           ,(next))))))

(define-ir (vector-ref (scm! dst) (vector src) (u64 idx))
  (let* ((src/v (src-ref src))
         (idx/v (src-ref idx))
         (dst/v (dst-ref dst))
         (r2 (make-tmpvar 2)))
    (with-type-guard &vector src
      `(let ((_ ,(take-snapshot! ip 0)))
         (let ((,r2 (%cref ,src/v 0)))
           (let ((,r2 (%rsh ,r2 8)))
             (let ((_ (%lt ,idx/v ,r2)))
               (let ((,r2 (%add ,idx/v 1)))
                 (let ((,dst/v (%cref ,src/v ,r2)))
                   ,(next))))))))))

(define-ir (vector-ref/immediate (scm! dst) (vector src) (const idx))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst))
         (r2 (make-tmpvar 2)))
    (with-type-guard &vector src
      `(let ((,dst/v (%cref ,src/v ,(+ idx 1))))
         ,(next)))))

(define-ir (vector-set! (vector dst) (u64 idx) (scm src))
  (let* ((idx/v (src-ref idx))
         (src/v (src-ref src))
         (dst/v (dst-ref dst))
         (src/t (type-ref src))
         (r0 (make-tmpvar 0))
         (r1 (make-tmpvar 1))
         (r2 (make-tmpvar 2)))
    (with-type-guard &vector dst
      `(let ((_ ,(take-snapshot! ip 0)))
         (let ((,r2 (%cref ,dst/v 0)))
           (let ((,r2 (%rsh ,r2 8)))
             (let ((_ (%lt ,idx/v ,r2)))
               ,(with-boxing src/t src/v r1
                  (lambda (boxed)
                    `(let ((,r0 (%add ,idx/v 1)))
                       (let ((_ (%cset ,dst/v ,r0 ,boxed)))
                         ,(next))))))))))))

(define-ir (vector-set!/immediate (vector dst) (const idx) (scm src))
  (let* ((src/v (src-ref src))
         (dst/v (dst-ref dst))
         (src/t (type-ref src))
         (r1 (make-tmpvar 1)))
    (with-type-guard &vector dst
      (with-boxing src/t src/v r1
        (lambda (boxed)
          `(let ((_ (%cset ,dst/v ,(+ idx 1) ,boxed)))
             ,(next)))))))

(define-syntax-rule (define-binary-arith-f64-f64 name op)
  (define-ir (name (f64! dst) (f64 a) (f64 b))
    (let* ((a/v (src-ref a))
           (b/v (src-ref b))
           (dst/v (dst-ref dst)))
      `(let ((,dst/v (op ,a/v ,b/v)))
         ,(next)))))

(define-binary-arith-f64-f64 fadd %fadd)
(define-binary-arith-f64-f64 fsub %fsub)
(define-binary-arith-f64-f64 fmul %fmul)
(define-binary-arith-f64-f64 fdiv %fdiv)

(define-syntax-rule (define-binary-arith-u64-imm name op)
  (define-ir (name (u64! dst) (u64 src) (const imm))
    (let* ((src/v (src-ref src))
           (dst/v (dst-ref dst)))
      `(let ((,dst/v (op ,src/v ,imm)))
         ,(next)))))

(define-binary-arith-u64-imm uadd/immediate %add)
(define-binary-arith-u64-imm usub/immediate %sub)
;; XXX: umul/immediate

(define-syntax-rule (define-binary-arith-u64-u64 name op)
  (define-ir (name (u64! dst) (u64 a) (u64 b))
    (let* ((a/v (src-ref a))
           (b/v (src-ref b))
           (dst/v (dst-ref dst)))
      `(let ((,dst/v (op ,a/v ,b/v)))
         ,(next)))))

(define-binary-arith-u64-u64 uadd %add)
(define-binary-arith-u64-u64 usub %sub)
(define-binary-arith-u64-u64 umul %mul)
(define-binary-arith-u64-u64 ulogand %band)
(define-binary-arith-u64-u64 ulogior %bor)

;; XXX: ulogsub

;; XXX: ursh
;; XXX: ulsh
;; XXX: ursh/immediate
;; XXX: ulsh/immediate
