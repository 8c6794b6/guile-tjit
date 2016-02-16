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
         (nyi "~s: ~a ~a ~a" 'name dst a b))
       (define-ir (name (fixnum! dst) (fixnum a) (fixnum b))
         (let* ((dst/v (var-ref dst))
                (a/v (var-ref a))
                (b/v (var-ref b))
                (a/t (ty-ref a))
                (b/t (ty-ref b))
                (next-thunk
                 (lambda ()
                   `(let ((,dst/v (op-fx1 ,a/v ,b/v)))
                      (let ((,dst/v (op-fx2 ,dst/v 2)))
                        ,(next)))))
                )
           ;; `(let ((,dst/v (op-fx1 ,a/v ,b/v)))
           ;;    (let ((,dst/v (op-fx2 ,dst/v 2)))
           ;;      ,(next)))
           (cond
            ((and (eq? &exact-integer a/t) (eq? &exact-integer b/t))
             (next-thunk))
            ((and (eq? &scm a/t) (eq? &exact-integer b/t))
             (with-unboxing &exact-integer a/v a/v next-thunk))
            ((and (eq? &exact-integer a/t) (eq? &scm b/t))
             (with-unboxing &exact-integer b/v b/v next-thunk))
            ((and (eq? &scm a/t) (eq? &scm b/t))
             (with-unboxing &exact-integer a/v a/v
               (lambda ()
                 (with-unboxing &exact-integer b/v b/v
                   next-thunk))))
            (else
             (nyi "~s: et=(fixnum fixnum) it=(~a ~a)" 'name
                  (pretty-type a/t) (pretty-type b/t))))
           ))
       (define-ir (name (flonum! dst) (fixnum a) (flonum b))
         (let* ((dst/v (var-ref dst))
                (a/v (var-ref a))
                (b/v (var-ref b))
                (a/t (ty-ref a))
                (b/t (ty-ref b))
                (r2 (make-tmpvar 2))
                (f2 (make-tmpvar/f 2)))
           (cond
            ((and (eq? &exact-integer a/t) (eq? &flonum b/t))
             `(let ((,r2 (%rsh ,a/v 2)))
                (let ((,f2 (%i2d ,r2)))
                  (let ((,dst/v (op-fl ,f2 ,b/v)))
                    ,(next)))))
            ((and (eq? &scm a/t) (eq? &flonum b/t))
             (with-unboxing &exact-integer a/v a/v
               (lambda ()
                 `(let ((,r2 (%rsh ,a/v 2)))
                    (let ((,f2 (%i2d ,r2)))
                      (let ((,dst/v (op-fl ,f2 ,b/v)))
                        ,(next)))))))
            (else
             (nyi "~s: et=(fixnum flonum) it=(~a ~a)"
                  'name (pretty-type a/t) (pretty-type b/t))))))
       (define-ir (name (flonum! dst) (flonum a) (flonum b))
         (let* ((dst/v (var-ref dst))
                (a/v (var-ref a))
                (b/v (var-ref b))
                (a/t (ty-ref a))
                (b/t (ty-ref b))
                (f2 (make-tmpvar/f 2)))
           (debug 1 ";;; [IR] ~s: i=(~a ~a)~%" 'name
                  (pretty-type a/t) (pretty-type b/t))
           (cond
            ((and (eq? &flonum a/t) (eq? &flonum b/t))
             `(let ((,dst/v (op-fl ,a/v ,b/v)))
                ,(next)))
            ;; ((and (eq? &flonum a/t) (eq? &exact-integer b/t))
            ;;  (let ((f2 (make-tmpvar/f 2))
            ;;        (r2 (make-tmpvar 2)))
            ;;    `(let ((,r2 (%rsh ,b/v 2)))
            ;;       (let ((,f2 (%i2d ,r2)))
            ;;         (let ((,dst/v (op-fl ,a/v ,f2)))
            ;;           ,(next))))))
            ((and (eq? &scm a/t) (eq? &flonum b/t))
             (with-unboxing &flonum f2 a/v
               (lambda ()
                 `(let ((,dst/v (op-fl ,f2 ,b/v)))
                    ,(next)))))
            ((and (eq? &flonum a/t) (eq? &scm b/t))
             (with-unboxing &flonum f2 b/v
               (lambda ()
                 `(let ((,dst/v (op-fl ,a/v ,f2)))
                    ,(next)))))
            (else
             (nyi "~s: type mismatch i=(~a ~a)" 'name
                  (pretty-type a/t) (pretty-type b/t))))))))))

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
         (f1 (make-tmpvar/f 1))
         (f2 (make-tmpvar/f 2))
         (a/t (ty-ref a))
         (b/t (ty-ref b)))
    (cond
     ((and (eq? &exact-integer a/t) (eq? &flonum b/t))
      `(let ((,r2 (%rsh ,a/v 2)))
         (let ((,f2 (%i2d ,r2)))
           (let ((,dst/v (%fdiv ,f2 ,b/v)))
             ,(next)))))
     ((and (eq? &exact-integer a/t) (eq? &scm b/t))
      (with-unboxing &flonum f1 b/v
        (lambda ()
          `(let ((,r2 (%rsh ,a/v 2)))
             (let ((,f2 (%i2d ,r2)))
               (let ((,dst/v (%fdiv ,f2 ,f1)))
                 ,(next)))))))
     ((and (eq? &scm a/t) (eq? &scm b/t))
      (with-unboxing &exact-integer a/v a/v
        (with-unboxing &flonum f2 b/v
          (lambda ()
            `(let ((,r2 (%rsh ,a/v 2)))
               (let ((,f1 (%i2d ,r2)))
                 (let ((,dst/v (%fdiv ,f1 ,f2)))
                   ,(next))))))))
     (else
      (nyi "div: ety=(fixnum flonum) ity=(~a ~a)~%"
           (pretty-type a/t) (pretty-type b/t))))))

(define-ir (div (flonum! dst) (flonum a) (flonum b))
  (let* ((dst/v (var-ref dst))
         (a/v (var-ref a))
         (b/v (var-ref b))
         (a/t (ty-ref a))
         (b/t (ty-ref b)))
    (cond
     ((and (eq? &flonum a/t) (eq? &flonum b/t))
      `(let ((,dst/v (%fdiv ,a/v ,b/v)))
         ,(next)))
     (else
      (nyi "div: it=(~a ~a)~%" a/t b/t)))))

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
       (let ((,dst/v (%cref ,src/v ,r2)))
         ,(next)))))

(define-ir (vector-ref/immediate (scm! dst) (vector src) (const idx))
  (let ((dst/v (var-ref dst))
        (dst/l (let ((src/l (local-ref src)))
                 (if (vector? src/l)
                     (vector-ref src/l idx)
                     (tjitc-error 'vector-ref "not a vector ~s" src/l))))
        (src/v (var-ref src))
        (r2 (make-tmpvar 2)))
    `(let ((,dst/v (%cref ,src/v ,(+ idx 1))))
       ,(next))))

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
