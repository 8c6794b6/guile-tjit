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
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables))

(define-syntax define-binary-arith-scm-scm
  (syntax-rules ()
    ((_ name op-fx1 op-fx2 op-fl)
     (define-ir (name (scm dst) (scm a) (scm b))
       (let ((ra (local-ref a))
             (rb (local-ref b))
             (vdst (var-ref dst))
             (va (var-ref a))
             (vb (var-ref b)))
         (cond
          ((and (fixnum? ra) (fixnum? rb))
           `(let ((,vdst (op-fx1 ,va ,vb)))
              (let ((,vdst (op-fx2 ,vdst 2)))
                ,(next))))
          ((and (fixnum? ra) (flonum? rb))
           (let ((r0 (make-tmpvar 0))
                 (f0 (make-tmpvar/f 0)))
             `(let ((,r0 (%rsh ,va 2)))
                (let ((,f0 (%i2d ,r0)))
                  (let ((,vdst (op-fl ,f0 ,vb)))
                    ,(next))))))
          ((and (flonum? ra) (flonum? rb))
           `(let ((,vdst (op-fl ,va ,vb)))
              ,(next)))
          (else
           (nyi "~s: ~a ~a ~a" 'name (local-ref dst) ra rb))))))))

(define-syntax define-binary-arith-scm-imm
  (syntax-rules ()
    ((_ name op-fx)
     (define-ir (name (scm dst) (scm src) (const imm))
       (let ((rsrc (local-ref src))
             (vdst (var-ref dst))
             (vsrc (var-ref src)))
         (cond
          ((fixnum? rsrc)
           `(let ((,vdst (op-fx ,vsrc ,(* imm 4))))
              ,(next)))
          (else
           (nyi "~s: ~a ~a" 'name (local-ref dst) rsrc))))))))

(define-binary-arith-scm-scm add %add %sub %fadd)
(define-binary-arith-scm-imm add/immediate %add)
(define-binary-arith-scm-scm sub %sub %add %fsub)
(define-binary-arith-scm-imm sub/immediate %sub)

(define-ir (mul (scm dst) (scm a) (scm b))
  (let ((ra (local-ref a))
        (rb (local-ref b))
        (vdst (var-ref dst))
        (va (var-ref a))
        (vb (var-ref b)))
    (cond
     ((and (flonum? ra) (flonum? rb))
      `(let ((,vdst (%fmul ,va ,vb)))
         ,(next)))
     ((and (flonum? ra) (fixnum? rb))
      (let ((r2 (make-tmpvar 2))
            (f2 (make-tmpvar/f 2)))
        `(let ((,r2 (%rsh ,vb 2)))
           (let ((,f2 (%i2d ,r2)))
             (let ((,vdst (%fmul ,va ,f2)))
               ,(next))))))
     (else
      (nyi "mul: ~a ~a ~a" (local-ref dst) ra rb)))))

;; (define-ir (div (scm dst) (scm a) (scm b))
;;   (let ((ra (local-ref a))
;;         (rb (local-ref b))
;;         (vdst (var-ref dst))
;;         (va (var-ref a))
;;         (vb (var-ref b)))
;;     (cond
;;      ((and (flonum? ra) (flonum? rb))
;;       `(let ((,vdst (%fdiv ,va ,vb)))
;;          ,(next)))
;;      ((and (fixnum? ra) (flonum? rb))
;;       (let ((r2 (make-tmpvar 2))
;;             (f2 (make-tmpvar/f 2)))
;;         `(let ((,r2 (%rsh ,va 2)))
;;            (let ((,f2 (%i2d ,r2)))
;;              (let ((,vdst (%fdiv ,f2 ,vb)))
;;                ,(next))))))
;;      (else
;;       (nyi "div: ~a ~a ~a" (local-ref dst) ra rb)))))

;; XXX: quo
;; XXX: rem

(define-ir (mod (scm dst) (scm a) (scm b))
  (let ((ra (local-ref a))
        (rb (local-ref b))
        (vdst (var-ref dst))
        (va (var-ref a))
        (vb (var-ref b)))
    (cond
     ((and (fixnum? ra) (fixnum? rb))
      (let ((r2 (make-tmpvar 2)))
        `(let ((,r2 (%rsh ,va 2)))
           (let ((,vdst (%rsh ,vb 2)))
             (let ((,vdst (%mod ,r2 ,vdst)))
               (let ((,vdst (%lsh ,vdst 2)))
                 (let ((,vdst (%add ,vdst 2)))
                   ,(next))))))))
     (else
      (nyi "mod: ~a ~a ~a" (local-ref dst) ra rb)))))

;; XXX: ash
;; XXX: logand
;; XXX: logior
;; XXX: logxor
;; XXX: make-vector
;; XXX: make-vector/immediate

(define-ir (vector-length (u64 dst) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%cref ,src/v 0)))
       (let ((,dst/v (%rsh ,dst/v 8)))
         ,(next)))))

(define-ir (vector-ref (scm dst) (scm src) (u64 idx))
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

(define-ir (vector-ref/immediate (scm dst) (scm src) (const idx))
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
     (define-ir (name (f64 dst) (f64 a) (f64 b))
       `(let ((,(var-ref dst) (op ,(var-ref a) ,(var-ref b))))
          ,(next))))))

(define-binary-arith-f64-f64 fadd %fadd)
(define-binary-arith-f64-f64 fsub %fsub)
(define-binary-arith-f64-f64 fmul %fmul)
(define-binary-arith-f64-f64 fdiv %fdiv)

(define-syntax define-binary-arith-u64-imm
  (syntax-rules ()
    ((_ name op)
     (define-ir (name (u64 dst) (u64 src) (const imm))
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
