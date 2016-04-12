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


;; Definition order matters, define general types first, then follows
;; more specific types.
(define-syntax define-add-sub-scm-scm
  (syntax-rules ()
    ((_ name op-fx1 op-fx2 op-fl)
     (begin
       (define-ir (name (scm! dst) (scm a) (scm b))
         (nyi "~s: ~a ~a ~a" 'name dst a b))
       (define-ir (name (fixnum! dst) (fixnum a) (fixnum b))
         (let* ((dst/v (var-ref dst))
                (a/v (var-ref a))
                (b/v (var-ref b))
                (a/t (type-ref a))
                (b/t (type-ref b))
                (next-thunk
                 (lambda ()
                   `(let ((,dst/v (op-fx1 ,a/v ,b/v)))
                      (let ((,dst/v (op-fx2 ,dst/v 2)))
                        ,(next))))))
           (cond
            ((and (eq? &fixnum a/t) (eq? &fixnum b/t))
             (next-thunk))
            ((and (eq? &scm a/t) (eq? &fixnum b/t))
             (with-type-guard &fixnum a/v next-thunk))
            ((and (eq? &fixnum a/t) (eq? &scm b/t))
             (with-type-guard &fixnum b/v next-thunk))
            ((and (eq? &scm a/t) (eq? &scm b/t))
             (with-type-guard &fixnum a/v
               (lambda ()
                 (with-type-guard &fixnum b/v next-thunk))))
            (else
             (nyi "~s: et=(fixnum fixnum) it=(~a ~a)" 'name
                  (pretty-type a/t) (pretty-type b/t))))))
       (define-ir (name (flonum! dst) (fixnum a) (flonum b))
         (let* ((dst/v (var-ref dst))
                (a/v (var-ref a))
                (b/v (var-ref b))
                (a/t (type-ref a))
                (b/t (type-ref b))
                (r2 (make-tmpvar 2))
                (f1 (make-tmpvar/f 1))
                (f2 (make-tmpvar/f 2)))
           (cond
            ((and (eq? &fixnum a/t) (eq? &flonum b/t))
             `(let ((,r2 (%rsh ,a/v 2)))
                (let ((,f2 (%i2d ,r2)))
                  (let ((,dst/v (op-fl ,f2 ,b/v)))
                    ,(next)))))
            ((and (eq? &scm a/t) (eq? &flonum b/t))
             (with-type-guard &fixnum a/v
               (lambda ()
                 `(let ((,r2 (%rsh ,a/v 2)))
                    (let ((,f2 (%i2d ,r2)))
                      (let ((,dst/v (op-fl ,f2 ,b/v)))
                        ,(next)))))))
            ((and (eq? &scm a/t) (eq? &scm b/t))
             (with-type-guard &fixnum a/v
               (lambda ()
                 (with-type-guard &flonum b/v
                   (lambda ()
                     `(let ((,r2 (%rsh ,a/v 2)))
                        (let ((,f1 (%i2d ,r2)))
                          (let ((,f2 (%cref/f ,b/v 2)))
                            (let ((,dst/v (op-fl ,f1 ,f2)))
                              ,(next))))))))))
            (else
             (nyi "~s: et=(fixnum flonum) it=(~a ~a)"
                  'name (pretty-type a/t) (pretty-type b/t))))))
       (define-ir (name (flonum! dst) (flonum a) (fixnum b))
         (let* ((dst/v (var-ref dst))
                (a/v (var-ref a))
                (b/v (var-ref b))
                (a/t (type-ref a))
                (b/t (type-ref b))
                (r2 (make-tmpvar 2))
                (f1 (make-tmpvar/f 2))
                (f2 (make-tmpvar/f 2)))
           (cond
            ((and (eq? &flonum a/t) (eq? &fixnum b/t))
             `(let ((,r2 (%rsh ,b/v 2)))
                (let ((,f2 (%i2d ,r2)))
                  (let ((,dst/v (op-fl ,a/v ,f2)))
                    ,(next)))))
            ((and (eq? &flonum a/t) (eq? &scm b/t))
             (with-type-guard &fixnum b/v
               (lambda ()
                 `(let ((,r2 (%rsh ,b/v 2)))
                    (let ((,f2 (%i2d ,r2)))
                      (let ((,dst/v (op-fl ,a/v ,f2)))
                        ,(next)))))))
            ((and (eq? &scm a/t) (eq? &scm b/t))
             (with-type-guard &flonum a/v
               (lambda ()
                 (with-type-guard &fixnum b/v
                   (lambda ()
                     `(let ((,f1 (%cref/f ,a/v 2)))
                        (let ((,f2 (%i2d ,b/v)))
                          (let ((,dst/v (op-fl ,f1 ,f2)))
                            ,(next)))))))))
            (else
             (nyi "~s: et=(fixnum flonum) it=(~a ~a)"
                  'name (pretty-type a/t) (pretty-type b/t))))))
       (define-ir (name (flonum! dst) (flonum a) (flonum b))
         (let* ((dst/v (var-ref dst))
                (a/v (var-ref a))
                (b/v (var-ref b))
                (a/t (type-ref a))
                (b/t (type-ref b))
                (r2 (make-tmpvar 2))
                (f1 (make-tmpvar/f 1))
                (f2 (make-tmpvar/f 2)))
           (debug 2 ";;; [IR] ~s: i=(~a ~a)~%" 'name
                  (pretty-type a/t) (pretty-type b/t))
           (cond
            ((and (eq? &flonum a/t) (eq? &flonum b/t))
             `(let ((,dst/v (op-fl ,a/v ,b/v)))
                ,(next)))
            ((and (eq? &scm a/t) (eq? &flonum b/t))
             (with-type-guard &flonum a/v
               (lambda ()
                 `(let ((,f2 (%cref/f ,a/v 2)))
                    (let ((,dst/v (op-fl ,f2 ,b/v)))
                      ,(next))))))
            ((and (eq? &flonum a/t) (eq? &fixnum b/t))
             (with-type-guard &fixnum b/v
               (lambda ()
                 `(let ((,r2 (%rsh ,b/v 2)))
                    (let ((,f2 (%i2d ,r2)))
                      (let ((,dst/v (op-fl ,a/v ,f2)))
                        ,(next)))))))
            ((and (eq? &flonum a/t) (eq? &scm b/t))
             (with-type-guard &flonum b/v
               (lambda ()
                 `(let ((,f2 (%cref/f ,b/v 2)))
                    (let ((,dst/v (op-fl ,a/v ,f2)))
                      ,(next))))))
            ((and (eq? &scm a/t) (eq? &scm b/t))
             (with-type-guard &flonum a/v
               (lambda ()
                 (with-type-guard &flonum b/v
                   (lambda ()
                     `(let ((,f1 (%cref/f ,a/v 2)))
                        (let ((,f2 (%cref/f ,b/v 2)))
                          (let ((,dst/v (op-fl ,f1 ,f2)))
                            ,(next)))))))))
            (else
             (nyi "~s: et=(flonum flonum) it=(~a ~a)" 'name
                  (pretty-type a/t) (pretty-type b/t))))))))))

(define-syntax define-add-sub-scm-imm
  (syntax-rules ()
    ((_ name op-fx)
     (begin
       (define-ir (name (scm! dst) (scm src) (const imm))
         (nyi "~s: ~a ~a" 'name (scm-ref src) imm))
       (define-ir (name (fixnum! dst) (fixnum src) (const imm))
         (let* ((src/t (type-ref src))
                (src/v (var-ref src))
                (dst/v (var-ref dst))
                (next-thunk
                 (lambda ()
                   `(let ((,dst/v (op-fx ,src/v ,(* imm 4))))
                      ,(next)))))
           (cond
            ((eq? &fixnum src/t)
             (next-thunk))
            ((eq? &scm src/t)
             (with-type-guard &fixnum src/v next-thunk))
            (else
             (nyi "~s: et=fixnum it=~a" 'name (pretty-type src/t))))))))))

(define-add-sub-scm-scm add %add %sub %fadd)
(define-add-sub-scm-imm add/immediate %add)
(define-add-sub-scm-scm sub %sub %add %fsub)
(define-add-sub-scm-imm sub/immediate %sub)

(define-syntax-rule (define-mul-div-scm-scm name fx-op fl-op)
  (begin
    (define-ir (name (scm! dst) (scm a) (scm b))
      (let ((a/t (type-ref a))
            (b/t (type-ref b)))
        (nyi "~s: et=(scm scm) it=(~a ~a)" 'name
             (pretty-type a/t) (pretty-type b/t))))
    (define-ir (name (fixnum! dst) (fixnum a) (fixnum b))
      (let* ((a/t (type-ref a))
             (b/t (type-ref b))
             (dst/v (var-ref dst))
             (a/v (var-ref a))
             (b/v (var-ref b))
             (r1 (make-tmpvar 1))
             (r2 (make-tmpvar 2))
             (next-thunk (lambda ()
                           `(let ((,r1 (%rsh ,a/v 2)))
                              (let ((,r2 (%rsh ,b/v 2)))
                                (let ((,r2 (fx-op ,r1 ,r2)))
                                  (let ((,r2 (%lsh ,r2 2)))
                                    (let ((,dst/v (%add ,r2 2)))
                                      ,(next)))))))))
        (cond
         ((and (eq? &fixnum a/t) (eq? &fixnum b/t))
          `(let ((_ ,(take-snapshot! ip 0)))
             ,(next-thunk)))
         ((and (eq? &scm a/t) (eq? &fixnum b/t))
          (with-type-guard &fixnum a/v next-thunk))
         ((and (eq? &fixnum a/t) (eq? &scm b/t))
          (with-type-guard &fixnum b/v next-thunk))
         ((and (eq? &scm a/t) (eq? &scm b/t))
          (with-type-guard &fixnum a/v
            (lambda ()
              (with-type-guard &fixnum b/v
                next-thunk))))
         (else
          (nyi "~s: et=(fixnum fixnum) it=(~a ~a)" 'name
               (pretty-type a/t) (pretty-type b/t))))))
    (define-ir (name (flonum! dst) (flonum a) (fixnum b))
      (let* ((dst/v (var-ref dst))
             (a/v (var-ref a))
             (b/v (var-ref b))
             (r2 (make-tmpvar 2))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2))
             (a/t (type-ref a))
             (b/t (type-ref b)))
        (cond
         ((and (eq? &flonum a/t) (eq? &fixnum b/t))
          `(let ((,r2 (%rsh ,b/v 2)))
             (let ((,f2 (%i2d ,r2)))
               (let ((,dst/v (fl-op ,a/v ,f2)))
                 ,(next)))))
         ((and (eq? &flonum a/t) (eq? &scm b/t))
          (with-type-guard &fixnum b/v
            (lambda ()
              `(let ((,r2 (%rsh ,b/v 2)))
                 (let ((,f2 (%i2d ,r2)))
                   (let ((,dst/v (fl-op ,a/v ,f2)))
                     ,(next)))))))
         ((and (eq? &scm a/t) (eq? &scm b/t))
          (with-type-guard &flonum a/v
            (lambda ()
              (with-type-guard &fixnum b/v
                (lambda ()
                  `(let ((,f1 (%cref/f ,a/v 2)))
                     (let ((,f2 (%i2d ,b/v)))
                       (let ((,dst/v (fl-op ,f1 ,f2)))
                         ,(next)))))))))
         (else
          (nyi "~s: et=(flonum fixnum) it=(~a ~a)" 'name
               (pretty-type a/t) (pretty-type b/t))))))
    (define-ir (name (flonum! dst) (fixnum a) (flonum b))
      (let* ((dst/v (var-ref dst))
             (a/v (var-ref a))
             (b/v (var-ref b))
             (r2 (make-tmpvar 2))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2))
             (a/t (type-ref a))
             (b/t (type-ref b)))
        (cond
         ((and (eq? &fixnum a/t) (eq? &flonum b/t))
          `(let ((,r2 (%rsh ,a/v 2)))
             (let ((,f2 (%i2d ,r2)))
               (let ((,dst/v (fl-op ,f2 ,b/v)))
                 ,(next)))))
         ((and (eq? &fixnum a/t) (eq? &scm b/t))
          (with-type-guard &flonum b/v
            (lambda ()
              `(let ((,f1 (%cref/f ,b/v 2)))
                 (let ((,r2 (%rsh ,a/v 2)))
                   (let ((,f2 (%i2d ,r2)))
                     (let ((,dst/v (fl-op ,f2 ,f1)))
                       ,(next))))))))
         ((and (eq? &scm a/t) (eq? &scm b/t))
          (with-type-guard &fixnum a/v
            (lambda ()
              (with-type-guard &flonum b/v
                (lambda ()
                  `(let ((,f2 (%cref/f ,b/v 2)))
                     (let ((,r2 (%rsh ,a/v 2)))
                       (let ((,f1 (%i2d ,r2)))
                         (let ((,dst/v (fl-op ,f1 ,f2)))
                           ,(next))))))))))
         (else
          (nyi "~s: et=(fixnum flonum) it=(~a ~a)"
               'name (pretty-type a/t) (pretty-type b/t))))))
    (define-ir (name (flonum! dst) (flonum a) (flonum b))
      (let* ((dst/v (var-ref dst))
             (a/v (var-ref a))
             (b/v (var-ref b))
             (a/t (type-ref a))
             (b/t (type-ref b))
             (f1 (make-tmpvar/f 1))
             (f2 (make-tmpvar/f 2)))
        (cond
         ((and (eq? &flonum a/t) (eq? &flonum b/t))
          `(let ((,dst/v (fl-op ,a/v ,b/v)))
             ,(next)))
         ((and (eq? &scm a/t) (eq? &scm b/t))
          (with-type-guard &flonum a/v
            (lambda ()
              (with-type-guard &flonum b/v
                (lambda ()
                  `(let ((,f1 (%cref/f ,a/v 2)))
                     (let ((,f2 (%cref/f ,b/v 2)))
                       (let ((,dst/v (fl-op ,f1 ,f2)))
                         ,(next)))))))))
         (else
          (nyi "~s: et=(flonum flonum) it=(~a ~a)" 'name
               (pretty-type a/t) (pretty-type b/t))))))))

(define-mul-div-scm-scm mul %mulov %fmul)
(define-mul-div-scm-scm div %div %fdiv)

(define-syntax define-binary-arith-fx-fx
  (syntax-rules ()
    ((_ name op save-volatiles)
     (begin
       (define-ir (name (scm! dst) (scm a) (scm b))
         (nyi "~s: ~a ~a ~a" 'name dst a b))
       (define-ir (name (fixnum! dst) (fixnum a) (fixnum b))
         (when (and (env-parent-snapshot env) save-volatiles)
           (set-env-save-volatiles! env save-volatiles))
         (let ((dst/v (var-ref dst))
               (a/v (var-ref a))
               (b/v (var-ref b))
               (r1 (make-tmpvar 1))
               (r2 (make-tmpvar 2)))
           `(let ((,r2 (%rsh ,a/v 2)))
              (let ((,r1 (%rsh ,b/v 2)))
                (let ((,r2 (op ,r2 ,r1)))
                  (let ((,r2 (%lsh ,r2 2)))
                    (let ((,dst/v (%add ,r2 2)))
                      ,(next))))))))))))

;; Primitive %mod uses volatile register. Setting `env-save-volatiles?' flag to
;; true.
(define-binary-arith-fx-fx mod %mod #t)
(define-binary-arith-fx-fx quo %quo #f)
(define-binary-arith-fx-fx rem %rem #f)

;; XXX: ash
;; XXX: logand
;; XXX: logior
;; XXX: logxor
;; XXX: make-vector

(define-interrupt-ir (make-vector (scm! dst) (u64 len) (scm ini))
  (let ((dst/v (var-ref dst))
        (len/v (var-ref len))
        (ini/v (var-ref ini))
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

(define-interrupt-ir (make-vector/immediate (scm! dst) (const len) (scm ini))
  (let ((dst/v (var-ref dst))
        (ini/v (var-ref ini))
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
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%cref ,src/v 0)))
       (let ((,dst/v (%rsh ,dst/v 8)))
         ,(next)))))

(define-ir (vector-ref (scm! dst) (vector src) (u64 idx))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src))
        (idx/v (var-ref idx))
        (r2 (make-tmpvar 2)))
    `(let ((_ ,(take-snapshot! ip 0)))
       (let ((,r2 (%cref ,src/v 0)))
         (let ((,r2 (%rsh ,r2 8)))
           (let ((_ (%lt ,idx/v ,r2)))
             (let ((,r2 (%add ,idx/v 1)))
               (let ((,dst/v (%cref ,src/v ,r2)))
                 ,(next)))))))))

(define-ir (vector-ref/immediate (scm! dst) (vector src) (const idx))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src))
        (r2 (make-tmpvar 2)))
    `(let ((,dst/v (%cref ,src/v ,(+ idx 1))))
       ,(next))))

(define-ir (vector-set! (vector dst) (u64 idx) (scm src))
  (let ((dst/v (var-ref dst))
        (idx/v (var-ref idx))
        (src/v (var-ref src))
        (src/t (type-ref src))
        (r1 (make-tmpvar 1))
        (r2 (make-tmpvar 2)))
    `(let ((_ ,(take-snapshot! ip 0)))
       (let ((,r2 (%cref ,dst/v 0)))
         (let ((,r2 (%rsh ,r2 8)))
           (let ((_ (%lt ,idx/v ,r2)))
             ,(if (eq? &flonum src/t)
                  (with-boxing src/t src/v r2
                    (lambda (boxed)
                      `(let ((,r1 (%add ,idx/v 1)))
                         (let ((_ (%cset ,dst/v ,r1 ,boxed)))
                           ,(next)))))
                  `(let ((,r1 (%add ,idx/v 1)))
                     (let ((_ (%cset ,dst/v ,r1 ,src/v)))
                       ,(next))))))))))

(define-ir (vector-set!/immediate (vector dst) (const idx) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src))
        (src/t (type-ref src))
        (r2 (make-tmpvar 2)))
    (if (eq? &flonum src/t)
        (with-boxing src/t src/v r2
          (lambda (boxed)
            `(let ((_ (%cset ,dst/v ,(+ idx 1) ,boxed)))
               ,(next))))
        `(let ((_ (%cset ,dst/v ,(+ idx 1) ,src/v)))
           ,(next)))))

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
;; XXX: umul/immediate

(define-syntax define-binary-arith-u64-u64
  (syntax-rules ()
    ((_ name op)
     (define-ir (name (u64! dst) (u64 a) (u64 b))
       `(let ((,(var-ref dst) (op ,(var-ref a) ,(var-ref b))))
          ,(next))))))

(define-binary-arith-u64-u64 uadd %add)
(define-binary-arith-u64-u64 usub %sub)
(define-binary-arith-u64-u64 umul %mul)

;; XXX: ulogand
;; XXX: ulogior
;; XXX: ulogsub

;; XXX: ursh
;; XXX: ulsh
;; XXX: ursh/immediate
;; XXX: ulsh/immediate
