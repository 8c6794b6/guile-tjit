;;; ANF IR for branching

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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
;;; Module containing ANF IR definitions for branching operations.
;;;
;;; Code:

(define-module (system vm native tjit ir-branch)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-syntax-rule (end-of-root-trace?)
  (and (ir-last-op? ir)
       (not (env-parent-snapshot env))))

(define-syntax-rule (obj->tc7 obj)
  (let ((*obj (scm->pointer obj)))
    (if (zero? (logand (pointer-address *obj) 6))
        (let ((*tc (dereference-pointer *obj)))
          (logand (pointer-address *tc) #x7f))
        #f)))

(define-ir (br (const offset))
  ;; Nothing to emit for br.
  (next))

(define-ir (br-if-true (scm test) (const invert) (const offset))
  (let* ((test/v (var-ref test))
         (test/l (scm-ref test))
         (dest (if (end-of-root-trace?)
                   2
                   (if test/l
                       (if invert offset 2)
                       (if invert 2 offset))))
         (op (if (end-of-root-trace?)
                 (if invert '%eq '%ne)
                 (if test/l '%ne '%eq))))
    `(let ((_ ,(take-snapshot! ip dest)))
       (let ((_ (,op ,test/v #f)))
         ,(next)))))

(define-ir (br-if-null (scm test) (const invert) (const offset))
  (let* ((test/l (scm-ref test))
         (test/v (var-ref test))
         (dest (if (end-of-root-trace?)
                   2
                   (if (null? test/l)
                       (if invert offset 2)
                       (if invert 2 offset))))
         (op (if (end-of-root-trace?)
                 (if invert '%ne '%eq)
                 (if (null? test/l) '%eq '%ne))))
    `(let ((_ ,(take-snapshot! ip dest)))
       (let ((_ (,op ,test/v ())))
         ,(next)))))

;; XXX: br-if-nil

;; XXX: br-if-pair
(define-ir (br-if-pair (scm test) (const invert) (const offset))
  (let* ((test/l (scm-ref test))
         (test/v (var-ref test))
         (dest (if (end-of-root-trace?)
                   2
                   (if (pair? test/l)
                       (if invert offset 2)
                       (if invert 2 offset))))
         (op (if (end-of-root-trace?)
                 (if invert '%tcne '%tceq)
                 (if (pair? test/l) '%tceq '%tcne))))
    `(let ((_ ,(take-snapshot! ip dest)))
       (let ((_ (,op ,test/v 1 ,%tc3-cons)))
         ,(next)))))

;; XXX: br-if-struct
;; (define-ir (br-if-struct (scm test) (const invert) (const offset))
;;   (let* ((test/l (scm-ref test))
;;          (test/v (var-ref test))
;;          (dest (if (end-of-root-trace?)
;;                    2
;;                    (if (struct? test/l)
;;                        (if invert offset 2)
;;                        (if invert 2 offset))))
;;          (op (if (end-of-root-trace?)
;;                  (if invert '%tcne '%tceq)
;;                  (if (struct? test/l) '%tceq '%tcne))))
;;     `(let ((_ ,(take-snapshot! ip dest)))
;;        (let ((_ (,op ,test/v #x7 ,%tc3-struct)))
;;          ,(next)))))

;; XXX: br-if-char

;; XXX: br-if-tc7
;; (define-ir (br-if-tc7 (scm test) (const invert) (const tc7) (const offset))
;;   (let* ((test/l (scm-ref test))
;;          (test/v (var-ref test))
;;          (dest (if (end-of-root-trace?)
;;                    2
;;                    (if (eq? tc7 (obj->tc7 test/l))
;;                        (if invert offset 2)
;;                        (if invert 2 offset))))
;;          (op (if (end-of-root-trace?)
;;                  (if invert '%tcne '%tceq)
;;                  (if (eq? tc7 (obj->tc7 test/l)) '%tceq '%tcne))))
;;     `(let ((_ ,(take-snapshot! ip dest)))
;;        (let ((_ (,op ,test/v #x7f ,tc7)))
;;          ,(next)))))

;; XXX: br-if-eq
(define-ir (br-if-eq (scm a) (scm b) (const invert) (const offset))
  (let* ((a/l (scm-ref a))
         (b/l (scm-ref b))
         (a/v (var-ref a))
         (b/v (var-ref b))
         (dest (if (end-of-root-trace?)
                   3
                   (if (eq? a/l b/l)
                       (if invert offset 3)
                       (if invert 3 offset))))
         (op (if (end-of-root-trace?)
                 (if invert '%ne '%eq)
                 (if (eq? a/l b/l) '%eq '%ne))))
    `(let ((_ ,(take-snapshot! ip dest)))
       (let ((_ (,op ,a/v ,b/v)))
         ,(next)))))

;; XXX: br-if-eqv
;; (define-ir (br-if-eqv (scm a) (scm b) (const invert) (const offset))
;;   (let* ((a/l (scm-ref a))
;;          (b/l (scm-ref b))
;;          (a/v (var-ref a))
;;          (b/v (var-ref b))
;;          (r1 (make-tmpvar 1))
;;          (r2 (make-tmpvar 2))
;;          (dest (if (end-of-root-trace?)
;;                    3
;;                    (if (eq? a/l b/l)
;;                        (if invert offset 3)
;;                        (if invert 3 offset))))
;;          (op (if (end-of-root-trace?)
;;                  (if invert '%nev '%eqv)
;;                  (if (eq? a/l b/l) '%eqv '%nev))))
;;     `(let ((_ ,(take-snapshot! ip dest)))
;;        ,(with-boxing (type-ref a) a/v r2
;;           (lambda (boxed1)
;;             (with-boxing (type-ref b) b/v r1
;;               (lambda (boxed2)
;;                 `(let ((_ (,op ,boxed1 ,boxed2)))
;;                    ,(next)))))))))

;; XXX: br-if-logtest

(define-syntax define-br-binary-body
  (syntax-rules ()
    ((_ name a b invert offset test a-ref b-ref ra rb va vb dest . body)
     (let* ((ra (a-ref a))
            (rb (b-ref b))
            (va (var-ref a))
            (vb (var-ref b))
            (dest (if (end-of-root-trace?)
                      3
                      (if (test ra rb)
                          (if invert offset 3)
                          (if invert 3 offset)))))
       . body))))

(define-syntax define-br-binary-scm-scm-body
  (syntax-rules ()
    ((_ name a b invert offset test ra rb va vb dest . body)
     (define-br-binary-body name a b invert offset test scm-ref scm-ref
       ra rb va vb dest . body))))

(define-syntax define-br-binary-u64-scm-body
  (syntax-rules ()
    ((_ name a b invert offset test ra rb va vb dest . body)
     (define-br-binary-body name a b invert offset test u64-ref scm-ref
       ra rb va vb dest . body))))

(define-syntax define-br-binary-u64-u64-body
  (syntax-rules ()
    ((_ name a b invert offset test ra rb va vb dest . body)
     (define-br-binary-body name a b invert offset test u64-ref u64-ref
       ra rb va vb dest . body))))

(define-syntax define-br-binary-scm-scm
  (syntax-rules ()
    ((_  name op-scm op-fx-t op-fx-f op-fl-t op-fl-f)
     (begin
       ;; XXX: Delegate bignum, complex, rational to C function.
       (define-ir (name (scm a) (scm b) (const invert) (const offset))
         (nyi "~s: ~a ~a ~a ~a" 'name a b invert offset))
       (define-ir (name (fixnum a) (fixnum b) (const invert) (const offset))
         (define-br-binary-scm-scm-body name a b invert offset op-scm
           ra rb va vb dest
           (let* ((op (if (end-of-root-trace?)
                          (if invert 'op-fx-f 'op-fx-t)
                          (if (op-scm ra rb) 'op-fx-t 'op-fx-f)))
                  (a/t (type-ref a))
                  (b/t (type-ref b))
                  (next-thunk (lambda ()
                                `(let ((_ ,(take-snapshot! ip dest)))
                                   (let ((_ (,op ,va ,vb)))
                                     ,(next))))))
             (cond
              ((and (eq? &fixnum a/t) (eq? &fixnum b/t))
               (next-thunk))
              ((and (eq? &scm a/t) (eq? &fixnum b/t))
               (with-type-guard &fixnum va next-thunk))
              ((and (eq? &fixnum a/t) (eq? &scm b/t))
               (with-type-guard &fixnum vb next-thunk))
              ((and (eq? &scm a/t) (eq? &scm b/t))
               (with-type-guard &fixnum va
                 (lambda ()
                   (with-type-guard &fixnum vb next-thunk))))
              (else
               (nyi "~s: et=(fixnum fixnum) it=(~a ~a)" 'name (pretty-type a/t)
                    (pretty-type b/t)))))))
       (define-ir (name (flonum a) (flonum b) (const invert) (const offset))
         (define-br-binary-scm-scm-body name a b invert offset op-scm
           ra rb va vb dest
           (let ((op (if (end-of-root-trace?)
                         (if invert 'op-fl-f 'op-fl-t)
                         (if (op-scm ra rb) 'op-fl-t 'op-fl-f)))
                 (a/t (type-ref a))
                 (b/t (type-ref b)))
             (cond
              ((and (eq? &flonum a/t) (eq? &flonum b/t))
               `(let ((_ ,(take-snapshot! ip dest)))
                  (let ((_ (,op ,va ,vb)))
                    ,(next))))
              ((and (eq? &scm a/t) (eq? &flonum b/t))
               (let ((f2 (make-tmpvar/f 2)))
                 (with-type-guard &flonum va
                   (lambda ()
                     `(let ((,f2 (%cref/f ,va 2)))
                        (let ((_ (,op ,f2 ,vb)))
                          ,(next)))))))
              (else
               (nyi "~s: et=(flonum flonum) it=(~a ~a)" 'name
                    (pretty-type a/t) (pretty-type b/t)))))))))))

(define-br-binary-scm-scm br-if-= = %eq %ne %feq %fne)
(define-br-binary-scm-scm br-if-< < %lt %ge %flt %fge)
(define-br-binary-scm-scm br-if-<= <= %le %gt %fle %fgt)

(define-syntax define-br-binary-u64-u64
  (syntax-rules ()
    ((_ name op-scm op-fx-t op-fx-f)
     (define-ir (name (u64 a) (u64 b) (const invert) (const offset))
       (define-br-binary-u64-u64-body name a b invert offset op-scm
         ra rb va vb dest
         (let ((op (if (end-of-root-trace?)
                       (if invert 'op-fx-f 'op-fx-t)
                       (if (op-scm ra rb) 'op-fx-t 'op-fx-f))))
           `(let ((_ ,(take-snapshot! ip dest)))
              (let ((_ (,op ,va ,vb)))
                ,(next)))))))))

(define-br-binary-u64-u64 br-if-u64-= = %eq %ne)
(define-br-binary-u64-u64 br-if-u64-< < %lt %ge)
(define-br-binary-u64-u64 br-if-u64-<= <= %le %gt)

(define-syntax define-br-binary-u64-scm
  (syntax-rules ()
    ((_ name op-scm op-fx-t op-fx-f)
     (begin
       (define-ir (name (u64 a) (scm b) (const invert) (const offset))
         (nyi "~s: ~a ~a ~a ~a" 'name a b invert offset))
       (define-ir (name (u64 a) (fixnum b) (const invert) (const offset))
         (define-br-binary-u64-scm-body name a b invert offset op-scm
           ra rb va vb dest
           (let* ((r2 (make-tmpvar 2))
                  (b/t (type-ref b))
                  (op (if (end-of-root-trace?)
                          (if invert 'op-fx-f 'op-fx-t)
                          (if (op-scm ra rb) 'op-fx-t 'op-fx-f)))
                  (next-thunk
                   (lambda ()
                     `(let ((_ ,(take-snapshot! ip dest)))
                        (let ((,r2 (%rsh ,vb 2)))
                          (let ((_ (,op ,va ,r2)))
                            ,(next)))))))
             (cond
              ((eq? &fixnum b/t)
               (next-thunk))
              ((eq? &scm b/t)
               (with-type-guard &fixnum vb next-thunk))
              (else
               (nyi "~s: et=fixnum it=~a" 'name (pretty-type b/t)))))))))))

(define-br-binary-u64-scm br-if-u64-=-scm = %eq %ne)
(define-br-binary-u64-scm br-if-u64-<-scm < %lt %ge)
(define-br-binary-u64-scm br-if-u64-<=-scm <= %le %gt)
(define-br-binary-u64-scm br-if-u64->-scm > %gt %le)
(define-br-binary-u64-scm br-if-u64->=-scm >= %ge %lt)
