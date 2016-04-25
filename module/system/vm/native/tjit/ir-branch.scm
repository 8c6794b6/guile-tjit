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
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables))

(define-syntax ensure-loop
  (syntax-rules ()
    ((_ test invert offset op-size)
     (when (ir-last-op? ir)
       (let* ((jump (if invert
                        (if test op-size offset)
                        (if test offset op-size)))
              (entry-ip (or (and=> (env-linked-fragment env)
                                   fragment-entry-ip)
                            (env-entry-ip env)))
              (dest-ip (+ ip (* 4 jump))))
         (unless (= entry-ip dest-ip)
           (retrace "trace not looping")))))))

(define-syntax define-br-unary
  (syntax-rules ()
    ((_ name op-scm op-t op-f args)
     (define-ir (name (scm test) (const invert) (const offset))
       (let* ((test/v (var-ref test))
              (test/l (scm-ref test))
              (test/r (op-scm test/l))
              (dest (if test/r
                        (if invert offset 2)
                        (if invert 2 offset)))
              (op (if test/r 'op-t 'op-f)))
         (ensure-loop test/r invert offset 2)
         `(let ((_ ,(take-snapshot! ip dest)))
            (let ((_ (,op ,test/v . args)))
              ,(next))))))))

(define-syntax br-binary-body
  (syntax-rules ()
    ((_ a b invert offset test a-ref b-ref a/l b/l a/v b/v dest . body)
     (let* ((a/l (a-ref a))
            (b/l (b-ref b))
            (a/v (var-ref a))
            (b/v (var-ref b))
            (dest (if (test a/l b/l)
                      (if invert offset 3)
                      (if invert 3 offset))))
       . body))))

(define-syntax br-binary-scm-scm-body
  (syntax-rules ()
    ((_ a b invert offset test ra rb va vb dest . body)
     (br-binary-body a b invert offset test scm-ref scm-ref
       ra rb va vb dest . body))))

(define-syntax br-binary-u64-scm-body
  (syntax-rules ()
    ((_ a b invert offset test ra rb va vb dest . body)
     (br-binary-body a b invert offset test u64-ref scm-ref
       ra rb va vb dest . body))))

(define-syntax br-binary-u64-u64-body
  (syntax-rules ()
    ((_ a b invert offset test ra rb va vb dest . body)
     (br-binary-body a b invert offset test u64-ref u64-ref
       ra rb va vb dest . body))))

;; Nothing to emit for br.
(define-ir (br (const offset))
  (next))

(define-br-unary br-if-true (lambda (x) x) %ne %eq (#f))
(define-br-unary br-if-null null? %eq %ne (()))
(define-br-unary br-if-pair pair? %tceq %tcne (1 ,%tc3-cons))

;; XXX: br-if-nil

(define-br-unary br-if-struct struct? %tceq %tcne (7 ,%tc3-struct))

(define-ir (br-if-char (scm test) (const invert) (const offset))
  (let* ((test/v (var-ref test))
         (test/l (scm-ref test))
         (test/r (char? test/l))
         (dest (if test/r
                   (if invert offset 2)
                   (if invert 2 offset)))
         (op (if test/r '%eq '%ne))
         (r2 (make-tmpvar 2)))
    (ensure-loop test/r invert offset 2)
    `(let ((_ ,(take-snapshot! ip dest)))
       (let ((,r2 (%band ,test/v #xf)))
         (let ((_ (,op ,r2 ,%tc8-char)))
           ,(next))))))

(define-syntax-rule (obj->tc7 obj)
  (let ((ptr (scm->pointer obj)))
    (if (zero? (logand (pointer-address ptr) 6))
        (let ((cell-type (dereference-pointer ptr)))
          (logand (pointer-address cell-type) #x7f))
        #f)))

(define-ir (br-if-tc7 (scm test) (const invert) (const tc7) (const offset))
  (let* ((test/l (scm-ref test))
         (test/v (var-ref test))
         (tc7/l (obj->tc7 test/l))
         (dest (if (eq? tc7 tc7/l)
                   (if invert offset 2)
                   (if invert 2 offset)))
         (op (if (eq? tc7 tc7/l) '%tceq '%tcne)))
    (ensure-loop (eq? tc7 tc7/l) invert offset 2)
    `(let ((_ ,(take-snapshot! ip dest)))
       (let ((_ (,op ,test/v #x7f ,tc7)))
         ,(next)))))

(define-ir (br-if-eq (scm a) (scm b) (const invert) (const offset))
  (br-binary-scm-scm-body
   a b invert offset eq? a/l b/l a/v b/v dest
   (let* ((test/l (eq? a/l b/l))
          (op (if test/l '%eq '%ne)))
     (ensure-loop test/l invert offset 3)
     `(let ((_ ,(take-snapshot! ip dest)))
        (let ((_ (,op ,a/v ,b/v)))
          ,(next))))))

(define-ir (br-if-eqv (scm a) (scm b) (const invert) (const offset))
  (br-binary-scm-scm-body
   a b invert offset eqv? a/l b/l a/v b/v dest
   (let* ((test/l (eqv? a/l b/l))
          (op (if test/l '%eqv '%nev))
          (r1 (make-tmpvar 1))
          (r2 (make-tmpvar 2)))
     (ensure-loop test/l invert offset 3)
     `(let ((_ ,(take-snapshot! ip dest)))
        ,(with-boxing (type-ref a) a/v r2
           (lambda (boxed1)
             (with-boxing (type-ref b) b/v r1
               (lambda (boxed2)
                 `(let ((_ (,op ,boxed1 ,boxed2)))
                    ,(next))))))))))

(define-ir (br-if-eqv (flonum a) (flonum b) (const invert) (const offset))
  (br-binary-scm-scm-body
   a b invert offset eqv? a/l b/l a/v b/v dest
   (let* ((test/l (eqv? a/l b/l))
          (op (if test/l '%eq '%ne)))
     (ensure-loop test/l invert offset 3)
     `(let ((_ ,(take-snapshot! ip dest)))
        (let ((_ (,op ,a/v ,b/v)))
          ,(next))))))

;; XXX: br-if-logtest

(define-syntax define-br-binary-arith-scm-scm
  (syntax-rules ()
    ((_  name op-scm op-fx-t op-fx-f op-fl-t op-fl-f)
     (begin
       (define-ir (name (scm a) (scm b) (const invert) (const offset))
         ;; XXX: Delegate bignum, complex, and rational to C function.
         (nyi "~s: et=(scm scm) it=(~a ~a)" 'name
              (pretty-type (type-ref a)) (pretty-type (type-ref b))))
       (define-ir (name (fixnum a) (fixnum b) (const invert) (const offset))
         (br-binary-scm-scm-body
          a b invert offset op-scm ra rb va vb dest
          (let* ((op (if (op-scm ra rb) 'op-fx-t 'op-fx-f))
                 (a/t (type-ref a))
                 (b/t (type-ref b))
                 (next-thunk (lambda ()
                               `(let ((_ ,(take-snapshot! ip dest)))
                                  (let ((_ (,op ,va ,vb)))
                                    ,(next))))))
            (ensure-loop (op-scm ra rb) invert offset 3)
            (cond
             ((and (eq? &fixnum a/t) (eq? &fixnum b/t))
              (next-thunk))
             ((and (eq? &scm a/t) (eq? &fixnum b/t))
              (with-type-guard &fixnum va (next-thunk)))
             ((and (eq? &fixnum a/t) (eq? &scm b/t))
              (with-type-guard &fixnum vb (next-thunk)))
             ((and (eq? &scm a/t) (eq? &scm b/t))
              (with-type-guard &fixnum va
                (with-type-guard &fixnum vb
                  (next-thunk))))
             (else
              (nyi "~s: et=(fixnum fixnum) it=(~a ~a)" 'name (pretty-type a/t)
                   (pretty-type b/t)))))))
       (define-ir (name (flonum a) (fixnum b) (const invert) (const offset))
         (br-binary-scm-scm-body
          a b invert offset op-scm a/l b/l a/v b/v dest
          (let* ((op (if (op-scm a/l b/l) 'op-fl-t 'op-fl-f))
                 (r2 (make-tmpvar 2))
                 (f2 (make-tmpvar/f 2))
                 (a/t (type-ref a))
                 (b/t (type-ref b))
                 (next-thunk
                  (lambda ()
                    `(let ((_ ,(take-snapshot! ip dest)))
                       (let ((,r2 (%rsh ,b/v 2)))
                         (let ((,f2 (%i2d ,r2)))
                           (let ((_ (,op ,a/v ,f2)))
                             ,(next))))))))
            (ensure-loop (op-scm a/l b/l) invert offset 3)
            (cond
             ((and (eq? &flonum a/t) (eq? &fixnum b/t))
              (next-thunk))
             ((and (eq? &flonum a/t) (eq? &scm b/t))
              (with-type-guard &fixnum b/v (next-thunk)))
             ((and (eq? &scm a/t) (eq? &fixnum b/t))
              (with-type-guard &flonum a/v (next-thunk)))
             ((and (eq? &scm a/t) (eq? &scm b/t))
              (with-type-guard &flonum a/v
                (with-type-guard &fixnum b/v
                  (next-thunk))))
             (else
              (nyi "~s" 'name))))))
       (define-ir (name (flonum a) (flonum b) (const invert) (const offset))
         (br-binary-scm-scm-body
          a b invert offset op-scm ra rb va vb dest
          (let ((op (if (op-scm ra rb) 'op-fl-t 'op-fl-f))
                (a/t (type-ref a))
                (b/t (type-ref b))
                (f1 (make-tmpvar/f 1))
                (f2 (make-tmpvar/f 2)))
            (ensure-loop (op-scm ra rb) invert offset 3)
            (cond
             ((and (eq? &flonum a/t) (eq? &flonum b/t))
              `(let ((_ ,(take-snapshot! ip dest)))
                 (let ((_ (,op ,va ,vb)))
                   ,(next))))
             ((and (eq? &flonum a/t) (eq? &scm b/t))
              (with-type-guard &flonum vb
                `(let ((_ ,(take-snapshot! ip dest)))
                   (let ((,f2 (%cref/f ,vb 2)))
                     (let ((_ (,op ,va ,f2)))
                       ,(next))))))
             ((and (eq? &scm a/t) (eq? &flonum b/t))
              (with-type-guard &flonum va
                `(let ((_ ,(take-snapshot! ip dest)))
                   (let ((,f2 (%cref/f ,va 2)))
                     (let ((_ (,op ,f2 ,vb)))
                       ,(next))))))
             ((and (eq? &scm a/t) (eq? &scm b/t))
              (with-type-guard &flonum va
                (with-type-guard &flonum vb
                  `(let ((_ ,(take-snapshot! ip dest)))
                     (let ((,f1 (%cref/f ,va 2)))
                       (let ((,f2 (%cref/f ,vb 2)))
                         (let ((_ (,op ,f1 ,f2)))
                           ,(next))))))))
             (else
              (nyi "~s: et=(flonum flonum) it=(~a ~a)" 'name
                   (pretty-type a/t) (pretty-type b/t)))))))))))

(define-br-binary-arith-scm-scm br-if-= = %eq %ne %feq %fne)
(define-br-binary-arith-scm-scm br-if-< < %lt %ge %flt %fge)
(define-br-binary-arith-scm-scm br-if-<= <= %le %gt %fle %fgt)

(define-syntax define-br-binary-arith-u64-u64
  (syntax-rules ()
    ((_ name op-scm op-fx-t op-fx-f)
     (define-ir (name (u64 a) (u64 b) (const invert) (const offset))
       (br-binary-u64-u64-body a b invert offset op-scm ra rb va vb dest
         (let ((op (if (op-scm ra rb) 'op-fx-t 'op-fx-f)))
           (ensure-loop (op-scm ra rb) invert offset 3)
           `(let ((_ ,(take-snapshot! ip dest)))
              (let ((_ (,op ,va ,vb)))
                ,(next)))))))))

(define-br-binary-arith-u64-u64 br-if-u64-= = %eq %ne)
(define-br-binary-arith-u64-u64 br-if-u64-< < %lt %ge)
(define-br-binary-arith-u64-u64 br-if-u64-<= <= %le %gt)

(define-syntax define-br-binary-arith-u64-scm
  (syntax-rules ()
    ((_ name op-scm op-fx-t op-fx-f op-fl-t op-fl-f)
     (begin
       (define-ir (name (u64 a) (scm b) (const invert) (const offset))
         (nyi "~s: et=(u64 scm) it=(u64 ~a)" 'name
              (pretty-type (type-ref b))))
       (define-ir (name (u64 a) (flonum b) (const invert) (const offset))
         (br-binary-u64-scm-body
          a b invert offset op-scm a/l b/l a/v b/v dest
          (let ((f1 (make-tmpvar/f 1))
                (f2 (make-tmpvar/f 2))
                (b/t (type-ref b))
                (op (if (op-scm a/l b/l) 'op-fl-t 'op-fl-f)))
            (ensure-loop (op-scm a/l b/l) invert offset 3)
            (cond
             ((eq? &flonum b/t)
              `(let ((_ ,(take-snapshot! ip dest)))
                 (let ((,f2 (%i2d ,a/v)))
                   (let ((_ (,op ,f2 ,b/v)))
                     ,(next)))))
             ((eq? &scm b/t)
              (with-type-guard &flonum b/v
                `(let ((_ ,(take-snapshot! ip dest)))
                   (let ((,f1 (%i2d ,a/v)))
                     (let ((,f2 (%cref/f ,b/v 2)))
                       (let ((_ (,op ,f1 ,f2)))
                         ,(next)))))))
             (else
              (nyi "~s: et=(u64 flonum) it=(u64 ~a)" 'name
                   (pretty-type (type-ref b))))))))
       (define-ir (name (u64 a) (fixnum b) (const invert) (const offset))
         (br-binary-u64-scm-body
          a b invert offset op-scm ra rb va vb dest
          (let* ((r2 (make-tmpvar 2))
                 (b/t (type-ref b))
                 (op (if (op-scm ra rb) 'op-fx-t 'op-fx-f))
                 (next-thunk
                  (lambda ()
                    `(let ((_ ,(take-snapshot! ip dest)))
                       (let ((,r2 (%rsh ,vb 2)))
                         (let ((_ (,op ,va ,r2)))
                           ,(next)))))))
            (ensure-loop (op-scm ra rb) invert offset 3)
            (cond
             ((eq? &fixnum b/t)
              (next-thunk))
             ((eq? &scm b/t)
              (with-type-guard &fixnum vb (next-thunk)))
             (else
              (nyi "~s: et=fixnum it=~a" 'name (pretty-type b/t)))))))))))

(define-br-binary-arith-u64-scm br-if-u64-=-scm = %eq %ne %eq %ne)
(define-br-binary-arith-u64-scm br-if-u64-<-scm < %lt %ge %flt %fge)
(define-br-binary-arith-u64-scm br-if-u64-<=-scm <= %le %gt %fle %fgt)
(define-br-binary-arith-u64-scm br-if-u64->-scm > %gt %le %fgt %fle)
(define-br-binary-arith-u64-scm br-if-u64->=-scm >= %ge %lt %fge %flt)
