;;; -*- mode: scheme; coding: utf-8; -*-

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

;;; Compile traced bytecode to CPS intermediate representation via
;;; (almost) ANF Scheme.

;;; Code:

(define-module (system vm native tjit ir)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps2)
  #:use-module (language cps2 optimize)
  #:use-module (language cps2 renumber)
  #:use-module (language scheme spec)
  #:use-module (system base compile)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:export (trace->cps
            trace->scm
            scm->cps))

(define (accumulate-locals ops)
  (define (nyi op)
    (debug 2 "ir:accumulate-locals: NYI ~a~%" op))
  (define-syntax-rule (add! st i)
    (intset-add! st i))
  (define-syntax-rule (add2! st i j)
    (add! (add! st i) j))
  (define-syntax-rule (add3! st i j k)
    (add! (add2! st i j) k))
  (define (acc-one st op rest)
    (match op
      ((op a1)
       (case op
         ((br)
          (acc st rest))
         (else
          (nyi op)
          (acc st rest))))
      ((op a1 a2)
       (case op
         ((make-short-immediate
           make-long-immediate
           make-long-long-immediate
           static-ref)
          (acc (add! st a1) rest))
         ((mov sub1 add1 box-ref)
          (acc (add2! st a1 a2) rest))
         (else
          (nyi op)
          (acc st rest))))
      ((op a1 a2 a3)
       (case op
         ((add sub mul div quo)
          (acc (add3! st a1 a2 a3) rest))
         (else
          (nyi op)
          (acc st rest))))
      ((op a1 a2 a3 a4)
       (case op
         ((br-if-< br-if-= br-if-<=)
          (acc (add2! st a1 a2) rest))
         (else
          (nyi op)
          (acc st rest))))
      ((op a1 a2 a3 a4 a5)
       (case op
         ((toplevel-box)
          (acc (add! st a1) rest))
         (else
          (nyi op)
          (acc st rest))))
      (op
       (nyi op)
       (acc st rest))
      (_
       (error (format #f "ir:accumulate-locals: ~a" ops)))))
  (define (acc st ops)
    (match ops
      (((op ip . locals) . rest)
       (acc-one st op rest))
      (()
       st)))

  (intset-fold cons (acc empty-intset ops) '()))

;; XXX: Invoke functions in lightning to get this number.
(define *num-registers* 6)

(define (allocate-registers nlocals is)
  ;; Naive procedure to assign registers to locals. Does nothing
  ;; intellectual such as graph-coloring, linear-scan or bin-pack.
  (let lp ((is is)
           (regs (iota *num-registers*))
           (acc (make-vector nlocals #f)))
    (cond
     ((null? is)
      acc)
     ((null? regs)
      (debug 2 "local[~a]=~a~%" (car is) #f)
      (lp (cdr is) regs acc))
     (else
      (debug 2 "local[~a]=reg~a~%" (car is) (car regs))
      (vector-set! acc (car is) (car regs))
      (lp (cdr is) (cdr regs) acc)))))

(define (trace->scm ops)
  (define (dereference-scm addr)
    (pointer->scm (dereference-pointer (make-pointer addr))))
  (define (convert-one st escape ip op rest)
    (match op
      (('make-short-immediate dst low-bits)
       `(let ((,(vector-ref st dst) ,low-bits))
          ,(convert st escape rest)))

      (('make-long-immediate dst low-bits)
       `(let ((,(vector-ref st dst) ,low-bits))
          ,(convert st escape rest)))

      (('static-ref dst offset)
       `(let ((,(vector-ref st dst) ,(dereference-scm (+ ip (* 4 offset)))))
          ,(convert st escape rest)))

      (('br-if-< a b invert? offset)
       (let ((va (vector-ref st a))
             (vb (vector-ref st b)))
         `(if ,(if invert? `(< ,vb ,va) `(< ,va ,vb))
              ,ip
              ,(convert st escape rest))))

      (('br-if-= a b invert? offset)
       (let ((va (vector-ref st a))
             (vb (vector-ref st b)))
         `(if ,(if invert? `(not (= ,va ,vb)) `(= ,va ,vb))
              ,ip
              ,(convert st escape rest))))

      (('mov dst src)
       (let ((vdst (vector-ref st dst))
             (vsrc (vector-ref st src)))
         `(let ((,vdst ,vsrc))
            ,(convert st escape rest))))

      (('box-ref dst src)
       (let ((vdst (vector-ref st dst))
             (vsrc (vector-ref st src)))
         `(let ((,vdst (variable-ref ,vsrc)))
            ,(convert st escape rest))))

      (('toplevel-box dst var-offset mod-offset sym-offset bound?)
       (let ((vdst (vector-ref st dst))
             (var (dereference-scm (+ ip (* var-offset 4)))))
         `(let ((,vdst (make-variable ,(variable-ref var))))
            ,(convert st escape rest))))

      (('add dst a b)
       (let ((vdst (vector-ref st dst))
             (va (vector-ref st a))
             (vb (vector-ref st b)))
         `(let ((,vdst (+ ,va ,vb)))
            ,(convert st escape rest))))

      (('add1 dst src)
       (let ((vdst (vector-ref st dst))
             (vsrc (vector-ref st src)))
         `(let ((,vdst (+ ,vsrc 1)))
            ,(convert st escape rest))))

      (('sub dst a b)
       (let ((vdst (vector-ref st dst))
             (va (vector-ref st a))
             (vb (vector-ref st b)))
         `(let ((,vdst (- ,va ,vb)))
            ,(convert st escape rest))))

      (('sub1 dst src)
       (let ((vdst (vector-ref st dst))
             (vsrc (vector-ref st src)))
         `(let ((,vdst (- ,vsrc 1)))
            ,(convert st escape rest))))

      ;; (('mul dst a b)
      ;;  (let ((vdst (vector-ref st dst))
      ;;        (va (vector-ref st a))
      ;;        (vb (vector-ref st b)))
      ;;    `(let ((,vdst (* ,va ,vb)))
      ;;       ,(convert st escape rest))))

      (('br offset)
       (convert st escape rest))

      (op
       (debug 2 "ir:convert: NYI ~a~%" (car op))
       (escape #f))))

  (define (convert st escape ops)
    (match ops
      (((op ip . locals) . rest)
       (convert-one st escape ip op rest))
      (()
       `(loop ,@(filter identity (vector->list st))))))

  (define (make-var index)
    (string->symbol (string-append "v" (number->string index))))

  (define (make-vars nlocals locals)
    (let lp ((i nlocals) (locals locals) (acc '()))
      (cond
       ((< i 0)
        (list->vector acc))
       ((null? locals)
        (lp (- i 1) locals (cons #f acc)))
       ((= i (car locals))
        (lp (- i 1) (cdr locals) (cons (make-var i) acc)))
       (else
        (lp (- i 1) locals (cons #f acc))))))

  (let* ((locals (accumulate-locals ops))
         (max-local-num (or (and (null? locals) 0)
                            (apply max locals)))
         (regs (allocate-registers (+ max-local-num 1) (reverse locals)))
         (args (map (lambda (i) (make-var i))
                    (reverse locals)))
         (vars (make-vars max-local-num locals))
         (scm (call-with-escape-continuation
               (lambda (cont)
                 `(letrec ((loop (lambda ,args
                                   ,(convert vars cont ops))))
                    loop)))))
    (debug 2 "locals: ~a~%" locals)
    (debug 2 "regs: ~a~%" regs)
    (values regs scm)))

(define (scm->cps scm)
  (define (loop-body cps)
    (define (go cps k)
      (match (intmap-ref cps k)
        (($ $kfun _ _ _ _ next)
         (go (intmap-remove cps k) next))
        (($ $kclause _ next _)
         (go (intmap-remove cps k) next))
        (($ $kargs _ _ ($ $continue next _ expr))
         (go (intmap-remove cps k) next))
        (($ $ktail)
         (values (+ k 1) (intmap-remove cps k)))
        (_
         (error "loop-body: got ~a" (intmap-ref cps k)))))
    (call-with-values
        (lambda ()
          (set! cps (optimize cps))
          (set! cps (renumber cps))
          (go cps 0))
      (lambda (k cps)
        (set! cps (renumber cps k))
        cps)))

  (let ((cps (and scm (compile scm #:from 'scheme #:to 'cps2))))
    ;; (debug 2 ";;;; scm~%~a"
    ;;        (or (and scm
    ;;                 (call-with-output-string
    ;;                  (lambda (port)
    ;;                    (pretty-print (cadr (caadr scm)) #:port port))))
    ;;            "failed\n"))
    (set! cps (and cps (loop-body cps)))
    cps))

(define (trace->cps trace)
  (call-with-values (lambda () (trace->scm trace))
    (lambda (regs scm)
      (values regs (scm->cps scm)))))
