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
  #:use-module ((srfi srfi-1) #:select (every))
  #:use-module (system base compile)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit primitives)
  #:export (trace->cps
            trace->scm
            scm->cps
            accumulate-locals))


;;;
;;; Type check
;;;

(define (fixnums? . args)
  (define (fixnum? n)
    (and (exact? n)
         (< n most-positive-fixnum)
         (> n most-negative-fixnum)))
  (every fixnum? args))

(define (inexacts? a b)
  (and (inexact? a) (inexact? b)))


;;;
;;; Locals
;;;

(define (accumulate-locals ops)
  (define (nyi op)
    (debug 2 "ir:accumulate-locals: NYI ~a~%" op))

  (define-syntax-rule (add! st i)
    (intset-add! st i))
  (define-syntax-rule (add2! st i j)
    (add! (add! st i) j))
  (define-syntax-rule (add3! st i j k)
    (add! (add2! st i j) k))

  (define (acc-one st op)
    (match op
      ((op a1)
       (case op
         ((br) st)
         (else st)))
      ((op a1 a2)
       (case op
         ((make-short-immediate
           make-long-immediate
           make-long-long-immediate
           static-ref)
          (add! st a1))
         ((mov sub1 add1 box-ref)
          (add2! st a1 a2))
         (else
          (nyi op)
          st)))
      ((op a1 a2 a3)
       (case op
         ((add sub mul div quo)
          (add3! st a1 a2 a3))
         (else
          (nyi op)
          st)))
      ((op a1 a2 a3 a4)
       (case op
         ((br-if-< br-if-= br-if-<=)
          (add2! st a1 a2))
         (else
          (nyi op)
          st)))
      ((op a1 a2 a3 a4 a5)
       (case op
         ((toplevel-box)
          (add! st a1))
         (else
          (nyi op)
          st)))
      (op
       (nyi op)
       st)
      (_
       (error (format #f "ir:accumulate-locals: ~a" ops)))))

  (define (acc st ops)
    (match ops
      (((op ip . locals) . rest)
       (acc (acc-one st op) rest))
      (()
       st)))

  (intset-fold cons (acc empty-intset ops) '()))

(define (trace->scm ops)
  (define (dereference-scm addr)
    (pointer->scm (dereference-pointer (make-pointer addr))))

  (define (restore-frame! vars)
    (let ((end (vector-length vars)))
      (let lp ((i 0) (acc '()))
        (cond
         ((= i end)
          (reverse! acc))
         ((vector-ref vars i)
          =>
          (lambda (var)
            (lp (+ i 1) (cons `(%frame-set! ,i ,var) acc))))
         (else
          (lp (+ i 1) acc))))))

  ;; XXX: Add prologue for native functio, move calls to `%guard-fx' in <, add,
  ;; add1, sub ... etc to the prologue.
  (define (convert-one st escape op ip locals rest)
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
       (let ((ra (vector-ref locals a))
             (rb (vector-ref locals b))
             (va (vector-ref st a))
             (vb (vector-ref st b)))
         (cond
          ((fixnums? ra rb)
           `(if ,(if invert? `(%fx< ,vb ,va) `(%fx< ,va ,vb))
                (begin
                  ,@(restore-frame! st)
                  ,ip)
                ,(convert st escape rest)))
          (else
           (debug 2 "ir:convert < ~a ~a~%" ra rb)
           (escape #f)))))

      (('br-if-= a b invert? offset)
       (let ((va (vector-ref st a))
             (vb (vector-ref st b)))
         `(if ,(if invert? `(not (= ,va ,vb)) `(= ,va ,vb))
              (begin
                ,@(restore-frame! st)
                ,ip)
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
       (let ((rdst (vector-ref locals dst))
             (ra (vector-ref locals a))
             (rb (vector-ref locals b))
             (vdst (vector-ref st dst))
             (va (vector-ref st a))
             (vb (vector-ref st b)))
         (cond
          ((fixnums? rdst ra rb)
           `(let ((,vdst (%fxadd ,va ,vb)))
              ,(convert st escape rest)))
          (else
           (debug 2 "ir:convert add ~a ~a ~a" rdst ra rb)
           (escape #f)))))

      (('add1 dst src)
       (let ((rdst (vector-ref locals dst))
             (rsrc (vector-ref locals src))
             (vdst (vector-ref st dst))
             (vsrc (vector-ref st src)))
         (cond
          ((fixnums? rdst rsrc)
           `(let ((,vdst (%fxadd1 ,vsrc)))
              ,(convert st escape rest)))
          (else
           (debug 2 "ir:convert add1 ~a ~a" rdst rsrc)
           (escape #f)))))

      (('sub dst a b)
       (let ((rdst (vector-ref locals dst))
             (ra (vector-ref locals a))
             (rb (vector-ref locals b))
             (vdst (vector-ref st dst))
             (va (vector-ref st a))
             (vb (vector-ref st b)))
         (cond
          ((fixnums? rdst ra rb)
           `(let ((,vdst (%fxsub ,va ,vb)))
              ,(convert st escape rest)))
          (else
           (debug 2 "ir:convert sub ~a ~a ~a~%" rdst ra rb)
           (escape #f)))))

      (('sub1 dst src)
       (let ((rdst (vector-ref locals dst))
             (rsrc (vector-ref locals src))
             (vdst (vector-ref st dst))
             (vsrc (vector-ref st src)))
         (cond
          ((fixnums? rdst rsrc)
           `(let ((,vdst (%fxsub1 ,vsrc)))
              ,(convert st escape rest)))
          (else
           (debug 2 "ir:convert sub1 ~a ~a" rdst rsrc)
           (escape #f)))))

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

  (define (convert st escape env)
    (match env
      (((op ip . locals) . rest)
       (convert-one st escape op ip locals rest))
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
         (args (map make-var (reverse locals)))
         (vars (make-vars max-local-num locals))
         (scm (call-with-escape-continuation
               (lambda (escape)
                 `(letrec ((loop (lambda ,args
                                   ,(convert vars escape ops))))
                    loop)))))
    (debug 2 ";;; locals: ~a~%" (sort locals <))
    (values locals scm)))

(define (scm->cps scm)
  (define (body-fun cps)
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
         (error "body-fun: got ~a" (intmap-ref cps k)))))
    (call-with-values
        (lambda ()
          (set! cps (optimize cps))
          (set! cps (renumber cps))
          (go cps 0))
      (lambda (k cps)
        (set! cps (renumber cps k))
        cps)))

  (let ((cps (and scm (compile scm #:from 'scheme #:to 'cps2))))
    (set! cps (and cps (body-fun cps)))
    cps))

(define (trace->cps trace)
  (call-with-values (lambda () (trace->scm trace))
    (lambda (locals scm)
      (debug 2 ";;; scm~%~a"
             (or (and scm
                      (call-with-output-string
                       (lambda (port)
                         (pretty-print scm #:port port))))
                 "failed\n"))
      (values locals (scm->cps scm)))))
