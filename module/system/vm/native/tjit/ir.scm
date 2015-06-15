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

;;; Compile traced bytecode to CPS intermediate representation.

(define-module (system vm native tjit ir)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps intset)
  #:use-module (system foreign)
  #:export (bytecode->scm))

(define (accumulate-locals ops)
  (define (acc st ops)
    (define-syntax-rule (add! st i)
      (intset-add! st i))
    (define-syntax-rule (add2! st i j)
      (add! (add! st i) j))
    (define-syntax-rule (add3! st i j k)
      (add! (add2! st i j) k))
    (match ops
      (((op a1) . rest)
       (case op
         ((br)
          (acc st rest))
         (else
          (format #t "ir:accumulate-locals: NYI ~a~%" op)
          (acc st rest))))

      (((op a1 a2) . rest)
       (case op
         ((make-short-immediate make-long-immediate make-long-long-immediate)
          (acc (add! st a1) rest))
         ((mov sub1 add1 box-ref)
          (acc (add2! st a1 a2) rest))
         (else
          (format #t "ir:accumulate-locals: NYI ~a~%" op)
          (acc st rest))))

      (((op a1 a2 a3) . rest)
       (case op
         ((add sub mul div quo)
          (acc (add3! st a1 a2 a3) rest))
         (else
          (format #t "ir:accumulate-locals: NYI ~a~%" op)
          (acc st rest))))

      (((op a1 a2 _ _) . rest)
       (case op
         ((br-if-< br-if-= br-if-<=)
          (acc (add2! st a1 a2) rest))
         (else
          (format #t "ir:accumulate-locals: NYI ~a~%" op)
          (acc st rest))))

      (((op a1 a2 a3 a4 a5) . rest)
       (case op
         ((toplevel-box)
          (acc (add! st a1) rest))
         (else
          (format #t "ir:accumulate-locals: NYI ~a~%" op)
          (acc st rest))))

      ((op . rest)
       (format #t "ir:accumulate-locals: NYI ~a~%" (car op))
       (acc st rest))

      (()
       st)
      (else
       (error (format #f "ir:accumulate-locals: ~a" ops)))))

  (intset-fold cons (acc empty-intset ops) '()))

(define (bytecode->scm ops)
  (let ((escape #f))
    (define (convert st ops)
      (match ops
        ((('make-short-immediate dst low-bits) . rest)
         `(let ((,(vector-ref st dst) ,low-bits))
            ,(convert st rest)))

        ((('make-long-immediate dst low-bits) . rest)
         `(let ((,(vector-ref st dst) ,low-bits))
            ,(convert st rest)))

        ;; ((('toplevel-box dst var-offset mod-offset sym-offset bound?) . rest)
        ;;  `(let ((,(vector-ref st dst) ,var))
        ;;     ,(convert st rest)))

        ((('br-if-< a b invert? offset) . rest)
         (let ((va (vector-ref st a))
               (vb (vector-ref st b)))
           `(if ,(if invert? `(< ,vb ,va) `(< ,va ,vb))
                ,offset
                ,(convert st rest))))

        ((('br-if-= a b invert? offset) . rest)
         (let ((va (vector-ref st a))
               (vb (vector-ref st b)))
           `(if ,(if invert? `(not (= ,va ,vb)) `(= ,va ,vb))
                ,offset
                ,(convert st rest))))

        ((('mov dst src) . rest)
         (let ((vdst (vector-ref st dst))
               (vsrc (vector-ref st src)))
           `(let ((,vdst ,vsrc))
              ,(convert st rest))))

        ((('add dst a b) . rest)
         (let ((vdst (vector-ref st dst))
               (va (vector-ref st a))
               (vb (vector-ref st b)))
           `(let ((,vdst (+ ,va ,vb)))
              ,(convert st rest))))

        ((('sub dst a b) . rest)
         (let ((vdst (vector-ref st dst))
               (va (vector-ref st a))
               (vb (vector-ref st b)))
           `(let ((,vdst (- ,va ,vb)))
              ,(convert st rest))))

        ((('add1 dst src) . rest)
         (let ((vdst (vector-ref st dst))
               (vsrc (vector-ref st src)))
           `(let ((,vdst (+ ,vsrc 1)))
              ,(convert st rest))))

        ((('sub1 dst src) . rest)
         (let ((vdst (vector-ref st dst))
               (vsrc (vector-ref st src)))
           `(let ((,vdst (- ,vsrc 1)))
              ,(convert st rest))))

        ((('br offset) . rest)
         (convert st rest))

        ((op . rest)
         (format #t "ir.scm:convert: NYI ~a~%" (car op))
         (escape #f))

        (()
         `(loop ,@(vector->list st)))))

    (define (make-var index)
      (string->symbol (string-append "v" (number->string index))))

    (define (make-vars nlocals locals)
      (let lp ((i nlocals) (locals locals) (acc '()))
        (cond
         ((< i 0)
          (list->vector acc))
         (else
          (cond
           ((null? locals)
            (lp (- i 1) locals (cons #f acc)))
           ((= i (car locals))
            (lp (- i 1) (cdr locals) (cons (make-var i) acc)))
           (else
            (lp (- i 1) locals (cons #f acc))))))))

    (let* ((locals (accumulate-locals ops))
           (max-local-num (apply max locals))
           (args (map (lambda (i) (make-var i))
                      (reverse locals)))
           (vars (make-vars max-local-num locals)))
      (call-with-escape-continuation
       (lambda (cont)
         (set! escape cont)
         `(letrec ((loop (lambda ,args
                           ,(convert vars ops))))
            loop))))))
