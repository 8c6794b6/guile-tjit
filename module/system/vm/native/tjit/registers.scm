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

;;; Register allocation and CPS variable resolution.

;;; Code:

(define-module (system vm native tjit registers)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps intmap)
  #:use-module (language cps2)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:export (resolve-vars
            ref?
            ref-value
            ref-type
            constant?
            register?
            constant
            register-ref))

;;;
;;; Variable
;;;

(define (ref? x)
  (and (pair? x)
       (symbol? (car x))))

(define (ref-value x)
  (and (ref? x) (cdr x)))

(define (ref-type x)
  (and (ref? x) (car x)))

(define (constant? x)
  (eq? 'const (ref-type x)))

(define (register? x)
  (eq? 'reg (ref-type x)))

(define (constant x)
  (imm (ref-value x)))


;;;
;;; The registers
;;;

(define *tmp-registers*
  ;; Architecture dependent temporary registers.  Lightning has it's own
  ;; register management policy, not sure how the policy works under
  ;; other architecture than x86-64 linux.
  `#(,r0 ,r1 ,r2 ,r3 ,f5 ,f6))

(define (register-ref i)
  (vector-ref *tmp-registers* i))

;; XXX: Invoke functions in lightning to get this number.
(define *num-registers* 6)

(define (allocate-registers nlocals is)
  ;; Naive procedure to assign registers to locals. Does nothing
  ;; intellectual such as graph-coloring, linear-scan or bin-pack.
  (let lp ((is (reverse is))
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

(define (resolve-vars cps locals max-var)
  (define nlocals (or (and (null? locals) 0)
                      (apply max locals)))
  (define registers (allocate-registers (+ nlocals 1) locals))
  (define (resolve-cont cps env reqs inits args k)
    (match (intmap-ref cps k)
      (($ $kreceive _ knext)
       (resolve-cont cps env reqs inits '() knext))

      (($ $kargs names syms ($ $continue knext _ exp))
       (cond
        ((equal? names reqs)
         (for-each
          (lambda (sym name)
            (let ((current (vector-ref env sym)))
              (when (not (constant? current))
                (let ((idx (string->number
                            (substring (symbol->string name) 1))))
                  (vector-set! env sym
                               (cons 'reg (vector-ref registers idx)))))))
          syms names)
         (resolve-exp exp cps env reqs syms knext))
        ((and (not (null? args)) (not (null? names)))
         (for-each (lambda (sym arg)
                     (vector-set! env sym arg))
                   syms args)
         (resolve-exp exp cps env reqs inits knext))
        (else
         (resolve-exp exp cps env reqs inits knext))))

      (($ $kfun _ _ self _ knext)
       (vector-set! env self '(proc . self))
       (resolve-cont cps env reqs inits '() knext))

      (($ $ktail)
       (values env inits registers))

      (($ $kclause ($ $arity reqs _ _ _ _) knext _)
       (resolve-cont cps env reqs inits '() knext))))

  (define (resolve-exp exp cps env reqs inits k)
    (match exp
      (($ $const val)
       (resolve-cont cps env reqs inits (list (cons 'const val)) k))

      (($ $branch kt exp)
       (resolve-exp exp cps env reqs inits kt)
       (resolve-cont cps env reqs inits '() k))

      (($ $call 0 args)
       ;; Calling self, jumping back to beginning of this procedure.
       (let lp ((inits inits) (args args))
         (unless (null? inits)
           ;; Variable registers could be shared between arguments in
           ;; this call and initial call.
           (let ((init (car inits))
                 (arg (car args)))
             (vector-set! env arg (vector-ref env init)))
           (lp (cdr inits) (cdr args))))
       (resolve-cont cps env reqs inits '() k))

      (($ $primcall name args)
       (resolve-cont cps env reqs inits (list exp) k))

      (_
       (resolve-cont cps env reqs inits '() k))))

  (resolve-cont cps (make-vector (+ max-var 1)) '() '() '() 0))
