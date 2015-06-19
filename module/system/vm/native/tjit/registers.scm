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

;;; Register allocation and CPS variable resolution.  Applying naive strategy to
;;; assign registers to locals. Does nothing intellectual such as
;;; graph-coloring, linear-scan or binpack.

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
            memory?
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

(define (make-constant x)
  (cons 'const x))

(define (constant? x)
  (eq? 'const (ref-type x)))

(define (constant x)
  (imm (ref-value x)))

(define (make-register x)
  (cons 'reg x))

(define (register? x)
  (eq? 'reg (ref-type x)))

(define (make-memory x)
  (cons 'mem x))

(define (memory? x)
  (eq? 'mem (ref-type x)))


;;;
;;; The registers
;;;

(define *tmp-registers*
  ;; Architecture dependent temporary registers.  Lightning has it's own
  ;; register management policy, not sure how the policy works under
  ;; other architecture than x86-64 linux.
  `#(,r1 ,r2 ,r3 ,f5 ,f6))

(define *num-registers*
  (+ (vector-length *tmp-registers*) 1))

(define (register-ref i)
  (vector-ref *tmp-registers* i))

(define (resolve-vars cps locals max-var)
  "Resolve variables in CPS using local variables from LOCALS and MAX-VAR.

Returns 2 values: vector containing resolved variables, and an accos list for
locals and initial arguments."
  (define (local-var-alist locals vars)
    (let lp ((locals (reverse locals)) (vars vars) (acc '()))
      (cond
       ((and (null? locals) (null? vars))
        (reverse acc))
       ((or (null? locals) (null? vars))
        (error "local and initial var length mismatch" locals vars))
       (else
        (lp (cdr locals) (cdr vars) (acons (car locals) (car vars) acc))))))
  (define (resolve-cont cps env reqs init-syms args mem-idx k)
    (match (intmap-ref cps k)
      (($ $kreceive _ knext)
       (resolve-cont cps env reqs init-syms '() mem-idx knext))

      (($ $kargs names syms ($ $continue knext _ exp))
       (cond
        ((equal? names reqs)
         (let lp ((syms-tmp syms) (mem-idx mem-idx))
           (match syms-tmp
             ((sym . rest)
              (cond
               ((< sym *num-registers*)
                (vector-set! env sym (make-register (- sym 1)))
                (lp rest mem-idx))
               (else
                (vector-set! env sym (make-memory mem-idx))
                (lp rest (+ mem-idx 1)))))
             (()
              (resolve-exp exp cps env reqs syms mem-idx knext)))))

        ((and (not (null? args)) (not (null? syms)))
         (let lp ((syms syms) (args args) (mem-idx mem-idx))
           (match (cons syms args)
             (((sym . syms) . (arg . args))
              (cond
               ((constant? arg)
                (vector-set! env sym arg)
                (lp syms args mem-idx))
               ((< sym *num-registers*)
                (vector-set! env sym (make-register (- sym 1)))
                (lp syms args mem-idx))
               (else
                (vector-set! env sym (make-memory mem-idx))
                (lp syms args (+ mem-idx 1)))))
             (_
              (resolve-exp exp cps env reqs init-syms mem-idx knext)))))
        (else
         (resolve-exp exp cps env reqs init-syms mem-idx knext))))

      (($ $kfun _ _ self _ knext)
       (vector-set! env self '(proc . self))
       (resolve-cont cps env reqs init-syms '() mem-idx knext))

      (($ $ktail)
       (values env (local-var-alist locals init-syms)))

      (($ $kclause ($ $arity reqs _ _ _ _) knext _)
       (resolve-cont cps env reqs init-syms '() mem-idx knext))))

  (define (resolve-exp exp cps env reqs init-syms mem-idx k)
    (match exp
      (($ $const val)
       (resolve-cont cps env reqs init-syms
                     (list (make-constant val))
                     mem-idx k))

      (($ $branch kt exp)
       (resolve-exp exp cps env reqs init-syms mem-idx kt)
       (resolve-cont cps env reqs init-syms '() mem-idx k))

      (($ $call 0 args)
       ;; Jumping back to beginning of this procedure.  Variables could be
       ;; shared between arguments in this call and initial call.
       (for-each (lambda (init arg)
                   (vector-set! env arg (vector-ref env init)))
                 init-syms args)
       (resolve-cont cps env reqs init-syms '() mem-idx k))

      (($ $primcall _ _)
       (resolve-cont cps env reqs init-syms (list exp) mem-idx k))

      (_
       (resolve-cont cps env reqs init-syms '() mem-idx k))))

  (resolve-cont cps (make-vector (+ max-var 1)) '() '() '() 0 0))
