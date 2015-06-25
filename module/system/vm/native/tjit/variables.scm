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

;;; CPS variable resolution and register alloocatoion.  Applying naive strategy
;;; to assign registers to locals, does nothing intellectual such as
;;; linear-scan, binpacking, or graph coloring.

;;; Code:

(define-module (system vm native tjit variables)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps2)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:export (resolve-vars
            loop-start
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
  ;; register management policy, not sure how it works under other architecture
  ;; than x86-64 linux.
  `#(,r3 ,f5 ,f6))

(define *num-registers*
  (+ (vector-length *tmp-registers*) 1))

(define (register-ref i)
  (vector-ref *tmp-registers* i))

(define (resolve-vars cps locals max-var)
  "Resolve variables in CPS using local variables from LOCALS and MAX-VAR.

Returns 2 values: vector containing resolved variables, and an accos list for
locals and initial arguments."

  (define (local-var-alist locals vars)
    (map cons (reverse locals) vars))

  (define start (loop-start cps))

  (define (resolve-cont env reqs init-syms loop-syms args mem-idx seen k)
    (cond
     ((and (intset-ref seen k)
           (= k start))
      ;; Jumping back to beginning of loop.  Variables could be shared between
      ;; arguments in this call and initial call.
      (for-each (lambda (arg loop)
                  (vector-set! env arg (vector-ref env loop)))
                args loop-syms)
      (values env (local-var-alist locals init-syms)))
     (else
      (let ((seen (intset-add! seen k)))

        (match (intmap-ref cps k)
          (($ $kreceive _ knext)
           (resolve-cont env reqs init-syms loop-syms '() mem-idx seen knext))

          (($ $kargs names syms ($ $continue knext _ exp))
           (cond
            ((= k start)
             (for-each (lambda (arg sym)
                         (vector-set! env sym (vector-ref env arg)))
                       args syms)
             (resolve-exp exp env reqs init-syms syms mem-idx seen knext))

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
                  (resolve-exp exp env reqs syms loop-syms mem-idx seen knext)))))

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
                  (resolve-exp exp env reqs init-syms loop-syms mem-idx seen
                               knext)))))
            (else
             (resolve-exp exp env reqs init-syms loop-syms mem-idx seen
                          knext))))

          (($ $kfun _ _ self _ knext)
           (vector-set! env self '(proc . self))
           (resolve-cont env reqs init-syms loop-syms '() mem-idx seen
                         knext))

          (($ $ktail)
           (values env (local-var-alist locals init-syms)))

          (($ $kclause ($ $arity reqs _ _ _ _) knext _)
           (resolve-cont env reqs init-syms loop-syms '() mem-idx seen
                         knext)))))))

  (define (resolve-exp exp env reqs init-syms loop-syms mem-idx seen k)
    (match exp
      (($ $const val)
       (resolve-cont env reqs init-syms loop-syms
                     (list (make-constant val))
                     mem-idx seen k))

      (($ $branch kt exp)
       (resolve-exp exp env reqs init-syms loop-syms mem-idx seen kt)
       (resolve-cont env reqs init-syms loop-syms '() mem-idx seen k))

      (($ $primcall _ _)
       (resolve-cont env reqs init-syms loop-syms (list exp) mem-idx seen k))

      (($ $values args)
       (resolve-cont env reqs init-syms loop-syms args mem-idx seen k))

      ;; (($ $call 0 args)
      ;;  ;; Jumping back to beginning of this procedure.  Variables could be
      ;;  ;; shared between arguments in this call and initial call.
      ;;  (for-each (lambda (init arg)
      ;;              (vector-set! env arg (vector-ref env init)))
      ;;            init-syms args)
      ;;  (resolve-cont env reqs init-syms loop-syms '() mem-idx seen k))

      (_
       (resolve-cont env reqs init-syms loop-syms '() mem-idx seen k))))

  ;; (format #t ";;; ~a~%~{~4,,,'0@a  ~a~%~}"
  ;;         "resolve-vars"
  ;;         (or (and cps (reverse! (intmap-fold
  ;;                                 (lambda (i k acc)
  ;;                                   (cons (unparse-cps k)
  ;;                                         (cons i acc)))
  ;;                                 cps
  ;;                                 '())))
  ;;             '()))
  (resolve-cont (make-vector (+ max-var 1)) '() '() '() '() 0 empty-intset 0))


;;;
;;; Loop start
;;;

(define* (loop-start cps #:optional (kfun 0))
  (define (go k)
    (match (intmap-ref cps k)
      (($ $kfun src meta self tail next)
       (go next))
      (($ $kclause arity next)
       (go next))
      (($ $kargs names syms ($ $continue next src ($ $branch kt exp)))
       (or (and (< next k) next)
           (go kt)
           (go next)))
      (($ $kargs names syms ($ $continue next src exp))
       (or (and (< next k) next)
           (go next)))
      (($ $ktail)
       #f)
      (($ $kreceive _ _ next)
       (go next))))
  ;; (format #t ";;; ~a~%~{~4,,,'0@a  ~a~%~}"
  ;;         "resolve-vars"
  ;;         (or (and cps (reverse! (intmap-fold
  ;;                                 (lambda (i k acc)
  ;;                                   (cons (unparse-cps k)
  ;;                                         (cons i acc)))
  ;;                                 cps
  ;;                                 '())))
  ;;             '()))
  (go kfun))
