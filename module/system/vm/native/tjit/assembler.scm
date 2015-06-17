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

;;; Assemble CPS to native code.

;;; Code:

(define-module (system vm native tjit assembler)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps intmap)
  #:use-module (language cps2)
  #:use-module (language cps2 utils)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:export (assemble-cps vm-prolog))


;;;
;;; Type check
;;;

(define (fixnums? a b)
  (define (fixnum? n)
    (and (exact? n)
         (< n most-positive-fixnum)
         (> n most-negative-fixnum)))
  (and (fixnum? a) (fixnum? b)))

(define (inexacts? a b)
  (and (inexact? a) (inexact? b)))


;;;
;;; Auxiliary
;;;

(define-syntax jump
  (syntax-rules ()
    ((_ label)
     (jit-patch-at (jit-jmpi) label))
    ((_ condition label)
     (jit-patch-at condition label))))

(define reg-thread v0)
(define reg-vp v1)
(define reg-registers v2)
(define reg-fp v3)
(define word-size (sizeof '*))

(define-syntax local-ref
  (syntax-rules ()
    ((_ dst 0)
     (jit-ldr dst reg-fp))
    ((_ dst n)
     (jit-ldxi dst reg-fp (imm (* n word-size))))))

(define-syntax local-set!
  (syntax-rules ()
    ((_ 0 src)
     (jit-str reg-fp src))
    ((_ n src)
     (jit-stxi (imm (* n word-size)) reg-fp src))))

;;;
;;; Variable
;;;

(define (ref-value x)
  (cdr x))

(define (ref-type x)
  (car x))

(define (const? x)
  (eq? 'const (ref-type x)))

(define (reg? x)
  (eq? 'reg (ref-type x)))

(define (const x)
  (imm (ref-value x)))

(define (register var)
  ;; XXX: Lightning has it's own register management policy, not sure
  ;; how the policy works under other architecture than x86-64 linux.
  (match var
    (('reg . sym)
     (match sym
       ('v0 r0)
       ('v1 r1)
       ('v2 r2)
       ('v3 r3)
       ('v4 f5)
       ('v5 f6)
       ;; ('v6 f7)
       (_ (error "register: unknown register ~s" sym))))
    (_ (error "register: not a register ~s" var))))

(define (resolve-vars cps max-var)
  (define (resolve-cont cps env reqs inits args k)
    (match (intmap-ref cps k)
      (($ $kreceive _ knext)
       (resolve-cont cps env reqs inits '() knext))
      (($ $kargs names syms ($ $continue knext _ exp))
       (cond
        ((equal? names reqs)
         (for-each (lambda (sym name)
                     (vector-set! env sym (cons 'reg name)))
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
       (values inits env))
      (($ $kclause ($ $arity reqs _ _ _ _) knext _)
       (resolve-cont cps env reqs inits '() knext))))

  (define (resolve-exp exp cps env reqs inits k)
    (match exp
      (($ $const val)
       (resolve-cont cps env reqs inits (list (cons 'const val)) k))
      (($ $branch kt exp)
       (resolve-exp exp cps env reqs inits kt)
       (resolve-cont cps env reqs inits '() k))
      (($ $call proc args)
       (when (= proc 0)
         ;; Calling self, jumping back to beginning of this procedure.
         (let lp ((inits inits) (args args))
           (unless (null? inits)
             ;; Variable registers could be shared between arguments in
             ;; this call and initial call.
             (let ((init (car inits))
                   (arg (car args)))
               (vector-set! env arg (vector-ref env init)))
             (lp (cdr inits) (cdr args)))))
       (resolve-cont cps env reqs inits '() k))
      (($ $primcall name args)
       (resolve-cont cps env reqs inits (list exp) k))
      (_
       (resolve-cont cps env reqs inits '() k))))

  (resolve-cont cps (make-vector (+ max-var 1)) '() '() '() 0))


;;;
;;; Code generation
;;;

(define (vm-prolog cps)
  ;; Get arguments.
  (jit-getarg reg-thread (jit-arg))     ; *thread
  (jit-getarg reg-vp (jit-arg))         ; *vp
  (jit-getarg reg-registers (jit-arg))  ; registers, for prompt
  (jit-getarg r0 (jit-arg))             ; resume

  ;; Load vp->fp to register.
  (jit-ldxi reg-fp reg-vp (imm #x10))

  ;; XXX: Analyse CPS IR, load locals to registers with arguments
  ;; appeared.
  (local-ref r0 0)
  (local-ref r1 1)
  (local-ref r2 2)
  (local-ref r3 3)
  (local-ref f5 4)
  (local-ref f6 5))

(define (assemble-cps initial-node cps)
  (define (assemble-cont cps env inits arg label kcurrent)
    ;; (debug 1 "~4,,,'0@a  ~a~%" kcurrent
    ;;        (or (and (null? arg) arg)
    ;;            (unparse-cps arg)))
    (match (intmap-ref cps kcurrent)
      (($ $kreceive ($ $arity reqs _ _ _ _) knext)
       ;; (debug 1 "kreceive ~a ~a~%" reqs arg)
       (assemble-cont cps env inits arg label knext))

      (($ $kargs _ _ ($ $continue knext _ ($ $branch kt exp)))
       ;; (debug 1 "kargs ~a ~a ~a~%" names vars arg)
       (let ((label (jit-forward)))
         ;; (debug 1 "--- forward: ~a~%" label)
         (assemble-cont cps env inits exp label kt)
         (assemble-cont cps env inits arg #f knext)))

      (($ $kargs names syms ($ $continue knext _ exp))
       ;; (debug 1 "kargs ~a ~a ~a~%" names vars arg)
       (assemble-exp arg syms env inits label)
       (assemble-cont cps env inits exp label knext))

      (($ $kfun _ _ self _ knext)
       (vector-set! env self 'self)
       ;; (debug 1 "kfun ~a ~a~%" self arg)
       (assemble-cont cps env inits arg label knext))

      (($ $ktail)
       ;; (debug 1 "ktail~%")
       (assemble-exp arg '() env inits label)
       (when label
         ;; (debug 1 "(link ~a)~%" (pointer-address label))
         (jit-link label))
       #f)

      (($ $kclause ($ $arity reqs _ _ _ _) knext)
       ;; (debug 1 "~a~%" (cons 'start reqs))
       (assemble-cont cps env inits arg label knext))))

  (define (assemble-exp exp dst env inits label)
    (define (ref i)
      (vector-ref env i))
    ;; (debug 1 "dst=~a exp=~a~%" dst exp)
    (match exp
      (($ $primcall 'return (arg1))
       ;; XXX: Recover the frame with analyzed locals.
       (local-set! 0 r0)
       (local-set! 1 r1)
       (local-set! 2 r2)
       (local-set! 3 r3)
       (local-set! 4 f5)
       (local-set! 5 f6)
       (let ((a (ref arg1)))
         (cond
          ((reg? a)
           (jit-retr (ref-value a)))
          ((const? a)
           (jit-reti (imm (ref-value a)))))))

      (($ $primcall '< (arg1 arg2))
       (let ((a (ref arg1))
             (b (ref arg2)))
         (cond
          ((and (const? a) (reg? b))
           (jump (jit-blti (register b) (const a)) label))
          ((and (reg? a) (const? b))
           (jump (jit-bgei (register a) (const b)) label))
          ((and (reg? a) (reg? b))
           (jump (jit-bltr (register b) (register a)) label)))))

      (($ $primcall '= (arg1 arg2))
       (jump (jit-bner (register (ref arg1)) (register (ref arg2))) label))

      (($ $primcall 'add1 (arg1))
       (let ((v0 (register (ref (car dst))))
             (v1 (register (ref arg1))))
         (jit-addi v0 v1 (imm 4))))

      (($ $primcall 'add (arg1 arg2))
       (let ((v0 (register (ref (car dst))))
             (v1 (register (ref arg1)))
             (v2 (register (ref arg2))))
         (jit-addr v0 v1 v2)
         (jit-subi v0 v0 (imm 2))))

      (($ $primcall 'sub1 (arg1))
       (let ((v0 (register (ref (car dst))))
             (v1 (register (ref arg1))))
         (jit-subi v0 v1 (imm 4))))

      (($ $primcall name args)
       (debug 2 "*** Unhandled primcall: ~a ***~%" name))

      (($ $call 0 args)
       (for-each (lambda (old new)
                   (when (not (eq? old new))
                     (let ((v0 (ref old))
                           (v1 (ref new)))
                       (when (not (eq? v0 v1))
                         (jit-movr (register v0) (register v1))))))
                 inits args)
       (jump initial-node))

      (($ $call proc args)
       (debug 2 "      exp:call ~a ~a~%" proc args))

      (_
       ;; (debug 1 "      exp:~a~%" exp)
       #f)))

  (call-with-values
      (lambda () (compute-max-label-and-var cps))
    (lambda (max-label max-var)
      (call-with-values
          (lambda () (resolve-vars cps max-var))
        (lambda (inits env)
          (debug 2 "env: ~a~%" env)
          (assemble-cont cps env inits '() #f 0))))))
