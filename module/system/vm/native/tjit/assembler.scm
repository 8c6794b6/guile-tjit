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
  #:use-module (ice-9 pretty-print)
  #:use-module (language cps intmap)
  #:use-module (language cps2)
  #:use-module (language cps2 utils)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit registers)
  #:export (assemble-cps))


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

(define-syntax local-set!/immediate
  (syntax-rules ()
    ((_ 0 val)
     (begin
       (jit-movi r0 val)
       (jit-str reg-fp r0)))
    ((_ n val)
     (begin
       (jit-movi r0 val)
       (jit-stxi (imm (* n word-size)) reg-fp r0)))))


;;;
;;; Code generation
;;;

(define (dump-cps2 title cps)
  (format #t ";;; ~a~%~{~4,,,'0@a  ~a~%~}"
          title
          (or (and cps (reverse! (intmap-fold
                                  (lambda (i k acc)
                                    (cons (unparse-cps k)
                                          (cons i acc)))
                                  cps
                                  '())))
              '())))

(define (assemble-cps locals cps)
  (define (assemble cps env inits registers loop-start)

    (define (reg x)
      (register-ref (ref-value x)))

    (define (assemble-cont cps arg label kcurrent)
      ;; (debug 1 "~4,,,'0@a  ~a~%" kcurrent
      ;;        (or (and (null? arg) arg)
      ;;            (unparse-cps arg)))
      (match (intmap-ref cps kcurrent)
        (($ $kreceive ($ $arity reqs _ _ _ _) knext)
         (assemble-cont cps  arg label knext))

        (($ $kargs _ _ ($ $continue knext _ ($ $branch kt exp)))
         (let ((label (jit-forward)))
           (assemble-cont cps  exp label kt)
           (assemble-cont cps arg #f knext)))

        (($ $kargs names syms ($ $continue knext _ exp))
         (assemble-exp arg syms label)
         (assemble-cont cps exp label knext))

        (($ $kfun _ _ self _ knext)
         ;; (vector-set! env self 'self)
         (assemble-cont cps arg label knext))

        (($ $ktail)
         (assemble-exp arg '() label)
         (when label
           (jit-link label))
         #f)

        (($ $kclause ($ $arity reqs _ _ _ _) knext)
         (assemble-cont cps arg label knext))))

    (define (assemble-exp exp dst label)
      (define (env-ref i)
        (vector-ref env i))
      (match exp
        (($ $primcall 'return (arg1))
         (let ((a (env-ref arg1)))
           (cond
            ((register? a) (jit-retr (ref-value a)))
            ((constant? a) (jit-reti (imm (ref-value a)))))))

        (($ $primcall '< (arg1 arg2))
         (let ((a (env-ref arg1))
               (b (env-ref arg2)))
           (cond
            ((and (constant? a) (register? b))
             (jump (jit-blti (reg b) (constant a)) label))
            ((and (register? a) (constant? b))
             (jump (jit-bgei (reg a) (constant b)) label))
            ((and (register? a) (register? b))
             (jump (jit-bltr (reg b) (reg a)) label)))))

        (($ $primcall '= (arg1 arg2))
         (let ((a (env-ref arg1))
               (b (env-ref arg2)))
           (jump (jit-bner (reg a) (reg b)) label)))

        (($ $primcall 'add1 (arg1))
         (let ((v0 (reg (env-ref (car dst))))
               (v1 (reg (env-ref arg1))))
           (jit-addi v0 v1 (imm 4))))

        (($ $primcall 'add (arg1 arg2))
         (let ((v0 (reg (env-ref (car dst))))
               (v1 (reg (env-ref arg1)))
               (v2 (reg (env-ref arg2))))
           (jit-addr v0 v1 v2)
           (jit-subi v0 v0 (imm 2))))

        (($ $primcall 'sub1 (arg1))
         (let ((v0 (env-ref (car dst)))
               (v1 (env-ref arg1)))
           (jit-subi (reg v0) (reg v1) (imm 4))))

        (($ $primcall '%frame-set! (arg1 arg2))
         (let ((idx (env-ref arg1))
               (src (env-ref arg2)))
           (cond
            ((and (constant? idx) (register? src))
             (local-set! (ref-value idx) (reg src)))
            ((and (constant? idx) (constant? src))
             (local-set!/immediate (ref-value idx) (constant src)))
            (else
             (debug 2 "*** %frame-set!: type mismatch: idx=~a, src=~a~%"
                    idx src)))))

        (($ $primcall name args)
         (debug 2 "*** Unhandled primcall: ~a~%" name))

        (($ $call 0 args)
         (for-each (lambda (old new)
                     (when (not (eq? old new))
                       (let ((v0 (env-ref old))
                             (v1 (env-ref new)))
                         (when (not (eq? v0 v1))
                           (jit-movr (reg v0) (reg v1))))))
                   inits args)
         (jump loop-start))

        (($ $call proc args)
         (debug 2 "      exp:call ~a ~a~%" proc args))

        (_
         ;; (debug 1 "      exp:~a~%" exp)
         #f)))

    (assemble-cont cps '() #f 0))

  (define (load-locals registers)
    (let lp ((local-idx 0) (end (vector-length registers)))
      (when (< local-idx end)
        (cond ((vector-ref registers local-idx)
               =>
               (lambda (reg-idx)
                 (let ((r (register-ref reg-idx)))
                   (local-ref r local-idx)))))
        (lp (+ local-idx 1) end))))

  ;; Resolve variables in cps.
  (let*-values (((max-label max-var) (compute-max-label-and-var cps))
                ((env inits registers) (resolve-vars cps locals max-var)))

    ;; Get arguments.
    (jit-getarg reg-thread (jit-arg))    ; *thread
    (jit-getarg reg-vp (jit-arg))        ; *vp
    (jit-getarg reg-registers (jit-arg)) ; registers, for prompt
    (jit-getarg r0 (jit-arg))            ; resume

    ;; Load vp->fp to register, then load locals to temporary registers.
    (jit-ldxi reg-fp reg-vp (imm #x10))

    ;; Initialize registers with locals.
    (load-locals registers)

    ;; Assemble the loop.
    (let ((loop-start (jit-label)))
      (let ((verbosity (lightning-verbosity)))
        (when (and verbosity (<= 2 verbosity))
          (dump-cps2 "dump" cps)
          (display ";;; cps env\n")
          (pretty-print env))
        (assemble cps env inits registers loop-start)))))
