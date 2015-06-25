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
  #:use-module (language cps intset)
  #:use-module (language cps2)
  #:use-module (language cps2 utils)
  #:use-module (srfi srfi-11)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit variables)
  #:export (assemble-tjit))

;;;
;;; Raw Scheme, relates to C macro
;;;

(define-syntax-rule (scm-inump obj)
  (jit-bmsi obj (imm 2)))

(define-syntax-rule (scm-not-inump obj)
  (jit-bmci obj (imm 2)))


;;;
;;; Auxiliary
;;;

(define reg-thread v0)
(define reg-vp v1)
(define reg-registers v2)
(define reg-fp v3)

(define fp (jit-fp))

(define (make-offset-pointer offset n)
  (let ((addr (+ offset n)))
    (cond
     ((<= 0 addr)
      (make-pointer addr))
     (else
      (make-pointer (+ (expt 2 (* 8 %word-size)) addr))))))

(define-syntax jump
  (syntax-rules ()
    ((_ label)
     (jit-patch-at (jit-jmpi) label))
    ((_ condition label)
     (jit-patch-at condition label))))

(define-syntax local-ref
  (syntax-rules ()
    ((_ dst 0)
     (jit-ldr dst reg-fp))
    ((_ dst n)
     (jit-ldxi dst reg-fp (imm (* n %word-size))))))

(define-syntax local-set!
  (syntax-rules ()
    ((_ 0 src)
     (jit-str reg-fp src))
    ((_ n src)
     (jit-stxi (imm (* n %word-size)) reg-fp src))))


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

(define (assemble-cps cps env initial-args fp-offset)
  (define (env-ref i)
    (vector-ref env i))

  (define (reg x)
    (register-ref (ref-value x)))

  (define (moffs x)
    (make-offset-pointer fp-offset (* (ref-value x) %word-size)))

  (define start (loop-start cps))

  (define (maybe-move exp)
    (match exp
      (($ $values new-args)
       (when (= (length initial-args) (length new-args))
         (for-each
          (lambda (old new)
            (when (not (eq? old new))
              (let ((v0 (env-ref old))
                    (v1 (env-ref new)))
                (when (not (eq? v0 v1))
                  (cond
                   ((and (register? v0) (register? v1))
                    (jit-movr (reg v0) (reg v1)))
                   ((and (register? v0) (memory? v1))
                    (jit-ldxi r0 fp (moffs v1))
                    (jit-movr (reg v0) r0))
                   ((and (memory? v0) (register? v1))
                    (jit-stxi (moffs v0) fp (reg v1)))
                   ((and (memory? v0) (memory? v1))
                    (jit-ldxi r0 fp (moffs v1))
                    (jit-stxi (moffs v1) fp r0))
                   (else
                    (error "NYI: moving arguments: ~a ~a" v0 v1)))))))
          initial-args new-args)))))

  (define (assemble-cont cps arg br-label loop-label seen k)
    ;; (debug 1 "~4,,,'0@a  ~a~%" k (or (and (null? arg) arg)
    ;;                                  (unparse-cps arg)))
    (cond
     ((and (intset-ref seen k)
           (= k start))
      ;; End of the loop.
      #f)
     (else
      (let ((seen (intset-add! seen k)))
        (match (intmap-ref cps k)
          (($ $kreceive ($ $arity reqs _ _ _ _) knext)
           (assemble-cont cps arg br-label loop-label seen knext))

          (($ $kargs _ _ ($ $continue knext _ ($ $branch kt exp)))
           (cond
            ((= k start)
             (let ((loop-label (jit-label))
                   (br-label (jit-forward)))
               (assemble-cont cps exp br-label loop-label seen kt)
               (assemble-cont cps arg #f loop-label seen knext)))
            (else
             (let ((br-label (jit-forward)))
               (assemble-cont cps exp br-label loop-label seen kt)
               (assemble-cont cps arg #f loop-label seen knext)))))

          (($ $kargs names syms ($ $continue knext _ exp))
           (cond
            ((= k start)
             (let ((loop-label (jit-label)))
               (assemble-exp arg syms br-label)
               (assemble-cont cps exp br-label loop-label seen knext)))
            ((< knext k)
             ;; Jumping back to loop start.
             (assemble-exp arg syms br-label)
             ;; (assemble-cont cps exp br-label loop-label knext)
             (maybe-move exp)
             (jump loop-label)
             #f)
            (else
             (assemble-exp arg syms br-label)
             (assemble-cont cps exp br-label loop-label seen knext))))

          (($ $kfun _ _ self _ knext)
           (assemble-cont cps arg br-label loop-label seen knext))

          (($ $ktail)
           (assemble-exp arg '() br-label)
           (when br-label
             (jit-link br-label))
           ;; (when (and br-label (jit-forward-p br-label))
           ;;   (format #t "jit-forward-p: ~a~%" (jit-forward-p br-label))
           ;;   (jit-link br-label))
           #f)

          (($ $kclause ($ $arity reqs _ _ _ _) knext)
           (assemble-cont cps arg br-label loop-label seen knext)))))))

  (define (assemble-exp exp dst label)
    ;; Need at least 3 scratch registers. Currently using R0, R1, and
    ;; R2.  Might use floating point registers as whell, when double
    ;; numbers get involved.
    (match exp
      (($ $primcall 'return (arg1))
       (let ((a (env-ref arg1)))
         (cond
          ((constant? a)
           (jit-reti (constant a)))
          ((register? a)
           (jit-retr (reg a)))
          ((memory? a)
           (jit-ldxi r0 fp (moffs a))
           (jit-retr r0)))))

      ;;
      ;; guards
      ;;

      (($ $primcall '%fx< (arg1 arg2))
       (let ((a (env-ref arg1))
             (b (env-ref arg2)))
         (cond
          ((and (constant? a) (register? b))
           (jump (jit-blti (reg b) (constant a)) label))
          ((and (constant? a) (memory? b))
           (jit-ldxi r0 fp (moffs b))
           (jump (jit-blti r0 (constant a)) label))

          ((and (register? a) (constant? b))
           (jit-movi r0 (constant b))
           (jump (jit-bltr r0 (reg a)) label))
          ((and (register? a) (register? b))
           (jump (jit-bltr (reg b) (reg a)) label))
          ((and (register? a) (memory? b))
           (jit-ldxi r0 fp (moffs b))
           (jump (jit-bltr r0 (reg a)) label))

          ((and (memory? a) (constant? b))
           (jit-ldxi r0 fp (moffs a))
           (jit-movi r1 (constant b))
           (jump (jit-bltr r1 r0) label))
          ((and (memory? a) (register? b))
           (jit-ldxi r0 fp (moffs a))
           (jump (jit-bltr r0 (reg a)) label))
          ((and (memory? a) (memory? b))
           (jit-ldxi r0 fp (moffs a))
           (jit-ldxi r1 fp (moffs b))
           (jump (jit-bltr r1 r0) label)))))

      (($ $primcall '= (arg1 arg2))
       (let ((a (env-ref arg1))
             (b (env-ref arg2)))
         (jump (jit-bner (reg a) (reg b)) label)))

      (($ $primcall '%guard-fx (arg1))
       (let ((obj (env-ref arg1)))
         (cond
          ((register? obj)
           (jump (scm-inump (reg obj)) label))
          ((memory? obj)
           (jit-ldxi r0 fp (moffs obj))
           (jump (scm-inump r0) label)))))

      ;;
      ;; exact-integer
      ;;

      (($ $primcall '%fxadd (arg1 arg2))
       (let ((a (env-ref arg1))
             (b (env-ref arg2))
             (dst (env-ref (car dst))))
         (cond
          ((and (register? dst) (register? a) (register? b))
           (jit-addr (reg dst) (reg a) (reg b))
           (jit-subi (reg dst) (reg dst) (imm 2)))
          ((and (register? dst) (memory? a) (register? b))
           (jit-ldxi r0 fp (moffs a))
           (jit-addr (reg dst) r0 (reg b))
           (jit-subi (reg dst) (reg dst) (imm 2)))
          ((and (register? dst) (register? a) (memory? b))
           (jit-ldxi r0 fp (moffs b))
           (jit-addr (reg dst) (reg a) r0)
           (jit-subi (reg dst) (reg dst) (imm 2)))
          ((and (register? dst) (memory? a) (memory? b))
           (jit-ldxi r0 fp (moffs a))
           (jit-ldxi r1 fp (moffs b))
           (jit-addr (reg dst) r0 r1)
           (jit-subi (reg dst) (reg dst) (imm 2)))

          ((and (memory? dst) (register? a) (register? b))
           (jit-ldxi r0 fp (moffs dst))
           (jit-addr r0 (reg a) (reg b))
           (jit-subi r0 r0 (imm 2))
           (jit-stxi (moffs dst) fp r0))
          ((and (memory? dst) (memory? a) (register? b))
           (jit-ldxi r0 fp (moffs dst))
           (jit-ldxi r1 fp (moffs a))
           (jit-addr r0 r1 (reg b))
           (jit-subi r0 r0 (imm 2))
           (jit-stxi (moffs dst) fp r0))
          ((and (memory? dst) (register? a) (memory? b))
           (jit-ldxi r0 fp (moffs dst))
           (jit-ldxi r1 fp (moffs b))
           (jit-addr r0 (reg a) r1)
           (jit-subi r0 r0 (imm 2))
           (jit-stxi (moffs dst) fp r0))
          ((and (memory? dst) (memory? a) (memory? b))
           (jit-ldxi r0 fp (moffs dst))
           (jit-ldxi r1 fp (moffs a))
           (jit-ldxi r2 fp (moffs b))
           (jit-addr r0 r1 r2)
           (jit-subi r0 r0 (imm 2))
           (jit-stxi (moffs dst) fp r0))
          (else
           (error "assemble-exp: add: ~a ~a ~a~%" dst a b)))))

      (($ $primcall '%fxadd1 (arg1))
       (let ((dst (env-ref (car dst)))
             (src (env-ref arg1)))
         (cond
          ((and (register? dst) (register? src))
           (jit-addi (reg dst) (reg src) (imm 4)))
          ((and (register? dst) (memory? src))
           (jit-ldxi r0 fp (moffs src))
           (jit-addi (reg dst) r0 (imm 4)))

          ((and (memory? dst) (register? src))
           (jit-addi r0 (reg src) (imm 4))
           (jit-stxi (moffs dst) fp r0))
          ((and (memory? dst) (memory? src))
           (jit-ldxi r0 fp (moffs src))
           (jit-addi r0 r0 (imm 4))
           (jit-stxi (moffs dst) fp r0)))))

      (($ $primcall '%fxsub1 (arg1))
       (let ((dst (env-ref (car dst)))
             (src (env-ref arg1)))
         (cond
          ((and (register? dst) (register? src))
           (jit-subi (reg dst) (reg src) (imm 4)))
          ((and (register? dst) (memory? src))
           (jit-ldxi r0 fp (moffs src))
           (jit-subi (reg dst) r0 (imm 4)))

          ((and (memory? dst) (register? src))
           (jit-subi r0 (reg src) (imm 4))
           (jit-stxi (moffs dst) fp r0))
          ((and (memory? dst) (memory? src))
           (jit-ldxi r0 fp (moffs src))
           (jit-subi r0 r0 (imm 4))
           (jit-stxi (moffs dst) fp r0)))))

      (($ $primcall '%frame-set! (arg1 arg2))
       (let ((idx (env-ref arg1))
             (src (env-ref arg2)))
         (cond
          ((not (constant? idx))
           (debug 2 "*** %frame-set!: type mismatch ~a ~a~%" idx src))
          ((constant? src)
           (jit-movi r0 (constant src))
           (local-set! (ref-value idx) r0))
          ((register? src)
           (local-set! (ref-value idx) (reg src)))
          ((memory? src)
           (jit-ldxi r0 fp (moffs src))
           (local-set! (ref-value idx) r0))
          (else
           (debug 2 "*** %frame-set!: unknown args ~a ~a~%" idx src)))))

      (($ $primcall name args)
       (debug 2 "*** Unhandled primcall: ~a~%" name))

      ;; (($ $call 0 new-args)
      ;;  (for-each
      ;;   (lambda (old new)
      ;;     (when (not (eq? old new))
      ;;       (let ((v0 (env-ref old))
      ;;             (v1 (env-ref new)))
      ;;         (when (not (eq? v0 v1))
      ;;           (cond
      ;;            ((and (register? v0) (register? v1))
      ;;             (jit-movr (reg v0) (reg v1)))
      ;;            (else
      ;;             (error "NYI: call to self: ~a ~a~%" v0 v1)))))))
      ;;   initial-args new-args)
      ;;  (jump loop-start))

      (($ $call proc args)
       (debug 2 "      exp:call ~a ~a~%" proc args))

      (_
       ;; (debug 1 "      exp:~a~%" exp)
       #f)))

  (assemble-cont cps '() #f #f empty-intset 0))

(define (assemble-tjit locals cps)
  (define (max-moffs env)
    (let lp ((i 0) (end (vector-length env)) (current 0))
      (if (< i end)
          (let ((var (vector-ref env i)))
            (if (and (memory? var) (< current (ref-value var)))
                (lp (+ i 1) end (ref-value var))
                (lp (+ i 1) end current)))
          current)))

  (let*-values (((max-label max-var) (compute-max-label-and-var cps))
                ((env initial-locals) (resolve-vars cps locals max-var)))

    (let ((verbosity (lightning-verbosity)))
      (when (and verbosity (<= 2 verbosity))
        ;; (dump-cps2 "dump" cps)
        (display ";;; cps env\n")
        (let lp ((n 0) (end (vector-length env)))
          (when (< n end)
            (format #t ";;; ~3@a: ~a~%" n (vector-ref env n))
            (lp (+ n 1) end)))))

    ;; Allocate space for spilled variables, if any.
    (let* ((nspills (max-moffs env))
           (fp-offset (or (and (<= nspills 0) 0)
                          (jit-allocai (imm (* nspills %word-size))))))

      ;; (debug 2 ";;; nspills: ~a, fp-offset: ~a~%" nspills fp-offset)
      ;; (debug 2 ";;; initial-locals: ~a~%" initial-locals)

      ;; Get arguments.
      (jit-getarg reg-thread (jit-arg))    ; *thread
      (jit-getarg reg-vp (jit-arg))        ; *vp
      (jit-getarg reg-registers (jit-arg)) ; registers, for prompt
      (jit-getarg r0 (jit-arg))            ; resume

      ;; Load vp->fp to register.
      (jit-ldxi reg-fp reg-vp (imm #x10))

      ;; Load initial locals.
      (for-each
       (match-lambda
        ((local-idx . var-idx)
         (let ((var (vector-ref env var-idx)))
           (cond
            ((register? var)
             (local-ref (register-ref (ref-value var)) local-idx))
            ((memory? var)
             (let ((offset (* (ref-value var) %word-size)))
               (local-ref r0 local-idx)
               (jit-stxi (make-offset-pointer fp-offset offset) fp r0)))
            (else
             (debug 2 "Unknown initial argument: ~a~%" var))))))
       initial-locals)

      ;; Assemble the loop.
      (let ((initial-args (map cdr initial-locals)))
        (assemble-cps cps env initial-args fp-offset)))))
