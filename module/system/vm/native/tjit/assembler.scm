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
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps2)
  #:use-module (language cps2 types)
  #:use-module (language cps2 utils)
  #:use-module (language tree-il primitives)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit tlog)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit variables)
  #:export (assemble-tjit
            initialize-tjit-primitives
            *native-prim-types*))


;;;
;;; Raw Scheme, relates to C macro
;;;

(define-syntax scm-cell-object
  (syntax-rules ()
    ((_ dst obj 0)
     (jit-ldr dst obj))
    ((_ dst obj 1)
     (jit-ldxi dst obj (imm %word-size)))
    ((_ dst obj n)
     (jit-ldxi dst obj (imm (* n %word-size))))))

(define-syntax-rule (scm-cell-type dst src)
  (scm-cell-object dst src 0))

(define-syntax-rule (scm-typ16 dst obj)
  (jit-andi dst obj (imm #xffff)))

(define-syntax-rule (scm-real-value dst src)
  (jit-ldxi-d dst src (imm (* 2 %word-size))))

(define %scm-from-double
  (dynamic-pointer "scm_do_inline_from_double" (dynamic-link)))

(define-syntax-rule (scm-from-double dst src)
  (begin
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-pushargr-d src)
    (jit-calli %scm-from-double)
    (jit-retval dst)))

(define %scm-do-inline-double-cell
  (dynamic-pointer "scm_do_inline_double_cell" (dynamic-link)))


;;; Predicates

(define-syntax-rule (scm-imp obj)
  (jit-bmsi obj (imm 6)))

(define-syntax-rule (scm-inump obj)
  (jit-bmsi obj (imm 2)))

(define-syntax-rule (scm-not-inump obj)
  (jit-bmci obj (imm 2)))

(define-syntax-rule (scm-realp tag)
  (jit-beqi tag (imm (@@ (system base types) %tc16-real))))


;;;
;;; Assembler state
;;;

(define-record-type <asm>
  (%make-asm %env %fp-offset %out-code %end-address)
  asm?
  (%env asm-env set-asm-env!)
  (%fp-offset asm-fp-offset set-asm-fp-offset!)
  (%out-code asm-out-code set-out-code!)
  (%end-address asm-end-address set-asm-end-address!))

(define (make-asm env fp-offset out-code end-address)
  (%make-asm env fp-offset out-code end-address))

(define (env-ref asm i)
  (vector-ref (asm-env asm) i))

(define (moffs asm r)
  (make-offset-pointer (asm-fp-offset asm) (* (ref-value r) %word-size)))


;;;
;;; Auxiliary
;;;

(define reg-thread v0)

(define fp (jit-fp))

(define *max-address* (expt 2 (* 8 %word-size)))

(define (make-negative-pointer addr)
  "Make negative pointer with ADDR."
  (when (< 0 addr)
    (error "make-negative-pointer: expecting negative address" addr))
  (make-pointer (+ (expt 2 (* 8 %word-size)) addr)))

(define (make-offset-pointer offset n)
  (let ((addr (+ offset n)))
    (cond
     ((<= 0 addr)
      (make-pointer addr))
     (else
      (make-negative-pointer addr)))))

(define vp->fp-offset
  (make-negative-pointer (- %word-size)))

(define registers-offset
  (make-negative-pointer (- (* 2 %word-size))))

(define ra-offset
  (make-negative-pointer (- (* 3 %word-size))))

(define-syntax jump
  (syntax-rules ()
    ((_ label)
     (jit-patch-at (jit-jmpi) label))
    ((_ condition label)
     (jit-patch-at condition label))))

(define-syntax jumpi
  (syntax-rules ()
    ((_ address)
     (jit-patch-abs (jit-jmpi) address))))

(define-syntax local-ref
  (syntax-rules ()
    ((_ dst 0)
     (let ((vp->fp (if (eq? dst r0) r1 r0)))
       (jit-ldxi vp->fp fp vp->fp-offset)
       (jit-ldr dst vp->fp)))
    ((_ dst n)
     (let ((vp->fp (if (eq? dst r0) r1 r0)))
       (jit-ldxi vp->fp fp vp->fp-offset)
       (jit-ldxi dst vp->fp (imm (* n %word-size)))))))

(define-syntax local-set!
  (syntax-rules ()
    ((_ 0 src)
     (let ((vp->fp (if (eq? src r0) r1 r0)))
      (jit-ldxi vp->fp fp vp->fp-offset)
      (jit-str vp->fp src)))
    ((_ n src)
     (let ((vp->fp (if (eq? src r0) r1 r0)))
       (jit-ldxi vp->fp fp vp->fp-offset)
       (jit-stxi (imm (* n %word-size)) vp->fp src)))))

(define-syntax-rule (memory-ref asm dst src)
  (cond
   ((not (memory? src))
    (error "memory-ref: not a memory" src))
   (else
    (jit-ldxi dst fp (moffs asm src)))))

(define-syntax-rule (memory-ref/fpr asm dst src)
  (cond
   ((not (memory? src))
    (error "memory-ref/fpr: not a memory" src))
   (else
    (jit-ldxi-d dst fp (moffs asm src)))))

(define-syntax-rule (memory-set! asm dst src)
  (cond
   ((not (memory? dst))
    (error "memory-set!: not a memory" dst))
   (else
    (jit-stxi (moffs asm dst) fp src))))

(define-syntax-rule (memory-set!/fpr asm dst src)
  (cond
   ((not (memory? dst))
    (error "memory-set!/fpr: not a memory" dst))
   (else
    (jit-stxi-d (moffs asm dst) fp src))))

(define-syntax-rule (constant-word i)
  (imm (* (ref-value i) %word-size)))

(define-syntax-rule (goto-exit asm)
  ;; Save return address, then jump to address of asm-code of ASM.
  ;;
  ;; Expecting that CODE will jump back to return address, or patched to parent
  ;; trace already. When returned to patched address, exit from native code with
  ;; returning the contents of register R0.
  ;;
  ;; Side trace reuses RSP shifting from parent trace. Native code of side trace
  ;; does not reset stack pointer, because side traces use `jit-tramp'.
  ;;
  (let ((addr (jit-movi r1 (imm 0))))
    (jit-stxi ra-offset fp r1)
    (jumpi (asm-out-code asm))
    (jit-patch addr)
    (cond
     ((asm-end-address asm)
      =>
      (lambda (address)
        (debug 2 ";;; side-exit: jumping to ~a~%" address)
        (jumpi address)))
     (else
      (debug 2 ";;; side-exit: returning R0~%")
      (jit-retr r0)))))


;;;
;;; Primitives
;;;

;;; Primitives used for vm-tjit engine.  Primitives defined here are used during
;;; compilation of traced data to native code, and possibly useless for ordinal
;;; use as scheme procedure except for managing comment as document.
;;;
;;; Need at least 3 general purpose scratch registers, and 3 floating point
;;; scratch registers. Currently using R0, R1, and R2 for general purpose, F0,
;;; F1, and F2 for floating point.

(define *native-prim-arities* (make-hash-table))
(define *native-prim-procedures* (make-hash-table))
(define *native-prim-types* (make-hash-table))

(define (arity-of-args args)
  (cond
   ((null? args)
    '(0 . 0))
   ((eq? (car args) 'dst)
    `(1 . ,(length (cdr args))))
   (else
    `(0 . ,(length args)))))

(define-syntax define-prim
  (syntax-rules ()
    ((_ (name asm (ty arg) ...) <body>)
     (begin
       (define-prim (name asm arg ...)
         <body>)
       (hashq-set! *native-prim-arities* 'name (arity-of-args '(arg ...)))
       (hashq-set! *native-prim-types* 'name `(,ty ...))))
    ((_ (name asm arg ...) <body>)
     (begin
       (define (name asm arg ...)
         (let ((verbosity (lightning-verbosity)))
           (when (and verbosity (<= 4 verbosity))
             (jit-note (format #f "~a" `(name ,arg ...)) 0))
           (debug 3 ";;; (~12a ~{~a~^ ~})~%" 'name `(,arg ...)))
         <body>)
       (hashq-set! *native-prim-procedures* 'name name)))))

(define (initialize-tjit-primitives)
  (for-each
   (match-lambda
    ((name . arity)
     (module-add! the-root-module name (make-variable #f))
     (add-interesting-primitive! name)
     (hashq-set! (force (@@ (language cps primitives) *prim-instructions*))
                 name name)
     (hashq-set! (@@ (language cps primitives) *prim-arities*) name arity)))
   (hash-fold acons '() *native-prim-arities*)))


;;;
;;; Guards
;;;

(define-prim (%eq asm (int a) (int b))
  (let ((next (jit-forward)))
    (cond
     ((and (gpr? a) (constant? b))
      (jump (jit-beqi (gpr a) (constant b)) next))
     ((and (gpr? a) (gpr? b))
      (jump (jit-beqr (gpr a) (gpr b)) next))
     ((and (memory? a) (gpr? b))
      (memory-ref asm r0 a)
      (jump (jit-beqr r0 (gpr b)) next))
     ((and (memory? a) (memory? b))
      (memory-ref asm r0 a)
      (memory-ref asm r1 b)
      (jump (jit-beqr r0 r1) (next)))
     (else
      (error "%eq" a b)))
    (goto-exit asm)
    (jit-link next)))

(define-prim (%ne asm (int a) (int b))
  (let ((next (jit-forward)))
    (cond
     ((and (constant? a) (gpr? b))
      (jump (jit-bnei (gpr b) (constant a)) next))
     ((and (constant? a) (memory? b))
      (memory-ref asm r0 b)
      (jump (jit-bnei r0 (constant a)) next))

     ((and (gpr? a) (constant? b))
      (jump (jit-bnei (gpr a) (constant b)) next))
     ((and (gpr? a) (gpr? b))
      (jump (jit-bner (gpr a) (gpr b)) next))
     ((and (gpr? a) (memory? b))
      (memory-ref asm r0 b)
      (jump (jit-bner (gpr a) r0) next))

     ((and (memory? a) (constant? b))
      (memory-ref asm r0 a)
      (jump (jit-bnei r0 (constant b)) next))
     ((and (memory? a) (gpr? b))
      (memory-ref asm r0 a)
      (jump (jit-bner r0 (gpr b)) next))
     ((and (memory? a) (memory? b))
      (memory-ref asm r0 a)
      (memory-ref asm r1 b)
      (jump (jit-bner r0 r1) next))
     (else
      (error "%ne" a b)))
    (goto-exit asm)
    (jit-link next)))

(define-prim (%lt asm (int a) (int b))
  (let ((next (jit-forward)))
    (cond
     ((and (constant? a) (gpr? b))
      (jit-movi r0 (constant a))
      (jump (jit-bltr r0 (gpr b)) next))
     ((and (constant? a) (memory? b))
      (jit-movi r0 (constant a))
      (memory-ref asm r1 b)
      (jump (jit-bltr r0 r1) next))

     ((and (gpr? a) (constant? b))
      (jump (jit-blti (gpr a) (constant b)) next))
     ((and (gpr? a) (gpr? b))
      (jump (jit-bltr (gpr a) (gpr b)) next))
     ((and (gpr? a) (memory? b))
      (memory-ref asm r0 b)
      (jump (jit-bltr (gpr a) r0) next))

     ((and (memory? a) (constant? b))
      (memory-ref asm r0 a)
      (jump (jit-blti r0 (constant b)) next))
     ((and (memory? a) (gpr? b))
      (memory-ref asm r0 a)
      (jump (jit-bltr r0 (gpr a)) next))
     ((and (memory? a) (memory? b))
      (memory-ref asm r0 a)
      (memory-ref asm r1 b)
      (jump (jit-bltr r0 r1) next))

     (else
      (error "%lt" a b)))

    (goto-exit asm)
    (jit-link next)))

(define-prim (%ge asm (int a) (int b))
  (let ((next (jit-forward)))
    (cond
     ((and (constant? a) (gpr? b))
      (jit-movi r0 (constant a))
      (jump (jit-bger r0 (gpr b)) next))
     ((and (constant? a) (memory? b))
      (jit-movi r0 (constant a))
      (memory-ref asm r1 b)
      (jump (jit-bger r0 r1) next))

     ((and (gpr? a) (constant? b))
      (jump (jit-bgei (gpr a) (constant b)) next))
     ((and (gpr? a) (gpr? b))
      (jump (jit-bger (gpr a) (gpr b)) next))
     ((and (gpr? a) (memory? b))
      (memory-ref asm r0 b)
      (jump (jit-bger (gpr a) r0) next))

     ((and (memory? a) (gpr? b))
      (memory-ref asm r0 a)
      (jump (jit-bger r0 (gpr b)) next))
     ((and (memory? a) (memory? b))
      (memory-ref asm r0 a)
      (memory-ref asm r1 b)
      (jump (jit-bger r0 r1) next))

     (else
      (error "%ge" a b)))
    (goto-exit asm)
    (jit-link next)))

(define-prim (%flt asm (double a) (double b))
  (let ((next (jit-forward)))
    (cond
     ((and (constant? a) (fpr? b))
      (jit-movi-d f0 (constant a))
      (jump (jit-bltr-d f0 (fpr b)) next))

     ((and (fpr? a) (constant? b))
      (jump (jit-blti-d (fpr a) (constant b)) next))
     ((and (fpr? a) (fpr? b))
      (jump (jit-bltr-d (fpr a) (fpr b)) next))

     (else
      (error "%flt" a b)))
    (goto-exit asm)
    (jit-link next)))

(define-prim (%fge asm (double a) (double b))
  (let ((next (jit-forward)))
    (cond
     ((and (fpr? a) (fpr? b))
      (jump (jit-bger-d (fpr a) (fpr b)) next))
     ((and (fpr? a) (memory? b))
      (memory-ref/fpr asm f0 b)
      (jump (jit-bger-d (fpr a) f0) next))

     ((and (memory? a) (memory? b))
      (memory-ref/fpr asm f0 a)
      (memory-ref/fpr asm f1 b)
      (jump (jit-bger-d f0 f1) next))

     (else
      (error "%fge" a b)))
    (goto-exit asm)
    (jit-link next)))

(define-prim (%guard-fx asm (int obj))
  (let ((next (jit-forward)))
    (cond
     ((gpr? obj)
      (jump (scm-inump (gpr obj)) next))
     ((memory? obj)
      (memory-ref asm r0 obj)
      (jump (scm-inump r0) next))
     (else
      (error "%guard-fx" obj)))
    (goto-exit asm)
    (jit-link next)))

;;; XXX: Make low level instructions to compare typ16, rewrite this guard.
(define-prim (%guard-fl asm (int obj))
  (let ((exit (jit-forward))
        (next (jit-forward)))
    (cond
     ((gpr? obj)
      (jump (scm-imp (gpr obj)) exit)
      (scm-cell-type r0 (gpr obj))
      (scm-typ16 r0 r0)
      (jump (scm-realp r0) next))
     ((memory? obj)
      (memory-ref asm r0 obj)
      (jump (scm-imp r0) exit)
      (scm-cell-type r0 r0)
      (scm-typ16 r0 r0)
      (jump (scm-realp r0) next))
     (else
      (error "%guard-fl" obj)))
    (jit-link exit)
    (goto-exit asm)
    (jit-link next)))


;;;
;;; Exact integer
;;;

(define-prim (%add asm (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (constant? a) (constant? b))
    (jit-movi (gpr dst) (imm (+ (ref-value a) (ref-value b)))))
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-addi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-addr (gpr dst) (gpr a) (gpr b)))
   ((and (gpr? dst) (gpr? a) (memory? b))
    (jit-ldxi r0 fp (moffs asm b))
    (jit-addr (gpr dst) (gpr a) r0))
   ((and (gpr? dst) (memory? a) (gpr? b))
    (%add asm dst b a))
   ((and (gpr? dst) (memory? a) (memory? b))
    (jit-ldxi r0 fp (moffs asm a))
    (jit-ldxi r1 fp (moffs asm b))
    (jit-addr (gpr dst) r0 r1))

   ((and (memory? dst) (constant? a) (constant? b))
    (jit-movi r0 (imm (+ (ref-value a) (ref-value b))))
    (memory-set! asm dst r0))
   ((and (memory? dst) (constant? a) (gpr? b))
    (jit-addi r0 (gpr b) (constant a))
    (memory-set! asm dst r0))
   ((and (memory? dst) (gpr? a) (constant? b))
    (%add asm dst b a))
   ((and (memory? dst) (gpr? a) (gpr? b))
    (jit-ldxi r0 fp (moffs asm dst))
    (jit-addr r0 (gpr a) (gpr b))
    (jit-stxi (moffs asm dst) fp r0))
   ((and (memory? dst) (gpr? a) (memory? b))
    (jit-ldxi r0 fp (moffs asm dst))
    (jit-ldxi r1 fp (moffs asm b))
    (jit-addr r0 (gpr a) r1)
    (jit-stxi (moffs asm dst) fp r0))
   ((and (memory? dst) (constant? a) (memory? b))
    (memory-ref asm r0 b)
    (jit-addi r0 r0 (constant a))
    (memory-set! asm dst r0))
   ((and (memory? dst) (memory? a) (constant? b))
    (%add asm dst b a))
   ((and (memory? dst) (memory? a) (gpr? b))
    (%add asm dst b a))
   ((and (memory? dst) (memory? a) (memory? b))
    (jit-ldxi r0 fp (moffs asm dst))
    (jit-ldxi r1 fp (moffs asm a))
    (jit-ldxi r2 fp (moffs asm b))
    (jit-addr r0 r1 r2)
    (jit-stxi (moffs asm dst) fp r0))
   (else
    (error "%add" dst a b))))

(define-prim (%sub asm (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-subi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (memory? a) (constant? b))
    (memory-ref asm r0 a)
    (jit-subi (gpr dst) r0 (constant b)))

   ((and (memory? dst) (gpr? a) (constant? b))
    (jit-subi r0 (gpr a) (constant b))
    (memory-set! asm dst r0))
   ((and (memory? dst) (memory? a) (constant? b))
    (memory-ref asm r0 a)
    (jit-subi r0 r0 (constant b))
    (memory-set! asm dst r0))

   (else
    (error "%sub" dst a b))))

(define-prim (%rsh asm (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-rshr (gpr dst) (gpr a) (gpr b)))
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-rshi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (memory? a) (constant? b))
    (memory-ref asm r0 a)
    (jit-rshi (gpr dst) r0 (constant b)))

   ((and (memory? dst) (gpr? a) (constant? b))
    (jit-rshi r0 (gpr a) (constant b))
    (memory-set! asm dst r0))
   ((and (memory? dst) (memory? a) (constant? b))
    (memory-ref asm r0 a)
    (jit-rshi r0 r0 (constant b))
    (memory-set! asm dst r0))
   (else
    (error "%rsh" dst a b))))

(define-prim (%lsh asm (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (constant? a) (constant? b))
    (jit-movi (gpr dst) (imm (ash (ref-value a) (ref-value b)))))
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-lshi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-lshr (gpr dst) (gpr a) (gpr b)))

   ((and (memory? dst) (constant? a) (constant? b))
    (jit-movi r0 (imm (ash (ref-value a) (ref-value b))))
    (memory-set! asm dst r0))
   ((and (memory? dst) (gpr? a) (constant? b))
    (jit-lshi r0 (gpr a) (constant b))
    (memory-set! asm dst r0))
   ((and (memory? dst) (memory? a) (constant? b))
    (memory-ref asm r0 a)
    (jit-lshi r0 r0 (constant b))
    (memory-set! asm dst r0))
   (else
    (error "%lsh" dst a b))))


;;;
;;; Floating point
;;;

;;; XXX: Make lower level operation and rewrite.
(define-prim (%from-double asm (int dst) (double src))
  (cond
   ((and (gpr? dst) (constant? src))
    (jit-movi-d f0 (constant src))
    (scm-from-double (gpr dst) f0))
   ((and (gpr? dst) (fpr? src))
    (scm-from-double (gpr dst) (fpr src)))
   ((and (gpr? dst) (memory? src))
    (jit-ldxi-d f0 fp (moffs asm src))
    (scm-from-double (gpr dst) f0))

   ((and (memory? dst) (constant? src))
    (jit-movi-d f0 (constant src))
    (scm-from-double r0 f0)
    (jit-stxi (moffs asm dst) fp r0))
   ((and (memory? dst) (fpr? src))
    (scm-from-double r0 (fpr src))
    (jit-stxi (moffs asm dst) fp r0))
   ((and (memory? dst) (memory? src))
    (jit-ldxi-d f0 fp (moffs asm src))
    (scm-from-double r0 f0)
    (jit-stxi (moffs asm dst) f0 r0))
   (else
    (error "*** %scm-from-double: ~a ~a~%" dst src))))

(define-prim (%fadd asm (double dst) (double a) (double b))
  (cond
   ((and (fpr? dst) (constant? a) (fpr? b))
    (jit-addi-d (fpr dst) (fpr b) (constant a)))
   ((and (fpr? dst) (fpr? a) (constant? b))
    (%fadd asm dst b a))
   ((and (fpr? dst) (fpr? a) (fpr? b))
    (jit-addr-d (fpr dst) (fpr a) (fpr b)))

   ((and (memory? dst) (fpr? a) (fpr? b))
    (jit-addr-d f0 (fpr a) (fpr b))
    (memory-set!/fpr asm dst f0))
   ((and (memory? dst) (memory? a) (constant? b))
    (memory-ref/fpr asm f0 a)
    (jit-addi-d f0 f0 (constant b))
    (memory-set!/fpr asm dst f0))
   ((and (memory? dst) (memory? a) (fpr? b))
    (memory-ref/fpr asm f0 a)
    (jit-addr-d f0 f0 (fpr b))
    (memory-set!/fpr asm dst f0))
   (else
    (error "%fadd" dst a b))))

(define-prim (%fsub asm (double dst) (double a) (double b))
  (cond
   ((and (fpr? dst) (fpr? a) (fpr? b))
    (jit-subr-d (fpr dst) (fpr a) (fpr b)))
   ((and (fpr? dst) (constant? a) (fpr? b))
    (jit-movi-d f0 (constant a))
    (jit-subr-d (fpr dst) f0 (fpr b)))
   ((and (fpr? dst) (fpr? a) (constant? b))
    (jit-subi-d (fpr dst) (fpr a) (constant b)))

   ((and (memory? dst) (fpr? a) (fpr? b))
    (jit-subr-d f0 (fpr a) (fpr b))
    (memory-set!/fpr asm dst f0))
   ((and (memory? dst) (memory? a) (memory? b))
    (memory-ref/fpr asm f0 a)
    (memory-ref/fpr asm f1 b)
    (jit-subr-d f0 f0 f1)
    (memory-set!/fpr asm dst f0))
   (else
    (error "%fsub" dst a b))))

(define-prim (%fmul asm (double dst) (double a) (double b))
  (cond
   ((and (fpr? dst) (constant? a) (fpr? b))
    (jit-muli-d (fpr dst) (fpr b) (constant a)))
   ((and (fpr? dst) (fpr? a) (constant? b))
    (%fmul asm dst b a))
   ((and (fpr? dst) (fpr? a) (fpr? b))
    (jit-mulr-d (fpr dst) (fpr a) (fpr b)))

   ((and (memory? dst) (constant? a) (fpr? b))
    (jit-muli-d f0 (fpr b) (constant a))
    (memory-set!/fpr asm dst f0))
   ((and (memory? dst) (memory? a) (fpr? b))
    (memory-ref/fpr asm f0 a)
    (jit-mulr-d f0 f0 (fpr b))
    (memory-set!/fpr asm dst f0))
   (else
    (error "%fmul" dst a b))))


;;;
;;; Load and store
;;;

;;; XXX: Not sure whether it's better to couple `xxx-ref' and `xxx-set!'
;;; instructions with expected type as done in bytecode, to have vector-ref,
;;; struct-ref, box-ref, string-ref, fluid-ref, bv-u8-ref ... etc or not. When
;;; instructions specify its operand type, size of CPS will be more compact, but
;;; may loose chances to optimize away type checking instructions.

(define-prim (%cell-object asm (int dst) (int src) (int n))
  (cond
   ((and (gpr? dst) (constant? src) (constant? n))
    (let ((addr (+ (ref-value src) (* (ref-value n) %word-size))))
      (jit-ldi (gpr dst) (imm addr))))
   ((and (gpr? dst) (gpr? src) (constant? n))
    (jit-ldxi (gpr dst) (gpr src) (constant-word n)))

   ((and (memory? dst) (constant? src) (constant? n))
    (let ((addr (+ (ref-value src) (* (ref-value n) %word-size))))
      (jit-ldi r0 (imm addr))
      (memory-set! asm dst r0)))
   ((and (memory? dst) (gpr? src) (constant? n))
    (jit-ldxi r0 (gpr src) (constant-word n))
    (memory-set! asm dst r0))
   ((and (memory? dst) (memory? src) (constant? n))
    (memory-ref asm r0 src)
    (jit-ldxi r0 r0 (constant-word n))
    (memory-set! asm dst r0))
   (else
    (error "%cell-object" dst src n))))

(define-prim (%cell-object-f asm (double dst) (int src) (int idx))
  (cond
   ((and (fpr? dst) (gpr? src) (constant? idx))
    (jit-ldxi-d (fpr dst) (gpr src) (constant-word idx)))
   ((and (fpr? dst) (memory? src) (constant? idx))
    (memory-ref asm r0 src)
    (jit-ldxi-d (fpr dst) r0 (constant-word idx)))

   ((and (memory? dst) (memory? src) (constant? idx))
    (memory-ref asm r0 src)
    (jit-ldxi-d f0 r0 (constant-word idx))
    (memory-set!/fpr asm dst f0))

   (else
    (error "%cell-object-f" dst src idx))))

(define-prim (%set-cell-object! asm (int cell) (int n) (int src))
  (cond
   ((and (gpr? cell) (constant? n) (gpr? src))
    (jit-stxi (constant-word n) (gpr cell) (gpr src)))
   ((and (gpr? cell) (constant? n) (memory? src))
    (memory-ref asm r0 src)
    (jit-stxi (constant-word n) (gpr cell) r0))

   ((and (memory? cell) (constant? n) (gpr? src))
    (memory-ref asm r0 cell)
    (jit-stxi (constant-word n) r0 (gpr src)))
   ((and (memory? cell) (constant? n) (memory? src))
    (memory-ref asm r0 cell)
    (memory-ref asm r1 src)
    (jit-stxi (constant-word n) r0 r1))

   (else
    (error "%set-cell-object!" cell n src))))


;;;
;;; Frame instructions
;;;

;;; XXX: Perhaps remove or modify these instructions, manage side exit and
;;; snapshot data somewhere else.

(define-prim (%frame-ref asm (int dst) (void idx))
  (cond
   ((not (constant? idx))
    (error "%frame-ref: type mismatch~%" dst idx))
   ((gpr? dst)
    (local-ref (gpr dst) (ref-value idx)))
   ((memory? dst)
    (local-ref r0 (ref-value idx))
    (memory-set! asm dst r0))
   (else
    (error "%frame-ref" dst idx))))

(define-prim (%frame-set! asm (void idx) (int src))
  (cond
   ((not (constant? idx))
    (debug 2 "*** %frame-set!: type mismatch ~a ~a~%" idx src))
   ((constant? src)
    (jit-movi r0 (constant src))
    (local-set! (ref-value idx) r0))
   ((gpr? src)
    (local-set! (ref-value idx) (gpr src)))
   ((memory? src)
    (jit-ldxi r0 fp (moffs asm src))
    (local-set! (ref-value idx) r0))
   (else
    (error "%frame-set!" idx src))))


;;;
;;; Calls
;;;

;; (define-prim (%native-call asm (void addr))
;;   (cond
;;    ((constant? addr)
;;     (jit-prepare)
;;     (jit-pushargr reg-thread)           ; thread
;;     (jit-ldxi r0 fp vp->fp-offset)
;;     (jit-pushargr r0)         ; vp->fp
;;     ;; (jit-pushargi %null-pointer)        ; registers
;;     (jit-movi r0 (constant addr))
;;     (jit-callr r0))
;;    (else
;;     (error "%native-call" addr))))


;;;
;;; Code generation
;;;

(define (move moffs dst src)
  (cond
   ((and (gpr? dst) (constant? src))
    (jit-movi (gpr dst) (constant src)))
   ((and (gpr? dst) (gpr? src))
    (jit-movr (gpr dst) (gpr src)))
   ((and (gpr? dst) (memory? src))
    (jit-ldxi r0 fp (moffs src))
    (jit-movr (gpr dst) r0))

   ((and (fpr? dst) (constant? src))
    (jit-movi-d (fpr dst) (constant src)))
   ((and (fpr? dst) (fpr? src))
    (jit-movr-d (fpr dst) (fpr src)))
   ((and (fpr? dst) (memory? src))
    (jit-ldxi-d f0 fp (moffs src))
    (jit-movr-d (fpr dst) f0))

   ((and (memory? dst) (constant? src))
    (let ((val (ref-value src)))
      (cond
       ((fixnum? val)
        (jit-movi r0 (constant src))
        (jit-stxi (moffs dst) fp r0))
       ((flonum? val)
        (jit-movi-d f0 (constant src))
        (jit-stxi-d (moffs dst) fp f0)))))
   ((and (memory? dst) (gpr? src))
    (jit-stxi (moffs dst) fp (gpr src)))
   ((and (memory? dst) (fpr? src))
    (jit-stxi-d (moffs dst) fp (fpr src)))
   ((and (memory? dst) (memory? src))
    (jit-ldxi r0 fp (moffs src))
    (jit-stxi (moffs dst) fp r0))

   (else
    (debug 2 "*** move: ~a ~a~%" dst src))))

(define (load-frame moffs local type dst)
  (cond
   ((eq? type &exact-integer)
    (cond
     ((gpr? dst)
      (local-ref (gpr dst) local)
      (jit-rshi (gpr dst) (gpr dst) (imm 2)))
     ((memory? dst)
      (local-ref r0 local)
      (jit-rshi r0 r0 (imm 2))
      (jit-stxi (moffs dst) fp r0))))
   ((eq? type &flonum)
    (cond
     ((fpr? dst)
      (local-ref r0 local)
      (scm-real-value (fpr dst) r0))
     ((memory? dst)
      (local-ref r0 local)
      (scm-real-value f0 r0)
      (jit-stxi-d (moffs dst) fp f0))))
   ((eq? type &box)
    (cond
     ((gpr? dst)
      (local-ref (gpr dst) local))
     ((memory? dst)
      (local-ref r0 local)
      (jit-stxi (moffs dst) fp r0))))
   (else
    (error "load-frame" local type dst))))

(define (store-frame moffs local type src)
  (cond
   ((eq? type &exact-integer)
    (cond
     ((constant? src)
      (jit-movi r0 (constant src))
      (jit-lshi r0 r0 (imm 2))
      (jit-addi r0 r0 (imm 2))
      (local-set! local r0))
     ((gpr? src)
      (jit-lshi r0 (gpr src) (imm 2))
      (jit-addi r0 r0 (imm 2))
      (local-set! local r0))
     ((memory? src)
      (jit-ldxi r0 fp (moffs src))
      (jit-lshi r0 r0 (imm 2))
      (jit-addi r0 r0 (imm 2))
      (local-set! local r0))))
   ((eq? type &flonum)
    (cond
     ((constant? src)
      (jit-movi-d f0 (constant src))
      (scm-from-double r0 f0)
      (local-set! local r0))
     ((fpr? src)
      (scm-from-double r0 (fpr src))
      (local-set! local r0))
     ((memory? src)
      (jit-ldxi-d f0 fp (moffs src))
      (scm-from-double r0 f0)
      (local-set! local r0))))
   ((eq? type &box)
    (cond
     ((gpr? src)
      (local-set! local (gpr src)))
     ((memory? src)
      (jit-ldxi r0 fp (moffs src))
      (local-set! local r0))
     (else
      (error "store-frame: box" local type src))))
   ((eq? type &false)
    (jit-movi r0 (scm->pointer #f))
    (local-set! local r0))
   (else
    (error "Unknown local, type, src" local type src))))

(define (assemble-cps cps env kstart entry-ip snapshots initial-args fp-offset
                      tlog trampoline end-address linked-ip)
  (define (env-ref i)
    (vector-ref env i))

  (define (moffs x)
    (make-offset-pointer fp-offset (* (ref-value x) %word-size)))

  (define exit-codes (make-hash-table))

  (define exit-variables (make-hash-table))

  (define current-side-exit 0)

  (define loop-locals #f)

  (define loop-vars #f)

  (define (dump-exit-variables)
    (debug 2 ";;; exit-variables:~%")
    (hash-for-each (lambda (k v)
                     (debug 2 "~a => ~a~%" k v))
                   exit-variables))

  (define (dump-bailout ip current-side-exit code)
    (let ((verbosity (lightning-verbosity)))
      (when (and verbosity (<= 3 verbosity))
        (call-with-output-file
            (format #f "/tmp/bailout-~x-~4,,,'0@a.o"
                    ip current-side-exit)
          (lambda (port)
            (put-bytevector port code)
            (jit-print))))))

  (define (maybe-move exp old-args)
    (match exp
      (($ $values new-args)
       (when (= (length old-args) (length new-args))
         (for-each
          (lambda (old new)
            (when (not (eq? old new))
              (let ((dst (env-ref old))
                    (src (env-ref new)))
                (when (not (and (eq? (ref-type dst) (ref-type src))
                                (eq? (ref-value dst) (ref-value src))))
                  (debug 3 ";;; (%mov         ~a ~a)~%" dst src)
                  (move moffs dst src)))))
          old-args new-args)))
      (($ $primcall name args)
       (assemble-prim name initial-args args))
      (_
       (debug 2 "*** maybe-move: ~a ~a~%" exp old-args))))

  (define (assemble-prim name dsts args)
    (cond
     ((hashq-ref *native-prim-procedures* name)
      =>
      (lambda (proc)
        (let* ((side-exit (trampoline-ref trampoline (- current-side-exit 1)))
               (asm (make-asm env fp-offset side-exit end-address)))
          (apply proc asm
                 (map (lambda (arg)
                        (env-ref arg))
                      (append dsts args))))))
     (else
      (error "Unhandled primcall" name))))

  (define (assemble-exit proc args)
    (define (jump-to-linked-code)
      (let* ((tlog (get-tlog linked-ip))
             (current-locals (make-hash-table))
             (loop-locals (tlog-loop-locals tlog)))
        (cond
         ((hashq-ref snapshots current-side-exit)
          =>
          (lambda (local-x-types)
            ;; XXX: Lots of code are duplicating with side trace
            ;; initialization. Factor out and share.
            (for-each
             (lambda (local-x-type arg)
               (let ((local (car local-x-type))
                     (type (cdr local-x-type)))
                 ;; Value at the end of side trace is not passed to linked
                 ;; native code, store to frame.
                 (when (not (assq local loop-locals))
                   (store-frame moffs local type (env-ref arg)))
                 (hashq-set! current-locals local (env-ref arg))))
             local-x-types args)

            ;; Move registers first, then load from frame.
            (let ((variables-to-load-from-frame
                   (let lp ((locals loop-locals)
                            (dsts (tlog-loop-vars tlog))
                            (acc '()))
                     (match locals
                       (((local . type) . locals)
                        (match dsts
                          ((dst . dsts)
                           (cond
                            ((hashq-ref current-locals local)
                             =>
                             (lambda (src)
                               ;; XXX: src could be overwritten by move.
                               (move moffs dst src)
                               (lp locals dsts acc)))
                            (else
                             (let ((ltd (list local type dst)))
                               (lp locals dsts (cons ltd acc))))))))
                       (() (reverse! acc))))))
              (for-each
               (match-lambda ((local type dst)
                              (load-frame moffs local type dst)))
               variables-to-load-from-frame))

            ;; Jump to beginning of the loop in linked code.
            (jumpi (tlog-loop-address tlog))))
         (else
          (debug 2 ";;; assemble-exit: IP is 0, local info not found~%")))))

    (define (save-loop-info)
      (cond
       ((hashq-ref snapshots (- current-side-exit 1))
        =>
        (lambda (local-x-types)
          (set! loop-locals local-x-types)
          (set! loop-vars (map env-ref args))))
       (else
        (debug 2 ";;; end-of-entry: no snapshot at ~a~%" current-side-exit))))

    (define (emit-bailout next-ip)
      (define (make-retval dst)
        (jit-prepare)
        (jit-pushargr reg-thread)
        (jit-pushargi (imm (+ (ash next-ip 2) 2)))
        (jit-pushargi (imm (+ (ash current-side-exit 2) 2)))
        (jit-pushargi (imm (+ (ash entry-ip 2) 2)))
        (jit-pushargi (scm->pointer #f))
        (jit-calli %scm-do-inline-double-cell)
        (jit-retval dst))

      (with-jit-state
       (jit-prolog)
       (jit-tramp (imm (* 4 %word-size)))
       (cond
        ((hashq-ref snapshots (- current-side-exit 1))
         =>
         (lambda (local-x-types)
           (for-each
            (lambda (local-x-type arg)
              ;; XXX: Save frame of parent trace when compiling side trace?
              (let ((local (car local-x-type))
                    (type (cdr local-x-type)))
                (store-frame moffs local type (env-ref arg))))
            local-x-types args)))
        (else
         (debug 2 ";;; assemble-exit: entry IP ~x~%" next-ip)))

       ;; Caller places return address to address `fp + ra-offset', and
       ;; expects `R0' to hold the packed return values.
       (make-retval r0)
       (jit-ldxi r1 fp ra-offset)
       (jit-jmpr r1)
       (jit-epilog)
       (jit-realize)
       (let* ((estimated-code-size (jit-code-size))
              (code (make-bytevector estimated-code-size)))
         (jit-set-code (bytevector->pointer code) (imm estimated-code-size))
         (let ((ptr (jit-emit)))
           (make-bytevector-executable! code)
           (dump-bailout next-ip current-side-exit code)
           (hashq-set! exit-codes current-side-exit code)
           (hashq-set! exit-variables current-side-exit (map env-ref args))
           (trampoline-set! trampoline current-side-exit ptr)
           (set! current-side-exit (+ current-side-exit 1))))))

    (let* ((proc (env-ref proc))
           (ip (ref-value proc)))
      (cond
       ((not (constant? proc))
        (error "assemble-exit: not a constant" proc))
       ((= ip *ip-key-end-of-side-trace*)
        (jump-to-linked-code))
       ((= ip *ip-key-end-of-entry*)
        (save-loop-info))
       (else                            ; IP is bytecode destination.
        (emit-bailout ip)))))

  (define (assemble-exp exp dsts)
    (match exp
      (($ $primcall name args)
       (assemble-prim name dsts args))
      (($ $call proc args)
       (assemble-exit proc args))
      (_
       ;; (debug 2 "      exp:~a~%" exp)
       (values))))

  (define (assemble-cont cps exp loop-label k)
    (define (done)
      (dump-exit-variables)
      (values exit-variables exit-codes
              trampoline loop-label loop-locals loop-vars fp-offset))

    (match (intmap-ref cps k)
      (($ $kreceive _ knext)
       (assemble-cont cps exp loop-label knext))

      (($ $kclause _ knext)
       (assemble-cont cps exp loop-label knext))

      (($ $kfun _ _ _ _ knext)
       (assemble-cont cps exp loop-label knext))

      (($ $kargs _ syms ($ $continue knext _ next-exp))
       (assemble-exp exp syms)
       (cond
        ((< knext k)
         ;; Jump back to beginning of loop, return to caller.
         (maybe-move next-exp initial-args)
         (debug 3 ";;; -> loop~%")
         (jump loop-label)
         (done))
        ((= kstart k)
         ;; Emit label to mark beginning of the loop.
         (debug 3 ";;; loop:~%")
         (jit-note "loop" 0)
         (assemble-cont cps next-exp (jit-label) knext))
        (else
         (assemble-cont cps next-exp loop-label knext))))

      (($ $ktail)
       (assemble-exp exp '())
       (done))))

  (assemble-cont cps '() #f 0))

(define (assemble-tjit cps entry-ip locals snapshots tlog exit-id linked-ip)
  (define (max-moffs env)
    (let lp ((i 0) (end (vector-length env)) (current 0))
      (if (< i end)
          (let ((var (vector-ref env i)))
            (if (and (memory? var) (< current (ref-value var)))
                (lp (+ i 1) end (ref-value var))
                (lp (+ i 1) end current)))
          current)))

  (let*-values
      (((max-label max-var) (compute-max-label-and-var cps))
       ((env initial-locals loop-args kstart)
        (resolve-variables cps locals max-var)))
    (let* ((trampoline-size
            (hash-fold (lambda (k v acc) (+ acc 1)) 1 snapshots))
           (trampoline (make-trampoline trampoline-size))
           (end-addr (and tlog (tlog-end-address tlog))))
      (cond
       ((not tlog)                      ; Root trace.

        ;; Allocate spaces for spilled variables, two words for arguments passed
        ;; from C code: `vp->fp', and `registers', and one word for return
        ;; address used by side exits.
        (let* ((nspills (max-moffs env))
               (fp-offset (jit-allocai (imm (* (+ nspills 3) %word-size))))
               (moffs (lambda (mem)
                        (let ((offset (* (ref-value mem) %word-size)))
                          (make-offset-pointer fp-offset offset)))))

          ;; Get arguments.
          (jit-getarg reg-thread (jit-arg)) ; thread
          (jit-getarg r0 (jit-arg))         ; vp->fp
          (jit-getarg r1 (jit-arg))         ; registers, for prompt

          ;; Load and store `vp->fp' and `registers'.
          (jit-stxi vp->fp-offset fp r0)
          (jit-stxi registers-offset fp r1)

          ;; Load initial locals.
          (for-each
           (match-lambda
            ((local . var)
             (load-frame moffs local &box (vector-ref env var))))
           initial-locals)

          ;; Assemble the primitives in CPS.
          (assemble-cps cps env kstart entry-ip snapshots loop-args fp-offset
                        tlog trampoline end-addr linked-ip)))

       (else                            ; Side trace.
        (let* ((nspills (max-moffs env))
               (fp-offset (tlog-fp-offset tlog)))
          (debug 2 ";;; side trace: nspills=~a~%" nspills)

          ;; XXX: Cannot allocate more memory if nspills of side trace is
          ;; greater than area allocated by the parent trace, because side
          ;; traces use `jit-tramp'.  Perhaps better to allocate constant amount
          ;; in root trace, and make the amount configurable via parameter.
          (jit-tramp (imm (* 4 %word-size)))

          ;; Load initial arguments from parent trace.
          (let ((args (make-hash-table))
                (moffs
                 (lambda (mem)
                   (let ((offset (* (ref-value mem) %word-size)))
                     (make-offset-pointer fp-offset offset)))))
            (for-each
             (lambda (local-x-type var)
               (let ((local (car local-x-type))
                     (type (cdr local-x-type)))
                 ;; Save to frame when values from parent trace are not passed
                 ;; to side trace.
                 (when (not (assq local initial-locals))
                   (store-frame moffs local type var))
                 (hashq-set! args local var)))
             (hashq-ref (tlog-snapshots tlog) (- exit-id 1))
             (hashq-ref (tlog-exit-variables tlog) exit-id))

            ;; Move variables from parent trace before loading from frame.
            ;;
            ;; Values stored in register passed from parent trace could be
            ;; overwritten by values loaded from frame, since the pairing of
            ;; locals and register could be different from how it was done in
            ;; parent trace.
            ;;
            (let* ((snapshot (hashq-ref snapshots 0))
                   (variables-to-load-from-frame
                    (let lp ((locals initial-locals) (acc '()))
                      (match locals
                        (((local . var) . locals)
                         (let ((dst (vector-ref env var)))
                           (cond
                            ((hashq-ref args local)
                             =>
                             (lambda (src)
                               ;; XXX: src could be overwritten by move.
                               (move moffs dst src)
                               (lp locals acc)))
                            (else
                             (let* ((type (assq-ref snapshot local))
                                    (ltd (list local type dst)))
                               (lp locals (cons ltd acc)))))))
                        (() (reverse! acc))))))
              (for-each
               (match-lambda ((local type dst)
                              (load-frame moffs local type dst)))
               variables-to-load-from-frame)))

          ;; Assemble the primitives in CPS.
          (assemble-cps cps env kstart entry-ip snapshots loop-args fp-offset
                        tlog trampoline end-addr linked-ip)))))))
