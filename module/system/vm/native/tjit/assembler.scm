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

;;; Primitives for native code used in vm-tjit engine.

;;; Code:

(define-module (system vm native tjit assembler)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps types)
  #:use-module (language tree-il primitives)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit variables)
  #:export (*native-prim-procedures*
            *native-prim-types*
            *scm-false*
            *scm-true*
            *scm-undefined*
            *scm-unspecified*
            initialize-tjit-primitives
            make-asm
            asm-fp-offset
            asm-out-code
            asm-end-address
            make-negative-pointer
            make-signed-pointer
            constant-word
            jump
            jumpi
            frame-ref
            frame-set!
            return-to-interpreter
            scm-from-double
            scm-frame-dynamic-link
            scm-frame-set-dynamic-link!
            scm-frame-return-address
            scm-frame-set-return-address!
            scm-real-value
            vp-offset
            vp->fp-offset))

(define (make-negative-pointer addr)
  "Make negative pointer with ADDR."
  (when (< 0 addr)
    (error "make-negative-pointer: expecting negative address" addr))
  (make-pointer (+ (expt 2 (* 8 %word-size)) addr)))

(define (make-signed-pointer addr)
  (if (<= 0 addr)
      (make-pointer addr)
      (make-negative-pointer addr)))

(define-syntax-rule (constant-word i)
  (imm (* (ref-value i) %word-size)))


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
;;; SCM macros
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

(define-syntax-rule (scm-frame-dynamic-link dst src)
  (jit-ldxi dst src (make-negative-pointer (* -2 %word-size))))

(define-syntax-rule (scm-frame-set-dynamic-link! dst src)
  (jit-stxi (make-negative-pointer (* -2 %word-size)) dst src))

(define-syntax-rule (scm-frame-return-address dst src)
  (jit-ldxi dst src (make-negative-pointer (- %word-size))))

(define-syntax-rule (scm-frame-set-return-address! dst src)
  (jit-stxi (make-negative-pointer (- %word-size)) dst src))

(define vp-offset
  (make-negative-pointer (- %word-size)))

(define vp->fp-offset
  (make-negative-pointer (- (* 2 %word-size))))

(define-syntax-rule (frame-ref dst n)
  (let ((vp->fp (if (eq? dst r0) r1 r0)))
    (jit-ldxi vp->fp fp vp->fp-offset)
    (cond
     ((= 0 n)
      (jit-ldr dst vp->fp))
     ((< 0 n)
      (jit-ldxi dst vp->fp (imm (* n %word-size))))
     (else
      (debug 2 ";;; frame-ref: skipping negative local ~a~%" n)))))

(define-syntax-rule (frame-set! n src)
  (let ((vp->fp (if (eq? src r0) r1 r0)))
    (jit-ldxi vp->fp fp vp->fp-offset)
    (cond
     ((= 0 n)
      (jit-str vp->fp src))
     ((< 0 n)
      (jit-stxi (imm (* n %word-size)) vp->fp src))
     (else
      (debug 2 ";;; frame-set!: skipping negative local ~a~%" n)))))


;;;
;;; Predicates
;;;

(define-syntax-rule (scm-imp obj)
  (jit-bmsi obj (imm 6)))

(define-syntax-rule (scm-inump obj)
  (jit-bmsi obj (imm 2)))

(define-syntax-rule (scm-not-inump obj)
  (jit-bmci obj (imm 2)))

(define-syntax-rule (scm-realp tag)
  (jit-beqi tag (imm (@@ (system base types) %tc16-real))))

(define-syntax-rule (scm-not-realp tag)
  (jit-bnei tag (imm (@@ (system base types) %tc16-real))))


;;;
;;; Assembler state
;;;

(define-record-type <asm>
  (%make-asm env fp-offset out-code end-address)
  asm?

  ;; Vector containing CPS variables. Index of vector is CPS variable ID, value
  ;; is reference to logical register or memory.
  (env asm-env)

  ;; Offset for fp register. This is the offset allocated with `jit-allocai'.
  (fp-offset asm-fp-offset)

  ;; Pointer of native code for current side exit.
  (out-code asm-out-code)

  ;; Pointer of native code at the end of parent trace.
  (end-address asm-end-address))

(define (make-asm env fp-offset out-code end-address)
  (%make-asm env fp-offset out-code end-address))

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

(define-syntax-rule (moffs asm r)
  (make-signed-pointer (+ (asm-fp-offset asm)
                          (* (ref-value r) %word-size))))

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

(define *ra-offset*
  (make-negative-pointer (- (* 4 %word-size))))

(define-syntax-rule (return-to-interpreter)
  ;; Caller places return address to address `fp + ra-offset', and expects
  ;; `reg-retval' to hold the packed return values.
  (begin
    (jit-ldxi r1 fp *ra-offset*)
    (jit-jmpr r1)))

(define-syntax-rule (goto-exit asm)
  ;; Save return address, then jump to address of asm-code of ASM.
  ;;
  ;; Expecting that out-code of asm will jump back to return address, or patched
  ;; to parent trace already. When returned to patched address, exit from native
  ;; code with returning the contents of register R0.
  ;;
  ;; Side trace reuses RSP shifting from epilog of parent trace. Native code of
  ;; side trace does not reset stack pointer, because side traces use
  ;; `jit-tramp'.
  ;;
  (let ((addr (jit-movi r1 (imm 0))))
    (jit-stxi *ra-offset* fp r1)
    (jumpi (asm-out-code asm))
    (jit-patch addr)
    (cond
     ((asm-end-address asm)
      =>
      (lambda (address)
        (debug 2 ";;; goto-exit: jumping to ~a~%" address)
        (jumpi address)))
     (else
      (debug 2 ";;; goto-exit: returning R0~%")
      (jit-retr reg-retval)))))

;;;
;;; Scheme constants

(define *scm-false*
  (scm->pointer #f))

(define *scm-true*
  (scm->pointer #t))

(define *scm-unspecified*
  (scm->pointer *unspecified*))

(define *scm-undefined*
  (make-pointer #x904))


;;; XXX: Add more helper macros, e.g: %ne, %eq, %lt, %ge have similar structure.

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

     ((and (memory? a) (constant? b))
      (memory-ref asm r0 a)
      (jump (jit-beqi r0 (constant b)) next))
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
     ((and (constant? a) (constant? b))
      (when (not (eq? (ref-value a) (ref-value b)))
        (jump next)))
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
     ((and (constant? a) (constant? b))
      (when (>= (ref-value a) (ref-value b))
        (jump next)))
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

     ((and (memory? a) (constant? b))
      (memory-ref asm r0 a)
      (jump (jit-bgei r0 (constant b)) next))
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



;; (define-prim (%guard-fx asm (int obj))
;;   (let ((next (jit-forward)))
;;     (cond
;;      ((gpr? obj)
;;       (jump (scm-inump (gpr obj)) next))
;;      ((memory? obj)
;;       (memory-ref asm r0 obj)
;;       (jump (scm-inump r0) next))
;;      (else
;;       (error "%guard-fx" obj)))
;;     (goto-exit asm)
;;     (jit-link next)))

;; ;;; XXX: Make low level instructions to compare typ16, rewrite this guard.
;; (define-prim (%guard-fl asm (int obj))
;;   (let ((exit (jit-forward))
;;         (next (jit-forward)))
;;     (cond
;;      ((gpr? obj)
;;       (jump (scm-imp (gpr obj)) exit)
;;       (scm-cell-type r0 (gpr obj))
;;       (scm-typ16 r0 r0)
;;       (jump (scm-realp r0) next))
;;      ((memory? obj)
;;       (memory-ref asm r0 obj)
;;       (jump (scm-imp r0) exit)
;;       (scm-cell-type r0 r0)
;;       (scm-typ16 r0 r0)
;;       (jump (scm-realp r0) next))
;;      (else
;;       (error "%guard-fl" obj)))
;;     (jit-link exit)
;;     (goto-exit asm)
;;     (jit-link next)))

(define-prim (%return asm (int ip))
  (let ((next (jit-forward)))
    (cond
     ((constant? ip)
      (jit-ldxi r0 fp vp->fp-offset)
      (scm-frame-return-address r0 r0)
      (jump (jit-beqi r0 (constant ip)) next))
     (else
      (error "%return" ip)))
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
   ((and (gpr? dst) (constant? a) (constant? b))
    (jit-movi (gpr dst) (imm (- (ref-value a) (ref-value b)))))
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-subi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-subr (gpr dst) (gpr a) (gpr b)))
   ((and (gpr? dst) (memory? a) (constant? b))
    (memory-ref asm r0 a)
    (jit-subi (gpr dst) r0 (constant b)))

   ((and (memory? dst) (constant? a) (constant? b))
    (jit-movi r0 (imm (- (ref-value a) (ref-value b))))
    (memory-set! asm dst r0))
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

(define-prim (%mod asm (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-remi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-remr (gpr dst) (gpr a) (gpr b)))
   (else
    (error "%mod" dst a b))))


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

;; Load frame local to gpr or memory, with type check.
(define-prim (%frame-ref asm (int dst) (void n) (void type))
  (let ((exit (jit-forward))
        (next (jit-forward)))
    (define-syntax-rule (load-constant constant)
      (begin
        (jump (jit-bnei r0 constant) exit)
        (cond
         ((gpr? dst)
          (jit-movr (gpr dst) r0))
         ((memory? dst)
          (memory-set! asm dst r0))
         (else
          (error "%frame-ref" dst n type)))))
    (when (or (not (constant? n))
              (not (constant? type)))
      (error "%frame-ref" dst n type))

    ;; XXX: Won't work when n is negative.
    (frame-ref r0 (ref-value n))

    (let ((type (ref-value type)))
      (cond
       ((eq? type &exact-integer)
        (jump (scm-not-inump r0) exit)
        (cond
         ((gpr? dst)
          (jit-rshi (gpr dst) r0 (imm 2)))
         ((memory? dst)
          (jit-rshi r0 r0 (imm 2))
          (memory-set! asm dst r0))))
       ((eq? type &false)
        (load-constant *scm-false*))
       ((eq? type &true)
        (load-constant *scm-true*))
       ((eq? type &unspecified)
        (load-constant *scm-unspecified*))
       ((eq? type &unbound)
        (load-constant *scm-undefined*))
       ((memq type (list &box &procedure &pair))
        ;; XXX: Guard each type.
        (cond
         ((gpr? dst)
          (jit-movr (gpr dst) r0))
         ((memory? dst)
          (memory-set! asm dst r0))))
       (else
        (error "frame-ref" dst n type))))
    (jump next)
    (jit-link exit)
    (goto-exit asm)
    (jit-link next)))

;; Load frame local to fpr or memory, with type check. This primitive is used
;; for loading to floating point register.
(define-prim (%frame-ref/f asm (double dst) (void n))
  (let ((exit (jit-forward))
        (next (jit-forward)))
    (when (not (constant? n))
      (error "%frame-ref/f" dst n))
    (frame-ref r0 (ref-value n))
    (jump (scm-imp r0) exit)
    (scm-cell-type r1 r0)
    (scm-typ16 r1 r1)
    (jump (scm-not-realp r1) exit)
    (cond
     ((fpr? dst)
      (scm-real-value (fpr dst) r0))
     ((memory? dst)
      (scm-real-value f0 r0)
      (memory-set!/fpr asm dst f0))
     (else
      (error "%frame-ref/f" dst n)))
    (jump next)
    (jit-link exit)
    (goto-exit asm)
    (jit-link next)))

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

(define-prim (%cell-object/f asm (double dst) (int src) (int idx))
  (cond
   ((and (fpr? dst) (gpr? src) (constant? idx))
    (jit-ldxi-d (fpr dst) (gpr src) (constant-word idx)))
   ((and (fpr? dst) (memory? src) (constant? idx))
    (memory-ref asm r0 src)
    (jit-ldxi-d (fpr dst) r0 (constant-word idx)))

   ((and (memory? dst) (gpr? src) (constant? idx))
    (jit-ldxi-d f0 (gpr src) (constant-word idx))
    (memory-set!/fpr asm dst f0))

   ((and (memory? dst) (memory? src) (constant? idx))
    (memory-ref asm r0 src)
    (jit-ldxi-d f0 r0 (constant-word idx))
    (memory-set!/fpr asm dst f0))

   (else
    (error "%cell-object/f" dst src idx))))

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
