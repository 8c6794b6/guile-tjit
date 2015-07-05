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
  #:use-module (language tree-il primitives)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit variables)
  #:export (assemble-tjit
            initialize-tjit-primitives
            *prim-types*))


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

(define %scm-from-double
  (dynamic-pointer "scm_from_double" (dynamic-link)))

(define-syntax-rule (scm-from-double dst src)
  (begin
    (jit-prepare)
    (jit-pushargr-d src)
    (jit-calli %scm-from-double)
    (jit-retval dst)))


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
  (%make-asm %env %fp-offset %out-label %loop-label)
  asm?
  (%env asm-env set-asm-env!)
  (%fp-offset fp-offset set-asm-fp-offset!)
  (%out-label out-label set-asm-out-label!)
  (%loop-label loop-label set-asm-loop-label!))

(define (make-asm env fp-offset out-label)
  (%make-asm env fp-offset out-label #f))

(define (env-ref asm i)
  (vector-ref (asm-env asm) i))

(define (moffs asm r)
  (make-offset-pointer (fp-offset asm) (* (ref-value r) %word-size)))

(define-syntax-rule (constant-word i)
  (imm (* (ref-value i) %word-size)))

;;;
;;; Auxiliary
;;;

(define reg-thread v0)

(define fp (jit-fp))

(define vp->fp-offset
  (make-pointer (+ (expt 2 (* 8 %word-size)) (- %word-size))))

(define registers-offset
  (make-pointer (+ (expt 2 (* 8 %word-size)) (- (* 2 %word-size)))))

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


;;;
;;; Primitives
;;;

;;; Primitives used for vm-tjit engine.  Primitives defined here are used during
;;; compilation from traced data to native code, and possibly useless for
;;; ordinal use as scheme procedure.
;;;
;;; Need at least 3 general purpose scratch registers, and 3 floating point
;;; scratch registers. Currently using R0, R1, and R2 for general purpose, F0,
;;; F1, and F2 for floating point.

(define *simple-prim-arities* (make-hash-table))
(define *branching-prim-arities* (make-hash-table))
(define *all-prims* (make-hash-table))
(define *prim-types* (make-hash-table))

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
       (hashq-set! *simple-prim-arities* 'name (arity-of-args '(arg ...)))
       (hashq-set! *prim-types* 'name `(,ty ...))))
    ((_ (name asm arg ...) <body>)
     (begin
       (define (name asm arg ...)
         (let ((arg (env-ref asm arg))
               ...)
           (let ((verbosity (lightning-verbosity)))
             (when (and verbosity (<= 4 verbosity))
               (jit-note (format #f "~a" `(name ,arg ...)) 0))
             (debug 3 ";;; (~12a ~{~a~^ ~})~%" 'name `(,arg ...)))
           <body>))

       (hashq-set! *all-prims* 'name name)))))

(define-syntax define-branching-prim
  (syntax-rules ()
    ((_ (name asm (ty arg) ...) <body>)
     (begin
       (define-prim (name asm arg ...)
         <body>)
       (hashq-set! *branching-prim-arities* 'name (arity-of-args '(arg ...)))
       (hashq-set! *prim-types* 'name `(,ty ...))))))

(define (initialize-tjit-primitives)
  ;; Extending (@@ (language cps primitives) branching-primitives?) procedure
  ;; with CPS primitives used in vm-tjit.  Branching primitives are merged to
  ;; CPS term during compilation from tree-il to cps, with using
  ;; `branching-primitives?' predicate.
  (define (branching-primitive? name)
    (let ((cps-branching-primcall-arities (@@ (language cps primitives)
                                              *branching-primcall-arities*)))
      (or (and (assq name cps-branching-primcall-arities) #t)
          (and (hashq-ref *branching-prim-arities* name) #t))))
  (for-each
   (match-lambda
    ((name . arity)
     (module-add! the-root-module name (make-variable #f))
     (add-interesting-primitive! name)
     (hashq-set! (force (@@ (language cps primitives) *prim-instructions*))
                 name name)
     (hashq-set! (@@ (language cps primitives) *prim-arities*) name arity)))
   (append (hash-fold acons '() *simple-prim-arities*)
           (hash-fold acons '() *branching-prim-arities*)))

  ;; Overwrite the branch predicate procedure.
  (module-define! (resolve-module '(language cps primitives))
                  'branching-primitive?
                  branching-primitive?))


;;;
;;; Calls
;;;

(define-prim (%native-call asm (void addr))
  (cond
   ((constant? addr)
    (jit-prepare)
    (jit-pushargr reg-thread)           ; thread
    (jit-ldxi r0 fp vp->fp-offset)
    (jit-pushargr r0)         ; vp->fp
    ;; (jit-pushargi %null-pointer)        ; registers
    (jit-movi r0 (constant addr))
    (jit-callr r0))
   (else
    (error "%native-call" addr))))


;;;
;;; Exact integer
;;;

(define-branching-prim (%guard-fx asm (int obj))
  (let ((next (jit-forward))
        (out (out-label asm)))
    (cond
     ((gpr? obj)
      (jump (scm-inump (gpr obj)) next))
     ((memory? obj)
      (memory-ref asm r0 obj)
      (jump (scm-inump r0) next))
     (else
      (error "%guard-fx" obj)))
    (jump out)
    (jit-link next)))

(define-branching-prim (%eq asm (int a) (int b))
  (let ((out (out-label asm))
        (next (jit-forward)))
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
    (jump out)
    (jit-link next)))

(define-branching-prim (%lt asm (int a) (int b))
  (let ((out (out-label asm))
        (next (jit-forward)))
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
    (jump out)
    (jit-link next)))

(define-prim (%add asm (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-addi (gpr dst) (gpr a) (constant b))
    ;; (jit-subi (gpr dst) (gpr dst) (imm 2))
    )
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-addr (gpr dst) (gpr a) (gpr b))
    ;; (jit-subi (gpr dst) (gpr dst) (imm 2))
    )
   ((and (gpr? dst) (memory? a) (gpr? b))
    (jit-ldxi r0 fp (moffs asm a))
    (jit-addr (gpr dst) r0 (gpr b))
    ;; (jit-subi (gpr dst) (gpr dst) (imm 2))
    )
   ((and (gpr? dst) (gpr? a) (memory? b))
    (jit-ldxi r0 fp (moffs asm b))
    (jit-addr (gpr dst) (gpr a) r0)
    ;; (jit-subi (gpr dst) (gpr dst) (imm 2))
    )
   ((and (gpr? dst) (memory? a) (memory? b))
    (jit-ldxi r0 fp (moffs asm a))
    (jit-ldxi r1 fp (moffs asm b))
    (jit-addr (gpr dst) r0 r1)
    ;; (jit-subi (gpr dst) (gpr dst) (imm 2))
    )

   ((and (memory? dst) (gpr? a) (constant? b))
    (jit-addi r0 (gpr a) (constant b))
    ;; (jit-subi r0 r0 (imm 2))
    (memory-set! asm dst r0))
   ((and (memory? dst) (gpr? a) (gpr? b))
    (jit-ldxi r0 fp (moffs asm dst))
    (jit-addr r0 (gpr a) (gpr b))
    ;; (jit-subi r0 r0 (imm 2))
    (jit-stxi (moffs asm dst) fp r0))
   ((and (memory? dst) (gpr? a) (memory? b))
    (jit-ldxi r0 fp (moffs asm dst))
    (jit-ldxi r1 fp (moffs asm b))
    (jit-addr r0 (gpr a) r1)
    ;; (jit-subi r0 r0 (imm 2))
    (jit-stxi (moffs asm dst) fp r0))

   ((and (memory? dst) (constant? a) (memory? b))
    (memory-ref asm r0 b)
    (jit-addi r0 r0 (constant a))
    (memory-set! asm dst r0))
   ((and (memory? dst) (memory? a) (constant? b))
    (memory-ref asm r0 a)
    (jit-addi r0 r0 (constant b))
    (memory-set! asm dst r0))
   ((and (memory? dst) (memory? a) (gpr? b))
    (jit-ldxi r0 fp (moffs asm dst))
    (jit-ldxi r1 fp (moffs asm a))
    (jit-addr r0 r1 (gpr b))
    ;; (jit-subi r0 r0 (imm 2))
    (jit-stxi (moffs asm dst) fp r0))
   ((and (memory? dst) (memory? a) (memory? b))
    (jit-ldxi r0 fp (moffs asm dst))
    (jit-ldxi r1 fp (moffs asm a))
    (jit-ldxi r2 fp (moffs asm b))
    (jit-addr r0 r1 r2)
    ;; (jit-subi r0 r0 (imm 2))
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

;;; XXX: Make low level instructions to load cell object, and to compare
;;; typ16. Then rewrite this guard using thos instructions.
(define-branching-prim (%guard-fl asm (int obj))
  (let ((out (out-label asm))
        (next (jit-forward)))
    (cond
     ((gpr? obj)
      (jump (scm-imp (gpr obj)) out)
      (scm-cell-type r0 (gpr obj))
      (scm-typ16 r0 r0)
      (jump (scm-realp r0) next))
     ((memory? obj)
      (memory-ref asm r0 obj)
      (jump (scm-imp r0) out)
      (scm-cell-type r0 r0)
      (scm-typ16 r0 r0)
      (jump (scm-realp r0) next))
     (else
      (error "%guard-fl" obj)))
    (jump out)
    (jit-link next)))

(define-branching-prim (%flt asm (double a) (double b))
  (let ((out (out-label asm))
        (next (jit-forward)))
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
    (jump out)
    (jit-link next)))

(define-prim (%fadd asm (double dst) (double a) (double b))
  (cond
   ((and (fpr? dst) (constant? a) (fpr? b))
    (jit-addi-d (fpr dst) (fpr b) (constant a)))
   ((and (fpr? dst) (fpr? a) (constant? b))
    (jit-addi-d (fpr dst) (fpr a) (constant b)))
   ((and (fpr? dst) (fpr? a) (fpr? b))
    (jit-addr-d (fpr dst) (fpr a) (fpr b)))

   ((and (memory? dst) (memory? a) (constant? b))
    (jit-ldxi-d f0 fp (moffs asm a))
    (jit-addi-d f0 f0 (constant b))
    (jit-stxi-d (moffs asm dst) fp f0))
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
   (else
    (error "%fsub" dst a b))))

(define-prim (%fmul asm (double dst) (double a) (double b))
  (cond
   ((and (fpr? dst) (constant? a) (fpr? b))
    (jit-muli-d (fpr dst) (fpr b) (constant a)))
   ((and (fpr? dst) (fpr? a) (constant? b))
    (jit-muli-d (fpr dst) (fpr a) (constant b)))
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
    (debug 2 "*** %frame-ref: type mismatch ~a ~a~%" dst idx))
   ((gpr? dst)
    (local-ref (gpr dst) (ref-value idx)))
   ((memory? dst)
    (local-ref r0 (ref-value idx))
    (jit-stxi (moffs asm dst) fp r0))
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
;;; Code generation
;;;

(define (assemble-cps cps env initial-args fp-offset)
  (define (env-ref i)
    (vector-ref env i))

  (define (moffs x)
    (make-offset-pointer fp-offset (* (ref-value x) %word-size)))

  (define kstart (loop-start cps))

  (define (maybe-move exp old-args)
    (define (move dst src)
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
        (debug 2 "*** maybe-move: ~a ~a~%" dst src))))
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
                  (move dst src)))))
          old-args new-args)))))

  (define (assemble-cont cps exp loop-label k)
    ;; (debug 1 "~4,,,'0@a ~a~%" k
    ;;        (or (and (null? exp) exp)
    ;;            (and cps (unparse-cps exp))))
    (match (intmap-ref cps k)
      ;; (_ "ASSMBLING PHASE IGNORED.") ; Temporary, for debuging CPS IR.
      (($ $kreceive _ knext)
       (assemble-cont cps exp loop-label knext))

      (($ $kclause _ knext)
       (assemble-cont cps exp loop-label knext))

      (($ $kfun _ _ _ _ knext)
       (assemble-cont cps exp loop-label knext))

      (($ $kargs _ syms ($ $continue knext _ ($ $branch kt br-exp)))
       (let ((br-label (jit-forward))
             (loop-label (if (= k kstart) (jit-label) loop-label)))
         (when (= k kstart)
           (debug 3 ";;; loop:~%"))
         (when (= k kstart)
           (jit-note "loop" 0))
         (assemble-exp exp syms #f)
         (assemble-exp br-exp '() br-label)
         (assemble-cont cps #f loop-label kt)
         (jit-link br-label)
         (assemble-cont cps #f loop-label knext)))

      (($ $kargs _ syms ($ $continue knext _ next-exp))
       (cond
        ((< knext k)                    ; Jump to the loop start.
         (assemble-exp exp syms #f)
         (maybe-move next-exp initial-args)
         (debug 3 ";;; -> loop~%")
         (jump loop-label)
         #f)
        (else
         (assemble-exp exp syms #f)
         (let ((loop-label (if (= k kstart) (jit-label) loop-label)))
           (when (= k kstart)
             (jit-note "loop" 0)
             (debug 3 ";;; loop:~%"))
           (assemble-cont cps next-exp loop-label knext)))))

      (($ $ktail)
       (assemble-exp exp '() #f)
       #f)))

  (define (assemble-exp exp dst label)
    (match exp

      ;; XXX: Need to differentiate from `$values' expression at the loop dend
      ;; which jumps to loop start.

      ;; (($ $values (exit-id ip))
      ;;  (let ((a (env-ref ip)))
      ;;    (cond
      ;;     ((constant? a)
      ;;      (jit-reti (constant a)))
      ;;     ((gpr? a)
      ;;      (jit-retr (gpr a)))
      ;;     ((memory? a)
      ;;      (jit-ldxi r0 fp (moffs a))
      ;;      (jit-retr r0)))))

      (($ $primcall 'return (arg1))
       (let ((a (env-ref arg1)))
         (cond
          ((constant? a)
           (jit-reti (constant a)))
          ((register? a)
           (jit-retr (gpr a)))
          ((memory? a)
           (jit-ldxi r0 fp (moffs a))
           (jit-retr r0)))))

      (($ $primcall name args)
       (cond
        ((hashq-ref *all-prims* name)
         =>
         (lambda (proc)
           (let ((asm (make-asm env fp-offset label)))
             (apply proc asm (append dst args)))))
        (else
         (debug 2 "*** Unhandled primcall: ~a~%" name))))

      (($ $call proc args)
       (debug 2 "      exp:call ~a ~a~%" proc args))

      (_
       ;; (debug 1 "      exp:~a~%" exp)
       #f)))

  (assemble-cont cps '() #f 0))

(define (assemble-tjit locals cps)
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
       ((env initial-locals loop-start-args)
        (resolve-variables cps locals max-var)))

    ;; (let ((verbosity (lightning-verbosity)))
    ;;   (when (and verbosity (<= 3 verbosity))
    ;;     (display ";;; cps env\n")
    ;;     (let lp ((n 0) (end (vector-length env)))
    ;;       (when (< n end)
    ;;         (format #t ";;; ~3@a: ~a~%" n (vector-ref env n))
    ;;         (lp (+ n 1) end)))))

    ;; Allocate space for spilled variables, and two words for arguments passed
    ;; from C code, `vp->fp', and `registers'.
    (let* ((nspills (max-moffs env))
           (fp-offset (jit-allocai (imm (* (+ nspills 2) %word-size)))))

      ;; Get arguments.
      (jit-getarg reg-thread (jit-arg)) ; *thread
      (jit-getarg r0 (jit-arg))         ; vp->fp
      (jit-getarg r1 (jit-arg))         ; registers, for prompt

      ;; Load `vp->fp', store to vp->fp-offset.
      (jit-stxi vp->fp-offset fp r0)

      ;; Load registers for prompt, store to register-offset.
      (jit-stxi registers-offset fp r1)

      ;; Load initial locals.
      (for-each
       (match-lambda
        ((local-idx . var-idx)
         (let ((var (vector-ref env var-idx)))
           (cond
            ((gpr? var)
             (local-ref (gpr var) local-idx))
            ((memory? var)
             (let ((offset (* (ref-value var) %word-size)))
               (local-ref r0 local-idx)
               (jit-stxi (make-offset-pointer fp-offset offset) fp r0)))
            (else
             (debug 2 "Unknown initial argument: ~a~%" var))))))
       initial-locals)

      ;; Assemble the loop.
      (assemble-cps cps env loop-start-args fp-offset))))
