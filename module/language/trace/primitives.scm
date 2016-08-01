;;;; Primitive definitions for VM tjit engine

;;;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.
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
;;;
;;; Primitives for native code used in vm-tjit engine.
;;;
;;; Primitives defined here are used while compiling IR to native code. Need at
;;; least 3 general purpose scratch registers, and 3 floating point scratch
;;; registers. Currently using R0, R1, and R2 for general purpose, F0, F1, and
;;; F2 for floating point.
;;;
;;; Code:

(define-module (language trace primitives)
  #:use-module (ice-9 match)
  #:use-module (language trace assembler)
  #:use-module (language trace error)
  #:use-module (language trace parameters)
  #:use-module (language trace registers)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm program)
  #:use-module ((system vm native lightning)
                #:renamer (lambda (sym)
                            (let ((str (symbol->string sym)))
                              (if (string-prefix? "jit-" str)
                                  (string->symbol (substring str 4))
                                  sym))))
  ;; The renaming done above led to a duplicate of `link' defined (guile) module
  ;; The renamed `link' is not exported from this module, avoid showing warning
  ;; message during module compilation.
  #:duplicates (last)
  #:export (%eq
            %ne %le %lt %ge %gt
            %feq %fne %flt %fle %fgt %fge
            %eqv %nev %typeq %tceq %tcne

            %return %carg %ccall

            %band %bor

            %add %sub %mul %rsh %lsh
            %addov %subov %mulov
            %mod %rem %quo %div

            %fadd %fsub %fmul %fdiv

            %sref %sref/f %cref %cref/f %u8ref
            %cset %u8set %fill

            %cell %words

            %i2d %d2s

            %move %call/cc %callcnt))


;;;; C function addresses

(define-address-for-c-function
  (%scm-cell "scm_cell")
  (%scm-inline-cell "scm_do_inline_cell")
  (%scm-words "scm_words")
  (%scm-inline-words "scm_do_inline_words")
  (%scm-make-continuation "scm_do_make_continuation")
  (%scm-return-to-continuation "scm_do_return_to_continuation")
  (%scm-eqv "scm_eqv_p"))


;;;; Guards

(define-syntax-rule (binary-op op-ri op-rr arg1 arg2 tmp)
  (case (ref-type arg2)
    ((con) (op-ri arg1 (con arg2)))
    ((gpr) (op-rr arg1 (gpr arg2)))
    ((fpr) (op-rr arg1 (fpr->gpr tmp (fpr arg2))))
    ((mem) (op-rr arg1 (memory-ref tmp arg2)))
    (else (err))))

(define-syntax-rule (binary-op/f op-ri op-rr arg1 arg2 tmp)
  (case (ref-type arg2)
    ((con) (op-ri arg1 (ref-value arg2)))
    ((gpr) (op-rr arg1 (gpr->fpr tmp (gpr arg2))))
    ((fpr) (op-rr arg1 (fpr arg2)))
    ((mem) (op-rr arg1 (memory-ref/f tmp arg2)))
    (else (err))))

(define-syntax-rule (storage->gpr tmp src)
  (case (ref-type src)
    ((con) (movi/r tmp src))
    ((gpr) (gpr src))
    ((fpr) (fpr->gpr tmp (fpr src)))
    ((mem) (memory-ref tmp src))))

(define-syntax-rule (storage->fpr tmp src)
  (case (ref-type src)
    ((con) (movi/f tmp src))
    ((gpr) (gpr->fpr tmp (gpr src)))
    ((fpr) (fpr src))
    ((mem) (memory-ref/f tmp src))))

(define-syntax-rule (define-binary-guard-int name op-ri op-rr)
  "Macro for defining guard primitive which takes two int arguments.

`define-binary-guard-int NAME OP-RI OP-RR' will define a primitive named
NAME. Uses OP-RI when the first argument matched `gpr?'  or `memory?' predicate
while the second was constant. And, uses OP-RR when both arguments were register
or memory."
  (define-primitive (name (int a) (int b))
    (case (ref-type a)
      ((con)
       (let ((rt (ref-type b)))
         (case rt
           ((con)
            (debug 1 ";;; [assembler:~a] a=~a b=~a~%" 'name a b))
           (else
            (movi r0 (con a))
            (jump (case rt
                    ((gpr) (op-rr r0 (gpr b)))
                    ((fpr) (op-rr r0 (fpr->gpr r1 (fpr b))))
                    ((mem) (op-rr r0 (memory-ref r1 b)))
                    (else (err)))
                  (bailout))))))
      ((gpr)
       (jump (binary-op op-ri op-rr (gpr a) b r1) (bailout)))
      ((fpr)
       (jump (binary-op op-ri op-rr (fpr->gpr r0 (fpr a)) b r1) (bailout)))
      ((mem)
       (jump (binary-op op-ri op-rr (memory-ref r0 a) b r1) (bailout)))
      (else (err)))))

(define-binary-guard-int %eq bnei bner)
(define-binary-guard-int %ne beqi beqr)
(define-binary-guard-int %le bgti bgtr)
(define-binary-guard-int %lt bgei bger)
(define-binary-guard-int %ge blti bltr)
(define-binary-guard-int %gt blei bler)

(define-syntax-rule (define-binary-guard-double name op-ri op-rr)
  (define-primitive (name (double a) (double b))
    (case (ref-type a)
      ((con)
       (jump (op-rr (movi/f f0 a) (storage->fpr f1 b)) (bailout)))
      ((gpr)
       (jump (binary-op/f op-ri op-rr (gpr->fpr f0 (gpr a)) b f1) (bailout)))
      ((fpr)
       (jump (binary-op/f op-ri op-rr (fpr a) b f1) (bailout)))
      ((mem)
       (jump (binary-op/f op-ri op-rr (memory-ref/f f0 a) b f1) (bailout)))
      (else (err)))))

(define-binary-guard-double %feq bnei-d bner-d)
(define-binary-guard-double %fne beqi-d beqr-d)
(define-binary-guard-double %flt bgei-d bger-d)
(define-binary-guard-double %fle bgti-d bgtr-d)
(define-binary-guard-double %fgt blei-d bler-d)
(define-binary-guard-double %fge blti-d bltr-d)

(define-syntax-rule (define-eqv-primitive name next exit1 exit2 op-ri)
  (define-primitive (name (int a) (int b))
    (let ((volatiles (asm-volatiles asm))
          (next (forward))
          (a/r (storage->gpr r0 a))
          (b/r (storage->gpr r1 b)))
      (jump (beqr a/r b/r) exit1)
      (jump (scm-imp a/r) exit2)
      (jump (scm-imp b/r) exit2)
      (with-volatiles volatiles #f
        (prepare)
        (let ((b/r (if (equal? b (argr 1))
                       (movr/r r1 b/r)
                       b/r)))
          (pushargr a/r)
          (pushargr b/r))
        (calli %scm-eqv)
        (retval r0))
      (jump (op-ri r0 *scm-false*) (bailout))
      (link next))))

(define-eqv-primitive %eqv proceed proceed (bailout) beqi)
(define-eqv-primitive %nev proceed (bailout) proceed bnei)

;;; Type guard.
(define-primitive (%typeq (int src) (void type))
  (let ((rt (ref-type src)))
    (unless (eq? 'con rt)
      (let ((reg (case rt
                   ((gpr) (gpr src))
                   ((fpr) (fpr->gpr r0 (fpr src)))
                   ((mem) (memory-ref r0 src)))))
        (guard-type reg (ref-value type))))))

;;; TC tag equal.
(define-primitive (%tceq (int src) (void mask) (void tag))
  (let ((typx r0)
        (obj (storage->gpr r1 src)))
    (jump (scm-imp obj) (bailout))
    (ldr typx obj)
    (andi typx typx (con mask))
    (jump (bnei typx (con tag)) (bailout))))

;;; TC tag not equal.
(define-primitive (%tcne (int src) (void mask) (void tag))
  (let ((typx r0)
        (obj (storage->gpr r1 src))
        (proceed (forward)))
    (jump (scm-imp obj) proceed)
    (ldr typx obj)
    (andi typx typx (con mask))
    (jump (beqi typx (con tag)) (bailout))
    (link proceed)))


;;;; Call and return

;;; Return from Scheme procedure call. Shift current FP to the one from dynamic
;;; link. Guard with return address, checks whether it match with the IP used at
;;; the time of compilation.
(define-primitive (%return (void dl) (void ra))
  (let ((vp r0)
        (vp->fp r1)
        (tmp r2))
    (load-vp vp)
    (load-vp->fp vp->fp vp)
    (scm-frame-return-address tmp vp->fp)
    (jump (bnei tmp (con ra)) (bailout))
    (addi vp->fp vp->fp (imm (* (ref-value dl) %word-size)))
    (store-vp->fp vp vp->fp)))

;;; Prepare argument for calling C function.
(define-primitive (%carg (int arg))
  (set-asm-cargs! asm (cons arg (asm-cargs asm))))

;;; C function call. Appears when Scheme primitive procedure defined as `gsubr'
;;; in C were called.
(define-primitive (%ccall (int dst) (void cfunc))
  (let ((volatiles (asm-volatiles asm))
        (subrf
         (lambda (subr-addr)
           (let ((subr (pointer->scm (make-pointer subr-addr))))
             (let ((var0 (program-free-variable-ref subr 0)))
               (if (pointer? var0)
                   (pointer-address var0)
                   (failure '%ccall "not a primitive ~s" subr)))))))
    (with-volatiles volatiles dst
      (prepare)
      (let lp ((cargs (asm-cargs asm)) (i 1) (pushed '()))
        (match cargs
          ((carg . cargs)
           (let ((overwritten? (and (member carg volatiles)
                                    (member carg pushed))))
             (push-as-gpr carg overwritten?)
             (lp cargs (+ i 1) (cons (argr i) pushed))))
          (() (values))))
      (calli (subrf (ref-value cfunc)))
      (retval-to-reg-or-mem dst))
    (load-vp r0)
    (vm-cache-sp r0)
    (set-asm-cargs! asm '())))


;;;; Bitwise arithmetic

(define-syntax-rule (define-binary-arith-int name op-ri op-rr)
  (define-primitive (name (int dst) (int a) (int b))
    (letrec-syntax
        ((op3b (syntax-rules ()
                 ((_ dst a)
                  (case (ref-type b)
                    ((con) (op-ri dst a (con b)))
                    ((gpr) (op-rr dst a (gpr b)))
                    ((fpr) (op-rr dst a (fpr->gpr r1 (fpr b))))
                    ((mem) (op-rr dst a (memory-ref r1 b)))
                    (else (err))))))
         (op3a (syntax-rules ()
                 ((_ dst)
                  (let ((a/r (storage->gpr r0 a)))
                    (op3b dst a/r)
                    dst)))))
      (case (ref-type dst)
        ((gpr) (op3a (gpr dst)))
        ((fpr) (gpr->fpr (fpr dst) (op3a r0)))
        ((mem) (memory-set! dst (op3a r0)))
        (else (err))))))

(define-binary-arith-int %band andi andr)
(define-binary-arith-int %bor ori orr)


;;;; Integer arithmetic

(define-binary-arith-int %add addi addr)
(define-binary-arith-int %sub subi subr)
(define-binary-arith-int %mul muli mulr)
(define-binary-arith-int %rsh rshi rshr)
(define-binary-arith-int %lsh lshi lshr)

(define-syntax-rule (define-binary-arith-int-overflow name op-rr op-ri)
  (define-primitive (name (int dst) (int a) (int b))
    (let ((tmp r2))
      (case (ref-type a)
        ((con) (movi/r tmp a))
        ((gpr) (movr tmp (gpr a)))
        ((fpr) (fpr->gpr tmp (fpr a)))
        ((mem) (memory-ref tmp a))
        (else (err)))
      (jump (binary-op op-ri op-rr tmp b r0) (bailout))
      (case (ref-type dst)
        ((gpr) (movr (gpr dst) tmp))
        ((fpr) (gpr->fpr (fpr dst) tmp))
        ((mem) (memory-set! dst tmp))
        (else (err))))))

(define-binary-arith-int-overflow %addov boaddr boaddi)
(define-binary-arith-int-overflow %subov bosubr bosubi)

(define-primitive (%mod (int dst) (int a) (int b))
  (let ((neg-b (forward))
        (proceed (forward))
        (tmp/r r0)
        (a/r (storage->gpr r0 a))
        (b/r (storage->gpr r1 b)))
    (remr tmp/r a/r b/r)
    ;; Adjust with checking sign of divisor and result.
    (jump (blti b/r (imm 0)) neg-b)
    (jump (bgei tmp/r (imm 0)) proceed)
    (addr tmp/r tmp/r b/r)
    (jump (jmpi) proceed)
    (link neg-b)
    (jump (blei tmp/r (imm 0)) proceed)
    (addr tmp/r tmp/r b/r)
    (link proceed)
    (case (ref-type dst)
      ((gpr) (movr (gpr dst) tmp/r))
      ((fpr) (gpr->fpr (fpr dst) tmp/r))
      ((mem) (memory-set! dst tmp/r))
      (else (err)))))

(define-binary-arith-int %rem remi remr)
(define-binary-arith-int %quo divi divr)

;; Integer multiplication, with overflow check.
(define-primitive (%mulov (int dst) (int a) (int b))
  (let ((tmp r1)
        (proceed (forward)))
    (letrec-syntax
        ((op3b (syntax-rules ()
                 ((_ dst a)
                  (begin
                    (case (ref-type b)
                      ((con) (qmuli dst tmp a (con b)))
                      ((gpr) (qmulr dst tmp a (gpr b)))
                      ((fpr) (qmulr dst tmp a (fpr->gpr r1 (fpr b))))
                      ((mem) (qmulr dst tmp a (memory-ref r1 b)))
                      (else (err)))))))
         (op3a (syntax-rules ()
                 ((_ dst)
                  (let ((a/r (storage->gpr r0 a)))
                    (op3b dst a/r)
                    dst)))))
      (case (ref-type dst)
        ((gpr) (op3a (gpr dst)))
        ((fpr) (gpr->fpr (fpr dst) (op3a r0)))
        ((mem) (memory-set! dst (op3a r0)))
        (else (err)))
      (jump (beqi tmp (imm -1)) proceed)
      (jump (bnei tmp (imm 0)) (bailout))
      (link proceed))))

;; Integer division, returns SCM value with fraction type when remainder is not
;; zero.
(define-primitive (%div (int dst) (int a) (int b))
  (let* ((tmp1-offset (moffs (make-memory -4)))
         (tmp2-offset (moffs (make-memory -5)))
         (make-fixnum (forward))
         (proceed (forward))
         (a/r (case (ref-type a)
                ((con) (movi/r r0 a))
                ((gpr) (movr/r r0 (gpr a)))
                ((fpr) (fpr->gpr r0 (fpr a)))
                ((mem) (memory-ref r0 a))
                (else (err))))
         (b/r (case (ref-type b)
                ((con) (movi/r r1 b))
                ((gpr) (movr/r r1 (gpr b)))
                ((fpr) (fpr->gpr r1 (fpr b)))
                ((mem) (memory-ref r1 b))
                (else (err))))
         (dst/r (case (ref-type dst)
                  ((gpr) (gpr dst))
                  (else r2)))
         (rem/r (if (equal? dst/r a/r)
                    b/r
                    a/r)))

    ;; Call div instruction, jump to proceed if remainder is 0.
    (stxi tmp1-offset %fp a/r)
    (stxi tmp2-offset %fp b/r)
    (qdivr dst/r rem/r a/r b/r)
    (jump (beqi rem/r (imm 0)) make-fixnum)

    ;; Allocate memory for SCM fraction value. Push %tc16-fraction as cell
    ;; header, fill in numerator, denominator and 0 to rest of the allocated
    ;; memory.
    (prepare)
    (let ((volatiles (asm-volatiles asm)))
      (with-volatiles volatiles dst
        (when (asm-gc-inline? asm)
          (pushargr %thread))
        (pushargi (imm %tc16-fraction))
        (pushargi (imm 4))
        (if (asm-gc-inline? asm)
            (calli %scm-inline-words)
            (calli %scm-words))
        (retval dst/r))
      (ldxi a/r %fp tmp1-offset)
      (lshi a/r a/r (imm 2))
      (addi a/r a/r (imm 2))
      (stxi (imm %word-size) dst/r a/r)
      (ldxi b/r %fp tmp2-offset)
      (lshi b/r b/r (imm 2))
      (addi b/r b/r (imm 2))
      (stxi (imm (* 2 %word-size)) dst/r b/r)
      (stxi (imm (* 3 %word-size)) dst/r (movi/r b/r 0))
      (jump proceed)

      ;; Adjust fixnum.
      (link make-fixnum)
      (lshi dst/r dst/r (imm 2))
      (addi dst/r dst/r (imm 2))

      ;; Move result to dst.
      (link proceed)
      (case (ref-type dst)
        ((gpr) (values))
        ((fpr) (gpr->fpr (fpr dst) dst/r))
        ((mem) (memory-set! dst dst/r))
        (else (err))))))


;;;; Floating point arithmetic

(define-syntax-rule (define-binary-arith-double name op-ri op-rr)
  (define-primitive (name (double dst) (double a) (double b))
    (let-syntax
        ((op3a (syntax-rules ()
                 ((_ dst)
                  (let ((a/r (storage->fpr f0 a))
                        (b/r (storage->fpr f1 b)))
                    (op-rr dst a/r b/r)
                    dst)))))
      (case (ref-type dst)
        ((gpr) (fpr->gpr (gpr dst) (op3a f2)))
        ((fpr) (op3a (fpr dst)))
        ((mem) (memory-set!/f dst (op3a f2)))
        (else (err))))))

(define-binary-arith-double %fadd addi-d addr-d)
(define-binary-arith-double %fsub subi-d subr-d)
(define-binary-arith-double %fmul muli-d mulr-d)
(define-binary-arith-double %fdiv divi-d divr-d)


;;;; Load and store

;;; XXX: Not sure whether it's better to couple `xxx-ref' and `xxx-set!'
;;; instructions with expected type as done in bytecode, to have vector-ref,
;;; struct-ref, box-ref, string-ref, fluid-ref, bv-u8-ref ... etc or not. When
;;; instructions specify its operand type, size of ANF will be more compact, but
;;; may loose chances to optimize away type checking instructions.

;; Type check local N with TYPE and load to gpr or memory DST.
(define-primitive (%sref (int dst) (void n) (void type))
  (let ((tref (ref-value type)))
    (when (or (not (con? n))
              (not (con? type))
              (con? dst))
      (err))
    (if (eq? &flonum tref)
        (begin
          (sp-ref r0 (ref-value n))
          (unbox-stack-element dst r0 tref))
        (case (ref-type dst)
          ((gpr)
           (sp-ref (gpr dst) (ref-value n)))
          ((fpr)
           (sp-ref r0 (ref-value n))
           (gpr->fpr (fpr dst) r0))
          ((mem)
           (sp-ref r0 (ref-value n))
           (memory-set! dst r0))
          (else (err))))))

;; Load frame local to fpr or memory, with type check. This primitive
;; is used for loading floating point number to FPR.
(define-primitive (%sref/f (double dst) (void n) (void type))
  (let ((t (ref-value type)))
    (when (or (not (con? n))
              (not (con? type))
              (con? dst))
      (err))
    (cond
     ((= t &flonum)
      (sp-ref r0 (ref-value n))
      (guard-tc16 r0 %tc16-real)
      (case (ref-type dst)
        ((fpr)
         (scm-real-value (fpr dst) r0))
        ((gpr)
         (scm-real-value f0 r0)
         (fpr->gpr (gpr dst) f0))
        ((mem)
         (scm-real-value f0 r0)
         (memory-set!/f dst f0))
        (else (err))))
     ((= t &f64)
      (case (ref-type dst)
        ((fpr)
         (sp-ref/f (fpr dst) (ref-value n)))
        ((gpr)
         (sp-ref/f f0 (ref-value n))
         (fpr->gpr (gpr dst) f0))
        ((mem)
         (sp-ref/f f0 (ref-value n))
         (memory-set!/f dst f0))
        (else (err))))
     (else (err)))))

;; Cell object reference.
(define-primitive (%cref (int dst) (int src) (int n))
  (letrec-syntax
      ((op3const
        (syntax-rules ()
          ((_ dst)
           (let ((nw (* (ref-value n) %word-size)))
             (case (ref-type src)
               ((con) (ldi dst (imm (+ (ref-value src) nw))))
               ((gpr) (ldxi dst (gpr src) (imm nw)))
               ((fpr) (ldxi dst (fpr->gpr r0 (fpr src)) (imm nw)))
               ((mem) (ldxi dst (memory-ref r0 src) (imm nw)))
               (else (err)))
             dst))))
       (op3reg
        (syntax-rules ()
          ((_ dst)
           (begin
             (case (ref-type src)
               ((con) (ldxi dst r0 (imm (ref-value src))))
               ((gpr) (ldxr dst (gpr src) r0))
               ((fpr) (ldxr dst (fpr->gpr r1 (fpr src)) r0))
               ((mem) (ldxr dst (memory-ref r1 src) r0))
               (else (err)))
             dst))))
       (op3dst
        (syntax-rules ()
          ((_ n/r)
           (begin
             (lshi r0 n/r (imm %word-size-in-bits))
             (case (ref-type dst)
               ((gpr) (op3reg (gpr dst)))
               ((fpr) (gpr->fpr (fpr dst) (op3reg r1)))
               ((mem) (memory-set! dst (op3reg r1)))
               (else (err))))))))
    (case (ref-type n)
      ((con)
       (case (ref-type dst)
         ((gpr) (op3const (gpr dst)))
         ((fpr) (gpr->fpr (fpr dst) (op3const r0)))
         ((mem) (memory-set! dst (op3const r0)))
         (else (err))))
      ((gpr) (op3dst (gpr n)))
      ((mem) (op3dst (memory-ref r0 n)))
      (else (err)))))

;; Cell reference to floating point register.
(define-primitive (%cref/f (double dst) (int src) (void n))
  (let-syntax
      ((op3
        (syntax-rules ()
          ((_ dst*)
           (let ((nw (constant-word n))
                 (dst dst*))
             (case (ref-type src)
               ((con) (ldi-d dst (imm (+ (object-address (ref-value src))
                                         (* (ref-value n) %word-size)))))
               ((gpr) (ldxi-d dst (gpr src) nw))
               ((fpr) (ldxi-d dst (fpr->gpr r0 (fpr src)) nw))
               ((mem) (ldxi-d dst (memory-ref r0 src) nw))
               (else (err)))
             dst)))))
    (case (ref-type dst)
      ((gpr) (fpr->gpr (gpr dst) (op3 f0)))
      ((fpr) (op3 (fpr dst)))
      ((mem) (memory-set!/f dst (op3 f0)))
      (else (err)))))

;; Unsigned char ref.
(define-primitive (%u8ref (int dst) (int src) (int n))
  (let ((dst/r (storage->gpr r1 dst))
        (src/r (storage->gpr r2 src)))
    (case (ref-type n)
      ((con) (ldxi-uc dst/r src/r (con n)))
      ((gpr) (ldxr-uc dst/r src/r (gpr n)))
      ((fpr) (ldxr-uc dst/r src/r (fpr->gpr r0 (fpr n))))
      ((mem) (ldxr-uc dst/r src/r (memory-ref r0 n))))
    (case (ref-type dst)
      ((gpr) (values))
      ((fpr) (gpr->fpr (fpr dst) dst/r))
      ((mem) (memory-set! dst dst/r))
      (else (err)))))

;; Set cell object. This operation uses `r2' register when the argument `cell'
;; was memory and argument `n' was not constant.
(define-primitive (%cset (int cell) (int n) (int src))
  (letrec-syntax
      ((op3a (syntax-rules ()
               ((_ dst)
                (let ((nw (* (ref-value n) %word-size)))
                  (stxi (imm nw) dst (storage->gpr r1 src))))))
       (op3b (syntax-rules ()
               ((_ dst)
                (stxr r0 dst (storage->gpr r1 src)))))
       (op3c (syntax-rules ()
               ((_ dst-addr)
                (sti (imm dst-addr) (storage->gpr r1 src)))))
       (op3d (syntax-rules ()
               ((_ n/r)
                (begin
                  (lshi r0 n/r (imm %word-size-in-bits))
                  (case (ref-type cell)
                    ((gpr) (op3b (gpr cell)))
                    ((fpr) (op3b (fpr->gpr r2 (fpr cell))))
                    ((mem) (op3b (memory-ref r2 cell)))
                    (else (err))))))))
    (case (ref-type n)
      ((con)
       (case (ref-type cell)
         ((con) (op3c (+ (* (ref-value n) %word-size) (ref-value cell))))
         ((gpr) (op3a (gpr cell)))
         ((fpr) (op3a (fpr->gpr r0 (fpr cell))))
         ((mem) (op3a (memory-ref r0 cell)))
         (else (err))))
      ((gpr) (op3d (gpr n)))
      ((mem) (op3d (memory-ref r0 n)))
      (else (err)))))

;; Unsigned char set.
(define-primitive (%u8set (int dst) (int n) (int src))
  (let ((dst/r (storage->gpr r2 dst))
        (src/r (storage->gpr r1 src))
        (n/t (ref-type n)))
    (case n/t
      ((con) (stxi-c (con n) dst/r src/r))
      ((gpr) (stxr-c (gpr n) dst/r src/r))
      ((fpr) (stxr-c (fpr->gpr r0 (fpr n)) dst/r src/r))
      ((mem) (stxr-c (memory-ref r0 n) dst/r src/r))
      (else (err)))))

;; Fill memory from DST for N words with SRC. The address of DST itself is not
;; filled, use %cset for such case.
;;
;; XXX: Cannot use `r2' for `n' and 'src', since r2 is hard coded to `dst'.
;;
(define-primitive (%fill (int dst) (int n) (int src))
  (let ((l0 (forward))
        (tmp r0)
        (dst/r (storage->gpr r2 dst))
        (n/r (storage->gpr r0 n))
        (src/r (storage->gpr r1 src)))
    (lshi tmp n/r (imm %word-size-in-bits))
    (link l0)
    (stxr tmp dst/r src/r)
    (subi tmp tmp (imm %word-size))
    (jump (bgti tmp (imm 0)) l0)
    (case (ref-type dst)
      ((gpr) (values))
      ((fpr) (gpr->fpr (fpr dst) dst/r))
      ((mem) (memory-set! dst dst/r))
      (else (err)))))


;;;; Allocation

(define-syntax-rule (define-alloc-primitive name inline-fn noinline-fn)
  (define-primitive (name (int dst) (int x) (int y))
    (let ((volatiles (asm-volatiles asm))
          (x-overwritten? (equal? x (argr 1)))
          (y-overwritten? (or (equal? y (argr 1))
                              (equal? y (argr 2)))))
      (with-volatiles volatiles dst
        (prepare)
        (when (asm-gc-inline? asm)
          (pushargr %thread))
        (push-as-gpr x x-overwritten?)
        (push-as-gpr y y-overwritten?)
        (if (asm-gc-inline? asm)
            (calli inline-fn)
            (calli noinline-fn))
        (retval-to-reg-or-mem dst)))))

;; Call C function `scm_do_inline_cell'. Save volatile registers before calling,
;; restore after getting returned value.
(define-alloc-primitive %cell %scm-inline-cell %scm-cell)

;; Allocate words for arg2 bytes, store to dst, fill head with arg1.
(define-alloc-primitive %words %scm-inline-words %scm-words)


;;;; Type conversion

;; Integer -> floating point
(define-primitive (%i2d (double dst) (int src))
  (let-syntax ((src->fpr
                (syntax-rules ()
                  ((_ dst*)
                   (let ((dst dst*))
                     (case (ref-type src)
                       ((con) (movi-d dst (exact->inexact (ref-value src))))
                       ((gpr) (extr-d dst (gpr src)))
                       ((fpr) (extr-d dst (fpr->gpr r0 (fpr src))))
                       ((mem) (extr-d dst (memory-ref r0 src)))
                       (else (err)))
                     dst)))))
    (case (ref-type dst)
      ((gpr) (fpr->gpr (gpr dst) (src->fpr f0)))
      ((fpr) (src->fpr (fpr dst)))
      ((mem) (memory-set!/f dst (src->fpr f0)))
      (else (err)))))

;; Floating point -> SCM
(define-primitive (%d2s (int dst) (double src))
  (let-syntax
      ((op3 (syntax-rules ()
              ((_ dst)
               (begin
                 (case (ref-type src)
                   ((con)
                    (movi-d f0 (con src))
                    (scm-from-double dst f0))
                   ((gpr)
                    (gpr->fpr f0 (gpr src))
                    (scm-from-double dst f0))
                   ((fpr)
                    (scm-from-double dst (fpr src)))
                   ((mem)
                    (memory-ref/f f0 src)
                    (scm-from-double dst f0))
                   (else (err)))
                 dst)))))
    (case (ref-type dst)
      ((gpr) (op3 (gpr dst)))
      ((fpr) (gpr->fpr (fpr dst) (op3 r0)))
      ((mem) (memory-set! dst (op3 r0)))
      (else (err)))))


;;;; Miscellaneous

(define-primitive (%move (int dst) (int src))
  (move dst src))

(define-primitive (%call/cc (int dst) (void id))
  (let ((volatiles (asm-volatiles asm)))
    ;; While making continuation, stack contents will get copied to continuation
    ;; data. Store register contents to stack before calling
    ;; `scm-make-continuation', to use updated stack element.
    (match (snapshots-ref (asm-snapshots asm) (ref-value id))
      (($ $snapshot _ sp-offset fp-offset nlocals locals vars _ _ dls)
       (let lp ((locals locals) (vars vars))
         (match (cons locals vars)
           ((((n . t) . locals) . (v . vars))
            (store-stack n t v)
            (lp locals vars))
           (_
            ;; Store dynamic links after storing locals.
            (store-dynamic-links dls)))))
      (_
       (failure '%call/cc "not a snapshot")))
    ;; Make continuation data via C function.
    (with-volatiles volatiles dst
      (prepare)
      (pushargr %thread)
      (load-vp r0)
      (pushargr r0)
      (calli %scm-make-continuation)
      (retval-to-reg-or-mem dst))
    ;; Bailout when returned continuation was #<undefined>. Otherwise, continue
    ;; with next instruction.
    (let ((dst/r (case (ref-type dst)
                   ((gpr) (gpr dst))
                   ((fpr) (fpr->gpr r0 (fpr dst)))
                   ((mem) (memory-ref r0 dst)))))
      (jump (beqi dst/r *scm-undefined*) (bailout)))))

;; XXX: Probably x86-64 only, move to (@ language trace compat).
(define-syntax-rule (scm-continuation-next-ip dst src)
  (begin
    (ldxi dst src (imm %word-size))
    (ldxi dst dst (imm (* %word-size 3)))
    (ldxi dst dst (imm %word-size))
    (ldxi dst dst (imm %word-size))))

;; Primitive for `continuation-call'. The SCM continuation value is stored in
;; r2.
(define-primitive (%callcnt (int local0) (void ncount) (void id) (void ip))
  (let ((vp r0)
        (cont r2))
    (scm-continuation-next-ip r1 cont)
    (jump (bnei r1 (con ip)) (bailout))

    ;; Storing stack elements from snapshot.
    (match (snapshots-ref (asm-snapshots asm) (ref-value id))
      (($ $snapshot _ sp-offset fp-offset nlocals locals vars)
       (let lp ((locals locals) (vars vars))
         (match (cons locals vars)
           ((((n . t) . locals) . (v . vars))
            (store-stack n t v)
            (lp locals vars))
           (_
            ;; Sync SP.
            (load-vp vp)
            (cond
             ((= 0 sp-offset) (values))
             ((< 0 sp-offset) (addi %sp %sp (imm (* sp-offset %word-size))))
             (else (subi %sp %sp (imm (* (- sp-offset) %word-size)))))
            (vm-sync-sp %sp vp)

            ;; Call the C function doing longjmp.
            (prepare)
            (pushargr cont)
            (pushargi (imm (ref-value ncount)))
            (pushargr %sp)
            (calli %scm-return-to-continuation)

            ;; Following code is used when scm-return-to-continuation didn't use
            ;; SCM_I_LONGJMP. Currently scm-return-to-continuation does use
            ;; SCM_I_LONGJMP, which jumps back to the place where `call/cc' were
            ;; called, could jump back to inlined native code, or VM interpreter.

            ;; ;; Restore local 0 from %sp
            ;; (case (ref-type local0)
            ;;   ((gpr)
            ;;    (ldr (gpr local0) %sp))
            ;;   ((fpr)
            ;;    (ldr r0 %sp)
            ;;    (gpr->fpr (fpr local0) r0))
            ;;   ((mem)
            ;;    (ldr r0 %sp)
            ;;    (memory-set! local0 r0))
            ;;   (else (err)))

            ;; ;; Shifting %sp back.
            ;; (load-vp vp)
            ;; (vm-cache-sp vp)
            ;; (cond
            ;;  ((= 0 sp-offset)
            ;;   (values))
            ;;  ((< 0 sp-offset)
            ;;   (subi %sp %sp (imm (* sp-offset %word-size))))
            ;;  (else
            ;;   (addi %sp %sp (imm (* (- sp-offset) %word-size)))))
            ))))
      (_
       (failure '%cont "not a snapshot")))))
