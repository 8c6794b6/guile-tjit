;;;; Assembler for VM tjit engine

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
;;;
;;; Primitives for native code used in vm-tjit engine.
;;;
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
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
  #:export (*native-prim-procedures*
            *native-prim-types*
            *scm-false*
            *scm-true*
            *scm-undefined*
            *scm-unspecified*
            *scm-null*
            make-asm
            asm-fp-offset
            asm-out-code
            set-asm-out-code!
            set-asm-exit!
            asm-end-address
            asm-volatiles
            make-negative-pointer
            make-signed-pointer
            spilled-offset
            constant-word
            move
            jump
            jumpi
            sp-ref
            sp-set!
            sp-ref/f
            sp-set!/f
            memory-ref
            memory-set!
            memory-ref/f
            memory-set!/f
            scm-from-double
            scm-frame-dynamic-link
            scm-frame-set-dynamic-link!
            scm-frame-return-address
            scm-frame-set-return-address!
            scm-real-value
            registers-offset
            load-vp
            store-vp
            load-vp->fp
            store-vp->fp
            vm-cache-sp
            vm-sync-ip
            vm-sync-sp
            vm-sync-fp
            vm-handle-interrupts
            vm-expand-stack
            unbox-stack-element))

(define (make-negative-pointer addr)
  "Make negative pointer with ADDR."
  (when (< 0 addr)
    (tjitc-error 'make-negative-pointer "non-negative address ~s" addr))
  (make-pointer (+ (expt 2 (* 8 %word-size)) addr)))

(define (make-signed-pointer addr)
  (if (<= 0 addr)
      (make-pointer addr)
      (make-negative-pointer addr)))

(define-syntax-rule (constant-word i)
  (imm (* (ref-value i) %word-size)))

(define %word-size-in-bits
  (inexact->exact (/ (log %word-size) (log 2))))

(define-syntax-rule (fpr->gpr dst src)
  (let ((tmp-offset (volatile-offset `(gpr . 8))))
    (jit-stxi-d tmp-offset %fp src)
    (jit-ldxi dst %fp tmp-offset)))

(define-syntax-rule (gpr->fpr dst src)
  (let ((tmp-offset (volatile-offset `(gpr . 8))))
    (jit-stxi tmp-offset %fp src)
    (jit-ldxi-d dst %fp tmp-offset)))


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

(define-syntax-parameter asm
  (lambda (x)
    (syntax-violation 'asm "asm used outside of primitive definition" x)))

(define-syntax define-native
  (syntax-rules ()
    ((_ (name (ty arg) ...) <body>)
     (begin
       (define (name asm-in-arg arg ...)
         (let ((verbosity (lightning-verbosity)))
           (when (and verbosity (<= 5 verbosity))
             (jit-note (format #f "~a" `(name ,arg ...)) 0))
           (debug 4 ";;; (~12a ~{~a~^ ~})~%" 'name `(,arg ...)))
         (syntax-parameterize ((asm (identifier-syntax asm-in-arg)))
           <body>))
       (hashq-set! *native-prim-procedures* 'name name)
       (hashq-set! *native-prim-arities* 'name (arity-of-args '(arg ...)))
       (hashq-set! *native-prim-types* 'name `(,ty ...))))))


;;;
;;; Scheme constants
;;;

(define *scm-false*
  (scm->pointer #f))

(define *scm-true*
  (scm->pointer #t))

(define *scm-unspecified*
  (scm->pointer *unspecified*))

(define *scm-undefined*
  (make-pointer #x904))

(define *scm-null*
  (scm->pointer '()))


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
    (jit-pushargr %thread)
    (jit-pushargr-d src)
    (jit-calli %scm-from-double)
    (jit-retval dst)))

(define %scm-inline-cons
  (dynamic-pointer "scm_do_inline_cons" (dynamic-link)))

(define %scm-async-tick
  (dynamic-pointer "scm_async_tick" (dynamic-link)))

(define %scm-vm-expand-stack
  (dynamic-pointer "scm_do_vm_expand_stack" (dynamic-link)))

(define-syntax-rule (scm-frame-return-address dst vp->fp)
  (jit-ldr dst vp->fp))

(define-syntax-rule (scm-frame-set-return-address! vp->fp src)
  (jit-str vp->fp src))

(define-syntax-rule (scm-frame-dynamic-link dst vp->fp)
  (jit-ldxi dst vp->fp (imm %word-size)))

(define-syntax-rule (scm-frame-set-dynamic-link! vp->fp src)
  (jit-stxi (imm %word-size) vp->fp src))

(define-syntax-rule (moffs mem)
  (let ((n (+ (ref-value mem) *num-volatiles*)))
    (make-negative-pointer (* (- -2 n) %word-size))))

(define registers-offset
  (make-negative-pointer (* -1 %word-size)))

(define (volatile-offset reg)
  (let ((n (- (ref-value reg) *num-non-volatiles*)))
    (make-negative-pointer (* (- -2 n) %word-size))))

(define (spilled-offset mem)
  (moffs mem))

(define-syntax-rule (load-vp dst)
  (jit-ldr dst %fp))

(define-syntax-rule (store-vp src)
  (jit-str %fp src))

(define-syntax-rule (load-vp->fp dst vp)
  (jit-ldxi dst vp (imm (* 2 %word-size))))

(define-syntax-rule (store-vp->fp vp src)
  (jit-stxi (imm (* 2 %word-size)) vp src))

(define-syntax-rule (vm-cache-sp vp)
  (jit-ldxi %sp vp (make-pointer %word-size)))

(define-syntax-rule (vm-sync-ip src)
  (let ((vp (if (eq? src r0) r1 r0)))
    (load-vp vp)
    (jit-str vp src)))

(define-syntax-rule (vm-sync-sp src)
  (let ((vp (if (eq? src r0) r1 r0)))
    (load-vp vp)
    (jit-stxi (imm %word-size) vp src)))

(define-syntax-rule (vm-sync-fp src)
  (let ((vp (if (eq? src r0) r1 r0)))
    (load-vp vp)
    (store-vp->fp vp src)))

(define-syntax-rule (sp-ref dst n)
  (if (= 0 n)
      (jit-ldr dst %sp)
      (jit-ldxi dst %sp (make-signed-pointer (* n %word-size)))))

(define-syntax-rule (sp-set! n src)
  (if (= 0 n)
      (jit-str %sp src)
      (jit-stxi (make-signed-pointer (* n %word-size)) %sp src)))

(define-syntax-rule (sp-ref/f dst n)
  (if (= 0 n)
      (jit-ldr-d dst %sp)
      (jit-ldxi-d dst %sp (make-signed-pointer (* n %word-size)))))

(define-syntax-rule (sp-set!/f n src)
  (if (= 0 n)
      (jit-str-d %sp src)
      (jit-stxi-d (make-signed-pointer (* n %word-size)) %sp src)))


(define (store-volatile src)
  (jit-stxi (volatile-offset src) %fp (gpr src)))

(define (load-volatile dst)
  (jit-ldxi (gpr dst) %fp (volatile-offset dst)))

(define-syntax-rule (push-gpr-or-mem arg overwritten?)
  (cond
   (overwritten?
    (jit-ldxi r0 %fp (volatile-offset arg))
    (jit-pushargr r0))
   ((gpr? arg)
    (jit-pushargr (gpr arg)))
   ((memory? arg)
    (memory-ref r0 arg)
    (jit-pushargr r0))
   (else
    (tjitc-error 'push-gpr-or-mem "unknown arg ~s" arg))))

(define-syntax-rule (retval-to-gpr-or-mem dst)
  (cond
   ((gpr? dst)
    (jit-retval (gpr dst)))
   ((memory? dst)
    (jit-retval r0)
    (memory-set! dst r0))
   (else
    (tjitc-error 'retval-to-gpr-or-mem "unknown dst ~s" dst))))

;;; XXX: Offsets for fields in C structs where manually taken with
;;; observing output from `objdump'. It would be nice if the offsets
;;; were derived in programmatic manner. Have not tested on
;;; architecture other than Linux x86-64.

(define-syntax-rule (vm-thread-pending-asyncs dst)
  (jit-ldxi dst %thread (imm #x104)))

(define-syntax-rule (vm-sp-min-since-gc dst vp)
  (jit-ldxi dst vp (imm #x28)))

(define-syntax-rule (vm-set-sp-min-since-gc! vp src)
  (jit-stxi (imm #x28) vp src))

(define-syntax-rule (vm-sp-stack-limit dst vp)
  (jit-ldxi dst vp (imm #x18)))

(define-syntax-rule (vm-sp-stack-size dst vp)
  (jit-ldxi dst vp (imm #x30)))

(define-syntax-rule (vm-handle-interrupts asm-arg)
  (let ((next (jit-forward))
        (volatiles (asm-volatiles asm-arg)))
    (vm-thread-pending-asyncs r0)
    (jump (jit-bmci r0 (imm 1)) next)
    (for-each store-volatile volatiles)
    (jit-prepare)
    (jit-pushargr r0)
    (jit-calli %scm-async-tick)
    (load-vp r0)
    (vm-cache-sp r0)
    (for-each load-volatile volatiles)
    (jit-link next)))

(define-syntax-rule (vm-expand-stack asm-arg size)
  (let ((volatiles (asm-volatiles asm-arg))
        (vp r0)
        (tmp r1)
        (set-min-since-gc (jit-forward))
        (sync-sp (jit-forward))
        (next (jit-forward)))

    (jit-subi %sp %sp (imm (* (abs size) %word-size)))
    (load-vp vp)
    (vm-sp-min-since-gc tmp vp)
    (jump (jit-bger %sp tmp) sync-sp)
    (vm-sp-stack-limit tmp vp)
    (jump (jit-bger %sp tmp) set-min-since-gc)

    (for-each store-volatile volatiles)
    (jit-prepare)
    (jit-pushargr vp)
    (jit-pushargr %sp)
    (jit-calli %scm-vm-expand-stack)
    (for-each load-volatile volatiles)
    (load-vp vp)
    (vm-cache-sp vp)
    (jump next)

    (jit-link set-min-since-gc)
    (vm-set-sp-min-since-gc! vp %sp)

    (jit-link sync-sp)
    (vm-sync-sp %sp)

    (jit-link next)))


;;;
;;; Predicates
;;;

(define-syntax-rule (scm-imp obj)
  (jit-bmsi obj (imm 6)))

(define-syntax-rule (scm-inump obj)
  (jit-bmsi obj (imm (@@ (system base types) %tc2-int))))

(define-syntax-rule (scm-not-inump obj)
  (jit-bmci obj (imm (@@ (system base types) %tc2-int))))

(define-syntax-rule (scm-realp tag)
  (jit-beqi tag (imm (@@ (system base types) %tc16-real))))

(define-syntax-rule (scm-not-realp tag)
  (jit-bnei tag (imm (@@ (system base types) %tc16-real))))


;;;
;;; Assembler state
;;;

(define-record-type <asm>
  (%make-asm volatiles exit out-code end-address cargs)
  asm?

  ;; Volatile registers in use.
  (volatiles asm-volatiles)

  ;; Current exit to jump.
  (exit asm-exit set-asm-exit!)

  ;; Pointer of native code for current side exit.
  (out-code asm-out-code set-asm-out-code!)

  ;; Pointer of native code at the end of parent trace.
  (end-address asm-end-address)

  ;; Arguments for function call.
  (cargs asm-cargs set-asm-cargs!))

(define (make-asm env end-address)
  (define (volatile-regs-in-use env)
    (hash-fold (lambda (_ reg acc)
                 (if (and (gpr? reg)
                          (<= *num-non-volatiles* (ref-value reg)))
                     (cons reg acc)
                     acc))
               '()
               env))
  (%make-asm (volatile-regs-in-use env) #f #f end-address '()))

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

(define-syntax-rule (memory-ref dst src)
  (cond
   ((not (memory? src))
    (tjitc-error 'memory-ref "not a memory ~s" src))
   (else
    (jit-ldxi dst %fp (moffs src)))))

(define-syntax-rule (memory-ref/f dst src)
  (cond
   ((not (memory? src))
    (tjitc-error 'memory-ref/f "not a memory ~s" src))
   (else
    (jit-ldxi-d dst %fp (moffs src)))))

(define-syntax-rule (memory-set! dst src)
  (cond
   ((not (memory? dst))
    (tjitc-error 'memory-set! "not a memory ~s" dst))
   (else
    (jit-stxi (moffs dst) %fp src))))

(define-syntax-rule (memory-set!/f dst src)
  (cond
   ((not (memory? dst))
    (tjitc-error 'memory-set!/f "not a memory" dst))
   (else
    (jit-stxi-d (moffs dst) %fp src))))

(define-syntax-rule (emit-side-exit)
  "Emit native code of side exit.

This macro emits native code which does jumping to current side exit. Assuming
that the `out-code' of ASM contains native code for next side exit.

Side trace reuses native code which does %rsp shifting from parent trace's
epilog part. Native code of side trace does not reset %rsp, since it uses
`jit-tramp'."
  (jumpi (asm-out-code asm)))

(define-syntax-rule (current-exit)
  (asm-exit asm))


;;;;
;;;; Native operations
;;;;

;;;
;;; Guards
;;;

(define-syntax define-binary-guard-int
  (lambda (x)
    "Macro for defining guard primitive which takes two int arguments.

`define-binary-int-guard NAME CONST-CONST-OP REG-CONST-OP REG-REG-OP' will
define a primitive named NAME. Uses CONST-CONST-OP when both arguments matched
`constant?' predicate. Uses REG-CONST-OP when the first argument matched `gpr?'
or `memory?' predicate while the second was constant. And, uses REG-REG-OP when
both arguments were register or memory."
    (syntax-case x ()
      ((_ name const-const-op reg-const-op reg-reg-op)
       #`(define-native (name (int a) (int b))
           (cond
            ((and (constant? a) (constant? b))
             (when (const-const-op (ref-value a) (ref-value b))
               (jump (current-exit))))
            ((and (constant? a) (gpr? b))
             (jit-movi r0 (constant a))
             (jump (reg-reg-op r0 (gpr b)) (current-exit)))
            ((and (constant? a) (memory? b))
             (jit-movi r0 (constant a))
             (memory-ref r1 b)
             (jump (reg-reg-op r0 r1) (current-exit)))

            ((and (gpr? a) (constant? b))
             (jump (reg-const-op (gpr a) (constant b)) (current-exit)))
            ((and (gpr? a) (gpr? b))
             (jump (reg-reg-op (gpr a) (gpr b)) (current-exit)))
            ((and (gpr? a) (memory? b))
             (memory-ref r0 b)
             (jump (reg-reg-op (gpr a) r0) (current-exit)))

            ((and (memory? a) (constant? b))
             (memory-ref r0 a)
             (jump (reg-const-op r0 (constant b))  (current-exit)))
            ((and (memory? a) (gpr? b))
             (memory-ref r0 a)
             (jump (reg-reg-op r0 (gpr b)) (current-exit)))
            ((and (memory? a) (memory? b))
             (memory-ref r0 a)
             (memory-ref r1 b)
             (jump (reg-reg-op r0 r1) (current-exit)))

            (else
             (tjitc-error 'name "~s ~s" a b))))))))

;; Auxiliary procedure for %ne.
(define (!= a b) (not (= a b)))

(define-binary-guard-int %eq != jit-bnei jit-bner)
(define-binary-guard-int %ne = jit-beqi jit-beqr)
(define-binary-guard-int %lt >= jit-bgei jit-bger)
(define-binary-guard-int %le > jit-bgti jit-bgtr)
(define-binary-guard-int %ge < jit-blti jit-bltr)
(define-binary-guard-int %gt <= jit-blei jit-bler)

(define-native (%flt (double a) (double b))
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
      (tjitc-error '%flt "~s ~s" a b)))
    (emit-side-exit)
    (jit-link next)))

(define-native (%fge (double a) (double b))
  (let ((next (jit-forward)))
    (cond
     ((and (fpr? a) (fpr? b))
      (jump (jit-bger-d (fpr a) (fpr b)) next))
     ((and (fpr? a) (memory? b))
      (memory-ref/f f0 b)
      (jump (jit-bger-d (fpr a) f0) next))

     ((and (memory? a) (memory? b))
      (memory-ref/f f0 a)
      (memory-ref/f f1 b)
      (jump (jit-bger-d f0 f1) next))

     (else
      (tjitc-error '%fge "~s ~s" a b)))
    (emit-side-exit)
    (jit-link next)))


;;;
;;; Call and return
;;;

(define-native (%carg (int dst))
  (set-asm-cargs! asm (cons dst (asm-cargs asm))))

;;; Scheme procedure call.
(define-native (%pcall (void proc))
  (let* ((vp r0)
         (vp->fp r1))
    (load-vp vp)
    (load-vp->fp vp->fp vp)
    (jit-subi vp->fp vp->fp (imm (* (ref-value proc) %word-size)))
    (store-vp->fp vp vp->fp)))

(define-native (%ccall (int dst) (void cfunc))
  (let ((volatiles (asm-volatiles asm))
        (cargs (asm-cargs asm)))
    (for-each store-volatile volatiles)
    (jit-prepare)
    (let lp ((cargs cargs) (i 1) (pushed '()))
      (match cargs
        ((carg . cargs)
         (let ((overwritten? (and (member carg volatiles)
                                  (member carg pushed))))
           (push-gpr-or-mem carg overwritten?)
           (lp cargs (+ i 1) (cons (argr i) pushed))))
        (()
         (values))))
    (jit-calli (imm (ref-value cfunc)))
    (retval-to-gpr-or-mem dst)
    (for-each (lambda (reg)
                (when (not (equal? reg dst))
                  (load-volatile reg)))
              volatiles)
    (set-asm-cargs! asm '())))

;;; Return from procedure call. Shift current FP to the one from dynamic
;;; link. Guard with return address, checks whether it match with the IP used at
;;; the time of compilation.
(define-native (%return (void ip))
  (let ((vp r0)
        (vp->fp r1)
        (tmp r2))
    (when (not (constant? ip))
      (tjitc-error '%return "got non-constant ip ~s" ip))
    (load-vp vp)
    (load-vp->fp vp->fp vp)
    (scm-frame-return-address tmp vp->fp)
    (jump (jit-bnei tmp (constant ip)) (current-exit))

    (scm-frame-dynamic-link tmp vp->fp)
    (jit-lshi tmp tmp (imm %word-size-in-bits))
    (jit-addr vp->fp vp->fp tmp)
    (store-vp->fp vp vp->fp)))


;;;
;;; Exact integer
;;;

;;; XXX: Manage overflow and underflow.
(define-syntax define-binary-arith-int
  (lambda (x)
    (syntax-case x ()
      ((_ name const-const-op reg-const-op reg-reg-op)
       #`(define-native (name (int dst) (int a) (int b))
           (cond
            ((and (gpr? dst) (constant? a) (constant? b))
             (let ((result (const-const-op (ref-value a) (ref-value b))))
               (jit-movi (gpr dst) (make-signed-pointer result))))
            ((and (gpr? dst) (gpr? a) (constant? b))
             (reg-const-op (gpr dst) (gpr a) (constant b)))
            ((and (gpr? dst) (gpr? a) (gpr? b))
             (reg-reg-op (gpr dst) (gpr a) (gpr b)))
            ((and (gpr? dst) (gpr? a) (memory? b))
             (memory-ref r0 b)
             (reg-reg-op (gpr dst) (gpr a) r0))
            ((and (gpr? dst) (memory? a) (constant? b))
             (memory-ref r0 a)
             (reg-const-op (gpr dst) r0 (constant b)))
            ((and (gpr? dst) (memory? a) (gpr? b))
             (memory-ref r0 a)
             (reg-reg-op (gpr dst) r0 (gpr b)))
            ((and (gpr? dst) (memory? a) (memory? b))
             (memory-ref r0 a)
             (memory-ref r1 b)
             (reg-reg-op (gpr dst) r0 r1))

            ((and (memory? dst) (constant? a) (constant? b))
             (let ((result (const-const-op (ref-value a) (ref-value b))))
               (jit-movi r0 (make-signed-pointer result))
               (memory-set! dst r0)))
            ((and (memory? dst) (constant? a) (gpr? b))
             (jit-movi r0 (constant a))
             (reg-reg-op r0 r0 (gpr b))
             (memory-set! dst r0))
            ((and (memory? dst) (gpr? a) (constant? b))
             (reg-const-op r0 (gpr a) (constant b))
             (memory-set! dst r0))
            ((and (memory? dst) (gpr? a) (gpr? b))
             (reg-reg-op r0 (gpr a) (gpr b))
             (memory-set! dst r0))
            ((and (memory? dst) (gpr? a) (memory? b))
             (memory-ref r1 b)
             (reg-reg-op r0 (gpr a) r1)
             (memory-set! dst r0))
            ((and (memory? dst) (constant? a) (memory? b))
             (jit-movi r0 (constant a))
             (memory-ref r1 b)
             (reg-reg-op r0 r0 r1)
             (memory-set! dst r0))
            ((and (memory? dst) (memory? a) (constant? b))
             (memory-ref r0 a)
             (reg-const-op r0 r0 (constant b))
             (memory-set! dst r0))
            ((and (memory? dst) (memory? a) (gpr? b))
             (memory-ref r0 a)
             (reg-reg-op r0 r0 (gpr b))
             (memory-set! dst r0))
            ((and (memory? dst) (memory? a) (memory? b))
             (memory-ref r0 a)
             (memory-ref r1 b)
             (reg-reg-op r0 r0 r1)
             (memory-set! dst r0))

            (else
             (tjitc-error 'name "~s ~s ~s" dst a b))))))))

(define-binary-arith-int %add + jit-addi jit-addr)
(define-binary-arith-int %sub - jit-subi jit-subr)

(define-native (%rsh (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-rshr (gpr dst) (gpr a) (gpr b)))
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-rshi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (memory? a) (constant? b))
    (memory-ref r0 a)
    (jit-rshi (gpr dst) r0 (constant b)))

   ((and (memory? dst) (gpr? a) (constant? b))
    (jit-rshi r0 (gpr a) (constant b))
    (memory-set! dst r0))
   ((and (memory? dst) (memory? a) (constant? b))
    (memory-ref r0 a)
    (jit-rshi r0 r0 (constant b))
    (memory-set! dst r0))
   (else
    (tjitc-error '%rsh "~s ~s ~s" dst a b))))

(define-native (%lsh (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (constant? a) (constant? b))
    (jit-movi (gpr dst) (imm (ash (ref-value a) (ref-value b)))))
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-lshi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-lshr (gpr dst) (gpr a) (gpr b)))
   ((and (gpr? dst) (fpr? a) (constant? b))
    (fpr->gpr r0 (fpr a))
    (jit-lshi (gpr dst) r0 (constant b)))
   ((and (gpr? dst) (memory? a) (constant? b))
    (memory-ref r0 a)
    (jit-lshi (gpr dst) r0 (constant b)))

   ((and (memory? dst) (constant? a) (constant? b))
    (jit-movi r0 (imm (ash (ref-value a) (ref-value b))))
    (memory-set! dst r0))
   ((and (memory? dst) (gpr? a) (constant? b))
    (jit-lshi r0 (gpr a) (constant b))
    (memory-set! dst r0))
   ((and (memory? dst) (memory? a) (constant? b))
    (memory-ref r0 a)
    (jit-lshi r0 r0 (constant b))
    (memory-set! dst r0))
   (else
    (tjitc-error '%lsh "~s ~s ~s" dst a b))))

(define-native (%mod (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-remi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-remr (gpr dst) (gpr a) (gpr b)))
   ((and (gpr? dst) (memory? a) (gpr? b))
    (memory-ref r0 a)
    (jit-remr (gpr dst) r0 (gpr b)))
   (else
    (tjitc-error '%mod "~s ~s ~s" dst a b))))


;;;
;;; Floating point
;;;

;;; XXX: Make lower level operation and rewrite.
(define-native (%from-double (int dst) (double src))
  (cond
   ((and (gpr? dst) (constant? src))
    (jit-movi-d f0 (constant src))
    (scm-from-double (gpr dst) f0))
   ((and (gpr? dst) (fpr? src))
    (scm-from-double (gpr dst) (fpr src)))
   ((and (gpr? dst) (memory? src))
    (memory-ref/f f0 src)
    (scm-from-double (gpr dst) f0))

   ((and (memory? dst) (constant? src))
    (jit-movi-d f0 (constant src))
    (scm-from-double r0 f0)
    (memory-set! dst r0))
   ((and (memory? dst) (fpr? src))
    (scm-from-double r0 (fpr src))
    (memory-set! dst r0))
   ((and (memory? dst) (memory? src))
    (memory-ref/f f0 src)
    (scm-from-double r0 f0)
    (memory-set! dst r0))
   (else
    (tjitc-error '%scm-from-double "~s ~s ~s" dst src))))

(define-syntax define-binary-arith-double
  (lambda (x)
    (syntax-case x ()
      ((k name op-ri op-rr)
       #`(define-native (name (double dst) (double a) (double b))
           (let-syntax ((show-error
                         (syntax-rules ()
                           ((_) (tjitc-error 'name "~s ~s ~s" dst a b)))))
             (cond
              ((fpr? dst)
               (cond
                ((constant? a)
                 (jit-movi-d f0 (constant a))
                 (cond
                  ((fpr? b)
                   (op-rr (fpr dst) f0 (fpr b)))
                  ((memory? b)
                   (memory-ref/f f1 b)
                   (op-rr (fpr dst) f0 f1))
                  (else (show-error))))
                ((fpr? a)
                 (cond
                  ((constant? b)
                   (op-ri (fpr dst) (fpr a) (constant b)))
                  ((gpr? b)
                   (gpr->fpr f0 (gpr b))
                   (op-rr (fpr dst) (fpr a) f0))
                  ((fpr? b)
                   (op-rr (fpr dst) (fpr a) (fpr b)))
                  ((memory? b)
                   (memory-ref/f f0 b)
                   (op-rr (fpr dst) (fpr a) f0))
                  (else (show-error))))
                ((memory? a)
                 (cond
                  ((constant? b)
                   (memory-ref/f f0 a)
                   (op-ri (fpr dst) f0 (constant b)))
                  ((fpr? b)
                   (memory-ref/f f0 a)
                   (op-rr (fpr dst) f0 (fpr b)))
                  ((memory? b)
                   (memory-ref/f f0 a)
                   (memory-ref/f f1 b)
                   (op-rr (fpr dst) f0 f1))
                  (else (show-error))))))
              ((memory? dst)
               (cond
                ((constant? a)
                 (jit-movi-d f1 a)
                 (cond
                  ((fpr? b)
                   (op-rr f0 f1 (fpr b)))
                  ((memory? b)
                   (memory-ref/f f2 b)
                   (op-rr f0 f1 f2))
                  (else (show-error))))
                ((fpr? a)
                 (cond
                  ((constant? b)
                   (op-ri f0 (fpr a) (constant b)))
                  ((fpr? b)
                   (op-rr f0 (fpr a) (fpr b)))
                  ((memory? b)
                   (memory-ref/f f1 b)
                   (op-rr f0 (fpr a) f1))
                  (else (show-error))))
                ((memory? a)
                 (memory-ref/f f1 a)
                 (cond
                  ((constant? b)
                   (op-ri f0 f1 (constant b)))
                  ((fpr? b)
                   (op-rr f0 f1 (fpr b)))
                  ((memory? b)
                   (memory-ref/f f2 b)
                   (op-rr f0 f1 f2))
                  (else (show-error)))))
               (memory-set!/f dst f0))
              (else (show-error)))))))))

(define-binary-arith-double %fadd jit-addi-d jit-addr-d)
(define-binary-arith-double %fsub jit-subi-d jit-subr-d)
(define-binary-arith-double %fmul jit-muli-d jit-mulr-d)
(define-binary-arith-double %fdiv jit-divi-d jit-divr-d)


;;;
;;; Load and store
;;;

(define-syntax unbox-stack-element
  (syntax-rules ()
    ((_ dst src type guard?)
     ;; Passing `guard?' parameter, control expansion of
     ;; `(current-exit)' at macro-expansion time, since it uses syntax
     ;; paramter `asm'.
     (letrec-syntax ((load-constant
                      (syntax-rules ()
                        ((_ constant #f)
                         (cond
                          ((gpr? dst)
                           (jit-movr (gpr dst) src))
                          ((memory? dst)
                           (memory-set! dst src))
                          (else
                           (tjitc-error 'unbox-stack-element "~s ~s"
                                        dst type))))
                        ((_ constant any)
                         (begin
                           (jump (jit-bnei src constant) (current-exit))
                           (load-constant constant #f)))))
                     (maybe-guard
                      (syntax-rules ()
                        ((_ #f exp)
                         (values))
                        ((_ any exp)
                         exp))))
       (cond
        ((eq? type &flonum)
         (cond
          ((fpr? dst)
           (scm-real-value (fpr dst) src))
          ((memory? dst)
           (scm-real-value f0 src)
           (memory-set!/f dst f0))))
        ((eq? type &exact-integer)
         (maybe-guard guard?
                      (jump (scm-not-inump src) (current-exit)))
         (cond
          ((gpr? dst)
           (jit-rshi (gpr dst) src (imm 2)))
          ((memory? dst)
           (jit-rshi r0 src (imm 2))
           (memory-set! dst r0))))
        ((eq? type &false)
         (load-constant *scm-false* guard?))
        ((eq? type &true)
         (load-constant *scm-true* guard?))
        ((eq? type &unspecified)
         (load-constant *scm-unspecified* guard?))
        ((eq? type &undefined)
         (load-constant *scm-undefined* guard?))
        ((eq? type &null)
         (load-constant *scm-null* guard?))
        ((memq type (list &char &box &procedure &pair &hash-table &u64))
         ;; XXX: Add guard for each type.
         (cond
          ((gpr? dst)
           (jit-movr (gpr dst) src))
          ((memory? dst)
           (memory-set! dst src))))
        (else
         (tjitc-error 'unbox-stack-element "~a ~a ~s" (physical-name dst)
                      (pretty-type type) guard?)))))))

;;; XXX: Not sure whether it's better to couple `xxx-ref' and `xxx-set!'
;;; instructions with expected type as done in bytecode, to have vector-ref,
;;; struct-ref, box-ref, string-ref, fluid-ref, bv-u8-ref ... etc or not. When
;;; instructions specify its operand type, size of ANF will be more compact, but
;;; may loose chances to optimize away type checking instructions.

;; Type check local N with TYPE and load to gpr or memory DST.
(define-native (%fref (int dst) (void n) (void type))
  (let ((t (ref-value type)))
    (when (or (not (constant? n))
              (not (constant? type)))
      (tjitc-error '%fref "~s ~s ~s" dst n type))
    (sp-ref r0 (ref-value n))
    (unbox-stack-element dst r0 t #t)))

;; Load frame local to fpr or memory, with type check. This primitive
;; is used for loading floating point number to FPR.
(define-native (%fref/f (double dst) (void n) (void type))
  (let ((exit (jit-forward))
        (next (jit-forward))
        (t (ref-value type)))
    (when (or (not (constant? n))
              (not (constant? type)))
      (tjitc-error '%fref/f "~s ~s ~s" dst n type))
    (cond
     ((= t &flonum)
      (sp-ref r0 (ref-value n))
      (jump (scm-imp r0) (current-exit))
      (scm-cell-type r1 r0)
      (scm-typ16 r1 r1)
      (jump (scm-not-realp r1) (current-exit))
      (cond
       ((fpr? dst)
        (scm-real-value (fpr dst) r0))
       ((memory? dst)
        (scm-real-value f0 r0)
        (memory-set!/f dst f0))
       (else
        (tjitc-error '%fref/f "~s ~s ~s" dst n type))))
     ((= t &f64)
      (cond
       ((fpr? dst)
        (sp-ref/f (fpr dst) (ref-value n)))
       ((memory? dst)
        (sp-ref/f f0 (ref-value n))
        (memory-set!/f dst f0))
       (else
        (tjitc-error '%fref/f "~s ~s ~s" dst n type)))))))

(define-native (%cref (int dst) (int src) (void n))
  (cond
   ((and (gpr? dst) (constant? src) (constant? n))
    (let ((addr (+ (ref-value src) (* (ref-value n) %word-size))))
      (jit-ldi (gpr dst) (imm addr))))
   ((and (gpr? dst) (gpr? src) (constant? n))
    (jit-ldxi (gpr dst) (gpr src) (constant-word n)))
   ((and (gpr? dst) (fpr? src) (constant? n))
    (jit-truncr-d r0 (fpr src))
    (jit-ldxi (gpr dst) r0 (constant-word n)))
   ((and (gpr? dst) (memory? src) (constant? n))
    (memory-ref r0 src)
    (jit-ldxi (gpr dst) r0 (constant-word n)))

   ((and (memory? dst) (constant? src) (constant? n))
    (let ((addr (+ (ref-value src) (* (ref-value n) %word-size))))
      (jit-ldi r0 (imm addr))
      (memory-set! dst r0)))
   ((and (memory? dst) (gpr? src) (constant? n))
    (jit-ldxi r0 (gpr src) (constant-word n))
    (memory-set! dst r0))
   ((and (memory? dst) (memory? src) (constant? n))
    (memory-ref r0 src)
    (jit-ldxi r0 r0 (constant-word n))
    (memory-set! dst r0))
   (else
    (tjitc-error '%cref "~s ~s ~s" dst src n))))

(define-native (%cref/f (double dst) (int src) (void n))
  (cond
   ((and (fpr? dst) (gpr? src) (constant? n))
    (jit-ldxi-d (fpr dst) (gpr src) (constant-word n)))
   ((and (fpr? dst) (memory? src) (constant? n))
    (memory-ref r0 src)
    (jit-ldxi-d (fpr dst) r0 (constant-word n)))

   ((and (memory? dst) (gpr? src) (constant? n))
    (jit-ldxi-d f0 (gpr src) (constant-word n))
    (memory-set!/f dst f0))

   ((and (memory? dst) (memory? src) (constant? n))
    (memory-ref r0 src)
    (jit-ldxi-d f0 r0 (constant-word n))
    (memory-set!/f dst f0))

   (else
    (tjitc-error '%cref/f "~s ~s ~s" dst src n))))

(define-native (%cset (int cell) (void n) (int src))
  (cond
   ((and (gpr? cell) (constant? n) (gpr? src))
    (jit-stxi (constant-word n) (gpr cell) (gpr src)))
   ((and (gpr? cell) (constant? n) (memory? src))
    (memory-ref r0 src)
    (jit-stxi (constant-word n) (gpr cell) r0))

   ((and (memory? cell) (constant? n) (gpr? src))
    (memory-ref r0 cell)
    (jit-stxi (constant-word n) r0 (gpr src)))
   ((and (memory? cell) (constant? n) (memory? src))
    (memory-ref r0 cell)
    (memory-ref r1 src)
    (jit-stxi (constant-word n) r0 r1))

   (else
    (tjitc-error '%cset "~s ~s ~s" cell n src))))


;;;
;;; Heap objects
;;;

;; Call C function `scm_do_inline_cons'. Save volatile registers before calling,
;; restore after getting returned value.
(define-native (%cons (int dst) (int x) (int y))
  (let ((x-overwritten? (equal? x (argr 1)))
        (y-overwritten? (or (equal? y (argr 1))
                            (equal? y (argr 2)))))
    (for-each (lambda (reg)
                (when (or (and x-overwritten? (equal? reg x))
                          (and y-overwritten? (equal? reg y))
                          (not (equal? reg dst)))
                  (store-volatile reg)))
              (asm-volatiles asm))
    (jit-prepare)
    (jit-pushargr %thread)
    (push-gpr-or-mem x x-overwritten?)
    (push-gpr-or-mem y y-overwritten?)
    (jit-calli %scm-inline-cons)
    (retval-to-gpr-or-mem dst)
    (for-each (lambda (reg)
                (when (not (equal? reg dst))
                  (load-volatile reg)))
              (asm-volatiles asm))))


;;;
;;; Type conversion
;;;

(define-native (%i2d (double dst) (int src))
  (cond
   ((and (fpr? dst) (gpr? src))
    (jit-extr-d (fpr dst) (gpr src)))
   ((and (fpr? dst) (memory? src))
    (memory-ref r0 src)
    (jit-extr-d (fpr dst) r0))
   (else
    (tjitc-error '%i2d "~s ~s" dst src))))


;;;
;;; Move
;;;

(define (move dst src)
  (cond
   ((and (gpr? dst) (constant? src))
    (jit-movi (gpr dst) (constant src)))
   ((and (gpr? dst) (gpr? src))
    (jit-movr (gpr dst) (gpr src)))
   ((and (gpr? dst) (fpr? src))
    (fpr->gpr (gpr dst) (fpr src)))
   ((and (gpr? dst) (memory? src))
    (memory-ref r0 src)
    (jit-movr (gpr dst) r0))

   ((and (fpr? dst) (constant? src))
    (let ((val (ref-value src)))
      (cond
       ((and (number? val) (flonum? val))
        (jit-movi-d (fpr dst) (constant src)))
       ((and (number? val) (exact? val))
        (jit-movi r0 (constant src))
        (jit-extr-d (fpr dst) r0))
       (else
        (debug 3 "XXX move: ~a ~a~%" dst src)))))
   ((and (fpr? dst) (gpr? src))
    (gpr->fpr (fpr dst) (gpr src)))
   ((and (fpr? dst) (fpr? src))
    (jit-movr-d (fpr dst) (fpr src)))
   ((and (fpr? dst) (memory? src))
    (memory-ref/f f0 src)
    (jit-movr-d (fpr dst) f0))

   ((and (memory? dst) (constant? src))
    (let ((val (ref-value src)))
      (cond
       ((fixnum? val)
        (jit-movi r0 (constant src))
        (memory-set! dst r0))
       ((flonum? val)
        (jit-movi-d f0 (constant src))
        (memory-set!/f dst f0)))))
   ((and (memory? dst) (gpr? src))
    (memory-set! dst (gpr src)))
   ((and (memory? dst) (fpr? src))
    (memory-set!/f dst (fpr src)))
   ((and (memory? dst) (memory? src))
    (memory-ref r0 src)
    (memory-set! dst r0))

   (else
    (debug 1 "XXX move: ~a ~a~%" dst src))))

(define-native (%move (int dst) (int src))
  (move dst src))
