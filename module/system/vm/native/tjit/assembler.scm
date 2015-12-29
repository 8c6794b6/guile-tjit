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
  #:use-module (system vm program)
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

(define-syntax-parameter err
  (lambda (x)
    (syntax-violation 'err "err used outside of primitive definition" x)))

(define-syntax define-native
  (syntax-rules ()
    ((_ (name (ty arg) ...) <body>)
     (begin
       (define (name asm-in-arg arg ...)
         (let ((verbosity (lightning-verbosity)))
           (when (and verbosity (<= 5 verbosity))
             (jit-note (format #f "~a" `(name ,arg ...)) 0))
           (debug 4 ";;; (~12a ~{~a~^ ~})~%" 'name `(,arg ...)))
         (syntax-parameterize
             ((asm (identifier-syntax asm-in-arg))
              (err (syntax-rules ()
                     ((_)
                      (tjitc-error 'name
                                   ((lambda args
                                      (let lp ((args args) (acc '()))
                                        (if (null? args)
                                            (string-join acc " ")
                                            (lp (cdr args) (cons "~s" acc)))))
                                    'arg ...)
                                   arg ...)))))
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

(define-syntax scm-from-double
  (syntax-rules ()
    ((_ dst src)
     (scm-from-double asm dst src))
    ((_ asm-arg dst src)
     (let ((volatiles (asm-volatiles asm-arg)))
       (for-each store-volatile volatiles)
       (jit-prepare)
       (jit-pushargr %thread)
       (jit-pushargr-d src)
       (jit-calli %scm-from-double)
       (jit-retval dst)
       (for-each (lambda (reg)
                   (when (not (cond ((fpr? reg)
                                     (equal? (fpr reg) dst))
                                    ((gpr? reg)
                                     (equal? (gpr reg) dst))
                                    (else #f)))
                     (load-volatile reg)))
                 volatiles)))))

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
  (let ((n (- (+ 2 1 (ref-value mem) *num-volatiles* *num-fpr*))))
    (make-negative-pointer (* n %word-size))))

(define registers-offset
  (make-negative-pointer (* -1 %word-size)))

(define (volatile-offset reg)
  (let ((n (cond
            ((gpr? reg)
             (+ 2 (- (ref-value reg) *num-non-volatiles*)))
            ((fpr? reg)
             (+ 2 1 (ref-value reg) *num-volatiles*))
            (else
             (tjitc-error 'volatile-offset "~s" reg)))))
    (make-negative-pointer (* (- n) %word-size))))

(define (spilled-offset mem)
  (moffs mem))

(define-syntax-rule (fpr->gpr dst src)
  (let ((tmp-offset (volatile-offset `(gpr . 8))))
    (jit-stxi-d tmp-offset %fp src)
    (jit-ldxi dst %fp tmp-offset)
    dst))

(define-syntax-rule (gpr->fpr dst src)
  (let ((tmp-offset (volatile-offset `(gpr . 8))))
    (jit-stxi tmp-offset %fp src)
    (jit-ldxi-d dst %fp tmp-offset)
    dst))

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
  (cond
   ((gpr? src) (jit-stxi (volatile-offset src) %fp (gpr src)))
   ((fpr? src) (jit-stxi-d (volatile-offset src) %fp (fpr src)))
   (else (tjitc-error 'store-volatile "~s" src))))

(define (load-volatile dst)
  (cond
   ((gpr? dst) (jit-ldxi (gpr dst) %fp (volatile-offset dst)))
   ((fpr? dst) (jit-ldxi-d (fpr dst) %fp (volatile-offset dst)))
   (else (tjitc-error 'load-volatile "~s" dst))))

(define-syntax-rule (push-as-gpr arg overwritten?)
  (cond
   (overwritten?
    (jit-ldxi r0 %fp (volatile-offset arg))
    (jit-pushargr r0))
   ((gpr? arg)
    (jit-pushargr (gpr arg)))
   ((fpr? arg)
    (jit-pushargr (fpr->gpr r0 (fpr arg))))
   ((memory? arg)
    (memory-ref r0 arg)
    (jit-pushargr r0))
   (else
    (tjitc-error 'push-as-gpr "unknown arg ~s" arg))))

(define-syntax-rule (retval-to-gpr-or-mem dst)
  (cond
   ((gpr? dst)
    (jit-retval (gpr dst)))
   ((memory? dst)
    (jit-retval r0)
    (memory-set! dst r0))
   (else
    (tjitc-error 'retval-to-gpr-or-mem "unknown dst ~s" dst))))

;;; XXX: Offsets for fields in C structs were manually taken with observing
;;; output from `objdump'. It would be nice if the offsets were derived in
;;; programmatic manner. Have not tested on architecture other than Linux
;;; x86-64.

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
                 (if (or (and (gpr? reg)
                              (<= *num-non-volatiles* (ref-value reg)))
                         (and (fpr? reg)
                              ;; Suppressing scratch registers.
                              (<= 0 (ref-value reg))))
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
    (jit-ldxi dst %fp (moffs src))
    dst)))

(define-syntax-rule (memory-ref/f dst src)
  (cond
   ((not (memory? src))
    (tjitc-error 'memory-ref/f "not a memory ~s" src))
   (else
    (jit-ldxi-d dst %fp (moffs src))
    dst)))

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

(define-syntax-rule (bailout)
  (asm-exit asm))


;;;;
;;;; Native operations
;;;;

;;;
;;; Guards
;;;

(define-syntax define-binary-guard-int
  (syntax-rules ()
    "Macro for defining guard primitive which takes two int arguments.

`define-binary-int-guard NAME OP-II OP-RI OP-RR' will define a primitive named
NAME. Uses OP-II when both arguments matched `constant?' predicate. Uses OP-RI
when the first argument matched `gpr?'  or `memory?' predicate while the second
was constant. And, uses OP-RR when both arguments were register or memory."
    ((_ name op-ii op-ri op-rr)
     (define-native (name (int a) (int b))
       (cond
        ((constant? a)
         (cond
          ((constant? b)
           (when (op-ii (ref-value a) (ref-value b))
             (jump (bailout))))
          (else
           (jit-movi r0 (constant a))
           (cond
            ((gpr? b)    (jump (op-rr r0 (gpr b)) (bailout)))
            ((memory? b) (jump (op-rr r0 (memory-ref r1 b)) (bailout)))
            (else (err))))))
        ((gpr? a)
         (cond
          ((constant? b) (jump (op-ri (gpr a) (constant b)) (bailout)))
          ((gpr? b)      (jump (op-rr (gpr a) (gpr b)) (bailout)))
          ((memory? b)   (jump (op-rr (gpr a) (memory-ref r1 b)) (bailout)))
          (else (err))))
        ((memory? a)
         (memory-ref r0 a)
         (cond
          ((constant? b) (jump (op-ri r0 (constant b)) (bailout)))
          ((gpr? b)      (jump (op-rr r0 (gpr b)) (bailout)))
          ((memory? b)   (jump (op-rr r0 (memory-ref r1 b)) (bailout)))
          (else (err))))
        (else
         (err)))))))

;; Auxiliary procedure for %ne.
(define (!= a b) (not (= a b)))

(define-binary-guard-int %eq != jit-bnei jit-bner)
(define-binary-guard-int %ne = jit-beqi jit-beqr)
(define-binary-guard-int %lt >= jit-bgei jit-bger)
(define-binary-guard-int %le > jit-bgti jit-bgtr)
(define-binary-guard-int %ge < jit-blti jit-bltr)
(define-binary-guard-int %gt <= jit-blei jit-bler)

(define-syntax define-binary-guard-double
  (syntax-rules ()
    ((_ name op-ii op-ri op-rr)
     (define-native (name (double a) (double b))
       (cond
        ((constant? a)
         (jit-movi-d f0 (constant a))
         (cond
          ((fpr? b)    (jump (op-rr f0 (fpr b)) (bailout)))
          ((memory? b) (jump (op-rr f0 (memory-ref/f f1 b)) (bailout)))
          (else (err))))
        ((fpr? a)
         (cond
          ((constant? b) (jump (op-ri (fpr a) (constant b)) (bailout)))
          ((fpr? b)      (jump (op-rr (fpr a) (fpr b)) (bailout)))
          ((memory? b)   (jump (op-rr (fpr a) (memory-ref/f f1 b)) (bailout)))
          (else (err))))
        ((memory? a)
         (memory-ref/f f0 a)
         (cond
          ((constant? b) (jump (op-ri f0 (constant b)) (bailout)))
          ((fpr? b)      (jump (op-rr f0 (fpr b)) (bailout)))
          ((memory? b)   (jump (op-rr f0 (memory-ref/f f1 b)) (bailout)))
          (else (err))))
        (else
         (err)))))))

(define-binary-guard-double %flt >= jit-bgei-d jit-bger-d)
(define-binary-guard-double %fge < jit-blti-d jit-bltr-d)


;;;
;;; Call and return
;;;

(define-native (%carg (int dst))
  (set-asm-cargs! asm (cons dst (asm-cargs asm))))

;;; Scheme procedure call. Shifts current FP.
(define-native (%scall (void proc))
  (let* ((vp r0)
         (vp->fp r1))
    (load-vp vp)
    (load-vp->fp vp->fp vp)
    (jit-subi vp->fp vp->fp (imm (* (ref-value proc) %word-size)))
    (store-vp->fp vp vp->fp)))

;;; C function call. Appears when Scheme primitive procedure defined as `gsubr'
;;; in C were called.
(define-native (%ccall (int dst) (void cfunc))
  (let ((volatiles (asm-volatiles asm))
        (cargs (asm-cargs asm)))
    (define (subrf subr-addr)
      (let ((subr (pointer->scm (make-pointer subr-addr))))
        (let ((var0 (program-free-variable-ref subr 0)))
          (if (pointer? var0)
              var0
              (tjitc-error '%ccall "not a primitive ~s" subr)))))
    (for-each store-volatile volatiles)
    (jit-prepare)
    (let lp ((cargs cargs) (i 1) (pushed '()))
      (match cargs
        ((carg . cargs)
         (let ((overwritten? (and (member carg volatiles)
                                  (member carg pushed))))
           (push-as-gpr carg overwritten?)
           (lp cargs (+ i 1) (cons (argr i) pushed))))
        (()
         (values))))
    (jit-calli (subrf (ref-value cfunc)))
    (retval-to-gpr-or-mem dst)
    (for-each (lambda (reg)
                (when (not (equal? reg dst))
                  (load-volatile reg)))
              volatiles)
    (set-asm-cargs! asm '())))

;;; Return from Scheme procedure call. Shift current FP to the one from dynamic
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
    (jump (jit-bnei tmp (constant ip)) (bailout))

    (scm-frame-dynamic-link tmp vp->fp)
    (jit-lshi tmp tmp (imm %word-size-in-bits))
    (jit-addr vp->fp vp->fp tmp)
    (store-vp->fp vp vp->fp)))


;;;
;;; Exact integer
;;;

;;; XXX: This macro does not manage overflow and underflow. Make another macro
;;; for managing overflow and underflow.
(define-syntax define-binary-arith-int
  (syntax-rules ()
    ((_ name op-ii op-ri op-rr)
     (define-native (name (int dst) (int a) (int b))
       (letrec-syntax
           ((op3b (syntax-rules ()
                    ((_ dst a)
                     (cond
                      ((constant? b) (op-ri dst a (constant b)))
                      ((gpr? b)      (op-rr dst a (gpr b)))
                      ((fpr? b)      (op-rr dst a (fpr->gpr r1 b)))
                      ((memory? b)   (op-rr dst a (memory-ref r1 b)))
                      (else (err))))))
            (op3a (syntax-rules ()
                    ((_ dst)
                     (begin
                       (cond
                        ((constant? a) (op3b dst (movi r0 a)))
                        ((gpr? a)      (op3b dst (gpr a)))
                        ((fpr? a)      (op3b dst (fpr->gpr r0 (fpr a))))
                        ((memory? a)   (op3b dst (memory-ref r0 a)))
                        (else (err)))
                       dst))))
            (movi (syntax-rules ()
                    ((_ x y)
                     (begin (jit-movi x (constant y))
                            x)))))
         (cond
          ((gpr? dst)    (op3a (gpr dst)))
          ((memory? dst) (memory-set! dst (op3a r0)))
          (else (err))))))))

(define (rsh a b) (ash a b))
(define (lsh a b) (ash a (- b)))

(define-binary-arith-int %add + jit-addi jit-addr)
(define-binary-arith-int %sub - jit-subi jit-subr)
(define-binary-arith-int %rsh rsh jit-rshi jit-rshr)
(define-binary-arith-int %lsh lsh jit-lshi jit-lshr)

(define-native (%mod (int dst) (int a) (int b))
  (cond
   ((and (gpr? dst) (gpr? a) (constant? b))
    (jit-remi (gpr dst) (gpr a) (constant b)))
   ((and (gpr? dst) (gpr? a) (gpr? b))
    (jit-remr (gpr dst) (gpr a) (gpr b)))
   ((and (gpr? dst) (memory? a) (gpr? b))
    (jit-remr (gpr dst) (memory-ref r0 a) (gpr b)))
   (else
    (err))))


;;;
;;; Floating point
;;;

(define-syntax define-binary-arith-double
  (syntax-rules ()
    ((k name op-ri op-rr)
     (define-native (name (double dst) (double a) (double b))
       (letrec-syntax
           ((op3b (syntax-rules ()
                    ((_ dst a)
                     (cond
                      ((constant? b) (op-ri dst a (constant b)))
                      ((gpr? b)      (op-rr dst a (gpr->fpr f1 (gpr b))))
                      ((fpr? b)      (op-rr dst a (fpr b)))
                      ((memory? b)   (op-rr dst a (memory-ref/f f1 b)))
                      (else (err))))))
            (op3a (syntax-rules ()
                    ((_ dst)
                     (begin
                       (cond
                        ((constant? a) (op3b dst (movi f2 a)))
                        ((fpr? a)      (op3b dst (fpr a)))
                        ((memory? a)   (op3b dst (memory-ref/f f2 a)))
                        (else (err)))
                       dst))))
            (movi (syntax-rules ()
                    ((_ x y)
                     (begin (jit-movi-d x (constant y))
                            x)))))
         (cond
          ((fpr? dst)    (op3a (fpr dst)))
          ((memory? dst) (memory-set!/f dst (op3a f0)))
          (else (err))))))))

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
     ;; Passing `guard?' parameter to control expansion of `(bailout)' at
     ;; macro-expansion time, since it uses syntax paramter `asm'.
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
                           (jump (jit-bnei src constant) (bailout))
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
         (maybe-guard guard? (jump (scm-not-inump src) (bailout)))
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
        ((memq type (list &char &box &procedure &pair &hash-table &u64
                          &vector))
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
      (err))
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
      (err))
    (cond
     ((= t &flonum)
      (sp-ref r0 (ref-value n))
      (jump (scm-imp r0) (bailout))
      (scm-cell-type r1 r0)
      (scm-typ16 r1 r1)
      (jump (scm-not-realp r1) (bailout))
      (cond
       ((fpr? dst)
        (scm-real-value (fpr dst) r0))
       ((memory? dst)
        (scm-real-value f0 r0)
        (memory-set!/f dst f0))
       (else
        (err))))
     ((= t &f64)
      (cond
       ((fpr? dst)
        (sp-ref/f (fpr dst) (ref-value n)))
       ((memory? dst)
        (sp-ref/f f0 (ref-value n))
        (memory-set!/f dst f0))
       (else
        (err)))))))

;; Cell object reference.
(define-native (%cref (int dst) (int src) (void n))
  (let ((nw (* (ref-value n) %word-size)))
    (cond
     ((not (constant? n))
      (err))
     ((gpr? dst)
      (cond
       ((constant? src) (jit-ldi (gpr dst) (imm (+ (ref-value src) nw))))
       ((gpr? src)      (jit-ldxi (gpr dst) (gpr src) (imm nw)))
       ((fpr? src)      (jit-ldxi (gpr dst) (fpr->gpr r0 (fpr src)) (imm nw)))
       ((memory? src)   (jit-ldxi (gpr dst) (memory-ref r0 src) (imm nw)))
       (else (err))))
     ((memory? dst)
      (cond
       ((constant? src) (jit-ldi r0 (imm (+ (ref-value src) nw))))
       ((gpr? src)      (jit-ldxi r0 (gpr src) (imm nw)))
       ((memory? src)   (jit-ldxi r0 (memory-ref r0 src) (imm nw)))
       (else (err)))
      (memory-set! dst r0))
     (else
      (err)))))

;; Cell reference to floating point register.
(define-native (%cref/f (double dst) (int src) (void n))
  (let ((nw (constant-word n)))
    (cond
     ((not (constant? n))
      (err))
     ((fpr? dst)
      (cond
       ((gpr? src)    (jit-ldxi-d (fpr dst) (gpr src) nw))
       ((memory? src) (jit-ldxi-d (fpr dst) (memory-ref r0 src) nw))
       (else (err))))
     ((memory? dst)
      (cond
       ((gpr? src)    (jit-ldxi-d f0 (gpr src) nw))
       ((memory? src) (jit-ldxi-d f0 (memory-ref r0 src) nw))
       (else (err)))
      (memory-set!/f dst f0))
     (else (err)))))

;; Set cell object.
(define-native (%cset (int cell) (void n) (int src))
  (let ((nw (constant-word n)))
    (cond
     ((not (constant? n))
      (err))
     ((gpr? cell)
      (cond
       ((gpr? src)    (jit-stxi nw (gpr cell) (gpr src)))
       ((memory? src) (jit-stxi nw (gpr cell) (memory-ref r0 src)))
       (else (err))))
     ((memory? cell)
      (memory-ref r0 cell)
      (cond
       ((gpr? src)    (jit-stxi nw r0 (gpr src)))
       ((memory? src) (jit-stxi nw r0 (memory-ref r1 src)))
       (else (err))))
     (else (err)))))


;;;
;;; Heap objects
;;;

;; Call C function `scm_do_inline_cons'. Save volatile registers before calling,
;; restore after getting returned value.
(define-native (%cons (int dst) (int x) (int y))
  (let ((volatiles (asm-volatiles asm))
        (x-overwritten? (equal? x (argr 1)))
        (y-overwritten? (or (equal? y (argr 1))
                            (equal? y (argr 2)))))
    (for-each (lambda (reg)
                (when (or (and x-overwritten? (equal? reg x))
                          (and y-overwritten? (equal? reg y))
                          (not (equal? reg dst)))
                  (store-volatile reg)))
              volatiles)
    (jit-prepare)
    (jit-pushargr %thread)
    (push-as-gpr x x-overwritten?)
    (push-as-gpr y y-overwritten?)
    (jit-calli %scm-inline-cons)
    (retval-to-gpr-or-mem dst)
    (for-each (lambda (reg)
                (when (not (equal? reg dst))
                  (load-volatile reg)))
              volatiles)))


;;;
;;; Type conversion
;;;

;; integer -> floating point
(define-native (%i2d (double dst) (int src))
  (cond
   ((fpr? dst)
    (cond
     ((gpr? src)    (jit-extr-d (fpr dst) (gpr src)))
     ((memory? src) (jit-extr-d (fpr dst) (memory-ref r0 src)))
     (else (err))))
   ((memory? dst)
    (cond
     ((gpr? src)    (jit-extr-d f0 (gpr src)))
     ((memory? src) (jit-extr-d f0 (memory-ref r0 src)))
     (else (err)))
    (memory-set!/f dst f0))
   (else (err))))

;; floating point -> SCM
(define-native (%d2s (int dst) (double src))
  (cond
   ((gpr? dst)
    (cond
     ((constant? src) (begin (jit-movi-d f0 (constant src))
                             (scm-from-double (gpr dst) f0)))
     ((fpr? src)      (scm-from-double (gpr dst) (fpr src)))
     ((memory? src)   (scm-from-double (gpr dst) (memory-ref/f f0 src)))
     (else (err))))
   ((memory? dst)
    (cond
     ((constant? src) (begin (jit-movi-d f0 (constant src))
                             (scm-from-double r0 f0)))
     ((fpr? src)      (scm-from-double r0 (fpr src)))
     ((memory? src)   (scm-from-double r0 (memory-ref/f f0 src)))
     (else (err)))
    (memory-set! dst r0))
   (else (err))))


;;;
;;; Move
;;;

(define (move dst src)
  (let-syntax ((err (syntax-rules ()
                      ((_) (tjitc-error 'move "~s ~s" dst src)))))
    (cond
     ((gpr? dst)
      (cond
       ((constant? src) (jit-movi (gpr dst) (constant src)))
       ((gpr? src)      (jit-movr (gpr dst) (gpr src)))
       ((fpr? src)      (fpr->gpr (gpr dst) (fpr src)))
       ((memory? src)   (memory-ref (gpr dst) src))
       (else (err))))
     ((fpr? dst)
      (cond
       ((constant? src) (let ((val (ref-value src)))
                          (cond
                           ((and (number? val) (flonum? val))
                            (jit-movi-d (fpr dst) (constant src)))
                           ((and (number? val) (exact? val))
                            (jit-movi r0 (constant src))
                            (gpr->fpr (fpr dst) r0))
                           (else
                            (err)))))
       ((gpr? src)      (gpr->fpr (fpr dst) (gpr src)))
       ((fpr? src)      (jit-movr-d (fpr dst) (fpr src)))
       ((memory? src)   (memory-ref/f (fpr dst) src))
       (else (err))))
     ((memory? dst)
      (cond
       ((constant? src) (let ((val (ref-value src)))
                          (cond
                           ((fixnum? val)
                            (jit-movi r0 (constant src))
                            (memory-set! dst r0))
                           ((flonum? val)
                            (jit-movi-d f0 (constant src))
                            (memory-set!/f dst f0)))))
       ((gpr? src)      (memory-set! dst (gpr src)))
       ((fpr? src)      (memory-set!/f dst (fpr src)))
       ((memory? src)   (memory-set! dst (memory-ref r0 src)))
       (else (err))))
     (else
      (err)))))

(define-native (%move (int dst) (int src))
  (move dst src))
