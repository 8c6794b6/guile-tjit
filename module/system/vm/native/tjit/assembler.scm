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
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables)
  #:export (*native-prim-procedures*
            *native-prim-types*
            *scm-false*
            *scm-true*
            *scm-undefined*
            *scm-unspecified*
            *scm-null*
            asm
            make-asm
            set-asm-exit!
            asm-end-address
            asm-volatiles
            make-signed-pointer
            spilled-offset
            constant-word
            gpr->fpr
            fpr->gpr
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
            unbox-stack-element
            guard-type))

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

(define *native-prim-procedures* (make-hash-table))
(define *native-prim-types* (make-hash-table))

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
       (define (name %asm arg ...)
         (let ((verbosity (lightning-verbosity)))
           (when (and verbosity (<= 4 verbosity))
             (let ((f (lambda (x)
                        (case (ref-type x)
                          ((con) (ref-value x))
                          (else (physical-name x))))))
               (jit-note (format #f "~a" (cons 'name (map f `(,arg ...)))) 0)))
           (debug 4 ";;; (~12a ~{~a~^ ~})~%" 'name `(,arg ...)))
         (syntax-parameterize
             ((asm (identifier-syntax %asm))
              (err
               (syntax-rules ()
                 ((_)
                  (failure 'name
                           ((lambda args
                              (let lp ((args args) (acc '()))
                                (if (null? args)
                                    (string-join acc " ")
                                    (lp (cdr args)
                                        (cons "~a" acc)))))
                            'arg ...)
                           arg ...)))))
           <body>))
       (hashq-set! *native-prim-procedures* 'name name)
       (hashq-set! *native-prim-types* 'name `(,ty ...))))))


;;;
;;; Scheme constants
;;;

(define *scm-false*
  (scm->pointer #f))

(define *scm-true*
  (scm->pointer #t))

(define *scm-nil*
  (scm->pointer #nil))

(define *scm-unspecified*
  (scm->pointer *unspecified*))

(define *scm-undefined*
  (make-pointer #x904))

(define *scm-unbound*
  (make-pointer #xb04))

(define *scm-null*
  (scm->pointer '()))


;;;
;;; SCM macros
;;;

(define-syntax-rule (scm-real-value dst src)
  (begin
    (jit-ldxi-d dst src (imm (* 2 %word-size)))
    dst))

(define %scm-from-double
  (dynamic-pointer "scm_from_double" (dynamic-link)))

(define %scm-inline-from-double
  (dynamic-pointer "scm_do_inline_from_double" (dynamic-link)))

(define-syntax scm-from-double
  (syntax-rules ()
    ((_  dst src)
     (let ((volatiles (asm-volatiles asm)))
       (for-each store-volatile volatiles)
       (jit-prepare)
       (when (asm-gc-inline? asm)
         (jit-pushargr %thread))
       (jit-pushargr-d src)
       (if (asm-gc-inline? asm)
           (jit-calli %scm-inline-from-double)
           (jit-calli %scm-from-double))
       (jit-retval dst)
       (for-each (lambda (reg)
                   (unless (case (ref-type reg)
                             ((fpr) (equal? (fpr reg) dst))
                             ((gpr) (equal? (gpr reg) dst))
                             (else #f))
                     (load-volatile reg)))
                 volatiles)))))

(define %scm-cell
  (dynamic-pointer "scm_cell" (dynamic-link)))

(define %scm-inline-cell
  (dynamic-pointer "scm_do_inline_cell" (dynamic-link)))

(define %scm-words
  (dynamic-pointer "scm_words" (dynamic-link)))

(define %scm-inline-words
  (dynamic-pointer "scm_do_inline_words" (dynamic-link)))

(define %scm-make-continuation
  (dynamic-pointer "scm_do_make_continuation" (dynamic-link)))

(define %scm-eqv
  (dynamic-pointer "scm_eqv_p" (dynamic-link)))

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

(define registers-offset
  (make-negative-pointer (* -1 %word-size)))

(define (volatile-offset reg)
  (let ((n (case (ref-type reg)
             ((gpr)
              (+ 2 (- (ref-value reg) *num-non-volatiles*)))
             ((fpr)
              (+ 2 1 (ref-value reg) *num-volatiles*))
             (else
              (failure 'volatile-offset "~s" reg)))))
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

(define-syntax vm-sync-sp
  (syntax-rules ()
    ((_ src)
     (let ((vp (if (eq? src r0) r1 r0)))
       (load-vp vp)
       (vm-sync-sp src vp)))
    ((_ src vp)
     (jit-stxi (imm %word-size) vp src))))

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
  (case (ref-type src)
    ((gpr) (jit-stxi (volatile-offset src) %fp (gpr src)))
    ((fpr) (jit-stxi-d (volatile-offset src) %fp (fpr src)))
    (else (failure 'store-volatile "~s" src))))

(define (load-volatile dst)
  (case (ref-type dst)
    ((gpr) (jit-ldxi (gpr dst) %fp (volatile-offset dst)))
    ((fpr) (jit-ldxi-d (fpr dst) %fp (volatile-offset dst)))
    (else (failure 'load-volatile "~s" dst))))

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

(define-syntax-rule (vm-handle-interrupts)
  (let ((next (jit-forward))
        (volatiles (asm-volatiles asm)))
    (vm-thread-pending-asyncs r0)
    (jump (jit-bmci r0 (imm 1)) next)
    (for-each store-volatile volatiles)
    (jit-prepare)
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
    (vm-sync-sp %sp vp)

    (jit-link next)))


;;;
;;; Predicates
;;;

(define-syntax-rule (scm-imp obj)
  (jit-bmsi obj (imm 6)))

(define-syntax-rule (scm-inump obj)
  (jit-bmsi obj (imm %tc2-int)))

(define-syntax-rule (scm-not-inump obj)
  (jit-bmci obj (imm %tc2-int)))

(define-syntax-rule (scm-realp tag)
  (jit-beqi tag (imm %tc16-real)))

(define-syntax-rule (scm-not-realp tag)
  (jit-bnei tag (imm %tc16-real)))


;;;
;;; Assembler state
;;;

(define-record-type <asm>
  (%make-asm volatiles exit end-address cargs gc-inline?)
  asm?

  ;; Volatile registers in use.
  (volatiles asm-volatiles)

  ;; Current exit to jump.
  (exit asm-exit set-asm-exit!)

  ;; Pointer of native code at the end of parent trace.
  (end-address asm-end-address)

  ;; Arguments for function call.
  (cargs asm-cargs set-asm-cargs!)

  ;; Flag to use C functions from "libguile/gc-inline.h".
  (gc-inline? asm-gc-inline?))

(define (make-asm storage end-address gc-inline save-volatiles?)
  (define (volatile-regs-in-use storage)
    (hash-fold (lambda (_ reg acc)
                 (if (or (and reg (gpr? reg)
                              (<= *num-non-volatiles* (ref-value reg)))
                         (and reg (fpr? reg)
                              ;; Suppressing scratch registers.
                              (<= 0 (ref-value reg))))
                     (cons reg acc)
                     acc))
               '()
               storage))
  (let ((volatiles (volatile-regs-in-use storage)))
    (when save-volatiles?
      (for-each store-volatile volatiles))
    (%make-asm volatiles #f end-address '() gc-inline)))

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
    (failure 'memory-ref "not a memory ~s" src))
   (else
    (jit-ldxi dst %fp (moffs src))
    dst)))

(define-syntax-rule (memory-ref/f dst src)
  (cond
   ((not (memory? src))
    (failure 'memory-ref/f "not a memory ~s" src))
   (else
    (jit-ldxi-d dst %fp (moffs src))
    dst)))

(define-syntax-rule (memory-set! dst src)
  (cond
   ((not (memory? dst))
    (failure 'memory-set! "not a memory ~s" dst))
   (else
    (jit-stxi (moffs dst) %fp src))))

(define-syntax-rule (memory-set!/f dst src)
  (cond
   ((not (memory? dst))
    (failure 'memory-set!/f "not a memory" dst))
   (else
    (jit-stxi-d (moffs dst) %fp src))))

(define-syntax-rule (movi dst src)
  (begin (jit-movi dst (con src))
         dst))

(define-syntax-rule (movi/f dst src)
  (begin (jit-movi-d dst (con src))
         dst))

(define-syntax-rule (movr dst src)
  (begin (jit-movr dst src)
         dst))

(define-syntax-rule (bailout)
  (asm-exit asm))

(define-syntax-rule (push-as-gpr arg overwritten?)
  (cond
   (overwritten?
    (jit-ldxi r1 %fp (volatile-offset arg))
    (jit-pushargr r1))
   (else
    (case (ref-type arg)
      ((con)
       (jit-pushargi (con arg)))
      ((gpr)
       (jit-pushargr (gpr arg)))
      ((fpr)
       (jit-pushargr (fpr->gpr r1 (fpr arg))))
      ((mem)
       (jit-pushargr (memory-ref r1 arg)))
      (else
       (failure 'push-as-gpr "unknown arg ~s" arg))))))

(define-syntax-rule (retval-to-reg-or-mem dst)
  (case (ref-type dst)
    ((gpr)
     (jit-retval (gpr dst)))
    ((fpr)
     (jit-retval r0)
     (gpr->fpr (fpr dst) r0))
    ((mem)
     (jit-retval r0)
     (memory-set! dst r0))
    (else
     (failure 'retval-to-reg-or-mem "unknown dst ~s" dst))))

;;;
;;; Type guards
;;;

(define-syntax guard-tc2
  (syntax-rules ()
    ((_ obj tc2)
     (jump (jit-bmci obj (imm tc2)) (bailout)))))

(define-syntax guard-tc8
  (syntax-rules ()
    ((_ obj tc8)
     (let ((typ8 (if (equal? obj r0) r1 r0)))
       (jit-andi typ8 obj (imm #xff))
       (jump (jit-bnei typ8 (imm tc8)) (bailout))))))

(define-syntax define-cell-guard
  (syntax-rules ()
    ((_ name mask)
     (define-syntax name
       (syntax-rules ()
         ((_ obj tcx)
          (let ((typx (if (equal? obj r0) r1 r0)))
            (name obj tcx typx)))
         ((_ obj tcx typx)
          (begin
            (jump (scm-imp obj) (bailout))
            (jit-ldr typx obj)
            (jit-andi typx typx (imm mask))
            (jump (jit-bnei typx (imm tcx)) (bailout)))))))))

(define-cell-guard guard-tc1 #x1)
(define-cell-guard guard-tc3 #x7)
(define-cell-guard guard-tc7 #x7f)
(define-cell-guard guard-tc16 #xffff)

(define-syntax guard-type
  (syntax-rules ()
    ((_ src type)
     (letrec-syntax
         ((err
           (syntax-rules ()
             ((_)
              (failure 'guard-type "~a ~a ~s"
                       (physical-name src) (pretty-type type)))))
          (guard-constant
           (syntax-rules ()
             ((_ constant)
              (jump (jit-bnei src constant) (bailout))))))
       (cond
        ((eq? type &fixnum) (guard-tc2 src %tc2-int))
        ((eq? type &flonum) (guard-tc16 src %tc16-real))
        ((eq? type &fraction) (guard-tc16 src %tc16-fraction))
        ((eq? type &char) (guard-tc8 src %tc8-char))
        ((eq? type &unspecified) (guard-constant *scm-unspecified*))
        ((eq? type &unbound) (guard-constant *scm-unspecified*))
        ((eq? type &undefined) (guard-constant *scm-undefined*))
        ((eq? type &false) (guard-constant *scm-false*))
        ((eq? type &true) (guard-constant *scm-true*))
        ((eq? type &nil) (guard-constant *scm-nil*))
        ((eq? type &null) (values))
        ((eq? type &symbol) (guard-tc7 src %tc7-symbol))
        ((eq? type &keyword) (guard-tc7 src %tc7-keyword))
        ((eq? type &procedure) (guard-tc7 src %tc7-program))
        ((eq? type &pointer) (guard-tc7 src %tc7-pointer))
        ((eq? type &fluid) (guard-tc7 src %tc7-fluid))
        ((eq? type &pair) (guard-tc1 src %tc3-cons))
        ((eq? type &vector) (guard-tc7 src %tc7-vector))
        ((eq? type &box) (guard-tc7 src %tc7-variable))
        ((eq? type &struct) (guard-tc3 src %tc3-struct))
        ((eq? type &string) (guard-tc7 src %tc7-string))
        ((eq? type &bytevector) (guard-tc7 src %tc7-bytevector))
        ((eq? type &bitvector) (guard-tc7 src %tc7-bitvector))
        ((eq? type &array) (guard-tc7 src %tc7-array))
        ((eq? type &hash-table) (guard-tc7 src %tc7-hashtable))
        ((memq type (list #f &scm &s64 &u64)) (values))
        (else (err)))))))


;;;;
;;;; Native operations
;;;;

;;;
;;; Guards
;;;

(define-syntax define-binary-guard-int
  (syntax-rules ()
    "Macro for defining guard primitive which takes two int arguments.

`define-binary-guard-int NAME OP-II OP-RI OP-RR' will define a primitive named
NAME. Uses OP-II when both arguments matched `con?' predicate. Uses OP-RI
when the first argument matched `gpr?'  or `memory?' predicate while the second
was constant. And, uses OP-RR when both arguments were register or memory."
    ((_ name op-ii op-ri op-rr)
     (define-native (name (int a) (int b))
       (case (ref-type a)
         ((con)
          (let ((rt (ref-type b)))
            (case rt
              ((con)
               (debug 1 ";;; [assembler:~a] a=~a b=~a~%" 'name a b))
              (else
               (jit-movi r0 (con a))
               (case rt
                 ((gpr) (jump (op-rr r0 (gpr b)) (bailout)))
                 ((fpr) (jump (op-rr r0 (fpr->gpr r1 (fpr b))) (bailout)))
                 ((mem) (jump (op-rr r0 (memory-ref r1 b)) (bailout)))
                 (else (err)))))))
         ((gpr)
          (case (ref-type b)
            ((con) (jump (op-ri (gpr a) (con b)) (bailout)))
            ((gpr) (jump (op-rr (gpr a) (gpr b)) (bailout)))
            ((fpr) (jump (op-rr (gpr a) (fpr->gpr r1 (fpr b))) (bailout)))
            ((mem) (jump (op-rr (gpr a) (memory-ref r1 b)) (bailout)))
            (else (err))))
         ((fpr)
          (fpr->gpr r0 (fpr a))
          (case (ref-type b)
            ((con) (jump (op-ri r0 (con b)) (bailout)))
            ((gpr) (jump (op-rr r0 (gpr b)) (bailout)))
            ((fpr) (jump (op-rr r0 (fpr->gpr r1 (fpr b))) (bailout)))
            ((mem) (jump (op-rr r0 (memory-ref r1 b)) (bailout)))
            (else (err))))
         ((mem)
          (memory-ref r0 a)
          (case (ref-type b)
            ((con) (jump (op-ri r0 (con b)) (bailout)))
            ((gpr) (jump (op-rr r0 (gpr b)) (bailout)))
            ((fpr) (jump (op-rr r0 (fpr->gpr r1 (fpr b))) (bailout)))
            ((mem) (jump (op-rr r0 (memory-ref r1 b)) (bailout)))
            (else (err))))
         (else
          (err)))))))

;; Auxiliary procedure for %ne.
(define (!= a b) (not (= a b)))

(define-binary-guard-int %eq != jit-bnei jit-bner)
(define-binary-guard-int %ne = jit-beqi jit-beqr)
(define-binary-guard-int %le > jit-bgti jit-bgtr)
(define-binary-guard-int %lt >= jit-bgei jit-bger)
(define-binary-guard-int %ge < jit-blti jit-bltr)
(define-binary-guard-int %gt <= jit-blei jit-bler)

(define-syntax define-binary-guard-double
  (syntax-rules ()
    ((_ name op-ii op-ri op-rr)
     (define-native (name (double a) (double b))
       (case (ref-type a)
         ((con)
          (jit-movi-d f0 (con a))
          (case (ref-type b)
            ((gpr) (jump (op-rr f0 (gpr->fpr f1 (gpr b))) (bailout)))
            ((fpr) (jump (op-rr f0 (fpr b)) (bailout)))
            ((mem) (jump (op-rr f0 (memory-ref/f f1 b)) (bailout)))
            (else (err))))
         ((gpr)
          (gpr->fpr f0 (gpr a))
          (case (ref-type b)
            ((con) (jump (op-ri f0 (con b)) (bailout)))
            ((gpr) (jump (op-rr f0 (gpr->fpr f1 (gpr b))) (bailout)))
            ((fpr) (jump (op-rr f0 (fpr b)) (bailout)))
            ((mem) (jump (op-rr f0 (memory-ref/f f1 b)) (bailout)))
            (else (err))))
         ((fpr)
          (case (ref-type b)
            ((con) (jump (op-ri (fpr a) (con b)) (bailout)))
            ((gpr) (jump (op-rr (fpr a) (gpr->fpr f1 (gpr b))) (bailout)))
            ((fpr) (jump (op-rr (fpr a) (fpr b)) (bailout)))
            ((mem) (jump (op-rr (fpr a) (memory-ref/f f1 b)) (bailout)))
            (else (err))))
         ((mem)
          (memory-ref/f f0 a)
          (case (ref-type b)
            ((con) (jump (op-ri f0 (con b)) (bailout)))
            ((gpr) (jump (op-rr f0 (gpr->fpr f1 (gpr b))) (bailout)))
            ((fpr) (jump (op-rr f0 (fpr b)) (bailout)))
            ((mem) (jump (op-rr f0 (memory-ref/f f1 b)) (bailout)))
            (else (err))))
         (else
          (err)))))))

(define-binary-guard-double %feq != jit-bnei-d jit-bner-d)
(define-binary-guard-double %fne = jit-beqi-d jit-beqr-d)
(define-binary-guard-double %flt >= jit-bgei-d jit-bger-d)
(define-binary-guard-double %fle > jit-bgti-d jit-bgtr-d)
(define-binary-guard-double %fgt <= jit-blei-d jit-bler-d)
(define-binary-guard-double %fge < jit-blti-d jit-bltr-d)

(define-native (%eqv (int a) (int b))
  (let ((volatiles (asm-volatiles asm))
        (proceed (jit-forward))
        (a/r (case (ref-type a)
               ((con) (movi r0 a))
               ((gpr) (gpr a))
               ((fpr) (fpr->gpr r0 (fpr a)))
               ((mem) (memory-ref r0 a))
               (else (err))))
        (b/r (case (ref-type b)
               ((con) (movi r1 b))
               ((gpr) (gpr b))
               ((fpr) (fpr->gpr r1 (fpr b)))
               ((mem) (memory-ref r1 b))
               (else (err)))))
    (jump (jit-beqr a/r b/r) proceed)
    (jump (scm-imp a/r) (bailout))
    (jump (scm-imp b/r) (bailout))
    (for-each store-volatile volatiles)
    (jit-prepare)
    (let ((b/r (if (equal? b (argr 1))
                   (movr r1 b/r)
                   b/r)))
      (jit-pushargr a/r)
      (jit-pushargr b/r))
    (jit-calli %scm-eqv)
    (jit-retval r0)
    (for-each load-volatile volatiles)
    (jump (jit-beqi r0 *scm-false*) (bailout))
    (jit-link proceed)))

(define-native (%nev (int a) (int b))
  (let ((volatiles (asm-volatiles asm))
        (proceed (jit-forward))
        (a/r (case (ref-type a)
               ((con) (movi r0 a))
               ((gpr) (gpr a))
               ((fpr) (fpr->gpr r0 (fpr a)))
               ((mem) (memory-ref r0 a))
               (else (err))))
        (b/r (case (ref-type b)
               ((con) (movi r1 b))
               ((gpr) (gpr b))
               ((fpr) (fpr->gpr r1 (fpr b)))
               ((mem) (memory-ref r1 b))
               (else (err)))))
    (jump (jit-beqr a/r b/r) (bailout))
    (jump (scm-imp a/r) proceed)
    (jump (scm-imp b/r) proceed)
    (for-each store-volatile volatiles)
    (jit-prepare)
    (let ((b/r (if (equal? b (argr 1))
                   (movr r1 b/r)
                   b/r)))
      (jit-pushargr a/r)
      (jit-pushargr b/r))
    (jit-calli %scm-eqv)
    (jit-retval r0)
    (for-each load-volatile volatiles)
    (jump (jit-bnei r0 *scm-false*) (bailout))
    (jit-link proceed)))

;;; Type guard.
(define-native (%typeq (int src) (void type))
  (let ((rt (ref-type src)))
    (unless (eq? 'con rt)
      (let ((reg (case rt
                   ((gpr) (gpr src))
                   ((fpr) (fpr->gpr r0 (fpr src)))
                   ((mem) (memory-ref r0 src)))))
        (guard-type reg (ref-value type))))))

;;; TC tag equal.
(define-native (%tceq (int src) (void mask) (void tag))
  (let ((typx r0)
        (obj (case (ref-type src)
               ((con) (movi r1 src))
               ((gpr) (gpr src))
               ((fpr) (fpr->gpr r1 (fpr src)))
               ((mem) (memory-ref r1 src))
               (else (err)))))
    (jump (scm-imp obj) (bailout))
    (jit-ldr typx obj)
    (jit-andi typx typx (con mask))
    (jump (jit-bnei typx (con tag)) (bailout))))

;;; TC tag not equal.
(define-native (%tcne (int src) (void mask) (void tag))
  (let ((typx r0)
        (obj (case (ref-type src)
               ((con) (movi r1 src))
               ((gpr) (gpr src))
               ((fpr) (fpr->gpr r1 (fpr src)))
               ((mem) (memory-ref r1 src))
               (else (err))))
        (proceed (jit-forward)))
    (jump (scm-imp obj) proceed)
    (jit-ldr typx obj)
    (jit-andi typx typx (con mask))
    (jump (jit-beqi typx (con tag)) (bailout))
    (jit-link proceed)))


;;;
;;; Call and return
;;;

;;; Scheme procedure call, shift current FP.
;;;
;;; Fill in the dynamic link, so that libguile/vm.c:scm_i_vm_mark_stack can keep
;;; on traversing the stack with SCM_FRAME_DYNAMIC_LINK, garbage collector may
;;; run while native code is running. Return address is not filled in at this
;;; momement, later filled in by bailout code with snapshot value.
(define-native (%scall (void proc))
  (let* ((vp r0)
         (vp->fp r1)
         (dl r2))
    (load-vp vp)
    (load-vp->fp vp->fp vp)
    (jit-subi vp->fp vp->fp (imm (* (ref-value proc) %word-size)))
    (jit-movi dl (con proc))
    (scm-frame-set-dynamic-link! vp->fp dl)
    (store-vp->fp vp vp->fp)))

;;; Return from Scheme procedure call. Shift current FP to the one from dynamic
;;; link. Guard with return address, checks whether it match with the IP used at
;;; the time of compilation.
(define-native (%return (void ra))
  (let ((vp r0)
        (vp->fp r1)
        (tmp r2))
    (when (not (con? ra))
      (failure '%return "got non-constant ra: ~s" ra))
    (load-vp vp)
    (load-vp->fp vp->fp vp)
    (scm-frame-return-address tmp vp->fp)
    (jump (jit-bnei tmp (con ra)) (bailout))

    (scm-frame-dynamic-link tmp vp->fp)
    (jit-lshi tmp tmp (imm %word-size-in-bits))
    (jit-addr vp->fp vp->fp tmp)
    (store-vp->fp vp vp->fp)))

;;; Prepare argument for calling C function.
(define-native (%carg (int arg))
  (set-asm-cargs! asm (cons arg (asm-cargs asm))))

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
              (failure '%ccall "not a primitive ~s" subr)))))
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
    (retval-to-reg-or-mem dst)
    (for-each (lambda (reg)
                (when (not (equal? reg dst))
                  (load-volatile reg)))
              volatiles)
    (load-vp r0)
    (vm-cache-sp r0)
    (set-asm-cargs! asm '())))


;;;
;;; Bitwise arithmetic
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
                     (case (ref-type b)
                       ((con) (op-ri dst a (con b)))
                       ((gpr) (op-rr dst a (gpr b)))
                       ((fpr) (op-rr dst a (fpr->gpr r1 (fpr b))))
                       ((mem) (op-rr dst a (memory-ref r1 b)))
                       (else (err))))))
            (op3a (syntax-rules ()
                    ((_ dst)
                     (begin
                       (case (ref-type a)
                         ((con) (op3b dst (movi r0 a)))
                         ((gpr) (op3b dst (gpr a)))
                         ((fpr) (op3b dst (fpr->gpr r0 (fpr a))))
                         ((mem) (op3b dst (memory-ref r0 a)))
                         (else (err)))
                       dst)))))
         (case (ref-type dst)
           ((gpr) (op3a (gpr dst)))
           ((fpr) (gpr->fpr (fpr dst) (op3a r0)))
           ((mem) (memory-set! dst (op3a r0)))
           (else (err))))))))

(define-binary-arith-int %band logand jit-andi jit-andr)
(define-binary-arith-int %bor logior jit-ori jit-orr)


;;;
;;; Integer arithmetic
;;;

(define (rsh a b) (ash a b))
(define (lsh a b) (ash a (- b)))

(define-binary-arith-int %add + jit-addi jit-addr)
(define-binary-arith-int %sub - jit-subi jit-subr)
(define-binary-arith-int %mul * jit-muli jit-mulr)
(define-binary-arith-int %rsh rsh jit-rshi jit-rshr)
(define-binary-arith-int %lsh lsh jit-lshi jit-lshr)

(define-native (%mod (int dst) (int a) (int b))
  (let ((neg-b (jit-forward))
        (proceed (jit-forward))
        (tmp/r r0)
        (a/r (case (ref-type a)
               ((con) (movi r0 a))
               ((gpr) (gpr a))
               ((fpr) (fpr->gpr r0 (fpr a)))
               ((mem) (memory-ref r0 a))))
        (b/r (case (ref-type b)
               ((con) (movi r1 b))
               ((gpr) (gpr b))
               ((fpr) (fpr->gpr r1 (fpr b)))
               ((mem) (memory-ref r1 b)))))

    (jit-remr tmp/r a/r b/r)

    ;; Adjust with checking sign of divisor and result.
    (jump (jit-blti b/r (imm 0)) neg-b)
    (jump (jit-bgei tmp/r (imm 0)) proceed)
    (jit-addr tmp/r tmp/r b/r)
    (jump (jit-jmpi) proceed)

    (jit-link neg-b)
    (jump (jit-blei tmp/r (imm 0)) proceed)
    (jit-addr tmp/r tmp/r b/r)

    (jit-link proceed)
    (case (ref-type dst)
      ((gpr) (jit-movr (gpr dst) tmp/r))
      ((fpr) (gpr->fpr (fpr dst) tmp/r))
      ((mem) (memory-set! dst tmp/r))
      (else (err)))))

(define-binary-arith-int %rem remainder jit-remi jit-remr)
(define-binary-arith-int %quo modulo jit-divi jit-divr)

;; Integer multiplication, with overflow check.
(define-native (%mulov (int dst) (int a) (int b))
  (let ((tmp r1)
        (proceed (jit-forward)))
    (letrec-syntax
        ((op3b (syntax-rules ()
                 ((_ dst a)
                  (begin
                    (case (ref-type b)
                      ((con) (jit-qmuli dst tmp a (con b)))
                      ((gpr) (jit-qmulr dst tmp a (gpr b)))
                      ((fpr) (jit-qmulr dst tmp a (fpr->gpr r1 (fpr b))))
                      ((mem) (jit-qmulr dst tmp a (memory-ref r1 b)))
                      (else (err)))))))
         (op3a (syntax-rules ()
                 ((_ dst)
                  (begin
                    (case (ref-type a)
                      ((con) (op3b dst (movi r0 a)))
                      ((gpr) (op3b dst (gpr a)))
                      ((fpr) (op3b dst (fpr->gpr r0 (fpr a))))
                      ((mem) (op3b dst (memory-ref r0 a)))
                      (else (err)))
                    dst)))))
      (case (ref-type dst)
        ((gpr) (op3a (gpr dst)))
        ((fpr) (gpr->fpr (fpr dst) (op3a r0)))
        ((mem) (memory-set! dst (op3a r0)))
        (else (err)))
      (jump (jit-beqi tmp (make-negative-pointer -1)) proceed)
      (jump (jit-bnei tmp (imm 0)) (bailout))
      (jit-link proceed))))

;; Integer division, returns SCM value with fraction type when remainder is not
;; zero.
(define-native (%div (int dst) (int a) (int b))
  (let ((tmp1-offset (moffs (make-memory -4)))
        (tmp2-offset (moffs (make-memory -5)))
        (make-fraction (jit-forward))
        (make-fixnum (jit-forward))
        (proceed (jit-forward)))
    (let ((a/r (case (ref-type a)
                 ((con) (movi r1 a))
                 ((gpr) (gpr a))
                 ((fpr) (fpr->gpr r1 (fpr a)))
                 ((mem) (memory-ref r1 a))
                 (else (err))))
          (b/r (case (ref-type b)
                 ((con) (movi r2 b))
                 ((gpr) (gpr b))
                 ((fpr) (fpr->gpr r2 (fpr b)))
                 ((mem) (memory-ref r2 b))
                 (else (err))))
          (dst/r (case (ref-type dst)
                   ((gpr) (gpr dst))
                   (else r0))))

      ;; Call div instruction, jump to proceed if remainder is 0.
      (jit-stxi tmp1-offset %fp a/r)
      (jit-stxi tmp2-offset %fp b/r)
      (jit-qdivr dst/r a/r a/r b/r)
      (jump (jit-beqi a/r (imm 0)) make-fixnum)

      ;; Allocate memory for SCM fraction value. Push %tc16-fraction as cell
      ;; header, fill in numerator, denominator and 0 to rest of the allocated
      ;; memory.
      (jit-prepare)
      (let ((volatiles (asm-volatiles asm)))
        (for-each store-volatile volatiles)
        (when (asm-gc-inline? asm)
          (jit-pushargr %thread))
        (jit-pushargi (imm %tc16-fraction))
        (jit-pushargi (imm 4))
        (if (asm-gc-inline? asm)
            (jit-calli %scm-inline-words)
            (jit-calli %scm-words))
        (jit-retval dst/r)
        (for-each (lambda (reg)
                    (unless (equal? reg dst)
                      (load-volatile reg)))
                  volatiles)
        (jit-ldxi a/r %fp tmp1-offset)
        (jit-lshi a/r a/r (imm 2))
        (jit-addi a/r a/r (imm 2))
        (jit-stxi (imm %word-size) dst/r a/r)
        (jit-ldxi b/r %fp tmp2-offset)
        (jit-lshi b/r b/r (imm 2))
        (jit-addi b/r b/r (imm 2))
        (jit-stxi (imm (* 2 %word-size)) dst/r b/r)
        (jit-stxi (imm (* 3 %word-size)) dst/r (movi b/r 0)))
      (jump proceed)

      ;; Adjust fixnum.
      (jit-link make-fixnum)
      (jit-lshi dst/r dst/r (imm 2))
      (jit-addi dst/r dst/r (imm 2))

      ;; Move result to dst.
      (jit-link proceed)
      (case (ref-type dst)
        ((gpr) (values))
        ((fpr) (gpr->fpr (fpr dst) dst/r))
        ((mem) (memory-set! dst dst/r))
        (else (err))))))


;;;
;;; Floating point arithmetic
;;;

(define-syntax define-binary-arith-double
  (syntax-rules ()
    ((k name op-ri op-rr)
     (define-native (name (double dst) (double a) (double b))
       (letrec-syntax
           ((op3b (syntax-rules ()
                    ((_ dst a)
                     (case (ref-type b)
                       ((con) (op-rr dst a (movi/f f0 b)))
                       ((gpr) (op-rr dst a (gpr->fpr f0 (gpr b))))
                       ((fpr) (op-rr dst a (fpr b)))
                       ((mem) (op-rr dst a (memory-ref/f f0 b)))
                       (else (err))))))
            (op3a (syntax-rules ()
                    ((_ dst)
                     (begin
                       (case (ref-type a)
                         ((con) (op3b dst (movi/f f1 a)))
                         ((gpr) (op3b dst (gpr->fpr f1 (gpr a))))
                         ((fpr) (op3b dst (fpr a)))
                         ((mem) (op3b dst (memory-ref/f f1 a)))
                         (else (err)))
                       dst)))))
         (case (ref-type dst)
           ((gpr) (fpr->gpr (gpr dst) (op3a f2)))
           ((fpr) (op3a (fpr dst)))
           ((mem) (memory-set!/f dst (op3a f2)))
           (else (err))))))))

(define-binary-arith-double %fadd jit-addi-d jit-addr-d)
(define-binary-arith-double %fsub jit-subi-d jit-subr-d)
(define-binary-arith-double %fmul jit-muli-d jit-mulr-d)
(define-binary-arith-double %fdiv jit-divi-d jit-divr-d)


;;;
;;; Load and store
;;;

;;; XXX: Not sure whether it's better to couple `xxx-ref' and `xxx-set!'
;;; instructions with expected type as done in bytecode, to have vector-ref,
;;; struct-ref, box-ref, string-ref, fluid-ref, bv-u8-ref ... etc or not. When
;;; instructions specify its operand type, size of ANF will be more compact, but
;;; may loose chances to optimize away type checking instructions.

(define (unbox-stack-element dst src type)
  (if (eq? type &flonum)
      (case (ref-type dst)
        ((gpr)
         (scm-real-value f0 src)
         (fpr->gpr (gpr dst) f0))
        ((fpr)
         (scm-real-value (fpr dst) src))
        ((mem)
         (scm-real-value f0 src)
         (memory-set!/f dst f0))
        (else
         (failure 'unbox-stack-element "~s ~s ~s" dst src type)))
      (case (ref-type dst)
        ((gpr) (jit-movr (gpr dst) src))
        ((fpr) (gpr->fpr (fpr dst) src))
        ((mem) (memory-set! dst src))
        (else
         (failure 'unbox-stack-element "~s ~s ~s" dst src type)))))

;; Type check local N with TYPE and load to gpr or memory DST.
(define-native (%fref (int dst) (void n) (void type))
  (let ((tref (ref-value type)))
    (when (or (not (con? n))
              (not (con? type)))
      (err))
    (sp-ref r0 (ref-value n))
    (unbox-stack-element dst r0 tref)))

;; Load frame local to fpr or memory, with type check. This primitive
;; is used for loading floating point number to FPR.
(define-native (%fref/f (double dst) (void n) (void type))
  (let ((exit (jit-forward))
        (next (jit-forward))
        (t (ref-value type)))
    (when (or (not (con? n))
              (not (con? type)))
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
(define-native (%cref (int dst) (int src) (int n))
  (let-syntax
      ((op3const
        (syntax-rules ()
          ((_ dst)
           (let ((nw (* (ref-value n) %word-size)))
             (case (ref-type src)
               ((con) (jit-ldi dst (imm (+ (ref-value src) nw))))
               ((gpr)
                (if (zero? nw)
                    (jit-ldr dst (gpr src))
                    (jit-ldxi dst (gpr src) (imm nw))))
               ((fpr)
                (if (zero? nw)
                    (jit-ldr dst (fpr->gpr r0 (fpr src)))
                    (jit-ldxi dst (fpr->gpr r0 (fpr src)) (imm nw))))
               ((mem)
                (if (zero? nw)
                    (jit-ldr dst (memory-ref r0 src))
                    (jit-ldxi dst (memory-ref r0 src) (imm nw))))
               (else (err)))
             dst))))
       (op3reg
        (syntax-rules ()
          ((_ dst)
           (begin
             (case (ref-type src)
               ((con) (jit-ldxi dst r0 (imm (ref-value src))))
               ((gpr) (jit-ldxr dst (gpr src) r0))
               ((fpr) (jit-ldxr dst (fpr->gpr r1 (fpr src)) r0))
               ((mem) (jit-ldxr dst (memory-ref r1 src) r0))
               (else (err)))
             dst)))))
    (case (ref-type n)
      ((con)
       (case (ref-type dst)
         ((gpr) (op3const (gpr dst)))
         ((fpr) (gpr->fpr (fpr dst) (op3const r0)))
         ((mem) (memory-set! dst (op3const r0)))
         (else (err))))
      ((gpr)
       (jit-lshi r0 (gpr n) (imm %word-size-in-bits))
       (case (ref-type dst)
         ((gpr) (op3reg (gpr dst)))
         ((fpr) (gpr->fpr (fpr dst) (op3reg r1)))
         ((mem) (memory-set! dst (op3reg r1)))
         (else (err))))
      ((mem)
       (jit-lshi r0 (memory-ref r0 n) (imm %word-size-in-bits))
       (case (ref-type dst)
         ((gpr) (op3reg (gpr dst)))
         ((fpr) (gpr->fpr (fpr dst) (op3reg r1)))
         ((mem) (memory-set! dst (op3reg r1)))
         (else (err))))
      (else
       (err)))))

;; Cell reference to floating point register.
(define-native (%cref/f (double dst) (int src) (void n))
  (let ((nw (constant-word n))
        (static-address
         (lambda ()
           (imm (+ (ref-value src) (* (ref-value n) %word-size))))))
    (case (ref-type dst)
      ((gpr)
       (case (ref-type src)
         ((con) (jit-ldi-d f0 (static-address)))
         ((gpr) (jit-ldxi-d f0 (gpr src) nw))
         ((fpr) (jit-ldxi-d f0 (fpr->gpr r0 (fpr src)) nw))
         ((mem) (jit-ldxi-d f0 (memory-ref r0 src) nw))
         (else (err)))
       (fpr->gpr (gpr dst) f0))
      ((fpr)
       (case (ref-type src)
         ((con) (jit-ldi-d (fpr dst) (static-address)))
         ((gpr) (jit-ldxi-d (fpr dst) (gpr src) nw))
         ((fpr) (jit-ldxi-d (fpr dst) (fpr->gpr r0 (fpr src)) nw))
         ((mem) (jit-ldxi-d (fpr dst) (memory-ref r0 src) nw))
         (else (err))))
      ((mem)
       (case (ref-type src)
         ((con) (jit-ldi-d f0 (static-address)))
         ((gpr) (jit-ldxi-d f0 (gpr src) nw))
         ((fpr) (jit-ldxi-d f0 (fpr->gpr r0 (fpr src)) nw))
         ((mem) (jit-ldxi-d f0 (memory-ref r0 src) nw))
         (else (err)))
       (memory-set!/f dst f0))
      (else (err)))))

;; Unsigned char ref.
(define-native (%u8ref (int dst) (int src) (int n))
  (let ((dst/r (case (ref-type dst)
                 ((gpr) (gpr dst))
                 ((fpr) (fpr->gpr r1 (fpr dst)))
                 ((mem) (memory-ref r1 dst))
                 (else (err))))
        (src/r (case (ref-type src)
                 ((gpr) (gpr src))
                 ((fpr) (fpr->gpr r2 (fpr src)))
                 ((mem) (memory-ref r2 src))
                 (else (err)))))
    (case (ref-type n)
      ((con)
       (jit-ldxi-uc dst/r src/r (con n)))
      (else
       (let ((n/r (case (ref-type n)
                    ((gpr) (gpr n))
                    ((fpr) (fpr->gpr r0 (fpr n)))
                    ((mem) (memory-ref r0 n))
                    (else (err)))))
         (jit-ldxr-uc dst/r src/r n/r))))
    (case (ref-type dst)
      ((gpr) (values))
      ((fpr) (gpr->fpr (fpr dst) dst/r))
      ((mem) (memory-set! dst dst/r))
      (else (err)))))

;; Set cell object. This operation uses `r2' register when the argument `cell'
;; was memory and argument `n' was not constant.
(define-native (%cset (int cell) (int n) (int src))
  (letrec-syntax
      ((op3a
        (syntax-rules ()
          ((_ dst)
           (let ((nw (* (ref-value n) %word-size)))
             (case (ref-type src)
               ((con) (jit-stxi (imm nw) dst (movi r1 src)))
               ((gpr) (jit-stxi (imm nw) dst (gpr src)))
               ((fpr) (jit-stxi (imm nw) dst (fpr->gpr r1 (fpr src))))
               ((mem) (jit-stxi (imm nw) dst (memory-ref r1 src)))
               (else (err)))))))
       (op3b (syntax-rules ()
               ((_ dst)
                (case (ref-type src)
                  ((con) (jit-stxr r0 dst (movi r1 src)))
                  ((gpr) (jit-stxr r0 dst (gpr src)))
                  ((fpr) (jit-stxr r0 dst (fpr->gpr r1 (fpr src))))
                  ((mem) (jit-stxr r0 dst (memory-ref r1 src)))))))
       (op3c (syntax-rules ()
               ((_ dst-addr)
                (jit-sti (imm dst-addr)
                         (case (ref-type src)
                           ((con) (movi r1 src))
                           ((gpr) (gpr src))
                           ((fpr) (fpr->gpr r1 (fpr src)))
                           ((mem) (memory-ref r1 src))))))))
    (case (ref-type n)
      ((con)
       (case (ref-type cell)
         ((con) (op3c (+ (* (ref-value n) %word-size)
                         (ref-value cell))))
         ((gpr) (op3a (gpr cell)))
         ((fpr) (op3a (fpr->gpr r0 (fpr cell))))
         ((mem) (op3a (memory-ref r0 cell)))
         (else (err))))
      ((gpr)
       (jit-lshi r0 (gpr n) (imm %word-size-in-bits))
       (case (ref-type cell)
         ((gpr) (op3b (gpr cell)))
         ((fpr) (op3b (fpr->gpr r2 (fpr cell))))
         ((mem) (op3b (memory-ref r2 cell)))
         (else (err))))
      ((mem)
       (memory-ref r0 n)
       (jit-lshi r0 r0 (imm %word-size-in-bits))
       (case (ref-type cell)
         ((gpr) (op3b (gpr cell)))
         ((fpr) (op3b (fpr->gpr r2 (fpr cell))))
         ((mem) (op3b (memory-ref r2 cell)))
         (else (err))))
      (else (err)))))

;; Unsigned char set.
(define-native (%u8set (int dst) (int n) (int src))
  (let ((dst/r (case (ref-type dst)
                 ((gpr) (gpr dst))
                 ((fpr) (fpr->gpr r2 (fpr dst)))
                 ((mem) (memory-ref r2 dst))
                 (else (err))))
        (src/r (case (ref-type src)
                 ((gpr) (gpr src))
                 ((fpr) (fpr->gpr r1 (fpr src)))
                 ((mem) (memory-ref r1 src))
                 (else (err)))))
    (case (ref-type n)
      ((con)
       (jit-stxi-c (con n) dst/r src/r))
      (else
       (let ((n/r (case (ref-type n)
                    ((gpr) (gpr n))
                    ((fpr) (fpr->gpr r0 (fpr n)))
                    ((mem) (memory-ref r0 n))
                    (else (err)))))
         (jit-stxr-c n/r dst/r src/r))))))

;; Fill memory from DST for N words with SRC. The address of DST itself is not
;; filled, use %cset for such case.
;;
;; XXX: Cannot use `r2' for `n' and 'src', since r2 is hard coded to `dst'.
;;
(define-native (%fill (int dst) (int n) (int src))
  (let ((l0 (jit-forward))
        (dst/r (case (ref-type dst)
                 ((gpr) (gpr dst))
                 ((fpr) (fpr->gpr r2 (fpr dst)))
                 ((mem) (memory-ref r2 dst))
                 (else (err))))
        (n/r (case (ref-type n)
               ((con) (movi r0 n))
               ((gpr) (jit-movr r0 (gpr n)) r0)
               ((fpr) (fpr->gpr r0 (fpr n)))
               ((mem) (memory-ref r0 n))
               (else (err))))
        (src/r (case (ref-type src)
                 ((con) (movi r1 src))
                 ((gpr) (gpr src))
                 ((fpr) (fpr->gpr r1 (fpr src)))
                 ((mem) (memory-ref r1 src))
                 (else (err)))))
    (jit-lshi n/r n/r (imm %word-size-in-bits))
    (jit-link l0)
    (jit-stxr n/r dst/r src/r)
    (jit-subi n/r n/r (imm %word-size))
    (jump (jit-bgti n/r (imm 0)) l0)
    (case (ref-type dst)
      ((gpr) (values))
      ((fpr) (gpr->fpr (fpr dst) dst/r))
      ((mem) (memory-set! dst dst/r))
      (else (err)))))


;;;
;;; Heap objects
;;;

;; Call C function `scm_do_inline_cell'. Save volatile registers before calling,
;; restore after getting returned value.
(define-native (%cell (int dst) (int x) (int y))
  (let ((volatiles (asm-volatiles asm))
        (x-overwritten? (equal? x (argr 1)))
        (y-overwritten? (or (equal? y (argr 1))
                            (equal? y (argr 2)))))
    (for-each store-volatile volatiles)
    (jit-prepare)
    (when (asm-gc-inline? asm)
      (jit-pushargr %thread))
    (push-as-gpr x x-overwritten?)
    (push-as-gpr y y-overwritten?)
    (if (asm-gc-inline? asm)
        (jit-calli %scm-inline-cell)
        (jit-calli %scm-cell))
    (retval-to-reg-or-mem dst)
    (for-each (lambda (reg)
                (when (not (equal? reg dst))
                  (load-volatile reg)))
              volatiles)))

;; Allocate words for n bytes, store to dst, fill head with a.
(define-native (%words (int dst) (int a) (int n))
  (let ((volatiles (asm-volatiles asm))
        (a-overwritten? (equal? a (argr 1)))
        (n-overwritten? (or (equal? n (argr 1))
                            (equal? n (argr 2)))))
    (for-each store-volatile volatiles)
    (jit-prepare)
    (when (asm-gc-inline? asm)
      (jit-pushargr %thread))
    (push-as-gpr a a-overwritten?)
    (push-as-gpr n n-overwritten?)
    (if (asm-gc-inline? asm)
        (jit-calli %scm-inline-words)
        (jit-calli %scm-words))
    (retval-to-reg-or-mem dst)
    (for-each (lambda (reg)
                (unless (equal? reg dst)
                  (load-volatile reg)))
              volatiles)))


;;;
;;; Type conversion
;;;

;; Integer -> floating point
(define-native (%i2d (double dst) (int src))
  (let ((con->double (lambda (x)
                       (scm->pointer (exact->inexact (ref-value x))))))
    (case (ref-type dst)
      ((gpr)
       (case (ref-type src)
         ((con) (jit-movi-d f0 (con->double src)))
         ((gpr) (jit-extr-d f0 (gpr src)))
         ((fpr) (jit-extr-d f0 (fpr->gpr r0 (fpr src))))
         ((mem) (jit-extr-d f0 (memory-ref r0 src)))
         (else (err)))
       (fpr->gpr (gpr dst) f0))
      ((fpr)
       (case (ref-type src)
         ((con) (jit-movi-d (fpr dst) (con->double src)))
         ((gpr) (jit-extr-d (fpr dst) (gpr src)))
         ((fpr) (jit-extr-d (fpr dst) (fpr->gpr r0 (fpr src))))
         ((mem) (jit-extr-d (fpr dst) (memory-ref r0 src)))
         (else (err))))
      ((mem)
       (case (ref-type src)
         ((con) (jit-movi-d f0 (con->double src)))
         ((gpr) (jit-extr-d f0 (gpr src)))
         ((fpr) (jit-extr-d f0 (fpr->gpr r0 (fpr src))))
         ((mem) (jit-extr-d f0 (memory-ref r0 src)))
         (else (err)))
       (memory-set!/f dst f0))
      (else (err)))))

;; Floating point -> SCM
(define-native (%d2s (int dst) (double src))
  (let-syntax
      ((op3
        (syntax-rules ()
          ((_ dst)
           (begin
             (case (ref-type src)
               ((con) (begin (jit-movi-d f0 (con src))
                             (scm-from-double dst f0)))
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


;;;
;;; Move
;;;

(define (move dst src)
  (let-syntax ((err (syntax-rules ()
                      ((_) (failure 'move "~a ~a"
                                    (physical-name dst)
                                    (physical-name src))))))
    (case (ref-type dst)
      ((gpr)
       (case (ref-type src)
         ((con) (let ((val (ref-value src)))
                  (cond
                   ((flonum? val)
                    (jit-movi-d f0 (con src))
                    (fpr->gpr (gpr dst) f0))
                   (else
                    (jit-movi (gpr dst) (con src))))))
         ((gpr) (jit-movr (gpr dst) (gpr src)))
         ((fpr) (fpr->gpr (gpr dst) (fpr src)))
         ((mem) (memory-ref (gpr dst) src))
         (else (err))))
      ((fpr)
       (case (ref-type src)
         ((con) (let ((val (ref-value src)))
                  (cond
                   ((flonum? val)
                    (jit-movi-d (fpr dst) (con src)))
                   (else
                    (jit-movi r0 (con src))
                    (gpr->fpr (fpr dst) r0)))))
         ((gpr) (gpr->fpr (fpr dst) (gpr src)))
         ((fpr) (jit-movr-d (fpr dst) (fpr src)))
         ((mem) (memory-ref/f (fpr dst) src))
         (else (err))))
      ((mem)
       (case (ref-type src)
         ((con) (let ((val (ref-value src)))
                  (cond
                   ((flonum? val)
                    (jit-movi-d f0 (con src))
                    (memory-set!/f dst f0))
                   (else
                    (jit-movi r0 (con src))
                    (memory-set! dst r0)))))
         ((gpr) (memory-set! dst (gpr src)))
         ((fpr) (memory-set!/f dst (fpr src)))
         ((mem) (memory-set! dst (memory-ref r0 src)))
         (else (err))))
      (else
       (err)))))

(define-native (%move (int dst) (int src))
  (move dst src))

(define-native (%cont (int dst))
  (let ((volatiles (asm-volatiles asm)))
    (for-each store-volatile volatiles)
    (jit-prepare)
    (jit-pushargr %thread)
    (load-vp r0)
    (jit-pushargr r0)
    (jit-calli %scm-make-continuation)
    (retval-to-reg-or-mem dst)
    (for-each (lambda (reg)
                (unless (equal? reg dst)
                  (load-volatile reg)))
              volatiles)))
