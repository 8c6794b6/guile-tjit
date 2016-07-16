;;;; Assembler for VM tjit engine

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
;;; Assembler for native code used in vm-tjit engine.
;;;
;;; Code:

(define-module (language trace assembler)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (language trace error)
  #:use-module (language trace parameters)
  #:use-module (language trace ra)
  #:use-module (language trace registers)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables)
  #:export (asm ; syntax parameter
            make-asm
            set-asm-exit!
            asm-end-address
            asm-cargs set-asm-cargs!
            asm-volatiles
            asm-gc-inline?
            asm-snapshots

            *scm-false* *scm-true* *scm-undefined*
            *scm-unspecified* *scm-null*
            make-signed-pointer
            %word-size-in-bits
            constant-word
            gpr->fpr fpr->gpr
            movi/r movi/f movr/r move
            jump jmpa
            sp-ref sp-set! sp-ref/f sp-set!/f
            memory-ref memory-set! memory-ref/f memory-set!/f
            scm-frame-dynamic-link scm-frame-set-dynamic-link!
            scm-frame-return-address scm-frame-set-return-address!
            scm-from-double scm-real-value
            scm-imp
            registers-offset
            with-volatiles  push-as-gpr retval-to-reg-or-mem
            load-vp store-vp
            load-vp->fp store-vp->fp
            vm-cache-sp
            vm-sync-ip vm-sync-sp vm-sync-fp
            vm-handle-interrupts
            vm-expand-stack
            store-stack unbox-stack-element
            bailout err guard-type guard-tc16

            prim-procedures-ref
            prim-types-ref
            reverse-native-prim-alist!
            define-native))


;;;; Scheme constants

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


;;;; SCM macros

(define (make-signed-pointer addr)
  (if (<= 0 addr)
      (make-pointer addr)
      (make-negative-pointer addr)))

(define-syntax-rule (constant-word i)
  (imm (* (ref-value i) %word-size)))

(define %word-size-in-bits
  (inexact->exact (/ (log %word-size) (log 2))))

(define-syntax-rule (scm-real-value dst src)
  (begin
    (jit-ldxi-d dst src (imm (* 2 %word-size)))
    dst))

(define %scm-from-double
  (dynamic-pointer "scm_from_double" (dynamic-link)))

(define %scm-inline-from-double
  (dynamic-pointer "scm_do_inline_from_double" (dynamic-link)))

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

(define-syntax-rule (volatile-offset reg)
  (let ((n (case (ref-type reg)
             ((gpr) (+ 2 (- (ref-value reg) *num-non-volatiles*)))
             ((fpr) (+ 2 1 (ref-value reg) *num-volatiles*))
             (else (failure 'volatile-offset "~s" reg)))))
    (make-negative-pointer (* (- n) %word-size))))

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

(define-syntax load-volatile-with-new-dst
  (syntax-rules ()
    ((_ dst #f)
     (case (ref-type dst)
       ((gpr) (jit-ldxi (gpr dst) %fp (volatile-offset dst)))
       ((fpr) (jit-ldxi-d (fpr dst) %fp (volatile-offset dst)))
       (else (failure 'load-volatile-with-new-dst "~s" dst))))
    ((_ dst new-dst)
     (unless (equal? dst new-dst)
       (case (ref-type dst)
         ((gpr) (jit-ldxi (gpr dst) %fp (volatile-offset dst)))
         ((fpr) (jit-ldxi-d (fpr dst) %fp (volatile-offset dst)))
         (else (failure 'load-volatile-with-new-dst "~s" dst)))))))

(define-syntax-rule (with-volatiles volatiles new-dst body ...)
  (begin
    (do ((ss volatiles (cdr ss)))
        ((null? ss))
      (let ((src (car ss)))
        (case (ref-type src)
          ((gpr) (jit-stxi (volatile-offset src) %fp (gpr src)))
          ((fpr) (jit-stxi-d (volatile-offset src) %fp (fpr src)))
          (else (failure 'store-volatiles "~s" src)))))
    body ...
    (do ((ds volatiles (cdr ds)))
        ((null? ds))
      (let ((dst (car ds)))
        (load-volatile-with-new-dst dst new-dst)))))

(define-syntax scm-from-double
  (syntax-rules ()
    ((_  dst src)
     (let ((volatiles (asm-volatiles asm)))
       (do ((vs volatiles (cdr vs)))
           ((null? vs))
         (store-volatile (car vs)))
       (jit-prepare)
       (when (asm-gc-inline? asm)
         (jit-pushargr %thread))
       (jit-pushargr-d src)
       (if (asm-gc-inline? asm)
           (jit-calli %scm-inline-from-double)
           (jit-calli %scm-from-double))
       (jit-retval dst)
       (do ((regs volatiles (cdr regs)))
           ((null? regs))
         (let ((reg (car regs)))
           (unless (case (ref-type reg)
                     ((fpr) (equal? (fpr reg) dst))
                     ((gpr) (equal? (gpr reg) dst))
                     (else #f))
             (load-volatile reg))))))))

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
    (with-volatiles volatiles #f
      (jit-prepare)
      (jit-calli %scm-async-tick)
      (load-vp r0)
      (vm-cache-sp r0))
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

    (with-volatiles volatiles #f
      (jit-prepare)
      (jit-pushargr vp)
      (jit-pushargr %sp)
      (jit-calli %scm-vm-expand-stack))
    (load-vp vp)
    (vm-cache-sp vp)
    (jump next)

    (jit-link set-min-since-gc)
    (vm-set-sp-min-since-gc! vp %sp)

    (jit-link sync-sp)
    (vm-sync-sp %sp vp)

    (jit-link next)))


;;;; Predicates

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


;;;; Assembler state

(define-record-type <asm>
  (%make-asm volatiles exit end-address cargs gc-inline? snapshots)
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
  (gc-inline? asm-gc-inline?)

  ;; Snapshots, used when capturing stack.
  (snapshots asm-snapshots))

(define-syntax-parameter asm
  (lambda (x)
    (syntax-violation 'asm "asm used outside of primitive definition" x)))

(define-syntax-parameter err
  (lambda (x)
    (syntax-violation 'err "err used outside of primitive definition" x)))

(define (make-asm storage end-address gc-inline save-volatiles? snapshots)
  (define (add-volatile k reg acc)
    (if (or (and reg (gpr? reg)
                 (<= *num-non-volatiles* (ref-value reg)))
            (and reg (fpr? reg)
                 (<= 0 (ref-value reg))))
        (cons reg acc)
        acc))
  (let ((volatiles (fold-storage add-volatile '() storage)))
    (when save-volatiles?
      (for-each store-volatile volatiles))
    (%make-asm volatiles #f end-address '() gc-inline snapshots)))

(define-syntax jump
  (syntax-rules ()
    ((_ label)
     (jit-patch-at (jit-jmpi) label))
    ((_ condition label)
     (jit-patch-at condition label))))

(define-syntax jmpa
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

(define-syntax-rule (movi/r dst src)
  (begin (jit-movi dst (con src))
         dst))

(define-syntax-rule (movi/f dst src)
  (begin (jit-movi-d dst (con src))
         dst))

(define-syntax-rule (movr/r dst src)
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


;;;; Type guards

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


;;;; Move

(define-inlinable (move dst src)
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

(define-syntax-rule (store-stack local type src)
  (let ((err (lambda ()
               (failure 'store-stack "~s ~a ~s"
                        local (pretty-type type) src))))
    (cond
     ;; Refilled immediates
     ((eq? type &false)
      (jit-movi r0 *scm-false*)
      (sp-set! local r0))
     ((eq? type &undefined)
      (jit-movi r0 *scm-undefined*)
      (sp-set! local r0))
     ((eq? type &unspecified)
      (jit-movi r0 *scm-unspecified*)
      (sp-set! local r0))
     ;; Types containing value
     ((return-address? type)
      ;; Moving value coupled with type to frame local. Return address of VM
      ;; frame need to be recovered when taking exit from inlined procedure
      ;; call. The actual value for return address is captured at the time of
      ;; Scheme IR conversion and stored in snapshot.
      (jit-movi r0 (imm (return-address-ip type)))
      (sp-set! local r0))
     ((dynamic-link? type)
      ;; Storing dynamid link to local. Dynamic link record type contains offset
      ;; in the field. VM may use different value at the time of compilation and
      ;; execution.
      (jit-movi r0 (imm (dynamic-link-offset type)))
      (sp-set! local r0))
     ((constant? type)
      ;; Value of constant could be flonum. In such case, using `scm->pointer'
      ;; to store the SCM representation.
      (let ((val (constant-value type)))
        (if (flonum? val)
            (jit-movi r0 (scm->pointer val))
            (jit-movi r0 (imm val)))
        (sp-set! local r0)))
     ;; Unboxed flonum values
     ((eq? type &flonum)
      (case (ref-type src)
        ((con)
         (jit-movi-d f0 (con src))
         (scm-from-double r0 f0)
         (sp-set! local r0))
        ((gpr)
         (gpr->fpr f0 (gpr src))
         (scm-from-double r0 f0)
         (sp-set! local r0))
        ((fpr)
         (scm-from-double r0 (fpr src))
         (sp-set! local r0))
        ((mem)
         (memory-ref/f f0 src)
         (scm-from-double r0 f0)
         (sp-set! local r0))
        (else (err))))
     ;; Raw f64 value
     ((eq? type &f64)
      (case (ref-type src)
        ((con)
         (jit-movi-d f0 (con src))
         (sp-set!/f local f0))
        ((gpr)
         (gpr->fpr f0 (gpr src))
         (sp-set!/f local f0))
        ((fpr)
         (sp-set!/f local (fpr src)))
        ((mem)
         (memory-ref/f f0 src)
         (sp-set!/f local f0))
        (else (err))))
     ;; Missing source code, do nothing.
     ((not src) (values))
     ;; All the other values
     (else
      (case (ref-type src)
        ((con)
         (jit-movi r0 (con src))
         (sp-set! local r0))
        ((gpr)
         (sp-set! local (gpr src)))
        ((fpr)
         (fpr->gpr r0 (fpr src))
         (sp-set! local r0))
        ((mem)
         (memory-ref r0 src)
         (sp-set! local r0))
        (else (err)))))))


;;;; Macro for defining primitives

;;; Primitives used for vm-tjit engine.  Primitives defined here are used during
;;; compilation of traced data to native code. Perhaps useless for ordinal use
;;; as scheme procedure, except for managing docstring.
;;;
;;; Need at least 3 general purpose scratch registers, and 3 floating point
;;; scratch registers. Currently using R0, R1, and R2 for general purpose, F0,
;;; F1, and F2 for floating point.

(define *native-prim-procedures* '())

(define (prim-procedures-ref key)
  (assq-ref *native-prim-procedures* key))

(define *native-prim-types* '())

(define (prim-types-ref key)
  (assq-ref *native-prim-types* key))

(define (reverse-native-prim-alist!)
  (set! *native-prim-procedures* (reverse! *native-prim-procedures*))
  (set! *native-prim-types* (reverse! *native-prim-types*)))

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
              (err (syntax-rules ()
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
       (set! *native-prim-procedures*
         (acons 'name name *native-prim-procedures*))
       (set! *native-prim-types*
         (acons 'name `(,ty ...) *native-prim-types*))))))
