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

;;; A virtual machine with method JIT compiler from bytecode to native
;;; code. Compiler is written in scheme, with GNU Lightning.

;;; Code:

(define-module (system vm lightning)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (language bytecode)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm lightning binding)
  #:use-module (system vm lightning debug)
  #:use-module (system vm lightning cfg)
  #:use-module (system vm program)
  #:use-module (system vm vm)
  #:export (compile-lightning
            call-lightning
            jit-code-guardian)
  #:re-export (lightning-verbosity))


;;;
;;; Auxiliary
;;;

;; State used during compilation.
(define-record-type <lightning>
  (%make-lightning cfg nodes pc ip labels handlers)
  lightning?

  ;; State from bytecode control flow graph.
  (cfg lightning-cfg set-lightning-cfg!)

  ;; Hash table containing compiled nodes.
  (nodes lightning-nodes)

  ;; Address of byte-compiled program code.
  (pc lightning-pc)

  ;; Current bytecode IP.
  (ip lightning-ip set-lightning-ip!)

  ;; Label objects used by lightning.
  (labels lightning-labels)

  ;; Handlers for prompt.
  (handlers lightning-handlers))

(define* (make-lightning cfg nodes pc
                         #:optional
                         (ip 0)
                         (labels (make-hash-table))
                         (handlers (make-hash-table)))
  (for-each (lambda (labeled-ip)
              (hashq-set! labels labeled-ip (jit-forward)))
            (cfg-labeled-ips cfg))
  (%make-lightning cfg nodes pc ip labels handlers))

(define jit-code-guardian (make-guardian))


;;;
;;; Inlined constants
;;;

(define-syntax define-inline
  (syntax-rules ()
    ((_ name val)
     (define-syntax name (identifier-syntax val)))))

;;; XXX: Use values from (system base types).
(define-inline tc3-struct 1)
(define-inline tc7-variable 7)
(define-inline tc7-vector 13)
(define-inline tc7-string 21)
(define-inline tc7-program 69)
(define-inline tc7-smob 127)
(define-inline tc16-real 535)

(define-inline scm-i-fixnum-bit (- (* (sizeof long) 8) 2))
(define-inline scm-f-program-is-jit-compiled #x4000)
(define-inline scm-vtable-index-flags 1)
(define-inline scm-vtable-index-self 2)
(define-inline scm-vtable-index-size 6)
(define-inline scm-vtable-flag-applicable 8)
(define-inline scm-applicable-struct-index-procedure 0)
(define-inline scm-dynstack-prompt-escape-only 16)
(define-inline scm-classf-goops 4096)

(define scm-undefined (make-pointer #x904))
(define scm-eol (scm->pointer '()))
(define scm-false (scm->pointer #f))


;;;
;;; The word size
;;;

(define-syntax word-size
  (lambda (x)
    (syntax-case x ()
      #'(datum->syntax x (sizeof '*)))))

(define-syntax word-size-length
  (lambda (x)
    (syntax-case x ()
      #'(datum->syntax x (inexact->exact
                          (/ (log (sizeof '*)) (log 2)))))))


;;;
;;; SCM macros for register read/write
;;;

(define-syntax scm-cell-object
  (syntax-rules ()
    ((_ dst obj 0)
     (jit-ldr dst obj))
    ((_ dst obj 1)
     (jit-ldxi dst obj (imm word-size)))
    ((_ dst obj n)
     (jit-ldxi dst obj (imm (* n word-size))))))

(define-syntax-rule (scm-cell-object-r dst obj reg-offset)
  (jit-ldxr dst obj reg-offset))

(define-syntax scm-set-cell-object
  (syntax-rules ()
    ((_ obj 0 src)
     (jit-str obj src))
    ((_ obj 1 src)
     (jit-stxi (imm word-size) obj src))
    ((_ obj n src)
     (jit-stxi (imm (* n word-size)) obj src))))

(define-syntax-rule (scm-set-cell-object-r obj reg-offset val)
  (jit-stxr reg-offset obj val))

(define-syntax-rule (scm-makinumi n)
  "Make scheme small fixnum from N."
  (imm (+ (ash n 2) 2)))

(define-syntax-rule (scm-makinumr dst src)
  "Make scheme small fixnum from SRC and store to DST."
  (begin
    (jit-lshi dst src (imm 2))
    (jit-addi dst dst (imm 2))))

(define-syntax-rule (scm-i-inumr dst src)
  (jit-rshi dst src (imm 2)))

(define-syntax-rule (scm-typ3 dst obj)
  (jit-andi dst obj (imm 7)))

(define-syntax-rule (scm-typ7 dst obj)
  (jit-andi dst obj (imm #x7f)))

(define-syntax-rule (scm-typ16 dst obj)
  (jit-andi dst obj (imm #xffff)))

(define-syntax-rule (scm-cell-type dst src)
  (scm-cell-object dst src 0))

(define-syntax-rule (scm-car dst obj)
  (scm-cell-object dst obj 0))

(define-syntax-rule (scm-cdr dst obj)
  (scm-cell-object dst obj 1))

(define-syntax-rule (scm-program-jit-compiled-code dst src)
  (scm-cell-object dst src 2))

(define-syntax-rule (scm-program-free-variable-ref dst src index)
  (scm-cell-object dst src (+ index 3)))

(define-syntax-rule (scm-program-free-variable-set dst index src)
  (scm-set-cell-object dst (+ index 3) src))

(define-syntax-rule (scm-pointer-value dst src)
  (scm-cell-object dst src 1))

(define-syntax-rule (scm-real-value dst src)
  (jit-ldxi-d dst src (imm (* 2 word-size))))

(define-syntax-rule (scm-i-vector-length dst src)
  (begin
    (scm-cell-object dst src 0)
    (jit-rshi dst dst (imm 8))))

(define-syntax-rule (scm-i-string-length dst obj)
  (scm-cell-object dst obj 3))

(define-syntax-rule (scm-struct-slots dst obj)
  (scm-cell-object dst obj 1))

(define-syntax-rule (scm-struct-data dst obj)
  (scm-cell-object dst obj 1))

(define-syntax-rule (scm-struct-data-ref dst obj i)
  (begin
    (scm-struct-data dst dst)
    (scm-cell-object dst obj i)))

(define-syntax-rule (scm-struct-vtable dst obj)
  (begin
    (scm-struct-vtable-slots dst obj)
    (scm-cell-object dst dst scm-vtable-index-self)))

(define-syntax-rule (scm-struct-vtable-slots dst obj)
  (begin
    (scm-cell-object dst obj 0)
    (jit-subi dst dst (imm tc3-struct))))

(define-syntax-rule (scm-bytevector-contents dst obj)
  (scm-cell-object dst obj 2))


;;;
;;; SCM macros for control flow condition
;;;

(define-syntax-rule (scm-imp obj)
  (jit-bmsi obj (imm 6)))

(define-syntax-rule (scm-heap-object-p obj)
  (jit-bmci obj (imm 6)))

(define-syntax-rule (scm-unbndp obj)
  (jit-beqi obj scm-undefined))

(define-syntax-rule (scm-not-unbndp obj)
  (jit-bnei obj scm-undefined))

(define-syntax-rule (scm-inump obj)
  (jit-bmsi obj (imm 2)))

(define-syntax-rule (scm-not-inump obj)
  (jit-bmci obj (imm 2)))

(define-syntax-rule (scm-realp tag)
  (jit-beqi tag (imm tc16-real)))

(define-syntax-rule (scm-not-realp tag)
  (jit-bnei tag (imm tc16-real)))

(define-syntax-rule (scm-program-p tag)
  (jit-beqi tag (imm tc7-program)))

(define-syntax-rule (scm-not-program-p tag)
  (jit-bnei tag (imm tc7-program)))

(define-syntax-rule (scm-program-is-jit-compiled obj)
  (jit-bmsi obj (imm scm-f-program-is-jit-compiled)))

(define-syntax-rule (scm-is-string tc7)
  (jit-beqi tc7 (imm tc7-string)))

(define-syntax-rule (scm-is-not-string tc7)
  (jit-bnei tc7 (imm tc7-string)))

(define-syntax-rule (scm-structp tc3)
  (jit-beqi tc3 (imm tc3-struct)))

(define-syntax-rule (scm-not-structp tc3)
  (jit-bnei tc3 (imm tc3-struct)))

(define-syntax-rule (scm-not-struct-applicable-p obj tmp)
  (begin
    (scm-cell-type tmp obj)
    (jit-addi tmp tmp (imm (- word-size tc3-struct)))
    (jit-ldr tmp tmp)
    (jit-bmci tmp (imm scm-vtable-flag-applicable))))

(define-syntax-rule (scm-smobp tc7)
  (jit-beqi tc7 (imm tc7-smob)))

(define-syntax-rule (scm-not-smobp tc7)
  (jit-bnei tc7 (imm tc7-smob)))

(define *values-vtable
  (dereference-pointer
   (dynamic-pointer "scm_values_vtable" (dynamic-link))))

(define-syntax-rule (scm-valuesp vt)
  (jit-beqi vt *values-vtable))

(define-syntax-rule (scm-not-valuesp vt)
  (jit-bnei vt *values-vtable))

(define-syntax-rule (scm-is-false obj)
  (jit-beqi obj scm-false))

(define-syntax-rule (scm-is-true obj)
  (jit-bnei obj scm-false))

(define-syntax-rule (scm-is-null obj)
  (jit-beqi obj scm-eol))

(define-syntax-rule (scm-is-not-null obj)
  (jit-bnei obj scm-eol))

(define-syntax-rule (scm-is-eqi obj val)
  (jit-beqi obj (imm val)))

(define-syntax-rule (scm-is-nei obj val)
  (jit-bnei obj (imm val)))

(define-syntax-rule (scm-inline-cons dst car cdr)
  (begin
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-pushargr car)
    (jit-pushargr cdr)
    (call-c "scm_do_inline_cons")
    (jit-retval dst)))

(define-syntax-rule (scm-inline-from-double dst obj)
  (begin
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-pushargr-d obj)
    (call-c "scm_do_inline_from_double")
    (jit-retval dst)))


;;;
;;; Auxiliary
;;;

(define-syntax-rule (make-negative-pointer n)
  (make-pointer (- (expt 2 (* 8 word-size)) n)))

(define-syntax-rule (dereference-scm pointer)
  (pointer->scm (dereference-pointer pointer)))


;;;
;;; VM registers with specific use
;;;

;; Frame pointer.
(define reg-fp (jit-fp))

;; Stack pointer.
(define-inline reg-sp v0)

;; Pointer to current `struct scm_vm* vp'.
(define-inline reg-vp v1)

;; Current thread.
(define-inline reg-thread v2)

;; Registers, used by prompt.
(define reg-registers (jit-f (- (jit-f-num) 1)))


;;;
;;; Macros for specific registers
;;;

(define-syntax-rule (vm-thread-dynamic-state dst)
  (jit-ldxi dst reg-thread (imm #xd8)))

(define-syntax-rule (vm-thread-pending-asyncs dst)
  (jit-ldxi dst reg-thread (imm #x104)))

(define-syntax-rule (vm-cache-fp)
  (jit-ldxi reg-fp reg-vp (imm #x10)))

(define-syntax-rule (vm-sync-fp)
  (jit-stxi (imm #x10) reg-vp reg-fp))

(define-syntax-rule (vm-cache-sp)
  (jit-ldxi reg-sp reg-vp (imm #x8)))

(define-syntax-rule (vm-sync-sp)
  (jit-stxi (imm #x8) reg-vp reg-sp))

(define-syntax-rule (vm-sp-max-since-gc dst)
  (jit-ldxi dst reg-vp (imm #x28)))

(define-syntax-rule (vm-set-sp-max-since-gc src)
  (jit-stxi (imm #x28) reg-vp src))

(define-syntax-rule (vm-stack-base dst)
  (jit-ldxi dst reg-vp (imm #x38)))

(define-syntax-rule (vm-stack-limit dst)
  (jit-ldxi dst reg-vp (imm #x18)))

(define-syntax-rule (scm-frame-dynamic-link dst)
  (jit-ldxi dst reg-fp (make-negative-pointer (* 2 word-size))))

(define-syntax-rule (scm-set-frame-dynamic-link src)
  (jit-stxi (make-negative-pointer (* 2 word-size)) reg-fp src))

(define-syntax-rule (scm-frame-previous-sp dst)
  (jit-subi dst reg-fp (imm (* 3 word-size))))

(define-syntax-rule (scm-set-frame-previous-sp src)
  (jit-stxi (imm #x8) reg-fp src))

(define-syntax-rule (scm-frame-return-address dst)
  (jit-ldxi dst reg-fp (make-negative-pointer word-size)))

(define-syntax-rule (scm-set-frame-return-address src)
  (jit-stxi (make-negative-pointer word-size) reg-fp src))


;;;
;;; VM op syntaxes
;;;

(define *vm-instr* (make-hash-table))

(define-syntax define-vm-op
  (syntax-rules ()
    ((_ (op st . args) body ...)
     (hashq-set! *vm-instr* 'op (lambda (st . args)
                                  body ...)))))

(define-syntax-rule (frame-local n)
  (imm (* n word-size)))

(define-syntax-rule (frame-locals-count-from dst n)
  (begin
    (jit-subr dst reg-sp reg-fp)
    (jit-subi dst dst (imm (* n word-size)))
    (jit-rshi dst dst (imm word-size-length))))

(define-syntax-rule (frame-locals-count dst)
  (begin
    (jit-subr dst reg-sp reg-fp)
    (jit-rshi dst dst (imm word-size-length))
    (jit-addi dst dst (imm 1))))

(define-syntax-rule (stored-ref st n)
  "Stored reference of address of local N."
  (frame-local n))

(define-syntax local-ref
  (syntax-rules ()
    ((local-ref st n)
     (local-ref st n r0))
    ((local-ref st 0 reg)
     (begin
       (jit-ldr reg reg-fp)
       reg))
    ((local-ref st n reg)
     (begin
       (jit-ldxi reg reg-fp (stored-ref st n))
       reg))))

(define-syntax local-set!
  (syntax-rules ()
    ((_ st 0 reg)
     (jit-str reg-fp reg))
    ((_ st dst reg)
     (jit-stxi (stored-ref st dst) reg-fp reg))))

(define-syntax-rule (local-set!/immediate st dst val)
  (begin
    (jit-movi r0 val)
    (jit-stxi (stored-ref st dst) reg-fp r0)))

(define-syntax-rule (offset-addr st offset)
  (+ (lightning-pc st) (* 4 (+ (lightning-ip st) offset))))

(define-syntax-rule (last-arg-offset st dst tmp)
  (begin
    (frame-locals-count tmp)
    (jit-subi tmp tmp (imm 1))
    (jit-lshi dst tmp (imm word-size-length))))

(define-syntax-rule (call-c name)
  (jit-calli (dynamic-func name (dynamic-link))))

(define-syntax-rule (resolve-dst st offset)
  "Resolve jump destination with <lightning> state ST and OFFSET given as
argument in VM operation."
  (hashq-ref (lightning-labels st) (+ (lightning-ip st) offset)))

(define-syntax jump
  (syntax-rules ()
    ((_ label)
     (jit-patch-at (jit-jmpi) label))
    ((_ condition label)
     (jit-patch-at condition label))))

(define-syntax vm-handle-interrupts
  (syntax-rules ()
    ((_)
     (vm-handle-interrupts r0))
    ((_ tmp)
     (let ((lexit (jit-forward)))
       (vm-thread-pending-asyncs tmp)
       (jump (jit-bmci tmp (imm 1)) lexit)
       (jit-prepare)
       (call-c "scm_async_tick")
       (vm-cache-fp)
       (vm-cache-sp)
       (jit-link lexit)))))

(define-syntax vm-return
  (syntax-rules ()
    ((_ st)
     (vm-return st r0))
    ((_ st tmp)
     (begin
       ;; Get return address to jump.
       (scm-frame-return-address tmp)
       ;; Restore previous dynamic link to current frame pointer.
       (scm-frame-dynamic-link reg-fp)
       (vm-sync-fp)
       ;; ... then jump to return address.
       (jit-jmpr tmp)))))

(define-syntax vm-reset-frame
  (syntax-rules ()
    ((_ n)
     (vm-reset-frame n r0))
    ((_ n tmp)
     (let ((lexit (jit-forward)))
       (jit-addi reg-sp reg-fp (imm (* (- n 1) word-size)))
       (vm-sync-sp)
       (vm-sp-max-since-gc tmp)
       (jump (jit-bger tmp reg-sp) lexit)
       (vm-set-sp-max-since-gc reg-sp)
       (jit-link lexit)))))

(define-syntax vm-alloc-frame
  (syntax-rules ()
    ((_)
     (let ((lexit (jit-forward))
           (lincr (jit-forward)))
       ;; Using r0 as temporary register.
       (vm-sp-max-since-gc r0)
       (jump (jit-bger r0 reg-sp) lexit)
       (vm-stack-limit r0)
       (jump (jit-bgtr r0 reg-sp) lincr)

       (jit-prepare)
       (jit-pushargr reg-vp)
       (jit-pushargr reg-sp)
       (call-c "scm_do_vm_expand_stack")
       (vm-cache-fp)
       (vm-cache-sp)
       (jump lexit)

       (jit-link lincr)
       (vm-set-sp-max-since-gc reg-sp)

       (jit-link lexit)))

    ((_ n)
     (begin
       (jit-addi reg-sp reg-fp (imm (* (- n 1) word-size)))
       (vm-sync-sp)
       (vm-alloc-frame)))))

(define-syntax with-frame
  (syntax-rules ()
  "Run body expression with new frame.

Stack poionter stored in reg-fp increased for `proc * word' size to
shift the locals.  Then patch the address after the jump, so that callee
can jump back.  Two locals below proc get overwritten by the callee."
    ((_ st proc nlocals body)
     (with-frame st r0 proc nlocals body))
    ((_ st tmp proc nlocals body)
     (let ((ra (jit-movi tmp (imm 0))))
       ;; Store return address.
       (jit-stxi (stored-ref st (- proc 1)) reg-fp tmp)
       ;; Store dynamic link.
       (jit-stxi (stored-ref st (- proc 2)) reg-fp reg-fp)
       ;; Shift frame pointer.
       (jit-addi reg-fp reg-fp (imm (* proc word-size)))
       (vm-sync-fp)
       ;; Shift stack pointer.
       (vm-reset-frame nlocals tmp)
       body
       (jit-patch ra)))))

(define-syntax-rule (return-value-list st proc rval tmp1 tmp2 tmp3)
  (let ((lexit (jit-forward))
        (lshuffle (jit-forward)))

    (vm-reset-frame 2 tmp1)
    (jump (scm-imp rval) lexit)

    (scm-cell-type tmp1 rval)
    (scm-typ3 tmp2 tmp1)
    (jump (scm-not-structp tmp2) lexit)

    (jit-subi tmp2 tmp1 (imm tc3-struct))
    (scm-cell-object tmp2 tmp2 scm-vtable-index-self)
    (jump (scm-not-valuesp tmp2) lexit)

    (scm-struct-slots tmp1 rval)
    (scm-cell-object tmp1 tmp1 0)
    (scm-car rval tmp1)
    (local-set! st (+ proc 1) rval)
    (jit-movi tmp2 (stored-ref st (+ proc 1)))

    ;; Reset frame, increment while shuffling with VALUES struct.
    (vm-reset-frame 1 tmp3)

    (jit-link lshuffle)
    (scm-car tmp3 tmp1)
    (jit-stxr tmp2 reg-fp tmp3)
    (jit-addi tmp2 tmp2 (imm word-size))
    (jit-addi reg-sp reg-sp (imm word-size))
    (scm-cdr tmp1 tmp1)
    (jump (scm-is-not-null tmp1) lshuffle)

    (vm-sync-sp)
    (vm-sp-max-since-gc r0)
    (jump (jit-bger r0 reg-sp) lexit)
    (vm-set-sp-max-since-gc reg-sp)

    (jit-link lexit)
    (vm-return st)))

(define-syntax-rule (vm-apply)
  "Apply the procedure in local 0.

Uses remaining locals as arguments passed to the local.  When local 0 is
not a program, unwraps until it get a program, or show error.  This
behaviour is similar to the `apply' label in vm-regular engine."

  (let ((lprogram (jit-forward))
        (lcompiled (jit-forward))
        (lunwrap (jit-forward))
        (lsmob (jit-forward))
        (lshuffle (jit-forward))
        (lerror (jit-forward)))

    (jit-ldxi r0 reg-fp (frame-local 0))

    ;; Unwrap local 0.
    (jit-link lunwrap)
    (jump (scm-imp r0) lerror)
    (scm-cell-type r2 r0)
    (scm-typ7 r1 r2)
    (jump (scm-program-p r1) lprogram)

    ;; Test for applicable struct.
    (scm-typ3 r1 r2)
    (jump (scm-not-structp r1) lsmob)
    (jump (scm-not-struct-applicable-p r0 r1) lerror)
    (scm-struct-data-ref r0 r0 0)
    (jit-stxi (frame-local 0) reg-fp r0)
    (jump lunwrap)

    ;; Test for applicable smob.  Apply trampoline for smob is not
    ;; inlined with lightning, calling C functions.
    (jit-link lsmob)
    (scm-typ7 r1 r2)
    (jump (scm-not-smobp r1) lerror)
    (jit-movr f0 r0)
    (jit-prepare)
    (jit-pushargr r0)
    (call-c "scm_do_smob_applicable_p")
    (jit-retval r0)
    (jump (jit-beqi r0 (imm 0)) lerror)
    (last-arg-offset st r1 r2)
    (jit-addi reg-sp reg-sp (imm word-size))
    (vm-sync-sp)
    (vm-sp-max-since-gc r0)
    (jump (jit-bger r0 reg-sp) lshuffle)
    (vm-set-sp-max-since-gc reg-sp)

    (jit-link lshuffle)
    (jit-ldxr r2 reg-fp r1)
    (jit-addi r1 r1 (imm word-size))
    (jit-stxr r1 reg-fp r2)
    (jit-subi r1 r1 (imm (* 2 word-size)))
    (jump (jit-bgei r1 (stored-ref st 0)) lshuffle)

    (jit-prepare)
    (jit-pushargr f0)
    (call-c "scm_do_smob_apply_trampoline")
    (jit-retval r0)
    (jit-stxi (frame-local 0) reg-fp r0)
    (jump lunwrap)

    ;; Show error message.
    (jit-link lerror)
    (error-wrong-type-apply r0)

    ;; Local is program, test for native code.
    (jit-link lprogram)
    (jump (scm-program-is-jit-compiled r2) lcompiled)

    ;; Does not have native code, compile the callee procedure.
    (jit-prepare)
    (jit-pushargr r0)
    ;; (jit-calli %compile-lightning)
    (call-c "scm_compile_lightning")
    (vm-cache-fp)
    (vm-cache-sp)
    (jit-ldxi r0 reg-fp (frame-local 0))

    ;; Has compiled code.
    (jit-link lcompiled)
    (scm-program-jit-compiled-code r1 r0)
    (jit-jmpr r1)))

(define-syntax-rule (halt)
  (let ((lshuffle (jit-forward))
        (lvalues (jit-forward))
        (lcall (jit-forward))
        (lexit (jit-forward)))

    (jit-ldxi r0 reg-fp (frame-local 4))
    (frame-locals-count-from r2 4)
    (jump (jit-beqi r2 (imm 0)) lexit)

    (jit-link lvalues)
    (jit-movi r0 scm-eol)
    (jit-addi r1 r2 (imm 1))
    (jit-lshi r1 r1 (imm word-size-length))
    (jit-addi r1 r1 (frame-local 3))

    (jit-link lshuffle)
    (jump (jit-blei r1 (frame-local 3)) lcall)
    (jit-ldxr r2 reg-fp r1)
    (scm-inline-cons r0 r2 r0)
    (jit-subi r1 r1 (imm word-size))
    (jump lshuffle)

    (jit-link lcall)
    (jit-prepare)
    (jit-pushargr r0)
    (call-c "scm_values")
    (jit-retval r0)

    (jit-link lexit)

    ;; Original contents of jit-fp used by lightning.
    (jit-ldr r2 reg-fp)

    ;; Move `vp->fp' once for boot continuation added in `scm_call_n',
    ;; then reset the stack pointer and return address.
    (scm-frame-dynamic-link reg-fp)
    (vm-sync-fp)
    (scm-frame-previous-sp reg-sp)
    (vm-sync-sp)
    (scm-frame-return-address r1)
    (scm-set-frame-return-address r1)

    ;; Restore the original frame pointer.
    (jit-movr reg-fp r2)

    ;; Return from native code.
    (jit-retr r0)))

(define-syntax compile-label
  (syntax-rules (assemble-lightning with-frame)

    ;; Non tail call
    ((_ st1 proc nlocals callee-addr #f)
     (compile-label st1 st2 proc nlocals callee-addr
                    (with-frame st2 proc nlocals
                                (assemble-lightning st2 (jit-forward)))))

    ;; Tail call
    ((_ st1 proc nlocals callee-addr #t)
     (compile-label st1 st2 proc nlocals callee-addr
                    (assemble-lightning st2 (jit-forward))))

    ((_ st1 st2 proc nlocals callee-addr body)
     (let ((st2 (make-lightning (procedure->cfg callee-addr)
                                (lightning-nodes st1)
                                (ensure-program-addr callee-addr))))
       body))))

(define-syntax-rule (in-same-procedure? st label)
  ;; Could look the last IP of current procedure.  Instead, looking for
  ;; backward jump.
  (and (<= 0 (+ (lightning-ip st) label))
       (< label 0)))

(define-syntax-rule (compiled-node st addr)
  (hashq-ref (lightning-nodes st) addr))

(define-syntax-rule (define-link l . body)
  (begin (jit-link l) . body))


;;;
;;; Assertion, validation, and error messages
;;;

;; Defining pointer to strings used in error messages as top-level, to
;; avoid garbage collection.

(define-syntax define-string-pointer
  (lambda (x)
    (define (string-name obj)
      (string->symbol
       (string-append "*"
                      (symbol->string (syntax->datum obj))
                      "-string")))
    (syntax-case x ()
      ((_ sym)
       #`(define #,(datum->syntax x (string-name #'sym))
           (string->pointer (symbol->string 'sym)))))))

(define-string-pointer pair)
(define-string-pointer car)
(define-string-pointer cdr)
(define-string-pointer set-car!)
(define-string-pointer set-cdr!)
(define-string-pointer vector)
(define-string-pointer vector-length)
(define-string-pointer vector-ref)
(define-string-pointer vector-set!)
(define-string-pointer struct)
(define-string-pointer struct-vtable)
(define-string-pointer struct-ref)
(define-string-pointer struct-set!)
(define-string-pointer bv-u8-ref)
(define-string-pointer bv-s8-ref)
(define-string-pointer bv-u16-ref)
(define-string-pointer bv-s16-ref)
(define-string-pointer bv-u32-ref)
(define-string-pointer bv-s32-ref)
(define-string-pointer bv-u64-ref)
(define-string-pointer bv-s64-ref)
(define-string-pointer bv-f32-ref)
(define-string-pointer bv-f64-ref)
(define-string-pointer bv-u8-set!)
(define-string-pointer bv-s8-set!)
(define-string-pointer bv-u16-set!)
(define-string-pointer bv-s16-set!)
(define-string-pointer bv-u32-set!)
(define-string-pointer bv-s32-set!)
(define-string-pointer bv-u64-set!)
(define-string-pointer bv-s64-set!)
(define-string-pointer bv-f32-set!)
(define-string-pointer bv-f64-set!)

(define (wrong-type-apply proc)
  (scm-error 'wrong-type-arg #f "Wrong type to apply: ~S" `(,proc) `(,proc)))

(define (%error-wrong-type-apply-proc proc)
  (wrong-type-apply (pointer->scm proc)))

(define %error-wrong-type-apply
  (procedure->pointer '* %error-wrong-type-apply-proc '(*)))

(define-syntax-rule (error-wrong-type-apply proc)
  (begin
    (jit-prepare)
    (jit-pushargr proc)
    (jit-calli %error-wrong-type-apply)
    (jit-reti (scm->pointer *unspecified*))))

(define-syntax-rule (error-wrong-type-arg-msg subr pos reg expected)
  (begin
    (jit-prepare)
    (jit-pushargi subr)
    (jit-pushargi (imm pos))
    (jit-pushargr reg)
    (jit-pushargi expected)
    (call-c "scm_wrong_type_arg_msg")
    (jit-reti (scm->pointer *unspecified*))))

(define-syntax-rule (error-out-of-range subr expr)
  (begin
    (jit-prepare)
    (jit-pushargi subr)
    expr
    (call-c "scm_out_of_range")
    (jit-reti (scm->pointer *unspecified*))))

(define (%error-wrong-num-values-proc nvalues)
  (scm-error 'vm-error
             'vm-lightning
             "Wrong number of values returned to continuation (expected ~a)"
             `(,nvalues) `(,nvalues)))

(define %error-wrong-num-values
  (procedure->pointer '* %error-wrong-num-values-proc `(,int)))

(define-syntax-rule (error-wrong-num-values nvalues)
  (begin
    (jit-prepare)
    (jit-pushargi (imm nvalues))
    (jit-calli %error-wrong-num-values)
    (jit-reti (scm->pointer *unspecified*))))

(define (%error-too-few-values-proc)
  (scm-error 'vm-error
             'vm-lightning
             "Too few values returned to continuation"
             '() '()))

(define %error-too-few-values
  (procedure->pointer '* %error-too-few-values-proc '()))

(define-syntax-rule (error-too-few-values)
  (begin
    (jit-prepare)
    (jit-calli %error-too-few-values)
    (jit-reti (scm->pointer *unspecified*))))

(define (%error-no-values-proc)
  (scm-error 'vm-error
             'vm-lightning
             "Zero values returned to single-valued continuation"
             '() '()))

(define %error-no-values
  (procedure->pointer '* %error-no-values-proc '()))

(define-syntax-rule (error-no-values)
  (begin
    (jit-prepare)
    (jit-calli %error-no-values)
    (jit-reti (scm->pointer *unspecified*))))

(define-syntax-rule (assert-wrong-num-args st jit-op expected local)
  (let ((lexit (jit-forward)))
    (frame-locals-count r0)
    (jump (jit-op r0 (imm expected)) lexit)
    (jit-prepare)
    (jit-pushargr (local-ref st 0))
    (call-c "scm_wrong_num_args")
    (jit-reti (scm->pointer *unspecified*))
    (jit-link lexit)))

(define-syntax-rule (validate-pair pair cell-0 subr pos)
  (let ((lerror (jit-forward))
        (lexit (jit-forward)))
    (jump (scm-imp pair) lerror)
    (scm-cell-type cell-0 pair)
    (jump (jit-bmsi cell-0 (imm 1)) lerror)
    (jump lexit)

    (jit-link lerror)
    (error-wrong-type-arg-msg subr pos r0 *pair-string)

    (jit-link lexit)))

(define-syntax-rule (validate-vector vec cell-0 tag subr)
  (let ((lerror (jit-forward))
        (lexit (jit-forward)))
    (jump (scm-imp vec) lerror)
    (scm-cell-type cell-0 vec)
    (scm-typ7 tag cell-0)
    (jump (scm-is-eqi tag tc7-vector) lexit)

    (jit-link lerror)
    (error-wrong-type-arg-msg subr 1 vec *vector-string)

    (jit-link lexit)))

(define-syntax-rule (validate-vector-range vec idx tmp1 tmp2 subr)
  ;; Registers `tmp1' and `tmp2' will get dirty after the validation.
  (let ((lerror (jit-forward))
        (lexit (jit-forward)))
    (jump (scm-not-inump idx) lerror)
    (scm-i-inumr tmp1 idx)
    (jump (jit-blti tmp1 (imm 0)) lerror)
    (scm-i-vector-length tmp2 vec)
    (jump (jit-bltr tmp1 tmp2) lexit)

    (jit-link lerror)
    (error-out-of-range subr (jit-pushargr idx))

    (jit-link lexit)))

(define-syntax-rule (validate-vector-range/immediate vec idx tmp subr)
  (let ((lexit (jit-forward)))
    (scm-i-vector-length tmp vec)
    (jump (jit-bgti tmp (imm idx)) lexit)
    (error-out-of-range subr (jit-pushargi (scm-makinumi idx)))
    (jit-link lexit)))

(define-syntax-rule (validate-struct obj tmp subr)
  (let ((lerror (jit-forward))
        (lexit (jit-forward)))
    (jump (scm-imp obj) lerror)
    (scm-cell-type tmp obj)
    (scm-typ3 tmp tmp)
    (jump (scm-structp tmp) lexit)

    (jit-link lerror)
    (error-wrong-type-arg-msg subr 1 obj *struct-string)

    (jit-link lexit)))


;;;
;;; Top-level syntax for specific VM operations
;;;

(define-syntax define-br-nargs-op
  (syntax-rules ()
    ((_ (name st expected offset) jit-op)
     (define-vm-op (name st expected offset)
       (frame-locals-count r1)
       (jump (jit-op r1 (imm expected))
             (resolve-dst st offset))))))

(define-syntax define-vm-br-unary-immediate-op
  (syntax-rules ()
    ((_ (name st a invert offset) expr)
     (define-vm-op (name st a invert offset)
       (when (< offset 0)
         (vm-handle-interrupts))
       (jump expr (resolve-dst st offset))))))

(define-syntax define-vm-br-unary-heap-object-op
  (syntax-rules ()
    ((_ (name st a invert offset) reg expr)
     (define-vm-op (name st a invert offset)
       (when (< offset 0)
         (vm-handle-interrupts))
       (let ((lexit (jit-forward)))
         (local-ref st a reg)
         (jump (scm-imp reg) (if invert (resolve-dst st offset) lexit))
         (scm-cell-object reg reg 0)
         (jump expr (if invert (resolve-dst st offset) lexit))
         (when (not invert)
           (jump (resolve-dst st offset)))
         (jit-link lexit))))))

(define-syntax define-vm-br-binary-op
  (syntax-rules ()
    ((_ (name st a b invert offset) cname)
     (define-vm-op (name st a b invert offset)
       (when (< offset 0)
         (vm-handle-interrupts))
       (let ((lexit (jit-forward)))
         (local-ref st a r0)
         (local-ref st b r1)
         (jump (jit-beqr r0 r1) (if invert lexit (resolve-dst st offset)))

         (jit-prepare)
         (jit-pushargr r0)
         (jit-pushargr r1)
         (call-c cname)
         (jit-retval r0)
         (jump (scm-is-false r0) (if invert (resolve-dst st offset) lexit))

         (when (not invert)
           (jump (resolve-dst st offset)))

         (jit-link lexit))))))

(define-syntax define-vm-br-arithmetic-op
  (syntax-rules ()
    ((_ (name st a b invert offset)
        fx-op fx-invert-op fl-op fl-invert-op cname)
     (define-vm-op (name st a b invert offset)
       (when (< offset 0)
         (vm-handle-interrupts))
       (let ((lreal (jit-forward))
             (lcall (jit-forward))
             (lexit (jit-forward))
             (rega (local-ref st a r1))
             (regb (local-ref st b r2)))

         (jump (scm-not-inump rega) lreal)
         (jump (scm-not-inump regb) lreal)
         (jump ((if invert fx-invert-op fx-op) rega regb)
               (resolve-dst st offset))
         (jump lexit)

         (jit-link lreal)
         (jump (scm-imp rega) lcall)
         (scm-cell-type r0 rega)
         (scm-typ16 r0 r0)
         (jump (scm-is-nei r0 tc16-real) lcall)
         (jump (scm-imp regb) lcall)
         (scm-cell-type r0 regb)
         (scm-typ16 r0 r0)
         (jump (scm-not-realp r0) lcall)
         (scm-real-value f0 rega)
         (scm-real-value f1 regb)
         (jump ((if invert fl-invert-op fl-op) f0 f1)
               (resolve-dst st offset))
         (jump lexit)

         (jit-link lcall)
         (jit-prepare)
         (jit-pushargr rega)
         (jit-pushargr regb)
         (call-c cname)
         (jit-retval r0)
         (jump (if invert (scm-is-false r0) (scm-is-true r0))
               (resolve-dst st offset))

         (jit-link lexit))))))

;; If var is not variable, resolve with `resolver' and move the resolved
;; value to var's address. Otherwise, var is `variable', move it to dst.
;; Currently, the variable is resolved at JIT compilation time.
(define-syntax define-vm-box-op
  (syntax-rules ()
    ((_ (name st mod-offset sym-offset) resolver ...)
     (define-vm-op (name st dst var-offset mod-offset sym-offset bound?)
       (let* ((current (lightning-ip st))
              (base (lightning-pc st))
              (offset->pointer
               (lambda (offset)
                 (make-pointer (+ base (* 4 (+ current offset))))))
              (var (dereference-scm (offset->pointer var-offset))))
         (let ((resolved (if (variable? var)
                             var
                             resolver ...)))
           (local-set!/immediate st dst (scm->pointer resolved))))))))

(define-syntax define-vm-binary-numeric-op
  (syntax-rules ()
    ((_ (name st dst a b)
        cname lcall rega regb <inum-expr>)
     (define-vm-op (name st dst a b)
       (let ((lcall (jit-forward))
             (lexit (jit-forward)))
         (local-ref st a rega)
         (local-ref st b regb)
         (jump (scm-not-inump rega) lcall)
         (jump (scm-not-inump regb) lcall)
         <inum-expr>
         (jump lexit)

         (jit-link lcall)
         (jit-prepare)
         (jit-pushargr rega)
         (jit-pushargr regb)
         (call-c cname)
         (jit-retval r0)

         (jit-link lexit)
         (local-set! st dst r0))))

    ((_ (name st dst a b)
        cname lcall rega regb xmma xmmb <inum-expr> <real-expr>)
     (define-vm-op (name st dst a b)
       (let ((lreal (jit-forward))
             (lcall (jit-forward))
             (lexit (jit-forward)))
         (local-ref st a rega)
         (local-ref st b regb)
         (jump (scm-not-inump rega) lreal)
         (jump (scm-not-inump regb) lreal)
         <inum-expr>
         (jump lexit)

         (jit-link lreal)
         (jump (scm-imp rega) lcall)
         (scm-cell-type r0 rega)
         (scm-typ16 r0 r0)
         (jump (scm-not-realp r0) lcall)
         (jump (scm-imp regb) lcall)
         (scm-cell-type r0 regb)
         (scm-typ16 r0 r0)
         (jump (scm-not-realp r0) lcall)
         (scm-real-value xmma rega)
         (scm-real-value xmmb regb)
         <real-expr>
         (scm-inline-from-double r0 f0)
         (jump lexit)

         (jit-link lcall)
         (jit-prepare)
         (jit-pushargr rega)
         (jit-pushargr regb)
         (call-c cname)
         (jit-retval r0)

         (jit-link lexit)
         (local-set! st dst r0))))))

(define-syntax define-vm-unary-step-op
  (syntax-rules ()
    ((_ (name st dst src) cname fx-op fl-op)
     (define-vm-op (name st dst src)
       (let ((lreal (jit-forward))
             (lcall (jit-forward))
             (lexit (jit-forward))
             (reg (local-ref st src r1)))

         (jump (scm-not-inump reg) lreal)
         (jit-movr r0 reg)
         (jump (fx-op r0 (imm 4)) lreal)
         (jump lexit)

         (jit-link lreal)
         (scm-cell-type r0 reg)
         (jump (scm-is-nei r0 tc16-real) lcall)
         (scm-real-value f0 reg)
         (jit-movi r0 (imm 1))
         (jit-extr-d f1 r0)
         (fl-op f0 f0 f1)
         (scm-inline-from-double r0 f0)
         (jump lexit)

         (jit-link lcall)
         (jit-prepare)
         (jit-pushargr reg)
         (jit-pushargi (imm 6))
         (call-c cname)
         (jit-retval r0)

         (jit-link lexit)
         (local-set! st dst r0))))))

(define-syntax define-vm-bv-ref-op
  (syntax-rules ()
    ((_ (name st dst src idx) <expr>)
     (define-vm-op (name st dst src idx)
       (local-ref st src r0)
       (local-ref st idx r2)
       (scm-i-inumr r1 r2)
       (scm-bytevector-contents r0 r0)
       <expr>
       (local-set! st dst r0)))

    ((_ name "float")
     (define-vm-bv-ref-op (name st dst src idx)
       (begin
         (jit-ldxr-f f0 r0 r1)
         (jit-extr-f-d f0 f0)
         (scm-inline-from-double r0 f0))))

    ((_ name "double")
     (define-vm-bv-ref-op (name st dst src idx)
       (begin
         (jit-ldxr-d f0 r0 r1)
         (scm-inline-from-double r0 f0))))

    ((_ name jit-op cname)
     (define-vm-bv-ref-op (name st dst src idx)
       (let ((lcall (jit-forward))
             (lexit (jit-forward)))
         (jit-ldxr r0 r0 r1)
         (jump (jit-bgei r0 (imm most-positive-fixnum)) lcall)
         (jump (jit-blei r0 (imm (logand #xffffffffffffffff
                                         most-negative-fixnum)))
               lcall)
         (scm-makinumr r0 r0)
         (jump lexit)

         (jit-link lcall)
         (jit-prepare)
         (jit-pushargr r0)
         (call-c cname)
         (jit-retval r0)

         (jit-link lexit))))

    ((_ name jit-op)
     (define-vm-bv-ref-op (name st dst src idx)
       (begin
         (jit-op r0 r0 r1)
         (scm-makinumr r0 r0))))))

(define-syntax define-vm-bv-set-op
  (syntax-rules ()
    ((_ (name st dst idx src) <expr>)
     (define-vm-op (name st dst idx src)
       (local-ref st dst r0)
       (local-ref st idx r2)
       (scm-i-inumr r1 r2)
       (local-ref st src f1)
       (scm-bytevector-contents r0 r0)
       <expr>))

    ((_ name "float")
     (define-vm-bv-set-op (name st dst idx src)
       (begin
         (scm-real-value f1 f1)
         (jit-extr-d-f f1 f1)
         (jit-stxr-f r1 r0 f1))))

    ((_ name "double")
     (define-vm-bv-set-op (name st dst idx src)
       (begin
         (scm-real-value f1 f1)
         (jit-stxr-d r1 r0 f1))))

    ((_ name jit-op cname)
     (define-vm-bv-set-op (name st dst idx src)
       (begin
         (jit-movr r2 f1)
         (jit-movr f0 r0)
         (jit-movr f1 r1)
         (jit-prepare)
         (jit-pushargr r2)
         (call-c cname)
         (jit-retval r0)
         (jit-op f1 f0 r0))))

    ((_ name jit-op)
     (define-vm-bv-set-op (name st dst idx src)
       (begin
         (scm-i-inumr f1 f1)
         (jit-op r1 r0 f1))))))


;;;
;;; VM operations
;;;

;; Groupings are taken from "guile/libguile/vm-engine.c".


;;; Call and return
;;; ---------------

(define-vm-op (call st proc nlocals)
  (vm-handle-interrupts)
  (with-frame st proc nlocals (vm-apply)))

(define-vm-op (call-label st proc nlocals label)
  (vm-handle-interrupts)
  (cond
   ((in-same-procedure? st label)
    (with-frame st proc nlocals (jump (resolve-dst st label))))
   ((compiled-node st (offset-addr st label))
    =>
    (lambda (node)
      (with-frame st proc nlocals (jump node))))
   (else
    (compile-label st proc nlocals (offset-addr st label) #f))))

(define-vm-op (tail-call st nlocals)
  (vm-handle-interrupts)
  (vm-reset-frame nlocals)
  (vm-apply))

(define-vm-op (tail-call-label st nlocals label)
  (vm-handle-interrupts)
  (vm-reset-frame nlocals)
  (cond
   ((in-same-procedure? st label)
    (jump (resolve-dst st label)))
   ((compiled-node st (offset-addr st label))
    =>
    (lambda (node)
      (jump node)))
   (else
    (compile-label st 0 nlocals (offset-addr st label) #t))))

(define-vm-op (tail-call/shuffle st from)
  (let ((lshuffle (jit-forward))
        (lreset (jit-forward))
        (lexit (jit-forward)))
    (vm-handle-interrupts)

    ;; r2 used to stop the loop in lshuffle.
    (jit-subr r2 reg-sp reg-fp)
    (jit-subi r2 r2 (imm (* (- from 3) word-size)))

    ;; r0 used as local index.
    (jit-movi r0 (imm 0))

    (jit-link lshuffle)
    (jump (jit-bger r0 r2) lreset)
    (jit-addi r1 r0 (imm (* from word-size)))
    (jit-addi r0 r0 (imm word-size))
    (jit-ldxr f0 reg-fp r1)
    (jit-stxr r0 reg-fp f0)
    (jump lshuffle)

    (jit-link lreset)
    (jit-subi r0 r0 (imm (* 2 word-size)))
    (jit-addr reg-sp reg-fp r0)

    ;; Doing similar thing to `vm-reset-frame', but reg-sp is already
    ;; set to new value.
    (vm-sync-sp)
    (vm-sp-max-since-gc r0)
    (jump (jit-bger r0 reg-sp) lexit)
    (vm-set-sp-max-since-gc reg-sp)

    (jit-link lexit)
    (vm-apply)))

(define-vm-op (receive st dst proc nlocals)
  (let ((lexit (jit-forward)))
    (frame-locals-count r0)
    (jump (jit-bgti r0 (imm (+ proc 1))) lexit)
    (error-no-values)

    (jit-link lexit)
    (vm-reset-frame nlocals)
    (local-ref st (+ proc 1) r0)
    (local-set! st dst r0)))

(define-vm-op (receive-values st proc allow-extra? nvalues)
  (let ((lexit (jit-forward)))
    (frame-locals-count r1)
    (if allow-extra?
        (begin
          (jump (jit-bgti r1 (imm (+ proc nvalues))) lexit)
          (error-too-few-values))
        (begin
          (jump (jit-beqi r1 (imm (+ proc 1 nvalues))) lexit)
          (error-wrong-num-values nvalues)))
    (jit-link lexit)))

(define-vm-op (return st dst)
  (local-ref st dst r0)
  (local-set! st 1 r0)
  (vm-reset-frame 2)
  (vm-return st))

(define-vm-op (return-values st)
  (vm-return st))


;;; Specialized call stubs
;;; ----------------------

(define-vm-op (subr-call st ptr-idx)
  (let ((lcall (jit-forward)))
    ;; `subr-call' accepts up to 10 arguments.
    (jit-prepare)
    (frame-locals-count r1)
    (for-each (lambda (n)
                (jump (jit-blei r1 (imm (+ n 1))) lcall)
                (jit-pushargr (local-ref st (+ n 1))))
              (iota 10))

    (jit-link lcall)
    (local-ref st 0 r0)
    (scm-program-free-variable-ref r0 r0 ptr-idx)
    (scm-pointer-value r1 r0)
    (jit-callr r1)
    (jit-retval r0)
    (vm-cache-fp)
    (vm-cache-sp)
    (local-set! st 1 r0)
    (return-value-list st 0 r0 r1 r2 f0)))

(define-vm-op (foreign-call st cif-idx ptr-idx)
  (local-ref st 0 r0)
  (scm-program-free-variable-ref r1 r0 cif-idx)
  (scm-program-free-variable-ref r2 r0 ptr-idx)
  (jit-addi r0 reg-fp (stored-ref st 1))
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr r1)
  (jit-pushargr r2)
  (jit-pushargr r0)
  (call-c "scm_do_foreign_call")
  (jit-retval r0)
  (vm-cache-fp)
  (vm-cache-sp)
  (local-set! st 1 r0)
  (return-value-list st 0 r0 r1 r2 f0))

;;; XXX: continuation-call

(define-vm-op (compose-continuation st cont)
  (local-ref st 0 r0)
  (scm-program-free-variable-ref r0 r0 cont)

  ;; XXX: Add assertion for rewindable vmcont.
  (jit-prepare)
  (jit-pushargr reg-vp)                 ; vp
  (jit-pushargr r0)                     ; cont
  (frame-locals-count r0)
  (jit-subi r0 r0 (imm 1))
  (jit-pushargr r0)                     ; n
  (jit-addi r0 reg-fp (imm word-size))
  (jit-pushargr r0)                     ; argv
  (jit-pushargr reg-thread)             ; thread
  (jit-pushargr reg-registers)          ; registers
  (call-c "scm_do_vm_reinstate_partial_continuation")

  (vm-cache-fp)
  (vm-cache-sp)
  (jit-ldr r0 reg-vp)
  (jit-jmpr r0))

;; (define-vm-op (compose-continuation st cont)
;;   (local-ref st cont r0)
;;   (scm-program-free-variable-ref r0 r0 0)

;;   ;; XXX: Call to C function not yet complete.
;;   (jit-prepare)
;;   (jit-pushargr reg-thread)
;;   (jit-pushargr r0)
;;   (frame-locals-count r1)
;;   (jit-subi r1 r1 (imm 1))
;;   (jit-pushargr r1)
;;   (jit-pushargi (stored-ref st 1))
;;   (call-c "scm_do_reinstate_partial_continuation")
;;   (jit-retval r0)

;;   ;; Jump to the adress stored in `vm-cont'.
;;   (jit-jmpr r0))

(define-vm-op (tail-apply st)
  (let ((llength (jit-forward))
        (lalloc (jit-forward))
        (lshift (jit-forward))
        (lshuffle (jit-forward))
        (lapply (jit-forward)))

    (vm-handle-interrupts)

    ;; Index for last local, a list containing rest of arguments.
    (last-arg-offset st r2 r0)
    (jit-subi reg-sp reg-sp (imm (* 2 word-size)))

    ;; Local offset for shifting.
    (last-arg-offset st f0 r1)

    ;; Load last local.
    (jit-ldxr r0 reg-fp r2)

    ;; Get list length, increase SP.
    (jit-link llength)
    (jump (scm-is-null r0) lalloc)
    (jit-addi reg-sp reg-sp (imm word-size))
    (scm-cdr r0 r0)
    (jump llength)

    (jit-link lalloc)
    (vm-sync-sp)
    (vm-alloc-frame)

    (jit-movi r1 (imm 0))

    ;; Shift non-list locals.
    (jit-link lshift)
    (jit-addi r0 r1 (imm word-size))
    (jit-ldxr r0 reg-fp r0)
    (jit-stxr r1 reg-fp r0)
    (jit-addi r1 r1 (imm word-size))
    (jump (jit-bler r1 f0) lshift)

    ;; Load last local again.
    (jit-ldxr r0 reg-fp r2)

    ;; Expand list contents to local.
    (jit-link lshuffle)
    (jump (scm-is-null r0) lapply)
    (scm-car f0 r0)
    (jit-stxr r1 reg-fp f0)
    (scm-cdr r0 r0)
    (jit-addi r1 r1 (imm word-size))
    (jump lshuffle)

    ;; Jump to the callee.
    (jit-link lapply)
    (vm-apply)))

;;; XXX: call/cc

(define-vm-op (abort st)
  (let ((ra (jit-movi r0 (imm 0))))
    (jit-str reg-vp r0)

    (jit-prepare)
    (jit-pushargr reg-vp)               ; *vp
    (local-ref st 1 r0)
    (jit-pushargr r0)                   ; tag
    (frame-locals-count r0)
    (jit-subi r0 r0 (imm 2))
    (jit-pushargr r0)                   ; nstack
    (jit-addi r0 reg-fp (imm (* 2 word-size)))
    (jit-pushargr r0)                   ; *stack_args
    (jit-pushargi scm-eol)              ; tail
    (jit-pushargr reg-fp)               ; *sp
    (jit-pushargr reg-registers)        ; registers
    (call-c "scm_do_vm_abort")

    ;; Should not reach here.
    (jit-prepare)
    (call-c "abort")

    ;; Return address for captured vmcont.
    (jit-patch ra)))

;; (define-vm-op (abort st)
;;   ;; Retuan address for partial continuation, used when reifying
;;   ;; the continuation.
;;   (let ((ra (jit-movi r1 (imm 0))))
;;     (jit-prepare)
;;     (jit-pushargr reg-thread)
;;     (local-ref st 1 r0)
;;     (jit-pushargr r0)
;;     (frame-locals-count r0)
;;     (jit-subi r0 r0 (imm 2))
;;     (jit-pushargr r0)
;;     (jit-addi r0 reg-fp (imm (* 2 word-size)))
;;     (jit-pushargr r0)
;;     (jit-pushargr r1)
;;     (call-c "scm_do_abort")
;;     (jit-retval r0)

;;     ;; Load values set in C function: address of handler, reg-sp, and
;;     ;; reg-fp.
;;     (scm-cell-object r1 r0 0)
;;     (scm-cell-object reg-sp r0 1)
;;     (scm-cell-object reg-fp r0 2)

;;     ;; Jump to the handler.
;;     (jit-jmpr r1)

;;     ;; The address for partial continuation to return.
;;     (jit-patch ra)))

(define-vm-op (builtin-ref st dst src)
  (jit-prepare)
  (jit-pushargi (imm src))
  (call-c "scm_do_vm_builtin_ref")
  (jit-retval r0)
  (local-set! st dst r0))


;;; Function prologues
;;; ------------------

(define-br-nargs-op (br-if-nargs-ne st expected offset)
  jit-bnei)

(define-br-nargs-op (br-if-nargs-lt st expected offset)
  jit-blti)

(define-br-nargs-op (br-if-nargs-gt st expected offset)
  jit-bgti)

(define-vm-op (assert-nargs-ee st expected)
  (assert-wrong-num-args st jit-beqi expected 0))

(define-vm-op (assert-nargs-ge st expected)
  (assert-wrong-num-args st jit-bgei expected 0))

(define-vm-op (assert-nargs-le st expected)
  (assert-wrong-num-args st jit-blei expected 0))

(define-vm-op (alloc-frame st nlocals)
  (let ((lshuffle (jit-forward))
        (lexit (jit-forward)))

    (frame-locals-count r1)
    (vm-alloc-frame nlocals)
    (jit-lshi r1 r1 (imm word-size-length))
    (jit-movi r2 (imm (* nlocals word-size)))
    (jit-movi r0 scm-undefined)

    ;; Using r2 as offset of location to store.
    (jit-link lshuffle)
    (jump (jit-bger r1 r2) lexit)
    (jit-subi r2 r2 (imm word-size))
    (jit-stxr r2 reg-fp r0)
    (jump lshuffle)

    (jit-link lexit)))

(define-vm-op (reset-frame st nlocals)
  (vm-reset-frame nlocals))

(define-vm-op (assert-nargs-ee/locals st expected locals)
  ;; XXX: Refill SCM_UNDEFINED?
  (assert-wrong-num-args st jit-beqi expected 0)
  (vm-alloc-frame (+ expected locals)))

;;; XXX: br-if-npos-gt

(define-vm-op (bind-kwargs st nreq flags nreq-and-opt ntotal kw-offset)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr reg-fp)
  (frame-locals-count r0)
  (jit-pushargr r0)
  (jit-pushargi (imm (+ (lightning-pc st) (* (lightning-ip st) 4))))
  (jit-pushargi (imm nreq))
  (jit-pushargi (imm flags))
  (jit-pushargi (imm nreq-and-opt))
  (jit-pushargi (imm ntotal))
  (jit-pushargi (imm kw-offset))
  (call-c "scm_do_bind_kwargs")
  (jit-retval r0)

  ;; Allocate frame with returned value.
  (jit-lshi r0 r0 (imm word-size-length))
  (jit-addr reg-sp reg-fp r0)
  (vm-sync-sp)
  (vm-alloc-frame))

(define-vm-op (bind-rest st dst)
  (let ((lrefill (jit-forward))
        (lprecons (jit-forward))
        (lcons (jit-forward))
        (lreset (jit-forward))
        (lexit (jit-forward)))

    ;; Initialize some values.
    (jit-subr r2 reg-sp reg-fp)
    (jit-addi r2 r2 (imm word-size)) ; r2 = last local index.
    (jit-movi r1 scm-eol)            ; r1 = initial list.
    (jit-movi f0 scm-undefined)
    (jump (jit-bgti r2 (imm (* dst word-size))) lprecons)

    ;; Refill the locals with SCM_UNDEFINED.
    (vm-alloc-frame (+ dst 1))

    (jit-link lrefill)
    (jump (jit-bgei r2 (imm (* dst word-size))) lexit)
    (jit-stxr r2 reg-fp f0)
    (jit-addi r2 r2 (imm word-size))
    (jump lrefill)

    ;; Create a list.
    (jit-link lprecons)
    (jit-movr r0 r1)
    (jit-movi r1 scm-false)

    (jit-link lcons)
    (jump (jit-blei r2 (imm (* dst word-size))) lreset)
    (jit-subi r2 r2 (imm word-size))
    (jit-ldxr r1 reg-fp r2)
    (scm-inline-cons r0 r1 r0)
    (jit-stxr r2 reg-fp f0)
    (jump lcons)

    (jit-link lreset)
    (jit-movr r1 r0)
    (vm-reset-frame (+ dst 1))

    (jit-link lexit)
    (local-set! st dst r1)))


;;; Branching instructions
;;; ----------------------

(define-vm-op (br st dst)
  (when (< dst 0)
    (vm-handle-interrupts))
  (jump (resolve-dst st dst)))

(define-vm-br-unary-immediate-op (br-if-true st a invert offset)
  ((if invert jit-beqi jit-bnei)
   (local-ref st a)
   scm-false))

(define-vm-br-unary-immediate-op (br-if-null st a invert offset)
  ((if invert jit-bnei jit-beqi)
   (local-ref st a)
   scm-eol))

(define-vm-op (br-if-nil st a invert offset)
  (when (< offset 0)
    (vm-handle-interrupts))
  (local-ref st a r0)
  (jit-movi r1 (imm (logior (pointer-address scm-false)
                            (pointer-address scm-eol))))
  (jit-comr r1 r1)
  (jit-andr r1 r0 r1)
  (jump ((if invert jit-bnei jit-beqi) r1 scm-false)
        (resolve-dst st offset)))

(define-vm-br-unary-heap-object-op (br-if-pair st a invert offset)
  r0 (jit-bmsi r0 (imm 1)))

(define-vm-br-unary-heap-object-op (br-if-struct st a invert offset)
  r0 (begin (scm-typ3 r0 r0)
            (jit-bnei r0 (imm 1))))

(define-vm-br-unary-immediate-op (br-if-char st a invert offset)
  ((if invert jit-bnei jit-beqi)
   (begin (local-ref st a r0)
          (jit-andi r0 r0 (imm #xff))
          r0)
   (imm 12)))

(define-vm-op (br-if-tc7 st a invert tc7 offset)
  (when (< offset 0)
    (vm-handle-interrupts))
  (let ((lexit (jit-forward)))
    (local-ref st a r0)
    (jump (scm-imp r0) (if invert (resolve-dst st offset) lexit))
    (scm-cell-type r0 r0)
    (scm-typ7 r0 r0)
    (jump (scm-is-nei r0 tc7) (if invert (resolve-dst st offset) lexit))
    (when (not invert)
      (jump (resolve-dst st offset)))
    (jit-link lexit)))

(define-vm-op (br-if-eq st a b invert offset)
  (when (< offset 0)
    (vm-handle-interrupts))
  (jump ((if invert jit-bner jit-beqr)
         (local-ref st a r0)
         (local-ref st b r1))
        (resolve-dst st offset)))

(define-vm-br-binary-op (br-if-eqv st a b invert offset)
  "scm_eqv_p")

(define-vm-br-binary-op (br-if-equal st a b invert offset)
  "scm_equal_p")

(define-vm-br-arithmetic-op (br-if-= st a b invert offset)
  jit-beqr jit-bner jit-beqr-d jit-bner-d "scm_num_eq_p")

(define-vm-br-arithmetic-op (br-if-< st a b invert offset)
  jit-bltr jit-bger jit-bltr-d jit-bunltr-d "scm_less_p")

(define-vm-br-arithmetic-op (br-if-<= st a b invert offset)
  jit-bler jit-bgtr jit-bler-d jit-bunler-d "scm_leq_p")

(define-vm-op (br-if-logtest st a b invert offset)
  (let ((lcall (jit-forward))
        (lexit (jit-forward)))
    (local-ref st a r0)
    (local-ref st b r1)
    (jump (scm-not-inump r0) lcall)
    (jump (scm-not-inump r1) lcall)
    (jit-andr r0 r0 r1)
    (jit-andi r0 r0 (imm #xfffffffffffffffd))
    (jump ((if invert jit-beqi jit-bnei) r0 (imm 0))
          (resolve-dst st offset))
    (jump lexit)

    (jit-link lcall)
    (jit-prepare)
    (jit-pushargr r0)
    (jit-pushargr r1)
    (call-c "scm_logtest")
    (jit-retval r0)
    (jump (if invert (scm-is-false r0) (scm-is-true r0))
          (resolve-dst st offset))

    (jit-link lexit)))


;;; Lexical binding instructions
;;; ----------------------------

(define-vm-op (mov st dst src)
  (local-set! st dst (local-ref st src)))

;;; XXX: long-mov

(define-vm-op (box st dst src)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargi (imm tc7-variable))
  (jit-pushargr (local-ref st src))
  (call-c "scm_do_inline_cell")
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (box-ref st dst src)
  (local-ref st src r0)
  (scm-cell-object r0 r0 1)
  (local-set! st dst r0))

(define-vm-op (box-set! st dst src)
  (local-ref st dst r0)
  (local-ref st src r1)
  (scm-set-cell-object r0 1 r1))

(define-vm-op (make-closure st dst offset nfree)
  (let ((lnext (jit-forward)))
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-pushargi (imm (logior tc7-program
                               scm-f-program-is-jit-compiled
                               (ash nfree 16))))
    (jit-pushargi (imm (+ nfree 3)))
    (call-c "scm_do_inline_words")
    (jit-retval r0)

    ;; Storing address of byte-compiled code.
    (jit-movi r1 (imm (offset-addr st offset)))
    (scm-set-cell-object r0 1 r1)

    ;; Storing address of native code.
    (let ((closure-addr (jit-movi r1 (imm 0))))
      (scm-set-cell-object r0 2 r1)
      (jump lnext)

      ;; Do the JIT compilation at the time of closure creation.
      (let* ((cfg (procedure->cfg (offset-addr st offset)))
             (lightning (make-lightning cfg
                                        (lightning-nodes st)
                                        (offset-addr st offset))))
        (jit-patch closure-addr)
        (assemble-lightning lightning (jit-forward))))

    (jit-link lnext)
    (when (< 0 nfree)
      (jit-movi r1 scm-false)
      (for-each (lambda (n)
                  (scm-program-free-variable-set r0 n r1))
                (iota nfree)))
    (local-set! st dst r0)))

(define-vm-op (free-ref st dst src idx)
  (local-ref st src r0)
  (scm-program-free-variable-ref r0 r0 idx)
  (local-set! st dst r0))

(define-vm-op (free-set! st dst src idx)
  (local-ref st dst r0)
  (local-ref st src r1)
  (scm-program-free-variable-set r0 idx r1))


;;; Immediates and statically allocated non-immediates
;;; --------------------------------------------------

(define-vm-op (make-short-immediate st dst a)
  (local-set!/immediate st dst (imm a)))

(define-vm-op (make-long-immediate st dst a)
  (local-set!/immediate st dst (imm a)))

(define-vm-op (make-long-long-immediate st dst hi lo)
  (jit-movi r0 (imm hi))
  (jit-movi r1 (imm lo))
  (jit-lshi r0 r0 (imm 32))
  (jit-orr r0 r0 r1)
  (local-set! st dst r0))

(define-vm-op (make-non-immediate st dst offset)
  (jit-movi r0 (imm (offset-addr st offset)))
  (local-set! st dst r0))

(define-vm-op (static-ref st dst offset)
  (jit-ldi r0 (imm (offset-addr st offset)))
  (local-set! st dst r0))

;;; XXX: static-set!
;;; XXX: static-patch!


;;; Mutable top-level bindings
;;; --------------------------

(define-vm-op (current-module st dst)
  (jit-prepare)
  (call-c "scm_current_module")
  (jit-retval r0)
  (local-set! st dst r0))

;;; XXX: resolve
;;; XXX: define!

(define-vm-box-op (toplevel-box st mod-offset sym-offset)
  (module-variable
   (or (dereference-scm (make-pointer (offset-addr st mod-offset)))
       the-root-module)
   (dereference-scm (make-pointer (offset-addr st sym-offset)))))

(define-vm-box-op (module-box st mod-offset sym-offset)
  (module-variable
   (resolve-module
    (cdr (pointer->scm (make-pointer (offset-addr st mod-offset)))))
   (dereference-scm (make-pointer (offset-addr st sym-offset)))))


;;; The dynamic environment
;;; -----------------------

;;; XXX: prompt
;;;
;;; In vm-regular, prompt is pushed to dynstack with VM operation
;;; "prompt".  The pushed dynstack has flag, tag, fp offset, sp offset,
;;; handler's IP, and register.  The C function doing the actual work
;;; for push is `scm_dynstack_push_prompt', written in "dynstack.c".
;;;
;;; Pushed prompt is referred from VM operation "abort", which is
;;; written in VM builtin code `abort-to-prompt'. The pushed dynstack
;;; entry is retrieved via `scm_dynstack_find_prompt', which is called
;;; from `scm_c_abort', which is called from `vm_abort'.
;;;
;;; In `scm_c_abort', if the prompt is not escape only, continuation is
;;; reified with `reify_partial_continuation'. Also, vp->fp, vp->sp, and
;;; vp->ip are set from prompt's value. Then the function does
;;; SCM_I_LONGJMP with `regisers' value.
;;;
;;; * C functions in VM operation "prompt":
;;;
;;; - dynstack.c:
;;;   scm_dynstack_push_prompt (scm_t_dynstack *dynstack,
;;;                             scm_t_dynstack_prompt_flags flags,
;;;                             SCM key,
;;;                             scm_t_ptrdiff fp_offset,
;;;                             scm_t_ptrdiff sp_offset,
;;;                             scm_t_uint32 *ip,
;;;                             scm_i_jmp_buf *registers);
;;;
;;; In vm-engine.c, arguments `dynstack', `flags', `key' are not so
;;; difficult, not much differ from other VM ops.  `fp_offset' is `fp -
;;; vp->stack_base', and `sp_offset' is `LOCAL_ADDRESS (proc_slot) -
;;; vp->stack_base'. ip is `ip + offset', which is next IP to jump in
;;; vm-regular interpreter, but fragment of code to compile in
;;; vm-lightning. `registers' is the one passed from `scm_call_n'.
;;;
;;; * C functions in VM operation "abort":
;;;
;;; - vm.c:
;;;   vm_abort (struct scm_vm *vp,
;;;             SCM tag,
;;;             size_t nstack,
;;;             SCM *stack_args,
;;;             SCM tail,
;;;             SCM *sp,
;;;             scm_i_jmp_buf *current_registers)
;;;
;;; `vm_abort' is called in VM operation "abort". The first argument
;;; `*vp' is `vp' in "vm-engine.c".  `tag' is in vm's local.  nstack is
;;; referred from FRAME_LOCALS_COUNT. `*stack_args' is `LOCAL_ADDRESS
;;; (2)'. `tail' is SCM_EOL, `*sp' is `LOCAL_ADDRESS (0)'. Finally,
;;; `*current_registers' is `registers'.
;;;
;;; - control.c:
;;;   scm_c_abort (struct scm_vm *vp,
;;;                SCM tag,
;;;                size_t n,
;;;                SCM *argv,
;;;                scm_i_jmp_buf *current_registers)
;;;
;;; `scm_c_abort' is called from `vm_abort'. `*vp', `tag', and
;;; `*current_registers' are the same argument passed to 'vm_abort' from
;;; vm-engine, `n' is `nstack' in vm_abort + length of tail, `argv' is
;;; an array constructed from `stack_args' in "vm_abort". `vp->sp' is
;;; set to `sp' in "vm_abort"'s argument before calling "scm_c_abort".
;;;
;;; * SCM_I_LONGJMP
;;;
;;; `SCM_I_LONGJMP' is called in the end of `scm_c_abort'.
;;; `SCM_I_SETJMP' is called at near the end of `scm_call_n', just
;;; before calling the implementation.

(define-vm-op (prompt st tag escape-only? proc-slot handler-offset)
  (let ((handler-addr (jit-movi r1 (imm 0)))
        (flags (if escape-only?
                   scm-dynstack-prompt-escape-only
                   0)))

    ;; Store address of handler-offset's bytecode IP.
    (hashq-set! (lightning-handlers st)
                (+ (lightning-ip st) handler-offset)
                handler-addr)

    (jit-prepare)
    (jit-pushargr reg-thread)           ; thread
    (jit-pushargi (imm flags))          ; flags
    (local-ref st tag r0)
    (jit-pushargr r0)                   ; key
    (vm-stack-base r2)
    (jit-subr r0 reg-fp r2)
    (jit-rshi r0 r0 (imm word-size-length))
    (jit-pushargr r0)                   ; fp_offset
    (jit-addi r0 r0 (imm proc-slot))
    (jit-pushargr r0)                   ; sp_offset
    (jit-pushargr r1)                   ; ip
    (jit-pushargr reg-registers)        ; `registers', from arguments.
    (call-c "scm_do_dynstack_push_prompt")))

(define-vm-op (wind st winder unwinder)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr (local-ref st winder))
  (jit-pushargr (local-ref st unwinder))
  (call-c "scm_do_dynstack_push_dynwind"))

(define-vm-op (unwind st)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (call-c "scm_do_dynstack_pop"))

(define-vm-op (push-fluid st fluid value)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr (local-ref st fluid))
  (jit-pushargr (local-ref st value))
  (call-c "scm_do_dynstack_push_fluid"))

(define-vm-op (pop-fluid st)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (call-c "scm_do_unwind_fluid"))

(define-vm-op (fluid-ref st dst src)
  (let ((lexit (jit-forward)))
    (local-ref st src r0)

    ;; r0 = fluids, in thread:
    ;;   thread->dynamic_state, then
    ;;   SCM_I_DYNAMIC_STATE_FLUIDS (dynstack)
    ;;   (i.e. SCM_CELL_WORD_1 (dynstack))
    (vm-thread-dynamic-state r0)
    (scm-cell-object r0 r0 1)

    ;; r1 = fluid, from local:
    (local-ref st src r1)

    ;; r2 = num, vector index.
    (scm-cell-object r2 r1 0)
    (jit-rshi r2 r2 (imm 8))
    (jit-addi r2 r2 (imm 1))
    ;; (jit-muli r2 r2 (imm 8))
    (jit-lshi r2 r2 (imm 3))

    ;; r0 = fluid value
    (jit-ldxr r0 r0 r2)

    ;; Load default value from local fluid if not set.
    (jump (scm-not-unbndp r0) lexit)
    (scm-cell-object r0 r1 1)

    (jit-link lexit)
    (local-set! st dst r0)))

;;; XXX: fluid-set

;;; String, symbols, and keywords
;;; -----------------------------

(define-vm-op (string-length st dst src)
  (let ((lcall (jit-forward))
        (lexit (jit-forward)))
    (local-ref st src r0)
    (jump (scm-imp r0) lcall)
    (scm-cell-type r1 r0)
    (scm-typ7 r1 r1)
    (jump (scm-is-not-string r1) lcall)
    (scm-i-string-length r0 r0)
    (scm-makinumr r0 r0)
    (jump lexit)

    (jit-link lcall)
    (jit-pushargr r0)
    (call-c "scm_string_length")
    (jit-retval r0)

    (jit-link lexit)
    (local-set! st dst r0)))

;;; XXX: Native code not inlined. Need to add test to see string width,
;;; test to see whether string is shared, do `get_str_buf_start',
;;; ...etc.
(define-vm-op (string-ref st dst src idx)
  (local-ref st src r0)
  (local-ref st idx r1)
  (jit-prepare)
  (jit-pushargr r0)
  (jit-pushargr r1)
  (call-c "scm_string_ref")
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (string->number st dst src)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (jit-pushargi scm-undefined)
  (call-c "scm_string_to_number")
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (string->symbol st dst src)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (call-c "scm_string_to_symbol")
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (symbol->keyword st dst src)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (call-c "scm_symbol_to_keyword")
  (jit-retval r0)
  (local-set! st dst r0))


;;; Pairs
;;; -----

(define-vm-op (cons st dst car cdr)
  (local-ref st car r0)
  (local-ref st cdr r1)
  (scm-inline-cons r0 r0 r1)
  (local-set! st dst r0))

(define-vm-op (car st dst src)
  (local-ref st src r0)
  (validate-pair r0 r1 *car-string 1)
  (local-set! st dst r1))

(define-vm-op (cdr st dst src)
  (local-ref st src r0)
  (validate-pair r0 r1 *cdr-string 1)
  (scm-cdr r0 r0)
  (local-set! st dst r0))

(define-vm-op (set-car! st pair car)
  (local-ref st pair r0)
  (validate-pair r0 r1 *set-car!-string 1)
  (local-ref st car r1)
  (scm-set-cell-object r0 0 r1))

(define-vm-op (set-cdr! st pair cdr)
  (local-ref st pair r0)
  (validate-pair r0 r1 *set-cdr!-string 1)
  (local-ref st cdr r1)
  (scm-set-cell-object r0 1 r1))


;;; Numeric operations
;;; ------------------

(define-vm-binary-numeric-op (add st dst a b)
  "scm_sum" lcall r1 r2 f0 f1
  (begin
    (jit-movr r0 r1)
    (jump (jit-boaddr r0 r2) lcall)
    (jit-subi r0 r0 (imm 2)))
  (jit-addr-d f0 f0 f1))

(define-vm-unary-step-op (add1 st dst src)
  "scm_sum" jit-boaddi jit-addr-d)

(define-vm-binary-numeric-op (sub st dst a b)
  "scm_difference" lcall r1 r2 f0 f1
  (begin
    (jit-movr r0 r1)
    (jump (jit-bosubr r0 r2) lcall)
    (jit-addi r0 r0 (imm 2)))
  (jit-subr-d f0 f0 f1))

(define-vm-unary-step-op (sub1 st dst src)
  "scm_difference" jit-bosubi jit-subr-d)

(define-vm-binary-numeric-op (mul st dst a b)
  "scm_product" lcall r1 r2 f0 f1
  (begin
    (scm-i-inumr r0 r1)
    (scm-i-inumr f0 r2)
    (jit-qmulr r0 f0 r0 f0)
    (jump (jit-bnei f0 (imm 0)) lcall)
    (jump (jit-bgti r0 (imm most-positive-fixnum)) lcall)
    (scm-makinumr r0 r0))
  (jit-mulr-d f0 f0 f1))

(define-vm-binary-numeric-op (div st dst a b)
  "scm_divide" lcall r1 r2 f0 f1
  (begin
    (scm-i-inumr r0 r1)
    (scm-i-inumr f0 r2)
    (jit-qdivr r0 f0 r0 f0)
    (jump (jit-bnei f0 (imm 0)) lcall)
    (scm-makinumr r0 r0))
  (jit-divr-d f0 f0 f1))

(define-vm-binary-numeric-op (quo st dst a b)
  "scm_quotient" lcall r0 r1
  (begin
    (scm-i-inumr r0 r0)
    (scm-i-inumr r1 r1)
    (jit-divr r0 r0 r1)
    (scm-makinumr r0 r0)))

(define-vm-binary-numeric-op (rem st dst a b)
  "scm_remainder" lcall r0 r1
  (begin
    (scm-i-inumr r0 r0)
    (scm-i-inumr r1 r1)
    (jit-remr r0 r0 r1)
    (scm-makinumr r0 r0)))

(define-vm-binary-numeric-op (mod st dst a b)
  "scm_modulo" lcall r0 r1
  (let ((lpositive (jit-forward))
        (lnegative (jit-forward))
        (lexit (jit-forward)))
    (scm-i-inumr r0 r0)
    (scm-i-inumr r1 r1)
    (jit-remr r0 r0 r1)
    (jump (jit-bgei r0 (imm 0)) lpositive)
    (jump lnegative)

    (jit-link lpositive)
    (jump (jit-bgei r1 (imm 0)) lexit)
    (jit-addr r0 r0 r1)
    (jump lexit)

    (jit-link lnegative)
    (jump (jit-blti r1 (imm 0)) lexit)
    (jit-addr r0 r0 r1)
    (jump lexit)

    (jit-link lexit)
    (scm-makinumr r0 r0)))

(define-vm-binary-numeric-op (ash st dst a b)
  "scm_ash" lcall r0 r1
  (let ((lright (jit-forward))
        (lleft (jit-forward))
        (ladjust (jit-forward))
        (lprepare (jit-forward))
        (lexit (jit-forward)))
    (scm-i-inumr r0 r0)
    (scm-i-inumr r1 r1)
    (jump (jit-bgti r1 (imm 0)) lleft)

    (jit-negr r1 r1)
    (jump (jit-bgei r1 (imm (- scm-i-fixnum-bit 1))) lright)
    (jit-rshr r0 r0 r1)
    (scm-makinumr r0 r0)
    (jump lexit)

    (jit-link lright)
    (jit-rshi r0 r0 (imm (- scm-i-fixnum-bit 1)))
    (scm-makinumr r0 r0)
    (jump lexit)

    (jit-link lleft)
    (jump (jit-bgei r1 (imm (- scm-i-fixnum-bit 1))) lprepare)
    (jit-movi r2 (imm (- scm-i-fixnum-bit 1)))
    (jit-subr r2 r2 r1)
    (jit-rshr-u r2 r0 r2)
    (jump (jit-bgti r2 (imm 0)) lprepare)
    (jump (jit-blti r0 (imm 0)) ladjust)
    (jit-lshr r0 r0 r1)
    (scm-makinumr r0 r0)
    (jump lexit)

    (jit-link ladjust)
    (jit-negr r0 r0)
    (jit-lshr r0 r0 r1)
    (jit-negr r0 r0)
    (scm-makinumr r0 r0)
    (jump lexit)

    (jit-link lprepare)
    (local-ref st a r0)
    (local-ref st b r1)
    (jump lcall)

    (jit-link lexit)
    (local-set! st dst r0)))

(define-vm-binary-numeric-op (logand st dst a b)
  "scm_logand" lcall r0 r1
  (jit-andr r0 r0 r1))

(define-vm-binary-numeric-op (logior st dst a b)
  "scm_logior" lcall r0 r1
  (jit-orr r0 r0 r1))

(define-vm-binary-numeric-op (logxor st dst a b)
  "scm_logxor" lcall r0 r1
  (begin
    (scm-i-inumr r0 r0)
    (scm-i-inumr r1 r1)
    (jit-orr r0 r0 r1)
    (scm-makinumr r0 r0)))

(define-vm-op (make-vector st dst length init)
  (jit-prepare)
  (jit-pushargr (local-ref st length))
  (jit-pushargr (local-ref st init))
  (call-c "scm_make_vector")
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (make-vector/immediate st dst length init)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargi (imm (logior tc7-vector (ash length 8))))
  (jit-pushargi (imm (+ length 1)))
  (call-c "scm_do_inline_words")
  (jit-retval r0)
  (local-ref st init r1)
  (for-each (lambda (n)
              (scm-set-cell-object r0 (+ n 1) r1))
            (iota length))
  (local-set! st dst r0))

(define-vm-op (vector-length st dst src)
  (local-ref st src r0)
  (validate-vector r0 r1 r2 *vector-length-string)
  (jit-rshi r1 r1 (imm 8))
  (scm-makinumr r1 r1)
  (local-set! st dst r1))

(define-vm-op (vector-ref st dst src idx)
  (local-ref st src r0)
  (validate-vector r0 r1 r2 *vector-ref-string)
  (local-ref st idx r1)
  (validate-vector-range r0 r1 r2 f0 *vector-ref-string)
  (scm-i-inumr r1 r1)
  (jit-addi r1 r1 (imm 1))
  (jit-lshi r1 r1 (imm word-size-length))
  (scm-cell-object-r r0 r0 r1)
  (local-set! st dst r0))

(define-vm-op (vector-ref/immediate st dst src idx)
  (local-ref st src r0)
  (validate-vector r0 r1 r2 *vector-ref-string)
  (validate-vector-range/immediate r0 idx r1 *vector-ref-string)
  (scm-cell-object r0 r0 (+ idx 1))
  (local-set! st dst r0))

(define-vm-op (vector-set! st dst idx src)
  (local-ref st dst r0)
  (validate-vector r0 r1 r2 *vector-set!-string)
  (local-ref st idx r1)
  (validate-vector-range r0 r1 r2 f0 *vector-set!-string)
  (scm-i-inumr r1 r1)
  (jit-addi r1 r1 (imm 1))
  (jit-lshi r1 r1 (imm word-size-length))
  (local-ref st src r2)
  (scm-set-cell-object-r r0 r1 r2))

(define-vm-op (vector-set!/immediate st dst idx src)
  (local-ref st dst r0)
  (validate-vector r0 r1 r2 *vector-set!-string)
  (validate-vector-range/immediate r0 idx r1 *vector-set!-string)
  (local-ref st src r1)
  (scm-set-cell-object r0 (+ idx 1) r1))


;;; Structs and GOOPS
;;; -----------------

(define-vm-op (struct-vtable st dst src)
  (local-ref st src r0)
  (validate-struct r0 r1 *struct-vtable-string)
  (scm-struct-vtable r0 r0)
  (local-set! st dst r0))

(define-vm-op (allocate-struct st dst vtable nfields)
  (local-ref st vtable r0)
  (local-ref st vtable r1)
  (jit-prepare)
  (jit-pushargr r0)
  (jit-pushargr r1)
  (jit-calli (program-free-variable-ref allocate-struct 0))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (allocate-struct/immediate st dst vtable nfields)
  (local-ref st vtable r0)
  (jit-prepare)
  (jit-pushargr r0)
  (jit-pushargi (scm-makinumi nfields))
  (jit-calli (program-free-variable-ref allocate-struct 0))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (struct-ref st dst src idx)
  ;; XXX: Validate struct flag.
  (local-ref st src r0)
  (validate-struct r0 r1 *struct-ref-string)
  (local-ref st idx r1)
  (scm-i-inumr r1 r1)
  (jit-lshi r1 r1 (imm word-size-length))
  (scm-struct-slots r0 r0)
  (scm-cell-object-r r0 r0 r1)
  (local-set! st dst r0))

(define-vm-op (struct-ref/immediate st dst src idx)
  ;; XXX: Validate struct flag.
  (local-ref st src r0)
  (validate-struct r0 r1 *struct-ref-string)
  (scm-struct-slots r0 r0)
  (scm-cell-object r0 r0 idx)
  (local-set! st dst r0))

(define-vm-op (struct-set! st dst idx src)
  ;; XXX: Validate struct flag.
  (local-ref st dst r0)
  (validate-struct r0 r1 *struct-set!-string)
  (local-ref st src r1)
  (local-ref st idx r2)
  (scm-cell-object r0 r0 1)
  (scm-set-cell-object-r r0 r2 r1))

(define-vm-op (struct-set!/immediate st dst idx src)
  ;; XXX: Validate struct flag.
  (local-ref st dst r0)
  (validate-struct r0 r1 *struct-set!-string)
  (local-ref st src r1)
  (scm-cell-object r0 r0 1)
  (scm-set-cell-object r0 idx r1))

(define-vm-op (class-of st dst type)
  (let ((lcall (jit-forward))
        (lexit (jit-forward)))
    (local-ref st type r0)
    (jump (scm-imp r0) lcall)
    (scm-cell-type r1 r0)
    (scm-typ3 r1 r1)
    (jump (scm-not-structp r1) lcall)
    (scm-struct-vtable-slots r1 r0)
    (scm-cell-object r2 r1 scm-vtable-index-flags)
    (jit-andi r2 r2 (imm scm-classf-goops))
    (jump (jit-beqi r2 (imm 0)) lcall)
    (scm-cell-object r0 r1 scm-vtable-index-self)
    (jump lexit)

    (jit-link lcall)
    (jit-prepare)
    (jit-pushargr r0)
    (call-c "scm_class_of")
    (jit-retval r0)

    (jit-link lexit)
    (local-set! st dst r0)))


;;; Arrays, packed uniform arrays, and bytevectors
;;; ----------------------------------------------

;;; XXX: load-typed-array
;;; XXX: make-array

;;; XXX: u64 and s64 ops are for 64bit architectures only.

(define-vm-bv-ref-op bv-u8-ref jit-ldxr-uc)
(define-vm-bv-ref-op bv-s8-ref jit-ldxr-c)
(define-vm-bv-ref-op bv-u16-ref jit-ldxr-us)
(define-vm-bv-ref-op bv-s16-ref jit-ldxr-s)
(define-vm-bv-ref-op bv-u32-ref jit-ldxr-ui)
(define-vm-bv-ref-op bv-s32-ref jit-ldxr-i)
(define-vm-bv-ref-op bv-u64-ref jit-ldxr-l "scm_from_uint64")
(define-vm-bv-ref-op bv-s64-ref jit-ldxr-l "scm_from_int64")
(define-vm-bv-ref-op bv-f32-ref "float")
(define-vm-bv-ref-op bv-f64-ref "double")

(define-vm-bv-set-op bv-u8-set! jit-stxr-c)
(define-vm-bv-set-op bv-s8-set! jit-stxr-c)
(define-vm-bv-set-op bv-u16-set! jit-stxr-s)
(define-vm-bv-set-op bv-s16-set! jit-stxr-s)
(define-vm-bv-set-op bv-u32-set! jit-stxr-i)
(define-vm-bv-set-op bv-s32-set! jit-stxr-i)
(define-vm-bv-set-op bv-u64-set! jit-stxr-l "scm_to_uint64")
(define-vm-bv-set-op bv-s64-set! jit-stxr-l "scm_to_int64")
(define-vm-bv-set-op bv-f32-set! "float")
(define-vm-bv-set-op bv-f64-set! "double")


;;;
;;; Compilation
;;;

(define-syntax-rule (with-jit-state . expr)
  (parameterize ((jit-state (jit-new-state)))
    (call-with-values (lambda () . expr)
      (lambda vals
        (jit-destroy-state)
        (apply values vals)))))

(define-syntax-rule (write-code-to-file file pointer)
  (call-with-output-file file
    (lambda (port)
      (put-bytevector port (pointer->bytevector pointer (jit-code-size))))))

(define space-string ": ")

(define (assemble-lightning st entry)
  "Assemble with STATE, using ENTRY as entry of the result."
  (define-syntax-rule (destination-label st)
    (hashq-ref (lightning-labels st) (lightning-ip st)))

  (define-syntax-rule (destination-handler st)
    (hashq-ref (lightning-handlers st) (lightning-ip st)))

  (define-syntax-rule (scm-display obj)
    (begin
      (jit-prepare)
      (jit-pushargi (scm->pointer obj))
      (jit-pushargi scm-undefined)
      (call-c "scm_display")))

  (define-syntax-rule (assemble-one st ip-x-op)
    (let* ((ip (car ip-x-op))
           (op (cdr ip-x-op))
           (instr (car op))
           (args (cdr op)))
      (set-lightning-ip! st ip)
      (let ((emitter (hashq-ref *vm-instr* instr)))
        (let ((verbosity (lightning-verbosity)))
          (when (and verbosity (<= 3 verbosity))
            (jit-note (format #f "~a" op) (lightning-ip st)))

          (when (and verbosity (<= 5 verbosity))
            (scm-display (offset-addr st 0))
            (scm-display space-string)
            (scm-display op)
            (jit-prepare)
            (jit-pushargi scm-undefined)
            (call-c "scm_newline")))

        ;; Link if this bytecode intruction is labeled as destination,
        ;; or patch it for prompt handler.
        (cond ((destination-label st)
               =>
               (lambda (node) (jit-link node)))
              ((destination-handler st)
               =>
               (lambda (node) (jit-patch node))))
        (or (and emitter (apply emitter st args))
            (debug 0 "compile-lightning: VM op `~a' not found~%" instr)))))

  (let* ((program-or-addr (lightning-pc st))
         (addr (ensure-program-addr program-or-addr))
         (cfg (lightning-cfg st))
         (name (cfg-name cfg)))

    (hashq-set! (lightning-nodes st) addr entry)

    (let ((verbosity (lightning-verbosity)))
      (when (and verbosity (<= 3 verbosity))
        (jit-note name addr)))

    ;; Link and compile the entry point.
    (jit-link entry)
    (jit-patch entry)
    (let lp ((ops (cfg-ops cfg)))
      (unless (null? ops)
        (assemble-one st (car ops))
        (lp (cdr ops))))

    entry))

(define (compile-lightning proc)
  "Compile bytecode of procedure PROC to native code, and save the
compiled result."
  (debug 1 ";;; compiling ~a (~a)~%"
         (try-program-name proc) (ensure-program-addr proc))
  (with-jit-state
   (jit-prolog)
   (let ((entry (jit-forward))
         (lightning (make-lightning (procedure->cfg proc)
                                    (make-hash-table)
                                    (ensure-program-addr proc))))
     (assemble-lightning lightning entry)
     (jit-epilog)
     (jit-realize)
     (let* ((estimated-code-size (jit-code-size))
            (bv (make-bytevector estimated-code-size)))
       (jit-set-code (bytevector->pointer bv)
                     (imm estimated-code-size))
       (jit-emit)

       ;; XXX: Generated codes never get freed.
       (jit-code-guardian bv)

       (set-jit-compiled-code! proc (jit-address entry))
       (make-bytevector-executable! bv)

       ;; Debug output.
       (let ((verbosity (lightning-verbosity)))
         (when (and verbosity (<= 2 verbosity))
           (format #t ";;; nodes:~%")
           (hash-for-each (lambda (k v)
                            (format #t ";;;   ~a => ~a~%" k v))
                          (lightning-nodes lightning)))
         (when (and verbosity (<= 3 verbosity))
           (write-code-to-file (format #f "/tmp/~a.o" (procedure-name proc))
                               (bytevector->pointer bv))
           (jit-print)
           (jit-clear-state)))))))

(define %compile-lightning
  (let ((f (lambda (proc*)
             (let ((proc (pointer->scm proc*)))
               (compile-lightning proc)))))
    (procedure->pointer void f '(*))))


;;;
;;; Runtime
;;;

;;; Size of bytevector to contain native code for `run-lightning'.
(define-inline run-lightning-code-size 4096)

;;; Bytevector to contain native code for `run-lightning'.
(define run-lightning-code
  (make-bytevector run-lightning-code-size 0))

(define (emit-run-lightning)
  "Emit native code used for vm-lightning runtime."
  (with-jit-state
   (jit-prolog)
   (let ((lapply (jit-forward))
         (return-address (jit-movi r1 (imm 0))))

     ;; Get arguments.
     (jit-getarg reg-thread (jit-arg))    ; thread
     (jit-getarg reg-vp (jit-arg))        ; vp
     (jit-getarg reg-registers (jit-arg)) ; registers, for prompt.
     (jit-getarg r0 (jit-arg))            ; resume.

     ;; Test for resume.
     (jump (jit-bmci r0 (imm 1)) lapply)

     ;; Resuming from non-local exit, jump to the handler.
     (vm-cache-fp)
     (vm-cache-sp)
     (jit-ldr r0 reg-vp)
     (jit-jmpr r0)

     ;; Procedure application.
     (jit-link lapply)

     ;; Before caching registers from the argument `vp', save the
     ;; original frame pointer used by lightning, then store to the
     ;; address used for vm_boot_continuation_code, since boot
     ;; continuation code is unused in this engine.
     (jit-movr r0 reg-fp)
     (vm-cache-fp)
     (vm-cache-sp)
     (jit-stxi (make-negative-pointer (* 3 word-size)) reg-fp r0)

     ;; Store return address.
     (scm-set-frame-return-address r1)

     ;; Apply the procedure.
     (vm-apply)

     ;; Patch the address for callee to return.
     (jit-patch return-address)

     ;; Back from callee, return the value(s) and halt.
     (halt))
   (jit-epilog)
   (jit-realize)
   (jit-set-code (bytevector->pointer run-lightning-code)
                 (imm run-lightning-code-size))
   (jit-emit)))

(define (call-lightning proc . args)
  "Switch vm engine to vm-lightning temporary, run procedure PROC with
arguments ARGS."
  (let ((current-engine (vm-engine)))
    (call-with-values (lambda ()
                        (set-vm-engine! 'lightning)
                        (apply call-with-vm proc args))
      (lambda vals
        (set-vm-engine! current-engine)
        (apply values vals)))))


;;;
;;; Initialization
;;;

(init-jit "")
(emit-run-lightning)
(make-bytevector-executable! run-lightning-code)

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_lightning")
