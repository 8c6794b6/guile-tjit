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

;;; A virtual machine with whole-method JIT compiler from bytecode to
;;; native code. Compiler is written in scheme, with GNU Lightning.

;;; Code:

(define-module (system vm native mjit)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (language bytecode)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native debug)
  #:use-module (system vm native cfg)
  #:use-module (system vm program)
  #:use-module (system vm vm)
  #:autoload (ice-9 regex) (regexp-substitute/global)
  #:export (compile-mjit
            call-mjit
            native-code-guardian
            init-vm-mjit)
  #:re-export (lightning-verbosity
               lightning-trace))


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

(define native-code-guardian (make-guardian))


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
(define-inline scm-f-program-is-native #x4000)
(define-inline scm-vtable-index-flags 1)
(define-inline scm-vtable-index-self 2)
(define-inline scm-vtable-index-size 6)
(define-inline scm-vtable-flag-applicable 8)
(define-inline scm-vtable-flag-simple 64)
(define-inline scm-applicable-struct-index-procedure 0)
(define-inline scm-dynstack-prompt-escape-only 16)
(define-inline scm-classf-goops 4096)

(define scm-undefined (make-pointer #x904))
(define scm-eol (scm->pointer '()))
(define scm-bool-f (scm->pointer #f))
(define scm-builtin-apply (scm->pointer apply))
(define scm-builtin-values (scm->pointer values))
(define scm-module-system-booted-p-ptr
  (dynamic-pointer "scm_module_system_booted_p" (dynamic-link)))

(define (scm-module-system-booted-p)
  (dereference-pointer scm-module-system-booted-p-ptr))


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
;;; Pointers to C function
;;;

(define-syntax define-cfunc
  (lambda (x)
    (define (sname cname)
      (let ((sname (regexp-substitute/global #f "_" (syntax->datum cname)
                                             'pre "-" 'post)))
        (string->symbol (string-append/shared "%" sname))))
    (syntax-case x ()
      ((_ cname)
       #`(define #,(datum->syntax x (sname #'cname))
           (dynamic-pointer cname (dynamic-link)))))))

(define-cfunc "scm_do_inline_cons")
(define-cfunc "scm_do_inline_from_double")
(define-cfunc "scm_display")
(define-cfunc "scm_newline")
(define-cfunc "scm_async_tick")
(define-cfunc "scm_do_vm_expand_stack")
(define-cfunc "scm_do_smob_applicable_p")
(define-cfunc "scm_do_smob_apply_trampoline")
(define-cfunc "scm_compile_mjit")
(define-cfunc "scm_values")
(define-cfunc "scm_do_foreign_call")
(define-cfunc "scm_do_vm_reinstate_partial_continuation")
(define-cfunc "scm_do_vm_abort")
(define-cfunc "abort")
(define-cfunc "scm_do_vm_builtin_ref")
(define-cfunc "scm_do_bind_kwargs")
(define-cfunc "scm_logtest")
(define-cfunc "scm_do_inline_cell")
(define-cfunc "scm_do_inline_words")
(define-cfunc "scm_current_module")
(define-cfunc "scm_lookup")
(define-cfunc "scm_define")
(define-cfunc "scm_the_root_module")
(define-cfunc "scm_module_lookup")
(define-cfunc "scm_private_lookup")
(define-cfunc "scm_do_dynstack_push_prompt")
(define-cfunc "scm_do_dynstack_push_dynwind")
(define-cfunc "scm_do_dynstack_pop")
(define-cfunc "scm_do_dynstack_push_fluid")
(define-cfunc "scm_do_unwind_fluid")
(define-cfunc "scm_string_length")
(define-cfunc "scm_string_ref")
(define-cfunc "scm_string_to_number")
(define-cfunc "scm_string_to_symbol")
(define-cfunc "scm_symbol_to_keyword")
(define-cfunc "scm_eqv_p")
(define-cfunc "scm_equal_p")
(define-cfunc "scm_num_eq_p")
(define-cfunc "scm_less_p")
(define-cfunc "scm_leq_p")
(define-cfunc "scm_sum")
(define-cfunc "scm_difference")
(define-cfunc "scm_product")
(define-cfunc "scm_divide")
(define-cfunc "scm_quotient")
(define-cfunc "scm_remainder")
(define-cfunc "scm_modulo")
(define-cfunc "scm_ash")
(define-cfunc "scm_logand")
(define-cfunc "scm_logior")
(define-cfunc "scm_logxor")
(define-cfunc "scm_make_vector")
(define-cfunc "scm_do_allocate_struct")
(define-cfunc "scm_struct_ref")
(define-cfunc "scm_struct_set_x")
(define-cfunc "scm_class_of")
(define-cfunc "scm_from_uint64")
(define-cfunc "scm_from_int64")
(define-cfunc "scm_to_uint64")
(define-cfunc "scm_to_int64")

(define-cfunc "scm_wrong_type_arg_msg")
(define-cfunc "scm_out_of_range")
(define-cfunc "scm_wrong_num_args")

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

(define-syntax-rule (scm-i-makinumi n)
  "Make scheme small fixnum from N."
  (imm (+ (ash n 2) 2)))

(define-syntax-rule (scm-i-makinumr dst src)
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

(define-syntax-rule (scm-program-native-code dst src)
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

(define-syntax-rule (scm-struct-slot-ref dst obj i)
  (begin
    (scm-struct-slots dst obj)
    (scm-cell-object dst dst i)))

(define-syntax-rule (scm-struct-data dst obj)
  (scm-cell-object dst obj 1))

(define-syntax-rule (scm-struct-data-ref dst obj i)
  (begin
    (scm-struct-data dst obj)
    (scm-cell-object dst dst i)))

(define-syntax-rule (scm-struct-vtable dst obj)
  (begin
    (scm-struct-vtable-slots dst obj)
    (scm-cell-object dst dst scm-vtable-index-self)))

(define-syntax-rule (scm-struct-vtable-slots dst obj)
  (begin
    (scm-cell-object dst obj 0)
    (jit-subi dst dst (imm tc3-struct))))

(define-syntax-rule (scm-class-of dst obj)
  (scm-cell-object dst obj scm-vtable-index-self))

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

(define-syntax-rule (scm-program-is-native obj)
  (jit-bmsi obj (imm scm-f-program-is-native)))

(define-syntax-rule (scm-is-string tc7)
  (jit-beqi tc7 (imm tc7-string)))

(define-syntax-rule (scm-is-not-string tc7)
  (jit-bnei tc7 (imm tc7-string)))

(define-syntax-rule (scm-variablep tc7)
  (jit-beqi tc7 (imm tc7-variable)))

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

(define *values-vtable*
  (dereference-pointer
   (dynamic-pointer "scm_values_vtable" (dynamic-link))))

(define-syntax-rule (scm-valuesp vt)
  (jit-beqi vt *values-vtable*))

(define-syntax-rule (scm-not-valuesp vt)
  (jit-bnei vt *values-vtable*))

(define-syntax-rule (scm-is-false obj)
  (jit-beqi obj scm-bool-f))

(define-syntax-rule (scm-is-true obj)
  (jit-bnei obj scm-bool-f))

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
    (call-c %scm-do-inline-cons)
    (jit-retval dst)))

(define-syntax-rule (scm-inline-from-double dst obj)
  (begin
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-pushargr-d obj)
    (call-c %scm-do-inline-from-double)
    (jit-retval dst)))

(define-syntax-rule (scm-displayr reg)
  (begin
    (jit-prepare)
    (jit-pushargr reg)
    (jit-pushargi scm-undefined)
    (call-c %scm-display)))

(define-syntax-rule (scm-displayi obj)
  (begin
    (jit-prepare)
    (jit-pushargi (scm->pointer obj))
    (jit-pushargi scm-undefined)
    (call-c %scm-display)))

(define-syntax-rule (scm-newline)
  (begin
    (jit-prepare)
    (jit-pushargi scm-undefined)
    (call-c %scm-newline)))


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

;; Pointer to current `struct scm_vm* vp'.
(define-inline reg-vp v0)

;; Current thread.
(define-inline reg-thread v1)

;; Registers, used by prompt.
(define-inline reg-registers v2)


;;;
;;; Macros for specific registers
;;;

(define-syntax-rule (vm-thread-dynamic-state dst)
  (jit-ldxi dst reg-thread (imm #xd8)))

(define-syntax-rule (vm-thread-pending-asyncs dst)
  (jit-ldxi dst reg-thread (imm #x104)))

(define-syntax-rule (vm-cache-ip dst)
  (jit-ldr dst reg-vp))

(define-syntax vm-sync-ip
  (syntax-rules ()
    ((_ st)
     (vm-sync-ip st r0))
    ((_ st tmp)
     (begin
       (jit-movi tmp (imm (+ (lightning-pc st) (* 4 (lightning-ip st)))))
       (jit-str reg-vp tmp)))))

(define-syntax-rule (vm-cache-sp dst)
  (jit-ldxi dst reg-vp (imm #x8)))

(define-syntax-rule (vm-sync-sp src)
  (jit-stxi (imm #x8) reg-vp src))

(define-syntax-rule (vm-cache-fp)
  (jit-ldxi reg-fp reg-vp (imm #x10)))

(define-syntax-rule (vm-sync-fp)
  (jit-stxi (imm #x10) reg-vp reg-fp))

(define-syntax-rule (vm-sp-max-since-gc dst)
  (jit-ldxi dst reg-vp (imm #x28)))

(define-syntax-rule (vm-set-sp-max-since-gc src)
  (jit-stxi (imm #x28) reg-vp src))

(define-syntax-rule (vm-stack-base dst)
  (jit-ldxi dst reg-vp (imm #x38)))

(define-syntax-rule (vm-stack-limit dst)
  (jit-ldxi dst reg-vp (imm #x18)))

(define-syntax-rule (vm-stack-size dst)
  (jit-ldxi dst reg-vp (imm #x30)))

(define-syntax-rule (scm-frame-dynamic-link dst)
  (jit-ldxi dst reg-fp (make-negative-pointer (* 2 word-size))))

(define-syntax-rule (scm-frame-set-dynamic-link src)
  (jit-stxi (make-negative-pointer (* 2 word-size)) reg-fp src))

(define-syntax-rule (scm-frame-previous-sp dst)
  (jit-subi dst reg-fp (imm (* 3 word-size))))

(define-syntax-rule (scm-frame-set-previous-sp src)
  (jit-stxi (imm #x8) reg-fp src))

(define-syntax-rule (scm-frame-return-address dst)
  (jit-ldxi dst reg-fp (make-negative-pointer word-size)))

(define-syntax-rule (scm-frame-set-return-address src)
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
    (vm-cache-sp dst)
    (jit-subr dst dst reg-fp)
    (jit-subi dst dst (imm (* n word-size)))
    (jit-rshi dst dst (imm word-size-length))))

(define-syntax-rule (frame-locals-count dst)
  (begin
    (vm-cache-sp dst)
    (jit-subr dst dst reg-fp)
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

(define-syntax-rule (offset-addr st offset)
  (+ (lightning-pc st) (* 4 (+ (lightning-ip st) offset))))

(define-syntax-rule (last-arg-offset st dst tmp)
  (begin
    (frame-locals-count tmp)
    (jit-subi tmp tmp (imm 1))
    (jit-lshi dst tmp (imm word-size-length))))

(define-syntax call-c
  (syntax-rules ()
    ((_ cfunc)
     (call-c cfunc r0))
    ((_ cfunc tmp)
     ;; Explicitly moving the address to a register.  In x86-64,
     ;; lightning's `jit-calli' function is moving the absolute address
     ;; to register, then emitting `call' opcode, which sometimes
     ;; overwrite non-volatile register used in VM.
     (begin
       (jit-movi tmp cfunc)
       (jit-callr tmp)))))

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
    ((_ st)
     (vm-handle-interrupts st r0))
    ((_ st tmp)
     (let ((lexit (jit-forward)))
       (vm-thread-pending-asyncs tmp)
       (jump (jit-bmci tmp (imm 1)) lexit)
       (vm-sync-ip st tmp)
       (jit-prepare)
       (call-c %scm-async-tick tmp)
       (vm-cache-fp)
       (jit-link lexit)))))

(define-syntax vm-alloc-frame
  (syntax-rules ()
    ((_ st)
     (let ((lexit (jit-forward))
           (lincr (jit-forward)))

       ;; Using r0 as temporary register.
       (vm-sp-max-since-gc r0)
       (vm-cache-sp r1)
       (jump (jit-bger r0 r1) lexit)
       (vm-stack-limit r0)
       (jump (jit-bgtr r0 r1) lincr)

       (vm-sync-ip st r0)
       (jit-prepare)
       (jit-pushargr reg-vp)
       (jit-pushargr r1)
       (call-c %scm-do-vm-expand-stack)
       (vm-cache-fp)
       (jump lexit)

       (jit-link lincr)
       (vm-set-sp-max-since-gc r1)

       (jit-link lexit)))

    ((_ st n)
     (begin
       (jit-addi r0 reg-fp (imm (* (- n 1) word-size)))
       ;;; XXX: Move sync-sp to end of alloc-frame.
       (vm-sync-sp r0)
       (vm-alloc-frame st)))))

(define-syntax vm-reset-frame
  (syntax-rules ()
    ((_ n)
     (vm-reset-frame n r0 r1))
    ((_ n tmp1 tmp2)
     (let ((lexit (jit-forward)))
       (jit-addi tmp1 reg-fp (imm (* (- n 1) word-size)))
       (vm-sync-sp tmp1)
       (vm-sp-max-since-gc tmp2)
       (jump (jit-bger tmp2 tmp1) lexit)
       (vm-set-sp-max-since-gc tmp1)
       (jit-link lexit)))))

(define-syntax with-frame
  (syntax-rules ()
    "Run body expression with new frame.

Stack poionter stored in reg-fp increased for `proc * word' size to
shift the locals.  Then patch the address after the jump, so that callee
can jump back.  Two locals below proc get overwritten by the callee."
    ((_ st proc nlocals <body>)
     (with-frame st proc nlocals <body> r0 r1))
    ((_ st proc nlocals <body> tmp1 tmp2)
     (let ((ra (jit-movi tmp1 (imm 0))))

       (jit-movr tmp2 reg-fp)
       (jit-addi reg-fp tmp2 (imm (* proc word-size)))
       (vm-sync-fp)
       (scm-frame-set-dynamic-link tmp2)
       (scm-frame-set-return-address tmp1)

       (vm-reset-frame nlocals tmp1 tmp2)

       <body>
       (jit-patch ra)))))

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

    ;; Load local 0.
    (jit-ldr r0 reg-fp)

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
    (jit-str reg-fp r0)
    (jump lunwrap)

    ;; Test for applicable smob.  Apply trampoline for smob is not
    ;; inlined with lightning, calling C functions.
    (jit-link lsmob)
    (scm-typ7 r1 r2)
    (jump (scm-not-smobp r1) lerror)
    (jit-movr f0 r0)
    (jit-prepare)
    (jit-pushargr r0)
    (call-c %scm-do-smob-applicable-p)
    (jit-retval r0)
    (jump (jit-beqi r0 (imm 0)) lerror)
    (last-arg-offset st r1 r2)
    (vm-cache-sp r2)
    (jit-addi r2 r2 (imm word-size))
    (vm-sync-sp r2)
    (vm-sp-max-since-gc r0)
    (jump (jit-bger r0 r2) lshuffle)
    (vm-set-sp-max-since-gc r2)

    (jit-link lshuffle)
    (jit-ldxr r2 reg-fp r1)
    (jit-addi r1 r1 (imm word-size))
    (jit-stxr r1 reg-fp r2)
    (jit-subi r1 r1 (imm (* 2 word-size)))
    (jump (jit-bgei r1 (stored-ref st 0)) lshuffle)

    (jit-prepare)
    (jit-pushargr f0)
    (call-c %scm-do-smob-apply-trampoline)
    (jit-retval r0)
    (jit-str reg-fp r0)
    (jump lunwrap)

    ;; Show error message.
    (jit-link lerror)
    (error-wrong-type-apply r0)

    ;; Local is program, test for native code.
    (jit-link lprogram)
    (jump (scm-program-is-native r2) lcompiled)

    ;; Does not have native code, compile the callee procedure.
    (jit-prepare)
    (jit-pushargr r0)
    (call-c %scm-compile-mjit)
    (vm-cache-fp)
    (jit-ldr r0 reg-fp)

    ;; Has compiled code.
    (jit-link lcompiled)
    (scm-program-native-code r1 r0)
    (jit-jmpr r1)))

(define-syntax-rule (return-one-value st val tmp1 tmp2 tmp3)
  (begin
    (vm-handle-interrupts st tmp1)
    (jit-movr tmp1 reg-fp)
    (scm-frame-return-address tmp2)
    (scm-frame-dynamic-link reg-fp)
    (vm-sync-fp)
    ;; Clear frame.
    (jit-movi tmp3 scm-bool-f)
    (jit-stxi (make-negative-pointer word-size) tmp1 tmp3)
    (jit-stxi (make-negative-pointer (* 2 word-size)) tmp1 tmp3)
    ;; Leave proc.
    (jit-addi tmp1 tmp1 (imm word-size))
    (jit-str tmp1 val)
    (vm-sync-sp tmp1)
    (jit-jmpr tmp2)))

(define-syntax-rule (return-value-list st rval tmp1 tmp2 tmp3)
  (let ((lone (jit-forward)))

    (jump (scm-imp rval) lone)

    (scm-cell-type tmp1 rval)
    (scm-typ3 tmp2 tmp1)
    (jump (scm-not-structp tmp2) lone)

    (jit-subi tmp2 tmp1 (imm tc3-struct))
    (scm-cell-object tmp2 tmp2 scm-vtable-index-self)
    (jump (scm-not-valuesp tmp2) lone)

    ;; Delegate the work to `vm-apply' with builtin `values'.
    (vm-handle-interrupts st tmp1)
    (scm-struct-slots tmp1 rval)
    (scm-cell-object tmp1 tmp1 0)
    (jit-movi tmp2 scm-builtin-apply)
    (local-set! st 0 tmp2)
    (jit-movi tmp2 scm-builtin-values)
    (local-set! st 1 tmp2)
    (local-set! st 2 tmp1)
    (vm-reset-frame 3)
    (vm-apply)

    (jit-link lone)
    (return-one-value st rval tmp1 tmp2 tmp3)))

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
    (call-c %scm-values)
    (jit-retval r0)

    (jit-link lexit)

    ;; Original value of jit-fp used by lightning.
    (jit-ldr r2 reg-fp)

    ;; Reset the stack pointer and return address.  Then move `vp->fp'
    ;; once for boot continuation added in `scm_call_n',
    (scm-frame-return-address r1)
    (jit-str reg-vp r1)
    (scm-frame-previous-sp r1)
    (vm-sync-sp r1)
    (scm-frame-dynamic-link reg-fp)
    (vm-sync-fp)

    ;; Restore the original frame pointer.
    (jit-movr reg-fp r2)

    ;; Return from native code.
    (jit-retr r0)))

(define-syntax compile-label
  (syntax-rules (assemble-lightning with-frame)

    ;; Tail call
    ((_ st1 nlocals callee-addr)
     (compile-label st1 st2 nlocals callee-addr
                    (assemble-lightning st2 (jit-forward))))

    ;; Non tail call
    ((_ st1 nlocals callee-addr proc)
     (compile-label st1 st2 nlocals callee-addr
                    (with-frame st2 proc nlocals
                                (assemble-lightning st2 (jit-forward)))))

    ((_ st1 st2 nlocals callee-addr body)
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
    (call-c %scm-wrong-type-arg-msg)
    (jit-reti (scm->pointer *unspecified*))))

(define-syntax-rule (error-out-of-range subr expr)
  (begin
    (jit-prepare)
    (jit-pushargi subr)
    expr
    (call-c %scm-out-of-range)
    (jit-reti (scm->pointer *unspecified*))))

(define (%error-wrong-num-values-proc nvalues)
  (scm-error 'vm-error
             'vm-mjit
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
             'vm-mjit
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
             'vm-mjit
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
    (local-ref st 0 r0)
    (jit-pushargr r0)
    (call-c %scm-wrong-num-args)
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
    (error-out-of-range subr (jit-pushargi (scm-i-makinumi idx)))
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
         (vm-handle-interrupts st))
       (jump expr (resolve-dst st offset))))))

(define-syntax define-vm-br-unary-heap-object-op
  (syntax-rules ()
    ((_ (name st a invert offset) reg expr)
     (define-vm-op (name st a invert offset)
       (when (< offset 0)
         (vm-handle-interrupts st))
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
       (let ((lexit (jit-forward))
             (ljump (jit-forward)))
         (local-ref st a r0)
         (local-ref st b r1)
         (jump (jit-beqr r0 r1) (if invert lexit ljump))

         (jit-prepare)
         (jit-pushargr r0)
         (jit-pushargr r1)
         (call-c cname)
         (jit-retval r0)
         (vm-cache-fp)
         (jump (scm-is-false r0) (if invert ljump lexit))

         (when (< offset 0)
           (vm-handle-interrupts st))

         (if (not invert)
             (jump (resolve-dst st offset))
             (jump lexit))

         (jit-link ljump)
         (jump (resolve-dst st offset))

         (jit-link lexit))))))

(define-syntax define-vm-br-arithmetic-op
  (syntax-rules ()
    ((_ (name st a b invert offset)
        fx-op fx-invert-op fl-op fl-invert-op cname)
     (define-vm-op (name st a b invert offset)
       (when (< offset 0)
         (vm-handle-interrupts st))
       (let ((lreal (jit-forward))
             (lcall (jit-forward))
             (lexit (jit-forward))
             (dest (resolve-dst st offset))
             (rega (local-ref st a r1))
             (regb (local-ref st b r2)))

         (jump (scm-not-inump rega) lreal)
         (jump (scm-not-inump regb) lreal)
         (jump ((if invert fx-invert-op fx-op) rega regb) dest)
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
         (jump ((if invert fl-invert-op fl-op) f0 f1) dest)
         (jump lexit)

         (jit-link lcall)
         (jit-prepare)
         (jit-pushargr rega)
         (jit-pushargr regb)
         (call-c cname)
         (jit-retval r0)
         (jump (if invert (scm-is-false r0) (scm-is-true r0)) dest)

         (jit-link lexit))))))

;; If var is not variable, resolve with `resolver' and move the resolved
;; value to var's address. Otherwise, var is `variable', move it to dst.
;; The variable is resolved at compilation time of native code.
(define-syntax define-vm-box-op
  (syntax-rules ()
    ((_ (name st dst var-offset mod-offset sym-offset bound?)
        <compile-time-resolver>
        <runtime-resolver>)
     (define-vm-op (name st dst var-offset mod-offset sym-offset bound?)
       (let* ((current (lightning-ip st))
              (base (lightning-pc st))
              (offset->pointer
               (lambda (offset)
                 (make-pointer (+ base (* 4 (+ current offset))))))
              (var (dereference-scm (offset->pointer var-offset))))
         (let ((resolved (if (variable? var)
                             var
                             <compile-time-resolver>)))
           ;; Box may needs to be resolved at runtime, e.g: a top level
           ;; call creating a record type defined in same module.
           (if (variable? resolved)
               (begin
                 (jit-movi r0 (scm->pointer resolved))
                 (local-set! st dst r0))
               <runtime-resolver>)))))))

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
         (jump (fx-op r0 (imm 4)) lcall)
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
         (scm-i-makinumr r0 r0)
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
         (scm-i-makinumr r0 r0))))))

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
  (vm-handle-interrupts st)
  (with-frame st proc nlocals (vm-apply)))

(define-vm-op (call-label st proc nlocals label)
  (vm-handle-interrupts st)
  (cond
   ((in-same-procedure? st label)
    (with-frame st proc nlocals (jump (resolve-dst st label))))
   ((compiled-node st (offset-addr st label))
    =>
    (lambda (node)
      (with-frame st proc nlocals (jump node))))
   (else
    (compile-label st nlocals (offset-addr st label) proc))))

(define-vm-op (tail-call st nlocals)
  (vm-handle-interrupts st)
  (vm-reset-frame nlocals)
  (vm-apply))

(define-vm-op (tail-call-label st nlocals label)
  (vm-handle-interrupts st)
  (vm-reset-frame nlocals)
  (cond
   ((in-same-procedure? st label)
    (jump (resolve-dst st label)))
   ((compiled-node st (offset-addr st label))
    =>
    (lambda (node)
      (jump node)))
   (else
    (compile-label st nlocals (offset-addr st label)))))

(define-vm-op (tail-call/shuffle st from)
  (let ((lshuffle (jit-forward))
        (lreset (jit-forward))
        (lexit (jit-forward)))
    (vm-handle-interrupts st)

    ;; r2 used to stop the loop in lshuffle.
    (vm-cache-sp r2)
    (jit-subr r2 r2 reg-fp)
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
    (jit-addr r1 reg-fp r0)

    ;; Doing similar thing to `vm-reset-frame', but reg-sp is already
    ;; set to new value.
    (vm-sync-sp r1)
    (vm-sp-max-since-gc r0)
    (jump (jit-bger r0 r1) lexit)
    (vm-set-sp-max-since-gc r1)

    (jit-link lexit)
    (vm-apply)))

(define-vm-op (receive st dst proc nlocals)
  (let ((lexit (jit-forward)))
    (frame-locals-count r0)
    (jump (jit-bgti r0 (imm (+ proc 1))) lexit)
    (error-no-values)

    (jit-link lexit)
    (vm-reset-frame nlocals r1 r2)
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

(define-vm-op (return st src)
  (local-ref st src r0)
  (return-one-value st r0 r1 r2 f0))

(define-vm-op (return-values st)
  (vm-handle-interrupts st r0)
  ;; Get return address to jump.
  (scm-frame-return-address r0)
  (jit-movr r1 reg-fp)
  ;; Restore previous dynamic link to current frame pointer.
  (scm-frame-dynamic-link reg-fp)
  (vm-sync-fp)
  ;; Clear frame
  (jit-movi r2 scm-bool-f)
  (jit-stxi (make-negative-pointer word-size) r1 r2)
  (jit-stxi (make-negative-pointer (* 2 word-size)) r1 r2)
  ;; Jump to return address.
  (jit-jmpr r0))


;;; Specialized call stubs
;;; ----------------------

(define-vm-op (subr-call st ptr-idx)
  (let ((lcall (jit-forward)))
    (vm-sync-ip st r0)

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
    (local-set! st 1 r0)
    (return-value-list st r0 r1 r2 f0)))

(define-vm-op (foreign-call st cif-idx ptr-idx)
  (vm-sync-ip st r0)
  (local-ref st 0 r0)
  (scm-program-free-variable-ref r1 r0 cif-idx)
  (scm-program-free-variable-ref r2 r0 ptr-idx)
  (jit-addi r0 reg-fp (stored-ref st 1))
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr r1)
  (jit-pushargr r2)
  (jit-pushargr r0)
  (call-c %scm-do-foreign-call)
  (jit-retval r0)
  (vm-cache-fp)
  (local-set! st 1 r0)
  (return-value-list st r0 r1 r2 f0))

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
  (call-c %scm-do-vm-reinstate-partial-continuation)

  (vm-cache-fp)
  (vm-cache-ip r0)
  (jit-jmpr r0))

(define-vm-op (tail-apply st)
  (let ((llength (jit-forward))
        (lalloc (jit-forward))
        (lshift (jit-forward))
        (lshuffle (jit-forward))
        (lapply (jit-forward)))

    (vm-handle-interrupts st)

    ;; Index for last local, a list containing rest of arguments.
    (last-arg-offset st r2 r0)
    (vm-cache-sp r1)
    (jit-subi r1 r1 (imm (* 2 word-size)))
    (vm-sync-sp r1)

    ;; Local offset for shifting.
    (last-arg-offset st f0 r1)

    ;; Load last local.
    (jit-ldxr r0 reg-fp r2)

    ;; Load sp
    (vm-cache-sp r1)

    ;; Get list length, increase SP.
    (jit-link llength)
    (jump (scm-is-null r0) lalloc)
    (jit-addi r1 r1 (imm word-size))
    (scm-cdr r0 r0)
    (jump llength)

    (jit-link lalloc)
    (vm-sync-sp r1)
    (vm-alloc-frame st)

    (jit-movi r1 (imm 0))

    ;; Shift non-list locals.
    (jit-link lshift)
    (jit-addi r0 r1 (imm word-size))
    (jit-ldxr r0 reg-fp r0)
    (jit-stxr r1 reg-fp r0)
    (jit-addi r1 r1 (imm word-size))
    (jump (jit-bler r1 f0) lshift)

    ;; Load last local, again.
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
    ;; Store return address to `vp->ip'.
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
    (call-c %scm-do-vm-abort)

    ;; Should not reach here.
    (jit-prepare)
    (call-c %abort)

    ;; Return address for captured vmcont.
    (jit-patch ra)))

(define-vm-op (builtin-ref st dst src)
  (jit-prepare)
  (jit-pushargi (imm src))
  (call-c %scm-do-vm-builtin-ref)
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

    (frame-locals-count r2)
    (vm-alloc-frame st nlocals)
    (jit-lshi r2 r2 (imm word-size-length))
    (jit-movi r1 (imm (* nlocals word-size)))
    (jit-movi r0 scm-undefined)

    ;; Using r1 as offset of location to store.
    (jit-link lshuffle)
    (jump (jit-bger r2 r1) lexit)
    (jit-subi r1 r1 (imm word-size))
    (jit-stxr r1 reg-fp r0)
    (jump lshuffle)

    (jit-link lexit)))

(define-vm-op (reset-frame st nlocals)
  (vm-reset-frame nlocals))

(define-vm-op (assert-nargs-ee/locals st expected nlocals)
  (assert-wrong-num-args st jit-beqi expected 0)
  (vm-alloc-frame st (+ expected nlocals))
  (when (< 0 nlocals)
    (jit-movi r0 scm-undefined)
    (for-each (lambda (n)
                (local-set! st (+ expected n) r0))
              (iota nlocals))))

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
  (call-c %scm-do-bind-kwargs)
  (jit-retval r0)

  ;; Allocate frame with returned value.
  (jit-lshi r0 r0 (imm word-size-length))
  (jit-addr r0 reg-fp r0)
  (vm-sync-sp r0)
  (vm-alloc-frame st))

(define-vm-op (bind-rest st dst)
  (let ((lrefill (jit-forward))
        (lprecons (jit-forward))
        (lcons (jit-forward))
        (lreset (jit-forward))
        (lexit (jit-forward)))

    ;; Initialize values.
    (vm-cache-sp f4)
    (jit-subr f4 f4 reg-fp)
    (jit-addi f4 f4 (imm word-size)) ; f4 = last local index.
    (jit-movi r2 scm-eol)            ; r2 = initial list.
    (jit-movi f0 scm-undefined)      ; f0 = undefined, for refill.
    (jump (jit-bgti f4 (imm (* dst word-size))) lprecons)

    ;; Refill the locals with SCM_UNDEFINED.
    (vm-alloc-frame st (+ dst 1))

    (jit-link lrefill)
    (jump (jit-bgei f4 (imm (* dst word-size))) lexit)
    (jit-stxr f4 reg-fp f0)
    (jit-addi f4 f4 (imm word-size))
    (jump lrefill)

    ;; Create a list.
    (jit-link lprecons)
    (jit-movr r0 r2)

    ;; There are chances for `scm-inline-cons' to call `GC_malloc_many',
    ;; which overwrite registers during `lcons' loop.  Thus moving
    ;; constant value `scm-undefined' to register r2 every time before
    ;; storing to local.  Register f4 seems working under x86-64 when
    ;; guile compiled with 'gcc -O2'.
    (jit-link lcons)
    (jump (jit-blei f4 (imm (* dst word-size))) lreset)
    (jit-subi f4 f4 (imm word-size))
    (jit-ldxr r2 reg-fp f4)
    (scm-inline-cons r0 r2 r0)
    (jit-movi r2 scm-undefined)
    (jit-stxr f4 reg-fp r2)
    (jump lcons)

    (jit-link lreset)
    (jit-movr r2 r0)
    (vm-reset-frame (+ dst 1))

    (jit-link lexit)
    (local-set! st dst r2)))


;;; Branching instructions
;;; ----------------------

(define-vm-op (br st dst)
  (when (< dst 0)
    (vm-handle-interrupts st))
  (jump (resolve-dst st dst)))

(define-vm-br-unary-immediate-op (br-if-true st a invert offset)
  ((if invert jit-beqi jit-bnei)
   (local-ref st a)
   scm-bool-f))

(define-vm-br-unary-immediate-op (br-if-null st a invert offset)
  ((if invert jit-bnei jit-beqi)
   (local-ref st a)
   scm-eol))

(define-vm-op (br-if-nil st a invert offset)
  (when (< offset 0)
    (vm-handle-interrupts st))
  (local-ref st a r0)
  (jit-movi r1 (imm (logior (pointer-address scm-bool-f)
                            (pointer-address scm-eol))))
  (jit-comr r1 r1)
  (jit-andr r1 r0 r1)
  (jump ((if invert jit-bnei jit-beqi) r1 scm-bool-f)
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
    (vm-handle-interrupts st))
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
    (vm-handle-interrupts st))
  (local-ref st a r0)
  (local-ref st b r1)
  (jump ((if invert jit-bner jit-beqr) r0 r1) (resolve-dst st offset)))

(define-vm-br-binary-op (br-if-eqv st a b invert offset)
  %scm-eqv-p)

(define-vm-br-binary-op (br-if-equal st a b invert offset)
  %scm-equal-p)

(define-vm-br-arithmetic-op (br-if-= st a b invert offset)
  jit-beqr jit-bner jit-beqr-d jit-bner-d %scm-num-eq-p)

(define-vm-br-arithmetic-op (br-if-< st a b invert offset)
  jit-bltr jit-bger jit-bltr-d jit-bunltr-d %scm-less-p)

(define-vm-br-arithmetic-op (br-if-<= st a b invert offset)
  jit-bler jit-bgtr jit-bler-d jit-bunler-d %scm-leq-p)

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
    (call-c %scm-logtest)
    (jit-retval r0)
    (jump (if invert (scm-is-false r0) (scm-is-true r0))
          (resolve-dst st offset))

    (jit-link lexit)))


;;; Lexical binding instructions
;;; ----------------------------

(define-vm-op (mov st dst src)
  (local-ref st src r0)
  (local-set! st dst r0))

;;; XXX: long-mov

(define-vm-op (box st dst src)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargi (imm tc7-variable))
  (jit-pushargr (local-ref st src))
  (call-c %scm-do-inline-cell)
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
                               scm-f-program-is-native
                               (ash nfree 16))))
    (jit-pushargi (imm (+ nfree 3)))
    (call-c %scm-do-inline-words)
    (jit-retval r0)

    ;; Storing address of byte-compiled code.
    (jit-movi r1 (imm (offset-addr st offset)))
    (scm-set-cell-object r0 1 r1)

    ;; Storing address of native code.
    (let ((closure-addr (jit-movi r1 (imm 0))))
      (scm-set-cell-object r0 2 r1)
      (jump lnext)

      ;; Do the compilation at the time of closure creation.
      (let* ((cfg (procedure->cfg (offset-addr st offset)))
             (lightning (make-lightning cfg
                                        (lightning-nodes st)
                                        (offset-addr st offset))))
        (jit-patch closure-addr)
        (assemble-lightning lightning (jit-forward))))

    (jit-link lnext)
    (when (< 0 nfree)
      (jit-movi r1 scm-bool-f)
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
  (jit-movi r0 (imm a))
  (local-set! st dst r0))

(define-vm-op (make-long-immediate st dst a)
  (jit-movi r0 (imm a))
  (local-set! st dst r0))

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
  ;; XXX: Add assertion for align.
  (jit-ldi r0 (imm (offset-addr st offset)))
  (local-set! st dst r0))

(define-vm-op (static-set! st src offset)
  ;; XXX: Add assertion for align.
  (local-ref st src r0)
  (jit-sti (imm (offset-addr st offset)) r0))

(define-vm-op (static-patch! st dst-offset src-offset)
  ;; XXX: Add assertion for align.
  (jit-movi r0 (imm (offset-addr st src-offset)))
  (jit-sti (imm (offset-addr st dst-offset)) r0))


;;; Mutable top-level bindings
;;; --------------------------

(define-vm-op (current-module st dst)
  (jit-prepare)
  (call-c %scm-current-module)
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (resolve st dst bound? sym)
  ;;; XXX: Assert with `bound?'.
  (vm-sync-ip st r0)
  (local-ref st sym r0)
  (jit-prepare)
  (jit-pushargr r0)
  (call-c %scm-lookup)
  (jit-retval r0)
  (vm-cache-fp)
  (local-set! st dst r0))

(define-vm-op (define! st sym val)
  (vm-sync-ip st r0)
  (jit-prepare)
  (local-ref st sym r0)
  (jit-pushargr r0)
  (local-ref st val r0)
  (jit-pushargr r0)
  (call-c %scm-define)
  (vm-cache-fp))

(define-vm-box-op (toplevel-box st dst var-offset mod-offset sym-offset bound?)
  (module-variable (or (dereference-scm
                        (make-pointer (offset-addr st mod-offset)))
                       the-root-module)
                   (dereference-scm
                    (make-pointer (offset-addr st sym-offset))))
  (let ((lunresolved (jit-forward))
        (llookup (jit-forward))
        (lexit (jit-forward)))

    (debug 1 ";;; Unresolved toplevel-box, ip: ~a, var: ~a, mod: ~a, sym: ~a~%"
           (lightning-ip st) var-offset mod-offset sym-offset)

    (jit-ldi r0 (imm (offset-addr st var-offset)))
    (jump (scm-imp r0) lunresolved)
    (scm-cell-type r1 r0)
    (scm-typ7 r1 r1)
    (jump (scm-variablep r1) lexit)

    (jit-link lunresolved)
    (jit-ldi r0 (imm (offset-addr st mod-offset)))
    (jump (scm-is-true r0) llookup)
    (jit-prepare)
    (call-c %scm-the-root-module)
    (jit-retval r0)

    (jit-link llookup)
    (vm-sync-ip st r1)
    (jit-ldi r1 (imm (offset-addr st sym-offset)))
    (jit-prepare)
    (jit-pushargr r0)
    (jit-pushargr r1)
    (call-c %scm-module-lookup)
    (jit-retval r0)
    (vm-cache-fp)
    (jit-sti (imm (offset-addr st var-offset)) r0)

    (jit-link lexit)
    (local-set! st dst r0)))

(define-vm-box-op (module-box st dst var-offset mod-offset sym-offset bound?)
  ;; XXX: Separate public and private lookup.
  (module-variable (resolve-module
                    (cdr (pointer->scm
                          (make-pointer (offset-addr st mod-offset)))))
                   (dereference-scm
                    (make-pointer (offset-addr st sym-offset))))
  (let ((lunresolved (jit-forward))
        (lbooted (jit-forward))
        (lsave (jit-forward))
        (lexit (jit-forward)))

    (debug 1 ";;; Unresolved module-box, ip: ~a, mod: ~a, sym: ~a~%"
           (lightning-ip st) mod-offset sym-offset)

    (jit-ldi r0 (imm (offset-addr st var-offset)))
    (jump (scm-imp r0) lunresolved)
    (scm-cell-type r1 r0)
    (jump (scm-variablep r1) lexit)

    (jit-link lunresolved)
    (jit-movi r1 (scm-module-system-booted-p))
    (jump (jit-bmsi r1 (imm 1)) lbooted)
    (jit-prepare)
    (jit-pushargi (imm (offset-addr st sym-offset)))
    (call-c %scm-lookup)
    (jit-retval r0)
    (jump lsave)

    (jit-link lbooted)
    (jit-movi r1 (imm (offset-addr st mod-offset)))
    (scm-cdr r1 r1)
    (jit-prepare)
    (jit-pushargr r1)
    (jit-pushargi (imm (offset-addr st sym-offset)))
    (call-c %scm-private-lookup)
    (jit-retval r0)

    (jit-link lsave)
    (vm-cache-fp)
    (jit-sti (imm (offset-addr st var-offset)) r0)

    (jit-link lexit)
    (local-set! st dst r0)))


;;; The dynamic environment
;;; -----------------------

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
    (call-c %scm-do-dynstack-push-prompt)))

(define-vm-op (wind st winder unwinder)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr (local-ref st winder))
  (jit-pushargr (local-ref st unwinder))
  (call-c %scm-do-dynstack-push-dynwind))

(define-vm-op (unwind st)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (call-c %scm-do-dynstack-pop))

(define-vm-op (push-fluid st fluid value)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr (local-ref st fluid))
  (jit-pushargr (local-ref st value))
  (call-c %scm-do-dynstack-push-fluid))

(define-vm-op (pop-fluid st)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (call-c %scm-do-unwind-fluid))

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
    (scm-i-makinumr r0 r0)
    (jump lexit)

    (jit-link lcall)
    (jit-prepare)
    (jit-pushargr r0)
    (call-c %scm-string-length)
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
  (call-c %scm-string-ref)
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (string->number st dst src)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (jit-pushargi scm-undefined)
  (call-c %scm-string-to-number)
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (string->symbol st dst src)
  (vm-sync-ip st r0)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (call-c %scm-string-to-symbol)
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (symbol->keyword st dst src)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (call-c %scm-symbol-to-keyword)
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
  %scm-sum lcall r1 r2 f0 f1
  (begin
    (jit-movr r0 r1)
    (jump (jit-boaddr r0 r2) lcall)
    (jit-subi r0 r0 (imm 2)))
  (jit-addr-d f0 f0 f1))

(define-vm-unary-step-op (add1 st dst src)
  %scm-sum jit-boaddi jit-addr-d)

(define-vm-binary-numeric-op (sub st dst a b)
  %scm-difference lcall r1 r2 f0 f1
  (begin
    (jit-movr r0 r1)
    (jump (jit-bosubr r0 r2) lcall)
    (jit-addi r0 r0 (imm 2)))
  (jit-subr-d f0 f0 f1))

(define-vm-unary-step-op (sub1 st dst src)
  %scm-difference jit-bosubi jit-subr-d)

(define-vm-binary-numeric-op (mul st dst a b)
  %scm-product lcall r1 r2 f0 f1
  (begin
    (scm-i-inumr r0 r1)
    (scm-i-inumr f0 r2)
    (jit-qmulr r0 f0 r0 f0)
    (jump (jit-bnei f0 (imm 0)) lcall)
    (jump (jit-bgti r0 (imm most-positive-fixnum)) lcall)
    (scm-i-makinumr r0 r0))
  (jit-mulr-d f0 f0 f1))

(define-vm-binary-numeric-op (div st dst a b)
  %scm-divide lcall r1 r2 f0 f1
  (begin
    (scm-i-inumr r0 r1)
    (scm-i-inumr f0 r2)
    (jit-qdivr r0 f0 r0 f0)
    (jump (jit-bnei f0 (imm 0)) lcall)
    (scm-i-makinumr r0 r0))
  (jit-divr-d f0 f0 f1))

(define-vm-binary-numeric-op (quo st dst a b)
  %scm-quotient lcall r0 r1
  (begin
    (scm-i-inumr r0 r0)
    (scm-i-inumr r1 r1)
    (jit-divr r0 r0 r1)
    (scm-i-makinumr r0 r0)))

(define-vm-binary-numeric-op (rem st dst a b)
  %scm-remainder lcall r0 r1
  (begin
    (scm-i-inumr r0 r0)
    (scm-i-inumr r1 r1)
    (jit-remr r0 r0 r1)
    (scm-i-makinumr r0 r0)))

(define-vm-binary-numeric-op (mod st dst a b)
  %scm-modulo lcall r0 r1
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
    (scm-i-makinumr r0 r0)))

(define-vm-binary-numeric-op (ash st dst a b)
  %scm-ash lcall r0 r1
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
    (scm-i-makinumr r0 r0)
    (jump lexit)

    (jit-link lright)
    (jit-rshi r0 r0 (imm (- scm-i-fixnum-bit 1)))
    (scm-i-makinumr r0 r0)
    (jump lexit)

    (jit-link lleft)
    (jump (jit-bgei r1 (imm (- scm-i-fixnum-bit 1))) lprepare)
    (jit-movi r2 (imm (- scm-i-fixnum-bit 1)))
    (jit-subr r2 r2 r1)
    (jit-rshr-u r2 r0 r2)
    (jump (jit-bgti r2 (imm 0)) lprepare)
    (jump (jit-blti r0 (imm 0)) ladjust)
    (jit-lshr r0 r0 r1)
    (scm-i-makinumr r0 r0)
    (jump lexit)

    (jit-link ladjust)
    (jit-negr r0 r0)
    (jit-lshr r0 r0 r1)
    (jit-negr r0 r0)
    (scm-i-makinumr r0 r0)
    (jump lexit)

    (jit-link lprepare)
    (local-ref st a r0)
    (local-ref st b r1)
    (jump lcall)

    (jit-link lexit)
    (local-set! st dst r0)))

(define-vm-binary-numeric-op (logand st dst a b)
  %scm-logand lcall r0 r1
  (jit-andr r0 r0 r1))

(define-vm-binary-numeric-op (logior st dst a b)
  %scm-logior lcall r0 r1
  (jit-orr r0 r0 r1))

(define-vm-binary-numeric-op (logxor st dst a b)
  %scm-logxor lcall r0 r1
  (begin
    (scm-i-inumr r0 r0)
    (scm-i-inumr r1 r1)
    (jit-orr r0 r0 r1)
    (scm-i-makinumr r0 r0)))

(define-vm-op (make-vector st dst length init)
  (jit-prepare)
  (jit-pushargr (local-ref st length))
  (jit-pushargr (local-ref st init))
  (call-c %scm-make-vector)
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (make-vector/immediate st dst length init)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargi (imm (logior tc7-vector (ash length 8))))
  (jit-pushargi (imm (+ length 1)))
  (call-c %scm-do-inline-words)
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
  (scm-i-makinumr r1 r1)
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
  (vm-sync-ip st r0)
  (local-ref st vtable r0)
  (local-ref st nfields r1)
  (jit-prepare)
  (jit-pushargr r0)
  (jit-pushargr r1)
  (call-c %scm-do-allocate-struct)
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (allocate-struct/immediate st dst vtable nfields)
  (vm-sync-ip st r0)
  (local-ref st vtable r0)
  (jit-prepare)
  (jit-pushargr r0)
  (jit-pushargi (scm-i-makinumi nfields))
  (call-c %scm-do-allocate-struct)
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (struct-ref st dst src idx)
  (let ((lcall (jit-forward))
        (lexit (jit-forward)))

    ;; Load object.
    (local-ref st src r0)

    ;; Test struct type.
    (jump (scm-imp r0) lcall)
    (scm-cell-type r1 r0)
    (scm-typ3 r1 r1)
    (jump (scm-not-structp r1) lcall)

    ;; Test vtable flag is set for SCM_VTABLE_FLAG_SIMPLE.
    (scm-struct-vtable-slots r1 r0)
    (scm-cell-object r2 r1 scm-vtable-index-flags)
    (jit-andi r2 r2 (imm scm-vtable-flag-simple))
    (jump (jit-bmci r2 (imm 1)) lcall)

    ;; Load index.
    (local-ref st idx r2)

    ;; Test index is positive inum.
    (jump (scm-not-inump r2) lcall)
    (scm-i-inumr r2 r2)
    (jump (jit-blti r2 (imm 0)) lcall)

    ;; Test index range.
    (scm-cell-object r1 r1 scm-vtable-index-self)
    (scm-struct-data-ref r1 r1 scm-vtable-index-size)
    (jump (jit-bler r1 r2) lcall)

    ;; Load from struct slot.
    (scm-struct-slots r0 r0)
    (scm-cell-object-r r0 r0 r2)
    (jump lexit)

    ;; Call C function.
    (jit-link lcall)
    (jit-prepare)
    (jit-pushargr r0)
    (local-ref st idx r0)
    (jit-pushargr r0)
    (call-c %scm-struct-ref)
    (jit-retval r0)

    (jit-link lexit)
    (local-set! st dst r0)))

(define-vm-op (struct-ref/immediate st dst src idx)
  (let ((lcall (jit-forward))
        (lexit (jit-forward)))

    ;; Load object.
    (local-ref st src r0)

    ;; Test struct type.
    (jump (scm-imp r0) lcall)
    (scm-cell-type r1 r0)
    (scm-typ3 r1 r1)
    (jump (scm-not-structp r1) lcall)

    ;; Test vtable flag is set for SCM_VTABLE_FLAG_SIMPLE.
    (scm-struct-vtable-slots r1 r0)
    (scm-cell-object r2 r1 scm-vtable-index-flags)
    (jit-andi r2 r2 (imm scm-vtable-flag-simple))
    (jump (jit-bmci r2 (imm 1)) lcall)

    ;; Test index range.
    (scm-cell-object r1 r1 scm-vtable-index-self)
    (scm-struct-data-ref r1 r1 scm-vtable-index-size)
    (jump (jit-bgti r1 (imm idx)) lcall)

    ;; Load from struct slot.
    (scm-struct-slot-ref r0 r0 idx)
    (jump lexit)

    ;; Call C function.
    (jit-link lcall)
    (jit-prepare)
    (jit-pushargr r0)
    (jit-pushargi (scm-i-makinumi idx))
    (call-c %scm-struct-ref)
    (jit-retval r0)

    (jit-link lexit)
    (local-set! st dst r0)))

(define-vm-op (struct-set! st dst idx src)
  ;; XXX: Validate struct flag.
  (jit-prepare)
  (local-ref st dst r0)
  ;; (validate-struct r0 r1 *struct-set!-string)
  ;; (local-ref st src r1)
  ;; (local-ref st idx r2)
  ;; (scm-cell-object r0 r0 1)
  ;; (scm-set-cell-object-r r0 r2 r1)
  (jit-pushargr r0)
  (local-ref st idx r0)
  (jit-pushargr r0)
  (local-ref st src r0)
  (jit-pushargr r0)
  (call-c %scm-struct-set-x))

(define-vm-op (struct-set!/immediate st dst idx src)
  ;; XXX: Validate struct flag.
  (jit-prepare)
  (local-ref st dst r0)
  ;; (validate-struct r0 r1 *struct-set!-string)
  ;; (local-ref st src r1)
  ;; (scm-cell-object r0 r0 1)
  ;; (scm-set-cell-object r0 idx r1)
  (jit-pushargr r0)
  (jit-pushargi (scm-i-makinumi idx))
  (local-ref st src r0)
  (jit-pushargr r0)
  (call-c %scm-struct-set-x))

(define-vm-op (class-of st dst type)
  (let ((lcall (jit-forward))
        (lexit (jit-forward)))
    (local-ref st type r0)
    (jump (scm-imp r0) lcall)
    (scm-cell-type r1 r0)
    (scm-typ3 r2 r1)
    (jump (scm-not-structp r2) lcall)

    (jit-ldxi r2 r1 (imm #x7))
    (jit-rshi r2 r2 (imm #xc))
    (jit-andi r2 r2 (imm 1))
    (jump (jit-bnei r2 (imm 1)) lcall)

    (scm-struct-vtable-slots r1 r0)
    (scm-class-of r0 r1)
    (jump lexit)

    (jit-link lcall)
    (vm-sync-ip st r1)
    (jit-prepare)
    (jit-pushargr r0)
    (call-c %scm-class-of)

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
(define-vm-bv-ref-op bv-u64-ref jit-ldxr-l %scm-from-uint64)
(define-vm-bv-ref-op bv-s64-ref jit-ldxr-l %scm-from-int64)
(define-vm-bv-ref-op bv-f32-ref "float")
(define-vm-bv-ref-op bv-f64-ref "double")

(define-vm-bv-set-op bv-u8-set! jit-stxr-c)
(define-vm-bv-set-op bv-s8-set! jit-stxr-c)
(define-vm-bv-set-op bv-u16-set! jit-stxr-s)
(define-vm-bv-set-op bv-s16-set! jit-stxr-s)
(define-vm-bv-set-op bv-u32-set! jit-stxr-i)
(define-vm-bv-set-op bv-s32-set! jit-stxr-i)
(define-vm-bv-set-op bv-u64-set! jit-stxr-l %scm-to-uint64)
(define-vm-bv-set-op bv-s64-set! jit-stxr-l %scm-to-int64)
(define-vm-bv-set-op bv-f32-set! "float")
(define-vm-bv-set-op bv-f64-set! "double")


;;;
;;; Compilation
;;;

(define-syntax-rule (write-code-to-file file pointer)
  (call-with-output-file file
    (lambda (port)
      (put-bytevector port (pointer->bytevector pointer (jit-code-size))))))

(define space-string ": ")

(define (assemble-lightning st entry)
  "Assemble with state ST, using ENTRY as entry of the result."
  (define-syntax-rule (destination-label st)
    (hashq-ref (lightning-labels st) (lightning-ip st)))

  (define-syntax-rule (destination-handler st)
    (hashq-ref (lightning-handlers st) (lightning-ip st)))

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

          (when (lightning-trace)
            (scm-displayi (offset-addr st 0))
            (scm-displayi space-string)
            (scm-displayi op)
            (scm-newline)))

        ;; Link if this bytecode intruction is labeled as destination,
        ;; or patch it for prompt handler.
        (cond ((destination-label st)
               =>
               (lambda (node) (jit-link node)))
              ((destination-handler st)
               =>
               (lambda (node) (jit-patch node))))
        (or (and emitter (apply emitter st args))
            (debug 0 "compile-mjit: VM op `~a' not found~%" instr)))))

  (let* ((addr (ensure-program-addr (lightning-pc st)))
         (cfg (lightning-cfg st))
         (name (cfg-name cfg)))

    (hashq-set! (lightning-nodes st) addr entry)

    (let ((verbosity (lightning-verbosity)))
      (when (and verbosity (<= 3 verbosity))
        (jit-note name 0)))

    ;; Link and compile the entry point.
    (jit-link entry)
    (jit-patch entry)
    (let lp ((ops (cfg-ops cfg)))
      (unless (null? ops)
        (assemble-one st (car ops))
        (lp (cdr ops))))

    entry))

(define (compile-mjit proc)
  "Compile bytecode of procedure PROC to native code, and save the
compiled result."
  (debug 1 ";;; compiling ~a (~a)~%"
         (try-program-name proc) (ensure-program-addr proc))
  (with-jit-state
   (jit-prolog)
   (jit-tramp (imm #x10))
   (let ((entry (jit-forward))
         (lightning (make-lightning (procedure->cfg proc)
                                    (make-hash-table)
                                    (ensure-program-addr proc))))
     (assemble-lightning lightning entry)
     (jit-epilog)
     (jit-realize)
     (let* ((estimated-code-size (jit-code-size))
            (bv (make-bytevector estimated-code-size)))

       ;; Generated codes persist until guile shutdown, no way to decide
       ;; whether the compiled result called again or not.
       (native-code-guardian bv)

       (jit-set-code (bytevector->pointer bv)
                     (imm estimated-code-size))
       (jit-emit)

       (set-native-code! proc (jit-address entry))
       (make-bytevector-executable! bv)

       ;; Debug output.
       (let ((verbosity (lightning-verbosity)))
         (when (and verbosity (<= 2 verbosity))
           (format #t ";;; nodes:~%")
           (hash-for-each (lambda (k v)
                            (format #t ";;;   ~a => ~a~%" k v))
                          (lightning-nodes lightning))
           (format #t ";;;~%"))
         (when (and verbosity (<= 3 verbosity))
           (let* ((name (try-program-name proc))
                  (escaped-name (regexp-substitute/global #f "/" name
                                                          'pre "_" 'post)))
             (write-code-to-file (format #f "/tmp/~a.o" escaped-name)
                                 (bytevector->pointer bv)))
           (jit-print)
           (jit-clear-state)))))))


;;;
;;; Runtime
;;;

;;; Size of bytevector to contain native code for `mjit-main'.
(define-inline mjit-main-code-size 4096)

;;; Bytevector to contain native code of `mjit-main'. This top
;;; level variable get filled in with actual value at the end of this
;;; file, and referenced from C code.
(define mjit-main-code
  (make-bytevector mjit-main-code-size 0))

(define (emit-mjit-main)
  "Emit native code used for vm-mjit runtime."
  (with-jit-state
   (jit-prolog)
   (let ((lapply (jit-forward))
         (return-address (jit-movi r1 (imm 0))))

     ;; Get arguments.
     (jit-getarg reg-thread (jit-arg))    ; thread
     (jit-getarg reg-vp (jit-arg))        ; vp
     (jit-getarg reg-registers (jit-arg)) ; registers, for prompt
     (jit-getarg r0 (jit-arg))            ; resume

     ;; Test for resume.
     (jump (jit-bmci r0 (imm 1)) lapply)

     ;; Resuming from non-local exit, jump to the handler. The native
     ;; code address of handler is stored in vp->ip.
     (vm-cache-fp)
     (vm-cache-ip r0)
     (jit-jmpr r0)

     ;; Procedure application.
     (jit-link lapply)

     ;; Before caching registers from the argument `vp', save the
     ;; original frame pointer contents used by lightning.  The original
     ;; frame pointer is then stored to the address used for
     ;; `vm_boot_continuation_code', since boot continuation code is not
     ;; used by this vm engine.
     (jit-movr r0 reg-fp)
     (vm-cache-fp)
     (jit-stxi (make-negative-pointer (* 3 word-size)) reg-fp r0)

     ;; Store return address.
     (scm-frame-set-return-address r1)

     ;; Apply the procedure.
     (vm-apply)

     ;; Patch the address for callee to return.
     (jit-patch return-address)

     ;; Back from callee, return the value(s) and halt.
     (halt))
   (jit-epilog)
   (jit-realize)
   (jit-set-code (bytevector->pointer mjit-main-code)
                 (make-pointer mjit-main-code-size))
   (jit-emit)))

(define (call-mjit proc . args)
  "Switch vm engine to vm-mjit temporary, run procedure PROC with
arguments ARGS."
  (let ((current-engine (vm-engine)))
    (call-with-values (lambda ()
                        (set-vm-engine! 'mjit)
                        (apply call-with-vm proc args))
      (lambda vals
        (set-vm-engine! current-engine)
        (apply values vals)))))


;;;
;;; Initialization
;;;

(define (init-vm-mjit interactive?)
  "Do some warming up compilations when INTERACTIVE? is true."
  (when interactive?
    (call-mjit (@@ (system base compile) compile)
               '(lambda (x) (display "")))))

(init-jit "")
(emit-mjit-main)
(make-bytevector-executable! mjit-main-code)

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_mjit")