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

;;; JIT compiler from VM bytecode to native code, written with
;;; lightning.

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
  #:use-module (system vm lightning trace)
  #:use-module (system vm program)
  #:use-module (system vm vm)
  #:export (compile-procedure
            compile-lightning
            call-lightning
            jit-code-guardian)
  #:re-export (lightning-verbosity))


;;;
;;; Auxiliary
;;;

;; Modified later by function defined in "vm-lightning.c". Defined with
;; dummy body to silent warning message.
(define thread-i-data *unspecified*)
(define smob-apply-trampoline *unspecified*)

(define *vm-instr* (make-hash-table))

;; State used during compilation.
(define-record-type <lightning>
  (%make-lightning trace nodes ip labels pc)
  lightning?

  ;; State from bytecode trace.
  (trace lightning-trace set-lightning-trace!)

  ;; Hash table containing compiled nodes.
  (nodes lightning-nodes)

  ;; Current bytecode IP.
  (ip lightning-ip set-lightning-ip!)

  ;; Label objects used by lightning.
  (labels lightning-labels)

  ;; Address of byte-compiled program code.
  (pc lightning-pc))

(define* (make-lightning trace nodes pc
                         #:optional
                         (ip 0)
                         (labels (make-hash-table)))
  (for-each (lambda (labeled-ip)
              (hashq-set! labels labeled-ip (jit-forward)))
            (trace-labeled-ips trace))
  (%make-lightning trace nodes ip labels pc))

(define jit-code-guardian (make-guardian))


;;;
;;; Inlined constants
;;;

(define-syntax define-inline
  (syntax-rules ()
    ((_ name val)
     (define-syntax name (identifier-syntax val)))))

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

(define-inline scm-classf-goops 4096)

(define-inline scm-undefined (make-pointer #x904))


;;;
;;; Registers with specific use
;;;

;; Number of locals.
(define-inline reg-nlocals v0)

;; Return value.
(define-inline reg-retval v1)

;; Current thread.
(define-inline reg-thread v2)


;;;
;;; SCM macros for register read/write
;;;

(define-syntax scm-cell-object
  (syntax-rules ()
    ((_ dst obj 0)
     (jit-ldr dst obj))
    ((_ dst obj 1)
     (jit-ldxi dst obj (imm (sizeof '*))))
    ((_ dst obj n)
     (jit-ldxi dst obj (imm (* n (sizeof '*)))))))

(define-syntax-rule (scm-cell-object-r dst obj reg-offset)
  (jit-ldxr dst obj reg-offset))

(define-syntax scm-set-cell-object
  (syntax-rules ()
    ((_ obj 0 src)
     (jit-str obj src))
    ((_ obj 1 src)
     (jit-stxi (imm (sizeof '*)) obj src))
    ((_ obj n src)
     (jit-stxi (imm (* n (sizeof '*))) obj src))))

(define-syntax-rule (scm-set-cell-object-r obj reg-offset val)
  (jit-stxr reg-offset obj val))

(define-syntax-rule (scm-thread-dynamic-state st)
  (jit-ldxi r0 reg-thread (imm #xd8)))

(define-syntax-rule (scm-thread-pending-asyncs st)
  (jit-ldxi r0 reg-thread (imm #x104)))

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

(define (scm-car dst obj)
  (scm-cell-object dst obj 0))

(define (scm-cdr dst obj)
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
  (jit-ldxi-d dst src (imm (* 2 (sizeof '*)))))

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
    (jit-addi tmp tmp (imm (- (sizeof '*) tc3-struct)))
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
  (jit-beqi obj (scm->pointer #f)))

(define-syntax-rule (scm-is-true obj)
  (jit-bnei obj (scm->pointer #f)))

(define-syntax-rule (scm-is-null obj)
  (jit-beqi obj (scm->pointer '())))

(define-syntax-rule (scm-is-not-null obj)
  (jit-bnei obj (scm->pointer '())))

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


;;;
;;; VM op syntaxes
;;;

(define-syntax define-vm-op
  (syntax-rules ()
    ((_ (op st . args) body ...)
     (hashq-set! *vm-instr* 'op (lambda (st . args)
                                  body ...)))))

(define-syntax-rule (dereference-scm pointer)
  (pointer->scm (dereference-pointer pointer)))

(define-syntax-rule (frame-local offset)
  (imm (* offset (sizeof '*))))

(define-syntax-rule (stored-ref st n)
  "Stored ref 0 is frame local 2. Frame local 0 contains previous (jit-fp),
frame local 1 contains return address."
  (frame-local (+ n 2)))

(define-syntax local-ref
  (syntax-rules ()
    ((local-ref st n)
     (local-ref st n r0))
    ((local-ref st n reg)
     (begin
       (jit-ldxi reg (jit-fp) (stored-ref st n))
       reg))))

(define-syntax-rule (local-set! st dst reg)
  (jit-stxi (stored-ref st dst) (jit-fp) reg))

(define-syntax-rule (local-set!/immediate st dst val)
  (begin
    (jit-movi r0 val)
    (jit-stxi (stored-ref st dst) (jit-fp) r0)))

(define-syntax-rule (offset-addr st offset)
  (+ (lightning-pc st) (* 4 (+ (lightning-ip st) offset))))

(define-syntax-rule (last-arg-offset st dst tmp)
  (begin
    (jit-movr tmp reg-nlocals)
    (jit-subi tmp tmp (imm 1))
    (jit-muli tmp tmp (imm (sizeof '*)))
    (jit-addi dst tmp (stored-ref st 0))))

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

(define-syntax return-jmp
  (syntax-rules ()
    ((_ st)
     (return-jmp st r0))
    ((_ st reg)
     (begin
       ;; Get return address to jump
       (jit-ldxi reg (jit-fp) (stored-ref st -1))
       ;; Restore previous dynamic link to current frame pointer
       (jit-ldxi (jit-fp) (jit-fp) (stored-ref st -2))
       ;; ... then jump to return address.
       (jit-jmpr reg)))))

(define-syntax-rule (vm-handle-interrupts st)
  (let ((lexit (jit-forward)))
    (scm-thread-pending-asyncs r0)
    (jump (jit-bmci r0 (imm 1)) lexit)
    (jit-prepare)
    (call-c "scm_async_tick")
    (jit-link lexit)))

(define-syntax with-frame
  ;; Stack poionter stored in (jit-fp) increased for `proc * word' size
  ;; to shift the locals.  Then patch the address after the jump, so
  ;; that callee can jump back. Two locals below proc get overwritten by
  ;; the callee.
  (syntax-rules ()
    ((_ st proc body)
     (with-frame st r0 proc body))
    ((_ st reg proc body)
     (let ((ra (jit-movi reg (imm 0))))
       ;; Store return address.
       (jit-stxi (stored-ref st (- proc 1)) (jit-fp) reg)
       ;; Store dynamic link.
       (jit-stxi (stored-ref st (- proc 2)) (jit-fp) (jit-fp))
       ;; Shift the frame pointer register.
       (jit-addi (jit-fp) (jit-fp) (imm (* (sizeof '*) proc)))
       body
       (jit-patch ra)))))

(define-syntax-rule (return-value-list st proc tmp1 tmp2 tmp3)
  (let ((lexit (jit-forward))
        (lshuffle (jit-forward)))

    (jit-movi reg-nlocals (imm 2))
    (jump (scm-imp reg-retval) lexit)

    (scm-cell-type tmp1 reg-retval)
    (scm-typ3 tmp2 tmp1)
    (jump (scm-not-structp tmp2) lexit)

    (jit-subi tmp2 tmp1 (imm tc3-struct))
    (scm-cell-object tmp2 tmp2 scm-vtable-index-self)
    (jump (scm-not-valuesp tmp2) lexit)

    (scm-struct-slots tmp1 reg-retval)
    (scm-cell-object tmp1 tmp1 0)
    (scm-car reg-retval tmp1)
    (local-set! st (+ proc 1) reg-retval)
    (jit-movi tmp2 (stored-ref st (+ proc 1)))

    ;; (jit-subi reg-nlocals reg-nlocals (imm 1))
    (jit-movi reg-nlocals (imm 1))
    (jit-link lshuffle)
    (scm-car tmp3 tmp1)
    (jit-stxr tmp2 (jit-fp) tmp3)
    (jit-addi tmp2 tmp2 (imm (sizeof '*)))
    (jit-addi reg-nlocals reg-nlocals (imm 1))
    (scm-cdr tmp1 tmp1)
    (jump (scm-is-not-null tmp1) lshuffle)

    (jit-link lexit)))

;; Apply trampoline for smob is not inlined with lightning, calling C
;; functions.
;;
;; XXX: Move the codes for unwrapping procedure to common place, it's
;; too much work to generate every call.
(define-syntax call-local
  (syntax-rules ()
    ((_ st proc nlocals #f)
     (call-local st proc nlocals (with-frame st proc (jit-jmpr r1))))

    ((_ st proc nlocals #t)
     (call-local st proc nlocals (jit-jmpr r1)))

    ((_ st proc nlocals <expr>)
     (let ((lprogram (jit-forward))
           (lcompiled (jit-forward))
           (lunwrap (jit-forward))
           (lsmob (jit-forward))
           (lsmobrt (jit-forward))
           (lshuffle (jit-forward))
           (lerror (jit-forward)))

       (local-ref st proc r0)

       ;; Unwrap local to get procedure. Doing similar work done in
       ;; vm-engine's label statement `apply:'.
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
       (local-set! st proc r0)
       (jump lunwrap)

       ;; Test for applicable smob.
       (jit-link lsmob)
       (scm-typ7 r1 r2)
       (jump (scm-not-smobp r1) lerror)
       (jit-movr f5 r0)
       (jit-prepare)
       (jit-pushargr r0)
       (call-c "scm_do_smob_applicable_p")
       (jit-retval r0)
       (jump (jit-beqi r0 (imm 0)) lerror)
       (jit-addi reg-nlocals reg-nlocals (imm 1))
       (last-arg-offset st r1 r2)

       (jit-link lshuffle)
       (jit-ldxr r2 (jit-fp) r1)
       (jit-addi r1 r1 (imm (sizeof '*)))
       (jit-stxr r1 (jit-fp) r2)
       (jit-subi r1 r1 (imm (* 2 (sizeof '*))))
       (jump (jit-bgei r1 (stored-ref st 0)) lshuffle)

       (jit-prepare)
       (jit-pushargr f5)
       (call-c "scm_do_smob_apply_trampoline")
       (jit-retval r0)
       (local-set! st proc r0)
       (jump lunwrap)

       ;; Show error message.
       (jit-link lerror)
       (error-wrong-type-apply r0)

       ;; Local is program, look for JIT compiled code.
       (jit-link lprogram)
       (jump (scm-program-is-jit-compiled r2) lcompiled)

       ;; Does not have compiled code, compile the callee procedure.
       (jit-prepare)
       (jit-pushargr r0)
       (jit-calli %compile-procedure)
       (local-ref st proc r0)

       ;; Has compiled code.
       (jit-link lcompiled)
       (scm-program-jit-compiled-code r1 r0)
       <expr>))))

(define-syntax compile-label
  (syntax-rules (compile-lightning with-frame)

    ;; Non tail call
    ((_ st1 proc nlocals callee-addr #f)
     (compile-label st1 st2 proc nlocals callee-addr
                    (with-frame st2 proc
                                (compile-lightning st2 (jit-forward)))))

    ;; Tail call
    ((_ st1 proc nlocals callee-addr #t)
     (compile-label st1 st2 proc nlocals callee-addr
                    (compile-lightning st2 (jit-forward))))

    ((_ st1 st2 proc nlocals callee-addr body)
     (let ((st2 (make-lightning (program->trace callee-addr)
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
(define-string-pointer procedure)
(define-string-pointer apply)

(define (wrong-type-apply proc)
  (scm-error 'wrong-type-arg #f "Wrong type to apply: ~S" `(,proc) `(,proc)))

(define %error-wrong-type-apply
  (let ((f (lambda (proc)
             (wrong-type-apply (pointer->scm proc)))))
    (procedure->pointer '* f '(*))))

(define-syntax error-wrong-type-apply
  (syntax-rules ()
    ((_ proc)
     (begin
       (jit-prepare)
       (jit-pushargr proc)
       (jit-calli %error-wrong-type-apply)
       (jit-reti (scm->pointer *unspecified*))))))

(define-syntax error-wrong-type-arg-msg
  (syntax-rules ()
    ((_ subr pos reg expected)
     (begin
       (jit-prepare)
       (jit-pushargi subr)
       (jit-pushargi (imm pos))
       (jit-pushargr reg)
       (jit-pushargi expected)
       (call-c "scm_wrong_type_arg_msg")
       (jit-reti (scm->pointer *unspecified*))))))

(define-syntax error-out-of-range
  (syntax-rules ()
    ((_ subr expr)
     (begin
       (jit-prepare)
       (jit-pushargi subr)
       expr
       (call-c "scm_out_of_range")
       (jit-reti (scm->pointer *unspecified*))))))

(define-syntax assert-wrong-num-args
  (syntax-rules ()
    ((_ st jit-op expected local)
     (let ((lexit (jit-forward)))
       (jump (jit-op reg-nlocals (imm expected)) lexit)
       (jit-prepare)
       (jit-pushargr (local-ref st 0))
       (call-c "scm_wrong_num_args")
       (jit-reti (scm->pointer *unspecified*))
       (jit-link lexit)))))

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
       (jump (jit-op reg-nlocals (imm expected))
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
       (when (< offset 0)
         (vm-handle-interrupts st))
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
         (vm-handle-interrupts st))
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

;; If var is not variable, resolve with `resolver' and move the resolved value
;; to var's address. Otherwise, var is `variable', move it to dst.
;;
;; At the moment, the variable is resolved at compilation time. May better to do
;; this variable resolution at run time when lightning code get preserved per
;; scheme procedure. Otherwise there is no way to update the procedure once it's
;; started.
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
         (jit-prepare)
         (jit-pushargr reg-thread)
         (jit-pushargr-d f0)
         (call-c "scm_do_inline_from_double")
         (jit-retval r0)
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
         (jit-prepare)
         (jit-pushargr reg-thread)
         (jit-pushargr-d f0)
         (call-c "scm_do_inline_from_double")
         (jit-retval r0)
         (jump lexit)

         (jit-link lcall)
         (jit-prepare)
         (jit-pushargr reg)
         (jit-pushargi (imm 6))
         (call-c cname)
         (jit-retval r0)

         (jit-link lexit)
         (local-set! st dst r0))))))


;;;
;;; VM operations
;;;

;; Groupings are taken from "guile/libguile/vm-engine.c".


;;; Call and return
;;; ---------------

(define-vm-op (call st proc nlocals)
  (vm-handle-interrupts st)
  (jit-movi reg-nlocals (imm nlocals))
  (call-local st proc nlocals #f))

(define-vm-op (call-label st proc nlocals label)
  (vm-handle-interrupts st)
  (jit-movi reg-nlocals (imm nlocals))
  (cond
   ((in-same-procedure? st label)
    (with-frame st proc (jump (resolve-dst st label))))
   ((compiled-node st (offset-addr st label))
    =>
    (lambda (node)
      (with-frame st proc (jump node))))
   (else
    (compile-label st proc nlocals (offset-addr st label) #f))))

(define-vm-op (tail-call st nlocals)
  (vm-handle-interrupts st)
  (jit-movi reg-nlocals (imm nlocals))
  (call-local st 0 nlocals #t))

(define-vm-op (tail-call-label st nlocals label)
  (vm-handle-interrupts st)
  (jit-movi reg-nlocals (imm nlocals))
  (cond
   ((in-same-procedure? st label)
    (jump (resolve-dst st label)))
   ((compiled-node st (offset-addr st label))
    =>
    (lambda (node)
      (jump node)))
   (else
    (compile-label st 0 nlocals (offset-addr st label) #t))))

(define-vm-op (receive st dst proc nlocals)
  (local-set! st dst reg-retval))

(define-vm-op (receive-values st proc allow-extra? nvalues)
  (local-set! st (+ proc 1) reg-retval))

(define-vm-op (return st dst)
  (let ((lexit (jit-forward)))
    (local-ref st dst reg-retval)
    (local-set! st 1 reg-retval)
    (jit-movi reg-nlocals (imm 2))

    (local-ref st -2 r1)
    (jump (jit-beqr r1 (jit-fp)) lexit)
    (jit-subr r1 (jit-fp) r1)
    (jit-divi r1 r1 (imm (sizeof '*)))
    (jit-addr reg-nlocals reg-nlocals r1)

    (jit-link lexit)
    (return-jmp st)))

(define-vm-op (return-values st)
  (let ((lexit (jit-forward)))
    (local-ref st 1 reg-retval)

    (local-ref st -2 r1)
    (jump (jit-beqr r1 (jit-fp)) lexit)
    (jit-subr r1 (jit-fp) r1)
    (jit-divi r1 r1 (imm (sizeof '*)))
    (jit-addr reg-nlocals reg-nlocals r1)

    (jit-link lexit)
    (return-jmp st)))


;;; Specialized call stubs
;;; ----------------------

(define-vm-op (subr-call st ptr-idx)
  (let ((lcall (jit-forward)))

    ;; `subr-call' accepts up to 10 arguments only.
    (jit-prepare)
    (for-each (lambda (n)
                (jump (jit-blei reg-nlocals (imm (+ n 1))) lcall)
                (jit-pushargr (local-ref st (+ n 1))))
              (iota 10))

    (jit-link lcall)
    (local-ref st 0 r0)
    (scm-program-free-variable-ref r0 r0 ptr-idx)
    (scm-pointer-value r1 r0)
    (jit-callr r1)
    (jit-retval reg-retval)
    (return-value-list st 0 r0 r1 r2)
    (return-jmp st)))

(define-vm-op (foreign-call st cif-idx ptr-idx)
  (local-ref st 0 r0)
  (scm-program-free-variable-ref r1 r0 cif-idx)
  (scm-program-free-variable-ref r2 r0 ptr-idx)
  (jit-addi r0 (jit-fp) (stored-ref st 1))
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr r1)
  (jit-pushargr r2)
  (jit-pushargr r0)
  (call-c "scm_do_foreign_call")
  (jit-retval reg-retval)
  (return-value-list st 0 r0 r1 r2)
  (return-jmp st))

;;; XXX: continuation-call
;;; XXX: compose-continuation

(define-vm-op (tail-apply st)
  (let ((lcons (jit-forward))
        (lrt (jit-forward))
        (lcompiled (jit-forward))
        (lshift (jit-forward))
        (lshuffle (jit-forward))
        (ljitcall (jit-forward)))

    ;; Test whether callee has JIT compiled code.
    (local-ref st 1 r2)
    (scm-cell-object r0 r2 0)
    ;; XXX: Unwrap callee as done in `call-local'
    (jump (scm-program-is-jit-compiled r0) lcompiled)

    ;; No JIT compiled code.
    (jit-prepare)
    (jit-pushargr r2)
    (jit-calli %compile-procedure)
    (local-ref st 1 r2)

    ;; Has JIT compiled code.
    (jit-link lcompiled)

    ;; Index for last local, a list containing rest of arguments.
    (last-arg-offset st f5 r0)
    (jit-subi reg-nlocals reg-nlocals (imm 2))

    ;; Local offset for shifting.
    (last-arg-offset st f1 r1)
    (jit-movi r1 (stored-ref st 0))

    ;; Shift non-list locals.
    (jit-link lshift)
    (jit-addi f0 r1 (imm (sizeof '*)))
    (jit-ldxr f0 (jit-fp) f0)
    (jit-stxr r1 (jit-fp) f0)
    (jit-addi r1 r1 (imm (sizeof '*)))
    (jump (jit-bler r1 f1) lshift)

    ;; Load last local.
    (jit-ldxr r0 (jit-fp) f5)

    ;; Expand list contents to local.
    (jit-link lshuffle)
    (jump (scm-is-null r0) ljitcall)
    (scm-car f0 r0)
    (jit-stxr r1 (jit-fp) f0)
    (scm-cdr r0 r0)
    (jit-addi r1 r1 (imm (sizeof '*)))
    (jit-addi reg-nlocals reg-nlocals (imm 1))
    (jump lshuffle)

    ;; Jump to the JIT compiled code.
    (jit-link ljitcall)
    (scm-program-jit-compiled-code r1 r2)
    (jit-jmpr r1)))

;;; XXX: call/cc
;;; XXX: abort

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

;;; XXX: Any way to ensure enough space allocated for nlocals?
(define-vm-op (alloc-frame st nlocals)
  (let ((lshuffle (jit-forward))
        (lexit (jit-forward)))

    (jit-movi f0 scm-undefined)
    (jit-movr r1 reg-nlocals)
    (jit-addi r1 r1 (imm 1))
    (jit-muli r1 r1 (imm (sizeof '*)))

    ;; Using r1 as offset of location to store.
    (jit-link lshuffle)
    (jump (jit-bgei r1 (stored-ref st nlocals)) lexit)
    (jit-addi r1 r1 (imm (sizeof '*)))
    (jit-stxr r1 (jit-fp) f0)
    (jump lshuffle)

    (jit-link lexit)
    (jit-movi reg-nlocals (imm nlocals))))

(define-vm-op (reset-frame st nlocals)
  (jit-movi reg-nlocals (imm nlocals)))

(define-vm-op (assert-nargs-ee/locals st expected locals)
  ;; XXX: Refill SCM_UNDEFINED?
  (assert-wrong-num-args st jit-beqi expected 0)
  (jit-movi reg-nlocals (imm (+ expected locals))))

;;; XXX: br-if-npos-gt

(define-vm-op (bind-kwargs st nreq flags nreq-and-opt ntotal kw-offset)
  (jit-prepare)
  (jit-pushargr (jit-fp))
  (jit-pushargi (stored-ref st 0))
  (jit-pushargr reg-nlocals)
  (jit-pushargi (imm (+ (lightning-pc st) (* (lightning-ip st) 4))))
  (jit-pushargi (imm nreq))
  (jit-pushargi (imm flags))
  (jit-pushargi (imm nreq-and-opt))
  (jit-pushargi (imm ntotal))
  (jit-pushargi (imm kw-offset))
  (call-c "scm_do_bind_kwargs"))

(define-vm-op (bind-rest st dst)
  (let ((lrefill (jit-forward))
        (lcons (jit-forward))
        (lexit (jit-forward)))

    (last-arg-offset st f5 r0)          ; f5 = initial local index.
    (jit-movi r0 (scm->pointer '()))    ; r0 = initial list.
    (jit-movi f0 scm-undefined)

    (jump (jit-bgti reg-nlocals (imm dst)) lcons)

    ;; Refill the locals with SCM_UNDEFINED.
    (jit-link lrefill)
    (jit-addi f5 f5 (imm (sizeof '*)))
    (jit-stxr f5 (jit-fp) f0)
    (jump (jit-bgei f5 (stored-ref st dst)) lexit)
    (jump lrefill)

    ;; Create a list.  Using register f5 to preserve the register
    ;; contents from callee C function "scm_do_inline_cons" and other
    ;; bookkeepings done in lightning.
    (jit-link lcons)
    (jit-ldxr r2 (jit-fp) f5)
    (jit-stxr f5 (jit-fp) f0)
    (scm-inline-cons r0 r2 r0)
    (jit-subi f5 f5 (imm (sizeof '*)))
    (jump (jit-bgei f5 (stored-ref st dst)) lcons)

    (jit-link lexit)
    (jit-movi reg-nlocals (imm (+ dst 1)))
    (local-set! st dst r0)))


;;; Branching instructions
;;; ----------------------

(define-vm-op (br st dst)
  (when (< dst 0)
    (vm-handle-interrupts st))
  (jump (resolve-dst st dst)))

(define-vm-br-unary-immediate-op (br-if-true st a invert offset)
  ((if invert jit-beqi jit-bnei)
   (local-ref st a)
   (scm->pointer #f)))

(define-vm-br-unary-immediate-op (br-if-null st a invert offset)
  ((if invert jit-bnei jit-beqi)
   (local-ref st a)
   (scm->pointer '())))

(define-vm-op (br-if-nil st a invert offset)
  (when (< offset 0)
    (vm-handle-interrupts st))
  (local-ref st a r0)
  (jit-movi r1 (imm (logior (pointer-address (scm->pointer #f))
                            (pointer-address (scm->pointer '())))))
  (jit-comr r1 r1)
  (jit-andr r1 r0 r1)
  (jump ((if invert jit-bnei jit-beqi) r1 (scm->pointer #f))
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

    ;; Storing address of JIT compiled code.
    (let ((closure-addr (jit-movi r1 (imm 0))))
      (scm-set-cell-object r0 2 r1)
      (jump lnext)

      ;; Do the JIT compilation at the time of closure creation.
      (let* ((trace (program->trace (offset-addr st offset)))
             (lightning (make-lightning trace
                                        (lightning-nodes st)
                                        (offset-addr st offset))))
        (jit-patch closure-addr)
        (compile-lightning lightning (jit-forward))))

    (jit-link lnext)
    (when (< 0 nfree)
      (jit-movi r1 (scm->pointer #f))
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
;;; vp->stack_base'. ip is `ip + offset', which is easy to handle in
;;; vm-regular interpreter, but fragment of code to compile in
;;; vm-lightning. `registers', is argument passed from `scm_call_n'.
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

;; (define-vm-op (prompt st tag escape-only? proc-slot handler-offset)
;;   (jit-prepare)
;;   (jit-pushargr reg-thread)
;;   ;; Pushing SCM_DYNSTACK_PROMPT_ESCAPE_ONLY
;;   (jit-pushargi (if escape-only? (imm 16) (imm 0)))
;;   (jit-pushargr (local-ref st tag))
;;   (jit-pushargr (jit-fp))
;;   (jit-pushargr (jit-fp))
;;   (jit-pushargi (imm (+ (lightning-pc st)
;;                         (* (+ (lightning-ip st) handler-offset) 4))))
;;   (jit-pushargi (imm 0))
;;   (call-c "scm_do_dynstack_push_prompt"))

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
    (scm-thread-dynamic-state r0)
    (scm-cell-object r0 r0 1)

    ;; r1 = fluid, from local:
    (local-ref st src r1)

    ;; r2 = num, vector index.
    (scm-cell-object r2 r1 0)
    (jit-rshi r2 r2 (imm 8))
    (jit-addi r2 r2 (imm 1))
    (jit-muli r2 r2 (imm 8))

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

;;; XXX: JIT code not inlined. Need to add test to see string width,
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
  (jit-muli r1 r1 (imm (sizeof '*)))
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
  (jit-muli r1 r1 (imm (sizeof '*)))
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

;;; XXX: Not working.

;; (define-syntax-rule (validate-struct-ref obj idx tmp1 tmp2 subr)
;;   (let ((l1 (jit-forward))
;;         (l2 (jit-forward)))
;;     (validate-struct obj tmp1 subr)
;;     (jump (scm-not-inump idx) l1)
;;     (scm-i-inumr tmp1 idx)
;;     (jump (jit-bmsi tmp1 tmp1) l1)

;;     (scm-struct-vtable tmp2 obj)
;;     (scm-struct-data-ref tmp2 tmp2 scm-vtable-index-size)

;;     (jump (jit-bltr tmp2 tmp1) l2)

;;     (jit-link l1)
;;     (jit-prepare)
;;     (jit-pushargr obj)
;;     (jit-pushargr idx)
;;     (call-c "scm_struct_ref")

;;     (jit-link l2)))

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
  (jit-muli r1 r1 (imm (sizeof '*)))
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
;;; XXX: bv-u8-ref
;;; XXX: bv-s8-ref
;;; XXX: bv-u16-ref
;;; XXX: bv-s16-ref
;;; XXX: bv-u32-ref
;;; XXX: bv-s32-ref
;;; XXX: bv-u64-ref
;;; XXX: bv-s64-ref
;;; XXX: bv-f32-ref
;;; XXX: bv-f64-ref
;;; XXX: bv-u8-set!
;;; XXX: bv-s8-set!
;;; XXX: bv-u16-set!
;;; XXX: bv-s16-set!
;;; XXX: bv-u32-set!
;;; XXX: bv-s32-set!
;;; XXX: bv-u64-set!
;;; XXX: bv-s64-set!
;;; XXX: bv-f32-set!
;;; XXX: bv-f64-set!


;;;
;;; Compilation
;;;

(define-syntax-rule (with-jit-state . expr)
    (parameterize ((jit-state (jit-new-state)))
      (call-with-values
          (lambda () . expr)
        (lambda vals
          (jit-destroy-state)
          (apply values vals)))))

(define (compile-lightning st entry)
  "Compile <lightning> data specified by ST to native code using
lightning, with ENTRY as lightning's node to itself."

  (define-syntax-rule (destination-label st)
    (hashq-ref (lightning-labels st) (lightning-ip st)))

  (define-syntax-rule (assemble-one st ip-x-op)
    (let* ((ip (car ip-x-op))
           (op (cdr ip-x-op))
           (instr (car op))
           (args (cdr op)))
      (set-lightning-ip! st ip)
      (let ((emitter (hashq-ref *vm-instr* instr)))
        (let ((verbosity (lightning-verbosity)))
          (when (and verbosity (<= 3 verbosity))
            (jit-note (format #f "~a" op) (lightning-ip st))))
        ;; Link if this bytecode intruction is labeled as destination.
        (cond ((destination-label st)
               =>
               (lambda (label)
                 (jit-link label))))
        (or (and emitter (apply emitter st args))
            (debug 0 "compile-lightning: VM op `~a' not found~%" instr)))))

  (let* ((program-or-addr (lightning-pc st))
         (addr (ensure-program-addr program-or-addr))
         (trace (lightning-trace st))
         (name (trace-name trace)))

    (hashq-set! (lightning-nodes st) addr entry)
    (jit-note name addr)

    ;; Link and compile the entry point.
    (jit-link entry)
    (jit-patch entry)
    (debug 1 ";;; compile-lightning: Start compiling ~a (~x)~%" name addr)
    (let lp ((ops (trace-ops trace)))
      (unless (null? ops)
        (assemble-one st (car ops))
        (lp (cdr ops))))
    (debug 1 ";;; compile-lightning: Finished compiling ~a (~x)~%" name addr)

    entry))

(define-syntax-rule (compile-procedure proc)
  "Compile bytecode of procedure PROC to native code, and save the
compiled result."
  (with-jit-state
   (jit-prolog)
   (let ((entry (jit-forward))
         (lightning (make-lightning (program->trace proc)
                                    (make-hash-table)
                                    (ensure-program-addr proc))))
     (compile-lightning lightning entry)
     (jit-epilog)
     (jit-realize)
     (let* ((estimated-code-size (jit-code-size))
            (bv (make-bytevector estimated-code-size)))
       (jit-set-code (bytevector->pointer bv)
                     (imm estimated-code-size))
       (jit-emit)
       (jit-code-guardian bv)
       (set-jit-compiled-code! proc (jit-address entry))
       (make-bytevector-executable! bv)

       ;; Debug output.
       (let ((verbosity (lightning-verbosity)))
         (when (and verbosity (<= 2 verbosity))
           (format #t ";;; nodes:~%")
           (hash-for-each (lambda (k v)
                            (format #t ";;;   0x~x => ~a~%" k v))
                          (lightning-nodes lightning)))
         (when (and verbosity (<= 3 verbosity))
           (write-code-to-file (format #f "/tmp/~a.o" (procedure-name proc))
                               (bytevector->pointer bv))
           (jit-print)
           (jit-clear-state)))))))

(define %compile-procedure
  (let ((f (lambda (proc*)
             (let ((proc (pointer->scm proc*)))
               (compile-procedure proc)))))
    (procedure->pointer void f '(*))))


;;;
;;; Code generation and execution
;;;

(define (write-code-to-file file pointer)
  (call-with-output-file file
    (lambda (port)
      (put-bytevector port (pointer->bytevector pointer (jit-code-size))))))

(define (call-lightning proc . args)
  "Compile PROC with lightning, and run with ARGS."
  (pointer->scm
   (c-call-lightning (thread-i-data (current-thread)) proc args)))

(define (c-call-lightning thread proc args)
  "Compile PROC with lightning and run with ARGS, within THREAD."

  (define-syntax stack-size
    (identifier-syntax (* 12 4096 (sizeof '*))))

  (define-syntax-rule (smob? obj)
    (let ((*obj (scm->pointer obj)))
      (cond
       ((< 0 (logand (pointer-address *obj) 6))
        #f)
       (else
        (= 127 (logand (pointer-address (dereference-pointer *obj)) #x7f))))))

  (define-syntax-rule (struct-applicable? obj)
    (let* ((cell0 (dereference-pointer (scm->pointer obj)))
           (vtable-data-addr (- (pointer-address cell0) 1)))
      (< 0 (logand (pointer-address
                    (dereference-pointer
                     (make-pointer (+ vtable-data-addr (sizeof '*)))))
                   scm-vtable-flag-applicable))))

  (define %smob-applicable?
    (pointer->procedure
     '* (dynamic-func "scm_do_smob_applicable_p" (dynamic-link)) '(*)))

  (define-syntax-rule (smob-applicable? obj)
    (not (eq? %null-pointer (%smob-applicable? (scm->pointer obj)))))

  (define-syntax-rule (vm-prolog ra-reg)
    (begin
      ;; XXX: Allocating constant amount at beginning of function call.
      ;; Stack size not expanded at runtime.
      ;; (jit-allocai (imm stack-size))
      (jit-frame (imm stack-size))

      ;; Initial dynamic link, frame pointer.
      (jit-subi (jit-fp) (jit-fp) (imm stack-size))
      (jit-stxi (frame-local 0) (jit-fp) (jit-fp))

      ;; Return address.
      (jit-stxi (frame-local 1) (jit-fp) ra-reg)

      ;; Argument 0, self procedure.
      (jit-movi r0 (scm->pointer proc))
      (jit-stxi (frame-local 2) (jit-fp) r0)

      ;; Pointers of given args.
      (let lp ((args args) (offset 3))
        (unless (null? args)
          (jit-movi r0 (scm->pointer (car args)))
          (jit-stxi (frame-local offset) (jit-fp) r0)
          (lp (cdr args) (+ offset 1))))

      ;; Initialize registers.
      (jit-movi reg-nlocals (imm (+ (length args) 1)))
      (jit-movi reg-thread thread)
      (jit-movi reg-retval scm-undefined)))

  (define epilog-string
    (string->pointer "vm-epilog: r1=%d\n"))

  ;; Check number of return values, call C function `scm_values' if
  ;; 1 < number of values.
  (define-syntax-rule (vm-epilog)
    (let ((lshuffle (jit-forward))
          (lvalues (jit-forward))
          (lcall (jit-forward))
          (lexit (jit-forward)))

      (jump (jit-beqi reg-nlocals (imm 2)) lexit)

      (jit-link lvalues)
      (jit-movi r0 (scm->pointer '()))
      (jit-subi r1 reg-nlocals (imm 1))
      (jit-muli r1 r1 (imm (sizeof '*)))
      (jit-addi r1 r1 (frame-local 2))

      (jit-link lshuffle)
      (jump (jit-blei r1 (frame-local 2)) lcall)
      (jit-ldxr r2 (jit-fp) r1)
      (scm-inline-cons r0 r2 r0)
      (jit-subi r1 r1 (imm (sizeof '*)))
      (jump lshuffle)

      (jit-link lcall)
      (jit-prepare)
      (jit-pushargr r0)
      (call-c "scm_values")
      (jit-retval reg-retval)

      (jit-link lexit)
      (jit-addi (jit-fp) (jit-fp) (imm stack-size))
      (jit-retr reg-retval)))

  (cond
   ((not (program? proc))
    (cond
     ((and (struct? proc)
           (struct-applicable? proc))
      (c-call-lightning thread (struct-ref proc 0) args))
     ((and (smob? proc)
           (smob-applicable? proc))
      (c-call-lightning thread (smob-apply-trampoline proc) (cons proc args)))
     (else
      (wrong-type-apply proc))))
   ((jit-compiled-code proc)
    =>
    (lambda (compiled)
      (debug 1 ";;; calling jit compiled code of ~a at 0x~x.~%" proc compiled)
      (with-jit-state
       (jit-prolog)
       (let ((return-address (jit-movi r1 (imm 0))))
         (vm-prolog r1)
         (jit-movi r0 (make-pointer compiled))
         (jit-jmpr r0)
         (jit-patch return-address))
       (vm-epilog)
       (jit-epilog)
       (jit-realize)
       (let* ((fptr (jit-emit))
              (thunk (pointer->procedure '* fptr '())))
         (let ((verbosity (lightning-verbosity)))
           (when (and verbosity (<= 3 verbosity))
             (jit-print)
             (jit-clear-state)))
         (thunk)))))
   (else
    (compile-procedure proc)
    (c-call-lightning thread proc args))))


;; This procedure is called from C function `vm_lightning'.
(define (vm-lightning thread fp registers nargs resume)
  (let* ((addr->scm
          (lambda (addr)
            (pointer->scm (dereference-pointer (make-pointer addr)))))
         (deref (lambda (addr)
                  (dereference-pointer (make-pointer addr))))
         (args (let lp ((n (- nargs 1)) (acc '()))
                 (if (< n 1)
                     acc
                     (lp (- n 1)
                         (cons (addr->scm (+ fp (* n (sizeof '*))))
                               acc))))))
    (pointer->scm
     (c-call-lightning (make-pointer thread) (addr->scm fp) args))))


;;;
;;; Initialization
;;;

(init-jit "")
(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_lightning")
