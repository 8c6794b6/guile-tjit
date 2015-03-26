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

;;; JIT compiler written with lightning, from bytecode to native code.

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
  #:export (compile-lightning
            call-lightning c-call-lightning
            jit-code-guardian)
  #:re-export (lightning-verbosity))


;;;
;;; Auxiliary
;;;

;; Modified later by function defined in "vm-lightning.c". Defined with
;; dummy body to silent warning message.
(define thread-i-data *unspecified*)

(define *vm-instr* (make-hash-table))

;; State used during compilation.
(define-record-type <lightning>
  (%make-lightning trace nodes ip labels pc fp nargs args nretvals indent)
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
  (pc lightning-pc)

  ;; Frame pointer
  (fp lightning-fp)

  ;; Arguments.
  (args lightning-args)

  ;; Number of arguments.
  (nargs lightning-nargs)

  ;; Number of return values.
  (nretvals lightning-nretvals set-lightning-nretvals!)

  ;; Indentation level for debug message.
  (indent lightning-indent))

(define* (make-lightning trace nodes fp nargs args pc
                         indent
                         #:optional
                         (nretvals 1)
                         (ip 0)
                         (labels (make-hash-table)))
  (for-each (lambda (labeled-ip)
              (hashq-set! labels labeled-ip (jit-forward)))
            (trace-labeled-ips trace))
  (%make-lightning trace nodes ip labels pc fp nargs args nretvals
                   indent))

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
(define-inline tc7-program 69)
(define-inline tc16-real 535)
(define-inline f-program-is-jit-compiled #x4000)
(define-inline scm-undefined (make-pointer #x904))


;;;
;;; Registers with specific use
;;;

;; Number of arguments.
(define-inline reg-nargs v0)

;; Return value.
(define-inline reg-retval v1)

;; Number of return values, shared with number of arguments.
(define-inline reg-nretvals v0)

;; Current thread.
(define-inline reg-thread v2)


;;;
;;; SCM macros
;;;

(define-syntax-rule (scm-cell-object dst obj n)
  (jit-ldxi dst obj (imm (* n (sizeof '*)))))

(define-syntax-rule (scm-cell-object-0 dst obj)
  (jit-ldr dst obj))

(define-syntax-rule (scm-cell-object-1 dst obj)
  (jit-ldxi dst obj (imm (sizeof '*))))

(define-syntax-rule (scm-cell-object-2 dst obj)
  (jit-ldxi dst obj (imm (* 2 (sizeof '*)))))

(define-syntax-rule (scm-cell-object-r dst obj reg-offset)
  (jit-ldxr dst obj reg-offset))

(define-syntax-rule (scm-set-cell-object obj n val)
  (jit-stxi (imm (* n (sizeof '*))) obj val))

(define-syntax-rule (scm-set-cell-object-0 obj val)
  (jit-str obj val))

(define-syntax-rule (scm-set-cell-object-1 obj val)
  (jit-stxi (imm (sizeof '*)) obj val))

(define-syntax-rule (scm-set-cell-object-2 obj val)
  (scm-set-cell-object obj 2 val))

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

(define-syntax-rule (scm-program-free-variable-ref dst src index)
  (jit-ldxi dst src (imm (* (+ index 3) (sizeof '*)))))

(define-syntax-rule (scm-pointer-value dst src)
  (scm-cell-object-1 dst src))

(define-syntax-rule (scm-i-vector-length dst src)
  (begin
    (jit-ldr dst src)
    (jit-rshi dst dst (imm 8))))

(define-syntax-rule (scm-program-is-jit-compiled obj)
  (jit-bmsi obj (imm f-program-is-jit-compiled)))

;;;
;;; VM op syntaxes
;;;

(define-syntax define-vm-op
  (syntax-rules ()
    ((_ (op st . args) body ...)
     (hashq-set! *vm-instr* 'op (lambda (st . args)
                                  body ...)))))

(define-syntax-rule (resolve-dst st offset)
  "Resolve jump destination with <lightning> state ST and OFFSET given as
argument in VM operation."
  (hashq-ref (lightning-labels st) (+ (lightning-ip st) offset)))

(define-syntax-rule (dereference-scm pointer)
  (pointer->scm (dereference-pointer pointer)))

(define-syntax-rule (reg=? a b)
  "Compare pointer address of register A and B."
  (= (pointer-address a) (pointer-address b)))

(define-syntax-rule (stored-ref st n)
  "Memory address of ST's local N."
  (imm (- (lightning-fp st) (* n (sizeof '*)))))

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

(define-syntax-rule (local-set-immediate! st dst val)
  (begin
    (jit-movi r0 val)
    (jit-stxi (stored-ref st dst) (jit-fp) r0)))

(define-syntax-rule (offset-addr st offset)
  (+ (lightning-pc st) (* 4 (+ (lightning-ip st) offset))))

(define-syntax-rule (last-arg-offset st dst tmp)
  (begin
    (jit-movr tmp reg-nargs)
    (jit-subi tmp tmp (imm 1))
    (jit-muli tmp tmp (imm (sizeof '*)))
    (jit-movi dst (imm (lightning-fp st)))
    (jit-subr dst dst tmp)))

(define-syntax-rule (c-pointer name)
  (dynamic-func name (dynamic-link)))

;; XXX: Add pre and post as in vm-engine.c?
(define-syntax-rule (vm-handle-interrupts st)
  (let ((l1 (jit-forward)))
    (scm-thread-pending-asyncs r0)
    (jit-patch-at (jit-bmci r0 (imm 1)) l1)
    (jit-prepare)
    (jit-calli (c-pointer "scm_async_tick"))
    (jit-link l1)))

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

(define-syntax with-frame
  ;; Stack poionter stored in (jit-fp), decreasing for `proc * word' size to
  ;; shift the locals.  Then patching the address after the jmp, so that
  ;; called procedure can jump back. Two locals below proc get overwritten by
  ;; callee.
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
       (jit-subi (jit-fp) (jit-fp) (imm (* (sizeof '*) proc)))
       body
       (jit-patch ra)))))

(define-syntax call-local
  (syntax-rules ()
    ((_ st proc nlocals #f)
     (call-local st proc nlocals (with-frame st proc (jit-jmpr r1))))

    ((_ st proc nlocals #t)
     (call-local st proc nlocals (jit-jmpr r1)))

    ((_ st proc nlocals expr)
     (let ((l1 (jit-forward))
           (l2 (jit-forward)))

       (local-ref st proc r0)
       (scm-cell-object-0 r2 r0)
       (jump (scm-program-is-jit-compiled r2) l1)

       ;; Does not have compiled code.
       (call-runtime st proc nlocals)
       ;; XXX: Handle multiple values.
       (local-set! st (+ proc 1) reg-retval)
       (jump l2)

       ;; Has compiled code.
       (jit-link l1)
       (scm-cell-object-2 r1 r0)
       expr

       (jit-link l2)))))

(define-syntax-rule (call-runtime st proc nlocals)
  (begin
    (jit-prepare)
    (for-each (lambda (n)
                (jit-pushargr (local-ref st (+ proc n 1))))
              (iota (- nlocals 1)))
    (jit-pushargi scm-undefined)
    (jit-calli (c-pointer "scm_list_n"))
    (jit-retval r1)
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-pushargr (local-ref st proc))
    (jit-pushargr r1)
    (jit-calli %call-lightning)

    ;; XXX: Add test for SCM_VALUESP.
    (jit-retval reg-retval)))

(define-syntax compile-callee
  (syntax-rules (compile-lightning with-frame)

    ;; Non tail call
    ((_ st1 proc nlocals callee-addr #f)
     (compile-callee st1 st2 proc nlocals callee-addr
                     (with-frame st2 proc
                                 (compile-lightning st2 (jit-forward)))))

    ;; Tail call
    ((_ st1 proc nlocals callee-addr #t)
     (compile-callee st1 st2 proc nlocals callee-addr
                     (compile-lightning st2 (jit-forward))))

    ((_ st1 st2 proc nlocals callee-addr body)
     (cond
      ((program->trace callee-addr nlocals)
       =>
       (lambda (trace)
         (let ((st2 (make-lightning trace
                                    (lightning-nodes st1)
                                    (lightning-fp st1)
                                    nlocals
                                    #f
                                    (ensure-program-addr callee-addr)
                                    (+ 2 (lightning-indent st1)))))
           body)))
      (else
       (debug 1 ";;; Trace failed, calling 0x~x at runtime.~%" callee-addr)
       (call-runtime st1 proc nlocals))))))

(define-syntax-rule (in-same-procedure? st label)
  ;; XXX: Could look the last IP of current procedure.  Instead, looking
  ;; for backward jump at the moment.
  (and (<= 0 (+ (lightning-ip st) label))
       (< label 0)))

(define-syntax-rule (compiled-node st addr)
  (hashq-ref (lightning-nodes st) addr))

(define-syntax-rule (define-link l . body)
  (begin (jit-link l) . body))


;;;
;;; Assertion, validation, and error messages
;;;

;; Define pointer to strings used in error messages as top-level, to
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
(define-string-pointer vector-ref/immediate)
(define-string-pointer vector-set!)
(define-string-pointer vector-set!/immediate)

(define-syntax error-wrong-type-arg-msg
  (syntax-rules ()
    ((_ subr pos reg expected)
     (begin
       (jit-prepare)
       (jit-pushargi subr)
       (jit-pushargi (imm pos))
       (jit-pushargr reg)
       (jit-pushargi expected)
       (jit-calli (c-pointer "scm_wrong_type_arg_msg"))
       (jit-reti (scm->pointer *unspecified*))))))

(define-syntax error-out-of-range
  (syntax-rules ()
    ((_ subr expr)
     (begin
       (jit-prepare)
       (jit-pushargi subr)
       expr
       (jit-calli (c-pointer "scm_out_of_range"))
       (jit-reti (scm->pointer *unspecified*))))))

(define-syntax assert-wrong-num-args
  (syntax-rules ()
    ((_ st jit-op expected local)
     (let ((l1 (jit-forward)))
       (jit-patch-at (jit-op reg-nargs (imm expected)) l1)
       (jit-prepare)
       (jit-pushargr (local-ref st 0))
       (jit-calli (c-pointer "scm_wrong_num_args"))
       (jit-reti (scm->pointer *unspecified*))
       (jit-link l1)))))

(define-syntax-rule (validate-pair pair cell-0 subr pos)
  (let ((l1 (jit-forward))
        (l2 (jit-forward)))
    (jit-patch-at (jit-bmsi pair (imm 6)) l1)
    (scm-cell-object-0 cell-0 pair)
    (jit-patch-at (jit-bmsi cell-0 (imm 1)) l1)
    (jit-patch-at (jit-jmpi) l2)
    (jit-link l1)
    (error-wrong-type-arg-msg subr pos r0 *pair-string)
    (jit-link l2)))

(define-syntax-rule (validate-vector vec cell-0 tag subr)
  (let ((l1 (jit-forward))
        (l2 (jit-forward)))
    (jit-patch-at (jit-bmsi vec (imm 6)) l1)
    (scm-cell-object-0 cell-0 vec)
    (jit-andi tag cell-0 (imm #x7f))
    (jit-patch-at (jit-beqi tag (imm tc7-vector)) l2)
    (jit-link l1)
    (error-wrong-type-arg-msg subr 1 vec *vector-string)
    (jit-link l2)))

(define-syntax-rule (validate-vector-range vec idx tmp1 tmp2 subr)
  ;; Registers `vec' and `tmp' will get dirty after the validation.
  (let ((l1 (jit-forward))
        (l2 (jit-forward)))
    (jit-patch-at (jit-bmci idx (imm 2)) l1)

    (scm-i-inumr tmp1 idx)
    (jit-patch-at (jit-blti tmp1 (imm 0)) l1)
    (scm-i-vector-length tmp2 vec)
    (jit-patch-at (jit-bltr tmp1 tmp2) l2)

    ;; (jit-patch-at (jit-blti idx (imm 2)) l1)
    ;; (scm-i-vector-length tmp vec)
    ;; (scm-makinumr tmp tmp)
    ;; (jit-patch-at (jit-bltr idx tmp) l2)

    (jit-link l1)
    (error-out-of-range subr (jit-pushargr idx))
    (jit-link l2)))

(define-syntax-rule (validate-vector-range/immediate vec idx tmp subr)
  (let ((l1 (jit-forward)))
    (scm-i-vector-length tmp vec)
    (jit-patch-at (jit-bgti tmp (imm idx)) l1)
    (error-out-of-range subr (jit-pushargi (scm-makinumi idx)))
    (jit-link l1)))


;;;
;;; Top-level syntax for specific VM operations
;;;

(define-syntax define-br-nargs-op
  (syntax-rules ()
    ((_ (name st expected offset) jit-op)
     (define-vm-op (name st expected offset)
       (jit-patch-at
        (jit-op reg-nargs (imm expected))
        (resolve-dst st offset))))))

(define-syntax define-vm-br-unary-immediate-op
  (syntax-rules ()
    ((_ (name st a invert offset) expr)
     (define-vm-op (name st a invert offset)
       (when (< offset 0)
         (vm-handle-interrupts st))
       (jit-patch-at expr (resolve-dst st offset))))))

(define-syntax define-vm-br-unary-heap-object-op
  (syntax-rules ()
    ((_ (name st a invert offset) reg expr)
     (define-vm-op (name st a invert offset)
       (when (< offset 0)
         (vm-handle-interrupts st))
       (let ((l1 (jit-forward)))
         (local-ref st a reg)
         (jit-patch-at (jit-bmsi reg (imm 6))
                       (if invert (resolve-dst st offset) l1))
         (scm-cell-object-0 reg reg)
         (jit-patch-at expr
                       (if invert (resolve-dst st offset) l1))
         ;; XXX: Any other way?
         (when (not invert)
           (jit-patch-at (jit-jmpi) (resolve-dst st offset)))
         (jit-link l1))))))

(define-syntax define-vm-br-binary-op
  (syntax-rules ()
    ((_ (name st a b invert offset) cname)
     (define-vm-op (name st a b invert offset)
       (when (< offset 0)
         (vm-handle-interrupts st))
       (let ((l1 (jit-forward)))
         (local-ref st a r0)
         (local-ref st b r1)
         (jit-patch-at
          (jit-beqr r0 r1)
          (if invert l1 (resolve-dst st offset)))

         (jit-prepare)
         (jit-pushargr r0)
         (jit-pushargr r1)
         (jit-calli (c-pointer cname))
         (jit-retval r0)

         (jit-patch-at
          (jit-beqi r0 (scm->pointer #f))
          (if invert (resolve-dst st offset) l1))
         (when (not invert)
           (jit-patch-at (jit-jmpi) (resolve-dst st offset)))

         (jit-link l1))))))

(define-syntax define-vm-br-arithmetic-op
  (syntax-rules ()
    ((_ (name st a b invert offset)
        fx-op fx-invert-op fl-op fl-invert-op cname)
     (define-vm-op (name st a b invert offset)
       (when (< offset 0)
         (vm-handle-interrupts st))
       (let ((l1 (jit-forward))
             (l2 (jit-forward))
             (l3 (jit-forward))
             (l4 (jit-forward))
             (rega (local-ref st a r1))
             (regb (local-ref st b r2)))

         ;; fixnum x fixnum
         (jit-patch-at (jit-bmci rega (imm 2)) l2)
         (jit-patch-at (jit-bmci regb (imm 2)) l1)
         (jit-patch-at
          ((if invert fx-invert-op fx-op) rega regb)
          (resolve-dst st offset))
         (jit-patch-at (jit-jmpi) l4)

         ;; XXX: Convert fixnum to flonum when one of the argument is fixnum,
         ;; and the other flonum.
         (jit-link l1)
         (jit-patch-at (jit-bmsi rega (imm 2)) l3)

         ;; flonum x flonum
         (jit-link l2)
         (jit-patch-at (jit-bmsi rega (imm 6)) l3)
         (jit-ldr r0 rega)
         (jit-patch-at (jit-bnei r0 (imm tc16-real)) l3)
         (jit-patch-at (jit-bmsi regb (imm 6)) l3)
         (jit-ldr r0 regb)
         (jit-patch-at (jit-bnei r0 (imm tc16-real)) l3)
         (jit-ldxi-d f0 rega (imm (* 2 (sizeof '*))))
         (jit-ldxi-d f1 regb (imm (* 2 (sizeof '*))))
         (jit-patch-at
          ((if invert fl-invert-op fl-op) f0 f1)
          (resolve-dst st offset))
         (jit-patch-at (jit-jmpi) l4)

         ;; else
         (jit-link l3)
         (jit-prepare)
         (jit-pushargr rega)
         (jit-pushargr regb)
         (jit-calli (c-pointer cname))
         (jit-retval r0)
         (jit-patch-at
          ((if invert jit-beqi jit-bnei) r0 (scm->pointer #f))
          (resolve-dst st offset))

         (jit-link l4))))))

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
           (local-set-immediate! st dst (scm->pointer resolved))))))))

(define-syntax define-vm-add-sub-op
  (syntax-rules ()
    ((_ (name st dst a b) fx-op-1 fx-op-2 fl-op c-name)
     (define-vm-op (name st dst a b)
       (let ((l1 (jit-forward))
             (l2 (jit-forward))
             (l3 (jit-forward))
             (l4 (jit-forward))
             (rega (local-ref st a r1))
             (regb (local-ref st b r2)))

         ;; Entry: a == small fixnum && b == small fixnum
         (jit-patch-at (jit-bmci rega (imm 2)) l2)
         (jit-patch-at (jit-bmci regb (imm 2)) l1)
         (jit-movr r0 rega)
         (jit-patch-at (fx-op-1 r0 regb) l3)
         (fx-op-2 r0 r0 (imm 2))
         (jit-patch-at (jit-jmpi) l4)

         ;; L1: Check for (a == small fixnum && b == flonm)
         (jit-link l1)
         (jit-patch-at (jit-bmsi rega (imm 2)) l3)

         ;; L2: flonum + flonum
         (jit-link l2)
         (jit-patch-at (jit-bmsi rega (imm 6)) l3)
         (jit-ldr r0 rega)
         (jit-patch-at (jit-bnei r0 (imm tc16-real)) l3)
         (jit-patch-at (jit-bmsi regb (imm 6)) l3)
         (jit-ldr r0 regb)
         (jit-patch-at (jit-bnei r0 (imm tc16-real)) l3)
         (jit-ldxi-d f0 rega (imm (* 2 (sizeof '*))))
         (jit-ldxi-d f1 regb (imm (* 2 (sizeof '*))))
         (fl-op f0 f0 f1)
         (jit-prepare)
         (jit-pushargr reg-thread)
         (jit-pushargr-d f0)
         (jit-calli (c-pointer "scm_do_inline_from_double"))
         (jit-retval r0)
         (jit-patch-at (jit-jmpi) l4)

         ;; L3: Call C function
         (jit-link l3)
         (jit-prepare)
         (jit-pushargr rega)
         (jit-pushargr regb)
         (jit-calli (c-pointer c-name))
         (jit-retval r0)

         (jit-link l4)
         (local-set! st dst r0))))))

(define-syntax define-vm-mul-div-op
  (syntax-rules ()
    ((_  (name st dst a b) fl-op cname)
     (define-vm-op (name st dst a b)
       (let ((l1 (jit-forward))
             (l2 (jit-forward))
             (l3 (jit-forward))
             (rega (local-ref st a r1))
             (regb (local-ref st b r2)))

         (jit-patch-at (jit-bmsi rega (imm 2)) l2)
         (jit-patch-at (jit-bmsi regb (imm 2)) l2)

         (jit-link l1)
         (jit-ldr r0 rega)
         (jit-patch-at (jit-bnei r0 (imm tc16-real)) l2)
         (jit-ldr r0 regb)
         (jit-patch-at (jit-bnei r0 (imm tc16-real)) l2)
         (jit-ldxi-d f0 rega (imm 16))
         (jit-ldxi-d f1 regb (imm 16))
         (fl-op f0 f0 f1)
         (jit-prepare)
         (jit-pushargr reg-thread)
         (jit-pushargr-d f0)
         (jit-calli (c-pointer "scm_do_inline_from_double"))
         (jit-retval r0)
         (jit-patch-at (jit-jmpi) l3)

         (jit-link l2)
         (jit-prepare)
         (jit-pushargr rega)
         (jit-pushargr regb)
         (jit-calli (c-pointer cname))
         (jit-retval r0)

         (jit-link l3)
         (local-set! st dst r0))))))

(define-syntax define-vm-unary-step-op
  (syntax-rules ()
    ((_ (name st dst src) fx-op fl-op cname)
     (define-vm-op (name st dst src)
       (let ((l1 (jit-forward))
             (l2 (jit-forward))
             (l3 (jit-forward))
             (reg (local-ref st src r1)))

         (jit-patch-at (jit-bmci reg (imm 2)) l1)
         (jit-movr r0 reg)
         (jit-patch-at (fx-op r0 (imm 4)) l1)
         (jit-patch-at (jit-jmpi) l3)

         (jit-link l1)
         (jit-ldr r0 reg)
         (jit-patch-at (jit-bnei r0 (imm tc16-real)) l2)
         (jit-ldxi-d f0 reg (imm 16))
         (jit-movi r0 (imm 1))
         (jit-extr-d f1 r0)
         (fl-op f0 f0 f1)
         (jit-prepare)
         (jit-pushargr reg-thread)
         (jit-pushargr-d f0)
         (jit-calli (c-pointer "scm_do_inline_from_double"))
         (jit-retval r0)
         (jit-patch-at (jit-jmpi) l3)

         (jit-link l2)
         (jit-prepare)
         (jit-pushargr reg)
         (jit-pushargi (imm 6))
         (jit-calli (c-pointer cname))
         (jit-retval r0)

         (jit-link l3)
         (local-set! st dst r0))))))


;;;
;;; VM operations
;;;

;; Groupings are taken from "guile/libguile/vm-engine.c".


;;; Call and return
;;; ---------------

;;; XXX: Any way to ensure enough space allocated for nlocals?

(define-vm-op (call st proc nlocals)
  (vm-handle-interrupts st)
  (jit-movi reg-nargs (imm nlocals))
  (call-local st proc nlocals #f))

(define-vm-op (call-label st proc nlocals label)
  (vm-handle-interrupts st)
  (jit-movi reg-nargs (imm nlocals))
  (let ((addr (offset-addr st label)))
    (cond
     ((in-same-procedure? st label)
      (with-frame st proc (jit-patch-at (jit-jmpi) (resolve-dst st label))))
     ((compiled-node st addr)
      =>
      (lambda (node)
        (with-frame st proc (jit-patch-at (jit-jmpi) node))))
     (else
      (compile-callee st proc nlocals addr #f)))))

(define-vm-op (tail-call st nlocals)
  (vm-handle-interrupts st)
  (jit-movi reg-nargs (imm nlocals))
  (call-local st 0 nlocals #t)
  (return-jmp st))

(define-vm-op (tail-call-label st nlocals label)
  (vm-handle-interrupts st)
  (jit-movi reg-nargs (imm nlocals))
  (cond
   ((in-same-procedure? st label)
    (jit-patch-at (jit-jmpi) (resolve-dst st label)))
   ((compiled-node st (offset-addr st label))
    =>
    (lambda (node)
      (jit-patch-at (jit-jmpi) node)))
   (else
    (compile-callee st 0 nlocals (offset-addr st label) #t))))

;; Return value stored in reg-retval by callee.
(define-vm-op (receive st dst proc nlocals)
  (local-set! st dst reg-retval))

(define-vm-op (receive-values st proc allow-extra? nvalues)
  (local-set! st (+ proc 1) reg-retval))

(define-vm-op (return st dst)
  (local-ref st dst reg-retval)
  (jit-movi reg-nretvals (imm 1))
  (return-jmp st))

(define-vm-op (return-values st)
  (local-ref st 1 reg-retval)
  (return-jmp st))


;;; Specialized call stubs
;;; ----------------------

(define-vm-op (subr-call st ptr-idx)
  (let ((l1 (jit-forward)))
    (jit-prepare)
    ;; `subr-call' accepts up to 10 arguments.
    (for-each (lambda (n)
                (jit-patch-at (jit-blei reg-nargs (imm (+ n 1))) l1)
                (jit-pushargr (local-ref st (+ n 1))))
              (iota 10))

    (jit-link l1)
    (local-ref st 0 r0)
    (scm-program-free-variable-ref r0 r0 ptr-idx)
    (scm-pointer-value r1 r0)
    (jit-callr r1)
    (jit-retval reg-retval)

    ;; XXX: Add test for SCM_VALUESP.
    ;; (local-set! st 1 reg-retval)

    (jit-movi reg-nretvals (imm 1))
    (return-jmp st)))

;;; XXX: foreign-call
;;; XXX: continuation-call
;;; XXX: compose-continuation

(define-vm-op (tail-apply st)
  (let ((l1 (jit-forward))
        (l2 (jit-forward))
        (l3 (jit-forward))
        (l4 (jit-forward))
        (l5 (jit-forward))
        (l6 (jit-forward))
        (l7 (jit-forward)))

    ;; Last local, a list containing rest of arguments.
    (last-arg-offset st f5 r0)
    (jit-ldxr r0 (jit-fp) f5)

    ;; Test whether the procedure has JIT compiled code.
    (jit-ldr f0 (local-ref st 1 r2))
    (jit-subi reg-nargs reg-nargs (imm 2))
    (jit-patch-at (scm-program-is-jit-compiled f0) l3)

    ;; No JIT code, making argument list.
    (jit-link l1)
    (jit-patch-at
     (jit-bgei f5 (imm (- (lightning-fp st) (* 2 (sizeof '*)))))
     l2)
    (jit-addi f5 f5 (imm (sizeof '*)))
    (jit-ldxr r2 (jit-fp) f5)
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-pushargr r2)
    (jit-pushargr r0)
    (jit-calli (c-pointer "scm_do_inline_cons"))
    (jit-retval r0)
    (jit-patch-at (jit-jmpi) l1)

    ;; Call `%call-lightning' with thread, proc, and argument list.
    (jit-link l2)
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-pushargr (local-ref st 1 f5))
    (jit-pushargr r0)
    (jit-calli %call-lightning)
    ;; XXX: Add test for SCM_VALUESP.
    (jit-retval reg-retval)
    (jit-movi reg-nretvals (imm 1))
    (jit-patch-at (jit-jmpi) l7)

    ;; Has jit compiled code.
    (jit-link l3)

    ;; Local offset for shifting.
    (last-arg-offset st f1 r1)
    (jit-movi r1 (imm (lightning-fp st)))

    ;; Shift non-list locals.
    (jit-link l4)
    (jit-subi f0 r1 (imm (sizeof '*)))
    (jit-ldxr f0 (jit-fp) f0)
    (jit-stxr r1 (jit-fp) f0)
    (jit-subi r1 r1 (imm (sizeof '*)))
    (jit-patch-at (jit-bger r1 f1) l4)

    ;; Expand list contents to local.
    (jit-link l5)
    (jit-patch-at (jit-beqi r0 (scm->pointer '())) l6)
    (jit-ldr f0 r0)
    (jit-stxr r1 (jit-fp) f0)
    (jit-ldxi r0 r0 (imm (sizeof '*)))
    (jit-subi r1 r1 (imm (sizeof '*)))
    (jit-addi reg-nargs reg-nargs (imm 1))
    (jit-patch-at (jit-jmpi) l5)

    ;; Jump to the JIT compiled code.
    (jit-link l6)
    (jit-ldxi r0 r2 (imm (* 2 (sizeof '*))))
    (jit-jmpr r0)

    (jit-link l7)))

;;; XXX: call/cc
;;; XXX: abort

(define-vm-op (builtin-ref st dst src)
  (jit-prepare)
  (jit-pushargi (imm src))
  (jit-calli (c-pointer "scm_do_vm_builtin_ref"))
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

(define-vm-op (assert-nargs-ee/locals st expected locals)
  (assert-wrong-num-args st jit-beqi expected 0))

(define-vm-op (bind-kwargs st nreq flags nreq-and-opt ntotal kw-offset)
  (jit-prepare)
  (jit-pushargr (jit-fp))
  (jit-pushargi (imm (lightning-fp st)))
  (jit-pushargr reg-nargs)
  (jit-pushargi (imm (+ (lightning-pc st) (* (lightning-ip st) 4))))
  (jit-pushargi (imm nreq))
  (jit-pushargi (imm flags))
  (jit-pushargi (imm nreq-and-opt))
  (jit-pushargi (imm ntotal))
  (jit-pushargi (imm kw-offset))
  (jit-calli (c-pointer "scm_do_bind_kwargs")))

(define-vm-op (bind-rest st dst)
  (let ((l1 (jit-forward))
        (l2 (jit-forward))
        (l3 (jit-forward)))

    (last-arg-offset st f5 r0)        ; f5 = initial local index.
    (jit-movi r0 (scm->pointer '()))    ; r0 = initial list.
    (jit-movi f0 scm-undefined)

    (jit-patch-at (jit-bgti reg-nargs (imm dst)) l2)

    ;; Refill the locals with SCM_UNDEFINED.
    (jit-link l1)
    (jit-subi f5 f5 (imm (sizeof '*)))
    (jit-stxr f5 (jit-fp) f0)
    (jit-patch-at
     (jit-blei f5 (imm (- (lightning-fp st) (* dst (sizeof '*)))))
     l3)
    (jit-patch-at (jit-jmpi) l1)

    ;; Create a list.  Using register f5 to preserve the register
    ;; contents from callee C function "scm_do_inline_cons" and other
    ;; bookkeepings done in lightning.
    (jit-link l2)
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-ldxr r2 (jit-fp) f5)
    (jit-stxr (jit-fp) f5 f0)
    (jit-pushargr r2)
    (jit-pushargr r0)
    (jit-calli (c-pointer "scm_do_inline_cons"))
    (jit-retval r0)
    (jit-addi f5 f5 (imm (sizeof '*)))
    (jit-patch-at
     (jit-bgti f5 (imm (- (lightning-fp st) (* dst (sizeof '*)))))
     l3)

    (jit-patch-at (jit-jmpi) l2)

    (jit-link l3)
    (local-set! st dst r0)

    ;; Updating nargs to prevent following `alloc-frame' to override
    ;; the rest list with #<unspecified>.
    (jit-movi reg-nargs (imm (+ dst 1)))))

(define-vm-op (assert-nargs-ee st expected)
  (assert-wrong-num-args st jit-beqi expected 0))

(define-vm-op (assert-nargs-ge st expected)
  (assert-wrong-num-args st jit-bgei expected 0))

(define-vm-op (assert-nargs-le st expected)
  (assert-wrong-num-args st jit-blei expected 0))

(define-vm-op (alloc-frame st nlocals)
  (let ((l1 (jit-forward))
        (l2 (jit-forward)))

    (jit-movr r1 reg-nargs)

    (jit-link l1)
    (jit-patch-at (jit-bgei r1 (imm nlocals)) l2)

    ;; Doing similar things to `stored-ref' in generated code. Using r2
    ;; as offset of location to store.
    (jit-movi r2 (imm (lightning-fp st)))
    (jit-movr r0 r1)
    (jit-muli r0 r0 (* (imm (sizeof '*))))
    (jit-subr r2 r2 r0)
    (jit-movi r0 scm-undefined)
    (jit-stxr r2 (jit-fp) r0)
    (jit-addi r1 r1 (imm 1))
    (jit-patch-at (jit-jmpi) l1)

    (jit-link l2)
    (jit-movi reg-nargs (imm nlocals))))

(define-vm-op (reset-frame st nlocals)
  (jit-movi reg-nretvals (imm nlocals)))


;;; Branching instructions
;;; ----------------------

(define-vm-op (br st dst)
  (when (< dst 0)
    (vm-handle-interrupts st))
  (jit-patch-at (jit-jmpi) (resolve-dst st dst)))

(define-vm-br-unary-immediate-op (br-if-true st a invert offset)
  ((if invert jit-beqi jit-bnei)
   (local-ref st a)
   (scm->pointer #f)))

(define-vm-br-unary-immediate-op (br-if-null st a invert offset)
  ((if invert jit-bnei jit-beqi)
   (local-ref st a)
   (scm->pointer '())))

;; XXX: br-if-nil

(define-vm-br-unary-heap-object-op (br-if-pair st a invert offset)
  r0 (jit-bmsi r0 (imm 1)))

(define-vm-br-unary-heap-object-op (br-if-struct st a invert offset)
  r0 (begin (jit-andi r0 r0 (imm 7))
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
  (let ((l1 (jit-forward)))
    (local-ref st a r0)
    (jit-patch-at (jit-bmsi r0 (imm 6))
                  (if invert (resolve-dst st offset) l1))
    (jit-ldr r0 r0)
    (jit-patch-at (begin (jit-andi r0 r0 (imm #x7f))
                         (jit-bnei r0 (imm tc7)))
                  (if invert (resolve-dst st offset) l1))
    (when (not invert)
      (jit-patch-at (jit-jmpi) (resolve-dst st offset)))
    (jit-link l1)))

(define-vm-op (br-if-eq st a b invert offset)
  (when (< offset 0)
    (vm-handle-interrupts st))
  (jit-patch-at
   ((if invert jit-bner jit-beqr)
    (local-ref st a r0)
    (local-ref st b r1))
   (resolve-dst st offset)))

(define-vm-br-binary-op (br-if-eqv st a b invert offset)
  "scm_eqv_p")

(define-vm-br-binary-op (br-if-equal st a b invert offset)
  "scm_equal_p")

(define-vm-br-arithmetic-op (br-if-< st a b invert offset)
  jit-bltr jit-bger jit-bltr-d jit-bunltr-d "scm_less_p")

(define-vm-br-arithmetic-op (br-if-<= st a b invert offset)
  jit-bler jit-bgtr jit-bler-d jit-bunler-d "scm_leq_p")

(define-vm-br-arithmetic-op (br-if-= st a b invert offset)
  jit-beqr jit-bner jit-beqr-d jit-bner-d "scm_num_eq_p")


;;; Lexical binding instructions
;;; ----------------------------

(define-vm-op (mov st dst src)
  (local-set! st dst (local-ref st src)))

(define-vm-op (box st dst src)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargi (imm tc7-variable))
  (jit-pushargr (local-ref st src))
  (jit-calli (c-pointer "scm_do_inline_cell"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (box-ref st dst src)
  (local-ref st src r0)
  (scm-cell-object-1 r0 r0)
  (local-set! st dst r0))

(define-vm-op (box-set! st dst src)
  (local-ref st dst r0)
  (local-ref st src r1)
  (scm-set-cell-object-1 r0 r1))

(define-vm-op (make-closure st dst offset nfree)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargi (imm (logior tc7-program (ash nfree 16))))
  (jit-pushargi (imm (+ nfree 3)))
  (jit-calli (c-pointer "scm_do_inline_words"))
  (jit-retval r0)

  ;; Storing address of byte-compiled program code.
  (jit-movi r1 (imm (offset-addr st offset)))
  (scm-set-cell-object-1 r0 r1)

  ;; XXX: Storing JIT compiled code. Could fill in the address with
  ;; already compiled code, but not done yet.
  (jit-movi r1 scm-undefined)
  (scm-set-cell-object-2 r0 r1)

  (jit-movi r1 (scm->pointer #f))
  (for-each (lambda (n)
              (jit-stxi (imm (* (sizeof '*) (+ n 2))) r0 r1))
            (iota nfree))
  (local-set! st dst r0))

(define-vm-op (free-ref st dst src idx)
  (local-ref st src r0)
  (scm-program-free-variable-ref r0 r0 idx)
  (local-set! st dst r0))

(define-vm-op (free-set! st dst src idx)
  (jit-stxi (imm (* (sizeof '*) (+ idx 3)))
            (local-ref st dst r1)
            (local-ref st src r0)))


;;; Immediates and statically allocated non-immediates
;;; --------------------------------------------------

(define-vm-op (make-short-immediate st dst a)
  (local-set-immediate! st dst (imm a)))

(define-vm-op (make-long-immediate st dst a)
  (local-set-immediate! st dst (imm a)))

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


;;; Mutable top-level bindings
;;; --------------------------

(define-vm-op (current-module st dst)
  (jit-prepare)
  (jit-calli (c-pointer "scm_current_module"))
  (jit-retval r0)
  (local-set! st dst r0))

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
;;; "prompt" The pushed dynstack has flag, tag, fp offset, sp offset,
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
;;; vp->ip are set from prompt's value. Then doing SCM_I_LONGJMP with
;;; `regisers' value.
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

(define-vm-op (prompt st tag escape-only? proc-slot handler-offset)
  (jit-prepare)
  (jit-pushargr reg-thread)
  ;; Pushing SCM_DYNSTACK_PROMPT_ESCAPE_ONLY
  (jit-pushargi (if escape-only? (imm 16) (imm 0)))
  (jit-pushargr (local-ref st tag))
  (jit-pushargr (jit-fp))
  (jit-pushargr (jit-fp))
  (jit-pushargi (imm (+ (lightning-pc st)
                        (* (+ (lightning-ip st) handler-offset) 4))))
  (jit-pushargi (imm 0))
  (jit-calli (c-pointer "scm_do_dynstack_push_prompt")))

(define-vm-op (wind st winder unwinder)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr (local-ref st winder))
  (jit-pushargr (local-ref st unwinder))
  (jit-calli (c-pointer "scm_do_dynstack_push_dynwind")))

(define-vm-op (unwind st)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-calli (c-pointer "scm_do_dynstack_pop")))

(define-vm-op (push-fluid st fluid value)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr (local-ref st fluid))
  (jit-pushargr (local-ref st value))
  (jit-calli (c-pointer "scm_do_dynstack_push_fluid")))

(define-vm-op (pop-fluid st)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-calli (c-pointer "scm_do_unwind_fluid")))

(define-vm-op (fluid-ref st dst src)
  (let ((l1 (jit-forward)))
    (local-ref st src r0)

    ;; r0 = fluids, in thread:
    ;;   thread->dynamic_state
    (scm-thread-dynamic-state r0)
    ;;   SCM_I_DYNAMIC_STATE_FLUIDS (dynstack)
    ;;   (i.e. SCM_CELL_WORD_1 (dynstack))
    (jit-ldxi r0 r0 (imm 8))

    ;; r1 = fluid, from local:
    (local-ref st src r1)

    ;; r2 = num, vector index.
    (jit-ldr r2 r1)
    (jit-rshi r2 r2 (imm 8))
    (jit-addi r2 r2 (imm 1))
    (jit-muli r2 r2 (imm 8))

    ;; r0 = fluid value
    (jit-ldxr r0 r0 r2)

    ;; Load default value from local fluid if not set.
    (jit-patch-at (jit-bnei r0 scm-undefined) l1)
    (jit-ldxi r0 r1 (imm 8))
    (jit-link l1)
    (local-set! st dst r0)))

;;; XXX: fluid-set

;;; String, symbols, and keywords
;;; -----------------------------

(define-vm-op (string-length st dst src)
  ;; XXX: Validate string.
  (jit-ldxi r0 (local-ref st src) (imm (* 3 (sizeof '*))))
  (scm-makinumr r0 r0)
  (local-set! st dst r0))

;;; XXX: Inline JIT code.
(define-vm-op (string-ref st dst src idx)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (jit-pushargr (local-ref st idx))
  (jit-calli (c-pointer "scm_string_ref"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (string->number st dst src)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (jit-pushargi scm-undefined)
  (jit-calli (c-pointer "scm_string_to_number"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (string->symbol st dst src)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (jit-calli (c-pointer "scm_string_to_symbol"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (symbol->keyword st dst src)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (jit-calli (c-pointer "scm_symbol_to_keyword"))
  (jit-retval r0)
  (local-set! st dst r0))


;;; Pairs
;;; -----

(define-vm-op (cons st dst car cdr)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargr (local-ref st car))
  (jit-pushargr (local-ref st cdr))
  (jit-calli (c-pointer "scm_do_inline_cons"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (car st dst src)
  (local-ref st src r0)
  (validate-pair r0 r1 *car-string 1)
  (local-set! st dst r1))

(define-vm-op (cdr st dst src)
  (local-ref st src r0)
  (validate-pair r0 r1 *cdr-string 1)
  (scm-cell-object-1 r0 r0)
  (local-set! st dst r0))

(define-vm-op (set-car! st pair car)
  (local-ref st pair r0)
  (validate-pair r0 r1 *set-car!-string 1)
  (local-ref st car r1)
  (scm-set-cell-object-0 r0 r1))

(define-vm-op (set-cdr! st pair cdr)
  (local-ref st pair r0)
  (validate-pair r0 r1 *set-cdr!-string 1)
  (local-ref st cdr r1)
  (scm-set-cell-object-1 r0 r1))


;;; Numeric operations
;;; ------------------

(define-vm-add-sub-op (add st dst a b)
  jit-boaddr jit-subi jit-addr-d "scm_sum")

(define-vm-unary-step-op (add1 st dst src)
  jit-boaddi jit-addr-d "scm_sum")

(define-vm-add-sub-op (sub st dst a b)
  jit-bosubr jit-addi jit-subr-d "scm_difference")

(define-vm-unary-step-op (sub1 st dst src)
  jit-bosubi jit-subr-d "scm_difference")

(define-vm-mul-div-op (mul st dst a b)
  jit-mulr-d "scm_product")

(define-vm-mul-div-op (div st dst a b)
  jit-divr-d "scm_divide")

(define-vm-op (quo st dst a b)
  (jit-prepare)
  (jit-pushargr (local-ref st a))
  (jit-pushargr (local-ref st b))
  (jit-calli (c-pointer "scm_quotient"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (rem st dst a b)
  (jit-prepare)
  (jit-pushargr (local-ref st a))
  (jit-pushargr (local-ref st b))
  (jit-calli (c-pointer "scm_remainder"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (make-vector st dst length init)
  (jit-prepare)
  (jit-pushargr (local-ref st length))
  (jit-pushargr (local-ref st init))
  (jit-calli (c-pointer "scm_make_vector"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (make-vector/immediate st dst length init)
  (jit-prepare)
  (jit-pushargr reg-thread)
  (jit-pushargi (imm (logior tc7-vector (ash length 8))))
  (jit-pushargi (imm (+ length 1)))
  (jit-calli (c-pointer "scm_do_inline_words"))
  (jit-retval r0)
  (local-ref st init r1)
  (for-each (lambda (n)
              (jit-stxi (imm (* (+ n 1) (sizeof '*))) r0 r1))
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
  (jit-rshi r1 r1 (imm 2))
  (jit-addi r1 r1 (imm 1))
  (jit-muli r1 r1 (imm (sizeof '*)))
  (jit-ldxr r0 r0 r1)
  (local-set! st dst r0))

(define-vm-op (vector-ref/immediate st dst src idx)
  (local-ref st src r0)
  (validate-vector r0 r1 r2 *vector-ref/immediate-string)
  (validate-vector-range/immediate r0 idx r1 *vector-ref/immediate-string)
  (scm-cell-object r0 r0 (+ idx 1))
  (local-set! st dst r0))

(define-vm-op (vector-set! st dst idx src)
  (local-ref st dst r0)
  (validate-vector r0 r1 r2 *vector-set!-string)
  (local-ref st idx r1)
  (validate-vector-range r0 r1 r2 f0 *vector-set!-string)
  (jit-rshi r1 r1 (imm 2))
  (jit-addi r1 r1 (imm 1))
  (jit-muli r1 r1 (imm (sizeof '*)))
  (local-ref st src r2)
  (scm-set-cell-object-r r0 r1 r2))

(define-vm-op (vector-set!/immediate st dst idx src)
  (local-ref st dst r0)
  (validate-vector r0 r1 r2 *vector-set!/immediate-string)
  (validate-vector-range/immediate r0 idx r1 *vector-set!/immediate-string)
  (local-ref st src r1)
  (scm-set-cell-object r0 (+ idx 1) r1))


;;; Structs and GOOPS
;;; -----------------

(define-vm-op (struct-vtable st dst src)
  (local-ref st src r0)
  (scm-cell-object-0 r0 r0)
  (jit-subi r0 r0 (imm tc3-struct))
  (scm-cell-object-2 r0 r0)
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
  ;; XXX: Validate struct.
  (local-ref st src r0)
  (local-ref st idx r1)
  (jit-rshi r1 r1 (imm 2))
  (jit-muli r1 r1 (imm (sizeof '*)))
  (scm-cell-object-1 r0 r0)
  (scm-cell-object-r r0 r0 r1)
  (local-set! st dst r0))

(define-vm-op (struct-ref/immediate st dst src idx)
  ;; XXX: Validate struct.
  (local-ref st src r0)
  (scm-cell-object-1 r0 r0)
  (scm-cell-object r0 r0 idx)
  (local-set! st dst r0))

(define-vm-op (struct-set!/immediate st dst idx src)
  ;; XXX: Validate struct.
  (local-ref st dst r0)
  (local-ref st src r1)
  (scm-cell-object-1 r0 r0)
  (scm-set-cell-object r0 idx r1))

;;; XXX: class-of
;;; XXX: allocate-struct
;;; XXX: struct-set!


;;; Arrays, packed uniform arrays, and bytevectors
;;; ----------------------------------------------

;;; load-typed-array
;;; make-array
;;; bv-u8-ref
;;; bv-s8-ref
;;; bv-u16-ref
;;; bv-s16-ref
;;; bv-u32-ref
;;; bv-s32-ref
;;; bv-u64-ref
;;; bv-s64-ref
;;; bv-f32-ref
;;; bv-f64-ref
;;; bv-u8-set!
;;; bv-s8-set!
;;; bv-u16-set!
;;; bv-s16-set!
;;; bv-u32-set!
;;; bv-s32-set!
;;; bv-u64-set!
;;; bv-s64-set!
;;; bv-f32-set!
;;; bv-f64-set!

;;;
;;; Compilation
;;;

(define (compile-lightning st entry)
  "Compile <lightning> data specified by ST to native code using
lightning, with ENTRY as lightning's node to itself."

  (define (destination-label st)
    (hashq-ref (lightning-labels st) (lightning-ip st)))

  (define (assemble-one st ip-x-op)
    (let* ((ip (car ip-x-op))
           (op (cdr ip-x-op))
           (instr (car op))
           (args (cdr op)))
      (set-lightning-ip! st ip)
      (let ((emitter (hashq-ref *vm-instr* instr)))
        (jit-note (format #f "~a" op) (lightning-ip st))
        ;; (debug 2 (make-string (lightning-indent st) #\space))
        ;; (debug 2 "~3d: ~a~%" ip op)
        ;; Link if this bytecode intruction is labeled as destination.
        (cond ((destination-label st)
               =>
               (lambda (label)
                 (jit-link label))))
        (or (and emitter (apply emitter st args))
            (debug 0 "compile-lightning: VM op not found `~a'~%" instr)))))

  (let* ((program-or-addr (lightning-pc st))
         (addr (ensure-program-addr program-or-addr))
         (trace (lightning-trace st))
         (name (program-name program-or-addr)))

    (hashq-set! (lightning-nodes st) addr entry)
    (jit-note name addr)

    ;; Link and compile the entry point.
    (jit-link entry)
    (jit-patch entry)
    (debug 1 ";;; compile-lightning: Start compiling ~a (~x)~%" name addr)
    (for-each (lambda (chunk)
                (assemble-one st chunk))
              (trace-ops trace))
    (debug 1 ";;; compile-lightning: Finished compiling ~a (~x)~%" name addr)

    entry))


;;;
;;; Code generation and execution
;;;

(define (unwrap-non-program args program-or-addr)
  (cond
   ((struct? program-or-addr)
    (vector-set! args 0 (struct-ref program-or-addr 0))
    args)
   (else
    args)))

(define (write-code-to-file file pointer)
  (call-with-output-file file
    (lambda (port)
      (put-bytevector port (pointer->bytevector pointer (jit-code-size))))))

(define (call-lightning proc . args)
  "Compile PROC with lightning, and run with ARGS."
  (c-call-lightning (thread-i-data (current-thread)) proc args))

(define (c-call-lightning* thread proc args)
  "Like `c-call-lightning', but ARGS and PROC are pointers to scheme
values. Returned value of this procedure is a pointer to scheme value."
  (scm->pointer
   (c-call-lightning thread (pointer->scm proc) (pointer->scm args))))

(define %call-lightning
  (procedure->pointer '* c-call-lightning* '(* * *)))

(define-syntax-rule (with-jit-state . expr)
    (parameterize ((jit-state (jit-new-state)))
      (call-with-values
          (lambda () . expr)
        (lambda vals
          (jit-destroy-state)
          (apply values vals)))))

(define (c-call-lightning thread proc args)
  "Compile PROC with lightning and run with ARGS, within THREAD."

  (define-syntax-rule (offset->addr offset)
    ;; `make-pointer' does not accept negative values.
    (imm (- (+ #xffffffffffffffff 1) (* offset (sizeof '*)))))

  (define-syntax-rule (fp->addr fp)
    (logxor #xffffffff00000000 (pointer-address fp)))

  (define-syntax-rule (vm-prolog ra-reg)
    (begin
      ;; XXX: Allocating constant amount at beginning of function call.
      ;; Might better to allocate at compile time or runtime.
      (jit-frame (imm (* 4 4096)))

      ;; Initial dynamic link, frame pointer.
      (jit-stxi (offset->addr 1) (jit-fp) (jit-fp))

      ;; Return address.
      (jit-stxi (offset->addr 2) (jit-fp) ra-reg)

      ;; Argument 0, self procedure.
      (jit-movi r0 (scm->pointer proc))
      (jit-stxi (offset->addr 3) (jit-fp) r0)

      ;; Pointers of given args.
      (let lp ((args args) (offset 4))
        (unless (null? args)
          (jit-movi r0 (scm->pointer (car args)))
          (jit-stxi (offset->addr offset) (jit-fp) r0)
          (lp (cdr args) (+ offset 1))))

      ;; Initialize registers.
      (jit-movi reg-nargs (imm (+ (length args) 1)))
      (jit-movi reg-thread thread)
      (jit-movi reg-retval (scm->pointer *unspecified*))))

  ;; Check number of return values, call C function `scm_values' if
  ;; 1 < number of values.
  (define-syntax-rule (vm-epilog)
    (let ((l1 (jit-forward))
          (l2 (jit-forward))
          (l3 (jit-forward)))

      (jit-patch-at (jit-blei reg-nretvals (imm 1)) l3)

      (jit-movi r0 (scm->pointer '()))
      (jit-subi reg-nargs reg-nargs (imm 2))
      (jit-movr r2 reg-nargs)
      (jit-muli r2 r2 (imm (sizeof '*)))
      (jit-movi r1 (offset->addr 4))
      (jit-subr r1 r1 r2)

      (jit-link l1)
      (jit-prepare)
      (jit-pushargr reg-thread)
      (jit-ldxr r2 (jit-fp) r1)
      (jit-pushargr r2)
      (jit-pushargr r0)
      (jit-calli (c-pointer "scm_do_inline_cons"))
      (jit-retval r0)
      (jit-addi r1 r1 (imm (sizeof '*)))
      (jit-patch-at (jit-blei r1 (offset->addr 4)) l1)

      (jit-link l2)
      (jit-prepare)
      (jit-pushargr r0)
      (jit-calli (c-pointer "scm_values"))
      (jit-retval reg-retval)

      (jit-link l3)
      (jit-retr reg-retval)))

  (cond
   ((jit-compiled-code proc)
    =>
    (lambda (compiled)
      (debug 1 ";;; found jit compiled code of ~a at 0x~x.~%" proc compiled)
      (with-jit-state
       (jit-prolog)
       (let* ((fp (jit-allocai (imm (* (+ 3 (length args)) (sizeof '*)))))
              (return-address (jit-movi r1 (imm 0))))
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
         (pointer->scm (thunk))))))
   ((program->trace proc (+ (length args) 1))
    =>
    (lambda (trace)
      (with-jit-state
       (jit-prolog)
       (let* ((entry (jit-forward))
              (fp (jit-allocai (imm (* (+ 3 (length args)) (sizeof '*)))))
              (nargs (+ (length args) 1))
              (args (apply vector proc args))
              (fp0 (+ (fp->addr fp) (* (sizeof '*) nargs)))
              (lightning (make-lightning trace
                                         (make-hash-table)
                                         fp0
                                         nargs
                                         args
                                         (ensure-program-addr proc)
                                         0))
              (return-address (jit-movi r1 (imm 0))))
         (vm-prolog r1)
         (compile-lightning lightning entry)
         (jit-patch return-address)
         (vm-epilog)
         (jit-epilog)
         (jit-realize)

         ;; Emit and call the thunk.
         (let* ((estimated-code-size (jit-code-size))
                (bv (make-bytevector estimated-code-size))
                (_ (jit-set-code (bytevector->pointer bv)
                                 (imm estimated-code-size)))
                (fptr (jit-emit))
                (thunk (pointer->procedure '* fptr '())))

           ;; XXX: Any where else to store `bv'?
           (jit-code-guardian bv)
           (set-jit-compiled-code! proc (jit-address entry))
           (make-bytevector-executable! bv)

           (let ((verbosity (lightning-verbosity)))
             (when (and verbosity (<= 2 verbosity))
               (format #t ";;; nodes:~%")
               (hash-for-each (lambda (k v)
                                (format #t ";;;   0x~x => ~a~%" k v))
                              (lightning-nodes lightning)))
             (when (and verbosity (<= 3 verbosity))
               (write-code-to-file
                (format #f "/tmp/~a.o" (procedure-name proc)) fptr)
               (jit-print)
               (jit-clear-state)))

           (debug 1 ";;; set jit compiled code of ~a to ~a~%"
                  proc (jit-address entry))

           (pointer->scm (thunk)))))))
   (else
    (debug 0 ";;; Trace failed, interpreting: ~a~%" (cons proc args))
    (let ((engine (vm-engine)))
      (dynamic-wind
        (lambda () (set-vm-engine! 'regular))
        (lambda () (apply proc args))
        (lambda () (set-vm-engine! engine)))))))


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
    (c-call-lightning (make-pointer thread) (addr->scm fp) args)))


;;;
;;; Initialization
;;;

(init-jit "")
(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_lightning")
