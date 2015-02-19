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

;;; Bytecode -> native code with lightning.

;;; Code:

(define-module (system vm lightning)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (language bytecode)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm basm)
  #:use-module (system vm debug)
  #:use-module (system vm lightning binding)
  #:use-module (system vm program)
  #:use-module (system vm vm)
  #:export (compile-lightning
            call-lightning c-call-lightning))

;;;
;;; Auxiliary
;;;

;; Modified later by function defined in "vm-lightning.c". Defined with
;; dummy body to silent warning message.
(define thread-i-data *unspecified*)

(define *vm-instr* (make-hash-table))

;; State used during compilation.
(define-record-type <lightning>
  (%make-lightning asm nodes ip labels pc sp thread nargs cached modified)
  lightning?

  ;; State from bytecode-asm.
  (asm lightning-asm)

  ;; Hash table containing compiled nodes.
  (nodes lightning-nodes)

  ;; Current bytecode IP.
  (ip lightning-ip set-lightning-ip!)

  ;; Label objects used by lightning.
  (labels lightning-labels)

  ;; Address of byte-compiled program code.
  (pc lightning-pc)

  ;; Stack pointer
  (sp lightning-sp)

  ;; Thread
  (thread lightning-thread)

  ;; Number of arguments.
  (nargs lightning-nargs)

  ;; Registers for cache.
  (cached lightning-cached set-lightning-cached!)

  ;; Modified cached registers, to be saved before calling procedure.
  (modified lightning-modified set-lightning-modified!))

(define-record-type <nretvals>
  (make-nretvals n)
  nretvals?
  (n nretvals-n set-nretvals-n!))

(define* (make-lightning asm nodes sp thread nargs pc #:optional
                         (ip 0)
                         (labels (make-hash-table)))
  (for-each (lambda (labeled-ip)
              (when (< labeled-ip (basm-ip asm))
                (hashq-set! labels labeled-ip (jit-forward))))
            (basm-labeled-ips asm))
  (%make-lightning asm nodes ip labels pc sp thread nargs #f #f))

(define-syntax define-vm-op
  (syntax-rules ()
    ((_ (op st . args) body ...)
     (hashq-set! *vm-instr* 'op (lambda (st . args)
                                  body ... )))))

(define (resolve-dst st offset)
  "Resolve jump destination with <lightning> state ST and OFFSET given as
argument in VM operation."
  (hashq-ref (lightning-labels st) (+ (lightning-ip st) offset)))

(define (dereference-scm pointer)
  (pointer->scm (dereference-pointer pointer)))

;; XXX: For x86-64.
(define *cache-registers*
  (vector v0 v1 v2 v3 f0 f1 f2 f3 f4))

(define (reg=? a b)
  "Compare pointer address of register A and B."
  (= (pointer-address a) (pointer-address b)))

(define (stored-ref st n)
  "Memory address of ST's local N."
  (imm (- (lightning-sp st) (* (sizeof '*) n))))

;;; === With argument caching ===

;; ;; XXX: Analyze which registers to cache, take benchmarks.
;; (define (cache-locals st nlocals)
;;   "Load memory contents of current locals from ST.
;; Naively load from 0 to (min NLOCALS (number of available cache registers))."
;;   (let* ((num-regs (vector-length *cache-registers*))
;;          (regs (make-vector (min nlocals num-regs))))
;;     (let lp ((n 0))
;;       (when (and (< n nlocals) (< n num-regs))
;;         (let ((reg (vector-ref *cache-registers* n)))
;;           (jit-ldxi reg (jit-fp) (stored-ref st n))
;;           (vector-set! regs n reg))
;;         (lp (+ n 1))))
;;     (set-lightning-cached! st regs)
;;     (set-lightning-modified! st (make-vector nlocals #f))))

;; (define (save-locals st)
;;   "Store modified registers in cache to memory."
;;   (let* ((cache (lightning-cached st))
;;          (ncache (vector-length cache))
;;          (modified (lightning-modified st)))
;;     (let lp ((n 0))
;;       (when (< n ncache)
;;         (when (vector-ref modified n)
;;           (jit-stxi (stored-ref st n) (jit-fp) (vector-ref cache n)))
;;         (lp (+ n 1))))))

;; (define-syntax local-ref
;;   (syntax-rules ()
;;     ((local-ref st n)
;;      (local-ref st n r0))
;;     ((local-ref st n reg)
;;      (or (let ((cache (lightning-cached st)))
;;            (and (< n (vector-length cache))
;;                 (vector-ref cache n)))
;;          (begin
;;            (jit-ldxi reg (jit-fp) (stored-ref st n))
;;            reg)))))

;; (define (local-set! st dst reg)
;;   (or (and (< dst (vector-length (lightning-cached st)))
;;            (let ((regb (vector-ref (lightning-cached st) dst)))
;;              (or (reg=? regb reg)
;;                  (jit-movr regb reg))
;;              (vector-set! (lightning-modified st) dst #t)))
;;       (jit-stxi (stored-ref st dst) (jit-fp) reg)))

;; (define (local-set-immediate! st dst val)
;;   (or (and (< dst (vector-length (lightning-cached st)))
;;            (let ((regb (vector-ref (lightning-cached st) dst)))
;;              (jit-movi regb val)
;;              (vector-set! (lightning-modified st) dst #t)))
;;       (and (jit-movi r0 val)
;;            (jit-stxi (stored-ref st dst) (jit-fp) r0))))

;;; === Without argument caching ===

(define (cache-locals st nlocals)
  *unspecified*)

(define (save-locals st)
  *unspecified*)

(define-syntax local-ref
  (syntax-rules ()
    ((local-ref st n)
     (local-ref st n r0))
    ((local-ref st n reg)
     (begin
       (jit-ldxi reg (jit-fp) (stored-ref st n))
       reg))))

(define (local-set! st dst reg)
  (jit-stxi (stored-ref st dst) (jit-fp) reg))

(define (local-set-immediate! st dst val)
  (jit-movi r0 val)
  (jit-stxi (stored-ref st dst) (jit-fp) r0))

;;;

(define (offset-addr st offset)
  (+ (lightning-pc st) (* 4 (+ (lightning-ip st) offset))))

(define-syntax-rule (tc7-variable) 7)

(define-syntax-rule (tc7-program) 69)

(define-syntax-rule (tc16-real) 535)

(define undefined (make-pointer #x904))

(define-syntax-rule (c-pointer name)
  (dynamic-func name (dynamic-link)))

(define-syntax-rule (c-inline name)
  (c-pointer name))

(define (call-primitive st proc nlocals primitive)
  (let* ((pargs (program-arguments-alist primitive))
         (required (cdr (assoc 'required pargs)))
         (optionals (cdr (assoc 'optional pargs)))
         (rest (cdr (assoc 'rest pargs)))
         (num-required (length required))
         (num-optionals (length optionals))
         (num-req+opts (+ num-required num-optionals)))

    ;; If the primitive contained `rest' argument, firstly build a list
    ;; for rest argument by calling `scm_list_n' or moving empty list to
    ;; register.
    (when rest
      (if (< (- nlocals 1) num-req+opts)
          (jit-movi r1 (scm->pointer '()))
          (begin
            (jit-prepare)
            (for-each (lambda (i)
                        (jit-pushargr
                         (local-ref st (+ proc i 1 num-req+opts))))
                      (iota (- nlocals num-req+opts 1)))
            ;; Additional `undefined', to end the arguments.
            (jit-pushargi undefined)
            (jit-calli (c-pointer "scm_list_n"))
            (jit-retval r1))))

    ;; Pushing argument for primitive procedure.
    (jit-prepare)
    (for-each (lambda (i)
                (jit-pushargr (local-ref st (+ proc 1 i))))
              (iota (min (- nlocals 1) num-req+opts)))

    ;; Filling in unspecified optionals, if any.
    (when (not (null? optionals))
      (for-each (lambda (i)
                  (jit-pushargi undefined))
                (iota (- num-req+opts (- nlocals 1)))))
    (when rest
      (jit-pushargr r1)))

  (jit-calli (car (program-free-variables primitive)))
  (jit-retval r0)
  (jit-stxi (stored-ref st (+ proc 1)) (jit-fp) r0))

(define (call-scm st proc-or-addr)
  (let* ((addr (ensure-program-addr proc-or-addr))
         (callee (hashq-ref (lightning-nodes st) addr)))
    (jit-patch-at (jit-jmpi) callee)))

(define (call-scm/returned st proc proc-or-addr)
  "Call local PROC in ST. PROC-OR-ADDR is the address of program code or program
procedure itself. Address to return after this call will get patched."
  ;; Stack poionter stored in (jit-fp), decreasing for `proc * word' size to
  ;; shift the locals.  Then patching the address after the jmp, so that
  ;; called procedure can jump back. Two locals below proc get overwritten by
  ;; callee.
  (let ((addr (jit-movi r0 (imm 0))))
    ;; Store return address.
    (jit-stxi (stored-ref st (- proc 1)) (jit-fp) r0)
    ;; Store dynamic link.
    (jit-stxi (stored-ref st (- proc 2)) (jit-fp) (jit-fp))
    ;; Put new frame pointer for the callee.
    (jit-subi (jit-fp) (jit-fp) (imm (* (sizeof '*) proc)))
    (call-scm st proc-or-addr)
    (jit-patch addr)))

(define (current-callee st)
  (hashq-ref (basm-callers (lightning-asm st)) (lightning-ip st)))

(define-syntax-rule (define-label l body ...)
  (begin (jit-link l) body ...))

;; XXX: Add pre and post as in vm-engine.c?
(define-syntax-rule (vm-handle-interrupts st)
  (let ((l1 (jit-forward)))
    (jit-movi r0 (lightning-thread st))
    (jit-ldxi r0 r0 (imm #x104)) ; scm_i_thread.pending_asyncs
    (jit-patch-at (jit-bmci r0 (imm 1)) l1)
    (jit-prepare)
    (jit-calli (c-pointer "scm_async_tick"))
    (jit-link l1)))

(define-syntax define-vm-br-binary-op
  (syntax-rules ()
    ((_ (name st a b invert offset)
        fx-op fx-invert-op fl-op fl-invert-op cname)
     (define-vm-op (name st a b invert offset)
       (vm-handle-interrupts st)
       (let ((l1 (jit-forward))
             (l2 (jit-forward))
             (l3 (jit-forward))
             (l4 (jit-forward))
             (rega (local-ref st a r1))
             (regb (local-ref st b r2)))

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

         (jit-link l2)
         (jit-patch-at (jit-bmsi rega (imm 6)) l3)
         (jit-ldr r0 rega)
         (jit-patch-at (jit-bnei r0 (imm (tc16-real))) l3)
         (jit-patch-at (jit-bmsi regb (imm 6)) l3)
         (jit-ldr r0 regb)
         (jit-patch-at (jit-bnei r0 (imm (tc16-real))) l3)
         (jit-ldxi-d f5 rega (imm (* 2 (sizeof '*))))
         (jit-ldxi-d f6 regb (imm (* 2 (sizeof '*))))
         (jit-patch-at
          ((if invert fl-invert-op fl-op) f5 f6)
          (resolve-dst st offset))
         (jit-patch-at (jit-jmpi) l4)

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
         (if (variable? var)
             (local-set-immediate! st dst (scm->pointer var))
             (let ((resolved resolver ...))
               ;; (jit-movi r0 (scm->pointer resolved))
               ;; VM does this while resolving the var.
               ;; (jit-sti (offset->pointer var-offset) r0)
               ;; (local-set! st dst r0)
               (local-set-immediate! st dst (scm->pointer resolved)))))))))

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
         (jit-patch-at (jit-bnei r0 (imm (tc16-real))) l3)
         (jit-patch-at (jit-bmsi regb (imm 6)) l3)
         (jit-ldr r0 regb)
         (jit-patch-at (jit-bnei r0 (imm (tc16-real))) l3)
         (jit-ldxi-d f5 rega (imm (* 2 (sizeof '*))))
         (jit-ldxi-d f6 regb (imm (* 2 (sizeof '*))))
         (fl-op f5 f5 f6)
         (jit-prepare)
         (jit-pushargi (lightning-thread st))
         (jit-pushargr-d f5)
         (jit-calli (c-inline "scm_do_inline_from_double"))
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
         (jit-patch-at (jit-bnei r0 (imm (tc16-real))) l2)
         (jit-ldr r0 regb)
         (jit-patch-at (jit-bnei r0 (imm (tc16-real))) l2)
         (jit-ldxi-d f5 rega (imm 16))
         (jit-ldxi-d f6 regb (imm 16))
         (fl-op f5 f5 f6)
         (jit-prepare)
         (jit-pushargi (lightning-thread st))
         (jit-pushargr-d f5)
         (jit-calli (c-inline "scm_do_inline_from_double"))
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
         (jit-patch-at (jit-bnei r0 (imm (tc16-real))) l2)
         (jit-ldxi-d f5 reg (imm 16))
         (jit-movi r0 (imm 1))
         (jit-extr-d f6 r0)
         (fl-op f5 f5 f6)
         (jit-prepare)
         (jit-pushargi (lightning-thread st))
         (jit-pushargr-d f5)
         (jit-calli (c-inline "scm_do_inline_from_double"))
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
;; Assertions not done.


;;; Call and return
;;; ---------------

;; XXX: Ensure enough space allocated for nlocals.
(define-vm-op (call st proc nlocals)
  (save-locals st)
  (let ((callee (current-callee st)))
    (cond
     ((primitive? callee)
      (call-primitive st proc nlocals callee))
     ((call? callee)
      (call-scm/returned st proc (call-node callee)))
     ((closure? callee)
      (call-scm/returned st proc (closure-addr callee)))
     (else
      (call-scm/returned st proc callee)))))

(define-vm-op (call-label st proc nlocals label)
  (save-locals st)
  (call-scm/returned st proc (offset-addr st label)))

(define-vm-op (tail-call st nlocals)
  (save-locals st)
  (let ((callee (current-callee st)))
    (cond
     ((primitive? callee)
      (call-primitive st 0 nlocals callee))
     ((call? callee)
      (call-scm st (call-node callee)))
     ((closure? callee)
      (call-scm st (closure-addr callee)))
     (else
      (call-scm st callee)))))

(define-vm-op (tail-call-label st nlocals label)
  (save-locals st)
  (call-scm st (offset-addr st label)))

;; Return value stored in local-ref (proc + 1) by callee. Local variable not
;; resolved at compile time.
(define-vm-op (receive st dst proc nlocals)
  (cache-locals st (lightning-nargs st))
  ;; (cache-locals st nlocals)
  (local-set! st dst (local-ref st (+ proc 1))))

(define-vm-op (receive-values st proc allow-extra? nvalues)
  (save-locals st))

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

(define-vm-op (return st dst)
  (vm-handle-interrupts st)
  ;; Store dst to local-ref 1
  (jit-stxi (stored-ref st 1) (jit-fp) (local-ref st dst))
  (return-jmp st))

(define-vm-op (return-values st)
  (vm-handle-interrupts st)
  (save-locals st)
  (return-jmp st))

;;; Specialized call stubs
;;; ----------------------

(define-vm-op (builtin-ref st dst src)
  (jit-prepare)
  (jit-pushargi (imm src))
  (jit-calli (c-pointer "scm_do_vm_builtin_ref"))
  (jit-retval r0)
  (local-set! st dst r0))

;;; Function prologues
;;; ------------------

;; XXX: Move stack pointer, call jit-allocai when necessary.
(define-vm-op (assert-nargs-ee/locals st expected locals)
  ;; (cache-locals st (+ expected locals))
  (cache-locals st expected)
  )

;; XXX: Does nothing.
(define-vm-op (alloc-frame st nlocals)
  ;; (cache-locals st (lightning-nargs st))
  *unspecified*)

;; XXX: Modify to manage (jit-fp) with absolute value of nlocal?
;;
;; Caching was once causing infinite loops and disabled. Brought back
;; after running `run-nfa' procedure, locals were mixed up from callee.
(define-vm-op (reset-frame st nlocals)
  ;; (save-locals st)
  (cache-locals st (lightning-nargs st))
  ;; (cache-locals st nlocals)
  ;; *unspecified*
  )

;;; Branching instructions
;;; ----------------------

(define-vm-op (br st dst)
  (jit-patch-at (jit-jmpi) (resolve-dst st dst)))

(define-vm-br-binary-op (br-if-< st a b invert offset)
  jit-bltr jit-bger jit-bltr-d jit-bunltr-d "scm_less_p")

(define-vm-br-binary-op (br-if-= st a b invert offset)
  jit-beqr jit-bner jit-beqr-d jit-bner-d "scm_num_eq_p")

(define-vm-op (br-if-true st a invert offset)
  (jit-patch-at
   ((if invert jit-beqi jit-bnei)
    (local-ref st a)
    (scm->pointer #f))
   (resolve-dst st offset)))

(define-vm-op (br-if-null st a invert offset)
  (jit-patch-at
   ((if invert jit-bnei jit-beqi)
    (local-ref st a)
    (scm->pointer '()))
   (resolve-dst st offset)))


;;; Lexical binding instructions
;;; ----------------------------

(define-vm-op (mov st dst src)
  (local-set! st dst (local-ref st src)))

(define-vm-op (box st dst src)
  (jit-prepare)
  (jit-pushargi (lightning-thread st))
  (jit-pushargi (imm (tc7-variable)))
  (jit-pushargr (local-ref st src))
  (jit-calli (c-pointer "scm_do_inline_cell"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (box-ref st dst src)
  (jit-ldxi r0 (local-ref st src) (imm (sizeof '*)))
  (local-set! st dst r0))

(define-vm-op (box-set! st dst src)
  (jit-stxi (imm (sizeof '*)) (local-ref st dst r0) (local-ref st src r1)))

(define-vm-op (make-closure st dst offset nfree)
  (jit-prepare)
  (jit-pushargi (imm (logior (tc7-program) (ash nfree 16))))
  (jit-pushargi (imm (+ nfree 2)))
  (jit-calli (c-pointer "scm_words"))
  (jit-retval r0)

  ;; Storing address of byte-compiled program code.
  (jit-movi r1 (imm (offset-addr st offset)))
  (jit-stxi (imm (sizeof '*)) r0 r1)

  (jit-movi r1 (scm->pointer #f))
  (for-each (lambda (n)
              (jit-stxi (imm (* (sizeof '*) (+ n 2))) r0 r1))
            (iota nfree))
  (local-set! st dst r0))

(define-vm-op (free-ref st dst src idx)
  (jit-ldxi r0 (local-ref st src) (imm (* (sizeof '*) (+ idx 2))))
  (local-set! st dst r0))

(define-vm-op (free-set! st dst src idx)
  (jit-stxi (imm (* (sizeof '*) (+ idx 2)))
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

(define-vm-box-op (toplevel-box st mod-offset sym-offset)
  (module-variable
   (dereference-scm (make-pointer (offset-addr st mod-offset)))
   (dereference-scm (make-pointer (offset-addr st sym-offset)))))

(define-vm-box-op (module-box st mod-offset sym-offset)
  (module-variable
   (resolve-module
    (cdr (pointer->scm (make-pointer (offset-addr st mod-offset)))))
   (dereference-scm (make-pointer (offset-addr st sym-offset)))))


;;; The dynamic environment
;;; -----------------------


;;; String, symbols, and keywords
;;; -----------------------------

(define-vm-op (string-length st dst src)
  (jit-prepare)
  (jit-pushargr (local-ref st src))
  (jit-calli (c-pointer "scm_do_i_string_length"))
  (jit-retval r0)
  (local-set! st dst r0))

;;; Pairs
;;; -----

(define-vm-op (cons st dst car cdr)
  (jit-prepare)
  (jit-pushargi (lightning-thread st))
  (jit-pushargr (local-ref st car r0))
  (jit-pushargr (local-ref st cdr r0))
  (jit-calli (c-inline "scm_do_inline_cons"))
  (jit-retval r0)
  (local-set! st dst r0))

(define-vm-op (car st dst src)
  (jit-ldr r0 (local-ref st src))
  (local-set! st dst r0))

(define-vm-op (cdr st dst src)
  (jit-ldxi r0 (local-ref st src) (imm (sizeof '*)))
  (local-set! st dst r0))

(define-vm-op (set-car! st pair car)
  (jit-str (local-ref st pair r0) (local-ref st car r1)))

(define-vm-op (set-cdr! st pair cdr)
  (jit-stxi (imm (sizeof '*)) (local-ref st pair r0) (local-ref st cdr r1)))


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


;;; Structs and GOOPS
;;; -----------------


;;; Arrays, packed uniform arrays, and bytevectors
;;; ----------------------------------------------


;;;
;;; Running generated function
;;;

(define (write-code-to-file file pointer)
  (call-with-output-file file
    (lambda (port)
      (put-bytevector port (pointer->bytevector pointer (jit-code-size))))))

(define (compile-lightning sp thread nargs args program-or-addr entry
                           nretvals)
  "Compile bytecode procedure specified by PROGRAM-OR-ADDR to native code
using lightning, with stack pointer SP and number of arguments NARGS."
  (let* ((nodes (make-hash-table)))
    (compile-lightning* nodes sp thread nargs args program-or-addr entry #t
                        nretvals)))

(define (compile-lightning* nodes sp thread nargs args program-or-addr
                            entry toplevel? nretvals)
  "Compile bytecode procedure specified by PROGRAM-OR-ADDR, with cached
procedures in CACHED-TABLE hash table. Using SP as stack pointer, the number of
args passed to target procedure is NARGS."

  (define (assemble-one st ip-x-chunk)
    (let* ((ip (car ip-x-chunk))
           (chunk (cdr ip-x-chunk))
           (op (chunk-op chunk))
           (instr (car op))
           (args (cdr op)))
      (set-lightning-ip! st ip)
      (let ((emitter (hashq-ref *vm-instr* instr)))
        ;; For debug.
        ;; (jit-note (format #f "~a" op) (lightning-ip st))
        ;; (format #t "~3d: ~a~%" ip op)
        ;; Link if this bytecode intruction is labeled as destination.
        (cond ((hashq-ref (lightning-labels st) (lightning-ip st))
               => (lambda (label) (jit-link label))))
        (or (and emitter (apply emitter st args))
            (format #t "compile-lightning: VM op not found `~a'~%" instr)))))

  (define (compile-callee st)
    (lambda (callee)
      (let* ((addr (car callee))
             (val (cdr callee))
             (self (hashq-ref nodes addr)))
        (cond
         ((and (basm? val)
               (not (basm-prim-op? val)))
          (let ((sp (lightning-sp st))
                (nargs (basm-nargs val))
                (args (basm-args val)))
            (compile-lightning* nodes sp thread nargs args addr self #f nretvals)))
         ((call? val)
          (let ((result (apply call-lightning (vector->list (call-args val))))
                (runtime-args (call-runtime-args val)))
            (vector-set! runtime-args 0 result)
            (let* ((sp (lightning-sp st))
                   (nargs (vector-length runtime-args))
                   (args runtime-args)
                   (node (compile-lightning* nodes sp thread nargs args result
                                             self #f nretvals)))
              (set-call-node! val (ensure-program-addr result)))))))))

  (define (unwrap-non-program args program-or-addr)
    (cond ((struct? program-or-addr)
           (vector-set! args 0 (struct-ref program-or-addr 0))
           args)
          (else
           args)))

  (let* ((addr (ensure-program-addr program-or-addr))
         (args* (unwrap-non-program args program-or-addr))
         (basm (proc->basm addr args*))
         (name
          (cond ((and (program? program-or-addr)
                      (procedure-name program-or-addr))
                 => symbol->string)
                ((find-program-debug-info
                  (ensure-program-addr program-or-addr))
                 => (lambda (pdi)
                      (cond ((program-debug-info-name pdi)
                             => symbol->string)
                            (else "anon"))))
                (else "anon")))
         (st (make-lightning basm nodes sp thread nargs addr)))

    ;; (format #t ";;; ~a (~a)~%" name addr)

    (hashq-set! nodes addr entry)
    (when toplevel?
      ;; Compile callees.
      (let ((callees (basm->callees-list basm)))
        (for-each
         (lambda (callee)
           (hashq-set! nodes (car callee) (jit-forward)))
         callees)
        (for-each (compile-callee st) callees)))
    (jit-note name addr)

    ;; Link and compile the entry point.
    (jit-link entry)
    (for-each (lambda (chunk)
                (assemble-one st chunk))
              (basm-chunks->alist (basm-chunks basm)))
    (set-nretvals-n! nretvals (basm-nretvals (lightning-asm st)))
    entry))

(define (call-lightning proc . args)
  "Compile PROC with lightning, and run with ARGS."
  (c-call-lightning (thread-i-data (current-thread)) 0 proc args))

;; Called by C code.
(define (c-call-lightning thread vp-fp proc args)
  "Compile PROC with lightning and run with ARGS."
  (define (offset->addr offset)
    ;; (imm (* offset (sizeof ssize_t)))
    (imm (- (+ #xffffffffffffffff 1) (* offset (sizeof ssize_t))))
    )
  (parameterize ((jit-state (jit-new-state)))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (jit-prolog)
        (let* ((szt (sizeof '*))
               ;; XXX: Use vp->sp?
               (sp (jit-allocai (imm (* szt (+ 3 (length args))))))
               (sp-addr (logxor #xffffffff00000000 (pointer-address sp)))
               ;; (sp-addr (- vp-fp 16))
               (addr (jit-movi r1 (imm 0))))

          ;; XXX: Allocating constant amount at beginning of function call.
          ;; Might better to allocate at compile time or runtime.
          ;; (jit-allocai (imm (* 16 4096)))
          (jit-frame (imm (* 4 4096)))

          ;; Initial dynamic link, frame pointer.
          ;; (jit-movi (jit-fp) (imm sp-addr))
          (jit-stxi (offset->addr 1) (jit-fp) (jit-fp))

          ;; Return address.
          (jit-stxi (offset->addr 2) (jit-fp) r1)

          ;; Argument 0, self procedure.
          (jit-movi r0 (scm->pointer proc))
          (jit-stxi (offset->addr 3) (jit-fp) r0)

          ;; Pointers of given args.
          (let lp ((args args) (offset 4))
            (unless (null? args)
              (jit-movi r0 (scm->pointer (car args)))
              (jit-stxi (offset->addr offset) (jit-fp) r0)
              (lp (cdr args) (+ offset 1))))

          ;; Compile the given proc,
          (let* ((entry (jit-forward))
                 (nargs (+ (length args) 1))
                 (args (apply vector proc args))
                 (sp0 (+ sp-addr (* szt nargs)))
                 ;; (sp0 (+ sp-addr (* szt (- nargs 1))))
                 (nretvals (make-nretvals 1)))
            (jit-patch-at (jit-jmpi) entry)
            (compile-lightning sp0 thread nargs args proc entry nretvals)

            ;; Link the return address, get single return value.
            (jit-patch addr)

            ;; Check number of return values, call C function
            ;; scm_values if number of values were not 1.
            (cond ((= (nretvals-n nretvals) 1)
                   (jit-ldxi r0 (jit-fp) (offset->addr 4)))
                  (else
                   (jit-prepare)
                   (for-each (lambda (n)
                               (jit-ldxi r0 (jit-fp) (offset->addr (+ 4 n)))
                               (jit-pushargr r0))
                             (iota (nretvals-n nretvals)))
                   (jit-pushargi undefined)
                   (jit-calli (c-pointer "scm_list_n"))
                   (jit-retval r1)
                   (jit-prepare)
                   (jit-pushargr r1)
                   (jit-calli (c-pointer "scm_values"))
                   (jit-retval r0)))
            (jit-retr r0)))
        (jit-epilog)

        ;; Emit and call the thunk.
        (let* ((fptr (jit-emit))
               (thunk (pointer->procedure '* fptr '())))
          ;; (write-code-to-file
          ;;  (format #f "/tmp/~a.o" (procedure-name proc)) fptr)
          ;; (jit-print)
          ;; (jit-clear-state)
          ;; (newline)
          (pointer->scm (thunk))))
      (lambda ()
        (jit-destroy-state)))))

(define (vm-lightning thread vp ip sp fp stack-limit sp-max-since-gc
                      stack-size stack-base registers nargs resume)
  (let* ((dereference-addr
          (lambda (addr)
            (pointer->scm (dereference-pointer (make-pointer addr)))))
         (deref (lambda (addr)
                  (dereference-pointer (make-pointer addr))))
         (args (let lp ((n (- nargs 1)) (acc '()))
                 (if (< n 1)
                     acc
                     (lp (- n 1)
                         (cons (dereference-addr
                                (+ fp (* n (sizeof '*))))
                               acc))))))
    (c-call-lightning (make-pointer thread)
                      fp
                      (dereference-addr fp)
                      args)))


;;;
;;; Initialization
;;;

(init-jit "")
(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_lightning")
