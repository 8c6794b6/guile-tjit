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

;;; Compile CPS to native code.

;;; Code:

(define-module (system vm native tjit compile-native)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps types)
  #:use-module (language cps utils)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit variables)
  #:export (compile-native))


;;;
;;; Scheme constants and syntax
;;;

(define %scm-make-tjit-retval
  (dynamic-pointer "scm_make_tjit_retval" (dynamic-link)))


;;;
;;; Code generation
;;;

(define (move moffs dst src)
  (cond
   ((and (gpr? dst) (constant? src))
    (jit-movi (gpr dst) (constant src)))
   ((and (gpr? dst) (gpr? src))
    (jit-movr (gpr dst) (gpr src)))
   ((and (gpr? dst) (memory? src))
    (jit-ldxi r0 fp (moffs src))
    (jit-movr (gpr dst) r0))

   ((and (fpr? dst) (constant? src))
    (jit-movi-d (fpr dst) (constant src)))
   ((and (fpr? dst) (fpr? src))
    (jit-movr-d (fpr dst) (fpr src)))
   ((and (fpr? dst) (memory? src))
    (jit-ldxi-d f0 fp (moffs src))
    (jit-movr-d (fpr dst) f0))

   ((and (memory? dst) (constant? src))
    (let ((val (ref-value src)))
      (cond
       ((fixnum? val)
        (jit-movi r0 (constant src))
        (jit-stxi (moffs dst) fp r0))
       ((flonum? val)
        (jit-movi-d f0 (constant src))
        (jit-stxi-d (moffs dst) fp f0)))))
   ((and (memory? dst) (gpr? src))
    (jit-stxi (moffs dst) fp (gpr src)))
   ((and (memory? dst) (fpr? src))
    (jit-stxi-d (moffs dst) fp (fpr src)))
   ((and (memory? dst) (memory? src))
    (jit-ldxi r0 fp (moffs src))
    (jit-stxi (moffs dst) fp r0))

   (else
    (debug 3 "*** move: ~a ~a~%" dst src))))

(define (load-frame moffs local type dst)
  (cond
   ((eq? type &exact-integer)
    (cond
     ((gpr? dst)
      (frame-ref (gpr dst) local)
      (jit-rshi (gpr dst) (gpr dst) (imm 2)))
     ((memory? dst)
      (frame-ref r0 local)
      (jit-rshi r0 r0 (imm 2))
      (jit-stxi (moffs dst) fp r0))))
   ((eq? type &flonum)
    (cond
     ((fpr? dst)
      (frame-ref r0 local)
      (scm-real-value (fpr dst) r0))
     ((memory? dst)
      (frame-ref r0 local)
      (scm-real-value f0 r0)
      (jit-stxi-d (moffs dst) fp f0))))
   ((memq type (list &box &procedure &pair))
    (cond
     ((gpr? dst)
      (frame-ref (gpr dst) local))
     ((memory? dst)
      (frame-ref r0 local)
      (jit-stxi (moffs dst) fp r0))))
   ((eq? type &false)
    (cond
     ((gpr? dst)
      (jit-movi (gpr dst) *scm-false*))
     ((memory? dst)
      (jit-movi r0 *scm-false*)
      (jit-stxi-d (moffs dst) fp r0))))
   ((eq? type &true)
    (cond
     ((gpr? dst)
      (jit-movi (gpr dst) *scm-true*))
     ((memory? dst)
      (jit-movi r0 *scm-true*)
      (jit-stxi-d (moffs dst) fp r0))))
   ((eq? type &unspecified)
    (cond
     ((gpr? dst)
      (jit-movi (gpr dst) *scm-unspecified*))
     ((memory? dst)
      (jit-movi r0 *scm-unspecified*)
      (jit-stxi-d (moffs dst) fp r0))))
   ((eq? type &unbound)
    (cond
     ((gpr? dst)
      (jit-movi (gpr dst) *scm-undefined*))
     ((memory? dst)
      (jit-movi r0 *scm-undefined*)
      (jit-stxi-d (moffs dst) fp r0))))
   ((or (return-address? type)
        (dynamic-link? type))
    (values))
   (else
    (error "load-frame" local type dst))))

(define (store-frame moffs local type src)
  (debug 3 ";;; store-frame: local=~a type=~a src=~a~%" local type src)
  (cond
   ;; Type is return address, moving value coupled with type to frame
   ;; local. Return address of VM frame need to be recovered when taking exit
   ;; from inlined procedure call. The actual value for return address is
   ;; captured at the time of bytecode to Scheme IR conversion, and stored in
   ;; snapshot as pointer.
   ((return-address? type)
    (jit-movi r0 (return-address-ip type))
    (frame-set! local r0))

   ;; Type is dynamic link, storing fp to local. Dynamic link is stored as
   ;; offset in type. VM's fp could move, may use different value at the time of
   ;; compilation and execution. Negative offset is ignored, storing to local
   ;; without adding the shift amount.
   ((dynamic-link? type)
    (jit-ldxi r0 fp vp->fp-offset)
    (let* ((amount (* (dynamic-link-offset type) %word-size))
           (ptr (make-signed-pointer amount)))
      (when (< 0 amount)
        (jit-addi r0 r0 ptr)))
    (frame-set! local r0))

   ;; Check type, recover SCM value.
   ((eq? type &exact-integer)
    (cond
     ((constant? src)
      (jit-movi r0 (constant src))
      (jit-lshi r0 r0 (imm 2))
      (jit-addi r0 r0 (imm 2))
      (frame-set! local r0))
     ((gpr? src)
      (jit-lshi r0 (gpr src) (imm 2))
      (jit-addi r0 r0 (imm 2))
      (frame-set! local r0))
     ((memory? src)
      (jit-ldxi r0 fp (moffs src))
      (jit-lshi r0 r0 (imm 2))
      (jit-addi r0 r0 (imm 2))
      (frame-set! local r0))))
   ((eq? type &flonum)
    (cond
     ((constant? src)
      (jit-movi-d f0 (constant src))
      (scm-from-double r0 f0)
      (frame-set! local r0))
     ((fpr? src)
      (scm-from-double r0 (fpr src))
      (frame-set! local r0))
     ((memory? src)
      (jit-ldxi-d f0 fp (moffs src))
      (scm-from-double r0 f0)
      (frame-set! local r0))))
   ((memq type (list &box &procedure &pair))
    (cond
     ((constant? src)
      (jit-movi r0 (constant src))
      (frame-set! local r0))
     ((gpr? src)
      (frame-set! local (gpr src)))
     ((memory? src)
      (jit-ldxi r0 fp (moffs src))
      (frame-set! local r0))))
   ((eq? type &false)
    (jit-movi r0 *scm-false*)
    (frame-set! local r0))
   ((eq? type &true)
    (jit-movi r0 *scm-true*)
    (frame-set! local r0))
   ((eq? type &unbound)
    (jit-movi r0 *scm-undefined*)
    (frame-set! local r0))
   ((eq? type &unspecified)
    (jit-movi r0 *scm-unspecified*)
    (frame-set! local r0))
   (else
    (error "store-frame" local type src))))

(define (maybe-store moffs local-x-types srcs src-unwrapper references)
  "Store src in SRCS to frame when local is not found in REFERENCES.

Locals are loaded with MOFFS to refer memory offset. Returns a hash-table
containing src with SRC-UNWRAPPER applied. Key of the returned hash-table is
local index in LOCAL-X-TYPES."
  (debug 3 ";;; maybe-store:~%")
  (debug 3 ";;;   srcs:          ~a~%" srcs)
  (debug 3 ";;;   local-x-types: ~a~%" local-x-types)
  (debug 3 ";;;   references:    ~a~%" references)
  (let lp ((local-x-types local-x-types)
           (srcs srcs)
           (acc (make-hash-table)))
    (match local-x-types
      (((local . type) . local-x-types)
       (match srcs
         ((src . srcs)
          (let ((unwrapped-src (src-unwrapper src)))
            ;; XXX: Might need to add more conditions for storing dynamic link
            ;; and return addresses, not sure always these should be stored.
            (when (or (dynamic-link? type)
                      (return-address? type)
                      (not (assq local references)))
              (store-frame moffs local type unwrapped-src))
            (hashq-set! acc local unwrapped-src))
          (lp local-x-types srcs acc))
         (()
          (debug 3 ";;;   srcs=null, local-x-types=~a~%" local-x-types)
          acc)))
      (() acc))))

(define (move-or-load-carefully dsts srcs types moffs)
  "Move SRCS to DSTS or load with TYPES and MOFFS, carefully.

Avoids overwriting source in hash-table SRCS while updating destinations in
hash-table DSTS.  If source is not found, load value from frame with using type
from hash-table TYPES and procedure MOFFS to get memory offset.  Hash-table key
of SRCS, DSTS, TYPES are local index number."

  (define (dst-is-full? as bs)
    (let lp ((as as))
      (match as
        ((a . as) (and (member a bs) (lp as)))
        (() #t))))
  (define (in-srcs? var)
    (hash-fold (lambda (k v acc)
                 (or acc (and (equal? v var) (hashq-ref dsts k))))
               #f
               srcs))
  (define (find-src-local var)
    (hash-fold (lambda (k v ret)
                 (or ret (and (equal? v var) k)))
               #f
               srcs))
  (define (dump-move local dst src)
    (debug 3 ";;; molc: [local ~a] (move ~a ~a)~%" local dst src))
  (define (dump-load local dst type)
    (debug 3 ";;; molc: [local ~a] loading to ~a, type=~a~%" local dst type))

  (let ((dsts-list (hash-map->list cons dsts))
        (car-< (lambda (a b) (< (car a) (car b)))))
    (debug 3 ";;; molc: dsts: ~a~%" (sort dsts-list car-<))
    (debug 3 ";;; molc: srcs: ~a~%" (sort (hash-map->list cons srcs) car-<))
    (let lp ((dsts dsts-list))
      (match dsts
        (((local . dst-var) . rest)
         (cond
          ((in-srcs? dst-var)
           => (lambda (src-var)
                (cond
                 ((equal? dst-var src-var)
                  (hashq-remove! srcs local)
                  (lp rest))
                 (else
                  (let ((srcs-list (hash-map->list cons srcs)))
                    (cond
                     ((dst-is-full? (map cdr dsts) (map cdr srcs-list))
                      ;; When all of the elements in dsts are in srcs, move one
                      ;; of the srcs to temporary location.  `-2' is for gpr R1
                      ;; or fpr F1 in lightning, used as scratch register in
                      ;; this module.
                      (let ((tmp (or (and (fpr? src-var) (make-fpr -2))
                                     (make-gpr -2)))
                            (src-local (find-src-local src-var)))
                        (dump-move local tmp src-var)
                        (move moffs tmp src-var)
                        (hashq-set! srcs src-local tmp)
                        (lp dsts)))
                     (else
                      ;; Rotate the list and try again.
                      (lp (append rest (list (cons local dst-var)))))))))))
          ((hashq-ref srcs local)
           => (lambda (src-var)
                (when (not (equal? src-var dst-var))
                  (dump-move local dst-var src-var)
                  (move moffs dst-var src-var))
                (hashq-remove! srcs local)
                (lp rest)))
          (else
           (dump-load local dst-var (hashq-ref types local))
           (let ((type (hashq-ref types local)))
             ;; XXX: Add test to check the case ignoring `load-frame'.
             (when type
               (load-frame moffs local type dst-var)))
           (lp rest))))
        (() (values))))))

(define (dump-bailout ip exit-id code)
  (let ((verbosity (lightning-verbosity)))
    (when (and verbosity (<= 4 verbosity))
      (call-with-output-file
          (format #f "/tmp/bailout-~x-~4,,,'0@a.o" ip exit-id)
        (lambda (port)
          (put-bytevector port code)
          (jit-print))))))


;;;
;;; The Compiler
;;;

(define (compile-cps cps env kstart entry-ip snapshots loop-args fp-offset
                     fragment trampoline linked-ip lowest-offset)

  ;; Internal states.
  (define current-side-exit 0)
  (define loop-locals #f)
  (define loop-vars #f)

  (define (env-ref i)
    (vector-ref env i))

  (define (moffs x)
    (make-signed-pointer (+  fp-offset (* (ref-value x) %word-size))))

  (define (compile-link args)
    (let* ((linked-fragment (get-fragment linked-ip))
           (loop-locals (fragment-loop-locals linked-fragment)))
      (define (shift-offset offset locals)
        ;; Shifting locals with given offset.
        (debug 3 ";;; compile-link: shift-offset, offset=~a~%" offset)
        (map (match-lambda ((local . type)
                            `(,(- local offset) . ,type)))
             locals))
      (debug 3 ";;; compile-link: args=~a~%" args)
      (cond
       ((hashq-ref snapshots current-side-exit)
        => (match-lambda
            (($ $snapshot local-offset _ local-x-types)
             (debug 3 ";;; compile-link: local-offset=~a~%" local-offset)
             ;; Store unpassed variables. When (not fragment), shifting locals
             ;; for references used in `maybe-store'.
             ;;
             ;; XXX: Move the shifting code somewhere else.
             ;;
             (let* ((dst-table (make-hash-table))
                    (shift-amount (if fragment
                                      lowest-offset
                                      local-offset))
                    (references (shift-offset shift-amount loop-locals))
                    (src-table (maybe-store moffs local-x-types args env-ref
                                            references))
                    (type-table (make-hash-table)))

               ;; Shift the locals in source code. Shifting after the call to
               ;; `maybe-store', to store lower frame info first.
               (let ((tmp (make-hash-table)))
                 (hash-for-each (lambda (k v)
                                  (hashq-set! tmp (- k local-offset) v))
                                src-table)
                 (set! src-table tmp))

               ;; Prepare arguments for linked trace.
               (let lp ((locals loop-locals)
                        (dsts (fragment-loop-vars linked-fragment)))
                 (match locals
                   (((local . type) . locals)
                    (hashq-set! type-table local type)
                    (match dsts
                      ((dst . dsts)
                       ;; Shiting with lowest offset.
                       (hashq-set! dst-table (- local lowest-offset) dst)
                       (lp locals dsts))
                      (()
                       (debug 3 ";;; compile-link: dsts=null, locals=~a~%"
                              locals))))
                   (_ (values))))

               ;; Move variables to linked trace.
               (move-or-load-carefully dst-table src-table type-table moffs))

             ;; Shift back FP and sync. Shifting back lowest-offset moved by
             ;; `%return', and shifting for local-offset to match the FP in
             ;; middle of procedure call.
             (when (< lowest-offset 0)
               (debug 3 ";;; compile-link: shifting FP, lowest-offset=~a~%"
                      lowest-offset)
               (let ((old-fp r0)
                     (new-fp r1)
                     (shift (- local-offset lowest-offset)))
                 (jit-ldxi old-fp fp vp->fp-offset)
                 (jit-addi new-fp old-fp (imm (* shift %word-size)))
                 (scm-frame-set-dynamic-link! new-fp old-fp)
                 (jit-stxi vp->fp-offset fp new-fp)
                 (vm-sync-fp new-fp)))

             ;; Jumping from loop-less root trace, shifting FP.
             ;;
             ;; XXX: Add more tests for checking loop-less root traces.
             (when (not fragment)
               (debug 3 ";;; compile-link: shifting FP, loop-less root~%")
               (let ((vp->fp r0))
                 (jit-ldxi vp->fp fp vp->fp-offset)
                 (jit-addi vp->fp vp->fp (imm (* local-offset %word-size)))
                 (jit-stxi vp->fp-offset fp vp->fp)
                 (vm-sync-fp vp->fp)))

             ;; Jump to the beginning of loop in linked code.
             (jumpi (fragment-loop-address linked-fragment)))))
       (else
        (debug 3 ";;; compile-link: IP is 0, snapshot not found~%")))))

  (define (compile-bailout next-ip args)
    (define-syntax-rule (scm-i-makinumi n)
      (make-signed-pointer (+ (ash n 2) 2)))
    (define (store-snapshot snapshot)
      (match snapshot
        (($ $snapshot local-offset nlocals local-x-types)
         (debug 3 ";;; store-snapshot:~%")
         (debug 3 ";;;   current-side-exit: ~a~%" current-side-exit)
         (debug 3 ";;;   local-offset: ~a~%" local-offset)
         (debug 3 ";;;   nlocals: ~a~%" nlocals)
         (debug 3 ";;;   local-x-types: ~a~%" local-x-types)
         (debug 3 ";;;   args: ~a~%" args)
         (cond
          ((and (not fragment) (= current-side-exit 0))
           ;; No need to recover the frame with snapshot.  Still snapshot data
           ;; is used, so that the bytevector of compiled native code could be
           ;; stored in fragment, to avoid garbage collection.
           ;;
           (values nlocals local-offset snapshot))
          (else
           ;; Matching ends when no more values found in local, args
           ;; exceeding the number of locals are ignored.
           ;;
           ;; XXX: Length of args and locals should match. Update snapshots
           ;; and save args.  Snapshot data need to contain locals in
           ;; caller procedure when VM bytecode op made this side exit was
           ;; inlined.
           ;;
           (let lp ((local-x-types local-x-types)
                    (args args))
             (match local-x-types
               (((local . type) . local-x-types)
                (match args
                  ((arg . args)
                   (let ((var (env-ref arg)))
                     (store-frame moffs local type var)
                     (lp local-x-types args)))
                  (()
                   (debug 3 ";;;   args=null, local-x-types=~a~%" local-x-types)
                   (values nlocals local-offset snapshot))))
               (()
                (values nlocals local-offset snapshot)))))))
        (_
         (debug 3 ";;; store-snapshot: exit ~a, not a snapshot ~a~%"
                current-side-exit snapshot))))

    (with-jit-state
     (jit-prolog)
     (jit-tramp (imm (* 4 %word-size)))
     (let-values (((nlocals local-offset snapshot)
                   (store-snapshot (hashq-ref snapshots current-side-exit))))
       (when (< 0 local-offset)
         ;; Internal FP in VM_ENGINE will get updated with C macro `CACHE_FP'.
         ;; Adding offset for positive local offset, fp is already shifted in
         ;; store-snapshot for negative local offset.
         (debug 3 ";;; compile-bailout: shifting FP, local-offset=~a~%"
                local-offset)
         (let ((vp->fp r0))
           (jit-ldxi vp->fp fp vp->fp-offset)
           (jit-addi vp->fp vp->fp (imm (* local-offset %word-size)))
           (jit-stxi vp->fp-offset fp vp->fp)
           (vm-sync-fp vp->fp)))

       ;; Sync next IP with vp->ip for VM.
       (jit-movi r0 (imm next-ip))
       (vm-sync-ip r0)

       ;; Return tjit-retval to VM interpreter.
       (jit-prepare)
       (jit-pushargr reg-thread)
       (jit-pushargi (scm-i-makinumi current-side-exit))
       (jit-pushargi (scm-i-makinumi entry-ip))
       (jit-pushargi (scm-i-makinumi nlocals))
       (jit-calli %scm-make-tjit-retval)
       (jit-retval reg-retval)
       (return-to-interpreter)
       (jit-epilog)
       (jit-realize)
       (let* ((estimated-code-size (jit-code-size))
              (code (make-bytevector estimated-code-size))
              (exit-vars (map env-ref args)))
         (jit-set-code (bytevector->pointer code) (imm estimated-code-size))
         (let ((ptr (jit-emit)))
           (debug 3 ";;; compile-bailout: ptr=~a~%" ptr)
           (make-bytevector-executable! code)
           (dump-bailout next-ip current-side-exit code)
           (set-snapshot-variables! snapshot exit-vars)
           (set-snapshot-code! snapshot code)
           (trampoline-set! trampoline current-side-exit ptr)
           (set! current-side-exit (+ current-side-exit 1)))))))

  (define (compile-call proc args)
    (debug 3 ";;; compile-call: proc=~a args=~a current-side-exit=~a~%"
           proc args current-side-exit)
    (let* ((proc (env-ref proc))
           (ip (ref-value proc)))
      (cond
       ((not (constant? proc))
        (error "compile-call: not a constant" proc))
       ((= ip *ip-key-set-loop-info!*)
        (cond
         ((hashq-ref snapshots current-side-exit)
          => (match-lambda
              (($ $snapshot _ _ local-x-types)
               (set! loop-locals local-x-types)
               (set! loop-vars (map env-ref args)))))
         (else
          (debug 3 ";;; compile-call: no snapshot at ~a~%" current-side-exit))))
       ((= ip *ip-key-jump-to-linked-code*)
        (compile-link args))
       (else                            ; IP is bytecode destination.
        (compile-bailout ip args)))))

  (define (compile-primcall name dsts args)
    (cond
     ((hashq-ref *native-prim-procedures* name)
      => (lambda (proc)
           (let* ((out-code (trampoline-ref trampoline (- current-side-exit 1)))
                  (end-address (or (and fragment
                                        (fragment-end-address fragment))
                                   (and=> (get-fragment linked-ip)
                                          fragment-end-address)))
                  (asm (make-asm env fp-offset out-code end-address)))
             (apply proc asm (map env-ref (append dsts args))))))
     (else
      (error "Unhandled primcall" name))))

  (define (compile-exp exp dsts)
    (match exp
      (($ $primcall name args)
       (compile-primcall name dsts args))
      (($ $call proc args)
       (compile-call proc args))
      (_
       (values))))

  (define (compile-cont cps)
    (define (maybe-move exp old-args)
      (match exp
        (($ $values new-args)
         (when (= (length old-args) (length new-args))
           (for-each
            (lambda (old new)
              (when (not (eq? old new))
                (let ((dst (env-ref old))
                      (src (env-ref new)))
                  (when (not (and (eq? (ref-type dst) (ref-type src))
                                  (eq? (ref-value dst) (ref-value src))))
                    (debug 3 ";;; (%mov         ~a ~a)~%" dst src)
                    (move moffs dst src)))))
            old-args new-args)))
        (($ $primcall name args)
         (compile-primcall name loop-args args))
        (_
         (debug 3 "*** maybe-move: ~a ~a~%" exp old-args))))
    (define (go k term exp loop-label)
      (match term
        (($ $kfun _ _ _ _ _)
         (values #f loop-label))
        (($ $kclause _ _)
         (values #f loop-label))
        (($ $kreceive _ _)
         (compile-exp exp '())
         (values #f loop-label))
        (($ $kargs _ syms ($ $continue knext _ next-exp))
         (compile-exp exp syms)
         (cond
          ((< knext k)
           ;; Jump back to beginning of the loop, return to caller.
           (maybe-move next-exp loop-args)
           (debug 3 ";;; -> loop~%")
           (jump loop-label)
           (values next-exp loop-label))
          ((= kstart k)
           ;; Emit label to mark beginning of the loop.
           (debug 3 ";;; loop:~%")
           (jit-note "loop" 0)
           (values next-exp (jit-label)))
          (else
           (values next-exp loop-label))))
        (($ $ktail)
         (compile-exp exp '())
         (values #f loop-label))))

    (call-with-values
        (lambda () (intmap-fold go cps #f #f))
      (lambda (_ loop-label)
        (values trampoline loop-label loop-locals loop-vars fp-offset))))

  (compile-cont cps))

(define (compile-native cps entry-ip locals snapshots fragment exit-id linked-ip
                        lowest-offset)
  (let*-values
      (((max-label max-var) (compute-max-label-and-var cps))
       ((env initial-locals loop-args kstart nspills)
        (resolve-variables cps locals max-var))
       ((trampoline-size)
        (hash-fold (lambda (k v acc) (+ acc 1)) 1 snapshots))
       ((trampoline) (make-trampoline trampoline-size))
       ((fp-offset)
        ;; Root trace allocates spaces for spilled variables, three words to
        ;; store `vp', `vp->fp', and `registers', and one more word for return
        ;; address used by side exits. Side trace cannot allocate additional
        ;; memory, because side trace uses `jit-tramp'. Native code will not
        ;; work if number of spilled variables exceeds the number returned from
        ;; parameter `(tjit-max-spills)'.
        (if (not fragment)
            (let ((max-spills (tjit-max-spills)))
              (when (< max-spills nspills)
                ;; XXX: Escape from this procedure, increment compilation
                ;; failure for this entry-ip.
                (error "Too many spilled variables" nspills))
              (jit-allocai (imm (* (+ max-spills 4) %word-size))))
            (fragment-fp-offset fragment)))
       ((moffs)
        (lambda (mem)
          (let ((offset (* (ref-value mem) %word-size)))
            (make-signed-pointer (+ fp-offset offset))))))

    (cond
     ;; Root trace.
     ((not fragment)
      ;; Get arguments.
      (jit-getarg reg-thread (jit-arg)) ; thread
      (jit-getarg r0 (jit-arg))         ; vp
      (jit-getarg r1 (jit-arg))         ; registers, for prompt

      ;; Store `vp', `vp->fp', and `registers'.
      (jit-stxi vp-offset fp r0)
      (vm-cache-fp r0)
      (jit-stxi registers-offset fp r1)

      (jit-ldxi r0 fp vp->fp-offset)
      (scm-frame-return-address r1 r0))

     ;; Side trace.
     (else
      ;; Avoid emitting prologue.
      (jit-tramp (imm (* 4 %word-size)))

      ;; Load initial arguments from parent trace.
      (cond
       ((hashq-ref (fragment-snapshots fragment) exit-id)
        => (match-lambda
            (($ $snapshot _ _ local-x-types exit-variables)
             ;; Store values passed from parent trace when it's not used by this
             ;; side trace.
             (maybe-store moffs local-x-types exit-variables identity
                          initial-locals)

             ;; When passing values from parent trace to side-trace, src could
             ;; be overwritten by move or load.  Pairings of local and register
             ;; could be different from how it was done in parent trace and this
             ;; side trace, move or load without overwriting the sources.
             (let ((locals (snapshot-locals (hashq-ref snapshots 0)))
                   (dst-table (make-hash-table))
                   (src-table (make-hash-table))
                   (type-table (make-hash-table)))
               (debug 3 ";;; side-trace: initial-locals: ~a~%" initial-locals)
               (debug 3 ";;; side-trace: locals: ~a~%" locals)
               (for-each
                (match-lambda
                 ((local . var)
                  (hashq-set! type-table local (assq-ref locals local))
                  (hashq-set! dst-table local (vector-ref env var))))
                initial-locals)
               (let lp ((local-x-types local-x-types)
                        (vars exit-variables))
                 (match local-x-types
                   (((local . type) . local-x-types)
                    (match vars
                      ((var . vars)
                       (hashq-set! src-table local var)
                       (when (not (hashq-ref type-table local))
                         (debug 3 ";;; side-exit entry, setting type from parent,")
                         (debug 3 "local ~a to type ~a~%" local type)
                         (hashq-set! type-table local type))
                       (lp local-x-types vars))
                      (()
                       (debug 3 ";;; side-exit: vars=null, local-x-types=~a~%"
                              local-x-types))))
                   (() (values))))

               ;; Move or load locals in current frame.
               (move-or-load-carefully dst-table src-table type-table moffs)))))
       (else
        (error "compile-tjit: snapshot not found in parent trace" exit-id)))))

    ;; Assemble the primitives in CPS.
    (compile-cps cps env kstart entry-ip snapshots loop-args fp-offset
                 fragment trampoline linked-ip lowest-offset)))