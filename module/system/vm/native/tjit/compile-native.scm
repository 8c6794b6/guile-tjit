;;;; CPS to native code compiler for vm-tjit

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
;;; Compile CPS to native code.
;;;
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
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit compile-ir)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
  #:export (compile-native))


;;;
;;; Scheme constants and syntax
;;;

(define %scm-tjit-make-retval
  (dynamic-pointer "scm_tjit_make_retval" (dynamic-link)))

(define %scm-tjit-dump-retval
  (dynamic-pointer "scm_tjit_dump_retval" (dynamic-link)))

(define %scm-tjit-dump-locals
  (dynamic-pointer "scm_tjit_dump_locals" (dynamic-link)))

(define-syntax-rule (scm-i-makinumi n)
  (make-signed-pointer (+ (ash n 2) 2)))

;;;
;;; Code generation
;;;

(define (load-frame moffs local type dst)
  (cond
   ((eq? type &exact-integer)
    (cond
     ((gpr? dst)
      (sp-ref (gpr dst) local)
      (jit-rshi (gpr dst) (gpr dst) (imm 2)))
     ((memory? dst)
      (sp-ref r0 local)
      (jit-rshi r0 r0 (imm 2))
      (jit-stxi (moffs dst) fp r0))))
   ((eq? type &flonum)
    (cond
     ((fpr? dst)
      (sp-ref r0 local)
      (scm-real-value (fpr dst) r0))
     ((memory? dst)
      (sp-ref r0 local)
      (scm-real-value f0 r0)
      (jit-stxi-d (moffs dst) fp f0))))
   ((memq type (list &box &procedure &pair))
    (cond
     ((gpr? dst)
      (sp-ref (gpr dst) local))
     ((memory? dst)
      (sp-ref r0 local)
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
   ((eq? type &null)
    (cond
     ((gpr? dst)
      (jit-movi (gpr dst) *scm-null*))
     ((memory? dst)
      (jit-movi r0 *scm-null*)
      (jit-stxi-d (moffs dst) fp r0))))
   ((or (return-address? type)
        (dynamic-link? type))
    (values))
   (else
    (error "load-frame" local type dst))))

(define (store-frame moffs local type src)
  (debug 3 ";;; store-frame: local:~a type:~a src:~a~%"
         local (pretty-type type) src)
  (cond
   ((return-address? type)
    ;; Moving value coupled with type to frame local. Return address of VM frame
    ;; need to be recovered when taking exit from inlined procedure call. The
    ;; actual value for return address is captured at the time of Scheme IR
    ;; conversion and stored in snapshot as pointer.
    (jit-movi r0 (return-address-ip type))
    (sp-set! local r0))

   ((dynamic-link? type)
    ;; Storing fp to local. Dynamic link is stored as offset in type. VM's fp
    ;; could move, may use different value at the time of compilation and
    ;; execution.
    (jit-movi r0 (imm (dynamic-link-offset type)))
    (sp-set! local r0))

   ;; Check type, recover SCM value.
   ((eq? type &exact-integer)
    (cond
     ((constant? src)
      (jit-movi r0 (constant src))
      (jit-lshi r0 r0 (imm 2))
      (jit-addi r0 r0 (imm 2))
      (sp-set! local r0))
     ((gpr? src)
      (jit-lshi r0 (gpr src) (imm 2))
      (jit-addi r0 r0 (imm 2))
      (sp-set! local r0))
     ((memory? src)
      (jit-ldxi r0 fp (moffs src))
      (jit-lshi r0 r0 (imm 2))
      (jit-addi r0 r0 (imm 2))
      (sp-set! local r0))))
   ((eq? type &flonum)
    (cond
     ((constant? src)
      (jit-movi-d f0 (constant src))
      (scm-from-double r0 f0)
      (sp-set! local r0))
     ((fpr? src)
      (scm-from-double r0 (fpr src))
      (sp-set! local r0))
     ((memory? src)
      (jit-ldxi-d f0 fp (moffs src))
      (scm-from-double r0 f0)
      (sp-set! local r0))))
   ((memq type (list &box &procedure &pair))
    (cond
     ((constant? src)
      (jit-movi r0 (constant src))
      (sp-set! local r0))
     ((gpr? src)
      (sp-set! local (gpr src)))
     ((memory? src)
      (jit-ldxi r0 fp (moffs src))
      (sp-set! local r0))))
   ((eq? type &false)
    (jit-movi r0 *scm-false*)
    (sp-set! local r0))
   ((eq? type &true)
    (jit-movi r0 *scm-true*)
    (sp-set! local r0))
   ((eq? type &unbound)
    (jit-movi r0 *scm-undefined*)
    (sp-set! local r0))
   ((eq? type &unspecified)
    (jit-movi r0 *scm-unspecified*)
    (sp-set! local r0))
   ((eq? type &null)
    (jit-movi r0 *scm-null*)
    (sp-set! local r0))
   (else
    (error "store-frame" local type src))))

(define (maybe-store moffs local-x-types srcs src-unwrapper references shift)
  "Store src in SRCS to frame when local is not found in REFERENCES.

Locals are loaded with MOFFS to refer memory offset. Returns a hash-table
containing src with SRC-UNWRAPPER applied. Key of the returned hash-table is
local index in LOCAL-X-TYPES. Locals in returned hash-table is shifted for
SHIFT."
  (debug 3 ";;; maybe-store:~%")
  (debug 3 ";;;   srcs:          ~a~%" srcs)
  (debug 3 ";;;   local-x-types: ~a~%" local-x-types)
  (debug 3 ";;;   references:    ~a~%" references)
  (let lp ((local-x-types local-x-types)
           (srcs srcs)
           (acc (make-hash-table)))
    (match (list local-x-types srcs)
      ((((local . type) . local-x-types) (src . srcs))
       (let ((unwrapped-src (src-unwrapper src)))
         ;; XXX: Might need to add more conditions for storing dynamic link and
         ;; return addresses, not sure always these should be stored.
         (when (or (dynamic-link? type)
                   (return-address? type)
                   (not (assq local references)))
           (store-frame moffs local type unwrapped-src))
         (hashq-set! acc (- local shift) unwrapped-src)
         (lp local-x-types srcs acc)))
      (_
       acc))))

(define (shift-fp fp-offset)
  (let ((op (if (< 0 fp-offset)
                jit-addi
                jit-subi))
        (vp r0)
        (vp->fp r1))
    (jit-ldxi vp fp vp-offset)
    (jit-ldxi vp->fp vp (imm (* 2 %word-size)))
    (op vp->fp vp->fp (imm (* (abs fp-offset) %word-size)))
    (jit-stxi (imm (* 2 %word-size)) vp vp->fp)))

(define (shift-sp sp-offset)
  (let ((vp->sp r0)
        (op (if (< 0 sp-offset)
                jit-addi
                jit-subi)))
    (jit-ldxi vp->sp fp vp->sp-offset)
    (op vp->sp vp->sp (imm (* (abs sp-offset) %word-size)))
    (jit-stxi vp->sp-offset fp vp->sp)
    (vm-sync-sp vp->sp)))

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
;;; The Native Code Compiler
;;;

(define (compile-native primlist entry-ip locals snapshots fragment
                        parent-exit-id linked-ip trace-id)
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log trace-id)))
      (set-tjit-time-log-assemble! log (get-internal-run-time))))
  (let* ((trampoline (make-trampoline (hash-fold (lambda (k v acc) (+ acc 1))
                                                 1 snapshots)))
         (fp-offset
          ;; Root trace allocates spaces for spilled variables, three words to
          ;; store `vp', `vp->sp', and `registers', and one more word for return
          ;; address used by side exits. Side trace cannot allocate additional
          ;; memory, because side trace uses `jit-tramp'. Native code will not
          ;; work if number of spilled variables exceeds the number returned
          ;; from parameter `(tjit-max-spills)'.
          (if (not fragment)
              (let ((max-spills (tjit-max-spills))
                    (nspills (primlist-nspills primlist)))
                (when (< max-spills nspills)
                  ;; XXX: Escape from this procedure, increment compilation
                  ;; failure for this entry-ip.
                  (error "Too many spilled variables" nspills))
                (jit-allocai (imm (* (+ max-spills 4) %word-size))))
              (fragment-fp-offset fragment)))
         (moffs (lambda (mem)
                  (let ((offset (* (ref-value mem) %word-size)))
                    (make-signed-pointer (+ fp-offset offset))))))
    (cond
     ;; Root trace.
     ((not fragment)
      (let ((vp r0)
            (registers r1))

        ;; Get arguments.
        (jit-getarg reg-thread (jit-arg)) ; thread
        (jit-getarg vp (jit-arg))         ; vp
        (jit-getarg registers (jit-arg))  ; registers, for prompt

        ;; Store `vp', `vp->sp', and `registers'.
        (jit-stxi vp-offset fp vp)
        (vm-cache-sp vp)
        (jit-stxi registers-offset fp registers)))

     ;; Side trace.
     (else
      ;; Avoid emitting prologue.
      (jit-tramp (imm (* 4 %word-size)))

      ;; Store values passed from parent trace when it's not used by this
      ;; side trace.
      (cond
       ((hashq-ref (fragment-snapshots fragment) parent-exit-id)
        => (match-lambda
            (($ $snapshot _ sp-offset fp-offset _ local-x-types exit-variables)
             (let ((locals (snapshot-locals (hashq-ref snapshots 0))))
               (debug 3 ";;; side-trace: locals: ~a~%" locals)
               (maybe-store moffs local-x-types exit-variables identity
                            locals 0)))))
       (else
        (error "compile-native: snapshot not found in parent trace"
               parent-exit-id)))))

    ;; Assemble the primitives.
    (compile-primlist primlist #f entry-ip snapshots fp-offset fragment
                      trampoline linked-ip trace-id)))

(define (compile-primlist primlist env entry-ip snapshots fp-offset fragment
                          trampoline linked-ip trace-id)
  (define (compile-ops asm ops)
    (let lp ((ops ops) (loop-locals #f) (loop-vars #f))
      (match ops
        ((('%snap snapshot-id . args) . ops)
         (cond
          ((hashq-ref snapshots snapshot-id)
           => (lambda (snapshot)
                (cond
                 ((snapshot-set-loop-info? snapshot)
                  (match snapshot
                    (($ $snapshot _ _ _ _ local-x-types)
                     (lp ops local-x-types args))
                    (else
                     (error "snapshot loop info not found"))))
                 ((snapshot-jump-to-linked-code? snapshot)
                  (compile-link args snapshot asm linked-ip fragment)
                  (lp ops loop-locals loop-vars))
                 (else
                  (let ((ptr (compile-bailout asm trace-id snapshot args))
                        (out-code (trampoline-ref trampoline snapshot-id)))
                    (trampoline-set! trampoline snapshot-id ptr)
                    (set-asm-out-code! asm out-code)
                    (lp ops loop-locals loop-vars))))))
          (else
           (error "compile-ops: no snapshot with id" snapshot-id))))
        (((op-name . args) . ops)
         (cond
          ((hashq-ref *native-prim-procedures* op-name)
           => (lambda (proc)
                (let ((verbosity (lightning-verbosity)))
                  (when (<= 4 verbosity)
                    (jit-note (format #f "~a" (cons op-name args)) 0)))
                (apply proc asm args)
                (lp ops loop-locals loop-vars)))
          (else
           (error "op not found" op-name))))
        (()
         (values loop-locals loop-vars)))))
  (match primlist
    (($ $primlist entry loop mem-idx)
     (let*-values (((end-address) (or (and=> fragment
                                             fragment-end-address)
                                      (and=> (get-root-trace linked-ip)
                                             fragment-end-address)))
                   ((asm) (make-asm env fp-offset #f end-address))
                   ((loop-locals loop-vars) (compile-ops asm entry))
                   ((loop-label) (if (null? loop)
                                     #f
                                     (let ((loop-label (jit-label)))
                                       (jit-note "loop" 0)
                                       (compile-ops asm loop)
                                       (jump loop-label)
                                       loop-label))))
       (values trampoline loop-label loop-locals loop-vars fp-offset)))
    (_
     (error "compile-primlist: not a $primlist" primlist))))

(define (compile-bailout asm trace-id snapshot args)
  (let ((ip (snapshot-ip snapshot))
        (id (snapshot-id snapshot)))
    (debug 3 ";;; compile-bailout:~%")
    (debug 3 ";;;   snapshot-id: ~a~%" id)
    (debug 3 ";;;   next-ip:     ~a~%" ip)
    (debug 3 ";;;   args:        ~a~%" args)
    (with-jit-state
     (jit-prolog)
     (jit-tramp (imm (* 4 %word-size)))
     (match snapshot
       (($ $snapshot _ sp-offset fp-offset nlocals local-x-types)

        ;; Store contents of args to frame. No need to recover the frame with
        ;; snapshot when local-x-types were null.  Still snapshot data is used,
        ;; so that the bytevector of compiled native code could be stored in
        ;; fragment, to avoid garbage collection.
        (when (not (null? local-x-types))
          (let lp ((local-x-types local-x-types)
                   (args args)
                   (moffs (lambda (x)
                            (make-signed-pointer
                             (+ (asm-fp-offset asm)
                                (* (ref-value x) %word-size))))))
            (match (list local-x-types args)
              ((((local . type) . local-x-types) (arg . args))
               (store-frame moffs local type arg)
               (lp local-x-types args moffs))
              (_
               (values)))))

        ;; Shift SP.
        (when (not (= sp-offset 0))
          (shift-sp sp-offset))

        ;; Shift FP for inlined procedure.
        (when (< fp-offset 0)
          (shift-fp fp-offset))

        ;; Sync next IP with vp->ip for VM.
        (jit-movi r0 (imm ip))
        (vm-sync-ip r0)

        ;; Make tjit-retval for VM interpreter.
        (jit-prepare)
        (jit-pushargr reg-thread)
        (jit-pushargi (scm-i-makinumi id))
        (jit-pushargi (scm-i-makinumi trace-id))
        (jit-pushargi (scm-i-makinumi nlocals))
        (jit-calli %scm-tjit-make-retval)
        (jit-retval reg-retval)

        ;; Debug code to dump tjit-retval and locals.
        (let ((dump-option (tjit-dump-option)))
          (when (tjit-dump-exit? dump-option)
            (jit-movr reg-thread reg-retval)
            (jit-prepare)
            (jit-pushargr reg-retval)
            (jit-ldxi reg-retval fp vp-offset)
            (jit-pushargr reg-retval)
            (jit-calli %scm-tjit-dump-retval)
            (jit-movr reg-retval reg-thread)
            (when (tjit-dump-locals? dump-option)
              (jit-movr reg-thread reg-retval)
              (jit-prepare)
              (jit-pushargi (scm-i-makinumi trace-id))
              (jit-pushargi (imm nlocals))
              (jit-ldxi reg-retval fp vp->sp-offset)
              (jit-pushargr reg-retval)
              (jit-calli %scm-tjit-dump-locals)
              (jit-movr reg-retval reg-thread)))))
       (_
        (debug 1 "*** compile-bailout: not a snapshot ~a~%" snapshot)))

     (return-to-interpreter)
     (jit-epilog)
     (jit-realize)
     (let* ((estimated-code-size (jit-code-size))
            (code (make-bytevector estimated-code-size)))
       (jit-set-code (bytevector->pointer code) (imm estimated-code-size))
       (let ((ptr (jit-emit)))
         (debug 3 ";;; compile-bailout: ptr=~a~%" ptr)
         (make-bytevector-executable! code)
         (dump-bailout ip id code)
         (set-snapshot-variables! snapshot args)
         (set-snapshot-code! snapshot code)
         ptr)))))

(define (compile-link args snapshot asm linked-ip fragment)
  (let* ((linked-fragment (get-root-trace linked-ip))
         (loop-locals (fragment-loop-locals linked-fragment)))
    (define (moffs mem)
      (let ((offset (* (ref-value mem) %word-size)))
        (make-signed-pointer (+ (asm-fp-offset asm) offset))))
    (match snapshot
      (($ $snapshot sid sp-offset fp-offset _ local-x-types)
       ;; Store unpassed variables, and move variables to linked trace.
       ;; Shift amount in `maybe-store' depending on whether the trace is
       ;; root trace or not.
       (let* ((src-shift-amount sp-offset)
              (src-table (maybe-store moffs local-x-types args identity
                                      loop-locals sp-offset))
              (type-table (make-hash-table))
              (dst-table (make-hash-table)))
         (let lp ((locals loop-locals)
                  (dsts (fragment-loop-vars linked-fragment)))
           (match (list locals dsts)
             ((((local . type) . locals) (dst . dsts))
              (hashq-set! type-table local type)
              (hashq-set! dst-table local dst)
              (lp locals dsts))
             (_
              (values))))
         (move-or-load-carefully dst-table src-table type-table moffs))

       ;; Shift SP.
       (when (not (= sp-offset 0))
         (shift-sp sp-offset))

       ;; Shift FP for loop-less root trace.
       (when (not fragment)
         (shift-fp fp-offset))

       ;; Jump to the beginning of linked trace.
       (jumpi (fragment-loop-address linked-fragment)))
      (_
       (debug 3 ";;; compile-link: IP is 0, snapshot not found~%")))))
