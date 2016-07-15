;;;; IR to native code compiler for vm-tjit

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
;;; Compile IR to native code.
;;;
;;; Code:

(define-module (language trace compile-native)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (language trace assembler)
  #:use-module (language trace error)
  #:use-module (language trace compile-ir)
  #:use-module (language trace fragment)
  #:use-module (language trace gdb)
  #:use-module (language trace ir)
  #:use-module (language trace env)
  #:use-module (language trace parameters)
  #:use-module (language trace primitives)
  #:use-module (language trace ra)
  #:use-module (language trace registers)
  #:use-module (language trace snapshot)
  #:use-module (language trace trampoline)
  #:use-module (language trace types)
  #:use-module (language trace variables)
  #:export (compile-native))


;;;; Auxiliary

(define %scm-set-tjit-retval
  (dynamic-pointer "scm_set_tjit_retval" (dynamic-link)))

(define %scm-tjit-dump-retval
  (dynamic-pointer "scm_tjit_dump_retval" (dynamic-link)))

(define %scm-tjit-dump-locals
  (dynamic-pointer "scm_tjit_dump_locals" (dynamic-link)))

(define-syntax-rule (scm-i-makinumi n)
  (make-signed-pointer (+ (ash n 2) 2)))

(define-inlinable (shift-fp nlocals)
  "Shift FP, new value will be SP plus NLOCALS."
  (let ((vp r0)
        (vp->fp r1))
    (load-vp vp)
    (jit-addi vp->fp %sp (imm (* nlocals %word-size)))
    (store-vp->fp vp vp->fp)))

(define-inlinable (shift-sp env %asm offset)
  "Shift SP for OFFSET, may expand stack with %ASM when offset is negative."
  (cond
   ((= 0 offset)
    (vm-sync-sp %sp))
   ((< 0 offset)
    (jit-addi %sp %sp (imm (* offset %word-size)))
    (vm-sync-sp %sp))
   (else
    ;; Stack space might ran out in the middle of next run, expand stack with
    ;; twice of the spaces used for current iteration to ensure enough space
    ;; allocated. Then adjust with SP offset saved in snapshot.
    (let* ((next-offset (* (env-min-sp-offset env) 2))
           (diff (- offset next-offset)))
      (vm-expand-stack %asm next-offset)
      (if (< 0 diff)
          (jit-addi %sp %sp (imm (* diff %word-size)))
          (jit-subi %sp %sp (imm (* (- diff) %word-size))))
      (vm-sync-sp %sp)))))

(define-inlinable (load-stack local type dst)
  (sp-ref r0 local)
  (unbox-stack-element dst r0 type))

(define-inlinable (maybe-store %asm local-x-types srcs references shift)
  "Store src in SRCS to frame when local is not found in REFERENCES."
  (syntax-parameterize ((asm (identifier-syntax %asm)))
    (let lp ((local-x-types local-x-types) (srcs srcs))
      (match (list local-x-types srcs)
        ((((local . type) . local-x-types) (src . srcs))
         (when (or (dynamic-link? type)
                   (return-address? type)
                   (not references)
                   (let ((reg (hashq-ref references (- local shift))))
                     (or (not reg)
                         (not (equal? src reg)))))
           (store-stack (- local shift) type src))
         (lp local-x-types srcs))
        (_ (values))))))

(define-syntax-rule (move-or-load-carefully dsts srcs dst-types src-types)
  "Move SRCS to DSTS or load without overwriting.

Avoids overwriting source in hash-table SRCS while updating destinations in
hash-table DSTS.  If source is not found, load value from frame with using type
from hash-table TYPES to get memory offset.  Hash-table key of SRCS, DSTS,
DST-TYPES, and SRC-TYPES are local index number."
  (begin
    (define (dst-is-full? as bs)
      (let lp ((as as))
        (match as
          ((a . as) (and (member a bs) (lp as)))
          (() #t))))
    (define (in-srcs? var)
      (call/ec
       (lambda (escape)
         (hash-fold (lambda (k v acc)
                      (and (equal? v var) (escape (hashq-ref dsts k))))
                    #f
                    srcs))))
    (define (find-src-local var)
      (call/ec
       (lambda (escape)
         (hash-fold (lambda (k v ret)
                      (and (equal? v var) (escape k)))
                    #f
                    srcs))))
    (define (unbox dst src type local)
      (case (ref-type src)
        ((gpr)
         (guard-type (gpr src) type)
         (unbox-stack-element dst (gpr src) type))
        ((fpr)
         (fpr->gpr r0 (fpr src))
         (guard-type r0 type)
         (unbox-stack-element dst r0 type))
        ((mem)
         (memory-ref r0 src)
         (guard-type r0 type)
         (unbox-stack-element dst r0 type))
        (else
         (failure 'move-or-load-carefully "unbox ~a ~a ~a" dst src type))))
    (define (dump-move local dst/v dst/t src/v src/t)
      (debug 3 ";;; molc: [local ~a] (move ~a:~a ~a:~a)~%" local
             (physical-name dst/v) (pretty-type dst/t)
             (physical-name src/v) (pretty-type src/t)))
    (define (dump-load local dst type)
      (debug 3 ";;; molc: [local ~a] loading to ~a, type=~a~%" local
             (physical-name dst) (pretty-type type)))
    (define (car-< a b)
      (< (car a) (car b)))
    (define (dump-regs label regs)
      (debug 3 ";;; molc: ~s vars: ~a~%" label
             (sort (map (match-lambda ((k . v) (cons k (physical-name v))))
                        regs)
                   car-<)))
    (define (dump-types label tbl)
      (debug 3 ";;; molc: ~s types: ~a~%"
             label
             (sort (map (match-lambda ((n . t) (cons n (pretty-type t))))
                        (hash-map->list cons tbl))
                   car-<)))
    (define (move-typed d/v d/t s/v s/t)
      (cond
       ((constant? d/t)
        (values))
       ((constant? s/t)
        (move d/v (make-con (constant-value s/t))))
       (else
        (move d/v s/v))))
    ;; (dump-regs 'dsts (hash-map->list cons dsts))
    ;; (dump-regs 'srcs (hash-map->list cons srcs))
    ;; (dump-types 'dsts dst-types)
    ;; (dump-types 'srcs src-types)
    (let lp ((dsts (sort (hash-map->list cons dsts) car-<)))
      (match dsts
        (((local . dst-var) . rest)
         (let ((dst-type (hashq-ref dst-types local))
               (src-type (hashq-ref src-types local)))
           (cond
            ((in-srcs? dst-var)
             => (lambda (src-var)
                  (cond
                   ((equal? dst-var src-var)
                    (cond
                     ((and (eq? &scm src-type)
                           (eq? &flonum dst-type))
                      (unbox dst-var src-var dst-type local))
                     ((and (constant? src-type)
                           (not (constant? dst-type)))
                      ;; (dump-move local dst-var dst-type src-var src-type)
                      (move dst-var (make-con (constant-value src-type)))))
                    (hashq-remove! srcs local)
                    (lp rest))
                   ((dst-is-full? (map cdr dsts)
                                  (map cdr (hash-map->list cons srcs)))
                    ;; When all of the elements in dsts are in srcs, move one of
                    ;; the srcs to temporary location. `-2' is for gpr R1 or fpr
                    ;; F1 in lightning, used as scratch register in this module.
                    (let ((tmp (if (fpr? src-var)
                                   (make-fpr -2)
                                   (make-gpr -2)))
                          (src-local (find-src-local src-var)))
                      ;; (dump-move local tmp dst-type src-var src-type)
                      (move-typed tmp #f src-var dst-type)
                      (hashq-set! srcs src-local tmp)
                      (lp dsts)))
                   (else
                    ;; Rotate the list and try again.
                    (lp (append rest (list (cons local dst-var))))))))
            ((hashq-ref srcs local)
             => (lambda (src-var)
                  (if (equal? src-var dst-var)
                      (when (and (constant? src-type)
                                 (not (constant? dst-type)))
                        ;; (dump-move local dst-var dst-type src-var src-type)
                        (move dst-var (make-con (constant-value src-type))))
                      (if (and (eq? &scm src-type)
                               (eq? &flonum dst-type))
                          (unbox dst-var src-var dst-type local)
                          (begin
                            ;; (dump-move local dst-var dst-type src-var src-type)
                            (move-typed dst-var dst-type src-var src-type))))
                  (hashq-remove! srcs local)
                  (lp rest)))
            (else
             ;; (dump-load local dst-var (hashq-ref dst-types local))
             (let ((type (hashq-ref dst-types local)))
               (load-stack local type dst-var))
             (lp rest)))))
        (() (values))))))


;;;; Native Code Compiler

(define (compile-native env primops snapshots sline)
  (with-jit-state
   (jit-prolog)
   (let-values (((trampoline loop-label bailouts storage)
                 (compile-entry env primops snapshots)))
     (let* ((epilog-label (jit-label))
            (_ (begin
                 (jit-patch epilog-label)
                 (jit-retr %retval)
                 (jit-epilog)
                 (jit-realize)))
            (estimated-size (jit-code-size))
            (code (make-bytevector estimated-size))
            (_ (jit-set-code (bytevector->pointer code) (imm estimated-size)))
            (ptr (jit-emit))
            (size (jit-code-size))
            (exit-counts (make-vector (hash-count (const #t) snapshots) 0))
            (loop-address (and loop-label (jit-address loop-label)))
            (end-address (or (and=> (env-parent-fragment env)
                                    fragment-end-address)
                             (jit-address epilog-label)))
            (parent-id (and=> (env-parent-fragment env) fragment-id))
            (gdb-jit-entry
             (and (tjit-dump-dwarf? (tjit-dump-option))
                  (let* ((addr (pointer-address (bytevector->pointer code)))
                         (elf (make-gdb-jit-elf (env-id env) addr size
                                                (car sline)
                                                (or (cdr sline) 1))))
                    (tjit-register-gdb-jit-entry! elf))))
            (loop-vars
             (and (list? (env-loop-vars env))
                  (let lp ((vars (env-loop-vars env)) (acc '()))
                    (if (null? vars)
                        acc
                        (lp (cdr vars)
                            (cons (storage-ref storage (car vars)) acc))))))
            (checker-types
             (if (env-uprec? env)
                 (let ((nlocals (snapshot-nlocals
                                 (snapshots-ref snapshots 0))))
                   (filter (match-lambda ((n . _) (< n nlocals)))
                           (env-entry-types env)))
                 (env-entry-types env)))
            (type-checker (and (not parent-id)
                               (gen-type-checker checker-types (env-id env))))
            (fragment (make-fragment (env-id env)
                                     code
                                     exit-counts
                                     (env-downrec? env)
                                     (env-uprec? env)
                                     type-checker
                                     (env-entry-ip env)
                                     0
                                     parent-id
                                     (env-parent-exit-id env)
                                     loop-address
                                     (env-loop-locals env)
                                     loop-vars
                                     snapshots
                                     trampoline
                                     end-address
                                     gdb-jit-entry
                                     storage
                                     #f
                                     (env-handle-interrupts? env)
                                     '() '()))
            (origin (or (and=> (env-parent-fragment env) get-origin-fragment)
                        fragment))
            (bailout-code (compile-bailouts env end-address trampoline
                                            fragment origin bailouts)))
       (make-bytevector-executable! code)
       (set-fragment-bailout-code! fragment bailout-code)

       ;; Same entry-ip could be used when side exit 0 was taken for
       ;; multiple times. Using trace ID as hash table key.
       (debug 4 ";;; jit-print:~%~a~%" (jit-print))

       ;; When this trace is a side trace, replace the native code of trampoline
       ;; in parent fragment.
       (let ((parent-fragment (env-parent-fragment env))
             (code-address (pointer-address ptr)))
         (when parent-fragment
           (let ((trampoline (fragment-trampoline parent-fragment))
                 (side-trace-ids (fragment-side-trace-ids parent-fragment)))
             (trampoline-set! trampoline (env-parent-exit-id env) ptr)
             (set-snapshot-code! (env-parent-snapshot env) code)
             (let ((new-ids (cons (env-id env) side-trace-ids)))
               (set-fragment-side-trace-ids! parent-fragment new-ids))))
         (values fragment code size code-address loop-address trampoline))))))

(define (compile-entry env primops snapshots)
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log (env-id env))))
      (set-tjit-time-log-assemble! log (get-internal-run-time))))
  (let ((trampoline (make-trampoline (hash-count (const #t) snapshots))))
    (cond
     ((env-parent-fragment env)         ; Side trace.
      => (lambda (parent-fragment)
           ;; Check for spilled variables.
           (let ((nspills (primops-nspills primops)))
             (when (< (tjit-max-spills) nspills)
               (break 1 "too many spills ~s" nspills)))

           ;; Avoid emitting prologue.
           (jit-tramp (imm (* 4 %word-size)))

           ;; Store values passed from parent trace when it's not used by this
           ;; side trace.
           (match (env-parent-snapshot env)
             (($ $snapshot _ _ _ _ locals vars)
              (let* ((snap0 (snapshots-ref snapshots 0))
                     (locals0 (snapshot-locals snap0))
                     (vars0 (snapshot-variables snap0))
                     (references (make-hash-table))
                     (storage (fragment-storage parent-fragment))
                     (asm (make-asm storage #f #t #f #f)))
                (let lp ((locals0 locals0) (vars0 vars0))
                  (match (cons locals0 vars0)
                    ((((local . _) . locals0) . (var . vars0))
                     (hashq-set! references local var)
                     (lp locals0 vars0))
                    (_
                     (maybe-store asm locals vars references 0))))))
             (_
              (failure 'compile-entry "snapshot not found")))))
     (else                              ; Root trace.
      (let ((max-spills (tjit-max-spills))
            (nspills (primops-nspills primops))
            (vp r0)
            (registers r1))
        (when (< max-spills nspills)
          (break 1 "too many spills ~s" nspills))

        ;; Root trace allocates spaces for spilled variables. One word to store
        ;; `registers' from argument, and space to save volatile registers.
        ;;
        ;; Side trace cannot allocate additional memory, because side trace uses
        ;; `jit-tramp'. Native code will not work if number of spilled variables
        ;; exceeds the number returned from parameter `(tjit-max-spills)'.
        ;;
        (let ((nwords (+ max-spills *num-fpr* *num-volatiles* 1)))
          (jit-allocai (imm (* nwords %word-size))))

        ;; Get arguments.
        (jit-getarg %thread (jit-arg))   ; thread
        (jit-getarg vp (jit-arg))        ; vp
        (jit-getarg registers (jit-arg)) ; registers, for prompt

        ;; Store `vp', `vp->sp', and `registers'.
        (store-vp vp)
        (vm-cache-sp vp)
        (jit-stxi registers-offset %fp registers))))

    ;; Assemble the primitives.
    (compile-body env primops snapshots trampoline)))

(define (compile-body env primops snapshots trampoline)
  (define (compile-ops asm ops storage acc)
    (let lp ((ops ops) (acc acc))
      (match ops
        ((('%snap snapshot-id . args) . ops)
         (cond
          ((snapshots-ref snapshots snapshot-id)
           => (lambda (snapshot)
                (cond
                 ((snapshot-downrec? snapshot)
                  (compile-downrec env asm args snapshot storage)
                  (lp ops acc))
                 ((snapshot-uprec? snapshot)
                  (compile-uprec env asm args snapshot storage)
                  (lp ops acc))
                 ((snapshot-link? snapshot)
                  (compile-link env asm args snapshot storage)
                  (lp ops acc))
                 (else
                  (let ((out-code (trampoline-ref trampoline snapshot-id))
                        (bailout (compile-bailout env asm snapshot
                                                  trampoline args))
                        (exit (jit-forward)))
                    (jit-patch-abs exit out-code)
                    (set-asm-exit! asm exit)
                    (lp ops (cons bailout acc)))))))
          (else
           (failure 'compile-ops "no snapshot ~s" snapshot-id))))
        (((op-name . args) . ops)
         (cond
          ((prim-procedures-ref op-name)
           => (lambda (proc)
                (apply proc asm args)
                (lp ops acc)))
          (else
           (failure 'compile-ops "op not found ~s" op-name))))
        (() acc))))
  (define (compile-loop %asm loop storage gen-bailouts)
    (if (null? loop)
        (values #f gen-bailouts)
        (let ((loop-label (jit-label)))
          (jit-note "loop" 0)
          (jit-patch loop-label)
          (when (env-handle-interrupts? env)
            (syntax-parameterize ((asm (identifier-syntax %asm)))
              (vm-handle-interrupts)))
          (let ((gen-bailouts (compile-ops %asm loop storage gen-bailouts)))
            (jump loop-label)
            (values loop-label gen-bailouts)))))
  (match primops
    (($ $primops entry loop mem-idx storage)
     (let* ((linked-fragment (env-linked-fragment env))
            (end-address (and=> linked-fragment fragment-end-address))
            (save-volatiles? (env-save-volatiles? env))
            (asm (make-asm storage end-address #t save-volatiles? snapshots))
            (gen-bailouts (compile-ops asm entry storage '())))
       (let-values (((loop-label gen-bailouts)
                     (compile-loop asm loop storage gen-bailouts)))
         (values trampoline loop-label gen-bailouts storage))))
    (_
     (failure 'compile-body "not a $primops" primops))))

(define (compile-bailout env %asm snapshot trampoline args)
  (syntax-parameterize ((asm (identifier-syntax %asm)))
    (lambda (end-address self-fragment origin)
      (let ((entry (jit-label)))
        (jit-patch entry)
        (match snapshot
          (($ $snapshot id sp-offset fp-offset nlocals locals _ _ ip)
           (define (store-locals shift)
             (let lp ((locals locals) (args args))
               (match (cons locals args)
                 ((((local . type) . locals) . (arg . args))
                  (store-stack (- local shift) type arg)
                  (lp locals args))
                 (_ (values)))))

           ;; Choose order to shift SP and store frame by sign of SP.
           (unless (snapshot-longjmp? snapshot)
             (cond
              ((= sp-offset 0)
               (store-locals 0)
               (shift-fp nlocals))
              ((< sp-offset 0)
               (shift-sp env %asm sp-offset)
               (store-locals sp-offset)
               (shift-fp nlocals))
              (else
               (store-locals 0)
               (shift-sp env %asm sp-offset)
               (shift-fp nlocals)))

             ;; Sync next IP with vp->ip for VM.
             (jit-movi r0 (imm ip))
             (vm-sync-ip r0)

             ;; Set tjit return values for VM interpreter.
             (jit-prepare)
             (jit-pushargi (imm id))
             (jit-pushargi (scm->pointer self-fragment))
             (jit-pushargi (scm->pointer origin))
             (jit-calli %scm-set-tjit-retval))

           ;; Debug code to dump tjit-retval and locals.
           (let ((dump-option (tjit-dump-option)))
             (when (tjit-dump-exit? dump-option)
               (jit-prepare)
               (load-vp r0)
               (jit-pushargr r0)
               (jit-calli %scm-tjit-dump-retval)
               (when (tjit-dump-verbose? dump-option)
                 (jit-prepare)
                 (jit-pushargi (scm-i-makinumi (env-id env)))
                 (jit-pushargi (imm nlocals))
                 (load-vp r0)
                 (jit-pushargr r0)
                 (jit-calli %scm-tjit-dump-locals))))

           (if (snapshot-longjmp? snapshot)
               (jit-movi %retval (imm 0))
               (jit-movi %retval (imm 1)))
           (jmpa end-address)

           (cons id entry))
          (_
           (debug 2 "*** compile-bailout: not a snapshot ~a~%" snapshot)))))))

(define (compile-bailouts env end-address trampoline self-fragment origin
                          bailouts)
  ;; Emit bailouts with end address of this code. Side traces need to
  ;; jump to the address of epilogue of parent root trace, to manage
  ;; non-volatile registers.
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log (env-id env))))
      (set-tjit-time-log-bailout! log (get-internal-run-time))))
  (with-jit-state
   (jit-prolog)
   (jit-tramp (imm (* 4 %word-size)))
   (let ((entries (let lp ((bailouts bailouts) (acc '()))
                    (match bailouts
                      ((proc . bailouts)
                       (let ((entry (proc end-address self-fragment origin)))
                         (lp bailouts (cons entry acc))))
                      (() acc)))))
     (jit-epilog)
     (jit-realize)
     (let* ((estimated-size (jit-code-size))
            (code (make-bytevector estimated-size))
            (_ (jit-set-code (bytevector->pointer code)
                             (imm estimated-size)))
            (ptr (jit-emit)))
       (make-bytevector-executable! code)
       (do ((entries entries (cdr entries)))
           ((null? entries))
         (let* ((entry (car entries))
                (id (car entry))
                (node (cdr entry)))
           (trampoline-set! trampoline id (jit-address node))))
       code))))

(define (compile-link env %asm args snapshot storage)
  (define (make-src-var-table storage indices shift)
    (let lp ((indices indices) (ret (make-hash-table)))
      (match indices
        ((n . indices)
         (let ((var (make-var n)))
           (and=> (storage-ref storage var)
                  (lambda (phy)
                    (hashq-set! ret (- n shift) phy)))
           (lp indices ret)))
        (() ret))))
  (define (make-src-type-table local-x-types sp-offset)
    (let lp ((local-x-types local-x-types) (tbl (make-hash-table)))
      (match local-x-types
        (((n . t) . local-x-types)
         (hashq-set! tbl (- n sp-offset) t)
         (lp local-x-types tbl))
        (() tbl))))
  (match snapshot
    (($ $snapshot id sp-offset fp-offset nlocals locals vars)
     ;; Store unpassed variables, and move variables to linked trace.  Shift
     ;; amount in `maybe-store' depends on whether the trace is root trace or
     ;; not.
     (let* ((linked (env-linked-fragment env))
            (loop-locals (and=> linked fragment-loop-locals))
            (dst-type-table (make-hash-table))
            (dst-var-table (make-hash-table))
            (lives (env-live-indices env))
            (src-var-table (make-src-var-table storage lives sp-offset))
            (src-type-table (make-src-type-table locals sp-offset))
            (ref-table (make-hash-table))
            (linked-snapshots (fragment-snapshots linked))
            (linked-snap1 (snapshots-ref linked-snapshots 1))
            (my-depth (snapshot-inline-depth snapshot)))

       (let lp ((loop-locals loop-locals)
                (vars (fragment-loop-vars linked)))
         (match (cons loop-locals vars)
           ((((n . type) . loop-locals) . (var . vars))
            (hashq-set! dst-type-table n type)
            (hashq-set! dst-var-table n var)
            (lp loop-locals vars))
           (_
            ;; Store locals not passed to linked trace. Using snapshot 1 from
            ;; linked fragment to get the write indices used in reference.
            (let lp ((ref-locals (snapshot-locals linked-snap1)))
              (match ref-locals
                (((n . t) . ref-locals)
                 (hashq-set! ref-table n t)
                 (lp ref-locals))
                (()
                 ;; Shift SP before storing locals when SP offset was negative,
                 ;; then shift FP with nlocals. This order is taken to preserve
                 ;; stored locals from garbage collection.
                 (cond
                  ((= 0 sp-offset)
                   (maybe-store %asm locals args ref-table 0)
                   (shift-fp nlocals))
                  ((< sp-offset 0)
                   (shift-sp env %asm sp-offset)
                   (maybe-store %asm locals args ref-table sp-offset)
                   (shift-fp nlocals))
                  (else
                   (maybe-store %asm locals args ref-table 0)
                   (shift-sp env %asm sp-offset)
                   (shift-fp nlocals)))

                 ;; Move or load locals for linked fragment.
                 (syntax-parameterize ((asm (identifier-syntax %asm)))
                   (move-or-load-carefully dst-var-table src-var-table
                                           dst-type-table src-type-table)

                   ;; Handle interrupts if linked fragment didn't.
                   (when (and (env-handle-interrupts? env)
                              (not (fragment-handle-interrupts? linked)))
                     (vm-handle-interrupts)))

                 ;; Jump to beginning of the loop in linked fragment.
                 (jmpa (fragment-loop-address linked))))))))))
    (_
     (failure 'compile-link "not a snapshot ~s" snapshot))))

(define (compile-downrec env %asm args snapshot storage)
  (let ((src-var-table (make-hash-table)))
    (syntax-parameterize ((asm (identifier-syntax %asm)))
      (match snapshot
        (($ $snapshot id sp-offset fp-offset nlocals locals vars)
         (shift-sp env asm sp-offset)
         (let lp ((locals locals) (vars vars))
           (match (cons locals vars)
             ((((n . t) . locals) . (v . vars))
              (when (<= sp-offset n)
                (hashq-set! src-var-table n v)
                (store-stack (- n sp-offset) t v))
              (lp locals vars))
             ((())
              (let lp ((loop-locals (env-loop-locals env))
                       (loop-vars (reverse (env-loop-vars env))))
                (match (cons loop-locals loop-vars)
                  ((((n . t) . loop-locals) . (v . loop-vars))
                   (and=> (hashq-ref src-var-table (+ n sp-offset))
                          (lambda (src)
                            (let ((dst (storage-ref storage v)))
                              (move dst src))))
                   (lp loop-locals loop-vars))
                  (_
                   (shift-fp nlocals))))))))
        (_
         (failure 'compile-downrec "not a snapshot ~a" snapshot))))))

;; XXX: Incomplete
(define (compile-uprec env %asm args snapshot storage)
  ;; XXX: Refill old dynamic link and return address with false.
  (debug 2 ";;; [compile-uprec] args=~a~%" args)
  (debug 2 ";;; [compile-uprec] env-loop-vars=~a~%" (env-loop-vars env))
  (debug 2 ";;; [compile-uprec] env-loop-locals=~a~%" (env-loop-locals env))
  (match snapshot
    (($ $snapshot id sp-offset fp-offset nlocals locals vars)
     (debug 2 ";;; [compile-uprec] sp-offset=~a~%" sp-offset)
     (debug 2 ";;; [compile-uprec] nlocals=~a~%" nlocals)
     (debug 2 ";;; [compile-uprec] locals=~a~%" locals)
     (let ((src-var-table (make-hash-table))
           (src-type-table (make-hash-table))
           (dst-var-table (make-hash-table))
           (dst-type-table (make-hash-table)))
       (syntax-parameterize ((asm (identifier-syntax %asm)))
         (let lp ((loop-locals (env-loop-locals env))
                  (loop-vars (reverse (env-loop-vars env))))
           (match (cons loop-locals loop-vars)
             ((((n . type) . loop-locals) . (var . loop-vars))
              (hashq-set! dst-type-table n type)
              (hashq-set! dst-var-table n (storage-ref storage var))
              (lp loop-locals loop-vars))
             (_
              (shift-sp env %asm sp-offset)
              (let lp ((locals locals) (vars vars))
                (match (cons locals vars)
                  ((((n . t) . locals) . (v . vars))
                   (let ((i (- n sp-offset)))
                     (when (and v (<= 0 i))
                       (store-stack i t v)
                       (hashq-set! src-type-table i t)
                       (hashq-set! src-var-table i v))
                     (lp locals vars)))
                  (_
                   ;; (shift-fp nlocals)
                   (move-or-load-carefully dst-var-table src-var-table
                                           dst-type-table
                                           src-type-table))))))))))
    (_
     (failure 'compile-uprec "not a snapshot"))))
