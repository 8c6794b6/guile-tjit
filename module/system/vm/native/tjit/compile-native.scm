;;;; IR to native code compiler for vm-tjit

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
;;; Compile IR to native code.
;;;
;;; Code:

(define-module (system vm native tjit compile-native)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit compile-ir)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit gdb)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit trampoline)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables)
  #:export (compile-native))


;;;
;;; Scheme constants and syntax
;;;

(define %scm-make-tjit-retval
  (dynamic-pointer "scm_make_tjit_retval" (dynamic-link)))

(define %scm-tjit-dump-retval
  (dynamic-pointer "scm_tjit_dump_retval" (dynamic-link)))

(define %scm-tjit-dump-locals
  (dynamic-pointer "scm_tjit_dump_locals" (dynamic-link)))

(define-syntax-rule (scm-i-makinumi n)
  (make-signed-pointer (+ (ash n 2) 2)))


;;;
;;; Auxiliary
;;;

(define (load-frame local type dst)
  (debug 3 ";;; load-frame: local:~a type:~a dst:~a~%"
         local (pretty-type type) (physical-name dst))
  (sp-ref r0 local)
  (unbox-stack-element dst r0 type))

;; Uses `guard-type', which requires syntax-parameter `asm', hence this is
;; defined as macro.
(define-syntax-rule (store-frame local type src)
  (let ((err (lambda ()
               (failure 'store-frame "~s ~a ~s"
                            local (pretty-type type) src))))
    (debug 3 ";;; store-frame: local:~a type:~a src:~a~%"
           local (pretty-type type) (physical-name src))
    (cond
     ((return-address? type)
      ;; Moving value coupled with type to frame local. Return address of VM
      ;; frame need to be recovered when taking exit from inlined procedure
      ;; call. The actual value for return address is captured at the time of
      ;; Scheme IR conversion and stored in snapshot.
      (jit-movi r0 (return-address-ip type))
      (sp-set! local r0))

     ((dynamic-link? type)
      ;; Storing dynamid link to local. Dynamic link record type contains offset
      ;; in the field. VM may use different value at the time of compilation and
      ;; execution.
      (jit-movi r0 (imm (dynamic-link-offset type)))
      (sp-set! local r0))

     ;; Floating point values
     ((eq? type &flonum)
      (case (ref-type src)
       ((const)
        (jit-movi-d f0 (constant src))
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
     ((eq? type &f64)
      (case (ref-type src)
       ((const)
        (jit-movi-d f0 (constant src))
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

     ;; XXX: `fixnum' need boxing when the value was greater than
     ;; `most-positive-fixnum' or less than `most-negative-fixnum'.

     ;; Cell values
     (else
      (case (ref-type src)
       ((const)
        (jit-movi r0 (constant src))
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

(define (maybe-store %asm local-x-types srcs references shift)
  "Store src in SRCS to frame when local is not found in REFERENCES."
  (debug 3 ";;; maybe-store:~%")
  (debug 3 ";;;   srcs:          ~a~%" srcs)
  (debug 3 ";;;   local-x-types: ~a~%"
         (map (match-lambda ((n . t) `(,n . ,(pretty-type t))))
              local-x-types))
  (debug 3 ";;;   references:    ~a~%" (hash-map->list cons references))
  (let lp ((local-x-types local-x-types) (srcs srcs))
    (match (list local-x-types srcs)
      ((((local . type) . local-x-types) (src . srcs))
       (when (or (dynamic-link? type)
                 (return-address? type)
                 (not references)
                 (let ((reg (hashq-ref references (- local shift))))
                   (or (not reg)
                       (not (equal? src reg)))))
         (syntax-parameterize ((asm (identifier-syntax %asm)))
           (store-frame local type src)))
       (lp local-x-types srcs))
      (_ (values)))))

(define (shift-fp nlocals)
  "Shift FP, new value will be SP plus NLOCALS."
  (let ((vp r0)
        (vp->fp r1))
    (load-vp vp)
    (jit-addi vp->fp %sp (imm (* nlocals %word-size)))
    (store-vp->fp vp vp->fp)))

(define (shift-sp %asm offset)
  "Shift SP for OFFSET, may expand stack with %ASM when offset is negative."
  (cond
   ((= 0 offset)
    (vm-sync-sp %sp))
   ((< 0 offset)
    (jit-addi %sp %sp (imm (* offset %word-size)))
    (vm-sync-sp %sp))
   (else
    (vm-expand-stack %asm offset))))

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
      (hash-fold (lambda (k v acc)
                   (or acc (and (equal? v var) (hashq-ref dsts k))))
                 #f
                 srcs))
    (define (find-src-local var)
      (hash-fold (lambda (k v ret)
                   (or ret (and (equal? v var) k)))
                 #f
                 srcs))
    (define (unbox dst src type local)
      (debug 3 ";;; molc [local ~a] unbox ~a ~a ~a~%"
             local (physical-name dst) (physical-name src) (pretty-type type))
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
    (define (dump-move local dst src)
      (debug 3 ";;; molc: [local ~a] (move ~a ~a)~%" local
             (physical-name dst) (physical-name src)))
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
    (dump-regs 'dsts (hash-map->list cons dsts))
    (dump-regs 'srcs (hash-map->list cons srcs))
    (dump-types 'dsts dst-types)
    (dump-types 'srcs src-types)
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
                    (when (and (eq? &scm src-type)
                               (eq? &flonum dst-type))
                      (unbox dst-var src-var dst-type local))
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
                      (dump-move local tmp src-var)
                      (move tmp src-var)
                      (hashq-set! srcs src-local tmp)
                      (lp dsts)))
                   (else
                    ;; Rotate the list and try again.
                    (lp (append rest (list (cons local dst-var))))))))
            ((hashq-ref srcs local)
             => (lambda (src-var)
                  (debug 3 ";;; molc: [local ~a] ~a:~a => ~a:~a~%" local
                         (physical-name src-var) (pretty-type src-type)
                         (physical-name dst-var) (pretty-type dst-type))
                  (unless (equal? src-var dst-var)
                    (if (and (eq? &scm src-type)
                             (eq? &flonum dst-type))
                        (unbox dst-var src-var dst-type local)
                        (begin
                          (dump-move local dst-var src-var)
                          (move dst-var src-var))))
                  (hashq-remove! srcs local)
                  (lp rest)))
            (else
             (dump-load local dst-var (hashq-ref dst-types local))
             (let ((type (hashq-ref dst-types local)))
               (load-frame local type dst-var))
             (lp rest)))))
        (() (values))))))

;;; XXX: Incomplete

;; (define (adjust-downrec-stack asm loop? snapshots dsts)
;;   (let* ((last-index (- (hash-count (const #t) snapshots) 1))
;;          (last-snapshot (hashq-ref snapshots last-index))
;;          (last-sp-offset (snapshot-sp-offset last-snapshot))
;;          (last-fp-offset (snapshot-fp-offset last-snapshot))
;;          (last-nlocals (snapshot-nlocals last-snapshot))
;;          (initial-nlocals (snapshot-nlocals (hashq-ref snapshots 0))))
;;     (vm-expand-stack asm last-sp-offset)
;;     (shift-fp (if loop?
;;                   (- (+ last-sp-offset last-nlocals) initial-nlocals)
;;                   last-sp-offset))
;;     (let lp ((locals (snapshot-locals last-snapshot))
;;              (vars (snapshot-variables last-snapshot)))
;;       (match (list locals vars)
;;         ((((local . type) . locals) (var . vars))
;;          (store-frame asm (- local last-sp-offset) type var)
;;          (lp locals vars))
;;         (_
;;          (let lp ((dsts dsts)
;;                   (srcs (snapshot-variables last-snapshot)))
;;            (match (list dsts srcs)
;;              (((dst . dsts) (src . srcs))
;;               (move dst src)
;;               (lp dsts srcs))
;;              (_
;;               (values)))))))))

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
            (exit-counts (make-hash-table))
            (loop-address (and loop-label (jit-address loop-label)))
            (end-address (or (and=> (env-parent-fragment env)
                                    fragment-end-address)
                             (jit-address epilog-label)))
            (parent-id (or (and=> (env-parent-fragment env) fragment-id)
                           0))
            (verbosity (lightning-verbosity))
            (gdb-jit-entry
             (if (tjit-dump-dwarf? (tjit-dump-option))
                 (let* ((addr (pointer-address (bytevector->pointer code)))
                        (elf (make-gdb-jit-elf (env-id env) addr size
                                               (car sline)
                                               (or (cdr sline) 1))))
                   (tjit-register-gdb-jit-entry! elf))
                 #f))
            (loop-vars
             (if (list? (env-loop-vars env))
                 (let lp ((vars (reverse (env-loop-vars env))) (acc '()))
                   (if (null? vars)
                       (reverse! acc)
                       (lp (cdr vars)
                           (cons (hashq-ref storage (car vars)) acc))))
                 #f))
            (type-checker (if (zero? parent-id)
                              (gen-type-checker (env-entry-types env)
                                                (env-id env))
                              #f))
            (bcode (compile-bailouts env end-address trampoline bailouts)))
       (make-bytevector-executable! code)

       ;; Same entry-ip could be used when side exit 0 was taken for
       ;; multiple times. Using trace ID as hash table key.
       (put-fragment! (env-id env)
                      (make-fragment (env-id env)
                                     code
                                     exit-counts
                                     (env-downrec? env)
                                     (env-uprec? env)
                                     type-checker
                                     (env-entry-ip env)
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
                                     bcode
                                     (env-handle-interrupts? env)
                                     '()))
       (debug 4 ";;; jit-print:~%~a~%" (jit-print))

       ;; When this trace is a side trace, replace the native code
       ;; of trampoline in parent fragment.
       (let ((fragment (env-parent-fragment env))
             (code-address (pointer-address ptr)))
         (when fragment
           (let ((trampoline (fragment-trampoline fragment))
                 (side-trace-ids (fragment-side-trace-ids fragment)))
             (trampoline-set! trampoline (env-parent-exit-id env) ptr)
             (set-snapshot-code! (env-parent-snapshot env) code)
             (set-fragment-side-trace-ids! fragment
                                           (cons (env-id env) side-trace-ids))))
         (values code size code-address loop-address trampoline))))))

(define (compile-entry env primops snapshots)
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log (env-id env))))
      (set-tjit-time-log-assemble! log (get-internal-run-time))))
  (let* ((trampoline (make-trampoline (hash-count (const #t) snapshots)))
         (fragment (env-parent-fragment env)))

    (cond
     ;; Root trace.
     ((not fragment)
      (let ((max-spills (tjit-max-spills))
            (nspills (primops-nspills primops))
            (vp r0)
            (registers r1))
        (when (< max-spills nspills)
          (failure 'compile-entry "Too many spills ~s" nspills))

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
        (jit-stxi registers-offset %fp registers)))

     ;; Side trace.
     (else
      ;; Avoid emitting prologue.
      (jit-tramp (imm (* 4 %word-size)))

      ;; Store values passed from parent trace when it's not used by this
      ;; side trace.
      (match (env-parent-snapshot env)
        (($ $snapshot _ _ _ _ locals vars)
         (let* ((snap0 (hashq-ref snapshots 0))
                (locals0 (snapshot-locals snap0))
                (vars0 (snapshot-variables snap0))
                (references (make-hash-table))
                (parent-fragment (env-parent-fragment env))
                (storage (fragment-storage parent-fragment))
                (asm (make-asm storage #f #t #f)))
           (let lp ((locals0 locals0) (vars0 vars0))
             (match (cons locals0 vars0)
               ((((local . _) . locals0) . (var . vars0))
                (hashq-set! references local var)
                (lp locals0 vars0))
               (_
                (maybe-store asm locals vars references 0))))))
        (_
         (failure 'compile-entry "snapshot not found"
                      (env-parent-exit-id env))))))

    ;; Assemble the primitives.
    (compile-body env primops snapshots trampoline)))

(define (compile-body env primops snapshots trampoline)
  (define (compile-ops asm ops storage acc)
    (let lp ((ops ops) (acc acc))
      (match ops
        ((('%snap snapshot-id . args) . ops)
         (cond
          ((hashq-ref snapshots snapshot-id)
           => (lambda (snapshot)
                (cond
                 ;; ((snapshot-downrec? snapshot)
                 ;;  (let-values
                 ;;      (((loop? dsts)
                 ;;        (if (tj-parent-fragment tj)
                 ;;            (let* ((linked-ip (env-linked-ip env))
                 ;;                   (linked-fragment (get-root-trace linked-ip)))
                 ;;              (values #f (fragment-loop-vars linked-fragment)))
                 ;;            (values #t (env-loop-vars env)))))
                 ;;    (compile-downrec tj asm loop? snapshot
                 ;;                     (snapshot-nlocals (hashq-ref snapshots 0))
                 ;;                     dsts))
                 ;;  (lp ops acc))
                 ;; ((snapshot-uprec? snapshot)
                 ;;  (compile-uprec tj asm snapshot (tj-loop-locals tj)
                 ;;                 (env-loop-vars env))
                 ;;  (lp ops acc))
                 ((snapshot-link? snapshot)
                  (compile-link env asm args snapshot storage)
                  (lp ops acc))
                 (else
                  (let ((out-code (trampoline-ref trampoline snapshot-id))
                        (gen-bailout (compile-bailout env asm snapshot
                                                      trampoline args)))
                    (let ((exit (jit-forward)))
                      (jit-patch-abs exit out-code)
                      (set-asm-exit! asm exit))
                    (lp ops (cons gen-bailout acc)))))))
          (else
           (failure 'compile-ops "no snapshot ~s" snapshot-id))))
        (((op-name . args) . ops)
         (cond
          ((hashq-ref *native-prim-procedures* op-name)
           => (lambda (proc)
                (apply proc asm args)
                (lp ops acc)))
          (else
           (failure 'compile-ops "op not found ~s" op-name))))
        (()
         acc))))
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
            (asm (make-asm storage end-address #t save-volatiles?))
            (gen-bailouts (compile-ops asm entry storage '())))
       (let-values (((loop-label gen-bailouts)
                     (compile-loop asm loop storage gen-bailouts)))
         (values trampoline loop-label gen-bailouts storage))))
    (_
     (failure 'compile-body "not a $primops" primops))))

(define (compile-bailout env %asm snapshot trampoline args)
  (lambda (end-address)
    (let ((ip (snapshot-ip snapshot))
          (id (snapshot-id snapshot)))
      (debug 3 ";;; compile-bailout:~%")
      (debug 3 ";;;   snapshot-id: ~a~%" id)
      (debug 3 ";;;   next-ip:     ~a~%" ip)
      (debug 3 ";;;   args:        ~a~%" args)
      (let ((entry (jit-label)))
        (jit-patch entry)
        (match snapshot
          (($ $snapshot id sp-offset fp-offset nlocals locals)
           ;; Store contents of args to the stack.
           (let lp ((locals locals) (args args))
             (match (cons locals args)
               ((((local . type) . locals) . (arg . args))
                (syntax-parameterize ((asm (identifier-syntax %asm)))
                  (store-frame local type arg))
                (lp locals args))
               (_ (values))))

           ;; Shift SP, then shift FP with nlocals.
           (shift-sp %asm sp-offset)
           (shift-fp nlocals)

           ;; Sync next IP with vp->ip for VM.
           (jit-movi r0 (imm ip))
           (vm-sync-ip r0)

           ;; Make tjit-retval for VM interpreter.
           (jit-prepare)
           (jit-pushargr %thread)
           (jit-pushargi (scm-i-makinumi id))
           (jit-pushargi (scm-i-makinumi (env-id env)))
           (jit-pushargi (scm-i-makinumi nlocals))
           (jit-calli %scm-make-tjit-retval)
           (jit-retval %retval)

           ;; Debug code to dump tjit-retval and locals.
           (let ((dump-option (tjit-dump-option)))
             (when (tjit-dump-exit? dump-option)
               (jit-movr %thread %retval)
               (jit-prepare)
               (jit-pushargr %retval)
               (load-vp %retval)
               (jit-pushargr %retval)
               (jit-calli %scm-tjit-dump-retval)
               (jit-movr %retval %thread)
               (when (tjit-dump-verbose? dump-option)
                 (jit-movr %thread %retval)
                 (jit-prepare)
                 (jit-pushargi (scm-i-makinumi (env-id env)))
                 (jit-pushargi (imm nlocals))
                 (load-vp %retval)
                 (jit-pushargr %retval)
                 (jit-calli %scm-tjit-dump-locals)
                 (jit-movr %retval %thread)))))
          (_
           (debug 2 "*** compile-bailout: not a snapshot ~a~%" snapshot)))

        (jumpi end-address)
        (cons id entry)))))

(define (compile-bailouts env end-address trampoline bailouts)
  ;; Emit bailouts with end address of this code. Side traces need to
  ;; jump to the address of epilogue of parent root trace, to manage
  ;; non-volatile registers.
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log (env-id env))))
      (set-tjit-time-log-bailout! log (get-internal-run-time))))
  (with-jit-state
   (jit-prolog)
   (jit-tramp (imm (* 4 %word-size)))
   (let ((entries (map (lambda (proc) (proc end-address)) bailouts)))
     (jit-epilog)
     (jit-realize)
     (let* ((estimated-size (jit-code-size))
            (code (make-bytevector estimated-size))
            (_ (jit-set-code (bytevector->pointer code)
                             (imm estimated-size)))
            (ptr (jit-emit)))
       (make-bytevector-executable! code)
       (for-each
        (match-lambda
          ((id . entry)
           (trampoline-set! trampoline id (jit-address entry))))
        entries)
       code))))

(define (compile-link env %asm args snapshot storage)
  (define (make-src-var-table storage indices shift)
    (let lp ((indices indices) (ret (make-hash-table)))
      (match indices
        ((n . indices)
         (let ((var (make-var n)))
           (and=> (hashq-ref storage var)
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
     (let* ((linked-fragment (env-linked-fragment env))
            (loop-locals (and=> linked-fragment fragment-loop-locals))
            (dst-type-table (make-hash-table))
            (dst-var-table (make-hash-table))
            (lives (env-live-indices env))
            (src-var-table (make-src-var-table storage lives sp-offset))
            (src-type-table (make-src-type-table locals sp-offset))
            (ref-table (make-hash-table))
            (linked-snapshots (fragment-snapshots linked-fragment))
            (linked-snap1 (hashq-ref linked-snapshots 1))
            (parent-snapshot (env-parent-snapshot env))
            (parent-sp-offset (snapshot-sp-offset parent-snapshot))
            (parent-depth (snapshot-inline-depth parent-snapshot))
            (my-depth (snapshot-inline-depth snapshot)))

       (let lp ((loop-locals loop-locals)
                (vars (fragment-loop-vars linked-fragment)))
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
                 (maybe-store %asm locals args ref-table sp-offset)

                 ;; Shift SP, then shift FP with nlocals.
                 (shift-sp %asm sp-offset)
                 (shift-fp nlocals)

                 ;; Move or load locals for linked trace.
                 (syntax-parameterize ((asm (identifier-syntax %asm)))
                   (move-or-load-carefully dst-var-table src-var-table
                                           dst-type-table src-type-table)

                   ;; Handle interrupts if linked fragment didn't.
                   (when (and (env-handle-interrupts? env)
                              (not (fragment-handle-interrupts?
                                    linked-fragment)))
                     (vm-handle-interrupts)))

                 ;; Jump to the beginning of the loop in linked trace.
                 (jumpi (fragment-loop-address linked-fragment))))))))))
    (_
     (failure 'compile-link "not a snapshot ~s" snapshot))))

;; XXX: Incomplete
;; (define (compile-downrec tj asm loop? snapshot initial-nlocals dsts)
;;   (let* ((last-sp-offset (snapshot-sp-offset snapshot))
;;          (last-fp-offset (snapshot-fp-offset snapshot))
;;          (last-nlocals (snapshot-nlocals snapshot)))
;;     (vm-expand-stack asm last-sp-offset)
;;     (shift-fp (if loop?
;;                   (- (+ last-sp-offset last-nlocals) initial-nlocals)
;;                   last-sp-offset))
;;     (let lp ((locals (snapshot-locals snapshot))
;;              (vars (snapshot-variables snapshot)))
;;       (match (list locals vars)
;;         ((((local . type) . locals) (var . vars))
;;          (store-frame asm (- local last-sp-offset) type var)
;;          (lp locals vars))
;;         (_
;;          (let lp ((dsts dsts)
;;                   (srcs (snapshot-variables snapshot)))
;;            (match (list dsts srcs)
;;              (((dst . dsts) (src . srcs))
;;               ;; XXX: Move carefully, avoid overwriting srcs with dsts.
;;               (move dst src)
;;               (lp dsts srcs))
;;              (_
;;               (values)))))))
;;     (when (not loop?)
;;       (let ((linked-fragment (get-root-trace (env-linked-ip env))))
;;         (jumpi (fragment-loop-address linked-fragment))))))

;; XXX: Incomplete
;; (define (compile-uprec tj asm snapshot dst-locals dst-vars)
;;   (define (make-local-var-table locals vars)
;;     (let ((t (make-hash-table)))
;;       (let lp ((locals locals)
;;                (vars vars))
;;         (match (list locals vars)
;;           ((((local . _ ) . locals) (var . vars))
;;            (hashq-set! t local var)
;;            (lp locals vars))
;;           (_
;;            t)))))
;;   (let ((dsts (make-local-var-table dst-locals dst-vars))
;;         (sp-offset (snapshot-sp-offset snapshot))
;;         (fp-offset (snapshot-fp-offset snapshot))
;;         (nlocals (snapshot-nlocals snapshot)))
;;     (let lp ((locals (snapshot-locals snapshot))
;;              (vars (snapshot-variables snapshot)))
;;       (match (list locals vars)
;;         ((((local . type) . locals) (var . vars))
;;          (store-frame asm local type var)
;;          (lp locals vars))
;;         (_
;;          (values))))
;;     (let lp ((locals (snapshot-locals snapshot))
;;              (vars (snapshot-variables snapshot)))
;;       (match (list locals vars)
;;         ((((local . type) . locals) (var . vars))
;;          (cond
;;           ((hashq-ref dsts (- local sp-offset))
;;            => (lambda (dst)
;;                 (move dst var)
;;                 (lp locals vars)))
;;           (else
;;            (lp locals vars))))
;;         (_
;;          (shift-sp fp-offset))))))
