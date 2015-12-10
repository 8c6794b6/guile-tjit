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
  #:use-module (system vm native tjit state)
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

(define (load-frame local type dst)
  (debug 3 ";;; load-frame: local:~a type:~a dst:~a~%"
         local (pretty-type type) dst)
  (sp-ref r0 local)
  (unbox-stack-element dst r0 type #f))

(define (store-frame local type src)
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
      (memory-ref r0 src)
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
      (memory-ref/f f0 src)
      (scm-from-double r0 f0)
      (sp-set! local r0))))
   ((eq? type &f64)
    (cond
     ((constant? src)
      (jit-movi-d f0 (constant src))
      (sp-set!/f local f0))
     ((fpr? src)
      (sp-set!/f local (fpr src)))
     ((memory? src)
      (memory-ref/f f0 src)
      (sp-set!/f local f0))))
   ((memq type (list &box &procedure &pair &u64))
    (cond
     ((constant? src)
      (jit-movi r0 (constant src))
      (sp-set! local r0))
     ((gpr? src)
      (sp-set! local (gpr src)))
     ((memory? src)
      (memory-ref r0 src)
      (sp-set! local r0))))
   ((eq? type &false)
    (jit-movi r0 *scm-false*)
    (sp-set! local r0))
   ((eq? type &true)
    (jit-movi r0 *scm-true*)
    (sp-set! local r0))
   ((eq? type &undefined)
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

(define (maybe-store local-x-types srcs references shift)
  "Store src in SRCS to frame when local is not found in REFERENCES.

Returns a hash-table containing src. Key of the returned hash-table is
local index in LOCAL-X-TYPES. Locals in returned hash-table is shifted
for SHIFT."
  (debug 3 ";;; maybe-store:~%")
  (debug 3 ";;;   srcs:          ~a~%" srcs)
  (debug 3 ";;;   local-x-types: ~a~%" local-x-types)
  (debug 3 ";;;   references:    ~a~%" references)
  (let lp ((local-x-types local-x-types)
           (srcs srcs)
           (acc (make-hash-table)))
    (match (list local-x-types srcs)
      ((((local . type) . local-x-types) (src . srcs))
       (when (or (dynamic-link? type)
                 (return-address? type)
                 (not references)
                 (let ((reg (hashq-ref references (- local shift))))
                   (or (not reg)
                       (not (equal? src reg)))))
         (store-frame local type src))
       (hashq-set! acc (- local shift) src)
       (lp local-x-types srcs acc))
      (_
       acc))))

(define (shift-fp fp-offset)
  (let ((op (if (< 0 fp-offset)
                jit-addi
                jit-subi))
        (vp r0)
        (vp->fp r1))
    (load-vp vp)
    (load-vp->fp vp->fp vp)
    (op vp->fp vp->fp (imm (* (abs fp-offset) %word-size)))
    (store-vp->fp vp vp->fp)))

(define (shift-sp sp-offset)
  (let ((op (if (< 0 sp-offset)
                jit-addi
                jit-subi))
        (vp->sp %sp))
    (op vp->sp vp->sp (imm (* (abs sp-offset) %word-size)))
    (vm-sync-sp vp->sp)))

(define (move-or-load-carefully dsts srcs types)
  "Move SRCS to DSTS or load with TYPES without overwriting.

Avoids overwriting source in hash-table SRCS while updating destinations in
hash-table DSTS.  If source is not found, load value from frame with using type
from hash-table TYPES to get memory offset.  Hash-table key of SRCS, DSTS, TYPES
are local index number."

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
                 ((dst-is-full? (map cdr dsts)
                                (map cdr (hash-map->list cons srcs)))
                  ;; When all of the elements in dsts are in srcs, move one
                  ;; of the srcs to temporary location.  `-2' is for gpr R1
                  ;; or fpr F1 in lightning, used as scratch register in
                  ;; this module.
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
                (when (not (equal? src-var dst-var))
                  (dump-move local dst-var src-var)
                  (move dst-var src-var))
                (hashq-remove! srcs local)
                (lp rest)))
          (else
           (dump-load local dst-var (hashq-ref types local))
           (let ((type (hashq-ref types local)))
             ;; XXX: Add test to check the case ignoring `load-frame'.
             (when type
               (load-frame local type dst-var)))
           (lp rest))))
        (() (values))))))

(define (adjust-downrec-stack asm loop? snapshots dsts)
  (let* ((last-index (- (hash-count (const #t) snapshots) 1))
         (last-snapshot (hashq-ref snapshots last-index))
         (last-sp-offset (snapshot-sp-offset last-snapshot))
         (last-fp-offset (snapshot-fp-offset last-snapshot))
         (last-nlocals (snapshot-nlocals last-snapshot))
         (initial-nlocals (snapshot-nlocals (hashq-ref snapshots 0))))
    (vm-expand-stack asm last-sp-offset)
    (shift-fp (if loop?
                  (- (+ last-sp-offset last-nlocals) initial-nlocals)
                  last-sp-offset))
    (let lp ((locals (snapshot-locals last-snapshot))
             (vars (snapshot-variables last-snapshot)))
      (match (list locals vars)
        ((((local . type) . locals) (var . vars))
         (store-frame (- local last-sp-offset) type var)
         (lp locals vars))
        (_
         (let lp ((dsts dsts)
                  (srcs (snapshot-variables last-snapshot)))
           (match (list dsts srcs)
             (((dst . dsts) (src . srcs))
              (move dst src)
              (lp dsts srcs))
             (_
              (values)))))))))

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

(define (compile-native tj primops snapshots)
  (with-jit-state
   (jit-prolog)
   (let-values
       (((trampoline loop-label loop-locals loop-vars fp-offset gen-bailouts)
         (compile-entry tj primops snapshots)))
     (let ((epilog-label (jit-label)))
       (jit-patch epilog-label)
       (jit-retr %retval)
       (jit-epilog)
       (jit-realize)
       (let* ((estimated-size (jit-code-size))
              (code (make-bytevector estimated-size)))
         (jit-set-code (bytevector->pointer code)
                       (imm estimated-size))
         (let* ((ptr (jit-emit))
                (exit-counts (make-hash-table))
                (loop-address (and loop-label (jit-address loop-label)))
                (end-address (or (and=> (tj-parent-fragment tj)
                                        fragment-end-address)
                                 (jit-address epilog-label)))
                (parent-id (or (and=> (tj-parent-fragment tj)
                                      fragment-id)
                               0)))
           (make-bytevector-executable! code)

           ;; Emit bailouts with end address of this code.
           ;; Side traces need to jump to the address of
           ;; epilogue of parent root trace, to manage
           ;; non-volatile registers.
           (for-each (lambda (proc)
                       (proc end-address))
                     gen-bailouts)

           ;; Same entry-ip could be used when side exit 0 was
           ;; taken for multiple times. Using trace ID as hash
           ;; table key.
           (put-fragment! (tj-id tj)
                          (make-fragment (tj-id tj)
                                         code
                                         exit-counts
                                         (tj-downrec? tj)
                                         (tj-uprec? tj)
                                         (tj-entry-ip tj)
                                         parent-id
                                         (tj-parent-exit-id tj)
                                         loop-address
                                         loop-locals
                                         loop-vars
                                         snapshots
                                         trampoline
                                         fp-offset
                                         end-address))
           (debug 4 ";;; jit-print:~%~a~%" (jit-print))
           ;; When this trace is a side trace, replace the native code
           ;; of trampoline in parent fragment.
           (let ((fragment (tj-parent-fragment tj)))
             (when fragment
               (let ((trampoline (fragment-trampoline fragment)))
                 (trampoline-set! trampoline (tj-parent-exit-id tj) ptr)
                 (set-snapshot-code! (tj-parent-snapshot tj) code))))
           (values code (jit-code-size) (pointer-address ptr)
                   loop-address trampoline)))))))

(define (compile-entry tj primops snapshots)
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log (tj-id tj))))
      (set-tjit-time-log-assemble! log (get-internal-run-time))))
  (let* ((trampoline (make-trampoline (hash-count (const #t) snapshots)))
         (fragment (tj-parent-fragment tj))
         (fp-offset
          ;; Root trace allocates spaces for spilled variables, 1 word
          ;; to store `registers' from argument, and space to save
          ;; volatile registers. Side trace cannot allocate additional
          ;; memory, because side trace uses `jit-tramp'. Native code
          ;; will not work if number of spilled variables exceeds the
          ;; number returned from parameter `(tjit-max-spills)'.
          (if (not fragment)
              (let ((max-spills (tjit-max-spills))
                    (nspills (primops-nspills primops)))
                (when (< max-spills nspills)
                  ;; XXX: Escape from this procedure, increment compilation
                  ;; failure for this entry-ip.
                  (error "Too many spilled variables" nspills))
                (jit-allocai (imm (* (+ max-spills 1 *num-volatiles*)
                                     %word-size))))
              (fragment-fp-offset fragment))))
    (cond
     ;; Root trace.
     ((not fragment)
      (let ((vp r0)
            (registers r1))

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
      (match (tj-parent-snapshot tj)
        (($ $snapshot _ sp-offset _ _ local-x-types exit-variables)
         (let* ((snap0 (hashq-ref snapshots 0))
                (locals (snapshot-locals snap0))
                (vars (snapshot-variables snap0))
                (references (make-hash-table)))
           (let lp ((locals locals) (vars vars))
             (match (list locals vars)
               ((((local . _) . locals) (var . vars))
                (hashq-set! references local var)
                (lp locals vars))
               (_
                (values))))
           (maybe-store local-x-types exit-variables references 0)))
        (_
         (error "compile-native: snapshot not found in parent trace"
                (tj-parent-exit-id tj))))))

    ;; Assemble the primitives.
    (compile-body tj primops snapshots fp-offset trampoline)))

(define (compile-body tj primops snapshots fp-offset trampoline)
  (define (compile-ops asm ops loop-locals loop-vars acc)
    (let lp ((ops ops)
             (loop-locals loop-locals)
             (loop-vars loop-vars)
             (acc acc))
      (match ops
        ((('%snap snapshot-id . args) . ops)
         (cond
          ((hashq-ref snapshots snapshot-id)
           => (lambda (snapshot)
                (cond
                 ((snapshot-downrec? snapshot)
                  (let-values (((loop? dsts)
                                (if (tj-parent-fragment tj)
                                    (let ((linked-fragment
                                           (get-root-trace
                                            (tj-linked-ip tj))))
                                      (values #f
                                              (fragment-loop-vars
                                               linked-fragment)))
                                    (values #t loop-vars))))
                    (compile-downrec tj asm loop? snapshot
                                     (snapshot-nlocals (hashq-ref snapshots 0))
                                     dsts))
                  (lp ops loop-locals loop-vars acc))
                 ((snapshot-uprec? snapshot)
                  (compile-uprec tj asm snapshot loop-locals loop-vars)
                  (lp ops loop-locals loop-vars acc))
                 ((snapshot-set-loop-info? snapshot)
                  (match snapshot
                    (($ $snapshot _ _ _ _ local-x-types)
                     (lp ops local-x-types args acc))
                    (else
                     (error "snapshot loop info not found"))))
                 ((snapshot-link? snapshot)
                  (compile-link tj asm args snapshot)
                  (lp ops loop-locals loop-vars acc))
                 (else
                  (let ((out-code (trampoline-ref trampoline snapshot-id))
                        (gen-bailout (compile-bailout tj asm snapshot
                                                      trampoline args)))
                    (set-asm-out-code! asm out-code)
                    (let ((exit (jit-forward)))
                      (jit-patch-abs exit out-code)
                      (set-asm-exit! asm exit))
                    (lp ops loop-locals loop-vars (cons gen-bailout acc)))))))
          (else
           (error "compile-ops: no snapshot with id" snapshot-id))))
        (((op-name . args) . ops)
         (cond
          ((hashq-ref *native-prim-procedures* op-name)
           => (lambda (proc)
                (let ((verbosity (lightning-verbosity)))
                  (when (and verbosity (<= 4 verbosity))
                    (jit-note (format #f "~a" (cons op-name args)) 0)))
                (apply proc asm args)
                (lp ops loop-locals loop-vars acc)))
          (else
           (error "op not found" op-name))))
        (()
         (values loop-locals loop-vars acc)))))
  (define (compile-loop asm loop gen-bailouts handle-interrupts?
                        loop-locals loop-vars)
    (if (null? loop)
        (values #f gen-bailouts)
        (let ((loop-label (jit-label)))
          (jit-note "loop" 0)
          (when handle-interrupts?
            (vm-handle-interrupts asm))
          (let-values
              (((unused-loop-locals unused-loop-vars gen-bailouts)
                (compile-ops asm loop loop-locals loop-vars gen-bailouts)))
            (jump loop-label)
            (values loop-label gen-bailouts)))))
  (match primops
    (($ $primops entry loop mem-idx env handle-interrupts?)
     (let*-values (((fragment) (tj-parent-fragment tj))
                   ((end-address)
                    (or (and=> fragment
                               fragment-end-address)
                        (and=> (get-root-trace (tj-linked-ip tj))
                               fragment-end-address)))
                   ((asm) (make-asm env end-address))
                   ((loop-locals loop-vars gen-bailouts)
                    (compile-ops asm entry #f #f '()))
                   ((loop-label gen-bailouts)
                    (compile-loop asm loop gen-bailouts handle-interrupts?
                                  loop-locals loop-vars)))
       (values trampoline loop-label loop-locals loop-vars fp-offset
               gen-bailouts)))
    (_
     (error "compile-body: not a $primops" primops))))

(define (compile-bailout tj asm snapshot trampoline args)
  (lambda (end-address)
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
                     (args args))
              (match (list local-x-types args)
                ((((local . type) . local-x-types) (arg . args))
                 (store-frame local type arg)
                 (lp local-x-types args))
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
          (jit-pushargr %thread)
          (jit-pushargi (scm-i-makinumi id))
          (jit-pushargi (scm-i-makinumi (tj-id tj)))
          (jit-pushargi (scm-i-makinumi nlocals))
          (jit-calli %scm-tjit-make-retval)
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
              (when (tjit-dump-locals? dump-option)
                (jit-movr %thread %retval)
                (jit-prepare)
                (jit-pushargi (scm-i-makinumi (tj-id tj)))
                (jit-pushargi (imm nlocals))
                (jit-pushargr %sp)
                (load-vp %retval)
                (jit-pushargr %retval)
                (jit-calli %scm-tjit-dump-locals)
                (jit-movr %retval %thread)))))
         (_
          (debug 1 "*** compile-bailout: not a snapshot ~a~%" snapshot)))

       (jumpi end-address)
       (jit-epilog)
       (jit-realize)
       (let* ((estimated-code-size (jit-code-size))
              (code (make-bytevector estimated-code-size)))
         (jit-set-code (bytevector->pointer code) (imm estimated-code-size))
         (let ((ptr (jit-emit)))
           (debug 3 ";;; compile-bailout: ptr=~a~%" ptr)
           (make-bytevector-executable! code)
           (dump-bailout ip id code)
           (set-snapshot-code! snapshot code)
           (trampoline-set! trampoline id ptr)))))))

(define (compile-link tj asm args snapshot)
  (let* ((linked-fragment (get-root-trace (tj-linked-ip tj)))
         (loop-locals (fragment-loop-locals linked-fragment)))
    (match snapshot
      (($ $snapshot sid sp-offset fp-offset _ local-x-types)
       ;; Store unpassed variables, and move variables to linked trace.
       ;; Shift amount in `maybe-store' depending on whether the trace is
       ;; root trace or not.
       (let* ((type-table (make-hash-table))
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
         (let ((src-table (maybe-store local-x-types args dst-table sp-offset)))
           (move-or-load-carefully dst-table src-table type-table)))

       ;; Shift SP.
       (when (not (= sp-offset 0))
         (shift-sp sp-offset))

       ;; Shift FP for loop-less root trace.
       (when (not (tj-parent-fragment tj))
         (shift-fp fp-offset))

       ;; Jump to the beginning of linked trace.
       (jumpi (fragment-loop-address linked-fragment)))
      (_
       (error "compile-link: not a snapshot" snapshot)))))

(define (compile-downrec tj asm loop? snapshot initial-nlocals dsts)
  (let* ((last-sp-offset (snapshot-sp-offset snapshot))
         (last-fp-offset (snapshot-fp-offset snapshot))
         (last-nlocals (snapshot-nlocals snapshot)))
    (vm-expand-stack asm last-sp-offset)
    (shift-fp (if loop?
                  (- (+ last-sp-offset last-nlocals) initial-nlocals)
                  last-sp-offset))
    (let lp ((locals (snapshot-locals snapshot))
             (vars (snapshot-variables snapshot)))
      (match (list locals vars)
        ((((local . type) . locals) (var . vars))
         (store-frame (- local last-sp-offset) type var)
         (lp locals vars))
        (_
         (let lp ((dsts dsts)
                  (srcs (snapshot-variables snapshot)))
           (match (list dsts srcs)
             (((dst . dsts) (src . srcs))
              (move dst src)
              (lp dsts srcs))
             (_
              (values)))))))
    (when (not loop?)
      (let ((linked-fragment (get-root-trace (tj-linked-ip tj))))
        (jumpi (fragment-loop-address linked-fragment))))))

(define (compile-uprec tj asm snapshot dst-locals dst-vars)
  (define (make-local-var-table locals vars)
    (let ((t (make-hash-table)))
      (let lp ((locals locals)
               (vars vars))
        (match (list locals vars)
          ((((local . _ ) . locals) (var . vars))
           (hashq-set! t local var)
           (lp locals vars))
          (_
           t)))))
  (let ((dsts (make-local-var-table dst-locals dst-vars))
        (sp-offset (snapshot-sp-offset snapshot))
        (fp-offset (snapshot-fp-offset snapshot))
        (nlocals (snapshot-nlocals snapshot)))
    (let lp ((locals (snapshot-locals snapshot))
             (vars (snapshot-variables snapshot)))
      (match (list locals vars)
        ((((local . type) . locals) (var . vars))
         (store-frame local type var)
         (lp locals vars))
        (_
         (values))))
    (let lp ((locals (snapshot-locals snapshot))
             (vars (snapshot-variables snapshot)))
      (match (list locals vars)
        ((((local . type) . locals) (var . vars))
         (cond
          ((hashq-ref dsts (- local sp-offset))
           => (lambda (dst)
                (move dst var)
                (lp locals vars)))
          (else
           (lp locals vars))))
        (_
         (shift-sp fp-offset))))))
