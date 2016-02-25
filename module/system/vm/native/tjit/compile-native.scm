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
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit compile-ir)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit gdb)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit state)
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

(define (store-frame asm local type src)
  (debug 3 ";;; store-frame: local:~a type:~a src:~a~%"
         local (pretty-type type) src)
  (let ((err (lambda ()
               (tjitc-error 'store-frame "~s ~a ~s"
                            local (pretty-type type) src))))
    (cond
     ((return-address? type)
      ;; Moving value coupled with type to frame local. Return address of VM
      ;; frame need to be recovered when taking exit from inlined procedure
      ;; call. The actual value for return address is captured at the time of
      ;; Scheme IR conversion and stored in snapshot as pointer.
      (jit-movi r0 (return-address-ip type))
      (sp-set! local r0))

     ((dynamic-link? type)
      ;; Storing fp to local. Dynamic link is stored as offset in type. VM's fp
      ;; could move, may use different value at the time of compilation and
      ;; execution.
      (jit-movi r0 (imm (dynamic-link-offset type)))
      (sp-set! local r0))

     ;; Floating point values
     ((eq? type &flonum)
      (cond
       ((constant? src)
        (jit-movi-d f0 (constant src))
        (scm-from-double asm r0 f0)
        (sp-set! local r0))
       ((gpr? src)
        (gpr->fpr f0 (gpr src))
        (scm-from-double asm r0 f0)
        (sp-set! local r0))
       ((fpr? src)
        (scm-from-double asm r0 (fpr src))
        (sp-set! local r0))
       ((memory? src)
        (memory-ref/f f0 src)
        (scm-from-double asm r0 f0)
        (sp-set! local r0))
       (else (err))))
     ((eq? type &f64)
      (cond
       ((constant? src)
        (jit-movi-d f0 (constant src))
        (sp-set!/f local f0))
       ((fpr? src)
        (sp-set!/f local (fpr src)))
       ((memory? src)
        (memory-ref/f f0 src)
        (sp-set!/f local f0))
       (else (err))))

     ;; Immediates
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

     ;; Cell values and small integers
     ((or (memq type (list &scm &fixnum &char &null &nil
                           &symbol &keyword &procedure &pointer
                           &pair &fluid &vector &box &struct &string &bytevector
                           &bitvector &array &hash-table &u64 &s64))
          ;; XXX: Should resolve copy at this point. Storeing copied value as
          ;; is, assuming that copy source is non-unboxed value.
          (and (pair? type)
               (eq? 'copy (car type))))
      (cond
       ((constant? src)
        (jit-movi r0 (constant src))
        (sp-set! local r0))
       ((gpr? src)
        (sp-set! local (gpr src)))
       ((fpr? src)
        (fpr->gpr r0 (fpr src))
        (sp-set! local r0))
       ((memory? src)
        (memory-ref r0 src)
        (sp-set! local r0))
       (else (err))))
     (else (err)))))

(define (maybe-store asm local-x-types srcs references shift)
  "Store src in SRCS to frame when local is not found in REFERENCES."
  (debug 3 ";;; maybe-store:~%")
  (debug 3 ";;;   srcs:          ~a~%" srcs)
  (debug 3 ";;;   local-x-types: ~a~%"
         (map (match-lambda ((n . t) `(,n . ,(pretty-type t))))
              local-x-types))
  (debug 3 ";;;   references:    ~a~%" (hash-map->list cons references))
  (let lp ((local-x-types local-x-types)
           (srcs srcs))
    (match (list local-x-types srcs)
      ((((local . type) . local-x-types) (src . srcs))
       (when (or (dynamic-link? type)
                 (return-address? type)
                 (not references)
                 (let ((reg (hashq-ref references (- local shift))))
                   (or (not reg)
                       (not (equal? src reg)))))
         (store-frame asm local type src))
       (lp local-x-types srcs))
      (_ (values)))))

(define (shift-fp nlocals)
  "Adjust FP with SP and NLOCALS."
  (let ((vp r0)
        (vp->fp r1))
    (load-vp vp)
    (jit-addi vp->fp %sp (imm (* nlocals %word-size)))
    (store-vp->fp vp vp->fp)))

(define (shift-sp offset)
  (let ((op (if (< 0 offset)
                jit-addi
                jit-subi)))
    (op %sp %sp (imm (* (abs offset) %word-size)))
    (vm-sync-sp %sp)))

(define (storage->src-table storage indices shift)
  (debug 2 ";;; [storage->src-table] indices=~s~%" indices)
  (let ((ret (make-hash-table)))
    (for-each (lambda (n)
                (let ((var (make-var n)))
                  (and=> (hashq-ref storage var)
                         (lambda (phy)
                           (hashq-set! ret (- n shift) phy)))))
              indices)
    ret))

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
    (debug 3 ";;; molc: [local ~a] (move ~a ~a)~%" local
           (physical-name dst) (physical-name src)))
  (define (dump-load local dst type)
    (debug 3 ";;; molc: [local ~a] loading to ~a, type=~a~%" local
           (physical-name dst) (pretty-type type)))
  (define (dump-regs label regs)
    (let ((car-< (lambda (a b) (< (car a) (car b)))))
      (debug 3 ";;; molc: ~s: ~a~%" label
             (sort (map (match-lambda ((k . v) (cons k (physical-name v))))
                        regs)
                   car-<))))
  (dump-regs 'dsts (hash-map->list cons dsts))
  (dump-regs 'srcs (hash-map->list cons srcs))
  (let lp ((dsts (hash-map->list cons dsts)))
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
                ;; When all of the elements in dsts are in srcs, move one of the
                ;; srcs to temporary location. `-2' is for gpr R1 or fpr F1 in
                ;; lightning, used as scratch register in this module.
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
           (load-frame local type dst-var))
         (lp rest))))
      (() (values)))))

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

(define (compile-native tj outline primops snapshots sline)
  (with-jit-state
   (jit-prolog)
   (let-values
       (((trampoline loop-label bailouts storage)
         (compile-entry tj outline primops snapshots)))
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
            (end-address (or (and=> (tj-parent-fragment tj)
                                    fragment-end-address)
                             (jit-address epilog-label)))
            (parent-id (or (and=> (tj-parent-fragment tj) fragment-id)
                           0))
            (verbosity (lightning-verbosity))
            (gdb-jit-entry
             (if (tjit-dump-dwarf? (tjit-dump-option))
                 (let* ((addr (pointer-address (bytevector->pointer code)))
                        (elf (make-gdb-jit-elf (tj-id tj) addr size
                                               (car sline) (cdr sline))))
                   (tjit-register-gdb-jit-entry! elf))
                 #f))
            (loop-vars
             (if (list? (tj-loop-vars tj))
                 (let lp ((vars (reverse (tj-loop-vars tj))) (acc '()))
                   (if (null? vars)
                       (reverse! acc)
                       (lp (cdr vars)
                           (cons (hashq-ref storage (car vars)) acc))))
                 #f)))
       (make-bytevector-executable! code)

       (when (tjit-dump-time? (tjit-dump-option))
         (let ((log (get-tjit-time-log (tj-id tj))))
           (set-tjit-time-log-bailout! log (get-internal-run-time))))

       ;; Emit bailouts with end address of this code. Side traces need to
       ;; jump to the address of epilogue of parent root trace, to manage
       ;; non-volatile registers.
       (for-each (lambda (proc) (proc end-address)) bailouts)

       ;; Same entry-ip could be used when side exit 0 was taken for
       ;; multiple times. Using trace ID as hash table key.
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
                                     (tj-loop-locals tj)
                                     loop-vars
                                     snapshots
                                     trampoline
                                     end-address
                                     gdb-jit-entry
                                     storage))

       (debug 4 ";;; jit-print:~%~a~%" (jit-print))

       ;; When this trace is a side trace, replace the native code
       ;; of trampoline in parent fragment.
       (let ((fragment (tj-parent-fragment tj))
             (code-address (pointer-address ptr)))
         (when fragment
           (let ((trampoline (fragment-trampoline fragment)))
             (trampoline-set! trampoline (tj-parent-exit-id tj) ptr)
             (set-snapshot-code! (tj-parent-snapshot tj) code)))
         (values code size code-address loop-address trampoline))))))

(define (compile-entry tj outline primops snapshots)
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log (tj-id tj))))
      (set-tjit-time-log-assemble! log (get-internal-run-time))))
  (let* ((trampoline (make-trampoline (hash-count (const #t) snapshots)))
         (fragment (tj-parent-fragment tj)))
    (cond
     ;; Root trace.
     ((not fragment)
      (let ((max-spills (tjit-max-spills))
            (nspills (primops-nspills primops))
            (vp r0)
            (registers r1))
        (when (< max-spills nspills)
          (tjitc-error 'compile-entry "Too many spills ~s" nspills))

        ;; Root trace allocates spaces for spilled variables. One word to store
        ;; `registers' from argument, and space to save volatile registers.
        ;;
        ;; Side trace cannot allocate additional memory, because side trace uses
        ;; `jit-tramp'. Native code will not work if number of spilled variables
        ;; exceeds the number returned from parameter `(tjit-max-spills)'.
        ;;
        (jit-allocai (imm (* (+ max-spills *num-fpr* *num-volatiles* 1)
                             %word-size)))

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
        (($ $snapshot _ _ _ _ local-x-types exit-variables)
         (let* ((snap0 (hashq-ref snapshots 0))
                (locals (snapshot-locals snap0))
                (vars (snapshot-variables snap0))
                (references (make-hash-table))
                (asm (make-asm (make-hash-table) #f)))
           (let lp ((locals locals) (vars vars))
             (match (list locals vars)
               ((((local . _) . locals) (var . vars))
                (hashq-set! references local var)
                (lp locals vars))
               (_
                (values))))
           (maybe-store asm local-x-types exit-variables references 0)))
        (_
         (tjitc-error 'compile-entry "snapshot not found"
                      (tj-parent-exit-id tj))))))

    ;; Assemble the primitives.
    (compile-body tj outline primops snapshots trampoline)))

(define (compile-body tj outline primops snapshots trampoline)
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
                 ;;            (let* ((linked-ip (tj-linked-ip tj))
                 ;;                   (linked-fragment (get-root-trace linked-ip)))
                 ;;              (values #f (fragment-loop-vars linked-fragment)))
                 ;;            (values #t (tj-loop-vars tj)))))
                 ;;    (compile-downrec tj asm loop? snapshot
                 ;;                     (snapshot-nlocals (hashq-ref snapshots 0))
                 ;;                     dsts))
                 ;;  (lp ops acc))
                 ;; ((snapshot-uprec? snapshot)
                 ;;  (compile-uprec tj asm snapshot (tj-loop-locals tj)
                 ;;                 (tj-loop-vars tj))
                 ;;  (lp ops acc))
                 ((snapshot-link? snapshot)
                  (compile-link tj outline asm args snapshot storage)
                  (lp ops acc))
                 (else
                  (let ((out-code (trampoline-ref trampoline snapshot-id))
                        (gen-bailout (compile-bailout tj asm snapshot
                                                      trampoline args)))
                    (set-asm-out-code! asm out-code)
                    (let ((exit (jit-forward)))
                      (jit-patch-abs exit out-code)
                      (set-asm-exit! asm exit))
                    (lp ops (cons gen-bailout acc)))))))
          (else
           (tjitc-error 'compile-ops "no snapshot ~s" snapshot-id))))
        (((op-name . args) . ops)
         (cond
          ((hashq-ref *native-prim-procedures* op-name)
           => (lambda (proc)
                (let ((verbosity (lightning-verbosity)))
                  (when (and verbosity (<= 4 verbosity))
                    (jit-note (format #f "~a" (cons op-name args)) 0)))
                (apply proc asm args)
                (lp ops acc)))
          (else
           (tjitc-error 'compile-ops "op not found ~s" op-name))))
        (()
         acc))))
  (define (compile-loop asm loop storage gen-bailouts)
    (if (null? loop)
        (values #f gen-bailouts)
        (let ((loop-label (jit-label)))
          (jit-note "loop" 0)
          (jit-patch loop-label)
          (when (tj-handle-interrupts? tj)
            (vm-handle-interrupts asm))
          (let ((gen-bailouts (compile-ops asm loop storage gen-bailouts)))
            (jump loop-label)
            (values loop-label gen-bailouts)))))
  (match primops
    (($ $primops entry loop mem-idx storage)
     (let* ((fragment (tj-parent-fragment tj))
            (end-address (or (and=> fragment
                                    fragment-end-address)
                             (and=> (get-root-trace (tj-linked-ip tj))
                                    fragment-end-address)))
            (asm (make-asm storage end-address))
            (gen-bailouts (compile-ops asm entry storage '())))
       (let-values (((loop-label gen-bailouts)
                     (compile-loop asm loop storage gen-bailouts)))
         (values trampoline loop-label gen-bailouts storage))))
    (_
     (tjitc-error 'compile-body "not a $primops" primops))))

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
         (($ $snapshot id sp-offset fp-offset nlocals local-x-types)

          ;; Store contents of args to frame. No need to recover the frame with
          ;; snapshot when local-x-types were null OR snapshot 0 of root trace.
          ;; Still snapshot data is used, so that the bytevector of compiled
          ;; native code could be stored in fragment, to avoid garbage
          ;; collection.
          (unless (or (null? local-x-types)
                      (and (not (tj-parent-fragment tj))
                           (zero? id))
                      (and (tj-parent-fragment tj)
                           (tj-loop? tj)
                           (zero? id)))
            (let lp ((local-x-types local-x-types)
                     (args args))
              (match (list local-x-types args)
                ((((local . type) . local-x-types) (arg . args))
                 (store-frame asm local type arg)
                 (lp local-x-types args))
                (_
                 (values)))))

          ;; Shift SP.
          (when (not (zero? sp-offset))
            (shift-sp sp-offset))

          ;; Shift FP, unless moved to up-frame explicitly with primitive
          ;; operations which update `vp->fp'.
          (when (<= fp-offset 0)
            (shift-fp nlocals))

          ;; Sync next IP with vp->ip for VM.
          (jit-movi r0 (imm ip))
          (vm-sync-ip r0)

          ;; Make tjit-retval for VM interpreter.
          (jit-prepare)
          (jit-pushargr %thread)
          (jit-pushargi (scm-i-makinumi id))
          (jit-pushargi (scm-i-makinumi (tj-id tj)))
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
                (jit-pushargi (scm-i-makinumi (tj-id tj)))
                (jit-pushargi (imm nlocals))
                (jit-pushargr %sp)
                (load-vp %retval)
                (jit-pushargr %retval)
                (jit-calli %scm-tjit-dump-locals)
                (jit-movr %retval %thread)))))
         (_
          (debug 2 "*** compile-bailout: not a snapshot ~a~%" snapshot)))

       (jumpi end-address)
       (jit-epilog)
       (jit-realize)
       (let* ((estimated-code-size (jit-code-size))
              (code (make-bytevector estimated-code-size)))
         (jit-set-code (bytevector->pointer code) (imm estimated-code-size))
         (let ((ptr (jit-emit)))
           (make-bytevector-executable! code)
           (dump-bailout ip id code)
           (set-snapshot-code! snapshot code)
           (trampoline-set! trampoline id ptr)))))))

(define (compile-link tj outline asm args snapshot storage)
  (let* ((linked-fragment (get-root-trace (tj-linked-ip tj)))
         (loop-locals (fragment-loop-locals linked-fragment)))
    (match snapshot
      (($ $snapshot _ sp-offset fp-offset nlocals local-x-types vars)
       ;; Store unpassed variables, and move variables to linked trace.
       ;; Shift amount in `maybe-store' depending on whether the trace is
       ;; root trace or not.
       (let* ((type-table (make-hash-table))
              (dst-table (make-hash-table))
              (live-indices (outline-live-indices outline))
              (src-table (storage->src-table storage live-indices sp-offset)))
         (let lp ((loop-locals loop-locals)
                  (dsts (fragment-loop-vars linked-fragment)))
           (match (list loop-locals dsts)
             ((((n . type) . loop-locals) (dst . dsts))
              (hashq-set! type-table n type)
              (hashq-set! dst-table n dst)
              (lp loop-locals dsts))
             (_
              ;; Store locals not passed to linked trace, shift SP, then move
              ;; or load locals for linked trace.
              (maybe-store asm local-x-types args dst-table sp-offset)
              (when (not (zero? sp-offset))
                (shift-sp sp-offset))
              (move-or-load-carefully dst-table src-table type-table)))))

       ;; Shift FP when loop-less root trace or linking root traces.
       (when (or (not (tj-parent-fragment tj))
                 (tj-linking-roots? tj))
         (shift-fp nlocals))

       ;; Jump to the beginning of the loop in linked trace.
       (jumpi (fragment-loop-address linked-fragment)))
      (_
       (tjitc-error 'compile-link "not a snapshot ~s" snapshot)))))

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
;;       (let ((linked-fragment (get-root-trace (tj-linked-ip tj))))
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
