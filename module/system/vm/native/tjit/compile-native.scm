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
  #:export (compile-native
            compile-mcode))


;;;
;;; Scheme constants and syntax
;;;

(define %scm-tjit-make-retval
  (dynamic-pointer "scm_tjit_make_retval" (dynamic-link)))

(define %scm-tjit-dump-retval
  (dynamic-pointer "scm_tjit_dump_retval" (dynamic-link)))

(define %scm-tjit-dump-locals
  (dynamic-pointer "scm_tjit_dump_locals" (dynamic-link)))

(define %scm-tjit-dump-fp
  (dynamic-pointer "scm_tjit_dump_fp" (dynamic-link)))

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
  (debug 3 ";;; store-frame: local:~a type:~a src:~a~%"
         local (pretty-type type) src)
  (cond
   ((return-address? type)
    ;; Moving value coupled with type to frame local. Return address of VM frame
    ;; need to be recovered when taking exit from inlined procedure call. The
    ;; actual value for return address is captured at the time of Scheme IR
    ;; conversion and stored in snapshot as pointer.
    (jit-movi r0 (return-address-ip type))
    (frame-set! local r0))

   ((dynamic-link? type)
    ;; Storing fp to local. Dynamic link is stored as offset in type. VM's fp
    ;; could move, may use different value at the time of compilation and
    ;; execution.
    (jit-ldxi r0 fp vp->fp-offset)
    (let* ((amount (* (dynamic-link-offset type) %word-size))
           (ptr (make-signed-pointer amount)))
      (jit-addi r0 r0 ptr))
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

(define (refill-dynamic-links local-offset local-x-types)
  ;; Take difference of dynamic links from the previous one, then refill
  ;; and update the chain of dynamic links.
  (let ((old-vp->fp r0)
        (new-vp->fp r1)
        (offsets
         (let lp ((local-x-types (reverse local-x-types))
                  (prev-offset local-offset)
                  (acc '()))
           (match local-x-types
             (((_ . ($ $dynamic-link offset)) . local-x-types)
              (lp local-x-types
                  offset
                  (cons (- prev-offset offset) acc)))
             ((_ . local-x-types)
              (lp local-x-types prev-offset acc))
             (()
              acc)))))
    (debug 1 ";;; refill-dynamic-links: offsets=~a~%" offsets)
    (jit-ldxi old-vp->fp fp vp->fp-offset)
    (jit-movr new-vp->fp old-vp->fp)
    (let lp ((offsets offsets))
      (match offsets
        ((offset . offsets)
         (jit-addi new-vp->fp old-vp->fp
                   (make-signed-pointer (* offset %word-size)))
         (scm-frame-set-dynamic-link! new-vp->fp old-vp->fp)

         ;; XXX: Workaround for lightning. Without move of old vp->fp to itself,
         ;; lightning will omit the move of old vp->fp to new vp->fp in next
         ;; iteration of `lp'. Tell lightning to force emit of `movr'.
         (jit-movr old-vp->fp old-vp->fp)

         (jit-movr old-vp->fp new-vp->fp)
         (lp offsets))
        (()
         (jit-stxi vp->fp-offset fp new-vp->fp)
         (vm-sync-fp new-vp->fp))))))

(define (dump-bailout ip exit-id code)
  (let ((verbosity (lightning-verbosity)))
    (when (and verbosity (<= 4 verbosity))
      (call-with-output-file
          (format #f "/tmp/bailout-~x-~4,,,'0@a.o" ip exit-id)
        (lambda (port)
          (put-bytevector port code)
          (jit-print))))))


;;;
;;; The Machine Code Compiler
;;;

(define (compile-mcode primlist entry-ip locals snapshots fragment
                       parent-exit-id linked-ip trace-id)
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log trace-id)))
      (set-tjit-time-log-assemble! log (get-internal-run-time))))
  (let* ((trampoline (make-trampoline (hash-fold (lambda (k v acc) (+ acc 1))
                                                 1 snapshots)))
         (fp-offset
          ;; Root trace allocates spaces for spilled variables, three words to
          ;; store `vp', `vp->fp', and `registers', and one more word for return
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

        ;; Store `vp', `vp->fp', and `registers'.
        (jit-stxi vp-offset fp vp)
        (vm-cache-fp vp)
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
            (($ $snapshot _ _ _ local-x-types exit-variables)
             (let ((locals (snapshot-locals (hashq-ref snapshots 0))))
               (debug 3 ";;; side-trace: locals: ~a~%" locals)
               (maybe-store moffs local-x-types exit-variables identity
                            locals 0)))))
       (else
        (error "compile-tjit: snapshot not found in parent trace"
               parent-exit-id)))))

    ;; Assemble the primitives in CPS.
    (compile-primlist primlist #f entry-ip snapshots fp-offset fragment
                      trampoline linked-ip trace-id)))

(define (compile-primlist primlist env entry-ip snapshots fp-offset fragment
                          trampoline linked-ip trace-id)
  (define (compile-ops asm ops)
    (let lp ((ops ops) (loop-locals #f) (loop-vars #f) (shifted 0))
      (match ops
        ((('%snap snapshot-id . args) . ops)
         (cond
          ((hashq-ref snapshots snapshot-id)
           => (lambda (snapshot)
                (let* ((this-offset (snapshot-offset snapshot))
                       ;; Keeping track of shifted amount of FP. This amount
                       ;; should matches with the amount shifted by `%return'.
                       (shifted (if (and (< this-offset 0)
                                         (< this-offset shifted))
                                    this-offset
                                    shifted)))
                  (cond
                   ((snapshot-set-loop-info? snapshot)
                    (match snapshot
                      (($ $snapshot _ _ _ local-x-types)
                       (lp ops local-x-types args shifted))
                      (else
                       (error "snapshot loop info not found~%"))))
                   ((snapshot-jump-to-linked-code? snapshot)
                    (compile-link args snapshot asm linked-ip fragment shifted)
                    (lp ops loop-locals loop-vars shifted))
                   (else
                    (let ((ptr (compile-bailout asm trace-id snapshot args))
                          (out-code (trampoline-ref trampoline snapshot-id)))
                      (trampoline-set! trampoline snapshot-id ptr)
                      (set-asm-out-code! asm out-code)
                      (lp ops loop-locals loop-vars shifted)))))))
          (else
           (hash-for-each (lambda (k v)
                            (format #t ";;; key:~a => val:~a~%" k v))
                          snapshots)
           (error "compile-ops: no snapshot with id" snapshot-id))))
        (((op-name . args) . ops)
         (cond
          ((hashq-ref *native-prim-procedures* op-name)
           => (lambda (proc)
                (let ((verbosity (lightning-verbosity)))
                  (when (<= 4 verbosity)
                    (jit-note (format #f "~a" (cons op-name args)) 0)))
                (apply proc asm args)
                (lp ops loop-locals loop-vars shifted)))
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
       (($ $snapshot _ local-offset nlocals local-x-types)

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

        ;; Internal FP in VM_ENGINE will get updated with C macro `CACHE_FP'.
        ;; Adding offset for positive local offset, fp is already shifted in
        ;; store-snapshot for negative local offset.
        (when (not (= 0 local-offset))
          (debug 3 ";;; compile-bailout: shifting FP, local-offset=~a~%"
                 local-offset)
          (refill-dynamic-links local-offset local-x-types))

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
              (jit-ldxi reg-retval fp vp->fp-offset)
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

(define (compile-link args snapshot asm linked-ip fragment fp-shifted)
  (let* ((linked-fragment (get-root-trace linked-ip))
         (loop-locals (fragment-loop-locals linked-fragment)))
    (define (moffs mem)
      (let ((offset (* (ref-value mem) %word-size)))
        (make-signed-pointer (+ (asm-fp-offset asm) offset))))
    (define (shift-offset offset locals)
      ;; Shifting locals with given offset.
      (debug 3 ";;; compile-link: shift-offset, offset=~a~%" offset)
      (map (match-lambda ((local . type)
                          `(,(- local offset) . ,type)))
           locals))
    (debug 3 ";;; compile-link: args=~a~%" args)
    (match snapshot
      (($ $snapshot sid local-offset _ local-x-types)
       ;; Store unpassed variables, and move variables to linked trace.
       ;; Shift amount in `maybe-store' depending on whether the trace is
       ;; root trace or not.
       (let* ((src-shift-amount (if fragment
                                    fp-shifted
                                    local-offset))
              (dst-shift-amount (if fragment
                                    fp-shifted
                                    0))
              (references (shift-offset src-shift-amount loop-locals))
              (src-table (maybe-store moffs local-x-types args identity
                                      references local-offset))
              (type-table (make-hash-table))
              (dst-table (make-hash-table)))
         (let lp ((locals loop-locals)
                  (dsts (fragment-loop-vars linked-fragment)))
           (match (list locals dsts)
             ((((local . type) . locals) (dst . dsts))
              (hashq-set! type-table local type)
              (hashq-set! dst-table (- local dst-shift-amount) dst)
              (lp locals dsts))
             (_
              (values))))
         (move-or-load-carefully dst-table src-table type-table moffs))

       ;; Shift back FP and sync. Shifting back fp-shifted moved by
       ;; `%return', and shifting for local-offset to match the FP in
       ;; middle of procedure call.
       (when (< fp-shifted 0)
         (refill-dynamic-links local-offset local-x-types))

       ;; Jumping from loop-less root trace, shifting FP.
       ;;
       ;; XXX: Add more tests for checking loop-less root traces.
       (when (and (not fragment)
                  (not (= local-offset 0)))
         (let ((vp->fp r0))
           (jit-ldxi vp->fp fp vp->fp-offset)
           (jit-addi vp->fp vp->fp (imm (* local-offset %word-size)))
           (jit-stxi vp->fp-offset fp vp->fp)
           (vm-sync-fp vp->fp)))

       ;; Jump to the beginning of linked trace.
       (jumpi (fragment-loop-address linked-fragment)))
      (_
       (debug 3 ";;; compile-link: IP is 0, snapshot not found~%")))))
