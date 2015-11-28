;;;; Definitions for compiling to IR

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
;;; A module containing definitions to compile bytecode to IR used in vm-tjit.
;;;
;;; Code:

(define-module (system vm native tjit ir)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
  #:export (make-ir
            ir-snapshots set-ir-snapshots!
            ir-snapshot-id set-ir-snapshot-id!
            ir-min-sp-offset set-ir-min-sp-offset!
            ir-max-sp-offset set-ir-max-sp-offset!
            ir-bytecode-index set-ir-bytecode-index!
            ir-vars

            make-var
            make-vars
            get-max-sp-offset
            take-snapshot
            with-frame-ref
            *ir-procedures*
            *local-accumulator*
            accumulate-locals))

;;;
;;; Auxiliary, exported
;;;

(define-record-type <ir>
  (make-ir snapshots snapshot-id parent-snapshot vars
           min-sp-offset max-sp-offset bytecode-index)
  ir?

  ;; Hash table containing snapshots.
  (snapshots ir-snapshots set-ir-snapshots!)

  ;; Current snapshot ID.
  (snapshot-id ir-snapshot-id set-ir-snapshot-id!)

  ;; Snapshot from parent trace, if any.
  (parent-snapshot ir-parent-snapshot)

  ;; List of symbols for variables.
  (vars ir-vars)

  ;; Current minimum SP offset.
  (min-sp-offset ir-min-sp-offset set-ir-min-sp-offset!)

  ;; Current maximum SP offset.
  (max-sp-offset ir-max-sp-offset set-ir-max-sp-offset!)

  ;; Current bytecode index.
  (bytecode-index ir-bytecode-index set-ir-bytecode-index!))

(define (make-var index)
  (string->symbol (string-append "v" (number->string index))))

(define (make-vars locals)
  ;; Might better to use other data structure than alist for variables.
  ;; Number of variables won't change after getting the number of locals from
  ;; `accumulate-locals'.
  (map (lambda (n)
         (cons n (make-var n)))
       locals))

(define (get-max-sp-offset sp-offset fp-offset nlocals)
  (max fp-offset
       (- (+ sp-offset nlocals) 1)
       (if (< fp-offset)
           (- (+ (- fp-offset) nlocals) 1)
           0)))

(define (take-snapshot ip dst-offset locals vars id
                       sp-offset fp-offset min-sp-offset max-sp-offset
                       parent-snapshot past-frame)
  (let* ((nlocals (vector-length locals))
         (dst-ip (+ ip (* dst-offset 4)))
         (args-and-indices
          (let lp ((vars vars) (args '()) (indices '()))
            (match vars
              (((n . var) . vars)
               (if (<= min-sp-offset n max-sp-offset)
                   (lp vars (cons var args) (cons n indices))
                   (lp vars args indices)))
              (()
               (cons args indices)))))
         (args (car args-and-indices))
         (indices (cdr args-and-indices))
         (snapshot (make-snapshot id
                                  sp-offset
                                  fp-offset
                                  min-sp-offset
                                  nlocals
                                  locals
                                  parent-snapshot
                                  indices
                                  past-frame
                                  dst-ip)))
    (values `(%snap ,id ,@args) snapshot)))

(define-syntax-rule (with-frame-ref next args var type idx)
  (cond
   ((not type)
    (debug 1 "XXX: with-frame-ref: var=~a type=~a~%" var type)
    `(let ((,var #f))
       ,(next args)))
   ((= type &flonum)
    `(let ((,var (%fref/f ,idx)))
       ,(next args)))
   (else
    `(let ((,var (%fref ,idx ,type)))
       ,(next args)))))

(define *ir-procedures*
  (make-hash-table 255))

(define *local-accumulator*
  (make-hash-table 255))


;;;
;;; Auxiliary, internal
;;;

(define-syntax define-ir-syntax-parameters
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define-syntax-parameter name
         (lambda (x)
           'name "uninitialized" x))
       ...))))

(define-ir-syntax-parameters
  ir
  past-frame
  ip
  ra
  locals
  handle-interrupts?
  next
  escape)

(define-syntax define-accum
  (syntax-rules (local const)
    "Macro to define local accumulator procedure.

First argument passed to the procedure is a hash-table for accumulating local,
key is local index and value is #t. Second argument is current offset during the
accumulation."
    ((_ st sp-offset)
     st)
    ((_ st sp-offset (local arg) . rest)
     (begin (hashq-set! st (+ arg sp-offset) #t)
            (define-accum st sp-offset . rest)))
    ((_ st sp-offset (const arg) . rest)
     (define-accum st sp-offset . rest))))

(define-syntax define-ir
  (syntax-rules ()
    "Defines procedure to compile VM operation to IR, and optionally local
accumulator when arguments in definition are lists. E.g:

  (define-ir (add1 (local dst) (local src))
    ...)

will define two procedures: one for IR compilation taking two arguments, and
another procedure for accumulator taking two arguments and saving index
referenced by dst and src value at runtime."
    ((_ (name (flag arg) ...) . body)
     (let ((acc (lambda (st sp-offset arg ...)
                  (define-accum st sp-offset (flag arg) ...))))
       (hashq-set! *local-accumulator* 'name acc)
       (define-ir (name arg ...) . body)))
    ((_ (name arg ...) . body)
     (let ((proc
            (lambda (%ir %past-frame %handle-interrupts? %next %escape
                     %ip %ra %locals arg ...)
              (syntax-parameterize
                  ((ir (identifier-syntax %ir))
                   (past-frame (identifier-syntax %past-frame))
                   (handle-interrupts? (identifier-syntax %handle-interrupts?))
                   (next (identifier-syntax %next))
                   (escape (identifier-syntax %escape))
                   (ip (identifier-syntax %ip))
                   (ra (identifier-syntax %ra))
                   (locals (identifier-syntax %locals)))
                . body))))
       (hashq-set! *ir-procedures* 'name proc)))))

(define-syntax define-interrupt-ir
  (syntax-rules ()
    ((_ names-and-args . body)
     (define-ir names-and-args
       (begin
         (variable-set! handle-interrupts? #t)
         . body)))))

(define-syntax-rule (to-fixnum scm)
  `(%rsh ,scm 2))

(define-syntax-rule (to-double scm)
  `(%cref/f ,scm 2))

(define-syntax-rule (dereference-scm addr)
  (pointer->scm (dereference-pointer (make-pointer addr))))

(define-syntax br-op-size
  (identifier-syntax 3))

(define-syntax-rule (local-ref n)
  (vector-ref locals n))

(define-syntax-rule (var-ref n)
  (assq-ref (ir-vars ir) (+ n (current-sp-offset))))

(define-syntax-rule (current-sp-offset)
  (vector-ref (past-frame-sp-offsets past-frame)
              (ir-bytecode-index ir)))

(define-syntax-rule (current-fp-offset)
  (vector-ref (past-frame-fp-offsets past-frame)
              (ir-bytecode-index ir)))

(define-syntax-rule (take-snapshot! ip dst-offset)
  (let-values (((ret snapshot)
                (take-snapshot ip
                               dst-offset
                               locals
                               (ir-vars ir)
                               (ir-snapshot-id ir)
                               (current-sp-offset)
                               (current-fp-offset)
                               (ir-min-sp-offset ir)
                               (ir-max-sp-offset ir)
                               (ir-parent-snapshot ir)
                               past-frame)))
    (let ((old-id (ir-snapshot-id ir)))
      (hashq-set! (ir-snapshots ir) old-id snapshot)
      (set-ir-snapshot-id! ir (+ old-id 1)))
    ret))

;; XXX: Tag more types.
(define-syntax-rule (with-boxing next val var tmp)
  (cond
   ((fixnum? val)
    `(let ((,tmp (%lsh ,var 2)))
       (let ((,tmp (%add ,tmp 2)))
         ,(next tmp))))
   ((flonum? val)
    `(let ((,tmp (%from-double ,var)))
       ,(next tmp)))
   ((pair? val)
    (next var))
   (else
    (debug 1 "XXX: with-boxing: ~a ~a ~a~%" val var tmp)
    (escape #f))))

;; XXX: Tag more types. Add guard.
(define-syntax-rule (with-unboxing next val var)
  (cond
   ((flonum? val)
    `(let ((,var ,(to-double var)))
       ,(next)))
   ((fixnum? val)
    `(let ((,var ,(to-fixnum var)))
       ,(next)))
   ((or (null? val)
        (pair? val)
        (procedure? val))
    (next))
   (else
    (debug 1 "XXX: with-unboxing: ~a ~a~%" val var)
    (escape #f))))


;;; *** Call and return

;;; XXX: halt is not defined, but might not necessary.

(define-ir (call proc nlocals)
  ;; When procedure get inlined, taking snapshot of previous frame.
  ;; Contents of previous frame could change in native code. Note that
  ;; frame return address will get checked at the time of `%return'.
  ;;
  ;; Refilling dynamic link and return address.  These two locals would be
  ;; restored with values in snapshot when taiking side exit. An `%eq' guard
  ;; is added to test the procedure value, to bailout when procedure has been
  ;; redefined.
  ;;
  (let* ((sp-offset (current-sp-offset))
         (stack-size (vector-length locals))
         (fp (- stack-size proc))
         (rra (cons (+ sp-offset fp)
                    (make-return-address (make-pointer (+ ip (* 2 4))))))
         (rdl (cons (+ sp-offset fp 1)
                    (make-dynamic-link proc)))
         (vra (var-ref fp))
         (vdl (var-ref (+ fp 1)))
         (vproc (var-ref (- fp 1)))
         (rproc (local-ref (- fp 1)))
         (rproc-addr (pointer-address (scm->pointer rproc)))
         (snapshot (take-snapshot! ip 0)))
    (push-past-frame! past-frame rdl rra sp-offset locals)
    `(let ((_ ,snapshot))
       (let ((_ (%eq ,vproc ,rproc-addr)))
         ,(if (< 0 (current-fp-offset))
              `(let ((_ (%pcall ,proc)))
                 ,(next))
              (next))))))

;; XXX: call-label
(define-ir (call-label proc nlocals label)
  (next))

;; XXX: tail-call
(define-ir (tail-call nlocals)
  (next))

;; XXX: tail-call-label
;; XXX: tail-call/shuffle

(define-ir (receive dst proc nlocals)
  ;; Two locals below callee procedure in VM frame contain dynamic link and
  ;; return address. VM interpreter refills these two with #f, doing the same
  ;; thing in `emit-next'.
  ;;
  (let* ((stack-size (vector-length locals))
         (vdst (var-ref (- stack-size dst 1)))
         (vsrc (var-ref (- (- stack-size proc) 2)))
         (vdl (var-ref (- stack-size proc)))
         (vra (var-ref (+ (- stack-size proc) 1)))
         (sp-offset (current-sp-offset))
         (min-local-index (+ (- stack-size proc 1) sp-offset 2))
         (max-local-index (+ stack-size sp-offset))
         (load-from-previous-frame
          (lambda ()
            (let lp ((vars (reverse (ir-vars ir))))
              (match vars
                (((n . var) . vars)
                 (cond
                  ((eq? var vdst)
                   (lp vars))
                  ((< min-local-index n max-local-index)
                   (let* ((i (- n sp-offset))
                          (val (if (< -1 i (vector-length locals))
                                   (local-ref i)
                                   (escape #f)))
                          (type (type-of val))
                          (idx n))
                     ;; Ignoring `unspecified' values when loading from previous
                     ;; frame. Those values might came from dead slots in stack
                     ;; which were overwritten by gc. See `scm_i_vm_mark_stack'
                     ;; in "libguile/vm.c".
                     ;;
                     ;; XXX: Add tests to check that this strategy works with
                     ;; explicitly given `unspecified' values.
                     ;;
                     (if (eq? type &unspecified)
                         (lp vars)
                         (with-frame-ref lp vars var type idx))))
                  (else
                   (lp vars))))
                (()
                 (next)))))))
    `(let ((,vdl #f))
       (let ((,vra #f))
         (let ((,vdst ,vsrc))
           ,(if (<= (current-fp-offset) 0)
                (next)
                `(let ((_ ,(take-snapshot! ip 0)))
                   ,(load-from-previous-frame))))))))

(define-ir (receive-values proc allow-extra? nvalues)
  (escape #f))

(define-interrupt-ir (return-values nlocals)
  (let ((snapshot (take-snapshot! ip 0)))
    (pop-past-frame! past-frame)
    `(let ((_ ,snapshot))
       ,(if (< (current-fp-offset) 0)
            (next)
            `(let ((_ (%return ,ra)))
               ,(next))))))


;;; *** Specialized call stubs

;; XXX: subr-call
;; XXX: foreign-call
;; XXX: continuation-call
;; XXX: compose-continuation
;; XXX: tail-apply
;; XXX: call/cc
;; XXX: abort
;; XXX: builtin-ref


;;; *** Function prologues

;; XXX: br-if-nargs-ne
;; XXX: br-if-nargs-lt
;; XXX; br-if-nargs-gt
;; XXX: assert-nargs-ee
;; XXX: assert-nargs-ge
;; XXX: assert-nargs-le
;; XXX: alloc-frame
;; XXX: reset-frame

(define-ir (assert-nargs-ee/locals expected nlocals)
  (let* ((stack-size (vector-length locals))
         (undefined (pointer->scm (make-pointer #x904))))
    (let lp ((n nlocals))
      (if (< 0 n)
          `(let ((,(var-ref (- n 1)) ,undefined))
             ,(lp (- n 1)))
          (next)))))

;; XXX: br-if-npos-gt
;; XXX: bind-kw-args
;; XXX: bind-rest


;;; *** Branching instructions

(define-ir (br (const offset))
  (next))

(define-ir (br-if-= (local a) (local b) (const invert?) (const offset))
  (let* ((ra (local-ref a))
         (rb (local-ref b))
         (va (var-ref a))
         (vb (var-ref b))
         (dest (if (= ra rb)
                   (if invert? offset br-op-size)
                   (if invert? br-op-size offset))))
    (cond
     ((and (fixnum? ra) (fixnum? rb))
      `(let ((_ ,(take-snapshot! ip dest)))
         (let ((_ ,(if (= ra rb) `(%eq ,va ,vb) `(%ne ,va ,vb))))
           ,(next))))
     (else
      (debug 3 "*** ir:convert = ~a ~a~%" ra rb)
      (escape #f)))))

(define-ir (br-if-< (local a) (local b) (const invert?) (const offset))
  (let* ((ra (local-ref a))
         (rb (local-ref b))
         (va (var-ref a))
         (vb (var-ref b))
         (dest (if (< ra rb)
                   (if invert? offset br-op-size)
                   (if invert? br-op-size offset))))
    (cond
     ((and (fixnum? ra) (fixnum? rb))
      `(let ((_ ,(take-snapshot! ip dest)))
         (let ((_ ,(if (< ra rb) `(%lt ,va ,vb) `(%ge ,va ,vb))))
           ,(next))))
     ((and (flonum? ra) (flonum? rb))
      `(let ((_ ,(take-snapshot! ip dest)))
         (let ((_ ,(if (< ra rb) `(%flt ,va ,vb) `(%fge ,va ,vb))))
           ,(next))))
     (else
      (debug 3 "ir:convert < ~a ~a~%" ra rb)
      (escape #f)))))

;; XXX: br-if-true

;; XXX: If `br-if-null' was commented out, the workaround in "tjit.scm" for
;; avoiding the compilation of traces in "system/vm/linker.scm" could be
;; removed. So this IR should relate to the cause of segfault in linker.scm
;; somehow.
(define-ir (br-if-null (local test) (const invert) (const offset))
  (let* ((rtest
          ;; XXX: Workaround for out-of-range error from `vector-ref' when
          ;; invoking REPL with `--tjit-dump=j' option.
          (catch #t
            (lambda ()
              (local-ref test))
            (lambda msgs
              (debug 0 "XXX: br-if-null: ~a~%" msgs)
              (escape #f))))
         (vtest (var-ref test))
         (dest (if (null? rtest)
                   (if invert offset 2)
                   (if invert 2 offset))))
    `(let ((_ ,(take-snapshot! ip dest)))
       (let ((_ ,(if (null? rtest) `(%eq ,vtest ()) `(%ne ,vtest ()))))
         ,(next)))))

;; XXX: br-if-nil
;; XXX: br-if-pair
;; XXX: br-if-struct
;; XXX: br-if-char
;; XXX: br-if-tc7
;; XXX: br-if-eq
;; XXX: br-if-eqv
;; XXX: br-if-equal
;; XXX: br-if-logtest
;; XXX: br-if-<=


;;; *** Lexical binding instructions

(define-ir (mov (local dst) (local src))
  (let ((vdst (var-ref dst))
        (vsrc (var-ref src)))
    `(let ((,vdst ,vsrc))
       ,(next))))

;; XXX: long-mov
;; XXX: box

;; XXX: Reconsider how to manage `box', `box-ref', and `box-set!'.
;; Boxing back tagged value every time will make the loop slow, need
;; more analysis when the storing could be removed from native code loop
;; and delayed to side exit code.
;;
;; XXX: Add test for nested boxes.

(define-ir (box-ref (local dst) (local src))
  (let ((vdst (var-ref dst))
        (vsrc (var-ref src))
        (rsrc (and (< src (vector-length locals))
                   (variable-ref (local-ref src)))))
    `(let ((,vdst (%cref ,vsrc 1)))
       ,(with-unboxing next rsrc vdst))))

(define-ir (box-set! (local dst) (local src))
  (let* ((vdst (var-ref dst))
         (vsrc (var-ref src))
         (rdst (and (< dst (vector-length locals))
                    (variable-ref (local-ref dst))))
         (r0 (make-tmpvar 0))
         (emit-next (lambda (tmp)
                      `(let ((_ (%cset ,vdst 1 ,tmp)))
                         ,(next)))))
    (with-boxing emit-next rdst vsrc r0)))

;; XXX: make-closure
;; XXX: free-ref
;; XXX: free-set!


;;; *** Immediates and statically allocated non-immediates

(define-ir (make-short-immediate (local dst) (const low-bits))
  ;; XXX: `make-short-immediate' could be used for other value than small
  ;; integer, e.g: '(). Check type from value of `low-bits' and choose
  ;; the type appropriately.
  `(let ((,(var-ref dst) ,(ash low-bits -2)))
     ,(next)))

(define-ir (make-long-immediate (local dst) (const low-bits))
  `(let ((,(var-ref dst) ,(ash low-bits -2)))
     ,(next)))

(define-ir (make-long-long-immediate (local dst)
                                     (const high-bits)
                                     (const low-bits))
  `(let ((,(var-ref dst) ,(ash (logior (ash high-bits 32) low-bits) -2)))
     ,(next)))

;; XXX: make-non-immediate

(define-ir (static-ref (local dst) (const offset))
  ;; XXX: Needs type check.
  `(let ((,(var-ref dst) ,(dereference-scm (+ ip (* 4 offset)))))
     ,(next)))

;; XXX: static-set!
;; XXX: static-patch!


;;; *** Mutable top-level bindings

;; XXX: current-module
;; XXX: resolve
;; XXX: define!

(define-ir (toplevel-box (local dst)
                         (const var-offset)
                         (const mod-offset)
                         (const sym-offset)
                         (const bound?))
  (let ((vdst (var-ref dst))
        (src (pointer-address
              (scm->pointer
               (dereference-scm (+ ip (* var-offset 4)))))))
    `(let ((,vdst ,src))
       ,(next))))

;; XXX: module-box


;;; *** The dynamic environment

;; XXX: prompt
;; XXX: wind
;; XXX: unwind
;; XXX: push-fluid
;; XXX: pop-fluid
;; XXX: fluid-ref
;; XXX: fluid-set


;;; *** Strings, symbols, and keywords

;; XXX: string-length
;; XXX: string-ref
;; XXX: string->number
;; XXX: string->symbol
;; XXX: symbol->keyword


;;; *** Pairs

;; Using dedicated IR for `cons'. Uses C function `scm_inline_cons', which
;; expects current thread as first argument. The value of current thread is not
;; stored in frame but in non-volatile register, and currently there is no way
;; to tell the register value as a variable from IR to assembler.
(define-interrupt-ir (cons (local dst) (local x) (local y))
  (let* ((vdst (var-ref dst))
         (vx (var-ref x))
         (vy (var-ref y))
         (r0 (make-tmpvar 0))
         (r1 (make-tmpvar 1))
         (lx (local-ref x))
         (ly (local-ref y))
         (emit-cons (lambda (a)
                      (lambda (b)
                        `(let ((,vdst (%cons ,a ,b)))
                           ,(next)))))
         (emit-y (lambda (a)
                   (with-boxing (emit-cons a) ly vy r1)))
         (emit-x (lambda ()
                   (with-boxing emit-y lx vx r0))))
    (emit-x)))

(define-ir (car (local dst) (local src))
  (let ((rdst (local-ref dst))
        (rsrc (local-ref src))
        (vdst (var-ref dst))
        (vsrc (var-ref src)))
    (when (not (pair? rsrc))
      (debug 1 "XXX: car ~a ~a~%" rdst rsrc)
      (escape #f))
    `(let ((,vdst (%cref ,vsrc 0)))
       ,(let ((rcar (car rsrc)))
          (with-unboxing next rcar vdst)))))

(define-ir (cdr (local dst) (local src))
  (let ((rdst (local-ref dst))
        (rsrc (local-ref src))
        (vdst (var-ref dst))
        (vsrc (var-ref src)))
    (when (not (pair? rsrc))
      (debug 1 "XXX: cdr ~a ~a~%" rdst rsrc)
      (escape #f))
    `(let ((,vdst (%cref ,vsrc 1)))
       ,(let ((rcdr (cdr rsrc)))
          (with-unboxing next rcdr vdst)))))

;; XXX: set-car!
;; XXX: set-cdr!


;;; *** Numeric operations

(define-ir (add (local dst) (local a) (local b))
  (let ((rdst (local-ref dst))
        (ra (local-ref a))
        (rb (local-ref b))
        (vdst (var-ref dst))
        (va (var-ref a))
        (vb (var-ref b)))
    (cond
     ((and (fixnum? ra) (fixnum? rb))
      `(let ((,vdst (%add ,va ,vb)))
         ,(next)))
     ((and (flonum? ra) (flonum? rb))
      `(let ((,vdst (%fadd ,va ,vb)))
         ,(next)))
     (else
      (debug 3 "XXX: add ~a ~a ~a~%" rdst ra rb)
      (escape #f)))))

(define-ir (add1 (local dst) (local src))
  (let ((rdst (local-ref dst))
        (rsrc (local-ref src))
        (vdst (var-ref dst))
        (vsrc (var-ref src)))
    (cond
     ((fixnum? rsrc)
      `(let ((,vdst (%add ,vsrc 1)))
         ,(next)))
     (else
      (debug 3 "XXX: add1 ~a ~a" rdst rsrc)
      (escape #f)))))

(define-ir (sub (local dst) (local a) (local b))
  (let ((rdst (local-ref dst))
        (ra (local-ref a))
        (rb (local-ref b))
        (vdst (var-ref dst))
        (va (var-ref a))
        (vb (var-ref b)))
    (cond
     ((and (fixnum? ra) (fixnum? rb))
      `(let ((,vdst (%sub ,va ,vb)))
         ,(next)))
     ((and (flonum? ra) (flonum? rb))
      `(let ((,vdst (%fsub ,va ,vb)))
         ,(next)))
     (else
      (debug 3 "XXX: sub ~a ~a ~a~%" rdst ra rb)
      (escape #f)))))

(define-ir (sub1 (local dst) (local src))
  (let ((rdst (local-ref dst))
        (rsrc (local-ref src))
        (vdst (var-ref dst))
        (vsrc (var-ref src)))
    (cond
     ((fixnum? rsrc)
      `(let ((,vdst (%sub ,vsrc 1)))
         ,(next)))
     (else
      (debug 3 "XXX: sub1 ~a ~a~%" rdst rsrc)
      (escape #f)))))

(define-ir (mul (local dst) (local a) (local b))
  (let ((rdst (local-ref dst))
        (ra (local-ref a))
        (rb (local-ref b))
        (vdst (var-ref dst))
        (va (var-ref a))
        (vb (var-ref b)))
    (cond
     ((and (flonum? ra) (flonum? rb))
      `(let ((,vdst (%fmul ,va ,vb)))
         ,(next)))
     (else
      (debug 3 "XXX: mul ~a ~a ~a~%" rdst ra rb)
      (escape #f)))))

;; XXX: div
;; XXX: quo
;; XXX: rem

(define-ir (mod (local dst) (local a) (local b))
  (let ((rdst (local-ref dst))
        (ra (local-ref a))
        (rb (local-ref b))
        (vdst (var-ref dst))
        (va (var-ref a))
        (vb (var-ref b)))
    (cond
     ((and (fixnum? ra) (fixnum? rb))
      `(let ((,vdst (%mod ,va ,vb)))
         ,(next)))
     (else
      (debug 3 "XXX: mod ~a ~a ~a~%" rdst ra rb)
      (escape #f)))))

;; XXX: ash
;; XXX: logand
;; XXX: logior
;; XXX: logxor
;; XXX: make-vector
;; XXX: make-vector/immediate
;; XXX: vector-length
;; XXX: vector-ref
;; XXX: vector-ref/immediate
;; XXX: vector-set!

(define-ir (vector-set! dst idx src)
 (let ((rdst (local-ref dst))
       (rsrc (local-ref src))
       (ridx (local-ref idx))
       (vdst (var-ref dst))
       (vsrc (var-ref src))
       (vidx (var-ref idx)))
   (cond
    ((and (vector? rsrc) (fixnum? ridx))
     `(let ((_ (%vector-set! ,vdst ,vsrc ,vidx)))
        ,(next)))
    (else
     (debug 3 "*** ir.scm:convert: NYI vector-set! ~a ~a ~a~%"
            rdst rsrc ridx)
     (escape #f)))))

;; XXX: vector-set!/immediate


;;; *** Structs and GOOPS

;; XXX: struct-vtable
;; XXX: allocate-struct
;; XXX: struct-ref
;; XXX: struct-set!
;; XXX: allocate-struct/immediate
;; XXX: struct-ref/immediate
;; XXX: struct-set!/immediate
;; XXX: class-of


;;; *** Arrays, packed uniform arrays, and bytevectors

;; XXX: load-typed-array
;; XXX: make-array
;; XXX: bv-u8-ref
;; XXX: bv-s8-ref
;; XXX: bv-u16-ref
;; XXX: bv-s16-ref
;; XXX: bv-u32-ref
;; XXX: bv-s32-ref
;; XXX: bv-u64-ref
;; XXX: bv-s64-ref
;; XXX: bv-f32-ref
;; XXX: bv-f64-ref
;; XXX: bv-u8-set!
;; XXX: bv-s8-set!
;; XXX: bv-u16-set!
;; XXX: bv-s16-set!
;; XXX: bv-u32-set!
;; XXX: bv-s32-set!
;; XXX: bv-u64-set!
;; XXX: bv-s64-set!
;; XXX: bv-f32-set!
;; XXX: bv-f64-set!

;; XXX: scm->f64
;; XXX: f64->scm
;; XXX: fadd
;; XXX: fsub
;; XXX: fmul
;; XXX: fdiv


;;;
;;; Local accumulator
;;;

(define (accumulate-locals sp-offset fp-offset traces)
  ;; Makes past-frame with locals in lower frames.
  ;;
  ;; Lower frame data is saved at the time of accumulation. Otherwise, if
  ;; one of the guard operation appeared soon after bytecode sequence
  ;; `return' and `receive', snapshot does not know the value of locals in
  ;; lower frame. When recorded bytecode contains `return' before `call',
  ;; snapshot will recover a frame lower than the one used to enter the
  ;; native call.
  ;;
  ;; The stack grows down.
  ;;
  (let ((ret (make-hash-table))
        (sp-offsets '())
        (fp-offsets '()))
    (define-syntax-rule (push-sp-offset! n)
      (set! sp-offset (- sp-offset n)))
    (define-syntax-rule (pop-sp-offset! n)
      (set! sp-offset (+ sp-offset n)))
    (define-syntax-rule (push-fp-offset! n)
      (set! fp-offset (- fp-offset n)))
    (define-syntax-rule (pop-fp-offset! n)
      (set! fp-offset (+ fp-offset n)))
    (define-syntax-rule (save-sp-offset!)
      (set! sp-offsets (cons sp-offset sp-offsets)))
    (define-syntax-rule (save-fp-offset!)
      (set! fp-offsets (cons fp-offset fp-offsets)))
    (define-syntax add!
      (syntax-rules ()
        ((_ st i ...)
         (begin
           (hashq-set! st (+ i sp-offset) #t) ...
           st))))
    (define (nyi st op)
      (debug 1 "ir:accumulate-locals: NYI ~a~%" op)
      st)
    ;; Lookup accumulating procedure stored in *local-accumulator* and apply
    ;; the procedure when found.
    ;;
    ;; VM operations which moves frame pointer are not stored in
    ;; *local-accumulator* and treated specially.
    ;;
    ;; `return' will accumulate indices of proc, returned value, VM frame
    ;; dynamic link, and VM frame return address.
    ;;
    ;; `call' and `call-label' will accumulate indices of proc, VM frame
    ;; dynamic link, and VM frame return address, then push the local sp-offset.
    ;;
    ;; `receive' and `receive-value' will pop the local sp-offset before
    ;; accumulating index of the local containing the callee procedure.
    ;;
    (define (acc-one st op locals)
      (cond
       ((hashq-ref *local-accumulator* (car op))
        => (lambda (proc)
             (let ((st (apply proc st sp-offset (cdr op))))
               (save-sp-offset!)
               (save-fp-offset!)
               st)))
       (else
        (match op
          (('call proc nlocals)
           (let* ((stack-size (vector-length locals))
                  (sp-proc (- stack-size proc 1)))
             (add! st sp-proc (+ sp-proc 1) (+ sp-proc 2))
             (save-sp-offset!)
             (save-fp-offset!)
             (push-fp-offset! proc)
             (push-sp-offset! (- (+ proc nlocals) stack-size))
             st))
          (('call-label proc nlocals _)
           (let ((stack-size (vector-length locals)))
             (add! st proc (- proc 1) (- proc 2))
             (save-sp-offset!)
             (save-fp-offset!)
             (push-fp-offset! proc)
             (push-sp-offset! (- (+ proc nlocals) stack-size))
             st))
          (('tail-call nlocals)
           (let ((stack-size (vector-length locals)))
             (add! st (- stack-size 1))
             (save-sp-offset!)
             (save-fp-offset!)
             st))
          (('receive dst proc nlocals)
           (let* ((stack-size (vector-length locals))
                  (fp (- stack-size proc)))
             (add! st (- stack-size dst 1) fp (+ fp 1) (+ fp 2))
             (save-sp-offset!)
             (pop-sp-offset! (- stack-size nlocals))
             (pop-fp-offset! proc)
             (save-fp-offset!)
             st))
          ;; XXX: NYI receive-values
          ;; (('receive-values proc _ _)
          ;;  (add! st proc))
          (('return-values nlocals)
           (let ((stack-size (vector-length locals)))
             (let lp ((n nlocals))
               (when (<= 2 n)
                 (add! st (- stack-size n))
                 (lp (- n 1))))
             (save-sp-offset!)
             (pop-sp-offset! (- stack-size nlocals))
             (save-fp-offset!)
             st))
          (('assert-nargs-ee/locals expected nlocals)
           (push-sp-offset! nlocals)
           (let lp ((n nlocals))
             (when (< 0 n)
               (add! st (- n 1))
               (lp (- n 1))))
           (save-sp-offset!)
           (save-fp-offset!)
           st)
          (_
           (nyi st (car op)))))))
    (define (acc st traces)
      (match traces
        (((op _ _ locals) . rest)
         (acc (acc-one st op locals) rest))
        (()
         st)))
    (let ((local-indices (sort (hash-fold (lambda (k v acc)
                                            (cons k acc))
                                          '()
                                          (acc ret traces))
                               >))
          (sp-offsets/vec (list->vector (reverse! sp-offsets)))
          (fp-offsets/vec (list->vector (reverse! fp-offsets))))
      (debug 1 ";;; local-indices: ~a~%" local-indices)
      (make-past-frame '()
                       '()
                       sp-offset
                       #()
                       local-indices
                       sp-offsets/vec
                       fp-offsets/vec))))
