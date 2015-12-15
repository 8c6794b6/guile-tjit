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
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
  #:export (make-ir
            <ir>
            ir-snapshots set-ir-snapshots!
            ir-snapshot-id set-ir-snapshot-id!
            ir-min-sp-offset set-ir-min-sp-offset!
            ir-max-sp-offset set-ir-max-sp-offset!
            ir-bytecode-index set-ir-bytecode-index!
            ir-vars
            ir-outline
            ir-handle-interrupts?

            make-var
            make-vars
            get-max-sp-offset
            get-initial-sp-offset
            get-initial-fp-offset
            take-snapshot
            with-frame-ref
            *ir-procedures*
            *element-type-scanners*
            *index-scanners*))

;;;
;;; Auxiliary, exported
;;;

(define-record-type <ir>
  (make-ir snapshots snapshot-id parent-snapshot vars
           min-sp-offset max-sp-offset bytecode-index
           outline handle-interrupts?)
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
  (bytecode-index ir-bytecode-index set-ir-bytecode-index!)

  ;; Past frame.
  (outline ir-outline)

  ;; Flag for handle interrupts
  (handle-interrupts? ir-handle-interrupts? set-ir-handle-interrupts!))


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

(define (get-initial-sp-offset parent-snapshot)
  ;; Initial offset of root trace is constantly 0. Initial offset of side
  ;; trace is where parent trace left, using offset value from SNAPSHOT.
  (match parent-snapshot
    (($ $snapshot _ sp-offset) sp-offset)
    (_ 0)))

(define (get-initial-fp-offset parent-snapshot)
  ;; Initial offset of root trace is constantly 0. Initial offset of side
  ;; trace is where parent trace left, using offset value from SNAPSHOT.
  (match parent-snapshot
    (($ $snapshot _ _ fp-offset) fp-offset)
    (_ 0)))

(define (take-snapshot ip dst-offset locals vars id
                       sp-offset fp-offset min-sp-offset max-sp-offset
                       parent-snapshot outline)
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
                                  nlocals
                                  locals
                                  parent-snapshot
                                  indices
                                  outline
                                  dst-ip)))
    (values `(%snap ,id ,@args) snapshot)))

(define-syntax-rule (with-frame-ref next args var type idx)
  (cond
   ((not type)
    (debug 1 "XXX: with-frame-ref: var=~a type=~a~%" var type)
    `(let ((,var #f))
       ,(next args)))
   ((dynamic-link? type)
    `(let ((,var ,(dynamic-link-offset type)))
       ,(next args)))
   ((return-address? type)
    `(let ((,var ,(pointer-address (return-address-ip type))))
       ,(next args)))
   ((or (= type &flonum)
        (= type &f64))
    `(let ((,var (%fref/f ,idx ,type)))
       ,(next args)))
   (else
    `(let ((,var (%fref ,idx ,type)))
       ,(next args)))))

(define *ir-procedures*
  (make-hash-table 255))


;;;
;;; Auxiliary, internal
;;;

(define *index-scanners*
  (make-hash-table 255))

(define *element-type-scanners*
  (make-hash-table 255))

(define-syntax define-ir-syntax-parameters
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define-syntax-parameter name
         (lambda (x)
           'name "uninitialized" x))
       ...))))

(define-ir-syntax-parameters ir ip ra dl locals next)

(define-syntax put-index!
  (syntax-rules ()
    ((_ ol offset arg)
     (let ((indices (assq-set! (outline-local-indices ol)
                               (+ arg offset) #t)))
       (set-outline-local-indices! ol indices)))))

(define-syntax put-element-type!
  (syntax-rules ()
    ((_ ol offset arg type)
     (let ((types (assq-set! (outline-types ol) (+ arg offset) type)))
       (set-outline-types! ol types)))))

(define-syntax gen-put-index
  (syntax-rules (const)
    ((_ ol sp-offset)
     ol)
    ((_ ol sp-offset (const arg) . rest)
     (gen-put-index ol sp-offset . rest))
    ((_ ol sp-offset (other arg) . rest)
     (begin
       (put-index! ol sp-offset arg)
       (gen-put-index ol sp-offset . rest)))))

(define-syntax gen-put-element-type
  (syntax-rules (any scm u64 f64 const)
    ((_ ol sp-offset)
     ol)
    ((_ ol sp-offset (scm arg) . rest)
     (begin
       (put-element-type! ol sp-offset arg 'scm)
       (gen-put-element-type ol sp-offset . rest)))
    ((_ ol sp-offset (u64 arg) . rest)
     (begin
       (put-element-type! ol sp-offset arg 'u64)
       (gen-put-element-type ol sp-offset . rest)))
    ((_ ol sp-offset (f64 arg) . rest)
     (begin
       (put-element-type! ol sp-offset arg 'f64)
       (gen-put-element-type ol sp-offset . rest)))
    ((_ ol sp-offset (any arg) . rest)
     (gen-put-element-type ol sp-offset . rest))
    ((_ ol sp-offset (const arg) . rest)
     (gen-put-element-type ol sp-offset . rest))))

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
     (let ((index-proc (lambda (ol sp-offset arg ...)
                         (gen-put-index ol sp-offset (flag arg) ...)))
           (type-proc (lambda (ol sp-offset arg ...)
                        (gen-put-element-type ol sp-offset (flag arg) ...))))
       (hashq-set! *index-scanners* 'name index-proc)
       (hashq-set! *element-type-scanners* 'name type-proc)
       (define-ir (name arg ...) . body)))
    ((_ (name arg ...) . body)
     (let ((proc
            (lambda (%ir %next %ip %ra %dl %locals arg ...)
              (syntax-parameterize
                  ((ir (identifier-syntax %ir))
                   (next (identifier-syntax %next))
                   (ip (identifier-syntax %ip))
                   (ra (identifier-syntax %ra))
                   (dl (identifier-syntax %dl))
                   (locals (identifier-syntax %locals)))
                . body))))
       (hashq-set! *ir-procedures* 'name proc)))))

(define-syntax define-interrupt-ir
  (syntax-rules ()
    ((_ names-and-args . body)
     (define-ir names-and-args
       (begin
         (set-ir-handle-interrupts! ir #t)
         . body)))))

(define-syntax-rule (to-fixnum scm)
  `(%rsh ,scm 2))

(define-syntax-rule (to-double scm)
  `(%cref/f ,scm 2))

(define-syntax-rule (dereference-scm addr)
  (pointer->scm (dereference-pointer (make-pointer addr))))

(define-syntax br-op-size
  (identifier-syntax 3))

(define-syntax-rule (current-sp-offset)
  (vector-ref (outline-sp-offsets (ir-outline ir))
              (ir-bytecode-index ir)))

(define-syntax-rule (current-fp-offset)
  (vector-ref (outline-fp-offsets (ir-outline ir))
              (ir-bytecode-index ir)))

(define-syntax-rule (local-ref n)
  (let ((t (outline-type-ref (ir-outline ir)
                                (+ n (current-sp-offset)))))
    (stack-element locals n t)))

(define-syntax-rule (var-ref n)
  (assq-ref (ir-vars ir) (+ n (current-sp-offset))))

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
                               (ir-outline ir))))
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
    ;; XXX: Save volatile registers.
    `(let ((,tmp (%from-double ,var)))
       ,(next tmp)))
   ((pair? val)
    (next var))
   (else
    (nyi "with-boxing: ~s ~s ~s" val var tmp))))

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
    (nyi "with-unboxing: ~a ~a" val var))))

(define-syntax-rule (expand-stack nlocals)
  (expand-outline (ir-outline ir) (current-sp-offset) nlocals))


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
         (dst-ptr (make-pointer (+ ip (* 2 4))))
         (rra (cons (+ sp-offset fp) (make-return-address dst-ptr)))
         (rdl (cons (+ sp-offset fp 1) (make-dynamic-link proc)))
         (vra (var-ref fp))
         (vdl (var-ref (+ fp 1)))
         (vproc (var-ref (- fp 1)))
         (rproc (local-ref (- fp 1)))
         (snapshot (take-snapshot! ip 0)))
    (push-outline! (ir-outline ir) rdl rra sp-offset locals)
    `(let ((_ ,snapshot))
       (let ((_ (%eq ,vproc ,(pointer-address (scm->pointer rproc)))))
         ,(if (< 0 (current-fp-offset))
              `(let ((_ (%pcall ,proc)))
                 ,(next))
              (next))))))

;; XXX: call-label
(define-ir (call-label proc nlocals label)
  (nyi "call-label"))

(define-ir (tail-call nlocals)
  (let* ((stack-size (vector-length locals))
         (proc-index (- stack-size 1))
         (vproc (var-ref proc-index))
         (rproc (local-ref proc-index)))
    `(let ((_ (%eq ,vproc ,(pointer-address (scm->pointer rproc)))))
       ,(next))))

(define-ir (tail-call-label nlocals label)
  (next))


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
         (load-previous-frame
          (lambda ()
            (let lp ((vars (reverse (ir-vars ir))))
              (match vars
                (((n . var) . vars)
                 (cond
                  ((eq? var vdst)
                   (lp vars))
                  ((< min-local-index n max-local-index)
                   (let* ((i (- n sp-offset))
                          (elem (outline-type-ref (ir-outline ir) n))
                          (type (cond
                                 ((eq? 'f64 elem) &f64)
                                 ((eq? 'u64 elem) &u64)
                                 ((eq? 's64 elem) &s64)
                                 ((eq? 'scm elem)
                                  (type-of (stack-element locals i elem)))
                                 (else
                                  (tjitc-error 'receive "unknown type ~s"
                                               elem)))))
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
                         (with-frame-ref lp vars var type n))))
                  (else
                   (lp vars))))
                (()
                 (next)))))))
    `(let ((,vdst ,vsrc))
       ,(if (<= (current-fp-offset) 0)
            (next)
            `(let ((_ ,(take-snapshot! ip 0)))
               ,(load-previous-frame))))))

;; XXX: receive-values
(define-ir (receive-values proc allow-extra? nvalues)
  (nyi "receive-values"))

(define-interrupt-ir (return-values nlocals)
  (let ((ra/val (make-return-address (make-pointer ra)))
        (dl/val (make-dynamic-link dl))
        (stack-size (vector-length locals)))
    (set-outline-previous-dl-and-ra! (ir-outline ir) stack-size
                                        ra/val dl/val))
  (let ((snapshot (take-snapshot! ip 0)))
    (pop-outline! (ir-outline ir) (current-sp-offset) locals)
    `(let ((_ ,snapshot))
       ,(if (< (current-fp-offset) 0)
            (next)
            (let* ((stack-size (vector-length locals))
                   (vra (var-ref stack-size))
                   (vdl (var-ref (+ stack-size 1))))
              `(let ((_ (%return ,ra)))
                 (let ((,vra #f))
                   (let ((,vdl #f))
                     ,(next)))))))))


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
    (expand-stack nlocals)
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
  ;; Nothing to emit for br.
  (next))

;; XXX: br-if-true

;; XXX: If `br-if-null' was commented out, the workaround in "tjit.scm" for
;; avoiding the compilation of traces in "system/vm/linker.scm" could be
;; removed. So this IR should relate to the cause of segfault in linker.scm
;; somehow.
(define-ir (br-if-null (scm test) (const invert) (const offset))
  (let* ((rtest (local-ref test))
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
;; XXX: br-if-logtest

(define-syntax define-br-binary-body
  (syntax-rules ()
    ((_ name a b invert? offset test ra rb va vb dest . body)
     (let* ((ra (local-ref a))
            (rb (local-ref b))
            (va (var-ref a))
            (vb (var-ref b))
            (dest (if (and (number? ra)
                           (number? rb))
                      (if (test ra rb)
                          (if invert? offset br-op-size)
                          (if invert? br-op-size offset))
                      (tjitc-error "~s: got ~s ~s" 'name ra rb))))
       . body))))

(define-syntax define-br-binary
  (syntax-rules ()
    ((_  name op-scm op-fx-t op-fx-f op-fl-t op-fl-f)
     (define-ir (name (scm a) (scm b) (const invert?) (const offset))
       (define-br-binary-body name a b invert? offset op-scm ra rb va vb dest
         (cond
          ((and (fixnum? ra) (fixnum? rb))
           `(let ((_ ,(take-snapshot! ip dest)))
              (let ((_ ,(if (op-scm ra rb)
                            `(op-fx-t ,va ,vb)
                            `(op-fx-f ,va ,vb))))
                ,(next))))
          ((and (flonum? ra) (flonum? rb))
           `(let ((_ ,(take-snapshot! ip dest)))
              (let ((_ ,(if (op-scm ra rb)
                            `(op-fl-t ,va ,vb)
                            `(op-fl-f ,va ,vb))))
                ,(next))))
          ;; XXX: Delegate other types to `scm_num_eq_p' function, e.g.: SCM
          ;; bignum, complex, ... etc.
          (else
           (nyi "~s: ~a ~a~%" 'name ra rb))))))))

(define-br-binary br-if-= = %eq %ne %feq %fne)
(define-br-binary br-if-< < %lt %ge %flt %fge)
(define-br-binary br-if-<= <= %le %gt %fle %fgt)

;;; *** Lexical binding instructions

;; XXX: Assuming both `dst' and `src' have `scm' stack element type. If not,
;; stack element type resolution may return incorrect result. To properly
;; resolve stack element types, may need to traverse bytecode operations
;; backward.
(define-ir (mov (scm dst) (scm src))
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

;; XXX: long-mov
;; XXX: long-fmov
;; XXX: box

;; XXX: Reconsider how to manage `box', `box-ref', and `box-set!'.
;; Boxing back tagged value every time will make the loop slow, need
;; more analysis when the storing could be removed from native code loop
;; and delayed to side exit code.
;;
;; XXX: Add test for nested boxes.
;; XXX: Add test for box contents not being other type than scm (no u64, no f64).

(define-ir (box-ref (scm dst) (scm src))
  (let ((vdst (var-ref dst))
        (vsrc (var-ref src))
        (rsrc (and (< src (vector-length locals))
                   (let ((var (local-ref src)))
                     (if (variable? var)
                         (variable-ref var)
                         (tjitc-error 'box-ref "got ~s" var))))))
    `(let ((,vdst (%cref ,vsrc 1)))
       ,(with-unboxing next rsrc vdst))))

(define-ir (box-set! (scm dst) (scm src))
  (let* ((vdst (var-ref dst))
         (vsrc (var-ref src))
         (rdst (and (< dst (vector-length locals))
                    (let ((var (local-ref dst)))
                      (if (variable? var)
                          (variable-ref var)
                          (tjitc-error 'box-set! "got ~s~%" var)))))
         (r0 (make-tmpvar 0))
         (emit-next (lambda (tmp)
                      `(let ((_ (%cset ,vdst 1 ,tmp)))
                         ,(next)))))
    (with-boxing emit-next rdst vsrc r0)))

;; XXX: make-closure
;; XXX: free-ref
;; XXX: free-set!


;;; *** Immediates and statically allocated non-immediates

(define-ir (make-short-immediate (scm dst) (const low-bits))
  ;; XXX: `make-short-immediate' could be used for other value than small
  ;; integer, e.g: '(). Check type from value of `low-bits' and choose
  ;; the type appropriately.
  `(let ((,(var-ref dst) ,(ash low-bits -2)))
     ,(next)))

(define-ir (make-long-immediate (scm dst) (const low-bits))
  `(let ((,(var-ref dst) ,(ash low-bits -2)))
     ,(next)))

(define-ir (make-long-long-immediate (scm dst)
                                     (const high-bits)
                                     (const low-bits))
  `(let ((,(var-ref dst) ,(ash (logior (ash high-bits 32) low-bits) -2)))
     ,(next)))

;; XXX: make-non-immediate

(define-ir (static-ref (scm dst) (const offset))
  ;; XXX: Needs type check.
  `(let ((,(var-ref dst) ,(dereference-scm (+ ip (* 4 offset)))))
     ,(next)))

;; XXX: static-set!
;; XXX: static-patch!


;;; *** Mutable top-level bindings

;; XXX: current-module
;; XXX: resolve
;; XXX: define!

(define-ir (toplevel-box (scm dst) (const var-offset) (const mod-offset)
                         (const sym-offset) (const bound?))
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
(define-interrupt-ir (cons (scm dst) (scm x) (scm y))
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

(define-ir (car (scm dst) (scm src))
  (let ((rdst (local-ref dst))
        (rsrc (local-ref src))
        (vdst (var-ref dst))
        (vsrc (var-ref src)))
    (when (not (pair? rsrc))
      (tjitc-error 'car "~a ~a~%" rdst rsrc))
    `(let ((,vdst (%cref ,vsrc 0)))
       ,(let ((rcar (car rsrc)))
          (with-unboxing next rcar vdst)))))

(define-ir (cdr (scm dst) (scm src))
  (let ((rdst (local-ref dst))
        (rsrc (local-ref src))
        (vdst (var-ref dst))
        (vsrc (var-ref src)))
    (when (not (pair? rsrc))
      (tjitc-error 'cdr "~a ~a~%" rdst rsrc))
    `(let ((,vdst (%cref ,vsrc 1)))
       ,(let ((rcdr (cdr rsrc)))
          (with-unboxing next rcdr vdst)))))

;; XXX: set-car!
;; XXX: set-cdr!


;;; *** Numeric operations

(define-syntax define-binary-arith-scm-scm
  (syntax-rules ()
    ((_ name op-fx op-fl)
     (define-ir (name (scm dst) (scm a) (scm b))
       (let ((ra (local-ref a))
             (rb (local-ref b))
             (vdst (var-ref dst))
             (va (var-ref a))
             (vb (var-ref b)))
         (cond
          ((and (fixnum? ra) (fixnum? rb))
           `(let ((,vdst (op-fx ,va ,vb)))
              ,(next)))
          ((and (flonum? ra) (flonum? rb))
           `(let ((,vdst (op-fl ,va ,vb)))
              ,(next)))
          (else
           (nyi "~s: ~a ~a ~a" 'name (local-ref dst) ra rb))))))))

(define-syntax define-binary-arith-scm-imm
  (syntax-rules ()
    ((_ name op-fx)
     (define-ir (name (scm dst) (scm src) (const imm))
       (let ((rsrc (local-ref src))
             (vdst (var-ref dst))
             (vsrc (var-ref src)))
         (cond
          ((fixnum? rsrc)
           `(let ((,vdst (op-fx ,vsrc ,imm)))
              ,(next)))
          (else
           (nyi "~s: ~a ~a" 'name (local-ref dst) rsrc))))))))

(define-binary-arith-scm-scm add %add %fadd)
(define-binary-arith-scm-imm add/immediate %add)
(define-binary-arith-scm-scm sub %sub %fsub)
(define-binary-arith-scm-imm sub/immediate %sub)

(define-ir (mul (scm dst) (scm a) (scm b))
  (let ((ra (local-ref a))
        (rb (local-ref b))
        (vdst (var-ref dst))
        (va (var-ref a))
        (vb (var-ref b)))
    (cond
     ((and (flonum? ra) (flonum? rb))
      `(let ((,vdst (%fmul ,va ,vb)))
         ,(next)))
     (else
      (nyi "mul: ~a ~a ~a" (local-ref dst) ra rb)))))

;; XXX: div
;; XXX: quo
;; XXX: rem

(define-ir (mod (scm dst) (scm a) (scm b))
  (let ((ra (local-ref a))
        (rb (local-ref b))
        (vdst (var-ref dst))
        (va (var-ref a))
        (vb (var-ref b)))
    (cond
     ((and (fixnum? ra) (fixnum? rb))
      `(let ((,vdst (%mod ,va ,vb)))
         ,(next)))
     (else
      (nyi "mod: ~a ~a ~a" (local-ref dst) ra rb)))))

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

(define-ir (scm->f64 (f64 dst) (scm src))
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

(define-ir (f64->scm (scm dst) (f64 src))
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

(define-syntax define-binary-arith-f64-f64
  (syntax-rules ()
    ((_ name op)
     (define-ir (name (f64 dst) (f64 a) (f64 b))
       `(let ((,(var-ref dst) (op ,(var-ref a) ,(var-ref b))))
          ,(next))))))

(define-binary-arith-f64-f64 fadd %fadd)
(define-binary-arith-f64-f64 fsub %fsub)
(define-binary-arith-f64-f64 fmul %fmul)
(define-binary-arith-f64-f64 fdiv %fdiv)

;; XXX: apply-non-program

(define-ir (scm->u64 (u64 dst) (scm src))
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

(define-ir (u64->scm (scm dst) (u64 src))
  `(let ((,(var-ref dst) ,(var-ref src)))
     ,(next)))

;; XXX: bv-length

(define-syntax define-br-binary-u64-u64
  (syntax-rules ()
    ((_ name op-scm op-fx-t op-fx-f)
     (define-ir (name (u64 a) (u64 b) (const invert?) (const offset))
       (define-br-binary-body name a b invert? offset op-scm ra rb va vb dest
         `(let ((_ ,(take-snapshot! ip dest)))
            (let ((_ ,(if (op-scm ra rb)
                          `(op-fx-t ,va ,vb)
                          `(op-fx-f ,va ,vb))))
              ,(next))))))))

(define-br-binary-u64-u64 br-if-u64-= = %eq %ne)
(define-br-binary-u64-u64 br-if-u64-< < %lt %ge)
(define-br-binary-u64-u64 br-if-u64-<= <= %le %gt)

;; XXX: uadd
;; XXX: usub
;; XXX: umul

(define-syntax define-binary-arith-u64-imm
  (syntax-rules ()
    ((_ name op)
     (define-ir (name (u64 dst) (u64 src) (const imm))
       `(let ((,(var-ref dst) (op ,(var-ref src) ,imm)))
          ,(next))))))

(define-binary-arith-u64-imm uadd/immediate %add)
(define-binary-arith-u64-imm usub/immediate %sub)

;; XXX: umul/immediate

;; XXX: load-f64

(define-ir (load-u64 (u64 dst) (const high-bits) (const low-bits))
  `(let ((,(var-ref dst) ,(logior (ash high-bits 32) low-bits)))
     ,(next)))

;; XXX: scm->s64
;; XXX: s64->scm
;; XXX: load-s64

;; XXX: current-thread

;; XXX: logsub

;; XXX: ulogand
;; XXX: ulogior
;; XXX: ulogsub
;; XXX: ursh
;; XXX: ulsh
;; XXX: scm->u64/truncate

;; XXX: ursh/immediate
;; XXX: ulsh/immediate

(define-syntax define-br-binary-u64-scm
  (syntax-rules ()
    ((_ name op-scm op-fx-t op-fx-f)
     (define-ir (name (u64 a) (scm b) (const invert?) (const offset))
       (define-br-binary-body name a b invert? offset op-scm ra rb va vb dest
        `(let ((_ ,(take-snapshot! ip dest)))
           (let ((_ ,(if (op-scm ra rb)
                         `(op-fx-t ,va ,vb)
                         `(op-fx-f ,va ,vb))))
             ,(next))))))))

(define-br-binary-u64-scm br-if-u64-=-scm = %eq %ne)
(define-br-binary-u64-scm br-if-u64-<-scm < %lt %ge)
(define-br-binary-u64-scm br-if-u64-<=scm <= %le %gt)
(define-br-binary-u64-scm br-if-u64->-scm > %gt %le)
(define-br-binary-u64-scm br-if-u64->=-scm >= %ge %lt)
