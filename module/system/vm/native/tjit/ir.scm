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
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
  #:export (make-var
            make-vars
            get-max-sp-offset
            take-snapshot
            with-frame-ref
            trace->ir
            accumulate-locals))

;;;
;;; Auxiliary, exported
;;;

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


;;;
;;; Auxiliary, internal
;;;

(define *ir-procedures*
  (make-hash-table))

(define *local-accumulator*
  (make-hash-table))

(define-syntax define-ir-syntax-parameters
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define-syntax-parameter name
         (lambda (x)
           'name "uninitialized" x))
       ...))))

(define-ir-syntax-parameters
  snapshots
  snapshot-id
  parent-snapshot
  past-frame
  locals
  vars
  min-sp-offset
  max-sp-offset
  ip
  ra
  handle-interrupts?
  bytecode-index
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
            (lambda (%snapshots
                     %snapshot-id %parent-snapshot %past-frame
                     %locals %vars %min-sp-offset %max-sp-offset
                     %ip %ra %handle-interrupts? %bytecode-index %next %escape
                     arg ...)
              (syntax-parameterize
                  ((snapshots (identifier-syntax %snapshots))
                   (snapshot-id (identifier-syntax %snapshot-id))
                   (parent-snapshot (identifier-syntax %parent-snapshot))
                   (past-frame (identifier-syntax %past-frame))
                   (locals (identifier-syntax %locals))
                   (vars (identifier-syntax %vars))
                   (min-sp-offset (identifier-syntax %min-sp-offset))
                   (max-sp-offset (identifier-syntax %max-sp-offset))
                   (ip (identifier-syntax %ip))
                   (ra (identifier-syntax %ra))
                   (bytecode-index (identifier-syntax %bytecode-index))
                   (handle-interrupts? (identifier-syntax %handle-interrupts?))
                   (next (identifier-syntax %next))
                   (escape (identifier-syntax %escape)))
                . body))))
       (hashq-set! *ir-procedures* 'name proc)))))

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

(define-syntax-rule (current-sp-offset)
  (vector-ref (past-frame-sp-offsets past-frame)
              (variable-ref bytecode-index)))

(define-syntax-rule (current-fp-offset)
  (vector-ref (past-frame-fp-offsets past-frame)
              (variable-ref bytecode-index)))

(define-syntax-rule (var-ref n)
  (assq-ref vars (+ n (current-sp-offset))))

(define-syntax-rule (take-snapshot! ip dst-offset)
  (let-values (((ret snapshot)
                (take-snapshot ip
                               dst-offset
                               locals
                               vars
                               (variable-ref snapshot-id)
                               (current-sp-offset)
                               (current-fp-offset)
                               (variable-ref min-sp-offset)
                               (variable-ref max-sp-offset)
                               parent-snapshot
                               past-frame)))
    (hashq-set! snapshots (variable-ref snapshot-id) snapshot)
    (variable-set! snapshot-id (+ (variable-ref snapshot-id) 1))
    ret))

(define-syntax define-interrupt-ir
  (syntax-rules ()
    ((_ names-and-args . body)
     (define-ir names-and-args
       (begin
         (variable-set! handle-interrupts? #t)
         . body)))))


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
         (vdl (var-ref (- stack-size proc)))
         (vra (var-ref (+ (- stack-size proc) 1)))
         (vret (var-ref (- (- stack-size proc) 2)))
         (emit-next (lambda ()
                      `(let ((,vdl #f))
                         (let ((,vra #f))
                           (let ((,vdst ,vret))
                             ,(next))))))
         (sp-offset (current-sp-offset))
         (fp-offset (current-fp-offset))
         (proc-offset (+ proc sp-offset)))
    (if (<= fp-offset 0)
        (emit-next)
        (begin
          `(let ((_ ,(take-snapshot! ip 0)))
             ,(let lp ((vars (reverse vars)))
                (match vars
                  (((n . var) . vars)
                   (cond
                    ((< (+ (- stack-size proc 1) sp-offset 2)
                        n
                        (+ stack-size sp-offset))
                     (let* ((i (- n sp-offset))
                            (val (if (< -1 i (vector-length locals))
                                     (vector-ref locals i)
                                     #f))
                            (type (type-of val))
                            (idx n))
                       (with-frame-ref lp vars var type idx)))
                    (else
                     (lp vars))))
                  (()
                   (emit-next)))))))))

(define-ir (receive-values proc allow-extra? nvalues)
  (escape #f))

(define-ir (return src)
  (let* ((stack-size (vector-length locals))
         (vsrc (var-ref src))
         (vdst (var-ref (- stack-size 2)))
         (snapshot (take-snapshot! ip 0))
         (_ (pop-past-frame! past-frame))
         (body `(let ((_ ,snapshot))
                  ,(if (< (current-fp-offset) 0)
                       (next)
                       `(let ((_ (%return ,ra)))
                          ,(next))))))
    (if (eq? vdst vsrc)
        body
        `(let ((,vdst ,vsrc))
           ,body))))

;; XXX: return-values


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
  (next))

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
  ;; XXX: Add guard to check type of box contents.
  (let ((vdst (var-ref dst))
        (vsrc (var-ref src))
        (rsrc (and (< src (vector-length locals))
                   (variable-ref (vector-ref locals src)))))
    `(let ((,vdst (%cref ,vsrc 1)))
       ,(cond
         ((flonum? rsrc)
          `(let ((,vdst ,(to-double vdst)))
             ,(next)))
         ((fixnum? rsrc)
          `(let ((,vdst ,(to-fixnum vdst)))
             ,(next)))
         (else
          (next))))))

(define-ir (box-set! (local dst) (local src))
  (let ((vdst (var-ref dst))
        (vsrc (var-ref src))
        (rdst (and (< dst (vector-length locals))
                   (variable-ref (vector-ref locals dst)))))
    (cond
     ((flonum? rdst)
      `(let ((,vsrc (%from-double ,vsrc)))
         (let ((_ (%cset ,vdst 1 ,vsrc)))
           ,(next))))
     ((fixnum? rdst)
      `(let ((,vsrc (%lsh ,vsrc 2)))
         (let ((,vsrc (%add ,vsrc 2)))
           (let ((_ (%cset ,vdst 1 ,vsrc)))
             ,(next)))))
     (else
      `(let ((_ (%cset ,vdst 1 ,vsrc)))
         ,(next))))))

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

;; XXX: Tag more types.
(define-syntax-rule (with-boxing next rt var tmp)
  (cond
   ((fixnum? rt)
    `(let ((,tmp (%lsh ,var 2)))
       (let ((,tmp (%add ,tmp 2)))
         ,(next tmp))))
   ((pair? rt)
    (next var))
   (else
    (debug 1 ";;; with-boxing: ~a ~a ~a~%" rt var tmp)
    (escape #f))))

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
       ;; XXX: Add guards.
       ,(cond
         ((fixnum? (car rsrc))
          `(let ((,vdst ,(to-fixnum vdst)))
             ,(next)))
         (else
          (debug 1 "XXX: car ~a ~a~%" rdst rsrc)
          (escape #f))))))

(define-ir (cdr (local dst) (local src))
  (let ((rdst (local-ref dst))
        (rsrc (local-ref src))
        (vdst (var-ref dst))
        (vsrc (var-ref src)))
    (when (not (pair? rsrc))
      (debug 1 "XXX: cdr ~a ~a~%" rdst rsrc)
      (escape #f))
    `(let ((,vdst (%cref ,vsrc 1)))
       ;; XXX: Add guards.
       ,(cond
         ((pair? (cdr rsrc))
          (next))
         ((null? (cdr rsrc))
          (next))
         (else
          (debug 1 "XXX: cdr ~a ~a~%" rdst rsrc)
          (escape #f))))))

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
      (debug 3 "ir:convert add ~a ~a ~a~%" rdst ra rb)
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
      (debug 3 "ir:convert add1 ~a ~a" rdst rsrc)
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
      (debug 3 "ir:convert sub ~a ~a ~a~%" rdst ra rb)
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
      (debug 3 "ir:convert sub1 ~a ~a~%" rdst rsrc)
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
      (debug 3 "*** ir:convert: NYI mul ~a ~a ~a~%" rdst ra rb)
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
      (debug 3 "*** ir:convert: NYI mod ~a ~a ~a~%" rdst ra rb)
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


(define (trace->ir trace escape loop? initial-snapshot-id snapshots
                   parent-snapshot past-frame vars
                   initial-sp-offset initial-fp-offset handle-interrupts?)
  (let* ((bytecode-index (make-variable 0))
         (min-sp-offset (make-variable (min initial-sp-offset 0)))
         (initial-nlocals (snapshot-nlocals (hashq-ref snapshots 0)))
         (max-sp-offset (make-variable (get-max-sp-offset initial-sp-offset
                                                          initial-fp-offset
                                                          initial-nlocals)))
         (last-sp-offset (let* ((sp-offsets (past-frame-sp-offsets past-frame))
                                (i (- (vector-length sp-offsets) 1)))
                           (vector-ref sp-offsets i)))
         (last-fp-offset (let* ((fp-offsets (past-frame-fp-offsets past-frame))
                                (i (- (vector-length fp-offsets) 1)))
                           (vector-ref fp-offsets i)))
         (snapshot-id (make-variable initial-snapshot-id)))
    (define (take-snapshot-with-locals! ip dst-offset locals)
      (let-values (((ret snapshot)
                    (take-snapshot ip
                                   dst-offset
                                   locals
                                   vars
                                   (variable-ref snapshot-id)
                                   last-sp-offset
                                   last-fp-offset
                                   (variable-ref min-sp-offset)
                                   (variable-ref max-sp-offset)
                                   parent-snapshot
                                   past-frame)))
        (let* ((old-snapshot-id (variable-ref snapshot-id))
               (new-snapshot-id (+ old-snapshot-id 1)))
          (hashq-set! snapshots old-snapshot-id snapshot)
          (variable-set! snapshot-id new-snapshot-id)
          ret)))
    (define (convert-one op ip fp ra locals rest)
      (cond
       ((hashq-ref *ir-procedures* (car op))
        => (lambda (proc)
             (let ((next
                    (lambda ()
                      (let* ((old-index (variable-ref bytecode-index))
                             (sp-offsets (past-frame-sp-offsets past-frame))
                             (sp-offset (vector-ref sp-offsets old-index))
                             (fp-offsets (past-frame-fp-offsets past-frame))
                             (fp-offset (vector-ref fp-offsets old-index))
                             (nlocals (vector-length locals))
                             (max-offset (get-max-sp-offset sp-offset
                                                            fp-offset
                                                            nlocals)))
                        (variable-set! bytecode-index (+ 1 old-index))
                        (when (< sp-offset (variable-ref min-sp-offset))
                          (variable-set! min-sp-offset sp-offset))
                        (when (< (variable-ref max-sp-offset) max-offset)
                          (variable-set! max-sp-offset max-offset)))
                      (convert rest))))
               (let* ((index (variable-ref bytecode-index))
                      (sp-offset (vector-ref (past-frame-sp-offsets past-frame)
                                             index))
                      (fp-offset (vector-ref (past-frame-fp-offsets past-frame)
                                             index)))
                 (debug 1 ";;; convert-one [sp ~3@a] [fp ~3@a] ~a~%"
                        sp-offset fp-offset op)
                 (debug 1 ";;; max-sp-offset: ~a~%" max-sp-offset))
               (apply proc
                      snapshots
                      snapshot-id
                      parent-snapshot
                      past-frame
                      locals
                      vars
                      min-sp-offset
                      max-sp-offset
                      ip
                      ra
                      handle-interrupts?
                      bytecode-index
                      next
                      escape
                      (cdr op)))))
       (else
        (debug 2 "*** ir:convert: NYI ~a~%" (car op))
        (escape #f))))
    (define (convert trace)
      ;; Last operation is wrapped in a thunk, to assign snapshot ID
      ;; in last expression after taking snapshots from guards in
      ;; traced operations.
      ;;
      ;; Trace with loop will emit `loop', which is the name of
      ;; procedure for looping the body of Scheme IR emitted in
      ;; `make-scm'.
      ;;
      ;; Side trace or loop-less root trace are apturing variables with
      ;; `take-snapshot!' at the end, so that the machine code can pass the
      ;; register information to linked code.
      ;;
      (match trace
        (((op ip fp ra locals) . ())
         (let ((last-op (if loop?
                            (lambda ()
                              `(loop ,@(reverse (map cdr vars))))
                            (lambda ()
                              `(let ((_ ,(take-snapshot-with-locals!
                                          *ip-key-jump-to-linked-code*
                                          0
                                          locals)))
                                 _)))))
           (convert-one op ip fp ra locals last-op)))
        (((op ip fp ra locals) . rest)
         (convert-one op ip fp ra locals rest))
        (last-op
         (or (and (procedure? last-op)
                  (last-op))
             (error "ir.scm: last arg was not a procedure" last-op)))))

    (convert trace)))


;;;
;;; Local accumulator
;;;

(define (accumulate-locals sp-offset fp-offset ops)
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
          ;; XXX: Multiple value not yet managed.
          ;; (('receive-values proc _ _)
          ;;  (add! st proc))
          (('return proc)
           (let ((stack-size (vector-length locals)))
             (add! st proc (- stack-size 2))
             (save-sp-offset!)
             (pop-sp-offset! (- stack-size 2))
             (save-fp-offset!)
             st))
          (('assert-nargs-ee/locals expected nlocals)
           (save-sp-offset!)
           (push-sp-offset! nlocals)
           (save-fp-offset!)
           st)
          (_
           (nyi st (car op)))))))
    (define (acc st ops)
      (match ops
        (((op _ _ _ locals) . rest)
         (acc (acc-one st op locals) rest))
        (()
         st)))
    (let ((local-indices (sort (hash-fold (lambda (k v acc)
                                            (cons k acc))
                                          '()
                                          (acc ret ops))
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
