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
  #:use-module (system vm program)
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
            *scan-procedures*))

;;;
;;; Auxiliary, exported
;;;

(define-record-type <ir>
  (make-ir snapshots snapshot-id parent-snapshot vars
           min-sp-offset max-sp-offset bytecode-index
           outline handle-interrupts? return-subr? proc-local)
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
  (handle-interrupts? ir-handle-interrupts? set-ir-handle-interrupts!)

  ;; Flag for subr call.
  (return-subr? ir-return-subr? set-ir-return-subr!)

  ;; Local index for inlined local of inlined procedure. Set from `call',
  ;; referred by `subr-call' to get the amount of shifted FP when procedure were
  ;; not inlined.
  (proc-local ir-proc-local set-ir-proc-local!))


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
       (if (< fp-offset 0)
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

(define *scan-procedures*
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

(define-syntax gen-put-index
  (syntax-rules (const)
    ((_ ol)
     ol)
    ((_ ol (const arg) . rest)
     (gen-put-index ol . rest))
    ((_ ol (other arg) . rest)
     (let ((indices (assq-set! (outline-local-indices ol)
                               (+ arg (outline-sp-offset ol)) #t)))
       (set-outline-local-indices! ol indices)
       (gen-put-index ol . rest)))))

(define-syntax gen-put-element-type
  (syntax-rules (scm u64 f64 const)
    ((_ ol)
     ol)
    ((_ ol (const arg) . rest)
     (gen-put-element-type ol . rest))
    ((_ ol (other arg) . rest)
     (let ((types (assq-set! (outline-types ol)
                             (+ arg (outline-sp-offset ol)) 'other)))
       (set-outline-types! ol types)
       (gen-put-element-type ol . rest)))))

(define-syntax define-ir
  (syntax-rules ()
    "Defines procedure to compile VM operation to IR, and optionally local
accumulator when arguments in definition are lists. E.g:

  (define-ir (add1 (local dst) (local src))
    ...)

will define two procedures: one for IR compilation taking two arguments, and
another procedure for accumulator taking two arguments and saving index
referenced by dst and src value at runtime."
    ((_ (name) . body)
     (let ((proc
            (lambda (%ir %next %ip %ra %dl %locals)
              (syntax-parameterize
                  ((ir (identifier-syntax %ir))
                   (next (identifier-syntax %next))
                   (ip (identifier-syntax %ip))
                   (ra (identifier-syntax %ra))
                   (dl (identifier-syntax %dl))
                   (locals (identifier-syntax %locals)))
                . body))))
       (hashq-set! *ir-procedures* 'name proc)))
    ((_ (name (flag arg) ...) . body)
     (let ((scan-proc (lambda (ol initialized? arg ...)
                        (unless initialized?
                          (gen-put-index ol (flag arg) ...))
                        (gen-put-element-type ol (flag arg) ...))))
       (hashq-set! *scan-procedures* 'name scan-proc)
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

(define-syntax-rule (to-double scm)
  `(%cref/f ,scm 2))

(define-syntax-rule (dereference-scm addr)
  (pointer->scm (dereference-pointer (make-pointer addr))))

(define-syntax-rule (current-sp-offset)
  (vector-ref (outline-sp-offsets (ir-outline ir)) (ir-bytecode-index ir)))

(define-syntax-rule (current-fp-offset)
  (vector-ref (outline-fp-offsets (ir-outline ir)) (ir-bytecode-index ir)))

(define-syntax-rule (local-ref n)
  (let ((t (outline-type-ref (ir-outline ir) (+ n (current-sp-offset)))))
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

(define-syntax-rule (with-boxing type var tmp proc)
  (cond
   ((eq? type &flonum)
    (set-ir-handle-interrupts! ir #t)
    `(let ((,tmp (%d2s ,var)))
       ,(proc tmp)))
   ;; XXX: Add more types.
   ((memq type (list &exact-integer &char &false &undefined &symbol &pair
                     &vector &string &struct &hash-table))
    (proc var))
   (else
    (nyi "with-boxing: ~a ~s ~s" (pretty-type type) var tmp))))

(define-syntax-rule (with-unboxing type var thunk)
  (let ((tmp (if (equal? var (make-tmpvar 2))
                 (make-tmpvar 1)
                 (make-tmpvar 2))))
    (letrec-syntax
        ((guard-const
          (syntax-rules ()
            ((_ val)
             `(let ((_ ,(take-snapshot! ip 0)))
                (let ((_ (%eq ,var val)))
                  ,(thunk))))))
         (gen-guard-imm
          (syntax-rules ()
            ((_ mask tcx)
             `(let ((_ ,(take-snapshot! ip 0)))
                (let ((,tmp (%band ,var ,mask)))
                  (let ((_ (%eq ,tmp ,tcx)))
                    ,(thunk)))))))
         (gen-guard-cell
          (syntax-rules ()
            ((_ mask tcx expr)
             `(let ((_ ,(take-snapshot! ip 0)))
                (let ((,tmp (%band ,var 6)))
                  (let ((_ (%eq ,tmp 0)))
                    (let ((,tmp (%cref ,var 0)))
                      (let ((,tmp (%band ,tmp mask)))
                        (let ((_ (%eq ,tmp ,tcx)))
                          expr)))))))
            ((_ mask tcx)
             (gen-guard-cell mask tcx ,(thunk)))))
         (guard-tc1
          (syntax-rules ()
            ((_ tag) (gen-guard-cell #x1 tag))))
         (guard-tc2
          (syntax-rules ()
            ((_ tag) (gen-guard-imm #x2 tag))))
         (guard-tc3
          (syntax-rules ()
            ((_ tag) (gen-guard-cell #x7 tag))))
         (guard-tc7
          (syntax-rules ()
            ((_ tag) (gen-guard-cell #x7f tag))))
         (guard-tc8
          (syntax-rules ()
            ((_ tag) (gen-guard-imm #xff tag))))
         (guard-tc16/f
          (syntax-rules ()
            ((_ tag) (gen-guard-cell #xffff tag
                                      (let ((,var (%cref/f ,var 2)))
                                        ,(thunk)))))))
      (cond
       ((eq? type &exact-integer) (guard-tc2 %tc2-int))
       ((eq? type &flonum) (guard-tc16/f %tc16-real))
       ((eq? type &char) (guard-tc8 %tc8-char))
       ((eq? type &unspecified) (guard-const *unspecified*))
       ((eq? type &unbound) (guard-const *unbound*))
       ((eq? type &false) (guard-const #f))
       ((eq? type &true) (guard-const #t))
       ((eq? type &nil) (guard-const #nil))
       ((eq? type &null) (guard-const ()))
       ((eq? type &symbol) (guard-tc7 %tc7-symbol))
       ((eq? type &keyword) (guard-tc7 %tc7-keyword))
       ((eq? type &procedure) (guard-tc7 %tc7-program))
       ((eq? type &pointer) (guard-tc7 %tc7-pointer))
       ((eq? type &pair) (guard-tc1 %tc3-cons))
       ((eq? type &fluid) (guard-tc7 %tc7-fluid))
       ((eq? type &vector) (guard-tc7 %tc7-vector))
       ((eq? type &box) (guard-tc7 %tc7-variable))
       ((eq? type &struct) (guard-tc3 %tc3-struct))
       ((eq? type &string) (guard-tc7 %tc7-string))
       ((eq? type &bytevector) (guard-tc7 %tc7-bytevector))
       ((eq? type &bitvector) (guard-tc7 %tc7-bitvector))
       ((eq? type &array) (guard-tc7 %tc7-array))
       ((eq? type &hash-table) (guard-tc7 %tc7-hashtable))
       ;; XXX: Add more numbers: bignum, complex, rational.
       (else
        (nyi "with-unboxing: ~a ~a" (pretty-type type) var))))))

(define-syntax-rule (expand-stack nlocals)
  (expand-outline (ir-outline ir) (current-sp-offset) nlocals))


;;; *** Call and return

;;; XXX: halt is not defined, might not necessary.

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
              (begin
                (set-ir-proc-local! ir proc)
                `(let ((_ (%scall ,proc)))
                   ,(next)))
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
  (let* ((stack-size (vector-length locals))
         (vdst (var-ref (- stack-size dst 1)))
         (vsrc (var-ref (- (- stack-size proc) 2)))
         (sp-offset (current-sp-offset))
         (min-local-index (+ (- stack-size proc 1) sp-offset 2))
         (max-local-index (+ stack-size sp-offset))
         (return-subr? (ir-return-subr? ir))
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
                                  (tjitc-error 'receive "unknown type ~s at ~s"
                                               elem n)))))
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

    ;; (let ((dst/i (- stack-size dst 1))
    ;;       (src/i (- stack-size proc 2)))
    ;;   (vector-set! locals dst/i (scm->pointer (local-ref src/i))))

    (set-ir-return-subr! ir #f)
    `(let ((,vdst ,vsrc))
       ,(if (or (<= (current-fp-offset) 0)
                return-subr?)
            (next)
            `(let ((_ ,(take-snapshot! ip 0)))
               ,(load-previous-frame))))))

;; XXX: receive-values
(define-ir (receive-values proc allow-extra? nvalues)
  (nyi "receive-values"))

(define-interrupt-ir (return-values nlocals)
  ;; Two locals below callee procedure in VM frame contain dynamic link and
  ;; return address. VM interpreter refills these two with #f, doing the same
  ;; thing in `emit-next'.
  ;;
  (let ((ra/val (make-return-address (make-pointer ra)))
        (dl/val (make-dynamic-link dl))
        (stack-size (vector-length locals)))
    (set-outline-previous-dl-and-ra! (ir-outline ir) stack-size
                                     ra/val dl/val))
  (let ((snapshot (take-snapshot! ip 0)))
    (pop-outline! (ir-outline ir) (current-sp-offset) locals)
    (set-ir-return-subr! ir #f)
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

;; Helper to get type from runtime value returned from external functions, i.e.:
;; `subr-call' and `foreign-call'.
(define-syntax-rule (current-ret-type)
  (let ((idx (+ (ir-bytecode-index ir) 1))
        (ret-types (outline-ret-types (ir-outline ir))))
    (if (<= 0 idx (- (vector-length ret-types) 1))
        (vector-ref ret-types idx)
        (tjitc-error 'current-ret-type "index ~s out of range ~s"
                     idx ret-types))))

(define-interrupt-ir (subr-call)
  (let* ((stack-size (vector-length locals))
         (dst/v (var-ref (- stack-size 2)))
         (subr/l (local-ref (- stack-size 1)))
         (ccode (and (program? subr/l)
                     (program-code subr/l)))
         (ret-type (current-ret-type))
         (r2 (make-tmpvar 2))
         (proc-local (ir-proc-local ir))
         (proc-addr (pointer-address (scm->pointer subr/l)))
         (emit-next
          (lambda ()
            `(let ((,r2 (%ccall ,proc-local ,proc-addr)))
               ,(if ret-type
                    (with-unboxing ret-type r2
                      (lambda ()
                        `(let ((,dst/v ,r2))
                           ,(next))))
                    (next))))))
    (debug 1 ";;; subr-call: (~a) (~s ~{~a~^ ~})~%"
           (pretty-type (current-ret-type))
           (procedure-name subr/l)
           (let lp ((n 0) (acc '()))
             (if (< n (- stack-size 1))
                 (begin
                   (let ((arg (local-ref n)))
                     (lp (+ n 1)
                         (cons (pretty-type (type-of arg)) acc))))
                 acc)))
    (pop-outline! (ir-outline ir) (current-sp-offset) locals)
    (set-ir-return-subr! ir #t)
    (set-ir-proc-local! ir 0)
    (if (primitive-code? ccode)
        (let lp ((n 0))
          (if (< n (- stack-size 1))
              (let ((n/v (var-ref n))
                    (n/l (local-ref n)))
                (with-boxing (type-of n/l) n/v n/v
                  (lambda (boxed)
                    `(let ((_ (%carg ,boxed)))
                       ,(lp (+ n 1))))))
              (emit-next)))
        (tjitc-error 'subr-call "not a primitive ~s" subr/l))))

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

(define-ir (assert-nargs-ee (const expected))
  ;; XXX: Unless this op was found at entry of down recursion, nothing to do.
  ;; Detect entry of down recursion, emit assertion in native code.
  (next))

(define-ir (assert-nargs-ge (const expected))
  ;; XXX: Same as assert-nargs-ee
  (next))

(define-ir (assert-nargs-le (const expected))
  ;; XXX: Same as assert-nargs-ee
  (next))

(define-ir (alloc-frame nlocals)
  (let ((stack-size (vector-length locals))
        (undefined (pointer->scm (make-pointer #x904))))
    (if (< stack-size nlocals)
        (begin
          (expand-stack (- nlocals stack-size))
          (let lp ((n 0))
            (if (< n (- nlocals stack-size))
                `(let ((,(var-ref (- n)) ,undefined))
                   ,(lp (+ n 1)))
                (next))))
        (next))))

(define-ir (reset-frame nlocals)
  (next))

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

(define-ir (br-if-true (scm test) (const invert) (const offset))
  (let* ((test/v (var-ref test))
         (test/l (local-ref test))
         (dest (if test/l
                   (if invert offset 2)
                   (if invert 2 offset)))
         (op (if test/l '%ne '%eq)))
    `(let ((_ ,(take-snapshot! ip dest)))
       (let ((_ (,op ,test/v #f)))
         ,(next)))))

(define-ir (br-if-null (scm test) (const invert) (const offset))
  (let* ((test/l (local-ref test))
         (test/v (var-ref test))
         (dest (if (null? test/l)
                   (if invert offset 2)
                   (if invert 2 offset)))
         (op (if (null? test/l) '%eq '%ne)))
    `(let ((_ ,(take-snapshot! ip dest)))
       (let ((_ (,op ,test/v ())))
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
                          (if invert? offset 3)
                          (if invert? 3 offset))
                      (tjitc-error "~s: got ~s ~s" 'name ra rb))))
       . body))))

(define-syntax define-br-binary-scm-scm
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
          ;; XXX: Delegate bignum, complex ... etc to C function
          (else
           (nyi "~s: ~a ~a~%" 'name ra rb))))))))

(define-br-binary-scm-scm br-if-= = %eq %ne %feq %fne)
(define-br-binary-scm-scm br-if-< < %lt %ge %flt %fge)
(define-br-binary-scm-scm br-if-<= <= %le %gt %fle %fgt)

;;; *** Lexical binding instructions

;; XXX: Assuming both `dst' and `src' have `scm' stack element type. If not,
;; stack element type resolution may return incorrect result. To properly
;; resolve stack element types, may need to traverse bytecode operations
;; backward.
(define-ir (mov dst src)
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
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src))
        (src/l (and (< src (vector-length locals))
                   (let ((var (local-ref src)))
                     (if (variable? var)
                         (variable-ref var)
                         (tjitc-error 'box-ref "got ~s" var)))))
        (r2 (make-tmpvar 2)))
    (vector-set! locals dst (scm->pointer src/l))
    `(let ((,r2 (%cref ,src/v 1)))
       ,(with-unboxing (type-of src/l) r2
          (lambda ()
            `(let ((,dst/v ,r2))
               ,(next)))))))

(define-ir (box-set! (scm dst) (scm src))
  (let* ((vdst (var-ref dst))
         (vsrc (var-ref src))
         (rdst (and (< dst (vector-length locals))
                    (let ((var (local-ref dst)))
                      (if (variable? var)
                          (variable-ref var)
                          (tjitc-error 'box-set! "got ~s~%" var)))))
         (r2 (make-tmpvar 2))
         (emit-next (lambda (tmp)
                      `(let ((_ (%cset ,vdst 1 ,tmp)))
                         ,(next)))))
    (with-boxing (type-of rdst) vsrc r2
      emit-next)))

;; XXX: make-closure
;; XXX: free-ref
;; XXX: free-set!


;;; *** Immediates and statically allocated non-immediates

(define-ir (make-short-immediate (scm dst) (const low-bits))
  `(let ((,(var-ref dst) ,low-bits))
     ,(next)))

(define-ir (make-long-immediate (scm dst) (const low-bits))
  `(let ((,(var-ref dst) ,low-bits))
     ,(next)))

(define-ir (make-long-long-immediate (scm dst)
                                     (const high-bits)
                                     (const low-bits))
  `(let ((,(var-ref dst) ,(logior (ash high-bits 32) low-bits)))
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
        (var (dereference-scm (+ ip (* var-offset 4)))))
    (if (variable? var)
        `(let ((,vdst ,(pointer-address (scm->pointer var))))
           ,(next))
        (nyi "toplevel-box: not a variable ~s" var))))

(define-ir (module-box (scm dst) (const var-offset) (const mod-offset)
                       (const sym-offset) (const bound?))
  (let ((vdst (var-ref dst))
        (var (dereference-scm (+ ip (* var-offset 4)))))
    (if (variable? var)
        `(let ((,vdst ,(pointer-address (scm->pointer var))))
           ,(next))
        (nyi "module-box: not a variable ~s" var))))


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
         (r1 (make-tmpvar 1))
         (r2 (make-tmpvar 2))
         (lx (local-ref x))
         (ly (local-ref y))
         (emit-cons (lambda (a)
                      (lambda (b)
                        `(let ((,vdst (%cons ,a ,b)))
                           ,(next)))))
         (emit-y (lambda (a)
                   (with-boxing (type-of ly) vy r1
                     (emit-cons a))))
         (emit-x (lambda ()
                   (with-boxing (type-of lx) vx r2
                     emit-y))))
    (emit-x)))

(define-ir (car (scm dst) (scm src))
  (let ((dst/l (local-ref dst))
        (src/l (local-ref src))
        (dst/v (var-ref dst))
        (src/v (var-ref src)))
    (when (not (pair? src/l))
      (tjitc-error 'car "~a ~a~%" dst/l src/l))
    (let ((car/l (car src/l))
          (r2 (make-tmpvar 2)))
      `(let ((_ ,(take-snapshot! ip 0)))
         (let ((,r2 (%cref ,src/v 0)))
           ,(with-unboxing (type-of car/l) r2
              (lambda ()
                `(let ((,dst/v ,r2))
                   ,(next)))))))))

(define-ir (cdr (scm dst) (scm src))
  (let ((dst/l (local-ref dst))
        (src/l (local-ref src))
        (dst/v (var-ref dst))
        (src/v (var-ref src)))
    (when (not (pair? src/l))
      (tjitc-error 'cdr "~a ~a~%" dst/l src/l))
    (let ((cdr/l (cdr src/l))
          (r2 (make-tmpvar 2)))
      `(let ((_ ,(take-snapshot! ip 0)))
         (let ((,r2 (%cref ,src/v 1)))
           ,(with-unboxing (type-of cdr/l) r2
              (lambda ()
                `(let ((,dst/v ,r2))
                   ,(next)))))))))

;; XXX: set-car!
;; XXX: set-cdr!


;;; *** Numeric operations

(define-syntax define-binary-arith-scm-scm
  (syntax-rules ()
    ((_ name op-fx1 op-fx2 op-fl)
     (define-ir (name (scm dst) (scm a) (scm b))
       (let ((ra (local-ref a))
             (rb (local-ref b))
             (vdst (var-ref dst))
             (va (var-ref a))
             (vb (var-ref b)))
         (cond
          ((and (fixnum? ra) (fixnum? rb))
           `(let ((,vdst (op-fx1 ,va ,vb)))
              (let ((,vdst (op-fx2 ,vdst 2)))
                ,(next))))
          ((and (fixnum? ra) (flonum? rb))
           (let ((r0 (make-tmpvar 0))
                 (f0 (make-tmpvar/f 0)))
             `(let ((,r0 (%rsh ,va 2)))
                (let ((,f0 (%i2d ,r0)))
                  (let ((,vdst (op-fl ,f0 ,vb)))
                    ,(next))))))
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
           `(let ((,vdst (op-fx ,vsrc ,(* imm 4))))
              ,(next)))
          (else
           (nyi "~s: ~a ~a" 'name (local-ref dst) rsrc))))))))

(define-binary-arith-scm-scm add %add %sub %fadd)
(define-binary-arith-scm-imm add/immediate %add)
(define-binary-arith-scm-scm sub %sub %add %fsub)
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
     ((and (flonum? ra) (fixnum? rb))
      (let ((r2 (make-tmpvar 2))
            (f2 (make-tmpvar/f 2)))
        `(let ((,r2 (%rsh ,vb 2)))
           (let ((,f2 (%i2d ,r2)))
             (let ((,vdst (%fmul ,va ,f2)))
               ,(next))))))
     (else
      (nyi "mul: ~a ~a ~a" (local-ref dst) ra rb)))))

;; (define-ir (div (scm dst) (scm a) (scm b))
;;   (let ((ra (local-ref a))
;;         (rb (local-ref b))
;;         (vdst (var-ref dst))
;;         (va (var-ref a))
;;         (vb (var-ref b)))
;;     (cond
;;      ((and (flonum? ra) (flonum? rb))
;;       `(let ((,vdst (%fdiv ,va ,vb)))
;;          ,(next)))
;;      ((and (fixnum? ra) (flonum? rb))
;;       (let ((r2 (make-tmpvar 2))
;;             (f2 (make-tmpvar/f 2)))
;;         `(let ((,r2 (%rsh ,va 2)))
;;            (let ((,f2 (%i2d ,r2)))
;;              (let ((,vdst (%fdiv ,f2 ,vb)))
;;                ,(next))))))
;;      (else
;;       (nyi "div: ~a ~a ~a" (local-ref dst) ra rb)))))

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
      (let ((r2 (make-tmpvar 2)))
        `(let ((,r2 (%rsh ,va 2)))
           (let ((,vdst (%rsh ,vb 2)))
             (let ((,vdst (%mod ,r2 ,vdst)))
               (let ((,vdst (%lsh ,vdst 2)))
                 (let ((,vdst (%add ,vdst 2)))
                   ,(next))))))))
     (else
      (nyi "mod: ~a ~a ~a" (local-ref dst) ra rb)))))

;; XXX: ash
;; XXX: logand
;; XXX: logior
;; XXX: logxor
;; XXX: make-vector
;; XXX: make-vector/immediate

(define-ir (vector-length (u64 dst) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%cref ,src/v 0)))
       (let ((,dst/v (%rsh ,dst/v 8)))
         ,(next)))))

(define-ir (vector-ref (scm dst) (scm src) (u64 idx))
  (let ((dst/v (var-ref dst))
        (dst/l (let ((src/l (local-ref src))
                     (idx/l (local-ref idx)))
                 (if (vector? src/l)
                     (vector-ref src/l idx/l)
                     (tjitc-error 'vector-ref "not a vector ~s" src/l))))
        (src/v (var-ref src))
        (idx/v (var-ref idx))
        (r2 (make-tmpvar 2)))
    (vector-set! locals dst (scm->pointer dst/l))
    `(let ((,r2 (%add ,idx/v 1)))
       (let ((,r2 (%cref ,src/v ,r2)))
         ,(with-unboxing (type-of dst/l) r2
            (lambda ()
              `(let ((,dst/v ,r2))
                 ,(next))))))))

(define-ir (vector-ref/immediate (scm dst) (scm src) (const idx))
  (let ((dst/v (var-ref dst))
        (dst/l (let ((src/l (local-ref src)))
                 (if (vector? src/l)
                     (vector-ref src/l idx)
                     (tjitc-error 'vector-ref "not a vector ~s" src/l))))
        (src/v (var-ref src))
        (r2 (make-tmpvar 2)))
    `(let ((,r2 (%cref ,src/v ,(+ idx 1))))
       ,(with-unboxing (type-of dst/l) r2
          (lambda ()
            `(let ((,dst/v ,r2))
               ,(next)))))))

(define-ir (vector-set! (scm dst) (u64 idx) (scm src))
  (let ((dst/v (var-ref dst))
        (idx/v (var-ref idx))
        (src/v (var-ref src))
        (src/l (local-ref src))
        (r1 (make-tmpvar 1))
        (r2 (make-tmpvar 2)))
    (with-boxing (type-of src/l) src/v r2
      (lambda (boxed)
        `(let ((,r1 (%add ,idx/v 1)))
           (let ((_ (%cset ,dst/v ,r1 ,boxed)))
             ,(next)))))))

(define-ir (vector-set!/immediate (scm dst) (const idx) (scm src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src))
        (src/l (local-ref src))
        (r2 (make-tmpvar 2)))
    (with-boxing (type-of src/l) src/v r2
      (lambda (boxed)
        `(let ((_ (%cset ,dst/v ,(+ idx 1) ,boxed)))
           ,(next))))))

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
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%rsh ,src/v 2)))
       ,(next))))

(define-ir (u64->scm (scm dst) (u64 src))
  (let ((dst/v (var-ref dst))
        (src/v (var-ref src)))
    `(let ((,dst/v (%lsh ,src/v 2)))
       (let ((,dst/v (%add ,dst/v 2)))
         ,(next)))))

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
         (let ((r2 (make-tmpvar 2)))
           `(let ((_ ,(take-snapshot! ip dest)))
              (let ((,r2 (%rsh ,vb 2)))
                (let ((_ ,(if (op-scm ra rb)
                              `(op-fx-t ,va ,r2)
                              `(op-fx-f ,va ,r2))))
                  ,(next))))))))))

(define-br-binary-u64-scm br-if-u64-=-scm = %eq %ne)
(define-br-binary-u64-scm br-if-u64-<-scm < %lt %ge)
(define-br-binary-u64-scm br-if-u64-<=-scm <= %le %gt)
(define-br-binary-u64-scm br-if-u64->-scm > %gt %le)
(define-br-binary-u64-scm br-if-u64->=-scm >= %ge %lt)
