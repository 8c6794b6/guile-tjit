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
  #:export (make-var
            make-vars
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

(define (take-snapshot ip dst-offset locals vars id
                       local-offset lowest-offset parent-snapshot past-frame)
  (let* ((nlocals (vector-length locals))
         (dst-ip (+ ip (* dst-offset 4)))
         (args-and-indices
          (let lp ((vars vars) (args '()) (indices '()))
            (match vars
              (((n . var) . vars)
               (if (<= lowest-offset n (+ local-offset nlocals))
                   (lp vars (cons var args) (cons n indices))
                   (lp vars args indices)))
              (()
               (cons args indices)))))
         (args (car args-and-indices))
         (indices (cdr args-and-indices))
         (snapshot (make-snapshot id
                                  local-offset
                                  lowest-offset
                                  nlocals
                                  locals
                                  parent-snapshot
                                  indices
                                  past-frame
                                  dst-ip)))
    (values `(%snap ,id ,@args) snapshot)))

(define-syntax-rule (with-frame-ref proc args var type idx)
  (cond
   ((not type)
    `(let ((,var #f))
       ,(proc args)))
   ((= type &flonum)
    `(let ((,var (%fref/f ,idx)))
       ,(proc args)))
   (else
    `(let ((,var (%fref ,idx ,type)))
       ,(proc args)))))

;;;
;;; Auxiliary, internal
;;;

(define *ir-procedures*
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
  local-offset
  lowest-offset
  ip
  ra
  next
  escape)

(define-syntax define-ir
  (syntax-rules ()
    ((_ (name . args) . body)
     (let ((proc
            (lambda (%snapshots
                     %snapshot-id %parent-snapshot %past-frame
                     %locals %vars
                     %local-offset %lowest-offset
                     %ip %ra %next %escape
                     . args)
              (syntax-parameterize
                  ((snapshots (identifier-syntax %snapshots))
                   (snapshot-id (identifier-syntax %snapshot-id))
                   (parent-snapshot (identifier-syntax %parent-snapshot))
                   (past-frame (identifier-syntax %past-frame))
                   (locals (identifier-syntax %locals))
                   (vars (identifier-syntax %vars))
                   (local-offset (identifier-syntax %local-offset))
                   (lowest-offset (identifier-syntax %lowest-offset))
                   (ip (identifier-syntax %ip))
                   (ra (identifier-syntax %ra))
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
  (identifier-syntax 2))

(define-syntax-rule (local-ref n)
  (vector-ref locals n))

(define-syntax-rule (var-ref n)
  (assq-ref vars (+ n (variable-ref local-offset))))

(define-syntax-rule (push-offset! n)
  (variable-set! local-offset (+ (variable-ref local-offset) n)))

(define-syntax-rule (pop-offset! n)
  (let* ((old-local-offset (variable-ref local-offset))
         (new-local-offset (- old-local-offset n)))
    (variable-set! local-offset new-local-offset)
    (when (< new-local-offset (variable-ref lowest-offset))
      (variable-set! lowest-offset new-local-offset))))

(define-syntax-rule (take-snapshot! ip dst-offset)
  (let-values (((ret snapshot)
                (take-snapshot ip
                               dst-offset
                               locals
                               vars
                               (variable-ref snapshot-id)
                               (variable-ref local-offset)
                               (variable-ref lowest-offset)
                               parent-snapshot
                               past-frame)))
    (hashq-set! snapshots (variable-ref snapshot-id) snapshot)
    (variable-set! snapshot-id (+ (variable-ref snapshot-id) 1))
    ret))

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
  (let* ((dl (cons (- (+ proc (variable-ref local-offset)) 2)
                   (make-dynamic-link (variable-ref local-offset))))
         (ra (cons (- (+ proc (variable-ref local-offset)) 1)
                   (make-return-address (make-pointer (+ ip (* 2 4))))))
         (vdl (var-ref (- proc 2)))
         (vra (var-ref (- proc 1)))
         (vproc (var-ref proc))
         (rproc (local-ref proc))
         (rproc-addr (pointer-address (scm->pointer rproc)))
         (snapshot (take-snapshot! ip 0)))
    (push-past-frame! past-frame dl ra (variable-ref local-offset) locals)
    (push-offset! proc)
    `(let ((,vdl #f))
       (let ((,vra ,(+ ip (* 2 4))))
         (let ((_ ,snapshot))
           (let ((_ (%eq ,vproc ,rproc-addr)))
             ,(next)))))))

;; XXX: call-label
(define-ir (call-label proc nlocals label)
  (push-offset! proc)
  (next))

;; XXX: tail-call
(define-ir (tail-call nlocals)
  (next))

;; XXX: tail-call-label
;; XXX: tail-call/shuffle

(define-ir (receive dst proc nlocals)
  ;; `local-offset' could be modified during `convert' with
  ;; `push-offset!' and `pop-offset!'. Wrapping next expression as
  ;; anonymous procedure to prevent overriding variables at this point.
  ;;
  ;; Two locals below callee procedure in VM frame contain dynamic link and
  ;; return address. VM interpreter refills these two with #f, doing the same
  ;; thing in `emit-next'.
  ;;
  (pop-offset! proc)
  (let* ((vdst (var-ref dst))
         (vdl (var-ref (- proc 2)))
         (vra (var-ref (- proc 1)))
         (vret (var-ref (+ proc 1)))
         (emit-next (lambda ()
                      `(let ((,vdl #f))
                         (let ((,vra #f))
                           (let ((,vdst ,vret))
                             ,(next))))))
         (proc-offset (+ proc (variable-ref local-offset))))
    (if (<= 0 (variable-ref local-offset))
        (emit-next)
        (begin
          `(let ((_ ,(take-snapshot! ip 0)))
             ,(let lp ((vars (reverse vars)))
                (match vars
                  (((n . var) . vars)
                   (cond
                    ((<= (variable-ref local-offset) n (+ proc-offset -3))
                     ;; Loading local from lower frame.
                     (let* ((i (- n (variable-ref local-offset)))
                            (val (if (< -1 i (vector-length locals))
                                     (vector-ref locals i)
                                     #f))
                            (type (type-of val)))
                       (let ((idx (- n (variable-ref local-offset))))
                         (with-frame-ref lp vars var type idx))))
                    (else
                     (lp vars))))
                  (()
                   (emit-next)))))))))

(define-ir (receive-values proc allow-extra? nvalues)
  (pop-offset! proc)
  (escape #f))

(define-ir (return src)
  (let* ((vsrc (var-ref src))
         (vdst (var-ref 1))
         (assign (if (eq? vdst vsrc)
                     '()
                     `((,vdst ,vsrc))))
         (snapshot (take-snapshot! ip 0))
         (_ (pop-past-frame! past-frame))
         (body `(let ((_ ,snapshot))
                  ,(if (< 0 (variable-ref local-offset))
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

(define-ir (br offset)
  (next))

(define-ir (br-if-= a b invert? offset)
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

(define-ir (br-if-< a b invert? offset)
  (let ((ra (local-ref a))
        (rb (local-ref b))
        (va (var-ref a))
        (vb (var-ref b)))
    (let ((dest (if (< ra rb)
                    (if invert? offset br-op-size)
                    (if invert? br-op-size offset))))
      (cond
       ((and (fixnum? ra) (fixnum? rb))
        (let ((op (if (< ra rb)
                      `(%lt ,va ,vb)
                      `(%ge ,va ,vb))))
          `(let ((_ ,(take-snapshot! ip dest)))
             (let ((_ ,(if (< ra rb) `(%lt ,va ,vb) `(%ge ,va ,vb))))
               ,(next)))))

       ((and (flonum? ra) (flonum? rb))
        `(let ((_ ,(take-snapshot! ip dest)))
           (let ((_ ,(if (< ra rb) `(%flt ,va ,vb) `(%fge ,va ,vb))))
             ,(next))))
       (else
        (debug 3 "ir:convert < ~a ~a~%" ra rb)
        (escape #f))))))

;; XXX: br-if-true
;; XXX: br-if-null
;; XXX: br-if-nil
;; XXX: br-if-pair
;; XXX: br-if-struct
;; XXX: br-if-char
;; XXX: br-if-tc7
;; XXX: br-if-eq
;; XXX: br-if-eqv
;; XXX: br-if-equal
;; XXX: br-if-<=


;;; *** Lexical binding instructions

(define-ir (mov dst src)
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

(define-ir (box-ref dst src)
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

(define-ir (box-set! dst src)
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

(define-ir (make-short-immediate dst low-bits)
  ;; XXX: `make-short-immediate' could be used for other value than small
  ;; integer, e.g: '(). Check type from value of `low-bits' and choose
  ;; the type appropriately.
  `(let ((,(var-ref dst) ,(ash low-bits -2)))
     ,(next)))

(define-ir (make-long-immediate dst low-bits)
  `(let ((,(var-ref dst) ,(ash low-bits -2)))
     ,(next)))

(define-ir (make-long-long-immediate dst high-bits low-bits)
  `(let ((,(var-ref dst)
          ,(ash (logior (ash high-bits 32) low-bits) -2)))
     ,(next)))

;; XXX: make-non-immediate

(define-ir (static-ref dst offset)
  `(let ((,(var-ref dst) ,(dereference-scm (+ ip (* 4 offset)))))
     ,(next)))

;; XXX: static-set!
;; XXX: static-patch!

;;; *** Mutable top-level bindings

;; XXX: current-module
;; XXX: resolve
;; XXX: define!

(define-ir (toplevel-box dst var-offset mod-offset sym-offset bound?)
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

;; XXX: cons
;; XXX: car
;; XXX: cdr
;; XXX: set-car!
;; XXX: set-cdr!

;;; *** Numeric operations

(define-ir (add1 dst src)
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

(define-ir (add dst a b)
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

(define-ir (sub dst a b)
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

(define-ir (sub1 dst src)
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

(define-ir (mul dst a b)
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

(define-ir (mod dst a b)
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

;; *** Move above

;; XXX: br-if-logtest
;; XXX: allocate-struct
;; XXX: struct-ref
;; XXX: struct-set!

(define (trace->ir trace escape loop? initial-snapshot-id snapshots
                   parent-snapshot past-frame vars initial-offset)
  (let* ((local-offset (make-variable initial-offset))
         (lowest-offset (make-variable (min initial-offset 0)))
         (snapshot-id (make-variable initial-snapshot-id)))
    (define (take-snapshot-with-locals! ip dst-offset locals)
      (let-values (((ret snapshot)
                    (take-snapshot ip
                                   dst-offset
                                   locals
                                   vars
                                   (variable-ref snapshot-id)
                                   (variable-ref local-offset)
                                   (variable-ref lowest-offset)
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
             (let ((next (lambda ()
                           (convert rest))))
               (apply proc
                      snapshots
                      snapshot-id
                      parent-snapshot
                      past-frame
                      locals
                      vars
                      local-offset
                      lowest-offset
                      ip
                      ra
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

(define (accumulate-locals offset ops)
  ;; Makes past-frame with locals in lower frames.
  ;;
  ;; Lower frame data is saved at the time of accumulation. Otherwise, if
  ;; one of the guard operation appeared soon after bytecode sequence
  ;; `return' and `receive', snapshot does not know the value of locals in
  ;; lower frame. When recorded bytecode contains `return' before `call',
  ;; snapshot will recover a frame lower than the one used to enter the
  ;; native call.
  ;;
  (let* ((ret (make-hash-table)))
    (define (nyi st op)
      (debug 3 "ir:accumulate-locals: NYI ~a~%" op)
      st)
    (define (acc-one st op locals)
      (define-syntax-rule (push-offset! n)
        (set! offset (+ offset n)))
      (define-syntax-rule (pop-offset! n)
        (set! offset (- offset n)))
      (define-syntax add!
        (syntax-rules ()
          ((_ st i j k l)
           (add! (add! st i j) k l))
          ((_ st i j k)
           (add! (add! st i j) k))
          ((_ st i j)
           (add! (add! st i) j))
          ((_ st i)
           (begin
             (hashq-set! st (+ i offset) #t)
             st))))
      (match op
        ((op a1)
         (case op
           ((return)
            ;; Store proc, returned value, VM frame dynamic link, and VM frame
            ;; return address.
            (add! st a1 1 -1 -2))
           ((br tail-call)
            st)
           (else
            (nyi st op))))
        ((op a1 a2)
         (case op
           ((call)
            ;; Store proc, VM frame dynamic link, and VM frame return address.
            (let ((st (add! st a1 (- a1 1) (- a1 2))))
              (push-offset! a1)
              st))
           ((static-ref
             make-short-immediate make-long-immediate make-long-long-immediate)
            (add! st a1))
           ((mov sub1 add1 box-ref box-set!)
            (add! st a1 a2))
           ((assert-nargs-ee/locals)
            st)
           (else
            (nyi st op))))
        ((op a1 a2 a3)
         (case op
           ((add sub mul div quo)
            (add! st a1 a2 a3))
           ((call-label)
            (let ((st (add! st a1)))
              (push-offset! a1)
              st))
           ((receive)
            ;; Modifying locals since procedure taking snapshot uses frame
            ;; lowers to recover the values after this `receive' bytecode
            ;; operation. Making a copy of locals so that later procedure can
            ;; see the original locals.
            (let ((locals-copy (vector-copy locals))
                  (ret-index (+ a2 1)))
              (pop-offset! a2)
              ;; XXX: If this test for `when' is removed, "mandelbrot.scm" with
              ;; `--jit-debug=0' will fail. However, running with `--jit-debug'
              ;; value greater than 0 will work.
              (when (and (< ret-index (vector-length locals))
                         (< a1 (vector-length locals-copy)))
                (vector-set! locals-copy a1 (vector-ref locals ret-index)))
              (add! st a1 a2)))
           ((receive-values)
            (pop-offset! a1)
            (add! st a1))
           (else
            (nyi st op))))
        ((op a1 a2 a3 a4)
         (case op
           ((br-if-< br-if-= br-if-<=)
            (add! st a1 a2))
           (else
            (nyi st op))))
        ((op a1 a2 a3 a4 a5)
         (case op
           ((toplevel-box)
            (add! st a1))
           (else
            (nyi st op))))
        ((op)
         (nyi st op))
        (_
         (error (format #f "ir:accumulate-locals: ~a" ops)))))
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
                               >)))
      (let ((verbosity (lightning-verbosity)))
        (when (and verbosity (<= 3 verbosity))
          (format #t ";;; local-indices:~%")
          (format #t ";;;   ~a~%" local-indices)))

      (make-past-frame '() '() offset #() local-indices))))
