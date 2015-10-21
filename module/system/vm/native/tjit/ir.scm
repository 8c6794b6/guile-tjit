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
            compile-ir))

;;;
;;; Auxiliary procedures
;;;

(define (to-fixnum scm)
  `(%rsh ,scm 2))

(define (to-double scm)
  `(%cref/f ,scm 2))


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
  rest
  snapshots
  snapshot-id
  parent-snapshot
  past-frame
  locals
  vars
  local-offset
  lowest-offset
  convert
  escape)

(define-syntax-rule (local-ref n)
  (vector-ref locals n))

(define-syntax-rule (var-ref n)
  (assq-ref vars (+ n local-offset)))

(define-syntax-rule (take-snapshot! ip dst-offset)
  (let-values (((ret snapshot)
                (take-snapshot ip
                               dst-offset
                               locals
                               vars
                               snapshot-id
                               local-offset
                               lowest-offset
                               parent-snapshot
                               past-frame)))
    (hashq-set! snapshots snapshot-id snapshot)
    (set! snapshot-id (+ snapshot-id 1))
    ret))

(define-syntax define-ir
  (syntax-rules ()
    ((_ (name . args) <body>)
     (begin
       (define (name %rest
                     %snapshots
                     %snapshot-id
                     %parent-snapshot
                     %past-frame
                     %locals
                     %vars
                     %local-offset
                     %lowest-offset
                     %convert
                     %escape
                     . args)
         (syntax-parameterize ((rest (identifier-syntax %rest))
                               (snapshots (identifier-syntax %snapshots))
                               (snapshot-id (identifier-syntax %snapshot-id))
                               (parent-snapshot
                                (identifier-syntax %parent-snapshot))
                               (past-frame (identifier-syntax %past-frame))
                               (locals (identifier-syntax %locals))
                               (vars (identifier-syntax %vars))
                               (local-offset (identifier-syntax %local-offset))
                               (lowest-offset
                                (identifier-syntax %lowest-offset))
                               (convert (syntax-rules ()
                                          ((_ x)
                                           (%convert x))))
                               (escape (syntax-rules ()
                                         ((_ x) (%escape x)))))
           <body>))
       (hashq-set! *ir-procedures* 'name name)))))

(define-ir (add1 dst src)
  (let ((rdst (local-ref dst))
        (rsrc (local-ref src))
        (vdst (var-ref dst))
        (vsrc (var-ref src)))
    (cond
     ((fixnum? rsrc)
      `(let ((,vdst (%add ,vsrc 1)))
         ,(convert rest)))
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
         ,(convert rest)))
     ((and (flonum? ra) (flonum? rb))
      `(let ((,vdst (%fadd ,va ,vb)))
         ,(convert rest)))
     (else
      (debug 3 "ir:convert add ~a ~a ~a~%" rdst ra rb)
      (escape #f)))))

(define (take-snapshot ip dst-offset locals vars snapshot-id
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
         (snapshot (make-snapshot snapshot-id
                                  local-offset
                                  lowest-offset
                                  nlocals
                                  locals
                                  parent-snapshot
                                  indices
                                  past-frame
                                  dst-ip)))
    (values `(%snap ,snapshot-id ,@args) snapshot)))

(define (compile-ir trace escape loop? snapshot-id snapshots
                    parent-snapshot past-frame vars initial-offset)
  (let* ((local-offset initial-offset)
         (lowest-offset (min local-offset 0)))
    (define (take-snapshot-with-locals! ip dst-offset locals)
      (let-values (((ret snapshot)
                    (take-snapshot ip
                                   dst-offset
                                   locals vars
                                   snapshot-id
                                   local-offset
                                   lowest-offset
                                   parent-snapshot
                                   past-frame)))
        (hashq-set! snapshots snapshot-id snapshot)
        (set! snapshot-id (+ snapshot-id 1))
        ret))

    (define (convert-one op ip fp ra locals rest)
      (define-syntax br-op-size
        (identifier-syntax 2))
      (define-syntax-rule (push-offset! n)
        (set! local-offset (+ local-offset n)))
      (define-syntax-rule (pop-offset! n)
        (begin
          (set! local-offset (- local-offset n))
          (when (< local-offset lowest-offset)
            (set! lowest-offset local-offset))))
      (define-syntax-rule (local-ref i)
        (vector-ref locals i))
      (define-syntax-rule (var-ref i)
        (assq-ref vars (+ i local-offset)))
      (define (dereference-scm addr)
        (pointer->scm (dereference-pointer (make-pointer addr))))
      (define (take-snapshot! ip dst-offset)
        (take-snapshot-with-locals! ip dst-offset locals))

      (cond
       ((hashq-ref *ir-procedures* (car op))
        => (lambda (proc)
             (apply proc
                    rest
                    snapshots
                    snapshot-id
                    parent-snapshot
                    past-frame
                    locals
                    vars
                    local-offset
                    lowest-offset
                    convert
                    escape
                    (cdr op))))
       (else
        (match op
          ;; *** Call and return

          ;; XXX: halt

          (('call proc nlocals)
           ;; When procedure get inlined, taking snapshot of previous frame.
           ;; Contents of previous frame could change in native code. Note that
           ;; frame return address will get checked at the time of `%return'.
           (let* ((dl (cons (- (+ proc local-offset) 2)
                            (make-dynamic-link local-offset)))
                  (ra (cons (- (+ proc local-offset) 1)
                            (make-return-address (make-pointer (+ ip (* 2 4))))))
                  (vdl (var-ref (- proc 2)))
                  (vra (var-ref (- proc 1)))
                  (vproc (var-ref proc))
                  (rproc (local-ref proc))
                  (rproc-addr (pointer-address (scm->pointer rproc)))
                  (snapshot (take-snapshot! ip 0)))
             (push-past-frame! past-frame dl ra local-offset locals)
             (push-offset! proc)

             ;; Refilling dynamic link and return address for CPS compilation
             ;; only. These two locals would be restored with values in snapshot
             ;; when taiking side exit.
             `(let ((,vdl #f))
                (let ((,vra ,(+ ip (* 2 4))))
                  (let ((_ ,snapshot))
                    ;; Adding `%eq' guard to test the procedure value, to bailout when
                    ;; procedure has been redefined.
                    (let ((_ (%eq ,vproc ,rproc-addr)))
                      ,(convert rest)))))))

          (('call-label proc nlocals label)
           (push-offset! proc)
           (convert rest))

          ;; XXX: tail-call
          (('tail-call nlocals)
           (convert rest))

          ;; XXX: tail-call-label
          ;; XXX: tail-call/shuffle

          (('receive dst proc nlocals)
           (pop-offset! proc)
           (debug 3 ";;; ir.scm:receive~%")
           (debug 3 ";;;   dst=~a proc=~a nlocals=~a~%" dst proc nlocals)
           (debug 3 ";;;   local-offset=~a~%" local-offset)

           ;; `local-offset' could be modified during `convert' with
           ;; `push-offset!' and `pop-offset!'. Wrapping next expression as
           ;; anonymous procedure to prevent overriding variables at this point.
           (let* ((vdst (var-ref dst))
                  (vdl (var-ref (- proc 2)))
                  (vra (var-ref (- proc 1)))
                  (vret (var-ref (+ proc 1)))
                  (do-next-convert
                   (lambda ()
                     ;; Two locals below callee procedure in VM frame contain
                     ;; dynamic link and return address. VM interpreter refills
                     ;; these two with #f, doing the same thing.
                     `(let ((,vdl #f))
                        (let ((,vra #f))
                          (let ((,vdst ,vret))
                            ,(convert rest))))))
                  (proc-offset (+ proc local-offset)))
             (if (<= 0 local-offset)
                 (do-next-convert)
                 (begin
                   `(let ((_ ,(take-snapshot! ip 0)))
                      ,(let lp ((vars (reverse vars)))
                         (match vars
                           (((n . var) . vars)
                            (cond
                             ((<= local-offset n (+ proc-offset -3))
                              ;; Loading local from lower frame.
                              (let* ((i (- n local-offset))
                                     (val (if (< -1 i (vector-length locals))
                                              (vector-ref locals i)
                                              #f))
                                     (type (type-of val)))
                                (let ((idx (- n local-offset)))
                                  (with-frame-ref lp vars var type idx))))
                             (else
                              (lp vars))))
                           (()
                            (do-next-convert)))))))))

          (('receive-values proc allow-extra? nvalues)
           (pop-offset! proc)
           (escape #f))

          (('return src)
           (let* ((vsrc (var-ref src))
                  (vdst (var-ref 1))
                  (assign (if (eq? vdst vsrc)
                              '()
                              `((,vdst ,vsrc))))
                  (snapshot (take-snapshot! ip 0))
                  (_ (pop-past-frame! past-frame))
                  (body `(let ((_ ,snapshot))
                           ,(if (< 0 local-offset)
                                (convert rest)
                                `(let ((_ (%return ,ra)))
                                   ,(convert rest))))))
             (if (eq? vdst vsrc)
                 body
                 `(let ((,vdst ,vsrc))
                    ,body))))

          ;; XXX: return-values

          ;; *** Specialized call stubs

          ;; XXX: subr-call
          ;; XXX: foreign-call
          ;; XXX: continuation-call
          ;; XXX: compose-continuation
          ;; XXX: tail-apply
          ;; XXX: call/cc
          ;; XXX: abort
          ;; XXX: builtin-ref

          ;; *** Function prologues

          ;; XXX: br-if-nargs-ne
          ;; XXX: br-if-nargs-lt
          ;; XXX; br-if-nargs-gt
          ;; XXX: assert-nargs-ee
          ;; XXX: assert-nargs-ge
          ;; XXX: assert-nargs-le
          ;; XXX: alloc-frame
          ;; XXX: reset-frame

          (('assert-nargs-ee/locals expected nlocals)
           (convert rest))

          ;; XXX: br-if-npos-gt
          ;; XXX: bind-kw-args
          ;; XXX: bind-rest

          ;; *** Branching instructions

          (('br offset)
           (convert rest))

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

          (('br-if-= a b invert? offset)
           (let ((ra (local-ref a))
                 (rb (local-ref b))
                 (va (var-ref a))
                 (vb (var-ref b)))
             (let ((dest (if (= ra rb)
                             (if invert? offset br-op-size)
                             (if invert? br-op-size offset))))
               (cond
                ((and (fixnum? ra) (fixnum? rb))
                 `(let ((_ ,(take-snapshot! ip dest)))
                    (let ((_ ,(if (= ra rb) `(%eq ,va ,vb) `(%ne ,va ,vb))))
                      ,(convert rest))))
                (else
                 (debug 3 "*** ir:convert = ~a ~a~%" ra rb)
                 (escape #f))))))

          (('br-if-< a b invert? offset)
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
                        ,(convert rest)))))

                ((and (flonum? ra) (flonum? rb))
                 `(let ((_ ,(take-snapshot! ip dest)))
                    (let ((_ ,(if (< ra rb) `(%flt ,va ,vb) `(%fge ,va ,vb))))
                      ,(convert rest))))
                (else
                 (debug 3 "ir:convert < ~a ~a~%" ra rb)
                 (escape #f))))))

          ;; XXX: br-if-<=

          ;; *** Lexical binding instructions

          (('mov dst src)
           (let ((vdst (var-ref dst))
                 (vsrc (var-ref src)))
             `(let ((,vdst ,vsrc))
                ,(convert rest))))

          ;; XXX: long-mov
          ;; XXX: box

          ;; XXX: Reconsider how to manage `box', `box-ref', and `box-set!'.
          ;; Boxing back tagged value every time will make the loop slow, need
          ;; more analysis when the storing could be removed from native code loop
          ;; and delayed to side exit code.
          ;;
          ;; XXX: Add test for nested boxes.

          (('box-ref dst src)
           ;; XXX: Add guard to check type of box contents.
           (let ((vdst (var-ref dst))
                 (vsrc (var-ref src))
                 (rsrc (and (< src (vector-length locals))
                            (variable-ref (vector-ref locals src)))))
             `(let ((,vdst (%cref ,vsrc 1)))
                ,(cond
                  ((flonum? rsrc)
                   `(let ((,vdst ,(to-double vdst)))
                      ,(convert rest)))
                  ((fixnum? rsrc)
                   `(let ((,vdst ,(to-fixnum vdst)))
                      ,(convert rest)))
                  (else
                   (convert rest))))))

          (('box-set! dst src)
           (let ((vdst (var-ref dst))
                 (vsrc (var-ref src))
                 (rdst (and (< dst (vector-length locals))
                            (variable-ref (vector-ref locals dst)))))
             (cond
              ((flonum? rdst)
               `(let ((,vsrc (%from-double ,vsrc)))
                  (let ((_ (%cset ,vdst 1 ,vsrc)))
                    ,(convert rest))))
              ((fixnum? rdst)
               `(let ((,vsrc (%lsh ,vsrc 2)))
                  (let ((,vsrc (%add ,vsrc 2)))
                    (let ((_ (%cset ,vdst 1 ,vsrc)))
                      ,(convert rest)))))
              (else
               `(let ((_ (%cset ,vdst 1 ,vsrc)))
                  ,(convert rest))))))

          ;; XXX: make-closure
          ;; XXX: free-ref
          ;; XXX: free-set!

          ;; *** Immediates and statically allocated non-immediates

          (('make-short-immediate dst low-bits)
           ;; XXX: `make-short-immediate' could be used for other value than small
           ;; integer, e.g: '(). Check type from value of `low-bits' and choose
           ;; the type appropriately.
           `(let ((,(var-ref dst) ,(ash low-bits -2)))
              ,(convert rest)))

          (('make-long-immediate dst low-bits)
           `(let ((,(var-ref dst) ,(ash low-bits -2)))
              ,(convert rest)))

          (('make-long-long-immediate dst high-bits low-bits)
           `(let ((,(var-ref dst)
                   ,(ash (logior (ash high-bits 32) low-bits) -2)))
              ,(convert rest)))

          ;; XXX: make-non-immediate

          (('static-ref dst offset)
           `(let ((,(var-ref dst) ,(dereference-scm (+ ip (* 4 offset)))))
              ,(convert rest)))

          ;; XXX: static-set!
          ;; XXX: static-patch!

          ;; *** Mutable top-level bindings

          ;; XXX: current-module
          ;; XXX: resolve
          ;; XXX: define!

          (('toplevel-box dst var-offset mod-offset sym-offset bound?)
           (let ((vdst (var-ref dst))
                 (src (pointer-address
                       (scm->pointer
                        (dereference-scm (+ ip (* var-offset 4)))))))
             `(let ((,vdst ,src))
                ,(convert rest))))

          ;; XXX: module-box

          ;; *** The dynamic environment

          ;; XXX: prompt
          ;; XXX: wind
          ;; XXX: unwind
          ;; XXX: push-fluid
          ;; XXX: pop-fluid
          ;; XXX: fluid-ref
          ;; XXX: fluid-set

          ;; *** Strings, symbols, and keywords

          ;; XXX: string-length
          ;; XXX: string-ref
          ;; XXX: string->number
          ;; XXX: string->symbol
          ;; XXX: symbol->keyword

          ;; *** Pairs

          ;; XXX: cons
          ;; XXX: car
          ;; XXX: cdr
          ;; XXX: set-car!
          ;; XXX: set-cdr!

          ;; *** Numeric operations

          ;; (('add dst a b)
          ;;  (let ((rdst (local-ref dst))
          ;;        (ra (local-ref a))
          ;;        (rb (local-ref b))
          ;;        (vdst (var-ref dst))
          ;;        (va (var-ref a))
          ;;        (vb (var-ref b)))
          ;;    (cond
          ;;     ((and (fixnum? ra) (fixnum? rb))
          ;;      `(let ((,vdst (%add ,va ,vb)))
          ;;         ,(convert rest)))
          ;;     ((and (flonum? ra) (flonum? rb))
          ;;      `(let ((,vdst (%fadd ,va ,vb)))
          ;;         ,(convert rest)))
          ;;     (else
          ;;      (debug 3 "ir:convert add ~a ~a ~a~%" rdst ra rb)
          ;;      (escape #f)))))

          ;; (('add1 dst src)
          ;;  (let ((rdst (local-ref dst))
          ;;        (rsrc (local-ref src))
          ;;        (vdst (var-ref dst))
          ;;        (vsrc (var-ref src)))
          ;;    (cond
          ;;     ((fixnum? rsrc)
          ;;      `(let ((,vdst (%add ,vsrc 1)))
          ;;         ,(convert rest)))
          ;;     (else
          ;;      (debug 3 "ir:convert add1 ~a ~a" rdst rsrc)
          ;;      (escape #f)))))

          (('sub dst a b)
           (let ((rdst (local-ref dst))
                 (ra (local-ref a))
                 (rb (local-ref b))
                 (vdst (var-ref dst))
                 (va (var-ref a))
                 (vb (var-ref b)))
             (cond
              ((and (fixnum? ra) (fixnum? rb))
               `(let ((,vdst (%sub ,va ,vb)))
                  ,(convert rest)))
              ((and (flonum? ra) (flonum? rb))
               `(let ((,vdst (%fsub ,va ,vb)))
                  ,(convert rest)))
              (else
               (debug 3 "ir:convert sub ~a ~a ~a~%" rdst ra rb)
               (escape #f)))))

          (('sub1 dst src)
           (let ((rdst (local-ref dst))
                 (rsrc (local-ref src))
                 (vdst (var-ref dst))
                 (vsrc (var-ref src)))
             (cond
              ((fixnum? rsrc)
               `(let ((,vdst (%sub ,vsrc 1)))
                  ,(convert rest)))
              (else
               (debug 3 "ir:convert sub1 ~a ~a~%" rdst rsrc)
               (escape #f)))))

          (('mul dst a b)
           (let ((rdst (local-ref dst))
                 (ra (local-ref a))
                 (rb (local-ref b))
                 (vdst (var-ref dst))
                 (va (var-ref a))
                 (vb (var-ref b)))
             (cond
              ((and (flonum? ra) (flonum? rb))
               `(let ((,vdst (%fmul ,va ,vb)))
                  ,(convert rest)))
              (else
               (debug 3 "*** ir:convert: NYI mul ~a ~a ~a~%" rdst ra rb)
               (escape #f)))))

          ;; XXX: div
          ;; XXX: quo
          ;; XXX: rem

          (('mod dst a b)
           (let ((rdst (local-ref dst))
                 (ra (local-ref a))
                 (rb (local-ref b))
                 (vdst (var-ref dst))
                 (va (var-ref a))
                 (vb (var-ref b)))
             (cond
              ((and (fixnum? ra) (fixnum? rb))
               `(let ((,vdst (%mod ,va ,vb)))
                  ,(convert rest)))
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
          (('vector-set! dst idx src)
           (let ((rdst (local-ref dst))
                 (rsrc (local-ref src))
                 (ridx (local-ref idx))
                 (vdst (var-ref dst))
                 (vsrc (var-ref src))
                 (vidx (var-ref idx)))
             (cond
              ((and (vector? rsrc) (fixnum? ridx))
               `(let ((_ (%vector-set! ,vdst ,vsrc ,vidx)))
                  ,(convert rest)))
              (else
               (debug 3 "*** ir.scm:convert: NYI vector-set! ~a ~a ~a~%"
                      rdst rsrc ridx)
               (escape #f)))))

          ;; XXX: vector-set!/immediate

          ;; *** Structs and GOOPS

          ;; XXX: struct-vtable
          ;; XXX: allocate-struct/immediate
          ;; XXX: struct-ref/immediate
          ;; XXX: struct-set!/immediate
          ;; XXX: class-of

          ;; *** Arrays, packed uniform arrays, and bytevectors

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

          (op
           (debug 3 "*** ir:convert: NYI ~a~%" (car op))
           (escape #f))))))

    (define (convert trace)
      ;; Last operation is wrapped in a thunk, to assign snapshot ID
      ;; in last expression after taking snapshots from guards in
      ;; traced operations.
      ;;
      ;; Trace with loop will emit `loop', which is the name of
      ;; procedure for looping the body of Scheme IR emitted in
      ;; `make-scm'.
      ;;
      ;; Side trace or loop-less root trace are apturing CPS variables
      ;; with `take-snapshot!' at the end, so that the native code can
      ;; pass the register information to linked code.
      ;;
      (match trace
        (((op ip fp ra locals) . ())
         (let ((last-op
                (if loop?
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
