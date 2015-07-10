;;; -*- mode: scheme; coding: utf-8; -*-

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

;;; Compile traced bytecode to CPS intermediate representation via Scheme in
;;; (almost) ANF.
;;;
;;; One of the reasons to convert bytecode to CPS is to do floating point
;;; arithmetic efficiently. VM bytecodes uses integer index to refer locals.
;;; Those locals does not distinguish floating point values from other. In CPS
;;; format, since every variables are named, it is possible to perform floating
;;; point arithmetic directly with unboxed value in floating point register
;;; inside a loop when registers and variables assigned properly.

;;; Code:

(define-module (system vm native tjit ir)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps2)
  #:use-module (language cps2 optimize)
  #:use-module (language cps2 renumber)
  #:use-module (language cps2 types)
  #:use-module (language scheme spec)
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (every))
  #:use-module (srfi srfi-9)
  #:use-module (system base compile)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:export (trace->cps
            trace->scm
            scm->cps
            accumulate-locals
            fixnum?
            flonum?))


;;;
;;; Type checker based on runtime values
;;;

(define (fixnum? val)
  (and (exact-integer? val)
       (<= most-negative-fixnum val most-positive-fixnum)))

(define (flonum? val)
  (and (real? val) (inexact? val)))


;;;
;;; Locals
;;;

(define (lowest-offset traces)
  (let lp ((traces traces) (offset 0) (lowest 0))
    (match traces
      (((op _ _ . _) . traces)
       (match op
         (('call proc _)
          (lp traces (+ offset proc) lowest))
         (('call-label proc _ _)
          (lp traces (+ offset proc) lowest))
         (('receive _ proc _)
          (let ((offset (- offset proc)))
            (lp traces offset (min offset lowest))))
         (('receive-values proc _ _)
          (let ((offset (- offset proc)))
            (lp traces offset (min offset lowest))))
         (else
          (lp traces offset lowest))))
      (() lowest))))

(define (accumulate-locals local-offset ops)
  (define (nyi op)
    (debug 3 "ir:accumulate-locals: NYI ~a~%" op))

  (define (acc-one st op)
    (define-syntax-rule (push-offset! n)
      (set! local-offset (+ local-offset n)))

    (define-syntax-rule (pop-offset! n)
      (set! local-offset (- local-offset n)))

    (define-syntax-rule (add! st i)
      (let ((n (+ i local-offset)))
        (intset-add! st n)))

    (define-syntax-rule (add2! st i j)
      (add! (add! st i) j))

    (define-syntax-rule (add3! st i j k)
      (add! (add2! st i j) k))

    (match op
      ((op a1)
       (case op
         ((return)
          (add! st a1))
         ((br native-call)
          st)
         (else
          (nyi op)
          st)))
      ((op a1 a2)
       (case op
         ((call)
          (let ((st (add! st a1)))
            (push-offset! a1)
            st))
         ((static-ref
           make-short-immediate make-long-immediate make-long-long-immediate)
          (add! st a1))
         ((mov sub1 add1 box-ref box-set!)
          (add2! st a1 a2))
         ((assert-nargs-ee/locals)
          st)
         (else
          (nyi op)
          st)))
      ((op a1 a2 a3)
       (case op
         ((add sub mul div quo)
          (add3! st a1 a2 a3))
         ((call-label)
          (let ((st (add! st a1)))
            (push-offset! a1)
            st))
         ((receive)
          (pop-offset! a2)
          (add2! st a1 a2))
         ((receive-values)
          (pop-offset! a1)
          (add! st a1))
         (else
          (nyi op)
          st)))
      ((op a1 a2 a3 a4)
       (case op
         ((br-if-< br-if-= br-if-<=)
          (add2! st a1 a2))
         (else
          (nyi op)
          st)))
      ((op a1 a2 a3 a4 a5)
       (case op
         ((toplevel-box)
          (add! st a1))
         (else
          (nyi op)
          st)))
      (op
       (nyi op)
       st)
      (_
       (error (format #f "ir:accumulate-locals: ~a" ops)))))

  (define (acc st ops)
    (match ops
      (((op ip bv locals) . rest)
       (acc (acc-one st op) rest))
      (()
       st)))

  ;; (debug 1 "accumulate-locals: lowest-offset=~a~%"
  ;;        (lowest-offset ops))

  (let ((locals (intset-fold cons (acc empty-intset ops) '())))
    (debug 2 ";;; locals: ~a~%" locals)
    locals))


;;;
;;; Auxiliary procedures for ANF compiler
;;;

(define (from-fixnum num)
  `(let ((tmp (%lsh ,num 2)))
     (%add tmp 2)))

(define (to-fixnum scm)
  `(%rsh ,scm 2))

(define (to-double scm)
  `(%cell-object-f ,scm 2))


;;;
;;; Scheme ANF compiler
;;;

(define (trace->scm nlog ops)
  (define br-op-size 2)
  (define (dereference-scm addr)
    (pointer->scm (dereference-pointer (make-pointer addr))))

  (define (make-var index)
    (string->symbol (string-append "v" (number->string index))))

  (define (make-vars nlocals locals)
    (let lp ((i nlocals) (locals locals) (acc '()))
      (cond
       ((< i 0)
        (list->vector acc))
       ((null? locals)
        (lp (- i 1) locals (cons #f acc)))
       ((= i (car locals))
        (lp (- i 1) (cdr locals) (cons (make-var i) acc)))
       (else
        (lp (- i 1) locals (cons #f acc))))))

  (define (make-types vars)
    (make-vector (vector-length vars) #f))

  (define (type-guard-op type)
    (cond
     ((eq? type &exact-integer) '%guard-fx)
     ((eq? type &flonum) '%guard-fl)
     (else #f)))

  (define (unbox-op type var next-exp)
    (cond
     ((eq? type &flonum)
      `(let ((,var ,(to-double var)))
         ,next-exp))
     ((eq? type &exact-integer)
      `(let ((,var ,(to-fixnum var)))
         ,next-exp))
     (else
      next-exp)))

  (define (make-entry ip vars types loop-exp)
    (define (make-args vars)
      (let ((end (vector-length vars)))
        (let lp ((i 0) (acc '()))
          (cond
           ((= i end)
            (reverse! acc))
           ((vector-ref vars i)
            =>
            (lambda (var)
              (lp (+ i 1) (cons var acc))))
           (else
            (lp (+ i 1) acc))))))
    `(begin
       (,ip ,@(make-args vars))
       ,(let lp ((i 0) (end (vector-length types)))
          (if (< i end)
              (let* ((type (vector-ref types i))
                     (var (and type (vector-ref vars i)))
                     (op (and type (type-guard-op type)))
                     (exp (and op
                               `(begin
                                  (,op ,var)
                                  ,(unbox-op type var (lp (+ i 1) end))))))
                (or exp (lp (+ i 1) end)))
              loop-exp))))

  (define (initial-ip ops)
    (cadr (car ops)))

  (let* ((local-offset (- (lowest-offset ops)))
         (locals (accumulate-locals local-offset ops))
         (max-local-num (or (and (null? locals) 0) (apply max locals)))
         (args (map make-var (reverse locals)))
         (vars (make-vars max-local-num locals))
         (types (make-types vars))
         (snapshots (make-hash-table))
         (snapshot-id 0))

    (define-syntax-rule (push-offset! n)
      (set! local-offset (+ local-offset n)))

    (define-syntax-rule (pop-offset! n)
      (set! local-offset (- local-offset n)))

    (define-syntax-rule (set-type! idx ty)
      (vector-set! types (+ idx local-offset) ty))

    (define (convert-one escape op ip locals rest)

      (define (local-ref i)
        (vector-ref locals i))

      (define (var-ref i)
        ;; (debug 2 ";;; var-ref: op:~a i:~a local-offset:~a var:~a~%"
        ;;        op i local-offset vars)
        (let ((idx (+ i local-offset)))
          (and (< idx (vector-length vars))
               (vector-ref vars (+ i local-offset)))))

      (define (save-frame!)
        (let ((end (vector-length vars)))
          (debug 2 ";;; save-frame!: local-offset=~a~%" local-offset)
          (let lp ((i 0))
            (cond
             ((= i end) '())
             ((vector-ref vars i)
              =>
              (lambda (var)
                (let* ((idx (- i local-offset))
                       (local (and (< idx (vector-length locals))
                                   (vector-ref locals idx))))
                  (debug 2 ";;; save-frame!: local[~a]=~a~%" i local)
                  (cond
                   ((flonum? local)
                    (cons `(let ((,var (%from-double ,var)))
                             (%frame-set! ,i ,var))
                          (lp (+ i 1))))
                   ((fixnum? local)
                    (cons `(let ((,var ,(from-fixnum var)))
                             (%frame-set! ,i ,var))
                          (lp (+ i 1))))
                   (else
                    (cons `(%frame-set! ,i ,var) (lp (+ i 1))))))))
             (else
              (lp (+ i 1)))))))

      (define (load-frame exp)
        (let ((end (vector-length vars)))
          (let lp ((i 0))
            (cond
             ((= i end)
              exp)
             ((vector-ref vars i)
              =>
              (lambda (var)
                (let* ((idx (+ i local-offset))
                       (local (and (< idx (vector-length locals))
                                   (vector-ref locals idx))))
                  (cond
                   ((fixnum? local)
                    `(let ((,var (%frame-ref ,i)))
                       (let ((,var ,(to-fixnum var)))
                         ,(lp (+ i 1)))))
                   (else
                    `(let ((,var (%frame-ref ,i)))
                       ,(lp (+ i 1))))))))
             (else
              (lp (+ i 1)))))))

      (define (take-snapshot! offset)
        (let ((end (vector-length vars)))
          (debug 2 ";;; take-snapshot!:~%;;; locals=~a~%;;; vars=~a~%"
                 locals vars)
          (let lp ((i 0) (acc '()))
            (cond
             ((= i end)
              (let ((acc (reverse! acc)))
                ;; Using snapshot-id as key for hash-table. Identical IP could
                ;; be used for entry clause and first operation in the loop.
                (hashq-set! snapshots snapshot-id acc)
                (set! snapshot-id (+ snapshot-id 1))

                ;; Call to dummy procedure to capture CPS variables by emitting
                ;; `$call' term.  It might not good to use `$call' this way,
                ;; though no other idea to capture CPS variables with less
                ;; amount of terms.
                `(,(cond
                    ;; XXX: When offset=#f, regard as snapsot at the end of side
                    ;; exit. Use IP=0 to tell assembler that this is not a
                    ;; ordinal snapshot.
                    (offset (+ ip (* offset 4)))
                    (else 0))
                  ,@args)))
             ((var-ref i)
              =>
              (lambda (var)
                (let ((local (or (and (< i (vector-length locals))
                                      (local-ref i))
                                 #f)))
                  (cond
                   ((fixnum? local)
                    (lp (+ i 1) (cons `(,i . ,&exact-integer) acc)))
                   ((flonum? local)
                    (lp (+ i 1) (cons `(,i . ,&flonum) acc)))
                   ((variable? local)
                    (lp (+ i 1) (cons `(,i . ,&box) acc)))
                   ((not local)
                    (lp (+ i 1) (cons `(,i . ,&false) acc)))
                   (else
                    (debug 2 ";;; take-snapshot!: local=~a~%" local)
                    (lp (+ i 1) acc))))))
             (else
              (lp (+ i 1) acc))))))

      ;; (debug 2 "convert-one: ~a (~a/~a) ~a~%"
      ;;        ip local-offset max-local-num op)

      (match op
        ;; *** Call and return

        ;; XXX: halt

        (('call proc nlocals)
         (push-offset! proc)
         (convert escape rest))

        (('call-label proc nlocals label)
         (push-offset! proc)
         (convert escape rest))

        ;; XXX: tail-call
        ;; XXX: tail-call-label
        ;; XXX: tail-call/shuffle

        (('receive dst proc nlocals)
         (pop-offset! proc)
         (let ((vdst (var-ref dst))
               (vproc (var-ref proc)))
           `(let ((,vdst ,vproc))
              ,(convert escape rest))))

        (('receive-values proc allow-extra? nvalues)
         (pop-offset! proc)
         (escape #f))

        (('return src)
         (let ((vsrc (var-ref src))
               (vdst (var-ref 0)))
           (cond
            ((eq? vdst vsrc)
             (convert escape rest))
            (else
             `(let ((,vdst ,vsrc))
                ,(convert escape rest))))))

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

        ;; VM op `native-call' is specific to vm-tjit engine.
        ;;
        ;; XXX: Add a guard to check the returned ip. If it differs, recompile
        ;; native code.
        ;;

        ;; (('native-call addr)
        ;;  `(begin
        ;;     ,@(save-frame!)
        ;;     (%native-call ,addr)
        ;;     ,(load-frame (convert escape rest))))

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
         (convert escape rest))

        ;; XXX: br-if-npos-gt
        ;; XXX: bind-kw-args
        ;; XXX: bind-rest

        ;; *** Branching instructions

        (('br offset)
         (convert escape rest))

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
           (cond
            ((and (fixnum? ra) (fixnum? rb))
             (set-type! a &exact-integer)
             (set-type! b &exact-integer)
             `(begin
                ,(take-snapshot!
                  (if (= ra rb)
                      (if invert? offset 2)
                      (if invert? 2 offset)))
                ,(if (= ra rb) `(%eq ,va ,vb) `(%ne ,va ,vb))
                ,(convert escape rest)))
            (else
             (debug 2 "*** ir:convert = ~a ~a~%" ra rb)
             (escape #f)))))

        (('br-if-< a b invert? offset)
         (let ((ra (local-ref a))
               (rb (local-ref b))
               (va (var-ref a))
               (vb (var-ref b)))
           (cond
            ((and (fixnum? ra) (fixnum? rb))
             (set-type! a &exact-integer)
             (set-type! b &exact-integer)
             `(begin
                ,(take-snapshot!
                  (if (< ra rb)
                      (if invert? offset br-op-size)
                      (if invert? br-op-size offset)))
                ,(if (< ra rb) `(%lt ,va ,vb) `(%ge ,va ,vb))
                ,(convert escape rest)))

            ((and (flonum? ra) (flonum? rb))
             (set-type! a &flonum)
             (set-type! b &flonum)
             `(begin
                ,(take-snapshot!
                  (if (< ra rb)
                      (if invert? offset 2)
                      (if invert? 2 offset)))
                ,(if (< ra rb) `(%flt ,va ,vb) `(%fge ,va ,vb))
                ,(convert escape rest)))
            (else
             (debug 2 "ir:convert < ~a ~a~%" ra rb)
             (escape #f)))))

        ;; XXX: br-if-<=

        ;; *** Lexical binding instructions

        (('mov dst src)
         (let ((vdst (var-ref dst))
               (vsrc (var-ref src)))
           (cond
            ((flonum? (local-ref src)) (set-type! src &flonum))
            ((fixnum? (local-ref src)) (set-type! src &exact-integer)))
           `(let ((,vdst ,vsrc))
              ,(convert escape rest))))

        ;; XXX: long-mov
        ;; XXX: box

        ;; XXX: Reconsider how to manage `box', `box-ref', and `box-set!'.
        ;; Boxing back to tagged value every time will make the loop slow,
        ;; though need more analysis when the storing could be removed from
        ;; native code loop and delayed to side exit code.
        ;;
        ;; XXX: Add test for nested boxes.

        (('box-ref dst src)
         ;; XXX: Add guard to check for `variable' type?
         (let ((vdst (var-ref dst))
               (vsrc (var-ref src))
               (rsrc (and (< src (vector-length locals))
                          (variable-ref (vector-ref locals src)))))
           `(let ((,vdst (%cell-object ,vsrc 1)))
              ,(cond
                ((flonum? rsrc)
                 `(let ((,vdst ,(to-double vdst)))
                    ,(convert escape rest)))
                ((fixnum? rsrc)
                 `(let ((,vdst ,(to-fixnum vdst)))
                    ,(convert escape rest)))
                (else
                 (convert escape rest))))))

        (('box-set! dst src)
         (let ((vdst (var-ref dst))
               (vsrc (var-ref src))
               (rdst (and (< dst (vector-length locals))
                          (variable-ref (vector-ref locals dst)))))
           (cond
            ((flonum? rdst)
             `(let ((,vsrc (%from-double ,vsrc)))
                (%set-cell-object! ,vdst 1 ,vsrc)
                ,(convert escape rest)))
            ((fixnum? rdst)
             `(let ((,vsrc ,(from-fixnum vsrc)))
                (%set-cell-object! ,vdst 1 ,vsrc)
                ,(convert escape rest)))
            (else
             `(begin
                (%set-cell-object! ,vdst 1 ,vsrc)
                ,(convert escape rest))))))

        ;; XXX: make-closure
        ;; XXX: free-ref
        ;; XXX: free-set!

        ;; *** Immediates and statically allocated non-immediates

        (('make-short-immediate dst low-bits)
         `(let ((,(var-ref dst) ,(ash low-bits -2)))
            ,(convert escape rest)))

        (('make-long-immediate dst low-bits)
         `(let ((,(var-ref dst) ,(ash low-bits -2)))
            ,(convert escape rest)))

        (('make-long-long-immediate dst high-bits low-bits)
         `(let ((,(var-ref dst)
                 ,(ash (logior (ash high-bits 32) low-bits) -2)))
            ,(convert escape rest)))

        ;; XXX: make-long-long-immediate
        ;; XXX: make-non-immediate

        (('static-ref dst offset)
         `(let ((,(var-ref dst) ,(dereference-scm (+ ip (* 4 offset)))))
            ,(convert escape rest)))

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
              ,(convert escape rest))))

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

        (('add dst a b)
         (let ((rdst (local-ref dst))
               (ra (local-ref a))
               (rb (local-ref b))
               (vdst (var-ref dst))
               (va (var-ref a))
               (vb (var-ref b)))
           (cond
            ((and (fixnum? ra) (fixnum? rb))
             (set-type! a &exact-integer)
             (set-type! b &exact-integer)
             `(let ((,vdst (%add ,va ,vb)))
                ,(convert escape rest)))
            ((and (flonum? ra) (flonum? rb))
             (set-type! a &flonum)
             (set-type! b &flonum)
             `(let ((,vdst (%fadd ,va ,vb)))
                ,(convert escape rest)))
            (else
             (debug 2 "ir:convert add ~a ~a ~a~%" rdst ra rb)
             (escape #f)))))

        (('add1 dst src)
         (let ((rdst (local-ref dst))
               (rsrc (local-ref src))
               (vdst (var-ref dst))
               (vsrc (var-ref src)))
           (cond
            ((fixnum? rsrc)
             (set-type! src &exact-integer)
             `(let ((,vdst (%add ,vsrc 1)))
                ,(convert escape rest)))
            (else
             (debug 2 "ir:convert add1 ~a ~a" rdst rsrc)
             (escape #f)))))

        (('sub dst a b)
         (let ((rdst (local-ref dst))
               (ra (local-ref a))
               (rb (local-ref b))
               (vdst (var-ref dst))
               (va (var-ref a))
               (vb (var-ref b)))
           (cond
            ((and (fixnum? ra) (fixnum? rb))
             (set-type! a &exact-integer)
             (set-type! b &exact-integer)
             `(let ((,vdst (%sub ,va ,vb)))
                ,(convert escape rest)))
            ((and (flonum? ra) (flonum? rb))
             (set-type! a &flonum)
             (set-type! b &flonum)
             `(let ((,vdst (%fsub ,va ,vb)))
                ,(convert escape rest)))
            (else
             (debug 2 "ir:convert sub ~a ~a ~a~%" rdst ra rb)
             (escape #f)))))

        (('sub1 dst src)
         (let ((rdst (local-ref dst))
               (rsrc (local-ref src))
               (vdst (var-ref dst))
               (vsrc (var-ref src)))
           (cond
            ((fixnum? rsrc)
             (set-type! src &exact-integer)
             `(let ((,vdst (%sub ,vsrc 1)))
                ,(convert escape rest)))
            (else
             (debug 2 "ir:convert sub1 ~a ~a~%" rdst rsrc)
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
             (set-type! a &flonum)
             (set-type! b &flonum)
             `(let ((,vdst (%fmul ,va ,vb)))
                ,(convert escape rest)))
            (else
             (debug 2 "*** ir:convert: NYI mul ~a ~a ~a~%" rdst ra rb)
             (escape #f)))))

        ;; XXX: div
        ;; XXX: quo
        ;; XXX: rem
        ;; XXX: mod
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

        ;; *** Misc

        (('patch-to-native)
         (take-snapshot! #f))

        (op
         (debug 2 "*** ir:convert: NYI ~a~%" (car op))
         (escape #f))))

    (define (convert escape traces)
      ;; (debug 2 ";;; convert: ~a~%" traces)
      (match traces
        (((op ip _ locals) . ())
         (cond
          (nlog
           (convert-one escape '(patch-to-native) ip locals '()))
          (else
           (convert-one escape op ip locals
                        `(loop ,@(filter identity (vector->list vars)))))))
        (((op ip _ locals) . rest)
         (convert-one escape op ip locals rest))
        (_ traces)))

    ;; Debug.
    ;; (debug 2 ";;; max-local-num: ~a~%" max-local-num)
    ;; (let lp ((i 0) (end (vector-length types)))
    ;;   (when (< i end)
    ;;     (debug 2 "ty~a => ~a~%" i (vector-ref types i))
    ;;     (lp (+ i 1) end)))

    (define (make-scm entry-body loop-body)
      (cond
       ((and nlog loop-body)            ; Side trace.
        `(letrec ((loop (lambda ,args ,loop-body)))
           loop))
       ((and entry-body loop-body)      ; Root trace.
        `(letrec ((entry (lambda ,args ,entry-body))
                  (loop (lambda ,args ,loop-body)))
           entry))
       (else
        (debug 2 ";;; Bytecode to Scheme conversion failed.~%")
        #f)))

    (let* ((loop-body (call-with-escape-continuation
                       (lambda (escape)
                         (convert escape ops))))
           (entry-body (make-entry (initial-ip ops) vars types `(loop ,@args)))
           (scm (make-scm entry-body loop-body)))
      ;; Debug, again
      ;; (debug 2 ";;; entry-guards:~%~y" entry-guards)
      (debug 2 ";;; snapshot:~%~{;;; ~a~%~}~%" (hash-fold acons '() snapshots))
      (values locals snapshots scm))))

(define (scm->cps scm)
  (define ignored-passes
    '(#:prune-top-level-scopes? #f
      #:specialize-primcalls? #f
      #:type-fold? #f
      #:inline-constructors? #f))
  (define (body-fun cps)
    (define (go cps k)
      (match (intmap-ref cps k)
        (($ $kfun _ _ _ _ next)
         (go (intmap-remove cps k) next))
        (($ $kclause _ next _)
         (go (intmap-remove cps k) next))
        (($ $kargs _ _ ($ $continue next _ expr))
         (go (intmap-remove cps k) next))
        (($ $ktail)
         (values (+ k 1) (intmap-remove cps k)))
        (_
         (error "body-fun: got ~a" (intmap-ref cps k)))))
    (call-with-values
        (lambda ()
          (set! cps (optimize cps ignored-passes))
          (go cps 0))
      (lambda (k cps)
        (set! cps (renumber cps k))
        cps)))

  (let ((cps (and scm (compile scm #:from 'scheme #:to 'cps2))))
    (set! cps (and cps (body-fun cps)))
    cps))

(define (trace->cps nlog trace)
  (call-with-values (lambda () (trace->scm nlog trace))
    (lambda (locals snapshots scm)
      (values locals snapshots scm (scm->cps scm)))))
