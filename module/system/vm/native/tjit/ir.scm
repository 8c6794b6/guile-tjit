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

;;; Compile traced bytecode to CPS intermediate representation via
;;; Scheme in (almost) ANF format.

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
  #:use-module ((srfi srfi-1) #:select (every))
  #:use-module (srfi srfi-9)
  #:use-module (system base compile)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit primitives)
  #:export (trace->cps
            trace->scm
            scm->cps
            accumulate-locals))


;;;
;;; Type check
;;;

(define (fixnums? . args)
  (define (fixnum? n)
    (and (number? n)
         (exact? n)
         (< n most-positive-fixnum)
         (> n most-negative-fixnum)))
  (every fixnum? args))

(define (inexacts? a b)
  (and (inexact? a) (inexact? b)))


;;;
;;; Locals
;;;

(define (lowest-offset traces)
  (let lp ((traces traces) (offset 0) (lowest 0))
    (match traces
      (((op _ . _) . traces)
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
      (intset-add! st (+ i local-offset)))

    (define-syntax-rule (add2! st i j)
      (add! (add! st i) j))

    (define-syntax-rule (add3! st i j k)
      (add! (add2! st i j) k))

    (match op
      ((op a1)
       (case op
         ((return)
          (add! st a1))
         ((br)
          st)
         (else
          (nyi op)
          st)))
      ((op a1 a2)
       (case op
         ((call)
          (push-offset! a1)
          (add! st a1))
         ((static-ref
           make-short-immediate make-long-immediate make-long-long-immediate)
          (add! st a1))
         ((mov sub1 add1 box-ref)
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
          (push-offset! a1)
          (add! st a1))
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
          ;; Box will be removed during IR transformation, no need to store box
          ;; in native code
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
      (((op ip . locals) . rest)
       (acc (acc-one st op) rest))
      (()
       st)))

  ;; (debug 1 "accumulate-locals: lowest-offset=~a~%"
  ;;        (lowest-offset ops))

  (intset-fold cons (acc empty-intset ops) '()))

(define (trace->scm ops)
  (define (dereference-scm addr)
    (pointer->scm (dereference-pointer (make-pointer addr))))

  (define (save-frame! vars)
    (let ((end (vector-length vars)))
      (let lp ((i 0) (acc '()))
        (cond
         ((= i end)
          (reverse! acc))
         ((vector-ref vars i)
          =>
          (lambda (var)
            (lp (+ i 1) (cons `(%frame-set! ,i ,var) acc))))
         (else
          (lp (+ i 1) acc))))))

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

  (define (make-entry-guards ip vars types)
    (define (type-guard-op type)
      (cond
       ((eq? type &exact-integer) '%guard-fx)
       (else #f)))
    (let lp ((i 0) (end (vector-length types)) (acc '()))
      (if (< i end)
          (let* ((type (vector-ref types i))
                 (op (and type (type-guard-op type)))
                 (guard (and op (list op (vector-ref vars i) ip)))
                 (acc (or (and guard (cons guard acc)) acc)))
            (lp (+ i 1) end acc))
          (reverse! acc))))

  (define (make-entry-body guards end)
    (define (go guards)
      (match guards
        (() end)
        (((op var ip) . rest)
         `(if (,op ,var)
              ,ip
              ,(go rest)))))
    (go guards))

  (define (initial-ip ops)
    (cadr (car ops)))

  (let* ((local-offset (- (lowest-offset ops)))
         (locals (accumulate-locals local-offset ops))
         (max-local-num (or (and (null? locals) 0) (1+ (apply max locals))))
         (args (map make-var (reverse locals)))
         (vars (make-vars max-local-num locals))
         (types (make-types vars))
         (boxes (make-vector max-local-num #f)))

    (define-syntax-rule (push-offset! n)
      (set! local-offset (+ local-offset n)))

    (define-syntax-rule (pop-offset! n)
      (set! local-offset (- local-offset n)))

    (define (convert-one escape op ip locals rest)

      (define (local-ref i)
        (vector-ref locals i))

      (define (var-ref i)
        (vector-ref vars (+ i local-offset)))

      ;; (debug 2 "convert-one: ~a (~a/~a) ~a~%"
      ;;        ip local-offset max-local-num op)

      (match op
        ;; *** Call and return

        ;; XXX: halt

        (('call proc nlocals)
         (push-offset! proc)
         (convert escape rest))

        ;; XXX: call-label
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

        ;; XXX: receive-values
        (('receive-values proc allow-extra? nvalues)
         (pop-offset! proc)
         (escape #f))
        ;; XXX: return

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
        ;; XXX: foreign-callp
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
        ;; XXX: assert-nargs-ee/locals
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
            ((fixnums? ra rb)
             (vector-set! types a &exact-integer)
             (vector-set! types b &exact-integer)
             `(if ,(if invert? `(not (= ,va ,vb)) `(= ,va ,vb))
                  (begin
                    ,@(save-frame! vars)
                    ,ip)
                  ,(convert escape rest)))
            (else
             (debug 2 "ir:convert = ~a ~a~%" ra rb)
             (escape #f)))))

        (('br-if-< a b invert? offset)
         (let ((ra (local-ref a))
               (rb (local-ref b))
               (va (var-ref a))
               (vb (var-ref b)))
           (cond
            ((fixnums? ra rb)
             (vector-set! types a &exact-integer)
             (vector-set! types b &exact-integer)
             `(if ,(if invert? `(%fx< ,vb ,va) `(%fx< ,va ,vb))
                  (begin
                    ,@(save-frame! vars)
                    ,ip)
                  ,(convert escape rest)))
            (else
             (debug 2 "ir:convert < ~a ~a~%" ra rb)
             (escape #f)))))

        ;; XXX: br-if-<=

        ;; *** Lexical binding instructions

        (('mov dst src)
         (let ((vdst (var-ref dst))
               (vsrc (var-ref src)))
           `(let ((,vdst ,vsrc))
              ,(convert escape rest))))

        ;; XXX: long-mov
        ;; XXX: box

        (('box-ref dst src)
         ;; Calling `%address-ref' primitive. Contents of box may change, but
         ;; assuming that the addresss of the box won't change. Need to perform
         ;; `load' operation every time, though.
         ;;
         ;; XXX: Need to add guards for later operations? Box contents may have
         ;; different type after toplevel `set!'.
         ;;
         ;; XXX: Add guard to check for is `variable' type?
         ;;
         (let ((vdst (var-ref dst))
               (vsrc (var-ref src))
               (box (vector-ref boxes src)))
           ;; (debug 2 "box-ref: box=~a~%" box)
           `(let ((,vdst (%address-ref
                          ,(+ (pointer-address (scm->pointer box)) %word-size))))
              ,(convert escape rest))))

        ;; XXX: box-set!
        ;; XXX: make-closure
        ;; XXX: free-ref
        ;; XXX: free-set!

        ;; *** Immediates and statically allocated non-immediates

        (('make-short-immediate dst low-bits)
         `(let ((,(var-ref dst) ,low-bits))
            ,(convert escape rest)))

        (('make-long-immediate dst low-bits)
         `(let ((,(var-ref dst) ,low-bits))
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
               (var (dereference-scm (+ ip (* var-offset 4)))))
           ;; (debug 2 "toplevel-box: var=~a~%" var)
           (vector-set! boxes dst var)
           (convert escape rest)))

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
            ((fixnums? ra rb)
             (vector-set! types a &exact-integer)
             (vector-set! types b &exact-integer)
             `(let ((,vdst (%fxadd ,va ,vb)))
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
            ((fixnums? rdst rsrc)
             (vector-set! types src &exact-integer)
             `(let ((,vdst (%fxadd1 ,vsrc)))
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
            ((fixnums? rdst ra rb)
             `(let ((,vdst (%fxsub ,va ,vb)))
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
            ((fixnums? rdst rsrc)
             (vector-set! types src &exact-integer)
             `(let ((,vdst (%fxsub1 ,vsrc)))
                ,(convert escape rest)))
            (else
             (debug 2 "ir:convert sub1 ~a ~a" rdst rsrc)
             (escape #f)))))

        ;; (('mul dst a b)
        ;;  (let ((vdst (var-ref dst))
        ;;        (va (var-ref a))
        ;;        (vb (var-ref b)))
        ;;    `(let ((,vdst (* ,va ,vb)))
        ;;       ,(convert escape rest))))

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

        (op
         (debug 2 "*** ir:convert: NYI ~a~%" (car op))
         (escape #f))))

    (define (convert escape traces)
      (match traces
        (((op ip . locals) . rest)
         (convert-one escape op ip locals rest))
        (()
         `(loop ,@(filter identity (vector->list vars))))))

    ;; Debug.
    (debug 2 ";;; max-local-num: ~a~%" max-local-num)
    (debug 2 ";;; locals: ~a~%" (sort locals <))
    (let lp ((i 0) (end (vector-length types)))
      (when (< i end)
        (debug 2 "ty~a => ~a~%" i (vector-ref types i))
        (lp (+ i 1) end)))

    (let* ((loop-body (call-with-escape-continuation
                       (lambda (escape)
                         (convert escape ops))))
           (entry-guards (make-entry-guards (initial-ip ops) vars types))
           (entry-body (make-entry-body entry-guards `(loop ,@args)))
           (scm (and entry-body
                     loop-body
                     `(letrec ((entry (lambda ,args
                                        ,entry-body))
                               (loop (lambda ,args
                                       ,loop-body)))
                        entry))))
      ;; Debug, again
      (debug 2 ";;; entry-guards:~%~y" entry-guards)
      (debug 2 ";;; scm:~%~a"
             (call-with-output-string
              (lambda (port) (pretty-print scm #:port port))))

      (values locals scm))))

(define (scm->cps scm)
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
          (set! cps (optimize cps))
          (set! cps (renumber cps))
          (go cps 0))
      (lambda (k cps)
        (set! cps (renumber cps k))
        cps)))

  (let ((cps (and scm (compile scm #:from 'scheme #:to 'cps2))))
    (set! cps (and cps (body-fun cps)))
    cps))

(define (trace->cps trace)
  (call-with-values (lambda () (trace->scm trace))
    (lambda (locals scm)
      ;; (debug 2 ";;; scm~%~a"
      ;;        (or (and scm
      ;;                 (call-with-output-string
      ;;                  (lambda (port)
      ;;                    (pretty-print scm #:port port))))
      ;;            "failed\n"))
      (values locals scm (scm->cps scm)))))
