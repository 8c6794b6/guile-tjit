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

;;; JIT compiler for `vm-tjit' engine.

(define-module (system vm native tjit)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 binary-ports)
  #:use-module (language cps intmap)
  #:use-module (language cps2)
  #:use-module (language cps2 optimize)
  #:use-module (language cps2 renumber)
  #:use-module (language cps2 dce)
  #:use-module (language scheme spec)
  #:use-module (rnrs bytevectors)
  #:use-module (system base compile)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit ir)
  #:autoload (ice-9 regex) (regexp-substitute/global)
  #:export (tjit-ip-counter
            tjit-hot-count
            set-tjit-hot-count!
            tjit-stats
            init-vm-tjit))


;;;
;;; Fetching bytecodes
;;;

(define disassemble-one
  (@@ (system vm disassembler) disassemble-one))

(define (traced-ops bytecode-ptr bytecode-len ips)
  (let ((bytecode (pointer->bytevector bytecode-ptr bytecode-len))
        (end (/ bytecode-len 4)))
    (let lp ((acc '()) (bytecode-offset 0) (ips (reverse! ips)))
      (if (< bytecode-offset end) ;; (not (null? ips))
          (call-with-values
              (lambda () (disassemble-one bytecode bytecode-offset))
            (lambda (len elt)
              (let* ((env (car ips)))
                (lp (cons (cons elt env) acc)
                    (+ bytecode-offset len)
                    (cdr ips)))))
          (reverse! acc)))))

;;;
;;; Type check
;;;

(define (fixnums? a b)
  (define (fixnum? n)
    (and (exact? n)
         (< n most-positive-fixnum)
         (> n most-negative-fixnum)))
  (and (fixnum? a) (fixnum? b)))

(define (inexacts? a b)
  (and (inexact? a) (inexact? b)))


;;;
;;; Generating native code
;;;

(define reg-thread v0)
(define reg-vp v1)
(define reg-registers v2)
(define reg-fp v3)
(define word-size (sizeof '*))

(define-syntax local-ref
  (syntax-rules ()
    ((_ dst 0)
     (jit-ldr dst reg-fp))
    ((_ dst n)
     (jit-ldxi dst reg-fp (imm (* n word-size))))))

(define-syntax local-set!
  (syntax-rules ()
    ((_ 0 src)
     (jit-str reg-fp src))
    ((_ n src)
     (jit-stxi (imm (* n word-size)) reg-fp src))))

(define-syntax jump
  (syntax-rules ()
    ((_ label)
     (jit-patch-at (jit-jmpi) label))
    ((_ condition label)
     (jit-patch-at condition label))))

(define-syntax call-c
  (syntax-rules ()
    ((_ cfunc)
     (call-c cfunc r0))
    ((_ cfunc tmp)
     ;; Explicitly moving the address to a register.  In x86-64,
     ;; lightning's `jit-calli' function is moving the absolute address
     ;; to register, then emitting `call' opcode, which sometimes
     ;; overwrite non-volatile register used in VM.
     (begin
       (jit-movi tmp cfunc)
       (jit-callr tmp)))))

(define-syntax define-cfunc
  (lambda (x)
    (define (sname cname)
      (let ((sname (regexp-substitute/global #f "_" (syntax->datum cname)
                                             'pre "-" 'post)))
        (string->symbol (string-append/shared "%" sname))))
    (syntax-case x ()
      ((_ cname)
       #`(define #,(datum->syntax x (sname #'cname))
           (dynamic-pointer cname (dynamic-link)))))))

(define-cfunc "scm_do_inline_from_double")
(define-cfunc "scm_sum")
(define-cfunc "scm_difference")

(define-syntax scm-cell-object
  (syntax-rules ()
    ((_ dst obj 0)
     (jit-ldr dst obj))
    ((_ dst obj 1)
     (jit-ldxi dst obj (imm word-size)))
    ((_ dst obj n)
     (jit-ldxi dst obj (imm (* n word-size))))))

(define-syntax-rule (scm-bytevector-contents dst obj)
  (scm-cell-object dst obj 2))

(define-syntax-rule (scm-i-inumr dst src)
  (jit-rshi dst src (imm 2)))

(define-syntax-rule (br-binary-arithmetic locals a b ip fxop flop)
  (let ((lnext (jit-forward))
        (va (vector-ref locals a))
        (vb (vector-ref locals b)))

    (local-ref r0 a)
    (local-ref r1 b)

    ;; XXX: Add guards.
    (cond
     ((fixnums? va vb)
      (jump (fxop r0 r1) lnext))

     ((inexacts? va vb)
      (scm-real-value f0 r0)
      (scm-real-value f1 r1)
      (jump (flop f0 f1) lnext))

     (else
      ;; XXX: Call C function.
      #f))

    (jit-reti (imm ip))
    (jit-link lnext)))

(define-syntax-rule (scm-inline-from-double dst obj)
  (begin
    (jit-prepare)
    (jit-pushargr reg-thread)
    (jit-pushargr-d obj)
    (call-c %scm-do-inline-from-double)
    (jit-retval dst)))

(define-syntax-rule (scm-real-value dst src)
  (jit-ldxi-d dst src (imm (* 2 word-size))))

(define (assemble-tjit-one escape op ip locals)
  (define (runtime-ref i)
    (vector-ref locals i))
  (define (dereference-scm pointer)
    (pointer->scm (dereference-pointer pointer)))
  (match op
    (('make-short-immediate dst low-bits)
     (jit-movi r0 (imm low-bits))
     (local-set! dst r0))

    (('make-long-immediate dst low-bits)
     (jit-movi r0 (imm low-bits))
     (local-set! dst r0))

    (('static-ref dst offset)
     (jit-ldi r0 (imm (+ ip (* 4 offset))))
     (local-set! dst r0))

    (('toplevel-box dst var-offset mod-offset sym-offset bound?)
     (let* ((ptr1 (make-pointer (+ ip (* var-offset 4))))
            (var1 (dereference-scm ptr1)))
       (when (variable? var1)
         (jit-movi r0 ptr1)
         (local-set! dst r0))))

    (('box-ref dst src)
     (local-ref r0 src)
     (scm-cell-object r0 r0 1)
     (local-set! dst r0))

    (('br-if-= a b invert? offset)
     (cond
      (invert?
       (br-binary-arithmetic locals a b (+ ip (* offset 4))
                             jit-beqr jit-beqr-d))
      (else
       (br-binary-arithmetic locals a b (+ ip (* offset 4))
                             jit-bner jit-bner-d))))

    (('br-if-< a b invert? offset)
     (cond
      (invert?
       (br-binary-arithmetic locals a b (+ ip (* offset 4))
                             jit-bltr jit-bltr-d))
      (else
       (br-binary-arithmetic locals b a (+ ip (* offset 4))
                             jit-bltr jit-bltr-d))))

    (('br dst)
     *unspecified*)

    (('mov dst src)
     (local-ref r0 src)
     (local-set! dst r0))

    (('add dst a b)
     (local-ref r0 a)
     (local-ref r1 b)
     (jit-addr r0 r0 r1)
     (jit-subi r0 r0 (imm 2))
     (local-set! dst r0))

    (('add1 dst src)
     (local-ref r0 src)
     (jit-addi r0 r0 (imm 4))
     (local-set! dst r0))

    (('sub dst a b)
     (let ((va (runtime-ref a))
           (vb (runtime-ref b)))
       (local-ref r0 a)
       (local-ref r1 b)

       ;; XXX: Add guards.
       (cond
        ((fixnums? va vb)
         (jit-subr r0 r0 r1)
         (jit-addi r0 r0 (imm 2)))

        ((inexacts? va vb)
         (scm-real-value f0 r0)
         (scm-real-value f1 r1)
         (jit-subr-d f0 f0 f1)
         (scm-inline-from-double r0 f0))

        (else
         (jit-prepare)
         (jit-pushargr r0)
         (jit-pushargr r1)
         (call-c %scm-difference)
         (jit-retval r0)))
       (local-set! dst r0)))

    (('sub1 dst src)
     (local-ref r0 src)
     (jit-subi r0 r0 (imm 4))
     (local-set! dst r0))

    (_
     (escape (format #f "#x~x ~a" ip op)))))

(define-syntax-rule (ip-ptr->source-line addr)
  (and=>
   (find-source-for-addr addr)
   (lambda (source)
     (format #f "~a:~d"
             (or (source-file source)
                 "(unknown file)")
             (source-line-for-user source)))))

(define-syntax-rule (assemble-tjit loop-start ip-x-ops)
  (call-with-escape-continuation
   (lambda (escape)
     (let lp ((ip-x-ops ip-x-ops))
       (match ip-x-ops
         (((op ip . locals) . ip-x-ops)
          (assemble-tjit-one escape op ip locals)
          (lp ip-x-ops))
         (()
          (jump loop-start)
          #f))))))

(define (compile-tjit bytecode-ptr bytecode-len ips)
  (let ((ip-x-ops (traced-ops bytecode-ptr bytecode-len ips))
        (verbosity (lightning-verbosity)))

    (define-syntax-rule (debug n fmt . args)
      (when (and verbosity (<= n verbosity))
        (format #t fmt . args)))

    (define (dump-cps2 cps2)
      (intmap-fold (lambda (i k acc)
                     (format #t "~4,,,'0@a  ~s~%" i
                             ((@@ (language cps2) unparse-cps) k)))
                   cps2
                   '()))

    (define (tidy-cps2 cps2)
      (renumber cps2))

    (define (loop-body cps)
      (define (work cps k)
        (match (intmap-ref cps k)
          (($ $kfun _ _ _ _ next)
           (work (intmap-remove cps k) next))
          (($ $kclause _ next _)
           (work (intmap-remove cps k) next))
          (($ $kargs _ _ ($ $continue next _ expr))
           (work (intmap-remove cps k) next))
          (($ $ktail)
           (values (+ k 1) (intmap-remove cps k)))
          (_
           (error "loop-body: got ~a" (intmap-ref cps k)))))
      (call-with-values
          (lambda ()
            (set! cps (optimize cps))
            (set! cps (renumber cps))
            (work cps 0))
        (lambda (k cps)
          (set! cps (renumber cps k))
          cps)))

    (let ((sline (ip-ptr->source-line (car (car ips)))))
      (debug 1 "~a: ~a~%" (yellow "trace") sline))

    (when (and verbosity (<= 2 verbosity))
      (display ";;;; bytecode\n")
      (for-each (match-lambda
                 ((op ip . locals)
                  (when (<= 3 verbosity)
                    (format #t "~a~%" locals))
                  (format #t "#x~x  ~a~%" ip op)))
                ip-x-ops)
      (let* ((ops-only (map car ip-x-ops))
             (scm (bytecode->scm ops-only))
             (cps (and scm (compile scm #:from 'scheme #:to 'cps2))))
        (display ";;;; scm\n")
        (pretty-print scm #:width 78)
        (display ";;;; cps\n")
        (or (and cps (dump-cps2 (loop-body cps)))
            (format #t "~a~%" cps))
        (display ";;;; locals\n")
        (format #t "~a~%" (cdr (cdr (car ip-x-ops))))))

    (with-jit-state
     (jit-prolog)

     ;; Get arguments.
     (jit-getarg reg-thread (jit-arg))    ; *thread
     (jit-getarg reg-vp (jit-arg))        ; *vp
     (jit-getarg reg-registers (jit-arg)) ; registers, for prompt
     (jit-getarg r0 (jit-arg))            ; resume

     ;; Load vp->fp to register.
     (jit-ldxi reg-fp reg-vp (imm #x10))

     (let ((loop-start (jit-label)))
       (cond
        ((assemble-tjit loop-start ip-x-ops)
         =>
         (lambda (msg)
           (debug 1 "~a: ~a~%" (red "abort") msg)
           #f))

        (else
         (jit-epilog)
         (jit-realize)
         (let* ((estimated-code-size (jit-code-size))
                (code (make-bytevector estimated-code-size)))
           (jit-set-code (bytevector->pointer code) (imm estimated-code-size))
           (let ((fptr (jit-emit)))
             (make-bytevector-executable! code)
             (let* ((code-size (jit-code-size)))
               (debug 1 "~a: addr=#x~x, size=~a~%" (green "ncode")
                      (pointer-address fptr) code-size)
               (when (and verbosity (<= 3 verbosity))
                 (jit-print)
                 (call-with-output-file
                     (format #f "/tmp/trace-~x.o" (car (car ips)))
                   (lambda (port)
                     (let ((code-copy (make-bytevector code-size)))
                       (bytevector-copy! code 0 code-copy 0 code-size)
                       (put-bytevector port code-copy)))))))
           code)))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_tjit")


;;;
;;; Statistics
;;;

(define (tjit-stats)
  (let ((num-loops 0)
        (num-hot-loops 0)
        (hot-count (tjit-hot-count)))
    (hash-fold (lambda (k v acc)
                 (set! num-loops (+ num-loops 1))
                 (when (< hot-count v)
                   (set! num-hot-loops (+ num-hot-loops 1))))
               '()
               (tjit-ip-counter))
    (list `(hot-count . ,hot-count)
          `(num-loops . ,num-loops)
          `(num-hot-loops . ,num-hot-loops))))
