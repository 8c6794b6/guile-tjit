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
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system vm native lightning)
  #:use-module (system vm native debug)
  #:export (tjit-ip-counter
            tjit-hot-count
            set-tjit-hot-count!
            tjit-stats
            init-vm-tjit))


;;;
;;; Compiler
;;;

(define disassemble-one
  (@@ (system vm disassembler) disassemble-one))

(define (traced-ops bytecode-ptr bytecode-len ips-ptr ips-len)
  (let ((bytecode (pointer->bytevector bytecode-ptr bytecode-len))
        (ips (pointer->bytevector ips-ptr (* ips-len (sizeof '*))))
        (end (/ bytecode-len 4)))
    (let lp ((acc '()) (bytecode-offset 0) (ips-offset 0))
      (if (< bytecode-offset end)
          (call-with-values
              (lambda () (disassemble-one bytecode bytecode-offset))
            (lambda (len elt)
              (let ((ip (bytevector-u64-native-ref ips ips-offset)))
                (lp (cons (cons ip elt) acc)
                    (+ bytecode-offset len)
                    (+ ips-offset (sizeof '*))))))
          (reverse! acc)))))

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
     (jit-str reg-fp dst))
    ((_ n src)
     (jit-stxi (imm (* n word-size)) reg-fp src))))

(define-syntax jump
  (syntax-rules ()
    ((_ label)
     (jit-patch-at (jit-jmpi) label))
    ((_ condition label)
     (jit-patch-at condition label))))

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

(define-syntax-rule (br-binary-arithmetic a b invert? offset ip <op1> <op2>)
  (let ((lnext (jit-forward)))
    (local-ref r0 a)
    (local-ref r1 b)
    (cond (invert?
           (jump (<op1> r0 r1) lnext))
          (else
           (jump (<op2> r0 r1) lnext)))
    (jit-reti (imm (+ ip (* offset 4))))

    (jit-link lnext)))

(define-syntax-rule (assemble-tjit ip op)
  (match op
    (('make-short-immediate dst low-bits)
     (jit-movi r0 (imm low-bits))
     (local-set! dst r0))

    (('br-if-= a b invert? offset)
     (br-binary-arithmetic a b invert? offset ip jit-beqr jit-bner))

    (('br-if-< a b invert? offset)
     (br-binary-arithmetic a b invert? offset ip jit-bltr jit-bger))

    (('br dst)
     *unspecified*)

    (('mov dst src)
     (local-ref r0 src)
     (local-set! dst r0))

    ;; XXX: Arithmetic ops work for small fixnums only, no overflow
    ;; check. Observe the types from traced bytecodes, emit arithmetic
    ;; ops with the type.

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

    (('sub1 dst src)
     (local-ref r0 src)
     (jit-subi r0 r0 (imm 4))
     (local-set! dst r0))

    ;; No index bound check.
    (('bv-u8-ref dst src idx)
     (local-ref r0 src)
     (local-ref r1 idx)
     (scm-i-inumr r1 r1)
     (scm-bytevector-contents r0 r0)
     (jit-ldxr-uc r0 r0 r1)
     (local-set! dst r0))

    (_
     ;; (format #t ";;; NYI: ~a~%" (car op))
     #f)))

(define native-code-guardian (make-guardian))

(define (compile-tjit bytecode-ptr bytecode-len ips-ptr ips-len)
  (let ((ip-x-ops (traced-ops bytecode-ptr bytecode-len ips-ptr ips-len))
        (verbosity (lightning-verbosity)))
    (define-syntax-rule (debug n fmt . args)
      (when (and verbosity (<= n verbosity))
        (format #t fmt . args)))
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
       (debug 2 ";;; Traced bytecode:\n")
       (let lp ((ip-x-ops ip-x-ops))
         (match ip-x-ops
           (((ip . op) . ip-x-ops)
            (debug 2 "#x~x: ~a~%" ip op)
            (assemble-tjit ip op)
            (lp ip-x-ops))
           (()
            (jump loop-start)))))

     (jit-epilog)
     (jit-realize)
     (let* ((estimated-code-size (jit-code-size))
            (code (make-bytevector estimated-code-size)))
       (debug 2 ";;; estimated-code-size: ~a~%" estimated-code-size)
       ;; (native-code-guardian code)
       (jit-set-code (bytevector->pointer code)
                     (imm estimated-code-size))

       ;; (jit-print)

       (jit-emit)
       (make-bytevector-executable! code)

       (let* ((size (jit-code-size)))
         (debug 2 ";;; actual-code-size:    ~a~%" size)
         ;; (call-with-output-file "/tmp/trace.o"
         ;;   (lambda (port)
         ;;     (put-bytevector port code)))
         code)))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_tjit")


;;;
;;; Debugging
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
