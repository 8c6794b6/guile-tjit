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
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps intmap)
  #:use-module (language cps2)
  #:use-module (language cps2 optimize)
  #:use-module (language cps2 renumber)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit primitives)
  #:export (compile-tjit
            tjit-ip-counter
            tjit-hot-count
            set-tjit-hot-count!
            tjit-stats
            init-vm-tjit))


;;;
;;; Compilation
;;;

(define (compile-tjit bytecode-ptr bytecode-len ips)
  (define disassemble-one
    (@@ (system vm disassembler) disassemble-one))

  (define (traced-ops bytecode-ptr bytecode-len ips)
    (let ((bytecode (pointer->bytevector bytecode-ptr bytecode-len))
          (end (/ bytecode-len 4)))
      (let lp ((acc '())
               (bytecode-offset 0)
               (ips (reverse! ips)))
        (if (not (null? ips)) ;; (< bytecode-offset end)
            (call-with-values
                (lambda () (disassemble-one bytecode bytecode-offset))
              (lambda (len elt)
                (let ((env (car ips)))
                  (lp (cons (cons elt env) acc)
                      (+ bytecode-offset len)
                      (cdr ips)))))
            (reverse! acc)))))

  (let ((ip-x-ops (traced-ops bytecode-ptr bytecode-len ips))
        (verbosity (lightning-verbosity)))

    (define-syntax-rule (debug n fmt . args)
      (when (and verbosity (<= n verbosity))
        (format #t fmt . args)))

    (define-syntax-rule (ip-ptr->source-line addr)
      (and=>
       (find-source-for-addr addr)
       (lambda (source)
         (format #f "~a:~d"
                 (or (source-file source) "(unknown file)")
                 (source-line-for-user source)))))

    (define-syntax-rule (show-debug-messages cps code code-size)
      (when (<= 2 verbosity)
        (display ";;; bytecode\n")
        (for-each (match-lambda
                   ((op ip . locals)
                    ;; (when (<= 3 verbosity)
                    ;;   (format #t "~a~%" locals))
                    (format #t "~x  ~a~%" ip op)))
                  ip-x-ops)
        (format #t ";;; cps~%~{~4,,,'0@a  ~a~%~}"
                (or (and cps (reverse! (intmap-fold
                                        (lambda (i k acc)
                                          (cons (unparse-cps k)
                                                (cons i acc)))
                                        cps
                                        '())))
                    '()))
        (when (and code (<= 3 verbosity))
          (jit-print)
          (call-with-output-file
              (format #f "/tmp/trace-~x.o" (car (car ips)))
            (lambda (port)
              (let ((code-copy (make-bytevector code-size)))
                (bytevector-copy! code 0 code-copy 0 code-size)
                (put-bytevector port code-copy)))))))

    (with-jit-state
     (jit-prolog)
     (let-values (((locals cps) (trace->cps ip-x-ops)))
       (cond
        ((if cps (assemble-tjit locals cps) "CPS conversion failed")
         =>
         (lambda (msg)
           (let ((sline (ip-ptr->source-line (car (car ips)))))
             (debug 1 ";;; trace: ~a ~a~%" sline "abort")
             (debug 2 ";;; msg: ~a~%" msg)
             (show-debug-messages #f #f #f))
           #f))
        (else
         (jit-epilog)
         (jit-realize)
         (let* ((estimated-code-size (jit-code-size))
                (code (make-bytevector estimated-code-size)))
           (jit-set-code (bytevector->pointer code) (imm estimated-code-size))
           (let* ((fptr (jit-emit))
                  (code-size (jit-code-size)))
             (make-bytevector-executable! code)
             (when (and verbosity (<= 1 verbosity))
               (let ((sline (ip-ptr->source-line (car (car ips)))))
                 (format #t ";;; trace: ~a size=~a~%"
                         sline (number->string code-size)))
               (show-debug-messages cps code code-size)))
           code)))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_tjit")

(initialize-tjit-primitives)


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
