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
  #:use-module (ice-9 pretty-print)
  #:use-module (language cps intmap)
  #:use-module (language cps2)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit primitives)
  #:use-module (system vm native tjit variables)
  #:export (compile-tjit
            init-vm-tjit)
  #:re-export (tjit-stats))


;;;
;;; Compilation
;;;

(define (compile-tjit trace-id bytecode-ptr bytecode-len ips)
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

    (define (mark-branch cont)
      (match cont
        (($ $kargs _ _ ($ $continue _ _ ($ $branch _ _)))
         ">")
        (_
         " ")))

    (define-syntax-rule (show-debug-messages scm cps code code-size)
      (when (<= 2 verbosity)
        (display ";;; bytecode\n")
        (for-each (match-lambda
                   ((op ip . locals)
                    ;; (when (<= 3 verbosity)
                    ;;   (format #t "~a~%" locals))
                    (format #t "~x  ~a~%" ip op)))
                  ip-x-ops)
        (format #t ";;; scm:~%~a"
                (call-with-output-string
                 (lambda (port) (pretty-print scm #:port port))))
        (display ";;; cps\n")
        (cond
         ((not cps)
          (display "#f\n"))
         (else
          (let ((kstart (loop-start cps)))
            (let lp ((conts (reverse! (intmap-fold acons cps '()))))
              (match conts
                (() #f)
                (((k . cont) . conts)
                 (and (= k kstart) (format #t "---- loop:~%"))
                 (format #t "~4,,,'0@a  ~a ~a~%"
                         k (mark-branch cont) (unparse-cps cont))
                 (match cont
                   (($ $kargs _ _ ($ $continue knext _ _))
                    (when (< knext k)
                      (format #t "---- ->loop~%")))
                   (_ #f))
                 (lp conts)))))
          ;; (display ";;; dfg")
          ;; ((@@ (language cps dfg) dump-dfg)
          ;;  ((@@ (language cps dfg) compute-dfg)
          ;;   ((@@ (system base compile) compile) cps #:from 'cps2 #:to 'cps)))
          ))
        ;; (format #t ";;; cps~%~{~4,,,'0@a  ~a~%~}"
        ;;         (or (and cps (reverse! (intmap-fold
        ;;                                 (lambda (i k acc)
        ;;                                   (cons (unparse-cps k)
        ;;                                         (cons i acc)))
        ;;                                 cps
        ;;                                 '())))
        ;;             '()))
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
     (let-values (((locals scm cps) (trace->cps ip-x-ops)))
       (cond
        ((if cps (assemble-tjit locals cps) "CPS conversion failed")
         =>
         (lambda (msg)
           (let ((sline (ip-ptr->source-line (car (car ips)))))
             (debug 1 ";;; trace ~a: ~a ~a~%" trace-id sline "abort")
             (debug 2 ";;; msg: ~a~%" msg)
             (show-debug-messages scm cps #f #f))
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
                 (format #t ";;; trace ~a: ~a size=~a~%"
                         trace-id sline (number->string code-size)))
               (show-debug-messages scm cps code code-size)))
           code)))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  (initialize-tjit-primitives)
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")
