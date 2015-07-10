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
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit variables)
  #:export (compile-tjit
            init-vm-tjit)
  #:re-export (tjit-stats))


;;;
;;; Compilation
;;;

(define (compile-tjit trace-id bytecode-ptr bytecode-len ips
                      parent-ip parent-exit-id)
  (define disassemble-one
    (@@ (system vm disassembler) disassemble-one))

  (define (traced-ops bytecode-ptr bytecode-len envs)
    (let ((bytecode (pointer->bytevector bytecode-ptr bytecode-len))
          (end (/ bytecode-len 4)))
      (let lp ((acc '())
               (offset 0)
               (envs (reverse! envs)))
        (match envs
          ((env . envs)
           (let-values (((len elt) (disassemble-one bytecode offset)))
             (match env
               ((ip _ local)
                (lp (cons (cons elt env) acc) (+ offset len) envs)))

             ;; XXX: Formerly, replaced opcode with vm-tjit specific
             ;; `native-call' when native code exists in recorded
             ;; trace.  When patching hote side exits are done, `bv'
             ;; element in trace may removed.

             ;; (match env
             ;;   ((ip #f local)
             ;;    (lp (cons (cons elt env) acc) (+ offset len) envs))
             ;;   ((ip bv local)
             ;;    (let* ((addr (pointer-address (bytevector->pointer bv)))
             ;;           (op `(native-call ,addr)))
             ;;      (lp (cons (cons op env) acc) (+ offset len) envs))))
             ))
          (()
           (reverse! acc))))))

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

    (define (mark-call cont)
      (match cont
        (($ $kargs _ _ ($ $continue _ _ ($ $call _ _)))
         "+")
        (_
         " ")))

    (define (mark-branch cont)
      (match cont
        (($ $kargs _ _ ($ $continue _ _ ($ $primcall name _)))
         (case name
           ((%eq %ne %lt %flt %le %ge %fge %guard-fx %guard-fl) ">")
           (else " ")))
        (_
         " ")))

    (define (make-indent n)
      (let lp ((n n) (acc '()))
        (if (< 0 n)
            (lp (- n 1) (cons #\. (cons #\space acc)))
            (list->string acc))))

    (define (lowest-level ip-x-ops)
      (let lp ((traces ip-x-ops) (level 0) (lowest 0))
        (match traces
          (((op _ _ . _) . traces)
           (case (car op)
             ((call call-label)
              (lp traces (+ level 1) lowest))
             ((return return-values subr-call foreign-call)
              (let ((level (- level 1)))
                (lp traces level (min level lowest))))
             (else
              (lp traces level lowest))))
          (() lowest))))

    (define-syntax-rule (show-debug-messages scm locals cps code code-size)
      (when (<= 2 verbosity)
        (let ((lowest (lowest-level ip-x-ops)))
          (format #t ";;; bytecode: ~a:~a:~a~%"
                  (length ip-x-ops) lowest (sort locals <))
          (let lp ((traces ip-x-ops) (level (- lowest)))
            (match traces
              (((op ip bv locals) . traces)
               (let ((op-val
                      (format #f "~x  ~a ~a~a"
                              ip (or (and bv "*") " ") (make-indent level) op)))
                 (if (<= 3 verbosity)
                     (format #t "~40a ; ~a~%" op-val locals)
                     (format #t "~a~%" op-val)))
               (case (car op)
                 ((call call-label)
                  (lp traces (+ level 1)))
                 ((return return-values subr-call foreign-call)
                  (lp traces (- level 1)))
                 (else
                  (lp traces level))))
              (() (values)))))
        ;; (format #t ";;; scm:~%~y" scm)
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
                 (format #t "~4,,,'0@a  ~a~a ~a~%" k
                         (mark-branch cont)
                         (mark-call cont)
                         (unparse-cps cont))
                 (match cont
                   (($ $kargs _ _ ($ $continue knext _ _))
                    (when (< knext k)
                      (format #t "---- ->loop~%")))
                   (_ (values)))
                 (lp conts)))))))
        (when (and code (<= 3 verbosity))
          (jit-print)
          (call-with-output-file
              (format #f "/tmp/trace-~x.o" (car (car ips)))
            (lambda (port)
              (let ((code-copy (make-bytevector code-size)))
                (bytevector-copy! code 0 code-copy 0 code-size)
                (put-bytevector port code-copy)))))))

    (let ((verbosity (lightning-verbosity)))
      (when (and verbosity (<= 2 verbosity))
        (let ((nlog (get-nlog parent-ip)))
          (and nlog (dump-nlog nlog)))))

    (with-jit-state
     (jit-prolog)
     (let-values (((locals snapshots scm cps) (trace->cps ip-x-ops)))
       (let ((sline (ip-ptr->source-line (car (car ips)))))
         (cond
          ((not cps)
           (debug 1 ";;; trace ~a:~a abort~%" trace-id sline)
           (debug 2 ";;; CPS conversion failed~%")
           #f)
          ((assemble-tjit locals snapshots cps)
           =>
           (lambda (loop-label)
             (jit-epilog)
             (jit-realize)
             (let* ((estimated-size (jit-code-size))
                    (code (make-bytevector estimated-size)))
               (jit-set-code (bytevector->pointer code) (imm estimated-size))
               (let* ((fptr (jit-emit))
                      (code-size (jit-code-size)))
                 (make-bytevector-executable! code)
                 (when (and verbosity (<= 1 verbosity))
                   (let ((nlog (get-nlog parent-ip)))
                     (format #t ";;; trace ~a:~a ~a~a~%"
                             trace-id sline code-size
                             (if (< 0 parent-ip)
                                 (format #f " (~a:~a)"
                                         (or (and nlog (nlog-id nlog))
                                             (format #f "~x" parent-ip))
                                         parent-exit-id)
                                 "")))
                   (show-debug-messages scm locals cps code code-size)))
               (let* ((ip (cadr (car ip-x-ops)))
                      (nlog (make-nlog trace-id code (make-hash-table)
                                       ip snapshots
                                       (jit-address loop-label) #f)))
                 (put-nlog! ip nlog))
               code)))
          (else
           (debug 1 ";;; trace ~a:~a abort~%" trace-id sline)
           (debug 2 ";;; Native code generation failed~%")
           #f)))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  (initialize-tjit-primitives)
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")
