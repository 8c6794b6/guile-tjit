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
  #:use-module (system vm native tjit tlog)
  #:use-module (system vm native tjit variables)
  #:export (compile-tjit
            init-vm-tjit)
  #:re-export (tjit-stats))


;;;
;;; Showing IR dump
;;;

(define (show-dump ip-x-ops scm locals cps code code-size)
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

  (let ((verbosity (lightning-verbosity)))
    (when (<= 2 verbosity)
      (let ((lowest (lowest-level ip-x-ops)))
        (format #t ";;; bytecode: ~a:~a:~a~%"
                (length ip-x-ops) lowest (and locals (sort locals <)))
        (let lp ((traces ip-x-ops) (level (- lowest)))
          (match traces
            (((op ip fp locals) . traces)
             (let ((op-val (format #f "~x  ~a~a" ip (make-indent level) op)))
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
      (format #t ";;; scm:~%~y" scm)
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
               (and (eq? k kstart) (format #t "---- loop:~%"))
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
            (format #f "/tmp/trace-~x.o" (cadr (car ip-x-ops)))
          (lambda (port)
            (let ((code-copy (make-bytevector code-size)))
              (bytevector-copy! code 0 code-copy 0 code-size)
              (put-bytevector port code-copy))))))))


;;;
;;; Compilation
;;;

(define (compile-tjit trace-id bytecode-ptr bytecode-len envs
                      parent-ip parent-exit-id linked-ip)
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
             (lp (cons (cons elt env) acc) (+ offset len) envs)))
          (()
           (reverse! acc))))))

  (define-syntax-rule (ip-ptr->source-line addr)
    (and=>
     (find-source-for-addr addr)
     (lambda (source)
       (format #f "~a:~d"
               (or (source-file source) "(unknown file)")
               (source-line-for-user source)))))

  (define-syntax-rule (show-one-line sline tlog code-size)
    (format #t ";;; trace ~a:~a ~a~a~a~%"
            trace-id sline
            code-size
            (if (< 0 parent-ip)
                (format #f " (~a:~a)"
                        (or (and tlog (tlog-id tlog))
                            (format #f "~x" parent-ip))
                        parent-exit-id)
                "")
            (if (or (= parent-ip 0))
                ""
                (format #f " -> ~a" (tlog-id (get-tlog linked-ip))))))

  (let* ((ip-x-ops (traced-ops bytecode-ptr bytecode-len envs))
         (entry-ip (cadr (car ip-x-ops)))
         (verbosity (lightning-verbosity))
         (tlog (get-tlog parent-ip))
         (sline (ip-ptr->source-line (cadr (car ip-x-ops)))))

    (when (and verbosity (<= 2 verbosity))
      (and tlog (dump-tlog tlog))
      (format #t
              ";;; tjit.scm: parent-ip=~x, linked-ip=~x, parent-exit-id=~a~%"
              parent-ip linked-ip parent-exit-id))

    (with-jit-state
     (jit-prolog)
     (let-values (((locals snapshots scm cps)
                   (trace->cps tlog parent-exit-id ip-x-ops)))
       (cond
        ((not cps)
         (debug 1 ";;; trace ~a:~a abort~%" trace-id sline)
         (debug 2 ";;; CPS conversion failed~%")
         (show-dump ip-x-ops scm locals cps #f #f)
         #f)
        (else
         (let-values
             (((exit-variables
                exit-codes
                trampoline
                loop-label
                loop-locals
                loop-vars
                fp-offset)
               (assemble-tjit cps entry-ip locals snapshots tlog
                              parent-exit-id linked-ip)))
           (let ((epilog-address (jit-label)))
             (jit-patch epilog-address)
             (jit-epilog)
             (jit-realize)
             (let* ((estimated-size (jit-code-size))
                    (code (make-bytevector estimated-size)))
               (jit-set-code (bytevector->pointer code) (imm estimated-size))
               (let* ((ptr (jit-emit))
                      (exit-counts (make-hash-table))
                      (loop-address (and loop-label (jit-address loop-label)))
                      (end-address (or (and tlog (tlog-end-address tlog))
                                       (jit-address epilog-address))))
                 (make-bytevector-executable! code)
                 (put-tlog! entry-ip (make-tlog trace-id
                                                code
                                                exit-counts
                                                entry-ip
                                                snapshots
                                                exit-variables
                                                exit-codes
                                                trampoline
                                                loop-address
                                                loop-locals
                                                loop-vars
                                                fp-offset
                                                end-address))

                 (when (and verbosity (<= 1 verbosity))
                   (let ((code-size (jit-code-size)))
                     (show-one-line sline tlog code-size)
                     (show-dump ip-x-ops scm locals cps code code-size)))

                 ;; When this trace is a side trace, replace the native code
                 ;; of trampoline in parent tlog.
                 (when tlog
                   (let ((trampoline (tlog-trampoline tlog))
                         (parent-exit-codes (tlog-exit-codes tlog)))
                     (trampoline-set! trampoline parent-exit-id ptr)
                     (hashq-set! parent-exit-codes parent-exit-id code)))

                 code))))))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  (initialize-tjit-primitives)
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")
