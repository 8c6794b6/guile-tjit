;;;; Trace to fragment compiler

;;;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.
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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; Module exporting `compile-fragment', the main entry point for trace language
;;; compiler.
;;;
;;; Code:

(define-module (language trace compile-fragment)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (last))
  #:use-module (srfi srfi-11)
  #:use-module (system vm native debug)
  #:use-module (language trace compile-ir)
  #:use-module (language trace compile-native)
  #:use-module (language trace env)
  #:use-module (language trace error)
  #:use-module (language trace fragment)
  #:use-module (language trace parameters)
  #:use-module (language trace parse)
  #:use-module (language trace snapshot)
  #:autoload (language trace dump) (dump-sline
                                    dump-bytecode
                                    dump-anf
                                    dump-primops
                                    dump-ncode)
  #:export (compile-fragment))


;;;; Fragment compiler

(define (compile-fragment bv env traces)
  (let ((result #f)
        (trace-id (env-id env))
        (entry-ip (env-entry-ip env))
        (parent-exit-id (env-parent-exit-id env))
        (parent-snapshot (env-parent-snapshot env))
        (parent-fragment (env-parent-fragment env))
        (linked-ip (env-linked-ip env))
        (loop? (env-loop? env))
        (downrec? (env-downrec? env))
        (uprec? (env-uprec? env))
        (parent-ip (and=> (env-parent-fragment env) fragment-entry-ip))
        (dump-option (tjit-dump-option)))
    (define-syntax dump
      (syntax-rules ()
        ((_ test data exp)
         (when (and (test dump-option) data)
           exp))))
    (define (dump-sline-and-bytecode test port sline)
      (dump tjit-dump-jitc? test
            (dump-sline port sline trace-id loop? downrec? uprec?
                        parent-ip parent-exit-id parent-snapshot
                        parent-fragment (env-linked-fragment env)))
      (dump tjit-dump-bytecode? test
            (dump-bytecode port trace-id traces)))
    (define (call-op? op)
      (or (eq? op 'call)
          (eq? op 'call-label)
          (eq? op 'tail-call)
          (eq? op 'tail-call-label)))
    (define (unsupported-downrec-prologue? op)
      (not (or (eq? op 'assert-nargs-ee/locals)
               (eq? op 'assert-nargs-ee))))

    (when (tjit-dump-time? dump-option)
      (let ((log (make-tjit-time-log (get-internal-run-time) 0 0 0 0 0)))
        (put-tjit-time-log! trace-id log)))
    (when (tjit-dump-prof? dump-option)
      (start-tjitc-profiler))
    (with-tjitc-error-handler env
      (lambda ()
        (let ((implemented? (parse-bytecode env bv traces))
              (last-op (and=> (vector-ref (last traces) 0) car))
              (first-op (car (vector-ref (car traces) 0)))
              (sline (addr->source-line entry-ip))
              (port (tjit-dump-log)))
          (when (tjit-dump-abort? dump-option)
            (dump-sline-and-bytecode #t port sline))
          (cond
           ((and (not parent-snapshot) (not loop?))
            (nyi "loop-less root trace"))
           ((and parent-snapshot loop?)
            (nyi "looping side trace"))
           ((not implemented?)
            (tjit-increment-compilation-failure! entry-ip 3))
           ((and downrec?
                 (not parent-snapshot)
                 (unsupported-downrec-prologue? first-op))
            (nyi "down recursion starting with ~a" first-op))
           (uprec?
            (nyi "up recursion"))
           ((and uprec? (<= (env-last-sp-offset env) 0))
            (nyi "up recursion with down SP shift"))
           ((and uprec? (not (eq? 'return-values last-op)))
            (nyi "unsupported up recursion epilogue ~a" last-op))
           ((and (not parent-snapshot)
                 (not (zero? (env-last-sp-offset env)))
                 (not (call-op? last-op))
                 (not uprec?))
            (nyi "root trace with SP shift, last op `~a'" last-op))
           ((and parent-snapshot (not (env-linked-fragment env)))
            (nyi "side trace with no linked fragment"))
           (else
            (unless (tjit-dump-abort? dump-option)
              (dump-sline-and-bytecode implemented? port sline))
            (let*-values (((snapshots anf ops) (compile-ir env traces))
                          ((fragment code size adjust loop-addr trampoline)
                           (compile-native env ops snapshots sline)))
              (dump tjit-dump-anf? anf (dump-anf port trace-id anf))
              (dump tjit-dump-ops? ops
                    (dump-primops port trace-id ops snapshots))
              (dump tjit-dump-ncode? code
                    (dump-ncode port trace-id entry-ip code size adjust
                                loop-addr snapshots trampoline
                                (not parent-snapshot)))
              (tjit-increment-id!)
              (and=> (and=> (env-parent-fragment env) get-origin-fragment)
                     increment-fragment-num-child!)
              (when (tjit-dump-time? dump-option)
                (let ((log (get-tjit-time-log trace-id))
                      (t (get-internal-run-time)))
                  (set-tjit-time-log-end! log t)))
              (set! result fragment)))))))
    (when (tjit-dump-prof? dump-option)
      (stop-tjitc-profiler))
    (values result #f #f)))
