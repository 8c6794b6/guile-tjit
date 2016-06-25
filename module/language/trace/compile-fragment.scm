;;; Trace to fragment compiler

;; Copyright (C) 2001, 2010, 2013 Free Software Foundation, Inc.

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
(define-module (language trace compile-fragment)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (last))
  #:use-module (srfi srfi-11)
  #:use-module (system vm native debug)
  #:use-module (language trace compile-ir)
  #:use-module (language trace compile-native)
  #:use-module (language trace dump)
  #:use-module (language trace env)
  #:use-module (language trace error)
  #:use-module (language trace fragment)
  #:use-module (language trace parameters)
  #:use-module (language trace parse)
  #:use-module (language trace snapshot)
  #:export (compile-fragment))

(define (compile-fragment bv env traces)
  (let ((trace-id (env-id env))
        (entry-ip (env-entry-ip env))
        (parent-exit-id (env-parent-exit-id env))
        (parent-snapshot (env-parent-snapshot env))
        (parent-fragment (env-parent-fragment env))
        (linked-ip (env-linked-ip env))
        (loop? (env-loop? env))
        (downrec? (env-downrec? env))
        (uprec? (env-uprec? env))
        (parent-ip (or (and=> (env-parent-fragment env) fragment-entry-ip)
                       0))
        (dump-option (tjit-dump-option)))
    (when (tjit-dump-time? dump-option)
      (let ((log (make-tjit-time-log (get-internal-run-time) 0 0 0 0 0)))
        (put-tjit-time-log! trace-id log)))
    (let ((sline (addr->source-line entry-ip)))
      (define (show-sline port)
        (let ((exit-pair (if (< 0 parent-ip)
                             (format #f " (~a:~a)"
                                     (or (and=> parent-fragment fragment-id)
                                         (format #f "~x" parent-ip))
                                     parent-exit-id)
                             ""))
              (linked-id (if (or parent-snapshot (not loop?))
                             (format #f " -> ~a"
                                     (and=> (env-linked-fragment env)
                                            fragment-id))
                             ""))
              (ttype (cond
                      ((not loop?) "")
                      (downrec? " - downrec")
                      (uprec? " - uprec")
                      (else ""))))
          (format port ";;; trace ~a: ~a:~a~a~a~a~%"
                  trace-id (car sline) (cdr sline) exit-pair linked-id ttype)))
      (define (increment-num-child! fragment)
        (let ((num-child (fragment-num-child fragment)))
          (set-fragment-num-child! fragment (+ num-child 1))))
      (define-syntax call-op?
        (syntax-rules ()
          ((_ op)
           (or (eq? op 'call)
               (eq? op 'call-label)
               (eq? op 'tail-call)
               (eq? op 'tail-call-label)))))
      (define-syntax unsupported-downrec-prologue?
        (syntax-rules ()
          ((_ op)
           (not (or (eq? op 'assert-nargs-ee/locals)
                    (eq? op 'assert-nargs-ee))))))
      (define-syntax dump
        (syntax-rules ()
          ((_ test data exp)
           (when (and (test dump-option) data)
             exp))))

      (with-tjitc-error-handler env
        (cond
         ((and (not parent-snapshot) (not loop?))
          (nyi "loop-less root trace"))
         ((and parent-snapshot loop?)
          (nyi "looping side trace"))
         (else
          (let ((implemented? (parse-bytecode env bv traces))
                (last-op (car (vector-ref (last traces) 0)))
                (first-op (car (vector-ref (car traces) 0)))
                (port (tjit-dump-log)))
            (define (dump-sline-and-bytecode test)
              (dump tjit-dump-jitc? test (show-sline port))
              (dump tjit-dump-bytecode? test
                    (dump-bytecode port trace-id traces)))
            (when (tjit-dump-abort? dump-option)
              (dump-sline-and-bytecode #t))
            (cond
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
                (dump-sline-and-bytecode implemented?))
              (let-values (((snapshots anf ops)
                            (compile-ir env traces)))
                (dump tjit-dump-anf? anf (dump-anf port trace-id anf))
                (dump tjit-dump-ops? ops
                      (dump-primops port trace-id ops snapshots))
                (let-values (((code size adjust loop-address trampoline)
                              (compile-native env ops snapshots sline)))
                  (tjit-increment-id!)
                  (and=> (and=> (env-parent-fragment env) get-origin-fragment)
                         increment-num-child!)
                  (dump tjit-dump-ncode? code
                        (dump-ncode port trace-id entry-ip code size adjust
                                    loop-address snapshots trampoline
                                    (not parent-snapshot))))
                (when (tjit-dump-time? dump-option)
                  (let ((log (get-tjit-time-log trace-id))
                        (t (get-internal-run-time)))
                    (set-tjit-time-log-end! log t))))))))))

      ;; Currently returned value from compilation is unused.
      (values #f #f #f))))
