;;;; Entry point for compiler used in vm-tjit engine

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
;;;
;;; Module exporting @code{tjitc}, entry point of just-in-time compiler
;;; for `vm-tjit' engine.
;;;
;;; Code:

(define-module (system vm native tjit tjitc)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (language cps)
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (last))
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit compile-ir)
  #:use-module (system vm native tjit compile-native)
  #:use-module (system vm native tjit dump)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables)
  #:export (tjitc init-vm-tjit)
  #:re-export (tjit-stats))


;;;
;;; Entry point
;;;

;; This procedure is called from C code in "libguile/vm-tjit.c".
(define (tjitc trace-id bytecode traces parent-ip parent-exit-id linked-ip
               loop? downrec? uprec?)
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (make-tjit-time-log (get-internal-run-time) 0 0 0 0 0)))
      (put-tjit-time-log! trace-id log)))
  (let* ((parent-fragment (get-fragment parent-ip))
         (parent-snapshot (if parent-fragment
                              (hashq-ref (fragment-snapshots parent-fragment)
                                         parent-exit-id)
                              #f))
         (entry-ip (car (last traces)))
         (dump-option (tjit-dump-option))
         (sline (addr->source-line entry-ip))
         (linking-roots?
          ;; Detecting root trace linkage by chasing parent id until it reaches
          ;; to root trace and compare it with linked trace. This loop could be
          ;; avoided by saving the origin trace id in fragment record type.
          (let ((origin-id
                 (let lp ((fragment parent-fragment))
                   (if (not fragment)
                       #f
                       (let ((parent-id (fragment-parent-id fragment)))
                         (if (zero? parent-id)
                             (fragment-id fragment)
                             (lp (get-fragment parent-id)))))))
                (linked-id
                 (and linked-ip
                      (and=> (get-root-trace linked-ip) fragment-id))))
            (and origin-id linked-id
                 (not (eq? origin-id linked-id)))))
         (env
          (call-with-values
              (lambda ()
                (match parent-snapshot
                  (($ $snapshot id sp fp nlocals locals variables code ip lives
                      depth)
                   (let ((inferred (if loop? '() locals)))
                     (values sp fp (map car locals) lives inferred depth)))
                  (_
                   (values 0 0 '() '() '() 0))))
            (lambda args
              (apply make-env trace-id entry-ip linked-ip
                     parent-exit-id parent-fragment parent-snapshot
                     loop? downrec? uprec? linking-roots?
                     args))))
         (initial-sp-offset (env-sp-offset env))
         (initial-fp-offset (env-fp-offset env)))
    (define (show-sline sline)
      (let ((exit-pair (if (< 0 parent-ip)
                           (format #f " (~a:~a)"
                                   (or (and=> parent-fragment fragment-id)
                                       (format #f "~x" parent-ip))
                                   parent-exit-id)
                           ""))
            (linked-id (if loop?
                           ""
                           (format #f " -> ~a"
                                   (fragment-id (get-root-trace linked-ip)))))
            (ttype (cond
                    (downrec? " - downrec")
                    (uprec? " - uprec")
                    (else ""))))
        (format #t ";;; trace ~a: ~a:~a~a~a~a~%"
                trace-id (car sline) (cdr sline) exit-pair linked-id ttype)))
    (define (parse-bytecode traces)
      (define disassemble-one
        (@@ (system vm disassembler) disassemble-one))
      (define (go)
        (let lp ((acc '()) (offset 0) (traces (reverse! traces))
                 (so-far-so-good? #t))
          (match traces
            ((trace . traces)
             (match trace
               ((ip ra dl locals)
                (let*-values
                    (((len op) (disassemble-one bytecode offset))
                     ((implemented?)
                      (if so-far-so-good?
                          (let* ((ret (scan-trace env op ip dl locals))
                                 (_ (infer-type env op ip dl locals))
                                 (ws (map car (env-inferred-types env)))
                                 (buf (env-write-buf env))
                                 (buf (cons (sort ws <) buf)))
                            (increment-env-call-return-num! env op)
                            (set-env-write-buf! env buf)
                            ret)
                          #f)))
                  (lp (cons (cons op trace) acc) (+ offset len) traces
                      implemented?)))
               (_ (error "malformed trace" trace))))
            (()
             (arrange-env! env)
             (values (reverse! acc) so-far-so-good?)))))
      (catch #t go
        (lambda (x y fmt args . z)
          (debug 2 "XXX: ~a~%" (apply format #f fmt args))
          (values '() #f))))
    (define-syntax dump
      (syntax-rules ()
        ((_ test data exp)
         (when (and (test dump-option)
                    (or data (tjit-dump-abort? dump-option)))
           exp))))
    (define (failure msg)
      (debug 2 ";;; trace ~a: ~a~%" trace-id msg)
      (tjit-increment-compilation-failure! entry-ip))
    (define (compile-traces traces)
      (set-env-sp-offset! env initial-sp-offset)
      (set-env-fp-offset! env initial-fp-offset)
      (let-values (((snapshots anf ops) (compile-ir env traces)))
        (dump tjit-dump-anf? anf (dump-anf trace-id anf))
        (dump tjit-dump-ops? ops (dump-primops trace-id ops snapshots))
        (let-values (((code size adjust loop-address trampoline)
                      (compile-native env ops snapshots sline)))
          (tjit-increment-id!)
          (when (tjit-dump-ncode? dump-option)
            (dump-ncode trace-id entry-ip code size adjust loop-address
                        snapshots trampoline (not parent-snapshot))))
        (when (tjit-dump-time? dump-option)
          (let ((log (get-tjit-time-log trace-id))
                (t (get-internal-run-time)))
            (set-tjit-time-log-end! log t)))))

    (with-tjitc-error-handler entry-ip
      (let-values (((traces implemented?) (parse-bytecode traces)))
        (dump tjit-dump-jitc? implemented? (show-sline sline))
        (dump tjit-dump-bytecode? implemented? (dump-bytecode trace-id traces))
        (cond
         ((not env) (failure "error during scan"))
         ((not implemented?) (failure "NYI found, aborted"))
         (uprec? (failure "NYI - up recursion"))
         (downrec? (failure "NYI - down recursion"))
         ((and (not parent-snapshot) loop?
               (not (zero? (env-sp-offset env))))
          (failure "NYI - looping root trace with SP shift"))
         ((and (not parent-snapshot) (not loop?))
          (failure "NYI - loop-less root trace"))
         (else
          (with-nyi-handler entry-ip (compile-traces traces))))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  #t)

;; Call `load-extension' from top-level after defining `tjitc',
;; "scm_bootstrap_vm_tjit" will lookup `tjitc' variable and assign to C
;; variable.
(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")
