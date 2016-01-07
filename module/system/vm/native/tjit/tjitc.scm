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
  #:use-module (srfi srfi-1)
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
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit scan)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit state)
  #:use-module (system vm native tjit variables)
  #:export (tjitc init-vm-tjit)
  #:re-export (tjit-stats))


;;;
;;; Entry point
;;;

;; This procedure is called from C code in "libguile/vm-tjit.c".
(define (tjitc trace-id bytecode traces parent-ip parent-exit-id linked-ip
               loop? downrec? uprec?)
  (define-syntax-rule (show-sline sline fragment)
    (let ((exit-pair (if (< 0 parent-ip)
                         (format #f " (~a:~a)"
                                 (or (and fragment (fragment-id fragment))
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
  (define (traced-ops bytecode traces sp-offset fp-offset types)
    (define disassemble-one
      (@@ (system vm disassembler) disassemble-one))
    (define (go)
      (let lp ((acc '())
               (offset 0)
               (traces (reverse! traces))
               (ol (make-outline types sp-offset fp-offset))
               (so-far-so-good? #t)
               (prev-op #f))
        (match traces
          ((trace . traces)
           (let*-values (((len op) (disassemble-one bytecode offset))
                         ((implemented? prev-op)
                          (if so-far-so-good?
                              (let ((locals (cadddr trace))
                                    (dl (caddr trace)))
                                (scan-locals ol op prev-op dl locals))
                              (values #f (car op)))))
             (lp (cons (cons op trace) acc) (+ offset len) traces ol
                 (and so-far-so-good? implemented?) prev-op)))
          (()
           (values (reverse! acc) (arrange-outline ol) so-far-so-good?)))))
    (catch #t go
      (lambda (x y fmt args . z)
        (debug 1 "XXX: ~s~%" (apply format #f fmt args))
        (values '() #f #f))))
  (define (scan-backward traces ol parent-snapshot sp fp)
    (let ((sp-offsets (outline-sp-offsets ol))
          (fp-offsets (outline-fp-offsets ol))
          (linked (get-root-trace linked-ip)))
      (when linked
        (merge-outline-types! ol (fragment-loop-locals linked)))
      (let lp ((traces (reverse traces))
               (index (- (vector-length sp-offsets) 1)))
        (match traces
          (((op _ _ dl locals) . traces)
           (set-outline-sp-offset! ol (vector-ref sp-offsets index))
           (set-outline-fp-offset! ol (vector-ref fp-offsets index))
           (scan-locals ol op #f dl locals #t #t)
           (lp traces (- index 1)))
          (()
           (set-outline-sp-offset! ol sp)
           (set-outline-fp-offset! ol fp)
           ol)))))
  (define (get-initial-types snapshot)
    (if snapshot
        (let lp ((locals (snapshot-locals snapshot)) (acc '()))
          (match locals
            (((local . type) . locals)
             (let ((etype (type->stack-element-type type)))
               (lp locals (cons (cons local etype) acc))))
            (()
             acc)))
        '()))
  (define (merge-types dst src)
    (let lp ((src src) (dst dst))
      (match src
        (((n . t) . src)
         (lp src (assq-set! dst n (type->stack-element-type t))))
        (() dst))))

  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (make-tjit-time-log (get-internal-run-time) 0 0 0 0)))
      (put-tjit-time-log! trace-id log)))
  (let* ((parent-fragment (get-fragment parent-ip))
         (parent-snapshot (if parent-fragment
                              (hashq-ref (fragment-snapshots parent-fragment)
                                         parent-exit-id)
                              #f))
         (entry-ip (car (last traces)))
         (verbosity (lightning-verbosity))
         (dump-option (tjit-dump-option))
         (sline (addr->source-line entry-ip))
         (initial-sp-offset (get-initial-sp-offset parent-snapshot))
         (initial-fp-offset (get-initial-fp-offset parent-snapshot)))
    (define-syntax-rule (dump test data exp)
      (when (and (test dump-option)
                 (or data (tjit-dump-abort? dump-option)))
        exp))
    (define-syntax-rule (failure msg)
      (begin
        (debug 1 ";;; trace ~a: ~a~%" trace-id msg)
        (tjit-increment-compilation-failure! entry-ip)))
    (define (compile-traces traces outline)
      ;; Copy outline types before scanning backward. Then if this trace was
      ;; side trace, update the initial stack item types with locals from parent
      ;; snapshot.
      (let* ((initial-stack-item-types (copy-tree (outline-types outline)))
             (outline (scan-backward traces outline parent-snapshot
                                     initial-sp-offset
                                     initial-fp-offset))
             (initial-stack-item-types
              (if (or (not parent-fragment) loop?)
                  initial-stack-item-types
                  (merge-types (outline-types outline)
                               (snapshot-locals parent-snapshot))))
             (tj (make-tj trace-id entry-ip linked-ip parent-exit-id
                          parent-fragment parent-snapshot outline
                          loop? downrec? uprec? #f
                          initial-stack-item-types)))
        (let-values (((snapshots anf ops) (compile-ir tj traces)))
          (dump tjit-dump-anf? anf (dump-anf trace-id anf))
          (dump tjit-dump-ops? ops (dump-primops trace-id ops snapshots))
          (let-values (((code size adjust loop-address trampoline)
                        (compile-native tj ops snapshots sline)))
            (tjit-increment-id!)
            (when (tjit-dump-ncode? dump-option)
              (dump-ncode trace-id entry-ip code size adjust
                          loop-address snapshots trampoline)))
          (when (tjit-dump-time? dump-option)
            (let ((log (get-tjit-time-log trace-id))
                  (t (get-internal-run-time)))
              (set-tjit-time-log-end! log t))))))

    (with-tjitc-error-handler entry-ip
      (let-values (((traces outline implemented?)
                    (traced-ops bytecode traces
                                initial-sp-offset initial-fp-offset
                                (get-initial-types parent-snapshot))))
        (dump tjit-dump-jitc? implemented? (show-sline sline parent-fragment))
        (dump tjit-dump-bytecode? implemented? (dump-bytecode trace-id traces))
        (cond
         ((not outline)
          (failure "error during scan"))
         ((not implemented?)
          (failure "NYI found, aborted"))
         (uprec?
          (failure "NYI up recursion"))
         (else
          (with-nyi-handler entry-ip
            (compile-traces traces outline))))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")
