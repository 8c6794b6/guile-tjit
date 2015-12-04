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
  #:use-module (ice-9 control)
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
;;; Auxiliary
;;;

(define (traced-ops bytecode traces sp-offset fp-offset types)
  (define disassemble-one
    (@@ (system vm disassembler) disassemble-one))
  (let lp ((acc '())
           (offset 0)
           (traces (reverse! traces))
           (pf (make-past-frame types sp-offset fp-offset))
           (so-far-so-good? #t))
    (match traces
      ((trace . traces)
       (let*-values (((len op) (disassemble-one bytecode offset))
                     ((implemented?) (scan-locals pf op (caddr trace))))
         (lp (cons (cons op trace) acc) (+ offset len) traces
             pf (and so-far-so-good? implemented?))))
      (()
       (values (reverse! acc) (arrange-past-frame pf) so-far-so-good?)))))


(define (scan-again traces pf parent-snapshot sp fp)
  ;; Scan traces again to resolve stack element types which were unresolved in
  ;; the first scan.
  ;;
  ;; XXX: Inefficient to scan again, consider resolving stack element types by
  ;; travercing the traces from last to first.
  (when parent-snapshot
    (merge-past-frame-types! pf (snapshot-locals parent-snapshot)))
  (set-past-frame-sp-offset! pf sp)
  (set-past-frame-fp-offset! pf fp)
  (let lp ((traces traces))
    (match traces
      (((op _ _ locals) . traces)
       (scan-locals pf op locals #t)
       (lp traces))
      (()
       (set-past-frame-sp-offset! pf sp)
       (set-past-frame-fp-offset! pf fp)
       pf))))


;;;
;;; Entry point
;;;

;; This procedure is called from C code in "libguile/vm-tjit.c".
(define (tjitc trace-id bytecode traces parent-ip parent-exit-id linked-ip
               loop? downrec? uprec?)
  (define-syntax-rule (increment-compilation-failure ip)
    (let ((current (hashq-ref (tjit-failed-ip-table) ip 0)))
      (hashq-set! (tjit-failed-ip-table) ip (+ current 1))))
  (define-syntax-rule (show-one-line sline fragment)
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
          (trace-type (cond
                       (downrec? " - downrec")
                       (uprec? " - uprec")
                       (else ""))))
      (format #t ";;; trace ~a: ~a~a~a~a~%"
              trace-id sline exit-pair linked-id trace-type)))
  (define (get-initial-types snapshot)
    (if snapshot
        (let lp ((locals (snapshot-locals snapshot)) (acc '()))
          (match locals
            (((local . type) . locals)
             (let ((etype (type->stack-element-type type)))
               (lp locals (cons (cons local etype) acc))))
            (()
             (debug 1 ";;; g-i-t: ~s~%" (sort acc (lambda (a b)
                                                    (< (car a) (car b)))))
             acc)))
        '()))

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
    (define-syntax dump
      (syntax-rules ()
        ((_ test data exp)
         (when (and (test dump-option)
                    (or data (tjit-dump-abort? dump-option)))
           exp))))
    (let-values (((traces past-frame implemented?)
                  (catch #t
                    (lambda ()
                      (traced-ops bytecode
                                  traces
                                  initial-sp-offset
                                  initial-fp-offset
                                  (get-initial-types parent-snapshot)))
                    (lambda (x y fmt args . z)
                      (debug 1 "XXX: ~s~%" (apply format #f fmt args))
                      (values '() #f #f)))))
      (dump tjit-dump-jitc? implemented? (show-one-line sline parent-fragment))
      (dump tjit-dump-bytecode? implemented? (dump-bytecode trace-id traces))
      (cond
       ((not past-frame)
        (debug 1 ";;; trace ~a: error in scan~%" trace-id)
        (increment-compilation-failure entry-ip))
       ((not implemented?)
        (debug 1 ";;; trace ~a: NYI found, aborted~%" trace-id)
        (increment-compilation-failure entry-ip))
       (else
        (let* ((past-frame (scan-again traces past-frame parent-snapshot
                                       initial-sp-offset initial-fp-offset))
               (tj (make-tj trace-id
                            entry-ip
                            linked-ip
                            parent-exit-id
                            parent-fragment
                            parent-snapshot
                            past-frame
                            loop?
                            downrec?
                            uprec?)))

          (let-values (((snapshots anf ops) (compile-primops tj traces)))
            (dump tjit-dump-anf? anf (dump-anf trace-id anf))
            (dump tjit-dump-ops? ops (dump-primops trace-id ops snapshots))
            (cond
             ((not ops)
              (debug 1 ";;; trace ~a: aborted~%" trace-id)
              (increment-compilation-failure entry-ip))
             (uprec?
              (debug 1 ";;; trace ~a: NYI up recursion~%" trace-id)
              (increment-compilation-failure entry-ip))
             (else
              (let-values (((code size adjust loop-address trampoline)
                            (compile-native tj ops snapshots)))
                (tjit-increment-id!)
                (when (tjit-dump-ncode? dump-option)
                  (dump-ncode trace-id
                              entry-ip
                              code
                              size
                              adjust
                              loop-address
                              snapshots
                              trampoline)))))
            (when (tjit-dump-time? dump-option)
              (let ((log (get-tjit-time-log trace-id))
                    (t (get-internal-run-time)))
                (set-tjit-time-log-end! log t))))))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")
