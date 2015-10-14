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

(define-module (system vm native tjit)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps types)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit compile-native)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
  #:export (tjitc
            init-vm-tjit)
  #:re-export (tjit-stats))


;;;
;;; Debug procedures
;;;

(define (dump-bytecode trace-id ip-x-ops)
  (define (lowest-level ip-x-ops)
    (let lp ((ip-x-ops ip-x-ops) (level 0) (lowest 0))
      (match ip-x-ops
        (((op . _) . ip-x-ops)
         (case (car op)
           ((call call-label)
            (lp ip-x-ops (+ level 1) lowest))
           ((return return-values subr-call foreign-call)
            (let ((level (- level 1)))
              (lp ip-x-ops level (min level lowest))))
           (else
            (lp ip-x-ops level lowest))))
        (() lowest))))
  (define (make-indent n)
    (let lp ((n n) (acc '()))
      (if (< 0 n)
          (lp (- n 1) (cons #\. (cons #\space acc)))
          (list->string acc))))
  (let ((lowest (lowest-level ip-x-ops)))
    (format #t ";;; trace ~a: bytecode ~a:~a~%"
            trace-id (length ip-x-ops) lowest)
    (let lp ((traces ip-x-ops) (level (- lowest)))
      (match traces
        (((op ip fp ra locals) . traces)
         (let ((op-val (format #f "~x  ~a~a" ip (make-indent level) op)))
           ;; (format #t "~a~%" op-val)
           (if (tjit-dump-locals? (tjit-dump-option))
               (format #t "~40a ; ~a~%" op-val locals)
               (format #t "~a~%" op-val))
           )
         (case (car op)
           ((call call-label)
            (lp traces (+ level 1)))
           ((return return-values subr-call foreign-call)
            (lp traces (- level 1)))
           (else
            (lp traces level))))
        (() (values))))))

(define (dump-scm trace-id scm)
  (match scm
    (`(letrec ((entry ,entry)
               (loop ,loop))
        entry)
     (format #t ";;; trace ~a: scm-entry~%~y" trace-id entry)
     (format #t ";;; trace ~a: scm-loop~%~y" trace-id loop))
    (`(letrec ((patch ,patch))
        patch)
     (format #t ";;; trace ~a: scm-patch~%~y" trace-id patch))
    (_
     (values))))

(define (dump-primlist trace-id plist snapshots)
  (define (mark-op op)
    (case (car op)
      ((%return
        %fref %fref/f
        %eq %ne %lt %ge
        %flt %fge)
       "  >")
      (else
       "   ")))
  (define (pretty-locals locals)
    (if (null? locals)
        "--"
        (map (match-lambda ((n . t)
                            (list n (pretty-type t))))
             locals)))
  (define (dump-snapshot snapshot)
    (match snapshot
      (($ $snapshot id offset nlocals locals)
       (format #t "----     [snap~3,,,' @a] ~a:~a ~a~%"
               id offset nlocals (pretty-locals locals)))
      (_
       (format #t "----     NOT-A-SNAPSHOT~%"))))
  (define (dump-one idx op)
    (match op
      (('%snap id . _)
       (dump-snapshot (hashq-ref snapshots id)))
      (_
       (let ((mark (mark-op op)))
         (format #t "~4,,,'0@a ~a (~7a ~{~a~^ ~})~%" idx mark
                 (car op) (cdr op))))))
  (define (dump-list idx ops)
    (let lp ((ops ops) (idx idx))
      (match ops
        ((op . ops)
         (dump-one idx op)
         (lp ops (+ idx 1)))
        (()
         idx))))
  (match plist
    (($ $primlist entry loop)
     (format #t ";;; trace ~a: primops:~%" trace-id)
     (let ((idx (dump-list 0 entry)))
       (when (not (null? loop))
         (format #t "==== loop:~%")
         (dump-list idx loop)
         (format #t "==== ->loop~%"))))
    (_
     (format #t ";;; primops: ~a~%" plist))))

(define (dump-native-code trace-id ip-x-ops code code-size)
  (jit-print)
  (let ((path (format #f "/tmp/trace-~a-~x.o"
                      trace-id (cadr (car ip-x-ops)))))
    (format #t ";;; Writing native code to ~a~%" path)
    (call-with-output-file path
      (lambda (port)
        (let ((code-copy (make-bytevector code-size)))
          (bytevector-copy! code 0 code-copy 0 code-size)
          (put-bytevector port code-copy))))))


;;;
;;; Entry point
;;;

;; This procedure is called from C code in "libguile/vm-tjit.c".
(define (tjitc trace-id bytecode-ptr bytecode-len envs
               parent-ip parent-exit-id linked-ip loop?)
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
  (define-syntax-rule (increment-compilation-failure ip)
    (let ((count (hashq-ref (tjit-failed-ip-table) ip 0)))
      (hashq-set! (tjit-failed-ip-table) ip (+ count 1))))
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
                                 (fragment-id (get-root-trace linked-ip))))))
      (format #t ";;; trace ~a: ~a~a~a~%" trace-id sline exit-pair linked-id)))

  (when (tjit-dump-time? (tjit-dump-option))
    (put-tjit-time-log! trace-id
                        (make-tjit-time-log (get-internal-run-time) 0 0 0 0)))

  (let* ((ip-x-ops (traced-ops bytecode-ptr bytecode-len envs))
         (entry-ip (cadr (car ip-x-ops)))
         (verbosity (lightning-verbosity))
         (fragment (get-fragment parent-ip))
         (sline (addr->source-line (cadr (car ip-x-ops))))
         (dump-option (tjit-dump-option)))
    (when (and verbosity (<= 3 verbosity))
      (format #t ":;; entry-ip:       ~x~%" entry-ip)
      (format #t ";;; parent-ip:      ~x~%" parent-ip)
      (format #t ";;; linked-ip:      ~x~%" linked-ip)
      (format #t ";;; parent-exit-id: ~a~%" parent-exit-id)
      (format #t ";;; loop?:          ~a~%" loop?)
      (and fragment (dump-fragment fragment)))
    (let-values (((locals snapshots lowest-offset scm ops)
                  (trace->primlist trace-id fragment parent-exit-id loop?
                                   ip-x-ops)))
      (when (and (tjit-dump-jitc? dump-option)
                 (or ops (tjit-dump-abort? dump-option)))
        (show-one-line sline fragment))
      (when (and (tjit-dump-bytecode? dump-option)
                 (or ops (tjit-dump-abort? dump-option)))
        (dump-bytecode trace-id ip-x-ops))
      (when (and (tjit-dump-scm? dump-option)
                 (or ops (tjit-dump-abort? dump-option)))
        (dump-scm trace-id scm))
      (cond
       ((not ops)
        (debug 1 ";;; trace ~a: aborted~%" trace-id)
        (increment-compilation-failure entry-ip))
       (else
        (when (tjit-dump-ops? dump-option)
          (dump-primlist trace-id ops snapshots))
        (with-jit-state
         (jit-prolog)
         (let-values
             (((trampoline loop-label loop-locals loop-vars fp-offset)
               (compile-mcode ops entry-ip locals snapshots fragment
                              parent-exit-id linked-ip lowest-offset
                              trace-id)))
           (let ((epilog-label (jit-label)))
             (jit-patch epilog-label)
             (jit-retr reg-retval)
             (jit-epilog)
             (jit-realize)
             (let* ((estimated-size (jit-code-size))
                    (code (make-bytevector estimated-size)))
               (jit-set-code (bytevector->pointer code) (imm estimated-size))
               (let* ((ptr (jit-emit))
                      (exit-counts (make-hash-table))
                      (loop-address (and loop-label (jit-address loop-label)))
                      (end-address (or (and fragment
                                            (fragment-end-address fragment))
                                       (jit-address epilog-label)))
                      (parent-id (or (and fragment (fragment-id fragment))
                                     0)))
                 (make-bytevector-executable! code)

                 ;; Same entry-ip could be used when side exit 0 was
                 ;; taken for multiple times. Using trace-id as hash
                 ;; table key.
                 (put-fragment! trace-id (make-fragment trace-id
                                                        code
                                                        exit-counts
                                                        entry-ip
                                                        parent-id
                                                        parent-exit-id
                                                        loop-address
                                                        loop-locals
                                                        loop-vars
                                                        snapshots
                                                        trampoline
                                                        fp-offset
                                                        end-address))
                 (when (and verbosity (<= 4 verbosity))
                   (dump-native-code trace-id ip-x-ops code (jit-code-size)))
                 ;; When this trace is a side trace, replace the native code
                 ;; of trampoline in parent fragment.
                 (when fragment
                   (let ((trampoline (fragment-trampoline fragment))
                         (snapshot (hashq-ref (fragment-snapshots fragment)
                                              parent-exit-id)))
                     (trampoline-set! trampoline parent-exit-id ptr)
                     (set-snapshot-code! snapshot code))))))))))
      (when (tjit-dump-time? dump-option)
        (let ((log (get-tjit-time-log trace-id)))
          (set-tjit-time-log-end! log (get-internal-run-time)))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")
