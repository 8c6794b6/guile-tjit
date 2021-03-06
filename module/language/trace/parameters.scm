;;;; Parameters for vm-tjit engine

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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA
;;;;

;;; Commentary:
;;;
;;; Module containing procedure for parameters and statistics used for vm-tjit
;;; engine. This module contains @code{load-extension} with
;;; @code{scm_init_vm_tjit} and exports Scheme procedures to access C functions
;;; for vm-tjit engine.
;;;
;;; Code:

(define-module (language trace parameters)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (statprof)
  #:use-module (system foreign)
  #:use-module (language trace trampoline)
  #:export (tjit-ip-counter
            tjit-fragment
            tjit-root-trace
            tjit-failed-ip
            tjit-increment-id!

            tjit-hot-loop
            set-tjit-hot-loop!
            tjit-hot-exit
            set-tjit-hot-exit!
            tjit-max-retries
            set-tjit-max-retries!
            tjit-try-sides
            set-tjit-try-sides!
            tjit-num-unrolls
            set-tjit-num-unrolls!
            tjit-scheme-engine
            set-tjit-scheme-engine!
            tjit-max-record
            set-tjit-max-record!
            tjit-max-sides
            set-tjit-max-sides!
            tjit-increment-compilation-failure!
            tjit-add-root-ip!
            tjit-remove-root-ip!
            tjit-register-gdb-jit-entry!
            make-negative-pointer
            continuation-next-ip
            scm-do-i-string-ref

            %cadd %csub %cmul %cdiv
            %cquo %crem %cmod
            %ceq %clt %cle %cgt %cge

            tjit-dump-abort?
            tjit-dump-bytecode?
            tjit-dump-dwarf?
            tjit-dump-ops?
            tjit-dump-ncode?
            tjit-dump-jitc?
            tjit-dump-prof?
            tjit-dump-anf?
            tjit-dump-snapshot?
            tjit-dump-time?
            tjit-dump-verbose?
            tjit-dump-exit?
            tjit-dump-log
            parse-tjit-dump-flags
            set-tjit-dump-option!

            make-tjit-time-log
            set-tjit-time-log-start!
            set-tjit-time-log-anf!
            set-tjit-time-log-ops!
            set-tjit-time-log-assemble!
            set-tjit-time-log-bailout!
            set-tjit-time-log-end!
            diff-tjit-time-log

            put-tjit-time-log!
            get-tjit-time-log
            fold-tjit-time-logs

            start-tjitc-profiler
            stop-tjitc-profiler
            display-tjitc-profile

            tjit-max-spills
            tjit-max-inline-depth
            tjit-max-locals
            tjit-dump-option
            tjit-disassembler))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_tjit")


;;;; Dump options

;; Record type for configuring dump options.
(define-record-type <tjit-dump>
  (make-tjit-dump abort bytecode dwarf ops jitc locals ncode prof anf
                  snapshot time exit)
  tjit-dump?
  (abort tjit-dump-abort? set-tjit-dump-abort!)
  (bytecode tjit-dump-bytecode? set-tjit-dump-bytecode!)
  (dwarf tjit-dump-dwarf? set-tjit-dump-dwarf!)
  (ops tjit-dump-ops? set-tjit-dump-ops!)
  (jitc tjit-dump-jitc? set-tjit-dump-jitc!)
  (locals tjit-dump-verbose? set-tjit-dump-verbose!)
  (ncode tjit-dump-ncode? set-tjit-dump-ncode!)
  (prof tjit-dump-prof? set-tjit-dump-prof!)
  (anf tjit-dump-anf? set-tjit-dump-anf!)
  (snapshot tjit-dump-snapshot? set-tjit-dump-snapshot!)
  (time tjit-dump-time? set-tjit-dump-time!)
  (exit tjit-dump-exit? set-tjit-dump-exit!))

(define (make-empty-tjit-dump-option)
  "Makes tjit-dump data with all fields set to #f"
  (make-tjit-dump #f #f #f #f #f #f #f #f #f #f #f #f))

(define (parse-tjit-dump-flags str)
  "Parse dump flags in string STR and return <tjit-dump> data.

Flags are:

- 'a': Dump abort, aborted traces are not shown without this flag.

- 'b': Dump recorded bytecode.

- 'd': Dump DWARF data for GDB.

- 'j': Dump brief info when starting JIT compilation.

- 'n': Dump native code.

- 'o': Dump primitive operations.

- 'p': Take profiling information of native compilation.

- 'i': Dump ANF IR.

- 's': Dump snapshot when dumping primitive operations.

- 't': Take elapsed time spent in native compilation.

- 'v': Dump verbosely, affects 'b', 'o', and 'x'.

- 'x': Dump exit.

For instance, @code{(parse-tjit-dump-flags \"jbx\")} will return a <tjit-dump>
data with jitx, bytecode, and exit fields set to @code{#t} and other fields to
@code{#f}."
  (let ((o (make-empty-tjit-dump-option)))
    (let lp ((cs (string->list str)))
      (let-syntax ((flags (syntax-rules ()
                            ((_ (char setter) ...)
                             (cond
                              ((null? cs)
                               o)
                              ((char=? (car cs) char)
                               (setter o #t)
                               (lp (cdr cs)))
                              ...
                              (else
                               (lp (cdr cs))))))))
        (flags (#\a set-tjit-dump-abort!)
               (#\b set-tjit-dump-bytecode!)
               (#\d set-tjit-dump-dwarf!)
               (#\j set-tjit-dump-jitc!)
               (#\n set-tjit-dump-ncode!)
               (#\o set-tjit-dump-ops!)
               (#\p set-tjit-dump-prof!)
               (#\i set-tjit-dump-anf!)
               (#\s set-tjit-dump-snapshot!)
               (#\t set-tjit-dump-time!)
               (#\v set-tjit-dump-verbose!)
               (#\x set-tjit-dump-exit!))))))

(define (set-tjit-dump-option! str)
  "Set @code{tjit-dump-option} parameter with flags in STR."
  (tjit-dump-option (parse-tjit-dump-flags str)))

;;; Parameter to hold log output for dump.
(define tjit-dump-log
  (make-parameter (current-output-port)))


;;;; Time log

;; Record type to hold internal run time at each stage of compilation.
(define-record-type <tjit-time-log>
  (make-tjit-time-log start anf ops assemble bailout end)
  tjit-time-log?

  ;; Internal time at the time of tjitc entry.
  (start tjit-time-log-start set-tjit-time-log-start!)

  ;; At ANF compilation entry.
  (anf tjit-time-log-anf set-tjit-time-log-anf!)

  ;; At primitive operation list compilation entry.
  (ops tjit-time-log-ops set-tjit-time-log-ops!)

  ;; At assemble entry.
  (assemble tjit-time-log-assemble set-tjit-time-log-assemble!)

  ;; At bailout compilation entry.
  (bailout tjit-time-log-bailout set-tjit-time-log-bailout!)

  ;; At end.
  (end tjit-time-log-end set-tjit-time-log-end!))

(define (diff-tjit-time-log log)
  (define (diff a b)
    (if (and (< 0 a b))
        (exact->inexact (/ (- b a) internal-time-units-per-second))
        0.0))
  (let ((start (tjit-time-log-start log))
        (anf (tjit-time-log-anf log))
        (ops (tjit-time-log-ops log))
        (assemble (tjit-time-log-assemble log))
        (bailout (tjit-time-log-bailout log))
        (end (tjit-time-log-end log)))
    (list (diff start end)
          (diff start anf)
          (diff anf ops)
          (diff ops assemble)
          (diff assemble bailout)
          (diff bailout end))))

(define *tjit-time-logs*
  ;; Hash table containing log of compilation times.
  (make-hash-table))

(define (put-tjit-time-log! id log)
  (hashq-set! *tjit-time-logs* id log))

(define (get-tjit-time-log id)
  (hashq-ref *tjit-time-logs* id))

(define (fold-tjit-time-logs proc init)
  (hash-fold proc init *tjit-time-logs*))


;;;; JIT compilation profiler

(define tjitc-profiler-state
  (let ((state ((@@ (statprof) fresh-profiler-state))))
    state))

(define-syntax-rule (with-tjitc-statprof-state . body)
  (parameterize (((@@ (statprof) profiler-state) tjitc-profiler-state))
    . body))

(define (start-tjitc-profiler)
  (with-tjitc-statprof-state (statprof-start)))

(define (stop-tjitc-profiler)
  (with-tjitc-statprof-state (statprof-stop)))

(define* (display-tjitc-profile #:optional (port (current-output-port))
                                #:key (style 'flat))
  (with-tjitc-statprof-state
   (statprof-display port tjitc-profiler-state #:style style)))


;;;; Scheme Parameters

(define (default-disassembler port trace-id entry-ip code code-size adjust
          loop-address snapshots trampoline root?)
  "Disassemble CODE with size CODE-SIZE, using ADDR as offset address.

TRACE-ID is the trace-id of given code, ENTRY-IP is the starting IP of the trace
which the code was compiled from. Default value is for x86-64 architecture,
assumes `objdump' executable already installed."
  (define trampoline-address-rxs
    (let ((nexit (hash-count (const #t) snapshots)))
      (let lp ((n 0) (acc '()))
        (if (< n nexit)
            (let* ((addr (trampoline-ref trampoline n))
                   (pat (format #f "j[a-z]+ +0x0*~x" addr)))
              (lp (+ n 1) (cons (cons n (make-regexp pat)) acc)))
            acc))))
  (define (side-exit-jump? line)
    (let lp ((rxs trampoline-address-rxs))
      (match rxs
        (((id . rx) . rxs)
         (if (regexp-exec rx line)
             id
             (lp rxs)))
        (()
         #f))))
  (define (done-line? line)
    (let ((rx (make-regexp "mov +rsp,rbp")))
      (regexp-exec rx line)))
  (let* ((path (format #f "/tmp/trace-~a-~x" trace-id entry-ip))
         (loop-address/i (or loop-address #f))
         (loop-start/rx
          (and loop-address/i
               (make-regexp (format #f "^0x0*~x" loop-address/i))))
         (loop-jump/rx
          (and loop-address/i
               (make-regexp (format #f " j[a-z]+ +0x0*~x" loop-address/i)))))
    (call-with-output-file path
      (lambda (port)
        (let ((code-copy (make-bytevector code-size)))
          (bytevector-copy! code 0 code-copy 0 code-size)
          (put-bytevector port code-copy))))
    (let* ((fmt "objdump -D -b binary -mi386 -Mintel,x86-64 \\
--prefix-address --dwarf-start=1 --adjust-vma=~a ~a")
           (objdump (format #f fmt adjust path))
           (pipe (open-input-pipe objdump)))
      ;; Skip first two lines.
      (read-line pipe)
      (read-line pipe)
      (let lp ((line (read-line pipe)) (done? #f))
        (when (not done?)
          (when (and loop-address/i (regexp-exec loop-start/rx line))
            (display "loop:\n" port))
          (display line port)
          (when (and loop-address/i (regexp-exec loop-jump/rx line))
            (display "    ->loop" port))
          (and=> (side-exit-jump? line)
                 (lambda (id)
                   (display "    ->" port)
                   (display id port)))
          (newline port)
          (let ((line (read-line pipe)))
            (lp line (eof-object? line)))))
      (close-pipe pipe)
      (delete-file path))))

;; Parameter to control dump setting during compilation of traces.
(define tjit-dump-option
  (make-parameter (make-empty-tjit-dump-option)))

;; Maximum number of spilled variables.
(define tjit-max-spills
  (make-parameter 400))

;; Maximum depth for inlined procedures.
(define tjit-max-inline-depth
  (make-parameter 25))

;; Maximum number of local variables in single trace.
(define tjit-max-locals
  (make-parameter 400))

;; Paramter for disassembling compiled native code.
;;
;; See `default-disassembler' for the use of the arguments passed to procedure
;; in the parameter.
;;
(define tjit-disassembler
  (make-parameter default-disassembler))
