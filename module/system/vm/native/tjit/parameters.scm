;;;; Parameters for vm-tjit engine

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
;;; Module containing procedure for parameters and statistics used for vm-tjit
;;; engine. This module contains @code{load-extension} with
;;; @code{scm_init_vm_tjit} and exports Scheme procedures to access C functions
;;; for vm-tjit engine.
;;;
;;; Code:

(define-module (system vm native tjit parameters)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system vm vm)
  #:use-module (srfi srfi-9)
  #:export (tjit-jump-counter
            tjit-call-counter
            tjit-return-counter
            tjit-fragment
            tjit-root-trace
            tjit-failed-ip
            tjit-increment-id!

            tjit-hot-loop
            set-tjit-hot-loop!
            tjit-hot-call
            set-tjit-hot-call!
            tjit-hot-exit
            set-tjit-hot-exit!
            tjit-max-retries
            set-tjit-max-retries!

            tjit-dump-abort?
            tjit-dump-bytecode?
            tjit-dump-ops?
            tjit-dump-locals?
            tjit-dump-ncode?
            tjit-dump-jitc?
            tjit-dump-anf?
            tjit-dump-time?
            tjit-dump-exit?
            parse-tjit-dump-flags
            set-tjit-dump-option!

            make-tjit-time-log
            set-tjit-time-log-start!
            set-tjit-time-log-scm!
            set-tjit-time-log-ops!
            set-tjit-time-log-assemble!
            set-tjit-time-log-end!
            diff-tjit-time-log

            put-tjit-time-log!
            get-tjit-time-log
            fold-tjit-time-logs

            tjit-stats
            dump-tjit-stats

            tjit-max-spills
            tjit-dump-option
            tjit-disassembler))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_tjit")

;;;
;;; Dump options
;;;

;; Record type for configuring dump options.
(define-record-type <tjit-dump>
  (make-tjit-dump abort bytecode ops jitc locals ncode scm time exit)
  tjit-dump?
  (abort tjit-dump-abort? set-tjit-dump-abort!)
  (bytecode tjit-dump-bytecode? set-tjit-dump-bytecode!)
  (ops tjit-dump-ops? set-tjit-dump-ops!)
  (jitc tjit-dump-jitc? set-tjit-dump-jitc!)
  (locals tjit-dump-locals? set-tjit-dump-locals!)
  (ncode tjit-dump-ncode? set-tjit-dump-ncode!)
  (scm tjit-dump-anf? set-tjit-dump-anf!)
  (time tjit-dump-time? set-tjit-dump-time!)
  (exit tjit-dump-exit? set-tjit-dump-exit!))

(define (make-empty-tjit-dump-option)
  "Makes tjit-dump data with all fields set to #f"
  (make-tjit-dump #f #f #f #f #f #f #f #f #f))

(define (parse-tjit-dump-flags str)
  "Parse dump flags in string STR and return <tjit-dump> data.

Flags are:

- 'a': Dump abort, without this flag true, aborted traces are not shown.

- 'b': Dump recorded bytecode.

- 'j': Dump brief info when starting JIT compilation.

- 'l': Dump locals, see 'x'.

- 'n': Dump native code.

- 'o': Dump list of primitive operations.

- 's': Dump Scheme IR.

- 't': Take elapsed time spent in native compilation.

- 'x': Dump exit. When 'l' option is set to true, also shows locals.

For instance, @code{(parse-tjit-dump-flags \"lexb\")} will return a <tjit-dump>
data with locals, entry, exit, and bytecodes field set to @code{#t} and other
fields to @code{#f}."
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
               (#\j set-tjit-dump-jitc!)
               (#\l set-tjit-dump-locals!)
               (#\n set-tjit-dump-ncode!)
               (#\o set-tjit-dump-ops!)
               (#\s set-tjit-dump-anf!)
               (#\t set-tjit-dump-time!)
               (#\x set-tjit-dump-exit!))))))

(define (set-tjit-dump-option! str)
  "Set @code{tjit-dump-option} parameter with flags in STR."
  (tjit-dump-option (parse-tjit-dump-flags str)))


;;;
;;; Time log
;;;

;; Record type to hold internal run time at each stage of compilation.
(define-record-type <tjit-time-log>
  (make-tjit-time-log start scm ops assemble end)
  tjit-time-log?
  ;; Internal time at the time of tjitc entry.
  (start tjit-time-log-start set-tjit-time-log-start!)
  ;; Time at SCM compilation entry.
  (scm tjit-time-log-scm set-tjit-time-log-scm!)
  ;; Time at primitive operation list compilation entry.
  (ops tjit-time-log-ops set-tjit-time-log-ops!)
  ;; At assemble entry.
  (assemble tjit-time-log-assemble set-tjit-time-log-assemble!)
  ;; At end.
  (end tjit-time-log-end set-tjit-time-log-end!))

(define (diff-tjit-time-log log)
  (define (diff a b)
    (if (and (< 0 a b))
        (exact->inexact (/ (- b a) internal-time-units-per-second))
        0.0))
  (let ((start (tjit-time-log-start log))
        (scm (tjit-time-log-scm log))
        (ops (tjit-time-log-ops log))
        (assemble (tjit-time-log-assemble log))
        (end (tjit-time-log-end log)))
    (list (diff start end)
          (diff start scm)
          (diff scm ops)
          (diff ops assemble)
          (diff assemble end))))

(define *tjit-time-logs*
  ;; Hash table containing log of compilation times.
  (make-hash-table))

(define (put-tjit-time-log! id log)
  (hashq-set! *tjit-time-logs* id log))

(define (get-tjit-time-log id)
  (hashq-ref *tjit-time-logs* id))

(define (fold-tjit-time-logs proc init)
  (hash-fold proc init *tjit-time-logs*))

(define (tjit-stats)
  "Returns statistics of vm-tjit engine.

Statistical times will be constantly @code{#f} unless @code{tjit-dump-time?}
option was set to true."
  (let* ((hot-loop (tjit-hot-loop))
         (hot-call (tjit-hot-call))
         (hot-exit (tjit-hot-exit))
         (num-loops 0)
         (num-calls 0)
         (num-returns 0)
         (num-hot-loops 0)
         (num-hot-calls 0)
         (num-hot-returns 0)
         (dump-time (tjit-dump-time? (tjit-dump-option)))
         (total-time (if dump-time 0 #f))
         (init-time (if dump-time 0 #f))
         (scm-time (if dump-time 0 #f))
         (ops-time (if dump-time 0 #f))
         (asm-time (if dump-time 0 #f))
         (num-fragments (hash-count (const #t) (tjit-fragment))))
    (hash-fold (lambda (k v acc)
                 (set! num-loops (+ num-loops 1))
                 (when (< hot-loop v)
                   (set! num-hot-loops (+ num-hot-loops 1))))
               '()
               (tjit-jump-counter))
    (hash-fold (lambda (k v acc)
                 (set! num-calls (+ num-calls 1))
                 (when (< hot-call v)
                   (set! num-hot-calls (+ num-hot-calls 1))))
               '()
               (tjit-call-counter))
    (hash-fold (lambda (k v acc)
                 (set! num-returns (+ num-returns 1))
                 (when (< hot-call v)
                   (set! num-hot-returns (+ num-hot-returns 1))))
               '()
               (tjit-return-counter))
    (when dump-time
      (fold-tjit-time-logs
       (lambda (k v acc)
         (match (diff-tjit-time-log v)
           ((t i s c a)
            (set! total-time (+ total-time t))
            (set! init-time (+ init-time i))
            (set! scm-time (+ scm-time s))
            (set! ops-time (+ ops-time c))
            (set! asm-time (+ asm-time a)))))
       #f))
    (list `(hot-loop . ,hot-loop)
          `(hot-call . ,hot-call)
          `(hot-exit . ,hot-exit)
          `(num-loops . ,num-loops)
          `(num-calls . ,num-calls)
          `(num-returns . ,num-returns)
          `(num-hot-loops . ,num-hot-loops)
          `(num-hot-calls . ,num-hot-calls)
          `(num-hot-returns . ,num-hot-returns)
          `(num-fragments . ,num-fragments)
          `(init-time . ,init-time)
          `(scm-time . ,scm-time)
          `(ops-time . ,ops-time)
          `(asm-time . ,asm-time)
          `(total-time . ,total-time))))

(define (dump-tjit-stats)
  (if (eq? 'tjit (vm-engine))
      (for-each
       (lambda (kv)
         (format #t "~16@a: ~a~%" (car kv) (cdr kv)))
       (tjit-stats))
      (display "not running with `vm-tjit' engine.\n")))

(define (default-disassembler trace-id entry-ip code code-size adjust
          loop-address snapshots trampoline)
  "Disassemble CODE with size CODE-SIZE, using ADDR as offset address.

TRACE-ID is the trace-id of given code, ENTRY-IP is the starting IP of the trace
which the code was compiled from. Default value is for x86-64 architecture,
assumes `objdump' executable already installed."
  (define trampoline-address-rxs
    (let ((nexit (hash-count (const #t) snapshots)))
      (let lp ((n 0) (acc '()))
        (if (< n nexit)
            (let* ((addr (pointer-address
                          ((@ (system vm native tjit fragment) trampoline-ref)
                           trampoline n)))
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
  (let* ((path (format #f "/tmp/trace-~a-~x" trace-id entry-ip))
         (loop-address/i (if (pointer? loop-address)
                             (pointer-address loop-address)
                             0))
         (loop-start/rx
          (make-regexp (format #f "^0x0*~x" loop-address/i)))
         (loop-jump/rx
          (make-regexp (format #f " j[a-z]+ +0x0*~x" loop-address/i))))
    (call-with-output-file path
      (lambda (port)
        (let ((code-copy (make-bytevector code-size)))
          (bytevector-copy! code 0 code-copy 0 code-size)
          (put-bytevector port code-copy))))
    (let* ((fmt "objdump -D -b binary -mi386 -Mintel,x86-64 \\
--prefix-address --dwarf-start=1 --adjust-vma=~a ~a")
           (objdump (format #f fmt adjust path))
           (pipe (open-input-pipe objdump)))
      (let lp ((line (read-line pipe)) (n 0))
        (when (not (eof-object? line))
          (when (<= 2 n)
            (when (and (pointer? loop-address)
                       (regexp-exec loop-start/rx line))
              (display "loop:\n"))
            (display line)
            (when (and (pointer? loop-address)
                       (regexp-exec loop-jump/rx line))
              (display "    ->loop"))
            (cond
             ((side-exit-jump? line)
              => (lambda (id)
                   (display "    ->")
                   (display id)))
             (else
              (values)))
            (newline))
          (lp (read-line pipe) (+ n 1))))
      (close-pipe pipe)
      (delete-file path))))


;;;
;;; Scheme Parameters
;;;

;; Parameter to control dump setting during compilation of traces.
(define tjit-dump-option
  (make-parameter (make-empty-tjit-dump-option)))

;; Maximum number of spilled variables.
(define tjit-max-spills
  (make-parameter 256))

;; Paramter for disassembling compiled native code.
;;
;; See `default-disassembler' for the use of the arguments passed to procedure
;; in the parameter.
;;
(define tjit-disassembler
  (make-parameter default-disassembler))
