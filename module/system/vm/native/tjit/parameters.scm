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
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (system vm vm)
  #:use-module (srfi srfi-9)
  #:export (tjit-ip-counter
            tjit-fragment-table
            tjit-root-trace-table
            tjit-failed-ip-table

            tjit-hot-loop
            set-tjit-hot-loop!
            tjit-hot-exit
            set-tjit-hot-exit!
            tjit-max-retries
            set-tjit-max-retries!

            tjit-dump-abort?
            tjit-dump-bytecode?
            tjit-dump-disassemble?
            tjit-dump-ops?
            tjit-dump-locals?
            tjit-dump-jitc?
            tjit-dump-scm?
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
  (make-tjit-dump abort bytecode disassemble ops jitc locals scm time exit)
  tjit-dump?
  (abort tjit-dump-abort? set-tjit-dump-abort!)
  (bytecode tjit-dump-bytecode? set-tjit-dump-bytecode!)
  (disassemble tjit-dump-disassemble? set-tjit-dump-disassemble!)
  (ops tjit-dump-ops? set-tjit-dump-ops!)
  (jitc tjit-dump-jitc? set-tjit-dump-jitc!)
  (locals tjit-dump-locals? set-tjit-dump-locals!)
  (scm tjit-dump-scm? set-tjit-dump-scm!)
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

- 'd': Dump disassembled code.

- 'j': Dump brief info when starting JIT compilation.

- 'l': Dump locals, see 'x'.

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
                             (if (null? cs)
                                 o
                                 (let ((c (car cs))
                                       (cs (cdr cs)))
                                   (cond
                                    ((char=? c char)
                                     (setter o #t)
                                     (lp cs))
                                    ...
                                    (else
                                     (lp cs)))))))))
        (flags (#\a set-tjit-dump-abort!)
               (#\b set-tjit-dump-bytecode!)
               (#\d set-tjit-dump-disassemble!)
               (#\j set-tjit-dump-jitc!)
               (#\l set-tjit-dump-locals!)
               (#\o set-tjit-dump-ops!)
               (#\s set-tjit-dump-scm!)
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
         (hot-exit (tjit-hot-exit))
         (num-loops 0)
         (num-hot-loops 0)
         (dump-time (tjit-dump-time? (tjit-dump-option)))
         (total-time (if dump-time 0 #f))
         (init-time (if dump-time 0 #f))
         (scm-time (if dump-time 0 #f))
         (ops-time (if dump-time 0 #f))
         (asm-time (if dump-time 0 #f))
         (num-fragments (hash-count (const #t) (tjit-fragment-table))))
    (hash-fold (lambda (k v acc)
                 (set! num-loops (+ num-loops 1))
                 (when (< hot-loop v)
                   (set! num-hot-loops (+ num-hot-loops 1))))
               '()
               (tjit-ip-counter))
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
          `(hot-exit . ,hot-exit)
          `(num-loops . ,num-loops)
          `(num-hot-loops . ,num-hot-loops)
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
         (format #t "~14@a: ~a~%" (car kv) (cdr kv)))
       (tjit-stats))
      (display "not running with `vm-tjit' engine.\n")))


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
;; Value of the parameter is a procedure taking two arguments, offset address
;; and file path of compiled code. Default value is for x86-64 architecture,
;; assumes `objdump' executable already installed.
;;
(define tjit-disassembler
  (make-parameter
   (lambda (offset file)
     (format #f
             "objdump -D -b binary -mi386 -Mintel,x86-64 \\
--prefix-addresses --dwarf-start=1 --adjust-vma=~a ~a"
             offset file))))
