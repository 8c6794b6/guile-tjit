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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (tjit-ip-counter
            fragment-table
            root-trace-table
            failed-ip-table

            tjit-hot-loop
            set-tjit-hot-loop!
            tjit-hot-exit
            set-tjit-hot-exit!
            tjit-max-retries
            set-tjit-max-retries!

            tjit-dump-option
            tjit-dump-abort?
            tjit-dump-bytecode?
            tjit-dump-cps?
            tjit-dump-enter?
            tjit-dump-locals?
            tjit-dump-scm?
            tjit-dump-time?
            tjit-dump-exit?
            parse-tjit-dump-flags
            set-tjit-dump-option!

            tjit-max-spills

            make-tjit-time-log
            set-tjit-time-log-start!
            set-tjit-time-log-scm!
            set-tjit-time-log-cps!
            set-tjit-time-log-assemble!
            set-tjit-time-log-end!
            diff-tjit-time-log

            put-tjit-time-log!
            get-tjit-time-log
            fold-tjit-time-logs

            tjit-stats))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_tjit")

;; Record type for configuring dump options.
(define-record-type <tjit-dump>
  (make-tjit-dump abort bytecode cps enter locals scm time exit)
  tjit-dump?
  (abort tjit-dump-abort? set-tjit-dump-abort!)
  (bytecode tjit-dump-bytecode? set-tjit-dump-bytecode!)
  (cps tjit-dump-cps? set-tjit-dump-cps!)
  (enter tjit-dump-enter? set-tjit-dump-enter!)
  (locals tjit-dump-locals? set-tjit-dump-locals!)
  (scm tjit-dump-scm? set-tjit-dump-scm!)
  (time tjit-dump-time? set-tjit-dump-time!)
  (exit tjit-dump-exit? set-tjit-dump-exit!))

(define (make-empty-tjit-dump-option)
  "Makes tjit-dump data with all fields set to #f"
  (make-tjit-dump #f #f #f #f #f #f #f #f))

(define tjit-dump-option
  ;; Parameter to control dump setting during compilation of traces.
  (make-parameter (make-empty-tjit-dump-option)))

(define (parse-tjit-dump-flags str)
  "Parse dump flags in string STR and return <tjit-dump> data.

Flags are:

- 'a': Dump abort, without this flag true, aborted traces are not shown.

- 'b': Dump recorded bytecode.

- 'c': Dump CPS IR.

- 'e': Dump entry, show brief info when starting native code compilation.

- 'l': Dump locals, see 'x'.

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
               (#\l set-tjit-dump-locals!)
               (#\e set-tjit-dump-enter!)
               (#\x set-tjit-dump-exit!)
               (#\b set-tjit-dump-bytecode!)
               (#\s set-tjit-dump-scm!)
               (#\t set-tjit-dump-time!)
               (#\c set-tjit-dump-cps!))))))

(define (set-tjit-dump-option! str)
  "Set @code{tjit-dump-option} parameter with flags in STR."
  (tjit-dump-option (parse-tjit-dump-flags str)))

(define tjit-max-spills
  ;; Maximum number of spilled variables.
  (make-parameter 256))


;; Record type to hold internal run time at each stage of compilation.
(define-record-type <tjit-time-log>
  (make-tjit-time-log start scm cps assemble end)
  tjit-time-log?
  ;; Internal time at the time of tjitc entry.
  (start tjit-time-log-start set-tjit-time-log-start!)
  ;; Time at SCM compilation entry.
  (scm tjit-time-log-scm set-tjit-time-log-scm!)
  ;; Time at CPS compilation entry.
  (cps tjit-time-log-cps set-tjit-time-log-cps!)
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
        (cps (tjit-time-log-cps log))
        (assemble (tjit-time-log-assemble log))
        (end (tjit-time-log-end log)))
    (list (diff start end)
          (diff start scm)
          (diff scm cps)
          (diff cps assemble)
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
option is set to true."
  (let* ((hot-loop (tjit-hot-loop))
         (hot-exit (tjit-hot-exit))
         (num-loops 0)
         (num-hot-loops 0)
         (dump-time (tjit-dump-time? (tjit-dump-option)))
         (total-time (if dump-time 0 #f))
         (init-time (if dump-time 0 #f))
         (scm-time (if dump-time 0 #f))
         (cps-time (if dump-time 0 #f))
         (asm-time (if dump-time 0 #f))
         (num-fragments (hash-count (const #t) (fragment-table))))
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
            (set! cps-time (+ cps-time c))
            (set! asm-time (+ asm-time a)))))
       #f))
    (list `(hot-loop . ,hot-loop)
          `(hot-exit . ,hot-exit)
          `(num-loops . ,num-loops)
          `(num-hot-loops . ,num-hot-loops)
          `(num-fragments . ,num-fragments)
          `(total-time . ,total-time)
          `(init-time . ,init-time)
          `(scm-time . ,scm-time)
          `(cps-time . ,cps-time)
          `(asm-time . ,asm-time))))
