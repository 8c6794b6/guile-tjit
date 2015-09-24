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
;;; @code{scm_init_vm_tjit}.
;;;
;;; Code:

(define-module (system vm native tjit parameters)
  #:use-module (srfi srfi-9)
  #:export (tjit-ip-counter
            fragment-table
            failed-ip-table

            tjit-hot-loop
            set-tjit-hot-loop!
            tjit-hot-exit
            set-tjit-hot-exit!
            tjit-max-retries
            set-tjit-max-retries!

            tjit-dump-option
            tjit-dump-locals?
            tjit-dump-enter?
            tjit-dump-exit?
            tjit-dump-bytecode?
            tjit-dump-scm?
            tjit-dump-cps?
            parse-tjit-dump-flags
            set-tjit-dump-option!

            tjit-max-spills
            tjit-stats))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_tjit")

;; Record type for configuring dump options.
(define-record-type <tjit-dump>
  (make-tjit-dump locals enter exit bytecode scm cps)
  tjit-dump?
  (locals tjit-dump-locals? set-tjit-dump-locals!)
  (enter tjit-dump-enter? set-tjit-dump-enter!)
  (exit tjit-dump-exit? set-tjit-dump-exit!)
  (bytecode tjit-dump-bytecode? set-tjit-dump-bytecode!)
  (scm tjit-dump-scm? set-tjit-dump-scm!)
  (cps tjit-dump-cps? set-tjit-dump-cps!))

(define (make-empty-tjit-dump-option)
  "Makes tjit-dump data with all fields set to #f"
  (make-tjit-dump #f #f #f #f #f #f))

(define tjit-dump-option
  ;; Parameter to control dump setting during compilation of traces.
  (make-parameter (make-empty-tjit-dump-option)))

(define (parse-tjit-dump-flags str)
  "Parse dump flags in string STR and return <tjit-dump> data.

Flags are:

- 'l': dump locals
- 'e': dump entry
- 'x': dump exit
- 'b': dump bytecodes
- 's': dump scheme IR
- 'c': dump CPS IR

For instance, @code{(parse-tjit-dump-flags \"lexb\")} will return a <tjit-dump>
data with locals, entry, exit, and bytecodes field set to #t and other fields to
#f."
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
        (flags (#\l set-tjit-dump-locals!)
               (#\e set-tjit-dump-enter!)
               (#\x set-tjit-dump-exit!)
               (#\b set-tjit-dump-bytecode!)
               (#\s set-tjit-dump-scm!)
               (#\c set-tjit-dump-cps!))))))

(define (set-tjit-dump-option! str)
  "Set @code{tjit-dump-option} parameter with flags in STR."
  (tjit-dump-option (parse-tjit-dump-flags str)))

(define tjit-max-spills
  ;; Maximum number of spilled variables.
  (make-parameter 256))

(define (tjit-stats)
  "Returns statistics of vm-tjit engine."
  (let ((hot-loop (tjit-hot-loop))
        (hot-exit (tjit-hot-exit))
        (num-loops 0)
        (num-hot-loops 0))
    (hash-fold (lambda (k v acc)
                 (set! num-loops (+ num-loops 1))
                 (when (< hot-loop v)
                   (set! num-hot-loops (+ num-hot-loops 1))))
               '()
               (tjit-ip-counter))
    (list `(hot-loop . ,hot-loop)
          `(hot-exit . ,hot-exit)
          `(num-loops . ,num-loops)
          `(num-hot-loops . ,num-hot-loops))))
