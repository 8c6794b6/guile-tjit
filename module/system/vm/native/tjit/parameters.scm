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
            tjit-dump-exit?
            parse-tjit-dump-flags

            tjit-max-spills
            tjit-stats))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_tjit")

;; Record type for configuring dump options.
(define-record-type <tjit-dump>
  (make-tjit-dump locals exit)
  tjit-dump?
  (locals tjit-dump-locals? set-tjit-dump-locals!)
  (exit tjit-dump-exit? set-tjit-dump-exit!))

(define (make-empty-tjit-dump-option)
  "Makes tjit-dump data with all fields set to #f"
  (make-tjit-dump #f #f))

(define tjit-dump-option
  ;; Parameter to control dump setting during compilation of traces.
  (make-parameter (make-empty-tjit-dump-option)))

(define (parse-tjit-dump-flags str)
  "Parse dump flags in string STR."
  (let ((o (make-empty-tjit-dump-option)))
    (let lp ((cs (string->list str)))
      (let-syntax ((p (syntax-rules ()
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
        (p (#\l set-tjit-dump-locals!)
           (#\x set-tjit-dump-exit!))))))

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
