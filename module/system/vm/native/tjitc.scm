;;;; Entry point for compiler used in vm-tjit engine

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
;;; Module exporting @code{tjitc}, entry point of just-in-time compiler for
;;; `vm-tjit' engine. The procedure @code{tjitc} is called from C code in
;;; "libguile/vm-tjit.c".
;;;
;;; Code:

(define-module (system vm native tjitc)
  #:use-module (ice-9 match)
  #:use-module (system base compile)
  #:use-module (system vm native debug)
  #:use-module (language trace dump)
  #:use-module (language trace env)
  #:use-module (language trace fragment)
  #:use-module (language trace parameters)
  #:use-module (language trace snapshot)
  #:export (tjitc init-vm-tjit)
  #:re-export (tjit-stats))


;;;
;;; Entry point for JIT compiler
;;;

(define (tjitc trace-id bv traces parent-ip parent-exit-id linked-ip
               loop? downrec? uprec?)
  "Compile recorded bytecodes in BV with TRACES and meta information."
  (let* ((entry-ip (vector-ref (car traces) 1))
         (parent-fragment (get-fragment parent-ip))
         (parent-snapshot (if parent-fragment
                              (hashq-ref (fragment-snapshots parent-fragment)
                                         parent-exit-id)
                              #f))
         (env (call-with-values
                  (lambda ()
                    (match parent-snapshot
                      (($ $snapshot id sp fp nlocals locals variables code
                          ip lives depth)
                       (values sp fp (map car locals) lives locals depth))
                      (_
                       (values 0 0 '() '() '() 0))))
                (lambda args
                  (apply make-env trace-id entry-ip linked-ip
                         parent-exit-id parent-fragment parent-snapshot
                         loop? downrec? uprec? args)))))
    (compile bv #:from 'trace #:to 'value #:env env #:opts traces)))


;;;
;;; Initialization
;;;

(define (init-vm-tjit use-debug-engine?)
  "Initialize vm-tjit, use vm-debug if USE-DEBUG-ENGINE? is true."
  ((@ (system vm native lightning) init-jit) "")
  (when use-debug-engine?
    (set-tjit-scheme-engine! 1))
  #t)

;; Call `load-extension' from top-level after defining `tjitc',
;; "scm_bootstrap_vm_tjit" will lookup `tjitc' variable and assign to C
;; variable.
(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")