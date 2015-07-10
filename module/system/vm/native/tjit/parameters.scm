;;; -*- mode: scheme; coding: utf-8; -*-

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

;;; Parameters and statistics in vm-tjit.

;;; Code:

(define-module (system vm native tjit parameters)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:export (tjit-ip-counter
            tjit-hot-loop
            tjit-hot-exit
            set-tjit-hot-loop!

            <nlog>
            make-nlog
            dump-nlog
            nlog-id
            nlog-code
            nlog-entry-ip
            nlog-snapshots
            nlog-loop-start-address
            nlog-loop-header

            put-nlog!
            get-nlog

            tjit-stats))

;;; Dummy to silent warning message. Contents of `nlog-table' is filled in by
;;; initialization function in C below.
(define (nlog-table) #f)

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_vm_tjit")


;;;
;;; Native compilation log
;;;

;;; Record type to contain various information for native code compilation.
;;; Information stored in this record type is used during patching native code
;;; from side exit.
;;;
;;; This record type is shared with C code. Macros written in
;;; "libguile/vm-tjit.h" with "SCM_NLOG" prefix are referring the
;;; contents.
;;;
(define-record-type <nlog>
  (%make-nlog id code exit-counts entry-ip
              snapshots loop-start-address loop-header)
  nlog?

  ;; Trace id number.
  (id nlog-id)

  ;; Bytevector of compiled native code.
  (code nlog-code)

  ;; Hash-table containing number of exits taken, per exit-id.
  (exit-counts nlog-exit-counts)

  ;; Entry bytecode IP.
  (entry-ip nlog-entry-ip)

  ;; Snapshot locals and types.
  (snapshots nlog-snapshots)

  ;; Address of start of the loop in native code.
  (loop-start-address nlog-loop-start-address)

  ;; Header information of loop.
  (loop-header nlog-loop-header))

(define (make-nlog id code exit-counts entry-ip snapshots loop-addr loop-header)
  (%make-nlog id code exit-counts entry-ip snapshots loop-addr loop-header))

(define (dump-nlog nlog)
  (format #t "~19@a~%" "*** nlog ***")
  (format #t "~19@a: ~a~%" 'id (nlog-id nlog))
  (format #t "~19@a: addr=~a size=~a~%" 'code
          (bytevector->pointer (nlog-code nlog))
          (bytevector-length (nlog-code nlog)))
  (format #t "~19@a: ~{~a ~}~%" 'exit-counts
          (reverse! (hash-fold acons '() (nlog-exit-counts nlog))))
  (format #t "~19@a: ~x~%" 'entry-ip (nlog-entry-ip nlog))
  (format #t "~19@a: ~{~a~^~%                     ~}~%" 'snapshots
          (let ((snapshots (nlog-snapshots nlog)))
            (sort (hash-fold acons '() snapshots)
                  (lambda (a b)
                    (< (car a) (car b))))))
  (format #t "~19@a: ~a~%" 'loop-start-address (nlog-loop-start-address nlog))
  (format #t "~19@a: ~a~%" 'loop-header (nlog-loop-header nlog)))

(define (put-nlog! key nlog)
  (hashq-set! (nlog-table) key nlog))

(define (get-nlog key)
  (hashq-ref (nlog-table) key #f))


;;;
;;; Statistics
;;;

(define (tjit-stats)
  (let ((num-loops 0)
        (num-hot-loops 0)
        (hot-loop (tjit-hot-loop)))
    (hash-fold (lambda (k v acc)
                 (set! num-loops (+ num-loops 1))
                 (when (< hot-loop v)
                   (set! num-hot-loops (+ num-hot-loops 1))))
               '()
               (tjit-ip-counter))
    (list `(hot-loop . ,hot-loop)
          `(num-loops . ,num-loops)
          `(num-hot-loops . ,num-hot-loops))))
