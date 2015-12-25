;;;; State for JIT compilation

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
;;; State during JIT compilation of single trace.
;;;
;;; Code:

(define-module (system vm native tjit state)
  #:use-module (srfi srfi-9)
  #:export ($tj
            make-tj
            tj?
            tj-id
            tj-entry-ip
            tj-linked-ip
            tj-parent-exit-id
            tj-parent-fragment
            tj-parent-snapshot
            tj-outline
            tj-loop?
            tj-downrec?
            tj-uprec?
            tj-handle-interrupts?
            set-tj-handle-interrupts!
            tj-initial-types))

(define-record-type $tj
  (make-tj id entry-ip linked-ip
           parent-exit-id parent-fragment parent-snapshot outline
           loop? downrec? uprec? handle-interrupts? initial-types)
  tj?

  ;; Trace ID of this compilation.
  (id tj-id)

  ;; Entry IP of trace.
  (entry-ip tj-entry-ip)

  ;; Linked IP, if any.
  (linked-ip tj-linked-ip)

  ;; Parent exit id of this trace.
  (parent-exit-id tj-parent-exit-id)

  ;; Parent fragment of this trace, or #f for root trace.
  (parent-fragment tj-parent-fragment)

  ;; Parent snapshot, snapshot of parent-exit-id in parent fragment.
  (parent-snapshot tj-parent-snapshot)

  ;; Past frame info of this trace.
  (outline tj-outline)

  ;; Flag for loop trace.
  (loop? tj-loop?)

  ;; Flag for down recursion trace.
  (downrec? tj-downrec?)

  ;; Flag for up recursion trace.
  (uprec? tj-uprec?)

  ;; Flag to emit interrupt handler.
  (handle-interrupts? tj-handle-interrupts? set-tj-handle-interrupts!)

  ;; Initial stack element types
  (initial-types tj-initial-types))
