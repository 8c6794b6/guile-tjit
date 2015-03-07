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

;;; Debugging codes for vm-lightning.

;;; Code:

(define-module (system vm lightning debug)
  #:use-module (system vm debug)
  #:export (lightning-verbosity debug program-name))

;;; Parameter to control verbosity level.
(define lightning-verbosity (make-parameter #f))

(define-syntax-rule (debug n args ...)
  (let ((verbosity (lightning-verbosity)))
    (when (and verbosity (<= n verbosity))
      (format #t args ...))))

(define-syntax-rule (program-name program-or-addr)
  (let* ((addr (cond
                ((integer? program-or-addr)
                 program-or-addr)
                ((program? program-or-addr)
                 (program-code program-or-addr))
                (else
                 (error "program-name: not a program" program-or-addr))))
         (name (and=> (find-program-debug-info addr)
                      program-debug-info-name)))
    (or (and=> name symbol->string)
        "anonymous")))
