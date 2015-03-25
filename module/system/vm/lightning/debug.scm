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

;; Parameter to control verbosity level.
;;
;; Current policy:
;; - #f : Do not show messages.
;; -  0 : Show message when something went wrong (default).
;; -  1 : Informative messages in scheme code.
;; -  2 : More informative messages in scheme code.
;; -  3 : All of the above, and dump from lightning C library.
;;
(define lightning-verbosity (make-parameter 0))

(define-syntax-rule (debug n args ...)
  (let ((verbosity (lightning-verbosity)))
    (when (and verbosity (<= n verbosity))
      (format #t args ...))))

(define-syntax-rule (program-name program-or-addr)
  (let ((name (cond
               ((integer? program-or-addr)
                (and=> (find-program-debug-info program-or-addr)
                       program-debug-info-name))
               ((procedure? program-or-addr)
                (procedure-name program-or-addr))
               (else
                (error "program-name: got " program-or-addr)))))
    (or (and=> name symbol->string)
        "anonymous")))
