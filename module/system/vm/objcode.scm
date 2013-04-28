;;; Guile VM object code

;; Copyright (C) 2001, 2010, 2012, 2013 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system vm objcode)
  #:export (objcode? objcode-meta
            bytecode->objcode objcode->bytecode
            load-thunk-from-file load-thunk-from-memory
            word-size byte-order
            find-mapped-elf-image))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_objcodes")
