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

;;; Register related codes. Currently works under x86-64 only, using
;;; architecture dependent registers.  Lightning has it's own register
;;; management policy, not sure how it works under other architecture than
;;; x86-64 linux.

;;; Code:

(define-module (system vm native tjit registers)
  #:use-module (system vm native lightning)
  #:export (*num-registers*
            register-ref))

(define r8 (jit-r 8))
(define r9 (jit-r 9))
(define rcx (jit-r 10))
(define rdx (jit-r 11))
(define rsi (jit-r 12))
(define rdi (jit-r 13))

(define *the-registers*
  `#(,v1 ,v2 ,v3 ,r3 ,r8 ,r9 ,rcx ,rdx ,rsi ,rdi))

(define *num-registers*
  (+ (vector-length *the-registers*) 1))

(define (register-ref i)
  (vector-ref *the-registers* i))
