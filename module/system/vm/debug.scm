;;; Guile runtime debug information

;;; Copyright (C) 2013 Free Software Foundation, Inc.
;;;
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

;;; Commentary:
;;;
;;; Guile's RTL compiler and linker serialize debugging information into
;;; separate sections of the ELF image.  This module reads those
;;; sections.
;;;
;;; Code:

(define-module (system vm debug)
  #:use-module (system vm elf)
  #:use-module (system vm objcode)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (debug-context-image

            program-debug-info-name
            program-debug-info-context
            program-debug-info-image
            program-debug-info-offset
            program-debug-info-addr
            program-debug-info-u32-offset
            program-debug-info-u32-offset-end

            find-debug-context
            find-program-debug-info))

;;; A compiled procedure comes from a specific loaded ELF image.  A
;;; debug context identifies that image.
;;;
(define-record-type <debug-context>
  (make-debug-context elf base text-base)
  debug-context?
  (elf debug-context-elf)
  ;; Address at which this image is loaded in memory, in bytes.
  (base debug-context-base)
  ;; Offset of the text section relative to the image start, in bytes.
  (text-base debug-context-text-base))

(define (debug-context-image context)
  "Return the bytevector aliasing the mapped ELF image corresponding to
@var{context}."
  (elf-bytes (debug-context-elf context)))

;;; A program debug info (PDI) is a handle on debugging meta-data for a
;;; particular program.
;;;
(define-record-type <program-debug-info>
  (make-program-debug-info context name offset size)
  program-debug-info?
  (context program-debug-info-context)
  (name program-debug-info-name)
  ;; Offset of the procedure in the text section, in bytes.
  (offset program-debug-info-offset)
  (size program-debug-info-size))

(define (program-debug-info-addr pdi)
  "Return the address in memory of the entry of the program represented
by the debugging info @var{pdi}."
  (+ (program-debug-info-offset pdi)
     (debug-context-text-base (program-debug-info-context pdi))
     (debug-context-base (program-debug-info-context pdi))))

(define (program-debug-info-image pdi)
  "Return the ELF image containing @var{pdi}, as a bytevector."
  (debug-context-image (program-debug-info-context pdi)))

(define (program-debug-info-u32-offset pdi)
  "Return the start address of the program represented by @var{pdi}, as
an offset from the beginning of the ELF image in 32-bit units."
  (/ (+ (program-debug-info-offset pdi)
        (debug-context-text-base (program-debug-info-context pdi)))
     4))

(define (program-debug-info-u32-offset-end pdi)
  "Return the end address of the program represented by @var{pdi}, as an
offset from the beginning of the ELF image in 32-bit units."
  (/ (+ (program-debug-info-size pdi)
        (program-debug-info-offset pdi)
        (debug-context-text-base (program-debug-info-context pdi)))
     4))

(define (find-debug-context addr)
  "Find and return the debugging context corresponding to the ELF image
containing the address @var{addr}.  @var{addr} is an integer."
  (let* ((bv (find-mapped-elf-image addr))
         (elf (parse-elf bv))
         (base (pointer-address (bytevector->pointer (elf-bytes elf))))
         (text-base (elf-section-offset
                     (or (elf-section-by-name elf ".rtl-text")
                         (error "ELF object has no text section")))))
    (make-debug-context elf base text-base)))

(define (find-elf-symbol elf text-offset)
  "Search the symbol table of @var{elf} for the ELF symbol containing
@var{text-offset}.  @var{text-offset} is a byte offset in the text
section of the ELF image.  Returns an ELF symbol, or @code{#f}."
  (and=>
   (elf-section-by-name elf ".symtab")
   (lambda (symtab)
     (let ((len (elf-symbol-table-len symtab))
           (strtab (elf-section elf (elf-section-link symtab))))
       ;; The symbols should be sorted, but maybe somehow that fails
       ;; (for example if multiple objects are relinked together).  So,
       ;; a modicum of tolerance.
       (define (bisect)
         ;; FIXME: Implement.
         #f)
       (define (linear-search)
         (let lp ((n 0))
           (and (< n len)
                (let ((sym (elf-symbol-table-ref elf symtab n strtab)))
                  (if (and (<= (elf-symbol-value sym) text-offset)
                           (< text-offset (+ (elf-symbol-value sym)
                                             (elf-symbol-size sym))))
                      sym
                      (lp (1+ n)))))))
       (or (bisect) (linear-search))))))

(define* (find-program-debug-info addr #:optional
                                  (context (find-debug-context addr)))
  "Find and return the @code{<program-debug-info>} containing
@var{addr}, or @code{#f}."
  (cond
   ((find-elf-symbol (debug-context-elf context)
                     (- addr
                        (debug-context-base context)
                        (debug-context-text-base context)))
    => (lambda (sym)
         (make-program-debug-info context
                                  (and=> (elf-symbol-name sym)
                                         ;; The name might be #f if
                                         ;; the string table was
                                         ;; stripped somehow.
                                         (lambda (x)
                                           (and (string? x)
                                                (not (string-null? x))
                                                (string->symbol x))))
                                  (elf-symbol-value sym)
                                  (elf-symbol-size sym))))
   (else #f)))
