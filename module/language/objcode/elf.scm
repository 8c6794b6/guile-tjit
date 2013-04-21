;;; Embedding bytecode in ELF

;; Copyright (C) 2012, 2013 Free Software Foundation, Inc.

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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

;; The eval-when is because (language objcode elf) will not be loaded
;; yet when we go to compile it, but later passes of the
;; compiler need it.  So we have to be sure that the module is present
;; at compile time, with all of its definitions.  The easiest way to do
;; that is just to go ahead and resolve it now.
;;
(define-module (language objcode elf)
  #:use-module (system vm objcode)
  #:use-module (system base target)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (system vm elf)
  #:use-module (system vm linker)
  #:export (write-objcode))

(define (bytecode->elf bv)
  (let ((string-table (make-string-table)))
    (define (intern-string! string)
      (call-with-values
          (lambda () (string-table-intern string-table string))
        (lambda (table idx)
          (set! string-table table)
          idx)))
    (define (make-object index name bv relocs . kwargs)
      (let ((name-idx (intern-string! (symbol->string name))))
        (make-linker-object (apply make-elf-section
                                   #:index index
                                   #:name name-idx
                                   #:size (bytevector-length bv)
                                   kwargs)
                            bv relocs
                            (list (make-linker-symbol name 0)))))
    (define (make-dynamic-section index word-size endianness)
      (define (make-dynamic-section/32)
        (let ((bv (make-bytevector 24 0)))
          (bytevector-u32-set! bv 0 DT_GUILE_RTL_VERSION endianness)
          (bytevector-u32-set! bv 4 #x02000000 endianness)
          (bytevector-u32-set! bv 8 DT_GUILE_ENTRY endianness)
          (bytevector-u32-set! bv 12 0 endianness)
          (bytevector-u32-set! bv 16 DT_NULL endianness)
          (bytevector-u32-set! bv 20 0 endianness)
          (values bv (make-linker-reloc 'abs32/1 12 0 '.rtl-text))))
      (define (make-dynamic-section/64)
        (let ((bv (make-bytevector 48 0)))
          (bytevector-u64-set! bv 0 DT_GUILE_RTL_VERSION endianness)
          (bytevector-u64-set! bv 8 #x02000000 endianness)
          (bytevector-u64-set! bv 16 DT_GUILE_ENTRY endianness)
          (bytevector-u64-set! bv 24 0 endianness)
          (bytevector-u64-set! bv 32 DT_NULL endianness)
          (bytevector-u64-set! bv 40 0 endianness)
          (values bv (make-linker-reloc 'abs64/1 24 0 '.rtl-text))))
      (call-with-values (lambda ()
                          (case word-size
                            ((4) (make-dynamic-section/32))
                            ((8) (make-dynamic-section/64))
                            (else (error "unexpected word size" word-size))))
        (lambda (bv reloc)
          (make-object index '.dynamic bv (list reloc)
                       #:type SHT_DYNAMIC #:flags SHF_ALLOC))))
    (define (make-string-table index)
      (intern-string! ".shstrtab")
      (make-object index '.shstrtab (link-string-table string-table) '()
                   #:type SHT_STRTAB #:flags 0))
    (let* ((word-size (target-word-size))
           (endianness (target-endianness))
           (text (make-object 1 '.rtl-text bv '()))
           (dt (make-dynamic-section 2 word-size endianness))
           ;; This needs to be linked last, because linking other
           ;; sections adds entries to the string table.
           (shstrtab (make-string-table 3)))
      (link-elf (list text dt shstrtab)
                #:endianness endianness #:word-size word-size))))

(define (write-objcode objcode port)
  (let ((bv (objcode->bytecode objcode (target-endianness))))
    (put-bytevector port (bytecode->elf bv))))
