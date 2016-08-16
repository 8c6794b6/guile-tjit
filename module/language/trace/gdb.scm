;;;; GDB JIT interface

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
;;; Module to support GDB JIT interface. Contains procedures to make in-memory
;;; ELF object from JIT compiled native code.
;;;
;;; As of gdb-7.10.1, to support repeated run in single GDB session, the option
;;; `disable-randomization' need to be set to `off'. This could be done with
;;; invoking:
;;;
;;;   set disasble-randomization off
;;;
;;; in (gdb) prompt, or write it in config file (e.g.: .gdbinit). This is
;;; because symbols used for GDB JIT are defined in shared library (i.e.:
;;; libguile-XXX.so). The symbol addresses resolved at first run in GDB session
;;; get cached and reused from second run, but the address in shared libraries
;;; get new when the `disable-randomization' option is set to `on'.
;;;
;;; Sample GDB session:
;;;
;;;   $ cat > /tmp/loop.scm
;;;   (define (loop n)
;;;     (let outer ((i n))
;;;       (if (< 0 i)
;;;           (let inner ((j n))
;;;             (if (< 0 j)
;;;                 (inner (- j 1))
;;;                 (outer (- i 1))))
;;;           i)))
;;;   (loop 100)
;;;   ^D
;;;   $ gdb --quiet --args guile --tjit --tjit-dump=d /tmp/loop.scm
;;;   Reading symbols from guile...done.
;;;   (gdb) set breakpoint pending on
;;;   (gdb) tb trace1
;;;   Function "trace1" not defined.
;;;   Temporary breakpoint 1 (trace1) pending.
;;;   (gdb) tb trace2
;;;   Function "trace2" not defined.
;;;   Temporary breakpoint 2 (trace2) pending.
;;;   (gdb) tb trace3
;;;   Function "trace3" not defined.
;;;   Temporary breakpoint 3 (trace3) pending.
;;;   (gdb) run
;;;   Starting program: guile --tjit --tjit-dump=d /tmp/loop.scm
;;;   [Thread debugging using libthread_db enabled]
;;;   Using host libthread_db library "/lib/libthread_db.so.1".
;;;   [New Thread 0x7fc8185ca700 (LWP 12873)]
;;;   [New Thread 0x7fc817d79700 (LWP 12874)]
;;;   [New Thread 0x7fc817528700 (LWP 12875)]
;;;
;;;   Temporary breakpoint 1, trace1 () at /tmp/loop.scm:6
;;;   6               (inner (- j 1))
;;;   (gdb) list
;;;   1	(define (loop n)
;;;   2   (let outer ((i n))
;;;   3     (if (< 0 i)
;;;   4         (let inner ((j n))
;;;   5           (if (< 0 j)
;;;   6               (inner (- j 1))
;;;   7               (outer (- i 1))))
;;;   8         i)))
;;;   9	(loop 100)
;;;   (gdb) bt 1
;;;   #0  trace1 () at /tmp/loop.scm:6
;;;   (More stack frames follow...)
;;;   (gdb) cont
;;;   Continuing.
;;;
;;;   Temporary breakpoint 2, trace2 () at /tmp/loop.scm:5
;;;   5           (if (< 0 j)
;;;   (gdb) cont
;;;   Continuing.
;;;
;;;   Temporary breakpoint 3, trace3 () at /tmp/loop.scm:7
;;;   7               (outer (- i 1)))))))
;;;   (gdb) disas trace3
;;;   Dump of assembler code for function trace3:
;;;   => 0x0000000001020020 <+0>:	lea    rax,[r14*4+0x0]
;;;      0x0000000001020028 <+8>:	add    rax,0x2
;;;      0x000000000102002c <+12>:	mov    QWORD PTR [rbx],rax
;;;      0x000000000102002f <+15>:	mov    QWORD PTR [rbx+0x18],r15
;;;      0x0000000001020033 <+19>:	mov    rax,QWORD PTR [rbx+0x8]
;;;      0x0000000001020037 <+23>:	test   rax,0x2
;;;      0x000000000102003d <+29>:	je     0x101c020
;;;      0x0000000001020043 <+35>:	mov    r9,rax
;;;      ...
;;;   (gdb) q
;;;
;;; Code:

(define-module (language trace gdb)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 vlist)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system base target)
  #:use-module (system foreign)
  #:use-module (system vm dwarf)
  #:use-module (system vm elf)
  #:use-module (system vm linker)
  #:export (make-gdb-jit-elf))


;;;; Auxiliary

(define-record-type <gdb>
  (%make-gdb next-section-number const symtab shstrtab)
  gdb?
  (next-section-number gdb-next-section-number set-gdb-next-section-number!)
  (const gdb-const set-gdb-const!)
  (symtab gdb-symtab set-gdb-symtab!)
  (shstrtab gdb-shstrtab set-gdb-shstrtab!))

(define (make-gdb)
  (%make-gdb 1 '() '() (make-string-table)))

(define (%put-u8 port val)
  (let ((bv (make-bytevector 1)))
    (bytevector-u8-set! bv 0 val)
    (put-bytevector port bv)))

(define (%put-uleb128 port val)
  (let lp ((val val))
    (let ((next (ash val -7)))
      (if (zero? next)
          (%put-u8 port val)
          (begin
            (%put-u8 port (logior #x80 (logand val #x7f)))
            (lp next))))))

(define (%put-sleb128 port val)
  (let lp ((val val))
    (if (<= 0 (+ val 64) 127)
        (put-u8 port (logand val #x7f))
        (begin
          (%put-u8 port (logior #x80 (logand val #x7f)))
          (lp (ash val -7))))))

(define (port-position port)
  (seek port 0 SEEK_CUR))

(define SHN_ABS #xfff1)

(define (intern-section-name! gdb str)
  (string-table-intern! (gdb-shstrtab gdb) str))

(define (make-object gdb name bv relocs labels . kwargs)
  (let ((name-idx (intern-section-name! gdb (symbol->string name)))
        (index (gdb-next-section-number gdb)))
    (set-gdb-next-section-number! gdb (+ index 1))
    (make-linker-object (apply make-elf-section
                               #:index index
                               #:name name-idx
                               #:size (bytevector-length bv)
                               kwargs)
                        bv relocs
                        (cons (make-linker-symbol name 0) labels))))

(define (link-text gdb)
  (make-object gdb '.text #vu8() '() '()
               #:type SHT_NOBITS
               #:flags (logior SHF_EXECINSTR SHF_ALLOC)
               #:addralign 16))

(define (link-symtab gdb trace-id text-section ncode-size)
  (let* ((endianness (target-endianness))
         (word-size (target-word-size))
         (size (elf-symbol-len word-size))
         (strtab (make-string-table))
         (bv (make-bytevector (* 3 size) 0))
         (file-name (string-table-intern! strtab "JIT ncode"))
         (file (make-elf-symbol #:name file-name
                                #:size 0
                                #:binding STB_LOCAL
                                #:type STT_FILE
                                #:visibility STV_DEFAULT
                                #:shndx SHN_ABS))
         (_ (write-elf-symbol bv size endianness word-size file))
         (label (string-append "trace" (number->string trace-id)))
         (func-name (string-table-intern! strtab label))
         (func (make-elf-symbol #:name func-name
                                #:size ncode-size
                                #:binding STB_GLOBAL
                                #:type STT_FUNC
                                #:visibility STV_DEFAULT
                                #:shndx (elf-section-index text-section)))
         (_ (write-elf-symbol bv (* 2 size) endianness word-size func))
         (strtab (make-object gdb '.strtab
                              (link-string-table! strtab) '() '()
                              #:type SHT_STRTAB
                              #:flags 0))
         (strtab-index (elf-section-index (linker-object-section strtab)))
         (symtab (make-object gdb '.symtab
                              bv '() '()
                              #:type SHT_SYMTAB
                              #:flags 0
                              #:entsize size
                              #:link strtab-index
                              #:info 2)))
    (values symtab strtab)))

(define (link-debug gdb naddr nsize src linenum)
  (define-syntax gen-setter
    (syntax-rules ()
      ((_ port setter)
       (lambda (val)
         (setter port val)))
      ((_ port setter size)
       (lambda (val)
         (let ((bv (make-bytevector size)))
           (setter bv 0 val)
           (put-bytevector port bv))))
      ((_ port setter size endian)
       (lambda (val)
         (let ((bv (make-bytevector size)))
           (setter bv 0 val endian)
           (put-bytevector port bv))))))
  (define-syntax define-puts
    (syntax-rules ()
      ((_ acc port)
       (values . acc))
      ((_ acc port setter-info . rest)
       (define-puts ((gen-setter port . setter-info) . acc)
         port . rest))))
  (define-syntax-rule (define-puts-for-port port endian)
    (define-puts () port
      (%put-sleb128)
      (%put-uleb128)
      (bytevector-u64-set! 8 endian)
      (bytevector-u32-set! 4 endian)
      (bytevector-u16-set! 2 endian)
      (bytevector-s8-set! 1)
      (bytevector-u8-set! 1)))
  (define word-size (sizeof '*))
  (define src-path
    (or (%search-load-path src) ""))
  (let-values (((info-port get-info-bv) (open-bytevector-output-port))
               ((abbrev-port get-abbrev-bv) (open-bytevector-output-port))
               ((line-port get-line-bv) (open-bytevector-output-port)))
    (define (make-debug-info strtab endian)
      (let*-values (((u8 s8 u16 u32 u64 uleb128 sleb128)
                     (define-puts-for-port info-port endian))
                    ((uword) (cond
                              ((eq? word-size 4) u32)
                              ((eq? word-size 8) u64)
                              (else (error "Unknown word-size"))))
                    ((src-code) (string-table-intern! strtab src-path)))
        (u32 0)                         ; Length, updated later
        (u16 2)                         ; DWARF version 2
        (u32 0)                         ; Abbrev offset
        (u8 word-size)                  ; Address size
        (uleb128 1)                     ; Abbrev #1: DW_TAG_compile_unit
        (u32 src-code)                  ; DW_AT_name
        (uword naddr)                   ; DW_AT_low_pc
        (uword (+ naddr nsize))         ; DW_AT_high_pc
        (u32 0)                         ; DW_AT_stmt_list
        (let ((bv (get-info-bv)))
          ;; Update DWARF32 length.
          (bytevector-u32-set! bv 0 (- (bytevector-length bv) 4) endian)
          (make-object gdb '.debug_info bv '() '()
                       #:type SHT_PROGBITS
                       #:flags 0))))
    (define (make-debug-abbrev strtab endian)
      (let-values (((u8 s8 u16 u32 u64 uleb128 sleb128)
                    (define-puts-for-port abbrev-port endian)))
        (uleb128 1)                     ; Abbrev number
        (uleb128 (tag-name->code 'compile-unit))
        (u8 (children-name->code 'no))
        (uleb128 (attribute-name->code 'name))
        (uleb128 (form-name->code 'strp))
        (uleb128 (attribute-name->code 'low-pc))
        (uleb128 (form-name->code 'addr))
        (uleb128 (attribute-name->code 'high-pc))
        (uleb128 (form-name->code 'addr))
        (uleb128 (attribute-name->code 'stmt-list))
        (uleb128 (form-name->code 'data4))
        (uleb128 0)
        (uleb128 0)
        (uleb128 0)
        (make-object gdb '.debug_abbrev (get-abbrev-bv) '() '()
                     #:type SHT_PROGBITS
                     #:flags 0)))
    (define (make-debug-line strtab endian)
      (let*-values (((u8 s8 u16 u32 u64 uleb128 sleb128)
                     (define-puts-for-port line-port endian))
                    ((uword) (cond
                              ((eq? word-size 4) u32)
                              ((eq? word-size 8) u64)
                              (else (error "Unknown word-size"))))
                    ((src-code) (string-table-intern! strtab src-path))
                    ((src-bv) (string->utf8 src-path))
                    ((extended-op) (lambda (op payload-len)
                                     (u8 0)
                                     (uleb128 (+ payload-len 1))
                                     (uleb128 op))))
        (u32 0)                         ; Length, patched later
        (u16 2)                         ; Dwarf version 2
        (u32 0)                         ; Prologue length, patched later
        (u8 1)                          ; Minimum instruction length
        (u8 1)                          ; is_stmt

        (s8 0)                          ; Line base for special opcodes
        (u8 2)                          ; Line range for special opcodes
        (u8 (+ 3 1))                    ; Opcode bases in use

        (u8 0)                          ; OP 1: copy
        (u8 1)                          ; OP 2: advance-pc
        (u8 1)                          ; OP 3: advance-line

        (u8 0)                          ; Directory table

        (put-bytevector line-port src-bv) ; File name
        (u8 0)                            ; Terminate string
        (uleb128 0)                       ; Directory
        (uleb128 0)                       ; Mtime
        (uleb128 0)                       ; Size
        (u8 0)                            ; Terminate file list

        ;; Patch prologue length
        (let ((offset (port-position line-port)))
          (seek line-port 6 SEEK_SET)
          (u32 (- offset 10))
          (seek line-port offset SEEK_SET))

        ;; Line number program
        (extended-op 2 word-size) (uword naddr) ; Extended op `set-address'
        (u8 3) (uleb128 (1- linenum))           ; Standard op `advance-line'
        (u8 1)                                  ; Standard op `copy'
        (u8 2) (uleb128 nsize)                  ; Standard op `advance-pc'
        (extended-op 1 0)                       ; Extended op `end-sequence'

        (let ((bv (get-line-bv)))
          ;; Patch length.
          (bytevector-u32-set! bv 0 (- (bytevector-length bv) 4) endian)
          (make-object gdb '.debug_line bv '() '()
                       #:type SHT_PROGBITS
                       #:flags 0))))
    (let* ((strtab (make-string-table))
           (endian (target-endianness))
           (debug-info (make-debug-info strtab endian))
           (debug-abbrev (make-debug-abbrev strtab endian))
           (debug-line (make-debug-line strtab endian))
           (debug-str (make-object gdb '.debug_str
                                   (link-string-table! strtab) '() '()
                                   #:type SHT_PROGBITS
                                   #:flags 0)))
      (values debug-info debug-abbrev debug-str debug-line))))

(define (link-shstrtab gdb)
  (intern-section-name! gdb ".shstrtab")
  (make-object gdb '.shstrtab
               (link-string-table! (gdb-shstrtab gdb)) '() '()
               #:type SHT_STRTAB
               #:flags 0))

(define (rewrite-address! elf naddr nsize)
  "Rewrite address and size in .text section in ELF to point NADDR."
  (let ((bv (elf-bytes elf))
        (byte-order (elf-byte-order elf))
        (tsoff (+ (elf-shoff elf) (elf-shentsize elf)))
        (phoff0 (elf-phoff elf))
        (phoff1 (+ (elf-phoff elf)
                   (elf-program-header-len (elf-word-size elf)))))

    ;; Update address, size, and offset in .text section.
    (bytevector-u64-set! bv (+ tsoff 16) naddr byte-order)
    (bytevector-u64-set! bv (+ tsoff 24) #x0 byte-order)
    (bytevector-u64-set! bv (+ tsoff 32) nsize byte-order)

    ;; Update program headers. (@@ (system vm linker) link-elf) makes two
    ;; program headers, which are unused in GDB JIT. Marking the program header
    ;; types as `PT_NULL'.
    (bytevector-u32-set! bv phoff0 PT_NULL byte-order)
    (bytevector-u32-set! bv phoff1 PT_NULL byte-order)))


;;;; Exported

(define (make-gdb-jit-elf trace-id naddr nsize src linenum)
  "Make a bytevector filled with ELF object for GDB JIT interface.

Uses TRACE-ID to make temorary function name to break in GDB, and NADDR which
should point the address of native code. The temporary function name will be
`trace~a', where `~a' replaced with TRACE-ID. Currently for x86-64 only."
  (let*-values
      (((gdb) (make-gdb))
       ((text) (link-text gdb))
       ((symtab strtab) (link-symtab gdb trace-id
                                     (linker-object-section text)
                                     nsize))
       ((debug-info debug-abbrev debug-str debug-line)
        (link-debug gdb naddr nsize src linenum))
       ((shstrtab) (link-shstrtab gdb))
       ((objects) (list text
                        debug-info debug-abbrev debug-line debug-str
                        symtab strtab shstrtab))
       ((bv) (link-elf objects
                       #:page-aligned? #f
                       #:abi ELFOSABI_NONE
                       #:type ET_REL
                       #:machine-type EM_X86_64))
       ((elf) (parse-elf bv)))
    (rewrite-address! elf naddr nsize)
    (elf-bytes elf)))
