;;; Guile ELF linker

;; Copyright (C)  2011, 2012, 2013 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; A linker combines several linker objects into an executable or a
;;; loadable library.
;;;
;;; There are several common formats for libraries out there.  Since
;;; Guile includes its own linker and loader, we are free to choose any
;;; format, or make up our own.
;;;
;;; There are essentially two requirements for a linker format:
;;; libraries should be able to be loaded with the minimal amount of
;;; work; and they should support introspection in some way, in order to
;;; enable good debugging.
;;;
;;; These requirements are somewhat at odds, as loading should not have
;;; to stumble over features related to introspection.  It so happens
;;; that a lot of smart people have thought about this situation, and
;;; the ELF format embodies the outcome of their thinking.  Guile uses
;;; ELF as its format, regardless of the platform's native library
;;; format.  It's not inconceivable that Guile could interoperate with
;;; the native dynamic loader at some point, but it's not a near-term
;;; goal.
;;;
;;; Guile's linker takes a list of objects, sorts them according to
;;; similarity from the perspective of the loader, then writes them out
;;; into one big bytevector in ELF format.
;;;
;;; It is often the case that different parts of a library need to refer
;;; to each other.  For example, program text may need to refer to a
;;; constant from writable memory.  When the linker places sections
;;; (linker objects) into specific locations in the linked bytevector,
;;; it needs to fix up those references.  This process is called
;;; /relocation/.  References needing relocations are recorded in
;;; "linker-reloc" objects, and collected in a list in each
;;; "linker-object".  The actual definitions of the references are
;;; stored in "linker-symbol" objects, also collected in a list in each
;;; "linker-object".
;;;
;;; By default, the ELF files created by the linker include some padding
;;; so that different parts of the file can be loaded in with different
;;; permissions.  For example, some parts of the file are read-only and
;;; thus can be shared between processes.  Some parts of the file don't
;;; need to be loaded at all.  However this padding can be too much for
;;; interactive compilation, when the code is never written out to disk;
;;; in that case, pass #:page-aligned? #f to `link-elf'.
;;;
;;; Code:

(define-module (system vm linker)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system base target)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 vlist)
  #:use-module (system vm elf)
  #:export (make-string-table
            string-table-intern
            link-string-table

            make-linker-reloc
            make-linker-symbol

            make-linker-object
            linker-object?
            linker-object-section
            linker-object-bv
            linker-object-relocs
            linker-object-symbols

            link-elf))

;; A relocation records a reference to a symbol.  When the symbol is
;; resolved to an address, the reloc location will be updated to point
;; to the address.
;;
;; Two types.  Abs32/1 and Abs64/1 are absolute offsets in bytes.
;; Rel32/4 is a relative signed offset in 32-bit units.  Either can have
;; an arbitrary addend as well.
;;
(define-record-type <linker-reloc>
  (make-linker-reloc type loc addend symbol)
  linker-reloc?
  (type linker-reloc-type) ;; rel32/4, abs32/1, abs64/1
  (loc linker-reloc-loc)
  (addend linker-reloc-addend)
  (symbol linker-reloc-symbol))

;; A symbol is an association between a name and an address.  The
;; address is always in regard to some particular address space.  When
;; objects come into the linker, their symbols live in the object
;; address space.  When the objects are allocated into ELF segments, the
;; symbols will be relocated into memory address space, corresponding to
;; the position the ELF will be loaded at.
;;
(define-record-type <linker-symbol>
  (make-linker-symbol name address)
  linker-symbol?
  (name linker-symbol-name)
  (address linker-symbol-address))

(define-record-type <linker-object>
  (make-linker-object section bv relocs symbols)
  linker-object?
  (section linker-object-section)
  (bv linker-object-bv)
  (relocs linker-object-relocs)
  (symbols linker-object-symbols))

(define (make-string-table)
  '(("" 0 #vu8())))

(define (string-table-length table)
  (let ((last (car table)))
    ;; The + 1 is for the trailing NUL byte.
    (+ (cadr last) (bytevector-length (caddr last)) 1)))

(define (string-table-intern table str)
  (cond
   ((assoc str table)
    => (lambda (ent)
         (values table (cadr ent))))
   (else
    (let* ((next (string-table-length table)))
      (values (cons (list str next (string->utf8 str))
                    table)
              next)))))

(define (link-string-table table)
  (let ((out (make-bytevector (string-table-length table) 0)))
    (for-each
     (lambda (ent)
       (let ((bytes (caddr ent)))
         (bytevector-copy! bytes 0 out (cadr ent) (bytevector-length bytes))))
     table)
    out))

(define (segment-kind section)
  (let ((flags (elf-section-flags section)))
    (cons (cond
           ((= (elf-section-type section) SHT_DYNAMIC) PT_DYNAMIC)
           ((zero? (logand SHF_ALLOC flags)) PT_NOTE)
           (else PT_LOAD))
          (logior (if (zero? (logand SHF_ALLOC flags))
                      0
                      PF_R)
                  (if (zero? (logand SHF_EXECINSTR flags))
                      0
                      PF_X)
                  (if (zero? (logand SHF_WRITE flags))
                      0
                      PF_W)))))

(define (group-by-cars ls)
  (let lp ((in ls) (k #f) (group #f) (out '()))
    (cond
     ((null? in)
      (reverse!
       (if group
           (cons (cons k (reverse! group)) out)
           out)))
     ((and group (equal? k (caar in)))
      (lp (cdr in) k (cons (cdar in) group) out))
     (else
      (lp (cdr in) (caar in) (list (cdar in))
          (if group
              (cons (cons k (reverse! group)) out)
              out))))))

(define (collate-objects-into-segments objects)
  (group-by-cars
   (stable-sort!
    (map (lambda (o)
           (cons (segment-kind (linker-object-section o)) o))
         objects)
    (lambda (x y)
      (let ((x-type (caar x)) (y-type (caar y))
            (x-flags (cdar x)) (y-flags (cdar y))
            (x-section (linker-object-section (cdr x)))
            (y-section (linker-object-section (cdr y))))
        (cond
         ((not (equal? x-flags y-flags))
          (< x-flags y-flags))
         ((not (equal? x-type y-type))
          (< x-type y-type))
         ((not (equal? (elf-section-type x-section)
                       (elf-section-type y-section)))
          (cond
           ((equal? (elf-section-type x-section) SHT_NOBITS) #t)
           ((equal? (elf-section-type y-section) SHT_NOBITS) #f)
           (else (< (elf-section-type x-section)
                    (elf-section-type y-section)))))
         (else
          (< (elf-section-size x-section)
             (elf-section-size y-section)))))))))

(define (align address alignment)
  (+ address
     (modulo (- alignment (modulo address alignment)) alignment)))

(define (fold1 proc ls s0)
  (let lp ((ls ls) (s0 s0))
    (if (null? ls)
        s0
        (lp (cdr ls) (proc (car ls) s0)))))

(define (fold2 proc ls s0 s1)
  (let lp ((ls ls) (s0 s0) (s1 s1))
    (if (null? ls)
        (values s0 s1)
        (receive (s0 s1) (proc (car ls) s0 s1)
          (lp (cdr ls) s0 s1)))))

(define (fold4 proc ls s0 s1 s2 s3)
  (let lp ((ls ls) (s0 s0) (s1 s1) (s2 s2) (s3 s3))
    (if (null? ls)
        (values s0 s1 s2 s3)
        (receive (s0 s1 s2 s3) (proc (car ls) s0 s1 s2 s3)
          (lp (cdr ls) s0 s1 s2 s3)))))

(define (fold5 proc ls s0 s1 s2 s3 s4)
  (let lp ((ls ls) (s0 s0) (s1 s1) (s2 s2) (s3 s3) (s4 s4))
    (if (null? ls)
        (values s0 s1 s2 s3 s4)
        (receive (s0 s1 s2 s3 s4) (proc (car ls) s0 s1 s2 s3 s4)
          (lp (cdr ls) s0 s1 s2 s3 s4)))))

(define (relocate-section-header sec fileaddr memaddr)
  (make-elf-section #:name (elf-section-name sec)
                    #:type (elf-section-type sec)
                    #:flags (elf-section-flags sec)
                    #:addr memaddr
                    #:offset fileaddr
                    #:size (elf-section-size sec)
                    #:link (elf-section-link sec)
                    #:info (elf-section-info sec)
                    #:addralign (elf-section-addralign sec)
                    #:entsize (elf-section-entsize sec)))

(define *page-size* 4096)

;; Adds object symbols to global table, relocating them from object
;; address space to memory address space.
(define (add-symbols symbols offset symtab)
  (fold1 (lambda (symbol symtab)
           (let ((name (linker-symbol-name symbol))
                 (addr (linker-symbol-address symbol)))
             (when (vhash-assq name symtab)
               (error "duplicate symbol" name))
             (vhash-consq name (make-linker-symbol name (+ addr offset)) symtab)))
         symbols
         symtab))

(define (alloc-segment type flags objects fileaddr memaddr symtab alignment)
  (let* ((loadable? (not (zero? flags)))
         (alignment (fold1 (lambda (o alignment)
                             (lcm (elf-section-addralign
                                   (linker-object-section o))
                                  alignment))
                           objects
                           alignment))
         (fileaddr (align fileaddr alignment))
         (memaddr (align memaddr alignment)))
    (receive (objects fileend memend symtab)
        (fold4 (lambda (o out fileaddr memaddr symtab)
                 (let* ((section (linker-object-section o))
                        (fileaddr
                         (if (= (elf-section-type section) SHT_NOBITS)
                             fileaddr
                             (align fileaddr (elf-section-addralign section))))
                        (memaddr
                         (align memaddr (elf-section-addralign section))))
                   (values
                    (cons (make-linker-object
                           (relocate-section-header section fileaddr
                                                    memaddr)
                           (linker-object-bv o)
                           (linker-object-relocs o)
                           (linker-object-symbols o))
                          out)
                    (if (= (elf-section-type section) SHT_NOBITS)
                        fileaddr
                        (+ fileaddr (elf-section-size section)))
                    (+ memaddr (elf-section-size section))
                    (add-symbols (linker-object-symbols o) memaddr symtab))))
               objects '() fileaddr memaddr symtab)
      (values
       (make-elf-segment #:type type #:offset fileaddr
                         #:vaddr (if loadable? memaddr 0)
                         #:filesz (- fileend fileaddr)
                         #:memsz (if loadable? (- memend memaddr) 0)
                         #:flags flags #:align alignment)
       (reverse objects)
       symtab))))

(define (process-reloc reloc bv file-offset mem-offset symtab endianness)
  (let ((ent (vhash-assq (linker-reloc-symbol reloc) symtab)))
    (unless ent
      (error "Undefined symbol" (linker-reloc-symbol reloc)))
    (let* ((file-loc (+ (linker-reloc-loc reloc) file-offset))
           (mem-loc (+ (linker-reloc-loc reloc) mem-offset))
           (addr (linker-symbol-address (cdr ent))))
      (case (linker-reloc-type reloc)
        ((rel32/4)
         (let ((diff (- addr mem-loc)))
           (unless (zero? (modulo diff 4))
             (error "Bad offset" reloc symbol mem-offset))
           (bytevector-s32-set! bv file-loc
                                (+ (/ diff 4) (linker-reloc-addend reloc))
                                endianness)))
        ((abs32/1)
         (bytevector-u32-set! bv file-loc addr endianness))
        ((abs64/1)
         (bytevector-u64-set! bv file-loc addr endianness))
        (else
         (error "bad reloc type" reloc))))))

(define (write-linker-object bv o symtab endianness)
  (let* ((section (linker-object-section o))
         (offset (elf-section-offset section))
         (addr (elf-section-addr section))
         (len (elf-section-size section))
         (bytes (linker-object-bv o))
         (relocs (linker-object-relocs o)))
    (if (not (= (elf-section-type section) SHT_NOBITS))
        (begin
          (if (not (= (elf-section-size section) (bytevector-length bytes)))
              (error "unexpected length" section bytes))
          (bytevector-copy! bytes 0 bv offset len)
          (for-each (lambda (reloc)
                      (process-reloc reloc bv offset addr symtab endianness))
                    relocs)))))

(define (compute-sections-by-name seglists)
  (let lp ((in (apply append (map cdr seglists)))
           (n 1) (out '()) (shstrtab #f))
    (if (null? in)
        (fold1 (lambda (x tail)
                 (cond
                  ((false-if-exception
                    (string-table-ref shstrtab (car x)))
                   => (lambda (str) (acons str (cdr x) tail)))
                  (else tail)))
               out '())
        (let* ((section (linker-object-section (car in)))
               (bv (linker-object-bv (car in)))
               (name (elf-section-name section)))
          (lp (cdr in) (1+ n) (acons name n out)
              (or shstrtab
                  (and (= (elf-section-type section) SHT_STRTAB)
                       (equal? (false-if-exception
                                (string-table-ref bv name))
                               ".shstrtab")
                       bv)))))))

;; Given a list of section-header/bytevector pairs, collate the sections
;; into segments, allocate the segments, allocate the ELF bytevector,
;; and write the segments into the bytevector, relocating as we go.
;;
(define* (link-elf objects #:key
                   (page-aligned? #t)
                   (endianness (target-endianness))
                   (word-size (target-word-size)))
  (let* ((seglists (collate-objects-into-segments objects))
         (sections-by-name (compute-sections-by-name seglists))
         (nsegments (length seglists))
         (nsections (1+ (length objects))) ;; 1+ for the first reserved entry.
         (program-headers-offset (elf-header-len word-size))
         (fileaddr (+ program-headers-offset
                      (* nsegments (elf-program-header-len word-size))))
         (memaddr 0))
    (receive (out fileend memend symtab _)
        (fold5
         (lambda (x out fileaddr memaddr symtab prev-flags)
           (let ((type (caar x))
                 (flags (cdar x))
                 (objects (cdr x)))
             (receive (segment objects symtab)
                 (alloc-segment type flags objects fileaddr memaddr symtab
                                (if (and page-aligned?
                                         (not (= flags prev-flags)))
                                    *page-size*
                                    8))
               (values
                (cons (cons segment objects) out)
                (+ (elf-segment-offset segment) (elf-segment-filesz segment))
                (if (zero? (elf-segment-memsz segment))
                    memaddr
                    (+ (elf-segment-vaddr segment)
                       (elf-segment-memsz segment)))
                symtab
                flags))))
         seglists '() fileaddr memaddr vlist-null 0)
      (let* ((out (reverse! out))
             (section-table-offset (+ (align fileend word-size)))
             (fileend (+ section-table-offset
                         (* nsections (elf-section-header-len word-size))))
             (bv (make-bytevector fileend 0)))
        (write-elf-header bv #:byte-order endianness #:word-size word-size
                          #:phoff program-headers-offset #:phnum nsegments
                          #:shoff section-table-offset #:shnum nsections
                          #:shstrndx (or (assoc-ref sections-by-name ".shstrtab")
                                         SHN_UNDEF))
        (write-elf-section-header bv section-table-offset
                                  endianness word-size
                                  (make-elf-section #:type SHT_NULL #:flags 0
                                                    #:addralign 0))
        (fold2 (lambda (x phidx shidx)
                 (write-elf-program-header
                  bv (+ program-headers-offset
                        (* (elf-program-header-len word-size) phidx))
                  endianness word-size (car x))
                 (values
                  (1+ phidx)
                  (fold1 (lambda (o shidx)
                           (write-linker-object bv o symtab endianness)
                           (write-elf-section-header
                            bv (+ section-table-offset
                                  (* (elf-section-header-len word-size) shidx))
                            endianness word-size (linker-object-section o))
                           (1+ shidx))
                         (cdr x) shidx)))
               out 0 1)
        bv))))
