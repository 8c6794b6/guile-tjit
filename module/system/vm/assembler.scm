;;; Guile RTL assembler

;;; Copyright (C) 2001, 2009, 2010, 2012, 2013 Free Software Foundation, Inc.
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
;;; This module implements an assembler that creates an ELF image from
;;; RTL assembly and macro-assembly.  The input can be given in
;;; s-expression form, like ((OP ARG ...) ...).  Internally there is a
;;; procedural interface, the emit-OP procedures, but that is not
;;; currently exported.
;;;
;;; "Primitive instructions" correspond to RTL VM operations.
;;; Assemblers for primitive instructions are generated programmatically
;;; from (rtl-instruction-list), which itself is derived from the VM
;;; sources.  There are also "macro-instructions" like "label" or
;;; "load-constant" that expand to 0 or more primitive instructions.
;;;
;;; The assembler also handles some higher-level tasks, like creating
;;; the symbol table, other metadata sections, creating a constant table
;;; for the whole compilation unit, and writing the dynamic section of
;;; the ELF file along with the appropriate initialization routines.
;;;
;;; Most compilers will want to use the trio of make-assembler,
;;; emit-text, and link-assembly.  That will result in the creation of
;;; an ELF image as a bytevector, which can then be loaded using
;;; load-thunk-from-memory, or written to disk as a .go file.
;;;
;;; Code:

(define-module (system vm assembler)
  #:use-module (system base target)
  #:use-module (system vm instruction)
  #:use-module (system vm elf)
  #:use-module (system vm linker)
  #:use-module (system vm objcode)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:export (make-assembler
            emit-text
            link-assembly
            assemble-program))




;;; RTL code consists of 32-bit units, often subdivided in some way.
;;; These helpers create one 32-bit unit from multiple components.

(define-syntax-rule (pack-u8-u24 x y)
  (logior x (ash y 8)))

(define-syntax-rule (pack-u8-s24 x y)
  (logior x (ash (cond
                  ((< 0 (- y) #x800000)
                   (+ y #x1000000))
                  ((<= 0 y #xffffff)
                   y)
                  (else (error "out of range" y)))
                 8)))

(define-syntax-rule (pack-u1-u7-u24 x y z)
  (logior x (ash y 1) (ash z 8)))

(define-syntax-rule (pack-u8-u12-u12 x y z)
  (logior x (ash y 8) (ash z 20)))

(define-syntax-rule (pack-u8-u8-u16 x y z)
  (logior x (ash y 8) (ash z 16)))

(define-syntax-rule (pack-u8-u8-u8-u8 x y z w)
  (logior x (ash y 8) (ash z 16) (ash w 24)))

;;; Helpers to read and write 32-bit units in a buffer.

(define-syntax-rule (u32-ref buf n)
  (bytevector-u32-native-ref buf (* n 4)))

(define-syntax-rule (u32-set! buf n val)
  (bytevector-u32-native-set! buf (* n 4) val))

(define-syntax-rule (s32-ref buf n)
  (bytevector-s32-native-ref buf (* n 4)))

(define-syntax-rule (s32-set! buf n val)
  (bytevector-s32-native-set! buf (* n 4) val))




;;; A <meta> entry collects metadata for one procedure.  Procedures are
;;; written as contiguous ranges of RTL code.
;;;
(define-record-type <meta>
  (make-meta name low-pc high-pc)
  meta?
  (name meta-name)
  (low-pc meta-low-pc)
  (high-pc meta-high-pc set-meta-high-pc!))

(define-syntax *block-size* (identifier-syntax 32))

;;; An assembler collects all of the words emitted during assembly, and
;;; also maintains ancillary information such as the constant table, a
;;; relocation list, and so on.
;;;
;;; RTL code consists of 32-bit units.  We emit RTL code using native
;;; endianness.  If we're targeting a foreign endianness, we byte-swap
;;; the bytevector as a whole instead of conditionalizing each access.
;;;
(define-record-type <asm>
  (make-asm cur idx start prev written
            labels relocs
            word-size endianness
            constants inits
            shstrtab next-section-number
            meta)
  asm?

  ;; We write RTL code into what is logically a growable vector,
  ;; implemented as a list of blocks.  asm-cur is the current block, and
  ;; asm-idx is the current index into that block, in 32-bit units.
  ;;
  (cur asm-cur set-asm-cur!)
  (idx asm-idx set-asm-idx!)

  ;; asm-start is an absolute position, indicating the offset of the
  ;; beginning of an instruction (in u32 units).  It is updated after
  ;; writing all the words for one primitive instruction.  It models the
  ;; position of the instruction pointer during execution, given that
  ;; the RTL VM updates the IP only at the end of executing the
  ;; instruction, and is thus useful for computing offsets between two
  ;; points in a program.
  ;;
  (start asm-start set-asm-start!)

  ;; The list of previously written blocks.
  ;;
  (prev asm-prev set-asm-prev!)

  ;; The number of u32 words written in asm-prev, which is the same as
  ;; the offset of the current block.
  ;;
  (written asm-written set-asm-written!)

  ;; An alist of symbol -> position pairs, indicating the labels defined
  ;; in this compilation unit.
  ;;
  (labels asm-labels set-asm-labels!)

  ;; A list of relocations needed by the program text.  We use an
  ;; internal representation for relocations, and handle textualn
  ;; relative relocations in the assembler.  Other kinds of relocations
  ;; are later reified as linker relocations and resolved by the linker.
  ;;
  (relocs asm-relocs set-asm-relocs!)

  ;; Target information.
  ;;
  (word-size asm-word-size)
  (endianness asm-endianness)

  ;; The constant table, as a vhash of object -> label.  All constants
  ;; get de-duplicated and written into separate sections -- either the
  ;; .rodata section, for read-only data, or .data, for constants that
  ;; need initialization at load-time (like symbols).  Constants can
  ;; depend on other constants (e.g. a symbol depending on a stringbuf),
  ;; so order in this table is important.
  ;;
  (constants asm-constants set-asm-constants!)

  ;; A list of RTL instructions needed to initialize the constants.
  ;; Will run in a thunk with 2 local variables.
  ;;
  (inits asm-inits set-asm-inits!)

  ;; The shstrtab, for section names.
  ;;
  (shstrtab asm-shstrtab set-asm-shstrtab!)

  ;; The section number for the next section to be written.
  ;;
  (next-section-number asm-next-section-number set-asm-next-section-number!)

  ;; A list of <meta>, corresponding to procedure metadata.
  ;;
  (meta asm-meta set-asm-meta!))

(define-inlinable (fresh-block)
  (make-u32vector *block-size*))

(define* (make-assembler #:key (word-size (target-word-size))
                         (endianness (target-endianness)))
  "Create an assembler for a given target @var{word-size} and
@var{endianness}, falling back to appropriate values for the configured
target."
  (make-asm (fresh-block) 0 0 '() 0
            '() '()
            word-size endianness
            vlist-null '()
            (make-string-table) 1
            '()))

(define (intern-section-name! asm string)
  "Add a string to the section name table (shstrtab)."
  (string-table-intern! (asm-shstrtab asm) string))

(define-inlinable (asm-pos asm)
  "The offset of the next word to be written into the code buffer, in
32-bit units."
  (+ (asm-idx asm) (asm-written asm)))

(define (allocate-new-block asm)
  "Close off the current block, and arrange for the next word to be
written to a fresh block."
  (let ((new (fresh-block)))
    (set-asm-prev! asm (cons (asm-cur asm) (asm-prev asm)))
    (set-asm-written! asm (asm-pos asm))
    (set-asm-cur! asm new)
    (set-asm-idx! asm 0)))

(define-inlinable (emit asm u32)
  "Emit one 32-bit word into the instruction stream.  Assumes that there
is space for the word, and ensures that there is space for the next
word."
  (u32-set! (asm-cur asm) (asm-idx asm) u32)
  (set-asm-idx! asm (1+ (asm-idx asm)))
  (if (= (asm-idx asm) *block-size*)
      (allocate-new-block asm)))

(define-inlinable (make-reloc type label base word)
  "Make an internal relocation of type @var{type} referencing symbol
@var{label}, @var{word} words after position @var{start}.  @var{type}
may be x8-s24, indicating a 24-bit relative label reference that can be
fixed up by the assembler, or s32, indicating a 32-bit relative
reference that needs to be fixed up by the linker."
  (list type label base word))

(define-inlinable (reset-asm-start! asm)
  "Reset the asm-start after writing the words for one instruction."
  (set-asm-start! asm (asm-pos asm)))

(define (emit-exported-label asm label)
  "Define a linker symbol associating @var{label} with the current
asm-start."
  (set-asm-labels! asm (acons label (asm-start asm) (asm-labels asm))))

(define (record-label-reference asm label)
  "Record an x8-s24 local label reference.  This value will get patched
up later by the assembler."
  (let* ((start (asm-start asm))
         (pos (asm-pos asm))
         (reloc (make-reloc 'x8-s24 label start (- pos start))))
    (set-asm-relocs! asm (cons reloc (asm-relocs asm)))))

(define* (record-far-label-reference asm label #:optional (offset 0))
  "Record an s32 far label reference.  This value will get patched up
later by the linker."
  (let* ((start (- (asm-start asm) offset))
         (pos (asm-pos asm))
         (reloc (make-reloc 's32 label start (- pos start))))
    (set-asm-relocs! asm (cons reloc (asm-relocs asm)))))




;;;
;;; Primitive assemblers are defined by expanding `assembler' for each
;;; opcode in `(rtl-instruction-list)'.
;;;

(eval-when (expand compile load eval)
  (define (id-append ctx a b)
    (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b)))))

(define-syntax assembler
  (lambda (x)
    (define-syntax op-case
      (lambda (x)
        (syntax-case x ()
          ((_ asm name ((type arg ...) code ...) clause ...)
           #`(if (eq? name 'type)
                 (with-syntax (((arg ...) (generate-temporaries #'(arg ...))))
                   #'((arg ...)
                      code ...))
                 (op-case asm name clause ...)))
          ((_ asm name)
           #'(error "unmatched name" name)))))

    (define (pack-first-word asm opcode type)
      (with-syntax ((opcode opcode))
        (op-case
         asm type
         ((U8_X24)
          (emit asm opcode))
         ((U8_U24 arg)
          (emit asm (pack-u8-u24 opcode arg)))
         ((U8_L24 label)
          (record-label-reference asm label)
          (emit asm opcode))
         ((U8_R24 rest)
          (emit asm (pack-u8-u24 opcode (list rest)))
          (for-each (lambda (x) (emit asm x)) rest))
         ((U8_U8_I16 a imm)
          (emit asm (pack-u8-u8-u16 opcode a (object-address imm))))
         ((U8_U12_U12 a b)
          (emit asm (pack-u8-u12-u12 opcode a b)))
         ((U8_U8_U8_U8 a b c)
          (emit asm (pack-u8-u8-u8-u8 opcode a b c))))))

    (define (pack-tail-word asm type)
      (op-case
       asm type
       ((U8_U24 a b)
        (emit asm (pack-u8-u24 a b)))
       ((U8_L24 a label)
        (record-label-reference asm label)
        (emit asm a))
       ((U8_R24 rest)
        (emit asm (pack-u8-u24 a (length rest)))
        (for-each (lambda (x) (emit asm x)) rest))
       ((U8_U8_I16 a b imm)
        (emit asm (pack-u8-u8-u16 a b (object-address imm))))
       ((U8_U12_U12 a b)
        (emit asm (pack-u8-u12-u12 a b c)))
       ((U8_U8_U8_U8 a b c d)
        (emit asm (pack-u8-u8-u8-u8 a b c d)))
       ((U32 a)
        (emit asm a))
       ((I32 imm)
        (let ((val (object-address imm)))
          (unless (zero? (ash val -32))
            (error "FIXME: enable truncation of negative fixnums when cross-compiling"))
          (emit asm val)))
       ((A32 imm)
        (unless (= (asm-word-size asm) 8)
          (error "make-long-immediate unavailable for this target"))
        (emit asm (ash (object-address imm) -32))
        (emit asm (logand (object-address imm) (1- (ash 1 32)))))
       ((B32))
       ((N32 label)
        (record-far-label-reference asm label)
        (emit asm 0))
       ((S32 label)
        (record-far-label-reference asm label)
        (emit asm 0))
       ((L32 label)
        (record-far-label-reference asm label)
        (emit asm 0))
       ((LO32 label offset)
        (record-far-label-reference asm label
                                    (* offset (/ (asm-word-size asm) 4)))
        (emit asm 0))
       ((X8_U24 a)
        (emit asm (pack-u8-u24 0 a)))
       ((X8_U12_U12 a b)
        (emit asm (pack-u8-u12-u12 0 a b)))
       ((X8_R24 rest)
        (emit asm (pack-u8-u24 0 (length rest)))
        (for-each (lambda (x) (emit asm x)) rest))
       ((X8_L24 label)
        (record-label-reference asm label)
        (emit asm 0))
       ((B1_X7_L24 a label)
        (record-label-reference asm label)
        (emit asm (pack-u1-u7-u24 (if a 1 0) 0 0)))
       ((B1_U7_L24 a b label)
        (record-label-reference asm label)
        (emit asm (pack-u1-u7-u24 (if a 1 0) b 0)))))

    (syntax-case x ()
      ((_ name opcode word0 word* ...)
       (with-syntax ((((formal0 ...)
                       code0 ...)
                      (pack-first-word #'asm
                                       (syntax->datum #'opcode)
                                       (syntax->datum #'word0)))
                     ((((formal* ...)
                        code* ...) ...)
                      (map (lambda (word) (pack-tail-word #'asm word))
                           (syntax->datum #'(word* ...)))))
         #'(lambda (asm formal0 ... formal* ... ...)
             (unless (asm? asm) (error "not an asm"))
             code0 ...
             code* ... ...
             (reset-asm-start! asm)))))))

(define assemblers (make-hash-table))

(define-syntax define-assembler
  (lambda (x)
    (syntax-case x ()
      ((_ name opcode arg ...)
       (with-syntax ((emit (id-append #'name #'emit- #'name)))
         #'(define emit
             (let ((emit (assembler name opcode arg ...)))
               (hashq-set! assemblers 'name emit)
               emit)))))))

(define-syntax visit-opcodes
  (lambda (x)
    (syntax-case x ()
      ((visit-opcodes macro arg ...)
       (with-syntax (((inst ...)
                      (map (lambda (x) (datum->syntax #'macro x))
                           (rtl-instruction-list))))
         #'(begin
             (macro arg ... . inst)
             ...))))))

(visit-opcodes define-assembler)

(define (emit-text asm instructions)
  "Assemble @var{instructions} using the assembler @var{asm}.
@var{instructions} is a sequence of RTL instructions, expressed as a
list of lists.  This procedure can be called many times before calling
@code{link-assembly}."
  (for-each (lambda (inst)
              (apply (or (hashq-ref assemblers (car inst))
                         (error 'bad-instruction inst))
                     asm
                     (cdr inst)))
            instructions))



;;;
;;; The constant table records a topologically sorted set of literal
;;; constants used by a program.  For example, a pair uses its car and
;;; cdr, a string uses its stringbuf, etc.
;;;
;;; Some things we want to add to the constant table are not actually
;;; Scheme objects: for example, stringbufs, cache cells for toplevel
;;; references, or cache cells for non-closure procedures.  For these we
;;; define special record types and add instances of those record types
;;; to the table.
;;;

(define-inlinable (immediate? x)
  "Return @code{#t} if @var{x} is immediate, and @code{#f} otherwise."
  (not (zero? (logand (object-address x) 6))))

(define-record-type <stringbuf>
  (make-stringbuf string)
  stringbuf?
  (string stringbuf-string))

(define-record-type <static-procedure>
  (make-static-procedure code)
  static-procedure?
  (code static-procedure-code))

(define-record-type <cache-cell>
  (make-cache-cell scope key)
  cache-cell?
  (scope cache-cell-scope)
  (key cache-cell-key))

(define (statically-allocatable? x)
  "Return @code{#t} if a non-immediate constant can be allocated
statically, and @code{#f} if it would need some kind of runtime
allocation."
  (or (pair? x) (vector? x) (string? x) (stringbuf? x) (static-procedure? x)))

(define (intern-constant asm obj)
  "Add an object to the constant table, and return a label that can be
used to reference it.  If the object is already present in the constant
table, its existing label is used directly."
  (define (recur obj)
    (intern-constant asm obj))
  (define (field dst n obj)
    (let ((src (recur obj)))
      (if src
          (list (if (statically-allocatable? obj)
                    `(make-non-immediate 0 ,src)
                    `(static-ref 0 ,src))
                `(static-set! 0 ,dst ,n))
          '())))
  (define (intern obj label)
    (cond
     ((pair? obj)
      (append (field label 0 (car obj))
              (field label 1 (cdr obj))))
     ((vector? obj)
      (let lp ((i 0) (inits '()))
        (if (< i (vector-length obj))
            (lp (1+ i)
                (append-reverse (field label (1+ i) (vector-ref obj i))
                                inits))
            (reverse inits))))
     ((stringbuf? obj) '())
     ((static-procedure? obj)
      `((make-non-immediate 0 ,label)
        (link-procedure! 0 ,(static-procedure-code obj))))
     ((cache-cell? obj) '())
     ((symbol? obj)
      `((make-non-immediate 0 ,(recur (symbol->string obj)))
        (string->symbol 0 0)
        (static-set! 0 ,label 0)))
     ((string? obj)
      `((make-non-immediate 0 ,(recur (make-stringbuf obj)))
        (static-set! 0 ,label 1)))
     ((keyword? obj)
      `((static-ref 0 ,(recur (keyword->symbol obj)))
        (symbol->keyword 0 0)
        (static-set! 0 ,label 0)))
     ((number? obj)
      `((make-non-immediate 0 ,(recur (number->string obj)))
        (string->number 0 0)
        (static-set! 0 ,label 0)))
     (else
      (error "don't know how to intern" obj))))
  (cond
   ((immediate? obj) #f)
   ((vhash-assoc obj (asm-constants asm)) => cdr)
   (else
    ;; Note that calling intern may mutate asm-constants and
    ;; asm-constant-inits.
    (let* ((label (gensym "constant"))
           (inits (intern obj label)))
      (set-asm-constants! asm (vhash-cons obj label (asm-constants asm)))
      (set-asm-inits! asm (append-reverse inits (asm-inits asm)))
      label))))

(define (intern-non-immediate asm obj)
  "Intern a non-immediate into the constant table, and return its
label."
  (when (immediate? obj)
    (error "expected a non-immediate" obj))
  (intern-constant asm obj))

(define (intern-cache-cell asm scope key)
  "Intern a cache cell into the constant table, and return its label.
If there is already a cache cell with the given scope and key, it is
returned instead."
  (intern-constant asm (make-cache-cell scope key)))

;; Return the label of the cell that holds the module for a scope.
(define (intern-module-cache-cell asm scope)
  "Intern a cache cell for a module, and return its label."
  (intern-cache-cell asm scope #t))




;;;
;;; Macro assemblers bridge the gap between primitive instructions and
;;; some higher-level operations.
;;;

(define-syntax define-macro-assembler
  (lambda (x)
    (syntax-case x ()
      ((_ (name arg ...) body body* ...)
       (with-syntax ((emit (id-append #'name #'emit- #'name)))
         #'(define emit
             (let ((emit (lambda (arg ...) body body* ...)))
               (hashq-set! assemblers 'name emit)
               emit)))))))

(define-macro-assembler (load-constant asm dst obj)
  (cond
   ((immediate? obj)
    (let ((bits (object-address obj)))
      (cond
       ((and (< dst 256) (zero? (ash bits -16)))
        (emit-make-short-immediate asm dst obj))
       ((zero? (ash bits -32))
        (emit-make-long-immediate asm dst obj))
       (else
        (emit-make-long-long-immediate asm dst obj)))))
   ((statically-allocatable? obj)
    (emit-make-non-immediate asm dst (intern-non-immediate asm obj)))
   (else
    (emit-static-ref asm dst (intern-non-immediate asm obj)))))

(define-macro-assembler (load-static-procedure asm dst label)
  (let ((loc (intern-constant asm (make-static-procedure label))))
    (emit-make-non-immediate asm dst loc)))

(define-macro-assembler (begin-program asm label)
  (emit-label asm label)
  (let ((meta (make-meta label (asm-start asm) #f)))
    (set-asm-meta! asm (cons meta (asm-meta asm)))))

(define-macro-assembler (end-program asm)
  (set-meta-high-pc! (car (asm-meta asm)) (asm-start asm)))

(define-macro-assembler (label asm sym)
  (set-asm-labels! asm (acons sym (asm-start asm) (asm-labels asm))))

(define-macro-assembler (cache-current-module! asm tmp scope)
  (let ((mod-label (intern-module-cache-cell asm scope)))
    (emit-current-module asm tmp)
    (emit-static-set! asm tmp mod-label 0)))

(define-macro-assembler (cached-toplevel-ref asm dst scope sym)
  (let ((sym-label (intern-non-immediate asm sym))
        (mod-label (intern-module-cache-cell asm scope))
        (cell-label (intern-cache-cell asm scope sym)))
    (emit-toplevel-ref asm dst cell-label mod-label sym-label)))

(define-macro-assembler (cached-toplevel-set! asm src scope sym)
  (let ((sym-label (intern-non-immediate asm sym))
        (mod-label (intern-module-cache-cell asm scope))
        (cell-label (intern-cache-cell asm scope sym)))
    (emit-toplevel-set! asm src cell-label mod-label sym-label)))

(define-macro-assembler (cached-module-ref asm dst module-name public? sym)
  (let* ((sym-label (intern-non-immediate asm sym))
         (key (cons public? module-name))
         (mod-name-label (intern-constant asm key))
         (cell-label (intern-cache-cell asm key sym)))
    (emit-module-ref asm dst cell-label mod-name-label sym-label)))

(define-macro-assembler (cached-module-set! asm src module-name public? sym)
  (let* ((sym-label (intern-non-immediate asm sym))
         (key (cons public? module-name))
         (mod-name-label (intern-non-immediate asm key))
         (cell-label (intern-cache-cell asm key sym)))
    (emit-module-set! asm src cell-label mod-name-label sym-label)))




;;;
;;; Helper for linking objects.
;;;

(define (make-object asm name bv relocs labels . kwargs)
  "Make a linker object.  This helper handles interning the name in the
shstrtab, assigning the size, allocating a fresh index, and defining a
corresponding linker symbol for the start of the section."
  (let ((name-idx (intern-section-name! asm (symbol->string name)))
        (index (asm-next-section-number asm)))
    (set-asm-next-section-number! asm (1+ index))
    (make-linker-object (apply make-elf-section
                               #:index index
                               #:name name-idx
                               #:size (bytevector-length bv)
                               kwargs)
                        bv relocs
                        (cons (make-linker-symbol name 0) labels))))




;;;
;;; Linking the constant table.  This code is somewhat intertwingled
;;; with the intern-constant code above, as that procedure also
;;; residualizes instructions to initialize constants at load time.
;;;

(define (write-immediate asm buf pos x)
  (let ((val (object-address x))
        (endianness (asm-endianness asm)))
    (case (asm-word-size asm)
      ((4) (bytevector-u32-set! buf pos val endianness))
      ((8) (bytevector-u64-set! buf pos val endianness))
      (else (error "bad word size" asm)))))

(define (emit-init-constants asm)
  "If there is writable data that needs initialization at runtime, emit
a procedure to do that and return its label.  Otherwise return
@code{#f}."
  (let ((inits (asm-inits asm)))
    (and (not (null? inits))
         (let ((label (gensym "init-constants")))
           (emit-text asm
                      `((begin-program ,label)
                        (assert-nargs-ee/locals 0 1)
                        ,@(reverse inits)
                        (load-constant 0 ,*unspecified*)
                        (return 0)
                        (end-program)))
           label))))

(define (link-data asm data name)
  "Link the static data for a program into the @var{name} section (which
should be .data or .rodata), and return the resulting linker object.
@var{data} should be a vhash mapping objects to labels."
  (define (align address alignment)
    (+ address
       (modulo (- alignment (modulo address alignment)) alignment)))

  (define tc7-vector 13)
  (define tc7-narrow-stringbuf 39)
  (define tc7-wide-stringbuf (+ 39 #x400))
  (define tc7-ro-string (+ 21 #x200))
  (define tc7-rtl-program 69)

  (let ((word-size (asm-word-size asm))
        (endianness (asm-endianness asm)))
    (define (byte-length x)
      (cond
       ((stringbuf? x)
        (let ((x (stringbuf-string x)))
          (+ (* 2 word-size)
             (case (string-bytes-per-char x)
               ((1) (1+ (string-length x)))
               ((4) (* (1+ (string-length x)) 4))
               (else (error "bad string bytes per char" x))))))
       ((static-procedure? x)
        (* 2 word-size))
       ((string? x)
        (* 4 word-size))
       ((pair? x)
        (* 2 word-size))
       ((vector? x)
        (* (1+ (vector-length x)) word-size))
       (else
        word-size)))

    (define (write-constant-reference buf pos x)
      ;; The asm-inits will fix up any reference to a non-immediate.
      (write-immediate asm buf pos (if (immediate? x) x #f)))

    (define (write buf pos obj)
      (cond
       ((stringbuf? obj)
        (let* ((x (stringbuf-string obj))
               (len (string-length x))
               (tag (if (= (string-bytes-per-char x) 1)
                        tc7-narrow-stringbuf
                        tc7-wide-stringbuf)))
          (case word-size
            ((4)
             (bytevector-u32-set! buf pos tag endianness)
             (bytevector-u32-set! buf (+ pos 4) len endianness))
            ((8)
             (bytevector-u64-set! buf pos tag endianness)
             (bytevector-u64-set! buf (+ pos 8) len endianness))
            (else
             (error "bad word size" asm)))
          (let ((pos (+ pos (* word-size 2))))
            (case (string-bytes-per-char x)
              ((1)
               (let lp ((i 0))
                 (if (< i len)
                     (let ((u8 (char->integer (string-ref x i))))
                       (bytevector-u8-set! buf (+ pos i) u8)
                       (lp (1+ i)))
                     (bytevector-u8-set! buf (+ pos i) 0))))
              ((4)
               (let lp ((i 0))
                 (if (< i len)
                     (let ((u32 (char->integer (string-ref x i))))
                       (bytevector-u32-set! buf (+ pos (* i 4)) u32 endianness)
                       (lp (1+ i)))
                     (bytevector-u32-set! buf (+ pos (* i 4)) 0 endianness))))
              (else (error "bad string bytes per char" x))))))

       ((static-procedure? obj)
        (case word-size
          ((4)
           (bytevector-u32-set! buf pos tc7-rtl-program endianness)
           (bytevector-u32-set! buf (+ pos 4) 0 endianness))
          ((8)
           (bytevector-u64-set! buf pos tc7-rtl-program endianness)
           (bytevector-u64-set! buf (+ pos 8) 0 endianness))
          (else (error "bad word size"))))

       ((cache-cell? obj)
        (write-immediate asm buf pos #f))

       ((string? obj)
        (let ((tag (logior tc7-ro-string (ash (string-length obj) 8))))
          (case word-size
            ((4)
             (bytevector-u32-set! buf pos tc7-ro-string endianness)
             (write-immediate asm buf (+ pos 4) #f) ; stringbuf
             (bytevector-u32-set! buf (+ pos 8) 0 endianness)
             (bytevector-u32-set! buf (+ pos 12) (string-length obj) endianness))
            ((8)
             (bytevector-u64-set! buf pos tc7-ro-string endianness)
             (write-immediate asm buf (+ pos 8) #f) ; stringbuf
             (bytevector-u64-set! buf (+ pos 16) 0 endianness)
             (bytevector-u64-set! buf (+ pos 24) (string-length obj) endianness))
            (else (error "bad word size")))))

       ((pair? obj)
        (write-constant-reference buf pos (car obj))
        (write-constant-reference buf (+ pos word-size) (cdr obj)))

       ((vector? obj)
        (let* ((len (vector-length obj))
               (tag (logior tc7-vector (ash len 8))))
          (case word-size
            ((4) (bytevector-u32-set! buf pos tag endianness))
            ((8) (bytevector-u64-set! buf pos tag endianness))
            (else (error "bad word size")))
          (let lp ((i 0))
            (when (< i (vector-length obj))
              (let ((pos (+ pos word-size (* i word-size)))
                    (elt (vector-ref obj i)))
                (write-constant-reference buf pos elt)
                (lp (1+ i)))))))

       ((symbol? obj)
        (write-immediate asm buf pos #f))

       ((keyword? obj)
        (write-immediate asm buf pos #f))

       ((number? obj)
        (write-immediate asm buf pos #f))

       (else
        (error "unrecognized object" obj))))

    (cond
     ((vlist-null? data) #f)
     (else
      (let* ((byte-len (vhash-fold (lambda (k v len)
                                     (+ (byte-length k) (align len 8)))
                                   0 data))
             (buf (make-bytevector byte-len 0)))
        (let lp ((i 0) (pos 0) (labels '()))
          (if (< i (vlist-length data))
              (let* ((pair (vlist-ref data i))
                     (obj (car pair))
                     (obj-label (cdr pair)))
                (write buf pos obj)
                (lp (1+ i)
                    (align (+ (byte-length obj) pos) 8)
                    (cons (make-linker-symbol obj-label pos) labels)))
              (make-object asm name buf '() labels))))))))

(define (link-constants asm)
  "Link sections to hold constants needed by the program text emitted
using @var{asm}.

Returns three values: an object for the .rodata section, an object for
the .data section, and a label for an initialization procedure.  Any of
these may be @code{#f}."
  (define (shareable? x)
    (cond
     ((stringbuf? x) #t)
     ((pair? x)
      (and (immediate? (car x)) (immediate? (cdr x))))
     ((vector? x)
      (let lp ((i 0))
        (or (= i (vector-length x))
            (and (immediate? (vector-ref x i))
                 (lp (1+ i))))))
     (else #f)))
  (let* ((constants (asm-constants asm))
         (len (vlist-length constants)))
    (let lp ((i 0)
             (ro vlist-null)
             (rw vlist-null))
      (if (= i len)
          (values (link-data asm ro '.rodata)
                  (link-data asm rw '.data)
                  (emit-init-constants asm))
          (let ((pair (vlist-ref constants i)))
            (if (shareable? (car pair))
                (lp (1+ i) (vhash-consq (car pair) (cdr pair) ro) rw)
                (lp (1+ i) ro (vhash-consq (car pair) (cdr pair) rw))))))))



;;;
;;; Linking program text.
;;;

(define (process-relocs buf relocs labels)
  "Patch up internal x8-s24 relocations, and any s32 relocations that
reference symbols in the text section.  Return a list of linker
relocations for references to symbols defined outside the text section."
  (fold
   (lambda (reloc tail)
     (match reloc
       ((type label base word)
        (let ((abs (assq-ref labels label))
              (dst (+ base word)))
          (case type
            ((s32)
             (if abs
                 (let ((rel (- abs base)))
                   (s32-set! buf dst rel)
                   tail)
                 (cons (make-linker-reloc 'rel32/4 (* dst 4) word label)
                       tail)))
            ((x8-s24)
             (unless abs
               (error "unbound near relocation" reloc))
             (let ((rel (- abs base))
                   (u32 (u32-ref buf dst)))
               (u32-set! buf dst (pack-u8-s24 (logand u32 #xff) rel))
               tail))
            (else (error "bad relocation kind" reloc)))))))
   '()
   relocs))

(define (process-labels labels)
  "Define linker symbols for the label-offset pairs in @var{labels}.
The offsets are expected to be expressed in words."
  (map (lambda (pair)
         (make-linker-symbol (car pair) (* (cdr pair) 4)))
       labels))

(define (swap-bytes! buf)
  "Patch up the text buffer @var{buf}, swapping the endianness of each
32-bit unit."
  (unless (zero? (modulo (bytevector-length buf) 4))
    (error "unexpected length"))
  (let ((byte-len (bytevector-length buf)))
    (let lp ((pos 0))
      (unless (= pos byte-len)
        (bytevector-u32-set!
         buf pos
         (bytevector-u32-ref buf pos (endianness big))
         (endianness little))
        (lp (+ pos 4))))))

(define (link-text-object asm)
  "Link the .rtl-text section, swapping the endianness of the bytes if
needed."
  (let ((buf (make-u32vector (asm-pos asm))))
    (let lp ((pos 0) (prev (reverse (asm-prev asm))))
      (if (null? prev)
          (let ((byte-size (* (asm-idx asm) 4)))
            (bytevector-copy! (asm-cur asm) 0 buf pos byte-size)
            (unless (eq? (asm-endianness asm) (native-endianness))
              (swap-bytes! buf))
            (make-object asm '.rtl-text
                         buf
                         (process-relocs buf (asm-relocs asm)
                                         (asm-labels asm))
                         (process-labels (asm-labels asm))))
          (let ((len (* *block-size* 4)))
            (bytevector-copy! (car prev) 0 buf pos len)
            (lp (+ pos len) (cdr prev)))))))




;;;
;;; Linking other sections of the ELF file, like the dynamic segment,
;;; the symbol table, etc.
;;;

(define (link-dynamic-section asm text rw rw-init)
  "Link the dynamic section for an ELF image with RTL text, given the
writable data section @var{rw} needing fixup from the procedure with
label @var{rw-init}.  @var{rw-init} may be false.  If @var{rw} is true,
it will be added to the GC roots at runtime."
  (define-syntax-rule (emit-dynamic-section word-size %set-uword! reloc-type)
    (let* ((endianness (asm-endianness asm))
           (bv (make-bytevector (* word-size (if rw (if rw-init 12 10) 6)) 0))
           (set-uword!
            (lambda (i uword)
              (%set-uword! bv (* i word-size) uword endianness)))
           (relocs '())
           (set-label!
            (lambda (i label)
              (set! relocs (cons (make-linker-reloc 'reloc-type
                                                    (* i word-size) 0 label)
                                 relocs))
              (%set-uword! bv (* i word-size) 0 endianness))))
      (set-uword! 0 DT_GUILE_RTL_VERSION)
      (set-uword! 1 #x02020000)
      (set-uword! 2 DT_GUILE_ENTRY)
      (set-label! 3 '.rtl-text)
      (cond
       (rw
        ;; Add roots to GC.
        (set-uword! 4 DT_GUILE_GC_ROOT)
        (set-label! 5 '.data)
        (set-uword! 6 DT_GUILE_GC_ROOT_SZ)
        (set-uword! 7 (bytevector-length (linker-object-bv rw)))
        (cond
         (rw-init
          (set-uword! 8 DT_INIT)        ; constants
          (set-label! 9 rw-init)
          (set-uword! 10 DT_NULL)
          (set-uword! 11 0))
         (else
          (set-uword! 8 DT_NULL)
          (set-uword! 9 0))))
       (else
        (set-uword! 4 DT_NULL)
        (set-uword! 5 0)))
      (make-object asm '.dynamic bv relocs '()
                   #:type SHT_DYNAMIC #:flags SHF_ALLOC)))
  (case (asm-word-size asm)
    ((4) (emit-dynamic-section 4 bytevector-u32-set! abs32/1))
    ((8) (emit-dynamic-section 8 bytevector-u64-set! abs64/1))
    (else (error "bad word size" asm))))

(define (link-shstrtab asm)
  "Link the string table for the section headers."
  (intern-section-name! asm ".shstrtab")
  (make-object asm '.shstrtab
               (link-string-table! (asm-shstrtab asm))
               '() '()
               #:type SHT_STRTAB #:flags 0))

(define (link-symtab text-section asm)
  (let* ((endianness (asm-endianness asm))
         (word-size (asm-word-size asm))
         (size (elf-symbol-len word-size))
         (meta (reverse (asm-meta asm)))
         (n (length meta))
         (strtab (make-string-table))
         (bv (make-bytevector (* n size) 0)))
    (define (intern-string! name)
      (string-table-intern! strtab (symbol->string name)))
    (for-each
     (lambda (meta n)
       (let ((name (intern-string! (meta-name meta))))
         (write-elf-symbol bv (* n size) endianness word-size
                           (make-elf-symbol
                            #:name name
                            ;; Symbol value and size are measured in
                            ;; bytes, not u32s.
                            #:value (* 4 (meta-low-pc meta))
                            #:size (* 4 (- (meta-high-pc meta)
                                           (meta-low-pc meta)))
                            #:type STT_FUNC
                            #:visibility STV_HIDDEN
                            #:shndx (elf-section-index text-section)))))
     meta (iota n))
    (let ((strtab (make-object asm '.strtab
                               (link-string-table! strtab)
                               '() '()
                               #:type SHT_STRTAB #:flags 0)))
      (values (make-object asm '.symtab
                           bv
                           '() '()
                           #:type SHT_SYMTAB #:flags 0 #:entsize size
                           #:link (elf-section-index
                                   (linker-object-section strtab)))
              strtab))))

(define (link-objects asm)
  (let*-values (((ro rw rw-init) (link-constants asm))
                ;; Link text object after constants, so that the
                ;; constants initializer gets included.
                ((text) (link-text-object asm))
                ((dt) (link-dynamic-section asm text rw rw-init))
                ((symtab strtab) (link-symtab (linker-object-section text) asm))
                ;; This needs to be linked last, because linking other
                ;; sections adds entries to the string table.
                ((shstrtab) (link-shstrtab asm)))
    (filter identity (list text ro rw dt symtab strtab shstrtab))))




;;;
;;; High-level public interfaces.
;;;

(define* (link-assembly asm #:key (page-aligned? #t))
  "Produce an ELF image from the code and data emitted into @var{asm}.
The result is a bytevector, by default linked so that read-only and
writable data are on separate pages.  Pass @code{#:page-aligned? #f} to
disable this behavior."
  (link-elf (link-objects asm) #:page-aligned? page-aligned?))

(define (assemble-program instructions)
  "Take the sequence of instructions @var{instructions}, assemble them
into RTL code, link an image, and load that image from memory.  Returns
a procedure."
  (let ((asm (make-assembler)))
    (emit-text asm instructions)
    (load-thunk-from-memory (link-assembly asm #:page-aligned? #f))))
