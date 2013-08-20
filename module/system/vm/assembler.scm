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

(define-syntax pack-flags
  (syntax-rules ()
    ;; Add clauses as needed.
    ((pack-flags f1 f2) (logior (if f1 (ash 1 0) 0)
                                (if f2 (ash 2 0) 0)))))

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
(define-syntax-rule (assert-match arg pattern kind)
  (let ((x arg))
    (unless (match x (pattern #t) (_ #f))
      (error (string-append "expected " kind) x))))

(define-record-type <meta>
  (%make-meta label properties low-pc high-pc arities)
  meta?
  (label meta-label)
  (properties meta-properties set-meta-properties!)
  (low-pc meta-low-pc)
  (high-pc meta-high-pc set-meta-high-pc!)
  (arities meta-arities set-meta-arities!))

(define (make-meta label properties low-pc)
  (assert-match label (? symbol?) "symbol")
  (assert-match properties (((? symbol?) . _) ...) "alist with symbolic keys")
  (%make-meta label properties low-pc #f '()))

(define (meta-name meta)
  (assq-ref (meta-properties meta) 'name))

;; Metadata for one <lambda-case>.
(define-record-type <arity>
  (make-arity req opt rest kw-indices allow-other-keys?
              low-pc high-pc)
  arity?
  (req arity-req)
  (opt arity-opt)
  (rest arity-rest)
  (kw-indices arity-kw-indices)
  (allow-other-keys? arity-allow-other-keys?)
  (low-pc arity-low-pc)
  (high-pc arity-high-pc set-arity-high-pc!))

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
       ((X8_L24 label)
        (record-label-reference asm label)
        (emit asm 0))
       ((B1_X7_L24 a label)
        (record-label-reference asm label)
        (emit asm (pack-u1-u7-u24 (if a 1 0) 0 0)))
       ((B1_U7_L24 a b label)
        (record-label-reference asm label)
        (emit asm (pack-u1-u7-u24 (if a 1 0) b 0)))
       ((B1_X31 a)
        (emit asm (pack-u1-u7-u24 (if a 1 0) 0 0)))
       ((B1_X7_U24 a b)
        (emit asm (pack-u1-u7-u24 (if a 1 0) 0 b)))))

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
      ((_ name opcode kind arg ...)
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
                    `(make-non-immediate 1 ,src)
                    `(static-ref 1 ,src))
                `(static-set! 1 ,dst ,n))
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
      `((make-non-immediate 1 ,label)
        (link-procedure! 1 ,(static-procedure-code obj))))
     ((cache-cell? obj) '())
     ((symbol? obj)
      `((make-non-immediate 1 ,(recur (symbol->string obj)))
        (string->symbol 1 1)
        (static-set! 1 ,label 0)))
     ((string? obj)
      `((make-non-immediate 1 ,(recur (make-stringbuf obj)))
        (static-set! 1 ,label 1)))
     ((keyword? obj)
      `((static-ref 1 ,(recur (keyword->symbol obj)))
        (symbol->keyword 1 1)
        (static-set! 1 ,label 0)))
     ((number? obj)
      `((make-non-immediate 1 ,(recur (number->string obj)))
        (string->number 1 1)
        (static-set! 1 ,label 0)))
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

(define-macro-assembler (begin-program asm label properties)
  (emit-label asm label)
  (let ((meta (make-meta label properties (asm-start asm))))
    (set-asm-meta! asm (cons meta (asm-meta asm)))))

(define-macro-assembler (end-program asm)
  (let ((meta (car (asm-meta asm))))
    (set-meta-high-pc! meta (asm-start asm))
    (set-meta-arities! meta (reverse (meta-arities meta)))))

(define-macro-assembler (begin-standard-arity asm req nlocals alternate)
  (emit-begin-opt-arity asm req '() #f nlocals alternate))

(define-macro-assembler (begin-opt-arity asm req opt rest nlocals alternate)
  (emit-begin-kw-arity asm req opt rest '() #f nlocals alternate))

(define-macro-assembler (begin-kw-arity asm req opt rest kw-indices
                                        allow-other-keys? nlocals alternate)
  (assert-match req ((? symbol?) ...) "list of symbols")
  (assert-match opt ((? symbol?) ...) "list of symbols")
  (assert-match rest (or #f (? symbol?)) "#f or symbol")
  (assert-match kw-indices (((? symbol?) . (? integer?)) ...)
                "alist of symbol -> integer")
  (assert-match allow-other-keys? (? boolean?) "boolean")
  (assert-match nlocals (? integer?) "integer")
  (assert-match alternate (or #f (? symbol?)) "#f or symbol")
  (let* ((meta (car (asm-meta asm)))
         (arity (make-arity req opt rest kw-indices allow-other-keys?
                            (asm-start asm) #f))
         ;; The procedure itself is in slot 0, in the standard calling
         ;; convention.  For procedure prologues, nreq includes the
         ;; procedure, so here we add 1.
         (nreq (1+ (length req)))
         (nopt (length opt))
         (rest? (->bool rest)))
    (set-meta-arities! meta (cons arity (meta-arities meta)))
    (cond
     ((or allow-other-keys? (pair? kw-indices))
      (emit-kw-prelude asm nreq nopt rest? kw-indices allow-other-keys?
                       nlocals alternate))
     ((or rest? (pair? opt))
      (emit-opt-prelude asm nreq nopt rest? nlocals alternate))
     (else
      (emit-standard-prelude asm nreq nlocals alternate)))))

(define-macro-assembler (end-arity asm)
  (let ((arity (car (meta-arities (car (asm-meta asm))))))
    (set-arity-high-pc! arity (asm-start asm))))

(define-macro-assembler (standard-prelude asm nreq nlocals alternate)
  (cond
   (alternate
    (emit-br-if-nargs-ne asm nreq alternate)
    (emit-alloc-frame asm nlocals))
   ((and (< nreq (ash 1 12)) (< (- nlocals nreq) (ash 1 12)))
    (emit-assert-nargs-ee/locals asm nreq (- nlocals nreq)))
   (else
    (emit-assert-nargs-ee asm nreq)
    (emit-alloc-frame asm nlocals))))

(define-macro-assembler (opt-prelude asm nreq nopt rest? nlocals alternate)
  (if alternate
      (emit-br-if-nargs-lt asm nreq alternate)
      (emit-assert-nargs-ge asm nreq))
  (cond
   (rest?
    (emit-bind-rest asm (+ nreq nopt)))
   (alternate
    (emit-br-if-nargs-gt asm (+ nreq nopt) alternate))
   (else
    (emit-assert-nargs-le asm (+ nreq nopt))))
  (emit-alloc-frame asm nlocals))

(define-macro-assembler (kw-prelude asm nreq nopt rest? kw-indices
                                    allow-other-keys? nlocals alternate)
  (if alternate
      (emit-br-if-nargs-lt asm nreq alternate)
      (emit-assert-nargs-ge asm nreq))
  (let ((ntotal (fold (lambda (kw ntotal)
                        (match kw
                          (((? keyword?) . idx)
                           (max (1+ idx) ntotal))))
                      (+ nreq nopt) kw-indices)))
    ;; FIXME: port 581f410f
    (emit-bind-kwargs asm nreq
                      (pack-flags allow-other-keys? rest?)
                      (+ nreq nopt)
                      ntotal
                      kw-indices)
    (emit-alloc-frame asm nlocals)))

(define-macro-assembler (label asm sym)
  (set-asm-labels! asm (acons sym (asm-start asm) (asm-labels asm))))

(define-macro-assembler (cache-current-module! asm module scope)
  (let ((mod-label (intern-module-cache-cell asm scope)))
    (emit-static-set! asm module mod-label 0)))

(define-macro-assembler (cached-toplevel-box asm dst scope sym bound?)
  (let ((sym-label (intern-non-immediate asm sym))
        (mod-label (intern-module-cache-cell asm scope))
        (cell-label (intern-cache-cell asm scope sym)))
    (emit-toplevel-box asm dst cell-label mod-label sym-label bound?)))

(define-macro-assembler (cached-module-box asm dst module-name sym public? bound?)
  (let* ((sym-label (intern-non-immediate asm sym))
         (key (cons public? module-name))
         (mod-name-label (intern-constant asm key))
         (cell-label (intern-cache-cell asm key sym)))
    (emit-module-box asm dst cell-label mod-name-label sym-label bound?)))




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
                      `((begin-program ,label ())
                        (assert-nargs-ee/locals 1 1)
                        ,@(reverse inits)
                        (load-constant 1 ,*unspecified*)
                        (return 1)
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
  (define stringbuf-shared-flag #x100)
  (define stringbuf-wide-flag #x400)
  (define tc7-stringbuf 39)
  (define tc7-narrow-stringbuf
    (+ tc7-stringbuf stringbuf-shared-flag))
  (define tc7-wide-stringbuf
    (+ tc7-stringbuf stringbuf-shared-flag stringbuf-wide-flag))
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
              (make-object asm name buf '() labels
                           #:flags (match name
                                     ('.data (logior SHF_ALLOC SHF_WRITE))
                                     ('.rodata SHF_ALLOC))))))))))

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
      (string-table-intern! strtab (if name (symbol->string name) "")))
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

;;; The .guile.arities section describes the arities that a function can
;;; have.  It is in two parts: a sorted array of headers describing
;;; basic arities, and an array of links out to a string table (and in
;;; the case of keyword arguments, to the data section) for argument
;;; names.  The whole thing is prefixed by a uint32 indicating the
;;; offset of the end of the headers array.
;;;
;;; The arity headers array is a packed array of structures of the form:
;;;
;;;   struct arity_header {
;;;     uint32_t low_pc;
;;;     uint32_t high_pc;
;;;     uint32_t offset;
;;;     uint32_t flags;
;;;     uint32_t nreq;
;;;     uint32_t nopt;
;;;   }
;;;
;;; All of the offsets and addresses are 32 bits.  We can expand in the
;;; future to use 64-bit offsets if appropriate, but there are other
;;; aspects of RTL that constrain us to a total image that fits in 32
;;; bits, so for the moment we'll simplify the problem space.
;;;
;;; The following flags values are defined:
;;;
;;;    #x1: has-rest?
;;;    #x2: allow-other-keys?
;;;    #x4: has-keyword-args?
;;;    #x8: is-case-lambda?
;;;
;;; Functions with a single arity specify their number of required and
;;; optional arguments in nreq and nopt, and do not have the
;;; is-case-lambda? flag set.  Their "offset" member links to an array
;;; of pointers into the associated .guile.arities.strtab string table,
;;; identifying the argument names.  This offset is relative to the
;;; start of the .guile.arities section.  Links for required arguments
;;; are first, in order, as uint32 values.  Next follow the optionals,
;;; then the rest link if has-rest? is set, then a link to the "keyword
;;; indices" literal if has-keyword-args? is set.  Unlike the other
;;; links, the kw-indices link points into the data section, and is
;;; relative to the ELF image as a whole.
;;;
;;; Functions with no arities have no arities information present in the
;;; .guile.arities section.
;;;
;;; Functions with multiple arities are preceded by a header with
;;; is-case-lambda? set.  All other fields are 0, except low-pc and
;;; high-pc which should be the bounds of the whole function.  Headers
;;; for the individual arities follow.  In this way the whole headers
;;; array is sorted in increasing low-pc order, and case-lambda clauses
;;; are contained within the [low-pc, high-pc] of the case-lambda
;;; header.

;; Length of the prefix to the arities section, in bytes.
(define arities-prefix-len 4)

;; Length of an arity header, in bytes.
(define arity-header-len (* 6 4))

;; The offset of "offset" within arity header, in bytes.
(define arity-header-offset-offset (* 2 4))

(define-syntax-rule (pack-arity-flags has-rest? allow-other-keys?
                                      has-keyword-args? is-case-lambda?)
  (logior (if has-rest? (ash 1 0) 0)
          (if allow-other-keys? (ash 1 1) 0)
          (if has-keyword-args? (ash 1 2) 0)
          (if is-case-lambda? (ash 1 3) 0)))

(define (meta-arities-size meta)
  (define (lambda-size arity)
    (+ arity-header-len
       (* 4    ;; name pointers
          (+ (length (arity-req arity))
             (length (arity-opt arity))
             (if (arity-rest arity) 1 0)
             (if (pair? (arity-kw-indices arity)) 1 0)))))
  (define (case-lambda-size arities)
    (fold +
          arity-header-len ;; case-lambda header
          (map lambda-size arities))) ;; the cases
  (match (meta-arities meta)
    (() 0)
    ((arity) (lambda-size arity))
    (arities (case-lambda-size arities))))

(define (write-arity-headers metas bv endianness)
  (define (write-arity-header* pos low-pc high-pc flags nreq nopt)
    (bytevector-u32-set! bv pos low-pc endianness)
    (bytevector-u32-set! bv (+ pos 4) high-pc endianness)
    (bytevector-u32-set! bv (+ pos 8) 0 endianness) ; offset
    (bytevector-u32-set! bv (+ pos 12) flags endianness)
    (bytevector-u32-set! bv (+ pos 16) nreq endianness)
    (bytevector-u32-set! bv (+ pos 20) nopt endianness))
  (define (write-arity-header pos arity)
    (write-arity-header* pos (arity-low-pc arity)
                         (arity-high-pc arity)
                         (pack-arity-flags (arity-rest arity)
                                           (arity-allow-other-keys? arity)
                                           (pair? (arity-kw-indices arity))
                                           #f)
                         (length (arity-req arity))
                         (length (arity-opt arity))))
  (let lp ((metas metas) (pos arities-prefix-len) (offsets '()))
    (match metas
      (()
       ;; Fill in the prefix.
       (bytevector-u32-set! bv 0 pos endianness)
       (values pos (reverse offsets)))
      ((meta . metas)
       (match (meta-arities meta)
         (() (lp metas pos offsets))
         ((arity)
          (write-arity-header pos arity)
          (lp metas
              (+ pos arity-header-len)
              (acons arity (+ pos arity-header-offset-offset) offsets)))
         (arities
          ;; Write a case-lambda header, then individual arities.
          ;; The case-lambda header's offset link is 0.
          (write-arity-header* pos (meta-low-pc meta) (meta-high-pc meta)
                               (pack-arity-flags #f #f #f #t) 0 0)
          (let lp* ((arities arities) (pos (+ pos arity-header-len))
                    (offsets offsets))
            (match arities
              (() (lp metas pos offsets))
              ((arity . arities)
               (write-arity-header pos arity)
               (lp* arities
                    (+ pos arity-header-len)
                    (acons arity
                           (+ pos arity-header-offset-offset)
                           offsets)))))))))))

(define (write-arity-links asm bv pos arity-offset-pairs strtab)
  (define (write-symbol sym pos)
    (bytevector-u32-set! bv pos
                         (string-table-intern! strtab (symbol->string sym))
                         (asm-endianness asm))
    (+ pos 4))
  (define (write-kw-indices pos kw-indices)
    ;; FIXME: Assert that kw-indices is already interned.
    (make-linker-reloc 'abs32/1 pos 0
                       (intern-constant asm kw-indices)))
  (let lp ((pos pos) (pairs arity-offset-pairs) (relocs '()))
    (match pairs
      (()
       (unless (= pos (bytevector-length bv))
         (error "expected to fully fill the bytevector"
                pos (bytevector-length bv)))
       relocs)
      (((arity . offset) . pairs)
       (bytevector-u32-set! bv offset pos (asm-endianness asm))
       (let ((pos (fold write-symbol
                        pos
                        (append (arity-req arity)
                                (arity-opt arity)
                                (cond
                                 ((arity-rest arity) => list)
                                 (else '()))))))
         (match (arity-kw-indices arity)
           (() (lp pos pairs relocs))
           (kw-indices
            (lp (+ pos 4)
                pairs
                (cons (write-kw-indices pos kw-indices) relocs)))))))))

(define (link-arities asm)
  (let* ((endianness (asm-endianness asm))
         (metas (reverse (asm-meta asm)))
         (size (fold (lambda (meta size)
                       (+ size (meta-arities-size meta)))
                     arities-prefix-len
                     metas))
         (strtab (make-string-table))
         (bv (make-bytevector size 0)))
    (let ((kw-indices-relocs
           (call-with-values
               (lambda ()
                 (write-arity-headers metas bv endianness))
             (lambda (pos arity-offset-pairs)
               (write-arity-links asm bv pos arity-offset-pairs strtab)))))
      (let ((strtab (make-object asm '.guile.arities.strtab
                                 (link-string-table! strtab)
                                 '() '()
                                 #:type SHT_STRTAB #:flags 0)))
        (values (make-object asm '.guile.arities
                             bv
                             kw-indices-relocs '()
                             #:type SHT_PROGBITS #:flags 0
                             #:link (elf-section-index
                                     (linker-object-section strtab)))
                strtab)))))

;;;
;;; The .guile.docstrs section is a packed, sorted array of (pc, str)
;;; values.  Pc and str are both 32 bits wide.  (Either could change to
;;; 64 bits if appropriate in the future.)  Pc is the address of the
;;; entry to a program, relative to the start of the text section, and
;;; str is an index into the associated .guile.docstrs.strtab string
;;; table section.
;;;

;; The size of a docstrs entry, in bytes.
(define docstr-size 8)

(define (link-docstrs asm)
  (define (find-docstrings)
    (filter-map (lambda (meta)
                  (define (is-documentation? pair)
                    (eq? (car pair) 'documentation))
                  (let* ((props (meta-properties meta))
                         (tail (find-tail is-documentation? props)))
                    (and tail
                         (not (find-tail is-documentation? (cdr tail)))
                         (string? (cdar tail))
                         (cons (meta-low-pc meta) (cdar tail)))))
                (reverse (asm-meta asm))))
  (let* ((endianness (asm-endianness asm))
         (docstrings (find-docstrings))
         (strtab (make-string-table))
         (bv (make-bytevector (* (length docstrings) docstr-size) 0)))
    (fold (lambda (pair pos)
            (match pair
              ((pc . string)
               (bytevector-u32-set! bv pos pc endianness)
               (bytevector-u32-set! bv (+ pos 4)
                                    (string-table-intern! strtab string)
                                    endianness)
               (+ pos docstr-size))))
          0
          docstrings)
    (let ((strtab (make-object asm '.guile.docstrs.strtab
                               (link-string-table! strtab)
                               '() '()
                               #:type SHT_STRTAB #:flags 0)))
      (values (make-object asm '.guile.docstrs
                           bv
                           '() '()
                           #:type SHT_PROGBITS #:flags 0
                           #:link (elf-section-index
                                   (linker-object-section strtab)))
              strtab))))

;;;
;;; The .guile.procprops section is a packed, sorted array of (pc, addr)
;;; values.  Pc and addr are both 32 bits wide.  (Either could change to
;;; 64 bits if appropriate in the future.)  Pc is the address of the
;;; entry to a program, relative to the start of the text section, and
;;; addr is the address of the associated properties alist, relative to
;;; the start of the ELF image.
;;;
;;; Since procedure properties are stored in the data sections, we need
;;; to link the procedures property section first.  (Note that this
;;; constraint does not apply to the arities section, which may
;;; reference the data sections via the kw-indices literal, because
;;; assembling the text section already makes sure that the kw-indices
;;; are interned.)
;;;

;; The size of a procprops entry, in bytes.
(define procprops-size 8)

(define (link-procprops asm)
  (define (assoc-remove-one alist key value-pred)
    (match alist
      (() '())
      ((((? (lambda (x) (eq? x key))) . value) . alist)
       (if (value-pred value)
           alist
           (acons key value alist)))
      (((k . v) . alist)
       (acons k v (assoc-remove-one alist key value-pred)))))
  (define (props-without-name-or-docstring meta)
    (assoc-remove-one
     (assoc-remove-one (meta-properties meta) 'name (lambda (x) #t))
     'documentation
     string?))
  (define (find-procprops)
    (filter-map (lambda (meta)
                  (let ((props (props-without-name-or-docstring meta)))
                    (and (pair? props)
                         (cons (meta-low-pc meta) props))))
                (reverse (asm-meta asm))))
  (let* ((endianness (asm-endianness asm))
         (procprops (find-procprops))
         (bv (make-bytevector (* (length procprops) procprops-size) 0)))
    (let lp ((procprops procprops) (pos 0) (relocs '()))
      (match procprops
        (()
         (make-object asm '.guile.procprops
                      bv
                      relocs '()
                      #:type SHT_PROGBITS #:flags 0))
        (((pc . props) . procprops)
         (bytevector-u32-set! bv pos pc endianness)
         (lp procprops
             (+ pos procprops-size)
             (cons (make-linker-reloc 'abs32/1 (+ pos 4) 0
                                      (intern-constant asm props))
                   relocs)))))))

(define (link-objects asm)
  (let*-values (;; Link procprops before constants, because it probably
                ;; interns more constants.
                ((procprops) (link-procprops asm))
                ((ro rw rw-init) (link-constants asm))
                ;; Link text object after constants, so that the
                ;; constants initializer gets included.
                ((text) (link-text-object asm))
                ((dt) (link-dynamic-section asm text rw rw-init))
                ((symtab strtab) (link-symtab (linker-object-section text) asm))
                ((arities arities-strtab) (link-arities asm))
                ((docstrs docstrs-strtab) (link-docstrs asm))
                ;; This needs to be linked last, because linking other
                ;; sections adds entries to the string table.
                ((shstrtab) (link-shstrtab asm)))
    (filter identity
            (list text ro rw dt symtab strtab arities arities-strtab
                  docstrs docstrs-strtab procprops shstrtab))))




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
