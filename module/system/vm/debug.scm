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
  #:use-module (system vm dwarf)
  #:use-module (system vm objcode)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (debug-context-image
            debug-context-base
            debug-context-text-base

            program-debug-info-name
            program-debug-info-context
            program-debug-info-image
            program-debug-info-offset
            program-debug-info-addr
            program-debug-info-u32-offset
            program-debug-info-u32-offset-end

            arity?
            arity-low-pc
            arity-high-pc
            arity-nreq
            arity-nopt
            arity-has-rest?
            arity-allow-other-keys?
            arity-has-keyword-args?
            arity-is-case-lambda?

            debug-context-from-image
            for-each-elf-symbol
            find-debug-context
            find-program-debug-info
            arity-arguments-alist
            find-program-arities
            program-minimum-arity

            find-program-docstring

            find-program-properties

            source?
            source-pre-pc
            source-post-pc
            source-file
            source-line
            source-line-for-user
            source-column
            find-source-for-addr
            find-program-sources))

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

(define (for-each-elf-symbol context proc)
  "Call @var{proc} on each symbol in the symbol table of @var{context}."
  (let ((elf (debug-context-elf context)))
    (cond
     ((elf-section-by-name elf ".symtab")
      => (lambda (symtab)
           (let ((len (elf-symbol-table-len symtab))
                 (strtab (elf-section elf (elf-section-link symtab))))
             (let lp ((n 0))
               (when (< n len)
                 (proc (elf-symbol-table-ref elf symtab n strtab))
                 (lp (1+ n))))))))))

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

(define (debug-context-from-image bv)
  "Build a debugging context corresponding to a given ELF image."
  (let* ((elf (parse-elf bv))
         (base (pointer-address (bytevector->pointer (elf-bytes elf))))
         (text-base (elf-section-offset
                     (or (elf-section-by-name elf ".rtl-text")
                         (error "ELF object has no text section")))))
    (make-debug-context elf base text-base)))

(define (find-debug-context addr)
  "Find and return the debugging context corresponding to the ELF image
containing the address @var{addr}.  @var{addr} is an integer.  If no ELF
image is found, return @code{#f}.  It's possible for an RTL program not
to have an ELF image if the program was defined in as a stub in C."
  (and=> (find-mapped-elf-image addr)
         debug-context-from-image))

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
   ((and context
         (find-elf-symbol (debug-context-elf context)
                          (- addr
                             (debug-context-base context)
                             (debug-context-text-base context))))
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

(define-record-type <arity>
  (make-arity context base header-offset)
  arity?
  (context arity-context)
  (base arity-base)
  (header-offset arity-header-offset))

(define arities-prefix-len 4)
(define arity-header-len (* 6 4))

;;;   struct arity_header {
;;;     uint32_t low_pc;
;;;     uint32_t high_pc;
;;;     uint32_t offset;
;;;     uint32_t flags;
;;;     uint32_t nreq;
;;;     uint32_t nopt;
;;;   }

(define (arity-low-pc* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 0 4))))
(define (arity-high-pc* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 1 4))))
(define (arity-offset* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 2 4))))
(define (arity-flags* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 3 4))))
(define (arity-nreq* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 4 4))))
(define (arity-nopt* bv header-pos)
  (bytevector-u32-native-ref bv (+ header-pos (* 5 4))))

;;;    #x1: has-rest?
;;;    #x2: allow-other-keys?
;;;    #x4: has-keyword-args?
;;;    #x8: is-case-lambda?

(define (has-rest? flags)         (not (zero? (logand flags (ash 1 0)))))
(define (allow-other-keys? flags) (not (zero? (logand flags (ash 1 1)))))
(define (has-keyword-args? flags) (not (zero? (logand flags (ash 1 2)))))
(define (is-case-lambda? flags)   (not (zero? (logand flags (ash 1 3)))))

(define (arity-low-pc arity)
  (arity-low-pc* (elf-bytes (debug-context-elf (arity-context arity)))
                 (arity-header-offset arity)))

(define (arity-high-pc arity)
  (arity-high-pc* (elf-bytes (debug-context-elf (arity-context arity)))
                  (arity-header-offset arity)))

(define (arity-nreq arity)
  (arity-nreq* (elf-bytes (debug-context-elf (arity-context arity)))
               (arity-header-offset arity)))

(define (arity-nopt arity)
  (arity-nopt* (elf-bytes (debug-context-elf (arity-context arity)))
               (arity-header-offset arity)))

(define (arity-flags arity)
  (arity-flags* (elf-bytes (debug-context-elf (arity-context arity)))
                (arity-header-offset arity)))

(define (arity-has-rest? arity) (has-rest? (arity-flags arity)))
(define (arity-allow-other-keys? arity) (allow-other-keys? (arity-flags arity)))
(define (arity-has-keyword-args? arity) (has-keyword-args? (arity-flags arity)))
(define (arity-is-case-lambda? arity) (is-case-lambda? (arity-flags arity)))

(define (arity-load-symbol arity)
  (let ((elf (debug-context-elf (arity-context arity))))
    (cond
     ((elf-section-by-name elf ".guile.arities")
      =>
      (lambda (sec)
        (let* ((strtab (elf-section elf (elf-section-link sec)))
               (bv (elf-bytes elf))
               (strtab-offset (elf-section-offset strtab)))
          (lambda (n)
            (string->symbol (string-table-ref bv (+ strtab-offset n)))))))
     (else (error "couldn't find arities section")))))

(define (arity-arguments-alist arity)
  (let* ((bv (elf-bytes (debug-context-elf (arity-context arity))))
         (%load-symbol (arity-load-symbol arity))
         (header (arity-header-offset arity))
         (link-offset (arity-offset* bv header))
         (link (+ (arity-base arity) link-offset))
         (flags (arity-flags* bv header))
         (nreq (arity-nreq* bv header))
         (nopt (arity-nopt* bv header)))
    (define (load-symbol idx)
      (%load-symbol (bytevector-u32-native-ref bv (+ link (* idx 4)))))
    (define (load-symbols skip n)
      (let lp ((n n) (out '()))
        (if (zero? n)
            out
            (lp (1- n)
                (cons (load-symbol (+ skip (1- n))) out)))))
    (define (unpack-scm n)
      (pointer->scm (make-pointer n)))
    (define (load-non-immediate idx)
      (let ((offset (bytevector-u32-native-ref bv (+ link (* idx 4)))))
        (unpack-scm (+ (debug-context-base (arity-context arity)) offset))))
    (and (not (is-case-lambda? flags))
         `((required . ,(load-symbols 0 nreq))
           (optional . ,(load-symbols nreq nopt))
           (rest . ,(and (has-rest? flags) (load-symbol (+ nreq nopt))))
           (keyword . ,(if (has-keyword-args? flags)
                           (load-non-immediate
                            (+ nreq nopt (if (has-rest? flags) 1 0)))
                           '()))
           (allow-other-keys? . ,(allow-other-keys? flags))))))

(define (find-first-arity context base addr)
  (let* ((bv (elf-bytes (debug-context-elf context)))
         (text-offset (- addr
                         (debug-context-text-base context)
                         (debug-context-base context)))
         (headers-start (+ base arities-prefix-len))
         (headers-end (+ base (bytevector-u32-native-ref bv base))))
    ;; FIXME: This is linear search.  Change to binary search.
    (let lp ((pos headers-start))
      (cond
       ((>= pos headers-end) #f)
       ((< text-offset (* (arity-low-pc* bv pos) 4))
        #f)
       ((<= (* (arity-high-pc* bv pos) 4) text-offset)
        (lp (+ pos arity-header-len)))
       (else
        (make-arity context base pos))))))

(define (read-sub-arities context base outer-header-offset)
  (let* ((bv (elf-bytes (debug-context-elf context)))
         (headers-end (+ base (bytevector-u32-native-ref bv base)))
         (low-pc (arity-low-pc* bv outer-header-offset))
         (high-pc (arity-high-pc* bv outer-header-offset)))
    (let lp ((pos (+ outer-header-offset arity-header-len)) (out '()))
      (if (and (< pos headers-end) (<= (arity-high-pc* bv pos) high-pc))
          (lp (+ pos arity-header-len)
              (cons (make-arity context base pos) out))
          (reverse out)))))

(define* (find-program-arities addr #:optional
                               (context (find-debug-context addr)))
  (and=>
   (and context
        (elf-section-by-name (debug-context-elf context) ".guile.arities"))
   (lambda (sec)
     (let* ((base (elf-section-offset sec))
            (first (find-first-arity context base addr)))
       ;; FIXME: Handle case-lambda arities.
       (cond
        ((not first) '())
        ((arity-is-case-lambda? first)
         (read-sub-arities context base (arity-header-offset first)))
        (else (list first)))))))

(define* (program-minimum-arity addr #:optional
                                (context (find-debug-context addr)))
  (and=>
   (and context
        (elf-section-by-name (debug-context-elf context) ".guile.arities"))
   (lambda (sec)
     (let* ((base (elf-section-offset sec))
            (first (find-first-arity context base addr)))
       (if (arity-is-case-lambda? first)
           (list 0 0 #t) ;; FIXME: be more precise.
           (list (arity-nreq first)
                 (arity-nopt first)
                 (arity-has-rest? first)))))))

(define* (find-program-docstring addr #:optional
                                 (context (find-debug-context addr)))
  (and=>
   (and context
        (elf-section-by-name (debug-context-elf context) ".guile.docstrs"))
   (lambda (sec)
     ;; struct docstr {
     ;;   uint32_t pc;
     ;;   uint32_t str;
     ;; }
     (define docstr-len 8)
     (let* ((start (elf-section-offset sec))
            (end (+ start (elf-section-size sec)))
            (bv (elf-bytes (debug-context-elf context)))
            (text-offset (- addr
                            (debug-context-text-base context)
                            (debug-context-base context))))
       ;; FIXME: This is linear search.  Change to binary search.
       (let lp ((pos start))
         (cond
          ((>= pos end) #f)
          ((< (bytevector-u32-native-ref bv pos) text-offset)
           (lp (+ pos docstr-len)))
          ((= text-offset (bytevector-u32-native-ref bv pos))
           (let ((strtab (elf-section (debug-context-elf context)
                                      (elf-section-link sec)))
                 (idx (bytevector-u32-native-ref bv (+ pos 4))))
             (string-table-ref bv (+ (elf-section-offset strtab) idx))))
          (else #f)))))))

(define* (find-program-properties addr #:optional
                                  (context (find-debug-context addr)))
  (define (add-name-and-docstring props)
    (define (maybe-acons k v tail)
      (if v (acons k v tail) tail))
    (let ((name (and=> (find-program-debug-info addr context)
                       program-debug-info-name))
          (docstring (find-program-docstring addr context)))
      (maybe-acons 'name name
                   (maybe-acons 'documentation docstring props))))
  (add-name-and-docstring
   (cond
    ((and context
          (elf-section-by-name (debug-context-elf context) ".guile.procprops"))
     => (lambda (sec)
          ;; struct procprop {
          ;;   uint32_t pc;
          ;;   uint32_t offset;
          ;; }
          (define procprop-len 8)
          (let* ((start (elf-section-offset sec))
                 (end (+ start (elf-section-size sec)))
                 (bv (elf-bytes (debug-context-elf context)))
                 (text-offset (- addr
                                 (debug-context-text-base context)
                                 (debug-context-base context))))
            (define (unpack-scm addr)
              (pointer->scm (make-pointer addr)))
            (define (load-non-immediate offset)
              (unpack-scm (+ (debug-context-base context) offset)))
            ;; FIXME: This is linear search.  Change to binary search.
            (let lp ((pos start))
              (cond
               ((>= pos end) '())
               ((< text-offset (bytevector-u32-native-ref bv pos))
                (lp (+ pos procprop-len)))
               ((> text-offset (bytevector-u32-native-ref bv pos))
                '())
               (else
                (load-non-immediate
                 (bytevector-u32-native-ref bv (+ pos 4)))))))))
    (else '()))))

(define-record-type <source>
  (make-source pre-pc file line column)
  source?
  (pre-pc source-pre-pc)
  (file source-file)
  (line source-line)
  (column source-column))

(define (make-source/dwarf pc file line column)
  (make-source pc file
               ;; Convert DWARF-numbered (1-based) lines and
               ;; columns to Guile conventions (0-based).
               (and line (1- line)) (and column (1- column))))

;; FIXME
(define (source-post-pc source)
  (source-pre-pc source))

;; Lines are zero-indexed inside Guile, but users expect them to be
;; one-indexed. Columns, on the other hand, are zero-indexed to both. Go
;; figure.
(define (source-line-for-user source)
  (1+ (source-line source)))

(define* (find-source-for-addr addr #:optional
                               (context (find-debug-context addr))
                               #:key exact?)
  (and=>
   (and context
        (false-if-exception
         (elf->dwarf-context (debug-context-elf context))))
   (lambda (dwarf-ctx)
     (let* ((base (debug-context-base context))
            (pc (- addr base)))
       (or-map (lambda (die)
                 (and=>
                  (die-line-prog die)
                  (lambda (prog)
                    (call-with-values
                        (lambda () (line-prog-scan-to-pc prog pc))
                      (lambda (pc* file line col)
                        (and pc* (or (= pc pc*) (not exact?))
                             (make-source/dwarf (+ pc* base)
                                                file line col)))))))
               (read-die-roots dwarf-ctx))))))

(define* (find-program-die addr #:optional
                           (context (find-debug-context addr)))
  (and=> (and context
              (false-if-exception
               (elf->dwarf-context (debug-context-elf context))))
         (lambda (dwarf-ctx)
           (find-die-by-pc (read-die-roots dwarf-ctx)
                           (- addr (debug-context-base context))))))

(define* (find-program-sources addr #:optional
                               (context (find-debug-context addr)))
  (cond
   ((find-program-die addr context)
    => (lambda (die)
         (let* ((base (debug-context-base context))
                (low-pc (die-ref die 'low-pc))
                (high-pc (die-high-pc die))
                (prog (let line-prog ((die die))
                        (and die
                             (or (die-line-prog die)
                                 (line-prog (ctx-die (die-ctx die))))))))
           (cond
            ((and low-pc high-pc prog)
             (let lp ((sources '()))
               (call-with-values (lambda ()
                                   (if (null? sources)
                                       (line-prog-scan-to-pc prog low-pc)
                                       (line-prog-advance prog)))
                 (lambda (pc file line col)
                   (if (and pc (< pc high-pc))
                       ;; For the first source, it's probable that the
                       ;; address of the line program is before the
                       ;; low-pc, since the line program is for the
                       ;; entire compilation unit, and there are no
                       ;; redundant "rows" in the line program.
                       ;; Therefore in that case use the addr of low-pc
                       ;; instead of the one we got back.
                       (let ((addr (+ (if (null? sources) low-pc pc) base)))
                         (lp (cons (make-source/dwarf addr file line col)
                                   sources)))
                       (reverse sources))))))
            (else '())))))
   (else '())))
