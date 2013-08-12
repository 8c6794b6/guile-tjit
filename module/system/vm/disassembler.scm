;;; Guile RTL disassembler

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

;;; Code:

(define-module (system vm disassembler)
  #:use-module (system vm instruction)
  #:use-module (system vm elf)
  #:use-module (system vm debug)
  #:use-module (system vm program)
  #:use-module (system vm objcode)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:export (disassemble-program))

(define-syntax-rule (u32-ref buf n)
  (bytevector-u32-native-ref buf (* n 4)))

(define-syntax-rule (s32-ref buf n)
  (bytevector-s32-native-ref buf (* n 4)))

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

(eval-when (expand compile load eval)
  (define (id-append ctx a b)
    (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b)))))

(define (unpack-scm n)
  (pointer->scm (make-pointer n)))

(define (unpack-s24 s)
  (if (zero? (logand s (ash 1 23)))
      s
      (- s (ash 1 24))))

(define (unpack-s32 s)
  (if (zero? (logand s (ash 1 31)))
      s
      (- s (ash 1 32))))

(define-syntax disassembler
  (lambda (x)
    (define (parse-first-word word type)
      (with-syntax ((word word))
        (case type
          ((U8_X24)
           #'())
          ((U8_U24)
           #'((ash word -8)))
          ((U8_L24)
           #'((unpack-s24 (ash word -8))))
          ((U8_U8_I16)
           #'((logand (ash word -8) #xff)
              (ash word -16)))
          ((U8_U12_U12)
           #'((logand (ash word -8) #xfff)
              (ash word -20)))
          ((U8_U8_U8_U8)
           #'((logand (ash word -8) #xff)
              (logand (ash word -16) #xff)
              (ash word -24)))
          (else
           (error "bad kind" type)))))

    (define (parse-tail-word word type)
      (with-syntax ((word word))
        (case type
          ((U8_X24)
           #'((logand word #ff)))
          ((U8_U24)
           #'((logand word #xff)
              (ash word -8)))
          ((U8_L24)
           #'((logand word #xff)
              (unpack-s24 (ash word -8))))
          ((U8_U8_I16)
           #'((logand word #xff)
              (logand (ash word -8) #xff)
              (ash word -16)))
          ((U8_U12_U12)
           #'((logand word #xff)
              (logand (ash word -8) #xfff)
              (ash word -20)))
          ((U8_U8_U8_U8)
           #'((logand word #xff)
              (logand (ash word -8) #xff)
              (logand (ash word -16) #xff)
              (ash word -24)))
          ((U32)
           #'(word))
          ((I32)
           #'(word))
          ((A32)
           #'(word))
          ((B32)
           #'(word))
          ((N32)
           #'((unpack-s32 word)))
          ((S32)
           #'((unpack-s32 word)))
          ((L32)
           #'((unpack-s32 word)))
          ((LO32)
           #'((unpack-s32 word)))
          ((X8_U24)
           #'((ash word -8)))
          ((X8_U12_U12)
           #'((logand (ash word -8) #xfff)
              (ash word -20)))
          ((X8_L24)
           #'((unpack-s24 (ash word -8))))
          ((B1_X7_L24)
           #'((not (zero? (logand word #x1)))
              (unpack-s24 (ash word -8))))
          ((B1_U7_L24)
           #'((not (zero? (logand word #x1)))
              (logand (ash word -1) #x7f)
              (unpack-s24 (ash word -8))))
          ((B1_X31)
           #'((not (zero? (logand word #x1)))))
          ((B1_X7_U24)
           #'((not (zero? (logand word #x1)))
              (ash word -8)))
          (else
           (error "bad kind" type)))))

    (syntax-case x ()
      ((_ name opcode word0 word* ...)
       (let ((vars (generate-temporaries #'(word* ...))))
         (with-syntax (((word* ...) vars)
                       ((n ...) (map 1+ (iota (length #'(word* ...)))))
                       ((asm ...)
                        (parse-first-word #'first (syntax->datum #'word0)))
                       (((asm* ...) ...)
                        (map (lambda (word type)
                               (parse-tail-word word type))
                             vars
                             (syntax->datum #'(word* ...)))))
           #'(lambda (buf offset first)
               (let ((word* (u32-ref buf (+ offset n)))
                     ...)
                 (values (+ 1 (length '(word* ...)))
                         (list 'name asm ... asm* ... ...))))))))))

(define (disasm-invalid buf offset first)
  (error "bad instruction" (logand first #xff) first buf offset))

(define disassemblers (make-vector 256 disasm-invalid))

(define-syntax define-disassembler
  (lambda (x)
    (syntax-case x ()
      ((_ name opcode kind arg ...)
       (with-syntax ((parse (id-append #'name #'parse- #'name)))
         #'(let ((parse (disassembler name opcode arg ...)))
             (vector-set! disassemblers opcode parse)))))))

(visit-opcodes define-disassembler)

;; -> len list
(define (disassemble-one buf offset)
  (let ((first (u32-ref buf offset)))
    ((vector-ref disassemblers (logand first #xff)) buf offset first)))

(define (u32-offset->addr offset context)
  "Given an offset into an image in 32-bit units, return the absolute
address of that offset."
  (+ (debug-context-base context) (* offset 4)))

(define (code-annotation code len offset start labels context)
  ;; FIXME: Print names for register loads and stores that correspond to
  ;; access to named locals.
  (define (reference-scm target)
    (unpack-scm (u32-offset->addr (+ offset target) context)))

  (define (dereference-scm target)
    (let ((addr (u32-offset->addr (+ offset target)
                                  context)))
      (pointer->scm
       (dereference-pointer (make-pointer addr)))))

  (match code
    (((or 'br
          'br-if-nargs-ne 'br-if-nargs-lt 'br-if-nargs-gt
          'br-if-true 'br-if-null 'br-if-nil 'br-if-pair 'br-if-struct
          'br-if-char 'br-if-tc7 'br-if-eq 'br-if-eqv 'br-if-equal
          'br-if-= 'br-if-< 'br-if-<= 'br-if-> 'br-if->=) _ ... target)
     (list "-> ~A" (vector-ref labels (- (+ offset target) start))))
    (('prompt tag flags handler)
     ;; The H is for handler.
     (list "H -> ~A" (vector-ref labels (- (+ offset handler) start))))
    (((or 'make-short-immediate 'make-long-immediate) _ imm)
     (list "~S" (unpack-scm imm)))
    (('make-long-long-immediate _ high low)
     (list "~S" (unpack-scm (logior (ash high 32) low))))
    (('assert-nargs-ee/locals nargs locals)
     ;; The nargs includes the procedure.
     (list "~a arg~:p, ~a local~:p" (1- nargs) locals))
    (('tail-call nargs proc)
     (list "~a arg~:p" nargs))
    (('make-closure dst target nfree)
     (let* ((addr (u32-offset->addr (+ offset target) context))
            (pdi (find-program-debug-info addr context)))
       ;; FIXME: Disassemble embedded closures as well.
       (list "~A at 0x~X (~A free var~:p)"
             (or (and pdi (program-debug-info-name pdi))
                 "(anonymous procedure)")
             addr
             nfree)))
    (('make-non-immediate dst target)
     (list "~@Y" (reference-scm target)))
    (((or 'static-ref 'static-set!) _ target)
     (list "~@Y" (dereference-scm target)))
    (('link-procedure! src target)
     (let* ((addr (u32-offset->addr (+ offset target) context))
            (pdi (find-program-debug-info addr context)))
       (list "~A at 0x~X"
             (or (and pdi (program-debug-info-name pdi))
                 "(anonymous procedure)")
             addr)))
    (('resolve-module dst name public)
     (list "~a" (if (zero? public) "private" "public")))
    (('toplevel-box _ var-offset mod-offset sym-offset bound?)
     (list "`~A'~A" (dereference-scm sym-offset)
           (if bound? "" " (maybe unbound)")))
    (('module-box _ var-offset mod-name-offset sym-offset bound?)
     (let ((mod-name (reference-scm mod-name-offset)))
       (list "`(~A ~A ~A)'~A" (if (car mod-name) '@ '@@) (cdr mod-name)
             (dereference-scm sym-offset)
             (if bound? "" " (maybe unbound)"))))
    (('load-typed-array dst type shape target len)
     (let ((addr (u32-offset->addr (+ offset target) context)))
       (list "~a bytes from #x~X" len addr)))
    (_ #f)))

(define (compute-labels bv start end)
  (let ((labels (make-vector (- end start) #f)))
    (define (add-label! pos header)
      (unless (vector-ref labels (- pos start))
        (vector-set! labels (- pos start) header)))

    (let lp ((offset start))
      (when (< offset end)
        (call-with-values (lambda () (disassemble-one bv offset))
          (lambda (len elt)
            (match elt
              ((inst arg ...)
               (case inst
                 ((br
                   br-if-nargs-ne br-if-nargs-lt br-if-nargs-gt
                   br-if-true br-if-null br-if-nil br-if-pair br-if-struct
                   br-if-char br-if-tc7 br-if-eq br-if-eqv br-if-equal
                   br-if-= br-if-< br-if-<= br-if-> br-if->=)
                  (match arg
                    ((_ ... target)
                     (add-label! (+ offset target) "L"))))
                 ((prompt)
                  (match arg
                    ((_ ... target)
                     (add-label! (+ offset target) "H")))))))
            (lp (+ offset len))))))
    (let lp ((offset start) (n 1))
      (when (< offset end)
        (let* ((pos (- offset start))
               (label (vector-ref labels pos)))
          (if label
              (begin
                (vector-set! labels
                             pos
                             (string->symbol
                              (string-append label (number->string n))))
                (lp (1+ offset) (1+ n)))
              (lp (1+ offset) n)))))
    labels))

(define (print-info port addr label info extra src)
  (when label
    (format port "~A:\n" label))
  (format port "~4@S    ~32S~@[;; ~1{~@?~}~]~@[~61t at ~a~]\n"
          addr info extra src))

(define (disassemble-buffer port bv start end context)
  (let ((labels (compute-labels bv start end)))
    (let lp ((offset start))
      (when (< offset end)
        (call-with-values (lambda () (disassemble-one bv offset))
          (lambda (len elt)
            (let ((pos (- offset start))
                  (annotation (code-annotation elt len offset start labels
                                               context)))
              (print-info port pos (vector-ref labels pos) elt annotation #f)
              (lp (+ offset len)))))))))

(define* (disassemble-program program #:optional (port (current-output-port)))
  (cond
   ((find-program-debug-info (rtl-program-code program))
    => (lambda (pdi)
         (format port "Disassembly of ~S at #x~X:\n\n" program
                 (program-debug-info-addr pdi))
         (disassemble-buffer port
                             (program-debug-info-image pdi)
                             (program-debug-info-u32-offset pdi)
                             (program-debug-info-u32-offset-end pdi)
                             (program-debug-info-context pdi))))
   (else
    (format port "Debugging information unavailable.~%")))
  (values))
