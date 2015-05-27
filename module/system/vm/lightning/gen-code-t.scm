;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public License
;;;; as published by the Free Software Foundation; either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA

;;; Commentary:

;;; Parses lightning header file, outputs scheme code.

;;; Code:

(use-modules (ice-9 peg)
             (ice-9 pretty-print)
             (ice-9 regex)
             (system foreign))

;;; Auxiliary

(define (lightning-header-path)
  (string-append (substring (cadr (command-line)) 2) "lightning.h"))

(define (header-contents path)
 (call-with-input-file path
   (lambda (port)
     (reverse-list->string
      (let lp ((c (read-char port)) (acc '()))
        (if (eof-object? c)
            acc
            (lp (read-char port) (cons c acc))))))))

(define-peg-string-patterns
  "jit-code-t <-- NL* SPACE* (cmacro* comment* jitcodes* NL*)*
   cmacro     <-- SHARP (!NL .)*
   CHARS      <-- (!NL !SPACE !TAB .)*
   jitcodes   <-- (SPACE* jitcode COMMA SPACE* TAB*)*
   jitcode    <-- 'jit_code_' (!COMMA .)*
   comment    <-- SPACE* TAB* COMSTART (!COMEND .)* COMEND SPACE* TAB*
   COMSTART   <   '/*'
   COMEND     <   '*/'
   SHARP      <-  '#'
   NL         <   '\n'
   SPACE      <   ' '
   TAB        <   '\t'
   COMMA      <   ','")

(define (typedef-enum-contents path)
  (let ((lst (string-split (header-contents path) #\newline)))
    (define (enum-start lst)
      (if (string-prefix? "typedef enum {" (car lst))
          (enum-inside (cdr lst))
          (enum-start (cdr lst))))
    (define (enum-inside lst)
      (let lp ((lst lst) (acc '()))
        (if (string-prefix? "}" (car lst))
            (reverse acc)
            (lp (cdr lst) (cons (car lst) acc)))))
    (string-append "\n" (string-join (enum-start lst) "\n"))))

(define (enum-peg-tree path)
  (peg:tree (match-pattern jit-code-t (typedef-enum-contents path))))


;;;
;;; Enum values
;;;

(define (print-enums t)
  (define (c-name->s-name str)
    (regexp-substitute/global #f "_" str 'pre "-" 'post))
  (define (enum-codes peg-tree)
    (let lp ((l (cdr peg-tree)))
      (if (null? l)
          '()
          (let ((term (car l)))
            (if (eq? (car term) 'jitcodes)
                (append (cdr term) (lp (cdr l)))
                (lp (cdr l)))))))
  (let ((codes (enum-codes t))
        (macros '()))
    (let lp ((codes codes) (n 0))
      (unless (null? codes)
        (format #t "(define ~a ~a)~%"
                (c-name->s-name (cadr (car codes))) n)
        (lp (cdr codes) (+ n 1))))))


;;;
;;; C macro -> scheme procedure
;;;

(define-peg-string-patterns
  "code    <-  CDEF SPACE EXPOSED TABS func
   CDEF    <   SHARP SPACE* 'define'
   EXPOSED <   (! TAB .)*
   TABS    <   TAB*
   TAB     <   '\t'
   func    <-- 'jit_new_node_' (! OPAREN .)* OPAREN ARGS CPAREN
   OPAREN  <   '('
   CPAREN  <   ')'
   ARGS    <-- enum (COMMA ARG)*
   ARG     <-- null/(! COMMA .)
   enum    <-- (! COMMA .)*
   null    <-  'NULL'")

(define (under-score->hyphen str)
  (regexp-substitute/global #f "_" str 'pre "-" 'post))

(define (print-jit-op term)
  (define (enum->proc-name str)
    (regexp-substitute/global #f "-code" str 'pre "" 'post))
  (define (c-args->scm-args args)
    (if (pair? (caaddr args))
        (let lp ((args (car (cddr args)))
                 (acc '()))
          (if (null? args)
              (string-join (reverse acc) " ")
              (let ((arg-name (cadr (car args))))
                (if (string=? "NULL" arg-name)
                    (lp (cdr args) acc)
                    (lp (cdr args) (cons arg-name acc))))))
        (let ((arg-name (cadr (caddr args))))
          (if (string=? "NULL" arg-name) "" arg-name))))
  (define (c-args->scm-vars args)
    (if (pair? (caaddr args))
        (string-join
         (map (lambda (pair)
                (let ((arg-name (cadr pair)))
                  (if (string=? "NULL" arg-name)
                      "%null-pointer"
                      arg-name)))
              (caddr args))
         " ")
        (let ((name (cadr (caddr args))))
          (if (string=? "NULL" name) "%null-pointer" name))))
  (let* ((new-node-c-name (cadr term))
         (new-node-s-name (under-score->hyphen new-node-c-name))
         (args-list (car (cddr term)))
         (args (c-args->scm-args args-list))
         (vars (c-args->scm-vars args-list))
         (enum-code-str (under-score->hyphen (cadr (cadr args-list))))
         (scm-name (enum->proc-name enum-code-str)))
    (format #t "(define (~a ~a)~%  (~a ~a ~a))~%~%"
            scm-name args new-node-s-name enum-code-str vars)))

(define (print-procs peg-tree)
  (let lp ((lst (cdr peg-tree)) (acc '()))
    (if (null? lst)
        (reverse acc)
        (let ((l (car lst))
              (rest (cdr lst)))
          (if (eq? 'cmacro (car l))
              (begin
                (let ((pt (match-pattern code (car (cdr l)))))
                  (and pt (print-jit-op (peg:tree pt))))
                (lp rest (cons (cdr l) acc)))
              (lp rest acc))))))

(define w64
  '((jit-getarg . jit-getarg-l)
    (jit-htonr . jit-htonr-ul)
    (jit-ntohr . jit-htonr-ul)
    (jit-ldr . jit-ldr-l)
    (jit-ldi . jit-ldi-l)
    (jit-ldxr . jit-ldxr-l)
    (jit-ldxi . jit-ldxi-l)
    (jit-str . jit-str-l)
    (jit-sti . jit-sti-l)
    (jit-stxr . jit-stxr-l)
    (jit-stxi . jit-stxi-l)
    (jit-retval . jit-retval-l)
    (jit-truncr-f . jit-truncr-f-l)
    (jit-truncr-d . jit-truncr-d-l)))

(define w32
  '((jit-getarg . jit-getarg-i)
    (jit-htonr . jit-htonr-ui)
    (jit-ntohr . jit-htonr-ui)
    (jit-ldr . jit-ldr-i)
    (jit-ldi . jit-ldi-i)
    (jit-ldxr . jit-ldxr-i)
    (jit-ldxi . jit-ldxi-i)
    (jit-str . jit-str-i)
    (jit-sti . jit-sti-i)
    (jit-stxr . jit-stxr-i)
    (jit-stxi . jit-stxi-i)
    (jit-retval . jit-retval-i)
    (jit-truncr-f . jit-truncr-f-i)
    (jit-truncr-d . jit-truncr-d-i)))

(define (print-word-specific)
  (for-each
   (lambda (pair)
     (format #t "(define ~a ~a)~%" (car pair) (cdr pair)))
   (case (sizeof '*)
     ((4) w32)
     ((8) w64)
     (else (format (current-warning-port)
                   "Unknown word size: ~a~%"
                   (sizeof '*))))))

;;;
;;; Printing out
;;;

(define (main)
  (let* ((path (lightning-header-path))
         (tree (enum-peg-tree path)))
    ;; (pretty-print tree (current-warning-port))

    (display ";;;; File: jit-code-t.scm\n")
    (display ";;;; \n")
    (format #t ";;;; Generated from \"~a\"~%" path)
    (newline)

    (display ";;;\n")
    (display ";;; Enum values of jit_code_t\n")
    (display ";;;\n")
    (newline)
    (print-enums tree)
    (newline)
    (newline)

    (display ";;;\n")
    (display ";;; C macro -> Scheme procedure\n")
    (display ";;;\n")
    (newline)
    (print-procs tree)
    (newline)

    (display ";;;\n")
    (display ";;; Word size specific aliases\n")
    (display ";;;\n")
    (newline)
    (print-word-specific)
    (newline)

    (display ";;; EOF")))

(main)

;;; gen-code-t.scm ends here
