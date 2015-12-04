;;;; Module containing procedure to dump various TJIT data

;;;; Copyright (C) 2015 Free Software Foundation, Inc.
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
;;; Module containing procedures used for dumping ANF, primitive operations,
;;; source lines, ... etc.
;;;
;;; Code:

(define-module (system vm native tjit dump)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
  #:export (addr->source-line
            dump-bytecode
            dump-anf
            dump-primops
            dump-ncode))

;;;
;;; Dump procedures
;;;

(define-syntax-rule (addr->source-line addr)
  (cond
   ((find-source-for-addr addr)
    => (lambda (source)
         (format #f "~a:~d"
                 (let ((file (source-file source)))
                   (or (and (string? file) (basename file))
                       "(unknown file)"))
                 (source-line-for-user source))))
   (else
    "(invalid IP)")))

(define (dump-bytecode trace-id ip-x-ops)
  (define (lowest-level ip-x-ops)
    (let lp ((ip-x-ops ip-x-ops) (level 0) (lowest 0))
      (match ip-x-ops
        (((op . _) . ip-x-ops)
         (case (car op)
           ((call call-label)
            (lp ip-x-ops (+ level 1) lowest))
           ((return return-values subr-call foreign-call)
            (let ((level (- level 1)))
              (lp ip-x-ops level (min level lowest))))
           (else
            (lp ip-x-ops level lowest))))
        (() lowest))))
  (define (make-indent n)
    (let lp ((n n) (acc '()))
      (if (< 0 n)
          (lp (- n 1) (cons #\. (cons #\space acc)))
          (list->string acc))))
  (define (as-address locals)
    (let ((end (vector-length locals)))
      (let lp ((i 0) (v (make-vector end)))
        (if (< i end)
            (let* ((ptr (vector-ref locals i)))
              (vector-set! v i (format #f "#x~x" (pointer-address ptr)))
              (lp (+ i 1) v))
            v))))
  (let ((lowest (lowest-level ip-x-ops)))
    (format #t ";;; trace ~a: bytecode ~a:~a~%"
            trace-id (length ip-x-ops) lowest)
    (let lp ((traces ip-x-ops) (level (- lowest)))
      (match traces
        (((op ip ra locals) . traces)
         (let ((op-val (format #f "~x  ~a~a" ip (make-indent level) op)))
           (if (tjit-dump-locals? (tjit-dump-option))
               (format #t "~48a; ~a~%" op-val (as-address locals))
               (format #t "~a~%" op-val)))
         (case (car op)
           ((call call-label)
            (lp traces (+ level 1)))
           ((return return-values subr-call foreign-call)
            (lp traces (- level 1)))
           (else
            (lp traces level))))
        (() (values))))))

(define (dump-anf trace-id scm)
  (define (dump-let args term)
    (format #t "(lambda ~a~%  (let* (" args)
    (let lp ((term term))
      (match term
        (('let ((var exp))
           '_)
         (format #t "(~4a ~a))~%    _))~%" var exp))
        (('let ((var exp))
           ('loop . args))
         (format #t "(~4a ~a))~%    ~a))~%" var exp (cons 'loop args)))
        (('let ((var exp))
           next-term)
         (format #t "(~4a ~a)~%         " var exp)
         (lp next-term)))))
  (match scm
    (`(letrec ((entry (lambda ,entry-args ,entry))
               (loop (lambda ,loop-args ,loop)))
        entry)
     (format #t ";;; trace ~a: anf~%" trace-id)
     (dump-let entry-args entry)
     (dump-let loop-args loop))
    (`(letrec ((patch (lambda ,patch-args ,patch)))
        patch)
     (format #t ";;; trace ~a: anf~%" trace-id)
     (dump-let patch-args patch))
    (_
     (values))))

(define (dump-primops trace-id plist snapshots)
  (define (mark-op op)
    (case (car op)
      ((%return
        %fref %fref/f
        %eq %ne %lt %le %ge %gt
        %flt %fge)
       "  >")
      (else
       "   ")))
  (define (pretty-locals locals variables)
    (if (null? locals)
        "--"
        (let lp ((locals locals)
                 (vars variables)
                 (acc '()))
          (match (list locals vars)
            ((((n . t) . locals) (v . vars))
             (let* ((elem
                     (if (tjit-dump-locals? (tjit-dump-option))
                         (list n (pretty-type t) (pretty-register v))
                         (list n (pretty-type t)))))
               (lp locals vars (cons elem acc))))
            (_
             (reverse! acc))))))
  (define (pretty-constant arg)
    (if (and (pair? arg)
             (eq? 'const (car arg)))
        (let ((x (cdr arg)))
          (cond
           ((exact-integer? x)
            (blue (if (<= 0 x)
                      (string-append "+" (number->string x))
                      (number->string x))))
           ((false? x)
            (green "#f"))
           ((null? x)
            (green "()"))
           ((undefined? x)
            (green "#<undefined>"))
           ((unspecified? x)
            (green "#<unspecified>"))
           (else
            arg)))
        arg))
  (define (pretty-register arg)
    (cond
     ((or (gpr? arg) (fpr? arg))
      (physical-name arg))
     ((memory? arg)
      (format #f "[@ 0x~x]"
              (+ (- #xffffffffffffffff
                    (pointer-address (spilled-offset arg)))
                 1)))
     ((constant? arg)
      (pretty-constant arg))
     (else
      arg)))
  (define (dump-snapshot snapshot)
    (match snapshot
      (($ $snapshot id sp-offset fp-offset nlocals locals variables)
       (format #t "----     [snap~3,,,' @a] ~a:~a:~a ~a~%"
               id sp-offset fp-offset nlocals
               (pretty-locals locals variables)))
      (_
       (format #t "----     NOT-A-SNAPSHOT~%"))))
  (define (dump-one idx op)
    (match op
      (('%snap id . _)
       (dump-snapshot (hashq-ref snapshots id)))
      (_
       (let ((mark (mark-op op)))
         (match op
           (('%fref dst n type)
            (format #t "~4,,,'0@a ~a (~7a ~a ~a ~a)~%" idx mark
                    '%fref
                    (pretty-register dst)
                    (pretty-constant n)
                    (pretty-type (cdr type))))
           (('%fref/f dst n type)
            (format #t "~4,,,'0@a ~a (~7a ~a ~a ~a)~%" idx mark
                    '%fref/f
                    (pretty-register dst)
                    (pretty-constant n)
                    (pretty-type (cdr type))))
           (('%return (const . addr))
            (format #t "~4,,,'0@a ~a (~7a ~a/~a)~%" idx mark
                    '%return
                    (cyan (number->string addr 16))
                    (addr->source-line addr)))
           (_
            (format #t "~4,,,'0@a ~a (~7a ~{~a~^ ~})~%" idx mark
                    (car op)
                    (map pretty-register (cdr op)))))))))
  (define (dump-list idx ops)
    (let lp ((ops ops) (idx idx))
      (match ops
        ((op . ops)
         (dump-one idx op)
         (lp ops (+ idx 1)))
        (()
         idx))))
  (match plist
    (($ $primops entry loop)
     (format #t ";;; trace ~a: primops:~%" trace-id)
     (let ((idx (dump-list 0 entry)))
       (when (not (null? loop))
         (format #t "==== loop:~%")
         (dump-list idx loop)
         (format #t "==== ->loop~%"))))
    (_
     (format #t ";;; primops: ~a~%" plist))))

(define (dump-ncode trace-id entry-ip code code-size adjust
                           loop-address snapshots trampoline)
  (format #t ";;; trace ~a: ncode~%" trace-id)
  ((tjit-disassembler) trace-id entry-ip code code-size adjust
   loop-address snapshots trampoline))
