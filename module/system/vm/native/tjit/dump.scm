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
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables)
  #:use-module (system vm vm)
  #:export (addr->source-line
            tjit-stats
            dump-bytecode
            dump-anf
            dump-primops
            dump-ncode
            dump-tjit-stats
            dump-fragment
            dump-outline))

;;;
;;; Statistics
;;;

(define (tjit-stats)
  "Returns statistics of vm-tjit engine.

Statistical times will be constantly @code{#f} unless @code{tjit-dump-time?}
option was set to true."
  (let* ((hot-loop (tjit-hot-loop))
         (hot-call (tjit-hot-call))
         (hot-exit (tjit-hot-exit))
         (dump-time (tjit-dump-time? (tjit-dump-option)))
         (total-time (if dump-time 0 #f))
         (init-time (if dump-time 0 #f))
         (scm-time (if dump-time 0 #f))
         (ops-time (if dump-time 0 #f))
         (asm-time (if dump-time 0 #f))
         (num-fragments (hash-count (const #t) (tjit-fragment))))
    (when dump-time
      (fold-tjit-time-logs
       (lambda (k v acc)
         (match (diff-tjit-time-log v)
           ((t i s c a)
            (set! total-time (+ total-time t))
            (set! init-time (+ init-time i))
            (set! scm-time (+ scm-time s))
            (set! ops-time (+ ops-time c))
            (set! asm-time (+ asm-time a)))))
       #f))
    (list `(hot-loop . ,hot-loop)
          `(hot-call . ,hot-call)
          `(hot-exit . ,hot-exit)
          `(num-fragments . ,num-fragments)
          `(init-time . ,init-time)
          `(scm-time . ,scm-time)
          `(ops-time . ,ops-time)
          `(asm-time . ,asm-time)
          `(total-time . ,total-time))))

;;;
;;; Auxiliary
;;;

(define (car-< a b)
  (< (car a) (car b)))

;;;
;;; Dump procedures
;;;

(define-syntax-rule (addr->source-line addr)
  (cond
   ((find-source-for-addr addr)
    => (lambda (source)
         (cons (let ((file (source-file source)))
                 (or (and (string? file) file)
                     "(unknown file)"))
               (source-line-for-user source))))
   (else
    (cons "(unknown source)" #f))))

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
        (((op ip ra dl locals) . traces)
         (let ((op-val (format #f "~12,'0x  ~a~a" ip (make-indent level) op)))
           (if (tjit-dump-verbose? (tjit-dump-option))
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
      ((%return %fref/f %eq %ne %lt %le %ge %gt %flt %fge)
       "  >")
      ((%fref)
       (if (cdr (cadddr op)) "  >" "   "))
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
                     (if (tjit-dump-verbose? (tjit-dump-option))
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
           ((flonum? x) (magenta (number->string x)))
           ((false? x) (green "#f"))
           ((eq? x #t) (green "#t"))
           ((null? x) (green "()"))
           ((undefined? x) (green "#<undefined>"))
           ((unspecified? x) (green "#<unspecified>"))
           (else arg)))
        arg))
  (define (pretty-register arg)
    (cond
     ((or (gpr? arg) (fpr? arg) (memory? arg))
      (physical-name arg))
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
                    (if (cdr type) (pretty-type (cdr type)) "---")))
           (('%fref/f dst n type)
            (format #t "~4,,,'0@a ~a (~7a ~a ~a ~a)~%" idx mark
                    '%fref/f
                    (pretty-register dst)
                    (pretty-constant n)
                    (pretty-type (cdr type))))
           (('%return (const . ra))
            (let ((sinfo (addr->source-line ra)))
              (format #t "~4,,,'0@a ~a (~7a ~a/~a:~a)~%" idx mark
                      '%return
                      (cyan (number->string ra 16))
                      (basename (car sinfo))
                      (cdr sinfo))))
           (('%ccall dst (const . addr))
            (format #t "~4,,,'0@a ~a (~7a ~a ~a:0x~x)~%" idx mark
                    '%ccall
                    (pretty-register dst)
                    (let ((proc (pointer->scm (make-pointer addr))))
                      (and (procedure? proc)
                           (cyan (symbol->string (procedure-name proc)))))
                    (let ((proc (pointer->scm (make-pointer addr))))
                      (and (procedure? proc)
                           (pointer-address
                            (program-free-variable-ref proc 0))))))
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
                    loop-address snapshots trampoline root?)
  (format #t ";;; trace ~a: ncode~%" trace-id)
  ((tjit-disassembler) trace-id entry-ip code code-size adjust
   loop-address snapshots trampoline root?))

(define (dump-tjit-stats)
  (if (eq? 'tjit (vm-engine))
      (for-each
       (lambda (kv)
         (format #t "~16@a: ~a~%" (car kv) (cdr kv)))
       (tjit-stats))
      (display "not running with `vm-tjit' engine.\n")))

(define (dump-fragment fragment)
  (format #t "~20@a~a~%" "*****" " fragment *****")
  (format #t "~19@a: ~a~%" 'id (fragment-id fragment))
  (format #t "~19@a: addr=~a size=~a~%" 'code
          (let ((code (fragment-code fragment)))
            (and (bytevector? code)
                 (bytevector->pointer code)))
          (bytevector-length (fragment-code fragment)))
  (format #t "~19@a: ~{~a ~}~%" 'exit-counts
          (reverse! (hash-fold acons '() (fragment-exit-counts fragment))))
  (format #t "~19@a: ~x~%" 'entry-ip (fragment-entry-ip fragment))
  (format #t "~19@a: ~a~%" 'parent-id (fragment-parent-id fragment))
  (format #t "~19@a: ~a~%" 'parent-exit-id (fragment-parent-exit-id fragment))
  (format #t "~19@a:~%" 'snapshots)
  (for-each
   (match-lambda
     ((i . ($ $snapshot id sp-offset fp-offset nlocals locals variables code ip))
      (format #t "~13@a: id=~a sp-offset=~a fp-offset=~a nlocals=~a locals=~a"
              i id sp-offset fp-offset nlocals locals)
      (format #t " variables=~a code=~a ip=~a~%"
              variables (and (bytevector? code) (bytevector->pointer code)) ip)))
   (sort (hash-fold acons '() (fragment-snapshots fragment)) car-<))
  (let ((code (fragment-trampoline fragment)))
    (format #t "~19@a: ~a:~a~%" 'trampoline
            (and (bytevector? code) (bytevector->pointer code))
            (and (bytevector? code) (bytevector-length code))))
  (format #t "~19@a: ~a~%" 'loop-address (fragment-loop-address fragment))
  (format #t "~19@a: ~a~%" 'loop-locals (fragment-loop-locals fragment))
  (format #t "~19@a: ~a~%" 'loop-vars (fragment-loop-vars fragment))
  (format #t "~19@a: ~a~%" 'end-address (fragment-end-address fragment)))

(define (dump-outline outline)
  (format #t ";;; outline:~%")
  (format #t "~{;;;  ~a~%~}"
          `((local-indices . ,(outline-local-indices outline))
            (types . ,(sort (outline-types outline) car-<))
            (read-indices . ,(outline-read-indices outline))
            (live-indices . ,(outline-live-indices outline))
            (write-indices . ,(outline-write-indices outline))
            (expected . ,(sort (map (match-lambda
                                      ((k . t)
                                       `(,k . ,(pretty-type t))))
                                    (outline-expected-types outline))
                               car-<))
            (inferred . ,(sort (map (match-lambda
                                      ((k . t)
                                       `(,k . ,(pretty-type t))))
                                    (outline-inferred-types outline))
                               car-<)))))
