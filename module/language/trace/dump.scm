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
;;; Module containing procedures used for dumping various information, such as
;;; ANF term, primitive operations, source lines, environment data ... etc.
;;;
;;; Code:

(define-module (language trace dump)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (language trace assembler)
  #:use-module (language trace fragment)
  #:use-module (language trace env)
  #:use-module (language trace parameters)
  #:use-module (language trace ra)
  #:use-module (language trace registers)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables)
  #:use-module (system vm vm)
  #:export (addr->source-line
            tjit-stats
            dump-bytecode
            dump-anf
            dump-primops
            dump-ncode
            dump-sline
            dump-tjit-stats
            dump-fragment
            dump-env))


;;;; Auxiliary

(define (car-< a b)
  (< (car a) (car b)))


;;;; Statistics

(define (tjit-stats)
  "Returns statistics of vm-tjit engine.

Statistical times will be constantly @code{#f} unless @code{tjit-dump-time?}
option was set to true."
  (let* ((hot-loop (tjit-hot-loop))
         (hot-exit (tjit-hot-exit))
         (dump-time (tjit-dump-time? (tjit-dump-option)))
         (total-time (if dump-time 0 #f))
         (init-time (if dump-time 0 #f))
         (anf-time (if dump-time 0 #f))
         (ops-time (if dump-time 0 #f))
         (asm-time (if dump-time 0 #f))
         (bailout-time (if dump-time 0 #f))
         (num-fragments (hash-count (const #t) (tjit-fragment))))
    (when dump-time
      (fold-tjit-time-logs
       (lambda (k v acc)
         (match (diff-tjit-time-log v)
           ((t i s c a b)
            (set! total-time (+ total-time t))
            (set! init-time (+ init-time i))
            (set! anf-time (+ anf-time s))
            (set! ops-time (+ ops-time c))
            (set! asm-time (+ asm-time a))
            (set! bailout-time (+ bailout-time b)))))
       #f))
    (list `(hot-loop . ,hot-loop)
          `(hot-exit . ,hot-exit)
          `(num-fragments . ,num-fragments)
          `(init-time . ,init-time)
          `(anf-time . ,anf-time)
          `(ops-time . ,ops-time)
          `(asm-time . ,asm-time)
          `(bailout-time . ,bailout-time)
          `(total-time . ,total-time))))


;;;; Dump procedures

(define-syntax-rule (addr->source-line addr)
  (or (and=> (find-source-for-addr addr)
             (lambda (source)
               (cons (let ((file (source-file source)))
                       (or (and (string? file) file)
                           "(unknown file)"))
                     (source-line-for-user source))))
      (cons "(unknown source)" #f)))

(define (symbolize-op op)
  (match op
    (((? number? op) . rest)
     (cons (prim-names-ref op) rest))
    (_ op)))

(define (dump-bytecode port trace-id ip-x-ops)
  (define (lowest-level ip-x-ops)
    (let lp ((ip-x-ops ip-x-ops) (level 0) (lowest 0))
      (match ip-x-ops
        ((#(op ip ra dl locals) . ip-x-ops)
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
            (let ((addr (object-address (vector-ref locals i))))
              (vector-set! v i (format #f "#x~x" addr))
              (lp (+ i 1) v))
            v))))
  (let ((lowest (lowest-level ip-x-ops)))
    (format port ";;; trace ~a: bytecode ~a~%" trace-id (length ip-x-ops))
    (let lp ((traces ip-x-ops) (level (- lowest)))
      (match traces
        ((#(op ip ra dl locals) . traces)
         (let ((op-val (format #f "~12,'0x  ~a~a" ip (make-indent level) op)))
           (if (tjit-dump-verbose? (tjit-dump-option))
               (format port "~48a; ~a~%" op-val (as-address locals))
               (format port "~a~%" op-val)))
         (case (car op)
           ((call call-label)
            (lp traces (+ level 1)))
           ((return return-values subr-call foreign-call)
            (lp traces (- level 1)))
           (else
            (lp traces level))))
        (() (values))))))

(define (dump-anf port trace-id scm)
  (define (dump-let args term)
    (format port "(lambda ~a~%  (let* (" args)
    (let lp ((term term))
      (match term
        (('let ((var exp)) '_)
         (format port "(~4a ~a))~%    _))~%" var (symbolize-op exp)))
        (('let ((var exp)) ('loop . args))
         (format port "(~4a ~a))~%    ~a))~%"
                 var (symbolize-op exp) (cons 'loop args)))
        (('let ((var exp)) next-term)
         (format port "(~4a ~a)~%         " var (symbolize-op exp))
         (lp next-term)))))
  (match scm
    (`(letrec ((entry (lambda ,entry-args ,entry))
               (loop (lambda ,loop-args ,loop)))
        entry)
     (format port ";;; trace ~a: anf~%" trace-id)
     (dump-let entry-args entry)
     (dump-let loop-args loop))
    (`(letrec ((patch (lambda ,patch-args ,patch)))
        patch)
     (format port ";;; trace ~a: anf~%" trace-id)
     (dump-let patch-args patch))
    (_
     (values))))

(define (dump-primops port trace-id plist snapshots)
  (define (mark-op op)
    (case (car op)
      ((%return %sref/f %eq %ne %lt %le %ge %gt %flt %fle %fgt %fge
                %eqv %nev %typeq %tceq %tcne %addov %subov)
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
                     (if (tjit-dump-verbose? (tjit-dump-option))
                         (list n (pretty-type t) (pretty-storage v))
                         (list n (pretty-type t)))))
               (lp locals vars (cons elem acc))))
            (_
             (reverse! acc))))))
  (define (pretty-constant arg)
    (if (and (ref? arg)
             (con? arg))
        (let ((x (cdr arg)))
          (cond
           ((exact-integer? x)
            (blue (if (<= 0 x)
                      (string-append "+" (number->string x))
                      (number->string x))))
           ((flonum? x) (magenta (number->string x)))
           ((false? x) (green "false"))
           ((eq? x #t) (green "true"))
           ((null? x) (green "()"))
           ((undefined? x) (green "<undefined>"))
           ((unspecified? x) (green "<unspecified>"))
           (else arg)))
        arg))
  (define (pretty-storage arg)
    (cond
     ((not (pair? arg))
      arg)
     ((or (gpr? arg) (fpr? arg) (memory? arg))
      (physical-name arg))
     ((con? arg)
      (pretty-constant arg))
     (else
      arg)))
  (define (dump-snapshot snapshot)
    (match snapshot
      (($ $snapshot id sp-offset fp-offset nlocals locals variables
          code ip live-indices inline-depth)
       (format port "----     [snap~3,,,' @a] ~a:~a:~a:~a ~a~%"
               id sp-offset fp-offset nlocals inline-depth
               (if (tjit-dump-snapshot? (tjit-dump-option))
                   (pretty-locals locals variables)
                   "")))
      (_
       (format port "----     NOT-A-SNAPSHOT~%"))))

  (define (dump-one idx op)
    (match op
      (('%snap id . _)
       (dump-snapshot (snapshots-ref snapshots id)))
      (_
       (let* ((op (symbolize-op op))
              (mark (mark-op op)))
         (match op
           (('%sref dst n type)
            (format port "~4,,,'0@a ~a (~8a ~a ~a ~a)~%" idx mark
                    '%sref
                    (pretty-storage dst)
                    (pretty-constant n)
                    (if (cdr type) (pretty-type (cdr type)) "---")))
           (('%sref/f dst n type)
            (format port "~4,,,'0@a ~a (~8a ~a ~a ~a)~%" idx mark
                    '%sref/f
                    (pretty-storage dst)
                    (pretty-constant n)
                    (pretty-type (cdr type))))
           (('%typeq src type)
            (format port "~4,,,'0@a ~a (~8a ~a ~a)~%" idx mark
                    '%typeq
                    (pretty-storage src)
                    (pretty-type (cdr type))))
           (('%return (const . ra))
            (let ((sinfo (addr->source-line ra)))
              (format port "~4,,,'0@a ~a (~8a ~a/~a:~a)~%" idx mark
                      '%return
                      (cyan (number->string ra 16))
                      (basename (car sinfo))
                      (cdr sinfo))))
           (('%ccall dst (const . addr))
            (format port "~4,,,'0@a ~a (~8a ~a ~a:0x~x)~%" idx mark
                    '%ccall
                    (pretty-storage dst)
                    (let ((proc (pointer->scm (make-pointer addr))))
                      (and (procedure? proc)
                           (cyan (symbol->string (procedure-name proc)))))
                    (let ((proc (pointer->scm (make-pointer addr))))
                      (and (procedure? proc)
                           (pointer-address
                            (program-free-variable-ref proc 0))))))
           (_
            (format port "~4,,,'0@a ~a (~8a ~{~a~^ ~})~%" idx mark
                    (car op)
                    (map pretty-storage (cdr op)))))))))
  (define (dump-list idx ops)
    (let lp ((ops ops) (idx idx))
      (match ops
        ((op . ops)
         (dump-one idx op)
         (lp ops (+ idx 1)))
        (() idx))))
  (match plist
    (($ $primops entry loop)
     (format port ";;; trace ~a: primops (~a snapshots):~%" trace-id
             (hash-count (const #t) snapshots))
     (let ((idx (dump-list 0 entry)))
       (when (not (null? loop))
         (format port "==== loop:~%")
         (dump-list idx loop)
         (format port "==== ->loop~%"))))
    (_
     (format port ";;; primops: ~a~%" plist))))

(define (dump-ncode port trace-id entry-ip code code-size adjust
                    loop-address snapshots trampoline root?)
  (format port ";;; trace ~a: ncode ~a~%" trace-id code-size)
  ((tjit-disassembler) port trace-id entry-ip code code-size adjust
   loop-address snapshots trampoline root?))

(define (dump-sline port sline trace-id loop? downrec? uprec?
                    parent-ip parent-exit-id parent-snapshot
                    parent-fragment linked-fragment)
  (let ((exit-pair (if parent-ip
                       (format #f " (~a:~a)"
                               (fragment-id parent-fragment)
                               parent-exit-id)
                       ""))
        (linked-id (if (or parent-snapshot (not loop?))
                       (format #f " -> ~a"
                               (fragment-id linked-fragment))
                       ""))
        (ttype (cond
                ((not loop?) "")
                (downrec? " - downrec")
                (uprec? " - uprec")
                (else ""))))
    (format port ";;; trace ~a: ~a:~a~a~a~a~%"
            trace-id (car sline) (cdr sline) exit-pair
            linked-id ttype)))

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

(define (dump-env env)
  (define (sort-types field)
    (sort (map (match-lambda
                 ((k . t) `(,k . ,(pretty-type t))))
               (field env))
          car-<))
  (define (highlight-call-return n-getter alst-getter)
    (let ((n (n-getter env)))
      (map (match-lambda
             (((? (lambda (x) (= n x)) k) . v)
              (cons (yellow (number->string k)) v))
             (other other))
           (alst-getter env))))
  (format #t ";;; env:~%")
  (format #t "~{;;;  ~a~%~}"
          `((read-indices . ,(env-read-indices env))
            (live-indices . ,(sort (env-live-indices env) <))
            (write-indices . ,(env-write-indices env))
            (entry  . ,(sort-types env-entry-types))
            (inferred . ,(sort-types env-inferred-types))
            (call-num ,(env-call-num env))
            (return-num ,(env-return-num env))
            (calls . ,(highlight-call-return env-call-num env-calls))
            (returns . ,(highlight-call-return env-return-num env-returns))
            (inline-depth ,(env-inline-depth env)))))
