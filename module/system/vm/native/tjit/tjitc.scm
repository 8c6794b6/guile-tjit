;;;; Entry point for compiler used in vm-tjit engine

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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
;;; Module exporting @code{tjitc}, entry point of just-in-time compiler
;;; for `vm-tjit' engine.
;;;
;;; Code:

(define-module (system vm native tjit tjitc)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (language cps)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit compile-native)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit compile-ir)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit state)
  #:use-module (system vm native tjit variables)
  #:export (tjitc init-vm-tjit)
  #:re-export (tjit-stats))


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
  (let ((lowest (lowest-level ip-x-ops)))
    (format #t ";;; trace ~a: bytecode ~a:~a~%"
            trace-id (length ip-x-ops) lowest)
    (let lp ((traces ip-x-ops) (level (- lowest)))
      (match traces
        (((op ip ra locals) . traces)
         (let ((op-val (format #f "~x  ~a~a" ip (make-indent level) op)))
           (if (tjit-dump-locals? (tjit-dump-option))
               (format #t "~48a; ~a~%" op-val locals)
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
        %eq %ne %lt %ge
        %flt %fge)
       "  >")
      (else
       "   ")))
  (define (pretty-locals locals)
    (if (null? locals)
        "--"
        (map (match-lambda ((n . t)
                            (list n (pretty-type t))))
             locals)))
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
               (pretty-locals locals)))
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

(define (dump-disassembler trace-id entry-ip code code-size adjust
                           loop-address snapshots trampoline)
  (format #t ";;; trace ~a: disassembly~%" trace-id)
  ((tjit-disassembler) trace-id entry-ip code code-size adjust
   loop-address snapshots trampoline))


;;;
;;; Auxiliary
;;;

(define *skipped-modules*
  ;; XXX: Workaround for segfault happening with fresh compile.
  ;;
  ;; Ideally, the modules listed here should not skipped native code
  ;; compilation, though the segfault were bothering too much. When
  ;; tracing JIT compiler get more stable, the list of skipped modules
  ;; should be removed.
  ;;
  '((system vm assembler)
    (system vm linker)))

(define (traced-ops bytecode traces sp-offset fp-offset)
  (define disassemble-one
    (@@ (system vm disassembler) disassemble-one))
  (let lp ((acc '())
           (offset 0)
           (traces (reverse! traces))
           (st (make-hash-table))
           (sp-offset sp-offset)
           (fp-offset fp-offset)
           (sp-offsets '())
           (fp-offsets '()))
    (match traces
      ((trace . traces)
       (let*-values (((len op) (disassemble-one bytecode offset))
                     ((sp-offset fp-offset sp-offsets fp-offsets)
                      (scan-locals st
                                   sp-offset fp-offset sp-offsets fp-offsets
                                   op (caddr trace))))
         (lp (cons (cons op trace) acc)
             (+ offset len)
             traces
             st
             sp-offset
             fp-offset
             sp-offsets
             fp-offsets)))
      (()
       (let ((local-indices (sort (hash-fold (lambda (k v acc)
                                               (cons k acc))
                                             '()
                                             st)
                                  >))
             (sp-offsets/vec (list->vector (reverse! sp-offsets)))
             (fp-offsets/vec (list->vector (reverse! fp-offsets))))
         (debug 1 ";;; local-indices: ~a~%" local-indices)
         (values (reverse! acc)
                 (make-past-frame '()
                                  '()
                                  sp-offset
                                  #()
                                  local-indices
                                  sp-offsets/vec
                                  fp-offsets/vec)))))))


;;;
;;; Entry point
;;;

;; This procedure is called from C code in "libguile/vm-tjit.c".
(define (tjitc trace-id bytecode traces parent-ip parent-exit-id linked-ip
               loop? downrec? uprec?)
  (define (module-to-skip? ip)
    (string-match
     (string-append "("
                    (string-join
                     (map (lambda (m)
                            (module-filename (resolve-module m)))
                          *skipped-modules*)
                     "|")
                    ")")
     (or (let ((src (find-source-for-addr ip)))
           (and src (source-file src)))
         "")))
  (define-syntax-rule (increment-compilation-failure ip)
    (let ((count (hashq-ref (tjit-failed-ip-table) ip 0)))
      (hashq-set! (tjit-failed-ip-table) ip (+ count 1))))
  (define-syntax-rule (show-one-line sline fragment)
    (let ((exit-pair (if (< 0 parent-ip)
                         (format #f " (~a:~a)"
                                 (or (and fragment (fragment-id fragment))
                                     (format #f "~x" parent-ip))
                                 parent-exit-id)
                         ""))
          (linked-id (if loop?
                         ""
                         (format #f " -> ~a"
                                 (fragment-id (get-root-trace linked-ip))))))
      (format #t ";;; trace ~a: ~a~a~a~a~%"
              trace-id sline exit-pair linked-id
              (cond
               (downrec? " - downrec")
               (uprec? " - uprec")
               (else "")))))

  ;; XXX: Workaround for freed SCM values during compilation.
  ;;
  ;; Better to explicitly specify the SCM values want to preserved from garbage
  ;; collection during compilation. At the moment, GC is disabled here and
  ;; enabled at the end of this procedure.
  ;;
  (gc-disable)

  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (make-tjit-time-log (get-internal-run-time) 0 0 0 0)))
      (put-tjit-time-log! trace-id log)))

  (let* ((parent-fragment (get-fragment parent-ip))
         (parent-snapshot (if parent-fragment
                              (hashq-ref (fragment-snapshots parent-fragment)
                                         parent-exit-id)
                              #f)))
    (let-values (((traces past-frame)
                  (catch #t
                    (lambda ()
                      (traced-ops bytecode
                                  traces
                                  (get-initial-sp-offset parent-snapshot)
                                  (get-initial-fp-offset parent-snapshot)))
                    (lambda msgs
                      (debug 1 ";;; ~s~%" msgs)
                      (values '() #f)))))
      (let* ((entry-ip (if (null? traces)
                           0
                           (cadr (car traces))))
             (verbosity (lightning-verbosity))
             (sline (addr->source-line entry-ip))
             (dump-option (tjit-dump-option)))
        (cond
         ((module-to-skip? entry-ip)
          ;; XXX: Workaround for modules causing segfault at the time of
          ;; bytecode compilation with `vm-tjit'.
          (debug 1 ";;; trace ~a: IP found in skipped modules ~%" trace-id)
          (increment-compilation-failure entry-ip))
         ((null? traces)
          (debug 1 ";;; trace ~a: error in disassembly~%" trace-id))
         (else
          (let ((tj (make-tj trace-id
                             entry-ip
                             linked-ip
                             parent-exit-id
                             parent-fragment
                             parent-snapshot
                             past-frame
                             loop?
                             downrec?
                             uprec?)))
            (let-values (((snapshots anf ops)
                          (compile-primops tj traces)))
              (define-syntax dump
                (syntax-rules ()
                  ((_ test exp)
                   (when (and (test dump-option)
                              (or ops
                                  (tjit-dump-abort? dump-option)))
                     exp))))
              (dump tjit-dump-jitc? (show-one-line sline parent-fragment))
              (dump tjit-dump-bytecode? (dump-bytecode trace-id traces))
              (dump tjit-dump-anf? (dump-anf trace-id anf))
              (dump tjit-dump-ops? (dump-primops trace-id ops snapshots))
              (cond
               ((not ops)
                (debug 1 ";;; trace ~a: aborted~%" trace-id)
                (increment-compilation-failure entry-ip))
               (uprec?
                (debug 1 ";;; trace ~a: NYI up recursion~%" trace-id)
                (increment-compilation-failure entry-ip))
               (else
                (let-values (((code size adjust loop-address trampoline)
                              (compile-native tj ops snapshots)))
                  (when (tjit-dump-disassemble? dump-option)
                    (dump-disassembler trace-id
                                       entry-ip
                                       code
                                       size
                                       adjust
                                       loop-address
                                       snapshots
                                       trampoline)))))
              (when (tjit-dump-time? dump-option)
                (let ((log (get-tjit-time-log trace-id))
                      (t (get-internal-run-time)))
                  (set-tjit-time-log-end! log t))))))))))
  (gc-enable))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")