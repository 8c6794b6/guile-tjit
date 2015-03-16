;;; -*- mode: scheme; coding: utf-8; -*-

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

;;; Traces bytecode, to get intermediate representation for compilation.

;;; Code:

(define-module (system vm lightning trace)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language bytecode)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm disassembler)
  #:use-module (system vm lightning debug)
  #:use-module (system vm program)
  #:autoload (system vm lightning) (call-lightning)
  #:export (program->trace
            make-trace trace? trace-ip trace-name trace-nargs
            trace-free-vars trace-ops trace-labeled-ips
            trace-callers trace-callee-args
            trace-locals trace-nretvals

            make-call call? call-program call-args

            make-closure closure? closure-addr closure-free-vars

            builtin? builtin-name
            unknown?

            program->entries entries-ops entries-table
            program->cfg dump-cfg

            ensure-program-addr))

;;;
;;; Sparse vector
;;;

;; Data type with interface resembling to vector, but with
;; implementation using hash table.
(define-record-type <sparse-vector>
  (%make-sparse-vector table size fill)
  sparse-vector?
  (table sparse-vector-table)
  (size sparse-vector-size)
  (fill sparse-vector-fill))

(define (make-sparse-vector size fill)
  (%make-sparse-vector (make-hash-table) size fill))

(define (sparse-vector-ref sv k)
  (cond ((vector? sv)
         (vector-ref sv k))
        ((sparse-vector? sv)
         (or (let ((h (hashq-get-handle (sparse-vector-table sv) k)))
               (and h (cdr h)))
             (if (<= 0 k (- (sparse-vector-size sv) 1))
                 (sparse-vector-fill sv)
                 (error "sparse-vector-ref: index out of range" k))))
        (else *unspecified*)))

(define (sparse-vector-set! sv k obj)
  (cond ((vector? sv)
         (vector-set! sv k obj))
        ((sparse-vector? sv)
         (if (<= 0 k (- (sparse-vector-size sv) 1))
             (hashq-set! (sparse-vector-table sv) k obj)
             (error "sparse-vector-set!: index out of range" k)))
        (else *unspecified*)))

;;;
;;; VM op global values
;;;

;; Hash table containing size of bytecodes, in byte.
(define *vm-op-sizes* (make-hash-table))

(for-each
 (lambda (op)
   (let ((name (car op))
         (size (- (length op) 3)))
     (hashq-set! *vm-op-sizes* name size)))
 (instruction-list))

(define (opsize op)
  (hashq-ref *vm-op-sizes* (car op)))

(define *br-ops*
  '(br
    br-if-nargs-ne br-if-nargs-lt br-if-nargs-gt br-if-npos-gt
    br-if-true br-if-null br-if-nil br-if-pair br-if-struct br-if-char
    br-if-tc7
    br-if-eq br-if-eqv br-if-equal
    br-if-= br-if-< br-if-<= br-if-logtest))

(define *return-ops*
  '(return return-values tail-call tail-call-label))

(define *known-ops*
  '(bind-kwargs
    push-fluid pop-fluid wind unwind
    add add1 sub sub1 mul div quo))

(define *label-call-ops*
  '(call-label tail-call-label))


;;;
;;; For bytecode entries
;;;

(define-record-type <entries>
  (make-entries table ops last-ip)
  entries?
  ;; Hash table containing IP used as jump destination. All values are
  ;; set to #t.
  (table entries-table)
  ;; VM operation from `fold-program-code'.
  (ops entries-ops set-entries-ops!)
  ;; Last IP of program.
  (last-ip entries-last-ip))

(define (program->entries program-or-addr)
  "Create hash-table containing key=entry IP, val=#t from PROGRAM."
  (let ((entries (make-hash-table))
        (ip 0))
    (define (add-entries . ips)
      (for-each (lambda (ip)
                  (hashq-set! entries ip #t))
                ips))
    (define (entries-one op acc)
      (cond
        ((eq? (car op) 'br)
         (add-entries (+ (cadr op) ip)))
        ((memq (car op) *br-ops*)
         (let ((dst (list-ref op (- (length op) 1))))
           (add-entries (+ ip (opsize op)) (+ ip dst))))
        ((memq (car op) *return-ops*)
         (add-entries (+ ip (opsize op)))))
      (let ((last-ip ip))
        (set! ip (+ ip (opsize op)))
        (cons (cons last-ip op) acc)))
    (let ((ops (fold-program-code entries-one '() program-or-addr #:raw? #t)))
      (make-entries entries ops ip))))

;;;
;;; For control flow graph
;;;

(define-record-type <bb>
  (make-bb entry exits ops)
  bb?
  ;; Entry IP of this block.
  (entry bb-entry)
  ;; List of exit IPs. Empty list when the block end with one of the
  ;; return operations.
  (exits bb-exits set-bb-exits!)
  ;; List of operations in this block.
  (ops bb-ops set-bb-ops!))

(define-record-type <cfg>
  (make-cfg current-bb bbs nenters entries)
  cfg?
  ;; Current <bb>.
  (current-bb cfg-current-bb set-cfg-current-bb!)
  ;; List of <bb>s.
  (bbs cfg-bbs set-cfg-bbs!)
  ;; Hash table containing key=destination IP, val=number of entry count
  ;; to the key.
  (nenters cfg-nenters)
  ;; The <entries> used by cfg.
  (entries cfg-entries))

(define (program->cfg program)
  (define entries (program->entries program))
  (define entries-t (entries-table entries))
  (define (br-op-dst op)
    (list-ref op (- (length op) 1)))
  (define (add-bb-op ip op bb)
    (cons (cons ip op) (bb-ops bb)))
  (define (new-bb! cfg ip op bb)
    (set-bb-ops! bb (reverse (add-bb-op ip op bb)))
    (set-cfg-bbs! cfg (cons bb (cfg-bbs cfg)))
    (set-cfg-current-bb! cfg (make-bb (+ ip (opsize op)) '() '())))
  (define (push-nenters! cfg bb key)
    (let ((t (cfg-nenters cfg)))
      (when (<= 0 key (entries-last-ip entries))
        (cond
         ((hashq-ref t key)
          =>
          (lambda (ips)
            (hashq-set! t key (append ips (list (bb-entry bb))))))
         (else
          (hashq-set! t key (list (bb-entry bb))))))))
  (define (cfg-one ip-x-op cfg)
    (let* ((ip (car ip-x-op))
           (op (cdr ip-x-op))
           (bb (cfg-current-bb cfg))
           (next-ip (+ ip (opsize op))))
      (when (memq (car op) *label-call-ops*)
        (push-nenters! cfg bb (+ ip (br-op-dst op))))
      (cond
       ((hashq-ref entries-t next-ip)
        (cond
         ((eq? (car op) 'br)
          (set-bb-exits! bb `(,(+ ip (br-op-dst op))))
          (push-nenters! cfg bb (+ ip (br-op-dst op)))
          (new-bb! cfg ip op bb))
         ((memq (car op) *br-ops*)
          (set-bb-exits! bb (list (+ ip (br-op-dst op)) next-ip))
          (push-nenters! cfg bb next-ip)
          (push-nenters! cfg bb (+ ip (br-op-dst op)))
          (new-bb! cfg ip op bb))
         ((memq (car op) *return-ops*)
          (new-bb! cfg ip op bb))
         (else
          (set-bb-exits! bb (list next-ip))
          (push-nenters! cfg bb next-ip)
          (new-bb! cfg ip op bb))))
       (else
        (set-bb-ops! bb (add-bb-op ip op bb))))
      cfg))
  (let ((cfg (fold-right cfg-one
                         (make-cfg (make-bb 0 '() '())
                                   '()
                                   (make-hash-table)
                                   entries)
                         (entries-ops entries))))
    (set-cfg-bbs! cfg (reverse (cfg-bbs cfg)))
    (let ((v (lightning-verbosity)))
      (and v (<= 2 v) (dump-cfg cfg)))
    cfg))

(define (dump-cfg cfg)
  (format #t ";;; cfg-nenters:~%")
  (hash-for-each (lambda (k v)
                   (format #t ";;;   ip: ~3@a <= ~a~%" k v))
                 (cfg-nenters cfg))
  (format #t ";;; cfg-bbs:~%")
  (format #t ";;;~%")
  (for-each
   (lambda (bb)
     (format #t ";;;   <bb> ~a => ~a~%" (bb-entry bb) (bb-exits bb))
     (for-each
      (lambda (op)
        (format #t ";;; ~5@a: ~a~%" (car op) (cdr op)))
      (bb-ops bb))
     (format #t ";;;~%"))
   (cfg-bbs cfg)))


;;;
;;; Auxiliary
;;;

(define (dereference-scm pointer)
  (pointer->scm (dereference-pointer pointer)))

(define (struct-nfields struct)
  (string-length (symbol->string (struct-layout struct))))

;; XXX: Refer C code.
(define-syntax-rule (struct-procedure-index) 0)

(define (ensure-program-addr program-or-addr)
  (or (and (primitive? program-or-addr)
           (pointer-address (program-free-variable-ref program-or-addr 0)))
      (and (program? program-or-addr)
           (program-code program-or-addr))
      (and (unknown? program-or-addr)
           unknown)
      (and (struct? program-or-addr)
           (let ((ref (struct-ref program-or-addr (struct-procedure-index))))
             (and (program? ref)
                  (program-code ref))))
      program-or-addr))


;;;
;;; Trace record type
;;;

(define-record-type <trace>
  (%make-trace name nargs free-vars cfg callers callee-args
               nlocals locals nretvals
               undecidables br-dests success escape)
  trace?

  ;; Name of procedure.
  (name trace-name)

  ;; Number of arguments.
  (nargs trace-nargs)

  ;; Vector containing free-vars.
  (free-vars trace-free-vars)

  ;; The <cfg> used by this trace.
  (cfg trace-cfg)

  ;; Hash table of caller, key=ip, value=program-code-addr.
  (callers trace-callers)

  ;; Hash table of arguments for callee, copy of vector.
  (callee-args trace-callee-args)

  ;; Number of locals. May not much to the `vector-length' of trace-locals.
  (nlocals trace-nlocals set-trace-nlocals!)

  ;; Vector for local variables.
  (locals trace-locals set-trace-locals!)

  ;; Number of return values.
  (nretvals trace-nretvals set-trace-nretvals!)

  ;; Hash table for undecidable locals, which might set during skipped
  ;; VM instructions. Key of the hash table is destination ip, value is
  ;; list of local index.
  (undecidables trace-undecidables)

  ;; List of branch destinations IP number, to keep track of undecidable
  ;; locals.
  (br-dests trace-br-dests set-trace-br-dests!)

  ;; Success or not.
  (success trace-success? set-trace-success!)

  ;; Escape.
  (escape trace-escape))

(define* (make-trace name args nargs cfg free-vars escape
                     #:optional
                     (ops '())
                     (callers (make-hash-table))
                     (callee-args (make-hash-table))
                     (nlocals 0)
                     (locals (make-vector 256 unknown))
                     (nretvals 1)
                     (undecidables (make-hash-table))
                     (br-dests '()))
  (when args
    (let lp ((n (- (vector-length args) 1)))
      (when (< 0 n)
        (vector-set! locals n (vector-ref args n))
        (lp (- n 1)))))
  (%make-trace name nargs free-vars cfg callers callee-args
               nlocals locals nretvals undecidables br-dests #t escape))

(define unknown 'unknown)
(define (unknown? obj) (eq? 'unknown obj))

(define-record-type <closure>
  (make-closure addr free-vars)
  closure?
  (addr closure-addr)
  (free-vars closure-free-vars))

(define-record-type <call>
  (make-call program args)
  call?
  (program call-program)
  (args call-args))

(define-record-type <builtin>
  (make-builtin idx name)
  builtin?
  (idx builtin-idx)
  (name builtin-name))

;;; XXX: Rewrite the `undecidable' local managment with <cfg>.
;;;
;;; In each entry of <bb>, if exit is determined uniquely, use the local
;;; preserved at the end of previous <bb>. If not, pick the preserved
;;; locals from one of the <bb>s at entry, then fill locals overwritten
;;; with other possible <bb>s as `unknown' value. When deciding
;;; uniqueness, filter out the IPs which is greater than entry of
;;; current <bb>, since it means backward jump.

(define (program->trace program-or-addr nargs args)
  "Make trace from PROGRAM-OR-ADDR and NARGS. If ARGS is not #f and a
vector containing arguments, assign initial locals from contents of
vector."
  (program->trace* (ensure-program-addr program-or-addr) nargs args))

(define (program->trace* program-or-addr nargs args)
  (define (trace-one ip-x-op trace)
    (let ((ip (car ip-x-op))
          (op (cdr ip-x-op)))
      (define (base-ip)
        (ensure-program-addr program-or-addr))
      (define (local-ref n)
        (vector-ref (trace-locals trace) n))
      (define (local-set! idx val)
        (for-each
         (lambda (dst)
           (let* ((u (trace-undecidables trace))
                  (undecidable-locals (hashq-ref u dst)))
             (when (and (< ip dst)
                        (list? undecidable-locals)
                        (not (memq idx undecidable-locals)))
               (debug 2 ";;; trace: (~a:~a) adding local ~a to undecidables.~%"
                      (trace-name trace) ip idx)
               (hashq-set! u dst (cons idx undecidable-locals)))))
         (trace-br-dests trace))
        (let ((verbosity (lightning-verbosity)))
          (and verbosity
               (<= 2 verbosity)
               (not (unknown? val))
               (format #t ";;; trace: (~a:~a) local-set! idx=~a, val=~a~%"
                       (trace-name trace) ip idx val)))
        (vector-set! (trace-locals trace) idx val))
      (define (offset->addr offset)
        (+ (base-ip) (* 4 (+ ip offset))))
      (define (nretvals-set! n)
        (set-trace-nretvals! trace n))
      (define (locals->args proc-local nlocals)
        (let ((args (make-vector nlocals)))
          (let lp ((n 0))
            (if (< n nlocals)
                (begin
                  (vector-set! args n (local-ref (+ n proc-local)))
                  (lp (+ n 1)))
                args))))
      (define (call-call obj)
        (cond
         ((call? obj)
          (let ((obj-proc (call-program obj)))
            (and obj-proc
                 (apply call-lightning
                        obj-proc
                        (map call-call
                             (cdr (vector->list (call-args obj))))))))
         (else
          obj)))
      (define (set-caller! proc proc-local nlocals)
        (cond
         ((or (procedure? proc)
              (builtin? proc))
          (debug 2 ";;; trace: (~a:~a) setting callee to ~a~%"
                 (trace-name trace) ip proc)
          (hashq-set! (trace-callers trace) ip proc))
         ;; ((call? proc)
         ;;  (let ((retval (call-call proc)))
         ;;    (and retval (hashq-set! (trace-callers trace) ip retval))))
         (else
          (hashq-set! (trace-callers trace) ip unknown)))
        (hashq-set! (trace-callee-args trace)
                    ip
                    (locals->args proc-local nlocals)))

      ;; (debug 2 ";;; ~3d: ~a~%" ip op)

      ;; Resolve label destinations.
      (cond
       ((or (memq (car op) *br-ops*)
            (memq (car op) *label-call-ops*))
        (let* ((offset (list-ref op (- (length op) 1)))
               (dest (+ ip offset)))
          (when (< 0 offset)
            (let ((u (trace-undecidables trace)))
              (when (and (< 0 offset)
                         (not (hashq-ref u dest))
                         (not (memq (car op) '(br-if-nargs-ne
                                               br-if-nargs-lt
                                               br-if-nargs-gt
                                               call-label
                                               tail-call-label))))
                (hashq-set! (trace-undecidables trace) dest '())))
            (set-trace-br-dests! trace (cons dest (trace-br-dests trace)))))))

      ;; Set undecidable locals if IP is destination of branch
      ;; instruction.
      (for-each
       (lambda (dst)
         (when (= ip dst)
           (let ((locals (hashq-ref (trace-undecidables trace) dst)))
             (when (list? locals)
               (debug 2 ";;; trace: (~a:~a) unknown locals ~a ~%"
                      (trace-name trace) ip locals)
               (for-each (lambda (idx)
                           (debug 12 "~a " idx)
                           (local-set! idx unknown))
                         locals)))))
       (trace-br-dests trace))

      ;; Resolve callers and callees with locals variables.
      (match op

        ;; Call and return
        ;; ---------------

        (('call proc nlocals)
         (set-caller! (local-ref proc) proc nlocals)
         (local-set! (+ proc 1) (make-call (local-ref proc)
                                           (locals->args proc nlocals))))

        (('tail-call nlocals)
         (set-caller! (local-ref 0) 0 nlocals)
         (local-set! 1 (make-call (local-ref 0) (locals->args 0 nlocals))))

        (('call-label proc nlocals target)
         (set-caller! (offset->addr target) proc nlocals)
         (local-set! (+ proc 1) (make-call (local-ref proc)
                                           (locals->args proc nlocals))))

        (('tail-call-label nlocals target)
         (set-caller! (offset->addr target) 0 nlocals)
         (local-set! 1 (make-call (offset->addr target)
                                  (locals->args 0 nlocals))))

        (('receive dst proc nlocals)
         (set-trace-nlocals! trace nlocals)
         (local-set! dst (local-ref (+ proc 1))))

        (('receive-values proc allow-extra? nvalues)
         *unspecified*)

        (('return src)
         (nretvals-set! 1))

        (('return-values)
         (nretvals-set! (- (trace-nlocals trace) 1)))


        ;; Specialized call stubs
        ;; ----------------------

        (('builtin-ref dst idx)
         (local-set! dst (make-builtin idx (builtin-index->name idx))))


        ;; Function prologues
        ;; ------------------

        (('assert-nargs-ee/locals expected nlocals)
         (when (not (= expected nargs))
           (debug 1 "trace: (~a:~a) assert-nargs-ee/locals: expected ~a, got ~a~%"
                  (trace-name trace) ip expected nargs))
         (set-trace-nlocals! trace (+ expected nlocals)))

        (('br-if-nargs-ne expected offset)
         *unspecified*)

        (('br-if-nargs-lt expected offset)
         *unspecified*)

        (('br-if-nargs-gt expected offset)
         *unspecified*)

        (('assert-nargs-ee expected)
         (when (not (= nargs expected))
           (debug 1 "assert-nargs-ee: argument mismatch" expected))
         (set-trace-nlocals! trace nargs))

        (('assert-nargs-ge expected)
         (when (< nargs expected)
           (debug 1 "assert-nargs-ge: argument mismatch" expected))
         (set-trace-nlocals! trace nargs))

        (('assert-nargs-le expected)
         (when (> nargs expected)
           (debug 1 "assert-nargs-le: argument mismatch" expected))
         (set-trace-nlocals! trace nargs))

        (('alloc-frame nlocals)
         (set-trace-nlocals! trace nlocals))

        (('reset-frame nlocals)
         (set-trace-nlocals! trace nlocals))

        (('bind-rest dst)
         (let* ((lst (let lp ((n (- (trace-nlocals trace) 1)) (acc '()))
                       (if (< n dst)
                           acc
                           (lp (- n 1) (cons (local-ref n) acc))))))
           (local-set! dst lst)))


        ;; Branching instructions
        ;; ----------------------

        ;; Keeping track of locals set inside branch, later used as
        ;; undecidable.
        ;;
        ;; XXX: When this jump is possible during compilation?
        ;; (('br offset)
        ;;  (when (< 0 offset)
        ;;    (set-trace-next-ip! trace (+ (trace-ip trace) offset))))


        ;; Lexical binding instructions
        ;; ----------------------------

        (('mov dst src)
         (local-set! dst (local-ref src)))

        (('box dst src)
         (local-set! dst (make-variable (local-ref src))))

        (('box-ref dst src)
         (let ((var (local-ref src)))
           (and (variable? var)
                (local-set! dst (variable-ref var)))))

        (('box-set! dst src)
         (let ((var (local-ref dst)))
           (and (variable? var)
                (variable-set! (local-ref dst) (local-ref src)))))

        (('make-closure dst offset nfree)
         (local-set! dst (make-closure (offset->addr offset)
                                       (make-vector nfree unknown))))

        (('free-ref dst src idx)
         (let ((p (local-ref src)))
           (cond
            ((and (program? p)
                  (< idx (program-num-free-variables p)))
             (local-set! dst (program-free-variable-ref p idx)))
            ((closure? p)
             (local-set! dst (vector-ref (closure-free-vars p) idx)))
            (else
             ;; (local-set! dst *unspecified*)
             ;; (local-set! dst runtime-call)
             (local-set! dst unknown)))))

        (('free-set! dst src idx)
         (let ((p (local-ref dst)))
           (cond
            ((program? p)
             (program-free-variable-set! p idx (local-ref src)))
            ((closure? p)
             (vector-set! (closure-free-vars p) idx (local-ref src)))
            (else
             ;; (local-set! dst unknown)
             *unspecified*))))


        ;; Immediates and staticaly allocated non-immediates
        ;; -------------------------------------------------

        (('make-short-immediate dst low-bits)
         (local-set! dst (pointer->scm (make-pointer low-bits))))

        (('make-long-immediate dst a)
         (local-set! dst (pointer->scm (make-pointer a))))

        (('make-long-long-immediate dst hi lo)
         *unspecified*)

        (('make-non-immediate dst target)
         (local-set! dst (pointer->scm (make-pointer (offset->addr target)))))

        (('static-ref dst offset)
         *unspecified*)


        ;; Mutable top-level bindings
        ;; ---------------------------

        (('current-module dst)
         (local-set! dst (current-module)))

        (('toplevel-box dst var-offset mod-offset sym-offset bound?)
         (let* ((offset->pointer
                 (lambda (offset) (make-pointer (offset->addr offset))))
                (var (dereference-scm (offset->pointer var-offset))))
           (if (variable? var)
               (local-set! dst var)
               (let* ((mod (dereference-scm (offset->pointer mod-offset)))
                      (sym (dereference-scm (offset->pointer sym-offset)))
                      (resolved (module-variable (or mod the-root-module) sym)))
                 (local-set! dst resolved)))))

        (('module-box dst var-offset mod-offset sym-offset bound?)
         (let* ((offset->pointer
                 (lambda (offset) (make-pointer (offset->addr offset))))
                (var (dereference-scm (offset->pointer var-offset))))
           (if (variable? var)
               (local-set! dst var)
               (let* ((mod (resolve-module
                            (cdr (pointer->scm (offset->pointer mod-offset)))))
                      (sym (dereference-scm (offset->pointer sym-offset)))
                      (resolved (module-variable mod sym)))
                 (local-set! dst resolved)))))


        ;; The dynamic environment
        ;; -----------------------

        (('fluid-ref dst src)
         (let ((obj (local-ref src)))
           (and (fluid? obj)
                (local-set! dst (fluid-ref obj)))))


        ;; String, symbols, and keywords
        ;; -----------------------------

        (('string-length dst src)
         (and (string? src)
              (local-set! dst (string-length src))))

        (('string-ref dst src idx)
         (let ((str (local-ref src))
               (i (local-ref idx)))
           (and (string? str)
                (integer? i)
                (local-set! dst (string-ref str i)))))


        ;; Pairs
        ;; -----

        (('cons dst a b)
         (local-set! dst (cons (local-ref a) (local-ref b))))

        (('car dst src)
         (let ((pair (local-ref src)))
           (and (pair? pair)
                (not (null? pair))
                (local-set! dst (car pair)))))

        (('cdr dst src)
         (let ((pair (local-ref src)))
           (and (pair? pair)
                (not (null? pair))
                (local-set! dst (cdr pair)))))

        (('set-car! pair car)
         (let ((pair (local-ref pair)))
           (and (pair? pair)
                (set-car! pair (local-ref car)))))

        (('set-cdr! pair cdr)
         (let ((pair (local-ref pair)))
           (and (pair? pair)
                (set-cdr! pair (local-ref cdr)))))


        ;; Numeric operations
        ;; ------------------

        ;; Vector related operations will slow down compilation time.
        ;; Though current approach requires book keeping the contents of
        ;; vector, since there is no way to determine whether vector
        ;; elements are used as callee. Using <sparse-vector> instead of
        ;; vector to manage vectors in locals.

        (('make-vector dst length init)
         (let ((len (local-ref length)))
           (and (integer? len)
                (local-set! dst (make-sparse-vector len (local-ref init))))))

        (('make-vector/immediate dst length init)
         (local-set! dst (make-sparse-vector length init)))

        (('vector-length dst src)
         (let ((v (local-ref src)))
           (and (vector? v)
                (local-set! dst (vector-length (local-ref src))))))

        (('vector-ref dst src idx)
         (let ((i (local-ref idx)))
           (and (integer? i)
                (local-set! dst (sparse-vector-ref (local-ref src) i)))))

        (('vector-ref/immediate dst src idx)
         (local-set! dst (sparse-vector-ref (local-ref src) idx)))

        (('vector-set! dst idx src)
         (let ((i (local-ref idx)))
           (and (integer? i)
                (sparse-vector-set! (local-ref dst)
                                    i
                                    (local-ref src)))))

        (('vector-set!/immediate dst idx src)
         (sparse-vector-set! (local-ref dst)
                             idx
                             (local-ref src)))


        ;; Structs and GOOPs
        ;; -----------------

        (('struct-vtable dst src)
         (let* ((obj (local-ref src))
                (vt (or (and (struct? obj)
                             (struct-vtable obj))
                        unknown)))
           (local-set! dst vt)))

        (('allocate-struct/immediate dst vtable nfields)
         (let* ((obj (local-ref vtable))
                (str (or (and (not (call? obj))
                              (not (closure? obj))
                              (struct? obj)
                              (allocate-struct obj nfields))
                         unknown)))
           (local-set! dst str)))

        (('struct-ref dst src idx)
         (let* ((src (local-ref src))
                (idx (local-ref idx))
                (ref (or (and (not (call? src))
                              (not (closure? src))
                              (struct? src)
                              (integer? idx)
                              (< idx (struct-nfields src))
                              (struct-ref src idx))
                         unknown)))
           (local-set! dst ref)))

        (('struct-ref/immediate dst src idx)
         (let* ((obj (local-ref src))
                (ref (and (not (call? obj))
                          (not (closure? obj))
                          (struct? obj)
                          (< idx (struct-nfields obj))
                          (struct-ref obj idx))))
           (local-set! dst ref)))

        (('struct-set!/immediate dst idx src)
         (let ((obj (local-ref dst)))
           (and (struct? obj)
                (struct-set! obj idx (local-ref src)))))


        ;; Arrays, packed uniform arrays, and bytevectors
        ;; ----------------------------------------------

        ;; Other op codes
        ;; --------------

        (_
         (cond
          ((or (memq (car op) *br-ops*)
               (memq (car op) *known-ops*))
           ;; Ignored.
           *unspecified*)
          (else
           (debug 0 ";;; trace: (~a:~a) Unknown op: ~a~%"
                  (trace-name trace) ip op)
           (set-trace-success! trace #f)
           ((trace-escape trace) trace)))))

      trace))

  (let ((name (program-name program-or-addr))
        (addr (ensure-program-addr program-or-addr))
        (free-vars (or (and (program? program-or-addr)
                            (list->vector
                             (program-free-variables program-or-addr)))
                       (and (closure? program-or-addr)
                            (closure-free-vars program-or-addr))
                       (make-vector 0))))
    (debug 1 ";;; trace: Start tracing ~a (~a)~%" name addr)
    (let* ((cfg (program->cfg addr))
           (result
            (call/ec
             (lambda (escape)
               (let ((acc (make-trace name args nargs cfg free-vars escape)))
                 (fold-right trace-one
                             acc
                             (entries-ops (cfg-entries cfg))))))))
      (debug 1 ";;; trace: Finished tracing ~a (~a)~%" name addr)
      (and (trace-success? result) result))))

(define (trace-ops trace)
  "Returns a list of ip and vm-operation in TRACE."
  (reverse (entries-ops (cfg-entries (trace-cfg trace)))))

(define (trace-labeled-ips trace)
  "Returns a list of labeld ips in TRACE."
  (hash-fold (lambda (k v acc) (cons k acc))
             '()
             (cfg-nenters (trace-cfg trace))))
