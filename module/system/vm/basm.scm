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

;;; Intermediate representation of bytecode.

;;; Code:

(define-module (system vm basm)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language bytecode)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm disassembler)
  #:use-module (system vm program)
  #:autoload (system vm lightning) (call-lightning)
  #:export (proc->basm
            make-basm basm? basm-ip basm-name basm-nargs basm-args
            basm-free-vars basm-chunks basm-labeled-ips
            basm-callees basm-callers basm-callee-args
            basm-locals basm-nretvals
            basm-prim-op?
            basm-chunks->alist basm->callees-list

            make-chunk chunk? chunk-labeled? chunk-dest-ip chunk-op

            make-call call? call-program call-args

            make-closure closure? closure-addr closure-free-vars

            builtin? builtin-name
            constant?

            ensure-program-addr))

(define-record-type <basm>
  (%make-basm name ip nargs args free-vars chunks labeled-ips
              callees callers callee-args locals nretvals prim-op?
              undecidables br-dests next-ip success escape)
  basm?
  ;; Name of procedure.
  (name basm-name)
  ;; Bytecode instruction point.
  (ip basm-ip set-basm-ip!)
  ;; Number of arguments.
  (nargs basm-nargs)
  ;; Vector containing arguments.
  (args basm-args)
  ;; Vector containing free-vars.
  (free-vars basm-free-vars)
  ;; Hash table with key=ip, value=basm-chunk.
  (chunks basm-chunks)
  ;; List of ips referred as label destination.
  (labeled-ips basm-labeled-ips set-basm-labeled-ips!)
  ;; Hash table of callee procedures, key=program-code, value=<basm>.
  (callees basm-callees set-basm-callees!)
  ;; Hash table of caller, key=ip, value=program-code-addr.
  (callers basm-callers)
  ;; Hash table of arguments for callee, copy of vector.
  (callee-args basm-callee-args)
  ;; Local variables.
  (locals basm-locals set-basm-locals!)
  ;; Number of return values.
  (nretvals basm-nretvals set-basm-nretvals!)
  ;; Primitive procedure, or #f.
  (prim-op? basm-prim-op?)
  ;; Hash table for undecidable locals, which might set during skipped
  ;; VM instructions. Key of the hash table is destination ip, value is
  ;; list of local index.
  (undecidables basm-undecidables)
  ;; List of branch destinations IP number, to keep track of undecidable
  ;; locals.
  (br-dests basm-br-dests set-basm-br-dests!)
  ;; Next IP.
  (next-ip basm-next-ip set-basm-next-ip!)
  ;; Success or not.
  (success basm-success? set-basm-success!)
  ;; Escape.
  (escape basm-escape))

(define* (make-basm name args free-vars prim-op? escape
                    #:optional
                    (ip 0)
                    (chunks (make-hash-table))
                    (labeled-ips '())
                    (callees (make-hash-table))
                    (callers (make-hash-table))
                    (callee-args (make-hash-table))
                    (locals #f)
                    (nretvals 1)
                    (undecidables (make-hash-table))
                    (br-dests '())
                    (next-ip #f))
  (%make-basm name ip (vector-length args) args free-vars chunks
              labeled-ips callees callers callee-args locals nretvals
              prim-op? undecidables br-dests next-ip #t escape))

(define-record-type <chunk>
  (make-chunk dest-ip op)
  chunk?
  ;; Bytecode ip of destination, or #f.
  (dest-ip chunk-dest-ip)
  ;; Bytecode VM operation.
  (op chunk-op))

(define-record-type <closure>
  (make-closure addr free-vars)
  closure?
  (addr closure-addr)
  (free-vars closure-free-vars))

(define-record-type <call>
  (%make-call program args)
  call?
  (program call-program)
  (args call-args))

(define (make-call program args)
  (%make-call program args))

(define runtime-call (make-call 0 (vector)))

(define-record-type <builtin>
  (make-builtin idx name)
  builtin?
  (idx builtin-idx)
  (name builtin-name))

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

(define (basm-chunks->alist chunks)
  (sort (hash-fold (lambda (k v acc) (cons (cons k v) acc))
                   '()
                   chunks)
        (lambda (a b) (< (car a) (car b)))))

;; Hash table containing size of bytecodes, in byte.
(define *vm-op-sizes* (make-hash-table))

(for-each
 (lambda (op)
   (let ((name (car op))
         (size (- (length op) 3)))
     (hashq-set! *vm-op-sizes* name size)))
 (instruction-list))

(define (dereference-scm pointer)
  (pointer->scm (dereference-pointer pointer)))

;; XXX: Refer C code.
(define struct-procedure-index 0)

(define (ensure-program-addr program-or-addr)
  (or (and (primitive? program-or-addr)
           (pointer-address (program-free-variable-ref program-or-addr 0)))
      (and (program? program-or-addr)
           (program-code program-or-addr))
      (and (struct? program-or-addr)
           (let ((ref (struct-ref program-or-addr struct-procedure-index)))
             (and (program? ref)
                  (program-code ref))))
      program-or-addr))

(define (proc->basm program-or-addr args)
  (proc->basm* (ensure-program-addr program-or-addr) args))

(define (proc->basm* program-or-addr args)
  (define (trace-one op basm)
    (define (base-ip)
      (ensure-program-addr program-or-addr))
    (define (local-ref n)
      (vector-ref (basm-locals basm) n))
    (define (local-set! idx val)
      (for-each (lambda (dst)
                  (let* ((u (basm-undecidables basm))
                         (undecidable-locals (hashq-ref u dst)))
                    (when (and (< (basm-ip basm) dst)
                               (list? undecidable-locals)
                               (not (memq idx undecidable-locals)))
                      (hashq-set! u dst (cons idx undecidable-locals)))))
                (basm-br-dests basm))
      ;; (format #t ";;; basm: (~a:~a) local-set! idx=~a, val=~a~%"
      ;;         (basm-name basm) (basm-ip basm) idx val)
      (vector-set! (basm-locals basm) idx val))
    (define (offset->addr offset)
      (+ (base-ip) (* 4 (+ (basm-ip basm) offset))))
    (define (nretvals-set! n)
      (set-basm-nretvals! basm n))
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
                      (map call-call (cdr (vector->list (call-args obj))))))))
       (else
        obj)))
    (define (set-caller! proc proc-local nlocals)
      (cond
       ((call? proc)
        ;; (hashq-set! (basm-callers basm) (basm-ip basm) #f)
        (let ((retval (call-call proc)))
          (and retval
               (hashq-set! (basm-callers basm) (basm-ip basm) retval))))
       (else
        (hashq-set! (basm-callers basm) (basm-ip basm) proc)))
      (hashq-set! (basm-callee-args basm)
                  (basm-ip basm)
                  (locals->args proc-local nlocals)))
    (define (set-caller/callee! proc proc-local nlocals)
      (set-caller! proc proc-local nlocals))

    ;; (format #t "basm (~a:~a): ~a~%" (basm-name basm) (basm-ip basm) op)

    ;; Resolve label destinations.
    (let ((dst #f))
      (case (car op)
        ((br
          br-if-nargs-ne br-if-nargs-lt br-if-nargs-gt br-if-npos-gt
          br-if-true br-if-null br-if-nil br-if-pair br-if-struct br-if-char
          br-if-tc7
          br-if-eq br-if-eqv br-if-equal
          br-if-= br-if-< br-if-<= br-if-logtest
          call-label tail-call-label)
         (let* ((offset (list-ref op (- (length op) 1)))
                (dest (+ (basm-ip basm) offset)))
           (when (< 0 offset)
             (let ((u (basm-undecidables basm)))
               (when (and (< 0 offset)
                          (not (hashq-ref u dest))
                          (not (memq (car op) '(br-if-nargs-ne
                                                br-if-nargs-lt
                                                br-if-nargs-gt
                                                call-label
                                                tail-call-label))))
                 (hashq-set! (basm-undecidables basm) dest '())))
             (when (not (memq dst (basm-br-dests basm)))
               (set-basm-br-dests! basm (cons dest (basm-br-dests basm)))))
           (set-basm-labeled-ips! basm (cons dest (basm-labeled-ips basm)))
           (set! dst dest))))
      (hashq-set! (basm-chunks basm) (basm-ip basm) (make-chunk dst op)))

    ;; Set undecidable locals if IP is destination of branch
    ;; instruction.
    (for-each
     (lambda (dst)
       (when (= (basm-ip basm) dst)
         (let ((locals (hashq-ref (basm-undecidables basm) dst)))
           (when (list? locals)
             (for-each (lambda (idx)
                         (local-set! idx *unspecified*))
                       locals)))))
     (basm-br-dests basm))

    ;; Resolve callers and callees with locals variables.
    (when (or (not (basm-next-ip basm))
              (<= (basm-next-ip basm) (basm-ip basm)))
      (match op

        ;; Call and return
        ;; ---------------

        (('call proc nlocals)
         (set-caller/callee! (local-ref proc) proc nlocals)
         (local-set! (+ proc 1) (make-call (local-ref proc)
                                           (locals->args proc nlocals))))
        (('tail-call nlocals)
         (set-caller/callee! (local-ref 0) 0 nlocals)
         (local-set! 1 (make-call 0 (locals->args 0 nlocals))))
        (('call-label proc nlocals target)
         (set-caller/callee! (offset->addr target) proc nlocals)
         (local-set! (+ proc 1) (make-call (local-ref proc)
                                           (locals->args proc nlocals))))
        (('tail-call-label nlocals target)
         (set-caller/callee! (offset->addr target) 0 nlocals)
         (local-set! 1 (make-call 0 (locals->args 0 nlocals))))
        (('receive dst proc nlocals)
         (local-set! dst (local-ref (+ proc 1))))
        (('receive-values proc allow-extra? nvalues)
         *unspecified*)
        (('return src)
         (nretvals-set! 1))
        (('return-values)
         (nretvals-set! (- (vector-length (basm-locals basm)) 1)))

        ;; Specialized call stubs
        ;; ----------------------

        (('builtin-ref dst idx)
         (local-set! dst (make-builtin idx (builtin-index->name idx))))

        ;; Function prologues
        ;; ------------------

        (('assert-nargs-ee/locals expected nlocals)
         (let ((locals (make-vector (+ expected nlocals) *unspecified*)))
           ;; (when (not (= expected (vector-length args)))
           ;;   ;; (error "assert-nargs-ee/locals: argument length mismatch"
           ;;   ;;        (basm-ip basm) expected args)
           ;;   (format #t "assert-nargs-ee/locals: ~a~%" program-or-addr)
           ;;   (format #t "  expected ~a, args ~a~%" expected args))
           (let lp ((n 0))
             (when (and (< n expected)
                        (< n (vector-length args)))
               (vector-set! locals n (vector-ref args n))
               (lp (+ n 1))))
           (set-basm-locals! basm locals)))

        (('assert-nargs-ge expected)
         ;; (when (< (vector-length args) expected)
         ;;   (error "assert-nargs-ge: argument mismatch" expected))
         ;; (set-basm-locals! basm args)
         ;; (format #t ";;; basm: assert-nargs-ge expected=~a~%" expected)
         ;; (format #t ";;; basm: args=~a~%" args)
         ;; (set-basm-locals! basm args)
         (let ((locals (make-vector (+ expected 1))))
           (let lp ((n 0))
             (when (and (< n expected)
                        (< n (basm-nargs basm)))
               (vector-set! locals n (vector-ref args n))
               (lp (+ n 1))))
           ;; (format #t ";;; basm: setting locals to new ~a~%" locals)
           (set-basm-locals! basm locals))

         ;; (if (< (basm-nargs basm) expected)
         ;;     (let ((locals (make-vector (+ expected 1))))
         ;;       (let lp ((n 0))
         ;;         (when (and (< n expected)
         ;;                    (< n (basm-nargs basm)))
         ;;           (vector-set! locals n (vector-ref args n))
         ;;           (lp (+ n 1))))
         ;;       (format #t ";;; basm: setting locals to new ~a~%" locals)
         ;;       (set-basm-locals! basm locals))
         ;;     (begin
         ;;       (format #t ";;; basm: setting locals to args ~a~%" args)
         ;;       (set-basm-locals! basm args)))
         )

        ;; XXX: Could decide next IP at compile time.
        (('br-if-nargs-ne expected offset)
         ;; (format #t ";;; br-if-nargs-ne, ip=~a, expected=~a, nargs=~a~%"
         ;;         (basm-ip basm) expected (basm-nargs basm))
         (let ((locals (make-vector expected *unspecified*)))
           (set-basm-locals! basm locals))
         ;; (when (not (= (basm-nargs basm) expected))
         ;;   (format #t ";;; br-if-nargs-ne, skipping until ip=~a~%"
         ;;           (+ (basm-ip basm) offset))
         ;;   (set-basm-next-ip! basm (+ (basm-ip basm) offset)))
         )

        (('br-if-nargs-lt expected offset)
         (let ((locals (make-vector expected *unspecified*)))
           (set-basm-locals! basm locals))
         ;; (when (< (basm-nargs basm) expected)
         ;;   (set-basm-next-ip! basm (+ (basm-ip basm) offset)))
         )

        (('br-if-nargs-gt expected offset)
         (let ((locals (make-vector expected *unspecified*)))
           (set-basm-locals! basm locals))
         ;; (when (< expected (basm-nargs basm))
         ;;   (set-basm-next-ip! basm (+ (basm-ip basm) offset)))
         )

        (('alloc-frame nlocals)
         (let* ((new-locals (make-vector nlocals *unspecified*))
                (old-locals (basm-locals basm))
                (old-length (vector-length old-locals))
                (nmax (min nlocals old-length)))
           (let lp ((n 0))
             (when (< n nmax)
               (vector-set! new-locals n (vector-ref old-locals n))
               (lp (+ n 1))))
           (set-basm-locals! basm new-locals)))

        (('reset-frame nlocals)
         (let* ((old-locals (basm-locals basm))
                (old-length (vector-length old-locals)))
           (when (< old-length nlocals)
             (let* ((new-locals (make-vector nlocals *unspecified*))
                    (nmax (min nlocals old-length)))
               (let lp ((n 0))
                 (when (< n nmax)
                   (vector-set! new-locals n (vector-ref old-locals n))
                   (lp (+ n 1))))
               (set-basm-locals! basm new-locals)))))

        (('bind-rest dst)
         (let* ((nargs (vector-length args))
                (lst (let lp ((n (- nargs 1)) (acc '()))
                       (if (< n dst)
                           acc
                           (lp (- n 1) (cons (vector-ref args n) acc))))))
           (local-set! dst lst)))

        ;; Branching instructions
        ;; ----------------------

        ;; Keeping track of locals set inside branch, later used as
        ;; undecidable.
        ;;
        ;; XXX: When this jump is possible during compilation?
        ;; (('br offset)
        ;;  (when (< 0 offset)
        ;;    (set-basm-next-ip! basm (+ (basm-ip basm) offset))))

        (('br dst)
         *unspecified*)
        (('br-if-true a invert offset)
         *unspecified*)
        (('br-if-null a invert offset)
         *unspecified*)
        (('br-if-pair a invert offset)
         *unspecified*)
        (('br-if-eq a b invert offset)
         *unspecified*)
        ;; (('br-if-eqv a b invert offset)
        ;;  *unspecified*)
        ;; (('br-if-equal a b invert offset)
        ;;  *unspecified*)
        (('br-if-< a b invert offset)
         *unspecified*)
        (('br-if-<= a b invert offset)
         *unspecified*)
        (('br-if-= a b invert offset)
         *unspecified*)

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
         (variable-set! (local-ref dst) (local-ref src)))

        (('make-closure dst offset nfree)
         (local-set! dst (make-closure (offset->addr offset)
                                       (make-vector nfree))))

        (('free-ref dst src idx)
         (let ((p (local-ref src)))
           (cond ((and (program? p)
                       (< idx (program-num-free-variables p)))
                  (local-set! dst (program-free-variable-ref p idx)))
                 ((closure? p)
                  (local-set! dst (vector-ref (closure-free-vars p) idx)))
                 (else
                  ;; (local-set! dst *unspecified*)
                  (local-set! dst runtime-call)))))

        (('free-set! dst src idx)
         (let ((p (local-ref dst)))
           (cond ((program? p)
                  (program-free-variable-set! p idx (local-ref src)))
                 ((closure? p)
                  (vector-set! (closure-free-vars p) idx (local-ref src)))
                 (else
                  (local-set! dst runtime-call)))))

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
         (let* ((current (basm-ip basm))
                (offset->pointer
                 (lambda (offset) (make-pointer (offset->addr offset))))
                (var (dereference-scm (offset->pointer var-offset))))
           (if (variable? var)
               (local-set! dst var)
               (let* ((mod (dereference-scm (offset->pointer mod-offset)))
                      (sym (dereference-scm (offset->pointer sym-offset)))
                      (resolved (module-variable (or mod the-root-module) sym)))
                 (local-set! dst resolved)))))

        (('module-box dst var-offset mod-offset sym-offset bound?)
         (let* ((current (basm-ip basm))
                (offset->pointer
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
         *unspecified*)

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

        (('add dst a b)
         *unspecified*)
        (('add1 dst src)
         *unspecified*)
        (('sub dst a b)
         *unspecified*)
        (('sub1 dst src)
         *unspecified*)
        (('mul dst a b)
         *unspecified*)
        (('div dst a b)
         *unspecified*)
        (('quo dst a b)
         *unspecified*)

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

        (_
         (format #t ";;; basm: unknown opcode: ~a~%" op)
         (set-basm-success! basm #f)
         (basm-escape basm))))

    ;; Increment IP.
    (set-basm-ip! basm (+ (basm-ip basm) (hashq-ref *vm-op-sizes* (car op))))
    basm)

  (let ((name (or (and (procedure? program-or-addr)
                       (procedure-name program-or-addr))
                  (string->symbol
                   (format #f "anon:~a" program-or-addr))))
        (prim-op? (and (primitive? program-or-addr)
                       program-or-addr))
        (free-vars (or (and (program? program-or-addr)
                            (list->vector
                             (program-free-variables program-or-addr)))
                       (and (closure? program-or-addr)
                            (closure-free-vars program-or-addr))
                       (make-vector 0))))
    (let ((result
           (call/ec
            (lambda (escape)
              (let ((acc (make-basm name args free-vars prim-op? escape))
                    (addr (ensure-program-addr program-or-addr)))
                (fold-program-code trace-one acc addr #:raw? #t))))))
      (and (basm-success? result) result))))
