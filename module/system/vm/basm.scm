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

;;; Intermediate representation of bytecode, for further assembling.

;;; Code:

(define-module (system vm basm)
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
            basm-callees basm-callers basm-locals basm-nretvals
            basm-prim-op?
            basm-chunks->alist basm->callees-list

            make-chunk chunk? chunk-labeled? chunk-dest-ip chunk-op

            make-call call? call-program call-args call-runtime-args
            call-node set-call-node!

            make-closure closure? closure-addr closure-free-vars

            builtin? builtin-name

            ensure-program-addr))

(define-record-type <basm>
  (%make-basm name ip nargs args free-vars chunks labeled-ips
              callees callers locals nretvals prim-op?
              undecidables br-dests next-ip)
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
  (next-ip basm-next-ip set-basm-next-ip!))

(define* (make-basm name args free-vars prim-op? #:optional
                    (ip 0)
                    (chunks (make-hash-table))
                    (labeled-ips '())
                    (callees (make-hash-table))
                    (callers (make-hash-table))
                    (locals #f)
                    (nretvals 1)
                    (undecidables (make-hash-table))
                    (br-dests '())
                    (next-ip #f))
  (%make-basm name ip (vector-length args) args free-vars chunks
              labeled-ips callees callers locals nretvals prim-op?
              undecidables br-dests next-ip))

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
  (%make-call program args runtime-args node)
  call?
  (program call-program)
  (args call-args)
  (runtime-args call-runtime-args set-call-runtime-args!)
  (node call-node set-call-node!))

(define (make-call program args)
  (%make-call program args #f #f))

(define runtime-call (make-call 0 (vector)))

(define-record-type <builtin>
  (make-builtin idx name)
  builtin?
  (idx builtin-idx)
  (name builtin-name))

;; Data type to tell that the value is constant.
(define-record-type <constant>
  (make-constant value)
  constant?
  (value constant-value))

(define (make-constant-locals locals)
  (let ((n (- (vector-length locals) 1))
        (v (make-vector (vector-length locals))))
    (let lp ((k n))
      (unless (< k 0)
        (let ((val (vector-ref locals k)))
          (vector-set! v k (if (constant? val)
                               val
                               (make-constant val))))
        (lp (- k 1))))
    v))

(define (local-value local)
  (cond ((constant? local)
         (constant-value local))
        (else
         local)))

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
  (cond ((constant? sv)
         (sparse-vector-ref (constant-value sv) k))
        ((vector? sv)
         (vector-ref sv k))
        ((sparse-vector? sv)
         (or (let ((h (hashq-get-handle (sparse-vector-table sv) k)))
               (and h (cdr h)))
             (if (<= 0 k (- (sparse-vector-size sv) 1))
                 (sparse-vector-fill sv)
                 (error "sparse-vector-ref: index out of range" k))))
        (else *unspecified*)))

(define (sparse-vector-set! sv k obj)
  (cond ((constant? sv)
         (sparse-vector-set! (constant-value sv) k obj))
        ((vector? sv)
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

;; (define (basm->callees-list basm)
;;   (define (lp basm)
;;     (if (basm? basm)
;;         (hash-fold (lambda (k v acc)
;;                      (cons (cons k v) (append (lp v) acc)))
;;                    '()
;;                    (basm-callees basm))
;;         '()))
;;   (reverse (lp basm)))

(define (basm->callees-list basm)
  (define (lp seen basm)
    (if (basm? basm)
        (hash-fold
         (lambda (k v acc)
           (cond ((hashq-ref seen k)
                  acc)
                 (else
                  (hashq-set! seen k #t)
                  (cons (cons k v) (append (lp seen v) acc)))))
         '()
         (basm-callees basm))
        '()))
  (reverse (lp (make-hash-table) basm)))

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
  ;; (format #t ";;; proc->basm:~%;;;   program-or-addr=~a~%;;;   args=~a~%"
  ;;         program-or-addr args)
  (proc->basm* (make-hash-table)
               (ensure-program-addr program-or-addr)
               (make-constant-locals args)))

(define (proc->basm* seen program-or-addr args)
  (define (f op basm)
    (define (base-ip)
      (ensure-program-addr program-or-addr))
    (define (local-ref n)
      (vector-ref (basm-locals basm) n))
    (define (local-ref/var n)
      (local-value (vector-ref (basm-locals basm) n)))
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
    (define (set-caller! pre-proc)
      (let ((proc (local-value pre-proc)))
        ;; Storing proc itself. Variables for primitive procedures may use same
        ;; address for different procedures.
        (hashq-set! (basm-callers basm) (basm-ip basm) proc)))
    (define (set-callee! pre-proc proc-local nlocals)
      ;; (format #t "basm: (set-callee! ~a ~a ~a)~%" pre-proc proc-local nlocals)
      (let ((proc (local-value pre-proc)))
        ;; (format #t "basm: set-callee!, proc=~a~%" proc)
        (cond
         ((and (closure? proc)
               (not (hashq-ref seen (closure-addr proc))))
          (hashq-set! seen (closure-addr proc) #t)
          (let ((callee (proc->basm* seen
                                     (closure-addr proc)
                                     (locals->args proc-local nlocals))))
            (hashq-set! (basm-callees basm) (closure-addr proc) callee)
            (hashq-set! (basm-callees basm)
                        (append (list 'closure (basm-ip basm)
                                      (map (lambda (n)
                                             (+ n proc-local))
                                           (iota nlocals))))
                        proc)))

         ((call? proc)
          ;; (hashq-set! seen (ensure-program-addr (call-program proc)) #t)
          (set-call-runtime-args! proc (locals->args proc-local nlocals))
          (hashq-set! (basm-callees basm)
                      (append (list 'call (basm-ip basm))
                              (map (lambda (n)
                                     (+ n proc-local))
                                   (iota nlocals)))
                      proc))

         ((or (unspecified? proc)
              (primitive? proc)
              (builtin? proc))
          *unspecified*)

         ;; XXX: Need to handle smob and structs, as done in vm-engine's
         ;; `apply:'.
         ((not (hashq-ref seen (ensure-program-addr proc)))
          (hashq-set! seen (ensure-program-addr proc) #t)
          (let ((callee (proc->basm* seen
                                     (ensure-program-addr proc)
                                     (locals->args proc-local nlocals))))
            (hashq-set! (basm-callees basm)
                        (ensure-program-addr proc)
                        callee))))))
    (define (set-caller/callee! proc proc-local nlocals)
      (set-callee! proc proc-local nlocals)
      (set-caller! proc))

    ;; (format #t "basm (~a:~a): ~a~%" (basm-name basm) (basm-ip basm) op)

    ;; Resolve label destinations.
    (let ((dst #f))
      (case (car op)
        ((br
          br-if-nargs-ne br-if-nargs-lt br-if-nargs-gt br-if-npos-gt
          br-if-true br-if-null br-if-nil br-if-pair br-if-struct br-if-char
          br-if-tc7
          br-if-eq br-if-eqv br-if-equal
          br-if-= br-if-< br-if-<= br-if-logtest)
         (let* ((offset (list-ref op (- (length op) 1)))
                (dest (+ (basm-ip basm) offset)))
           (when (< 0 offset)
             (let ((u (basm-undecidables basm)))
               (when (and (< 0 offset)
                          (not (hashq-ref u dest))
                          (not (memq (car op) '(br-if-nargs-ne
                                                br-if-nargs-lt
                                                br-if-nargs-gt)))
                          )
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
        (('return src)
         (nretvals-set! 1))
        (('return-values)
         (nretvals-set! (- (vector-length (basm-locals basm)) 1)))

        ;; Specialized call stubs
        (('builtin-ref dst idx)
         (local-set! dst (make-builtin idx (builtin-index->name idx))))

        ;; Function prologues
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
        ;;
        ;; Keeping track of locals set inside branch, later used as
        ;; undecidable.

        ;; XXX: When this jump is possible during compilation?
        ;; (('br offset)
        ;;  (when (< 0 offset)
        ;;    (set-basm-next-ip! basm (+ (basm-ip basm) offset))))

        ;; Lexical binding instructions
        (('mov dst src)
         (local-set! dst (local-ref src)))
        (('box dst src)
         (local-set! dst (make-variable (local-ref/var src))))
        (('box-ref dst src)
         (local-set! dst (variable-ref (local-ref/var src))))
        (('box-set dst src)
         (variable-set! (local-ref/var dst) (local-ref/var src)))
        (('make-closure dst offset nfree)
         (local-set! dst (make-closure (offset->addr offset)
                                       (make-vector nfree))))
        (('free-set! dst src idx)
         (let ((p (local-ref/var dst)))
           (cond ((program? p)
                  (program-free-variable-set! p idx (local-ref src)))
                 ((closure? p)
                  (vector-set! (closure-free-vars p) idx (local-ref src)))
                 (else
                  (local-set! dst runtime-call)))))
        (('free-ref dst src idx)
         (let ((p (local-ref/var src)))
           (cond ((and (program? p)
                       (< idx (program-num-free-variables p)))
                  (local-set! dst (program-free-variable-ref p idx)))
                 ((closure? p)
                  (local-set! dst (vector-ref (closure-free-vars p) idx)))
                 (else
                  ;; (local-set! dst *unspecified*)
                  (local-set! dst runtime-call)))))

        ;; Immediates and staticaly allocated non-immediates
        (('make-short-immediate dst low-bits)
         (local-set! dst (pointer->scm (make-pointer low-bits))))
        (('make-non-immediate dst target)
         (local-set! dst (pointer->scm (make-pointer (offset->addr target)))))

        ;; Mutable top-level bindings
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
        (('fluid-ref dst src)
         (let ((obj (local-ref/var src)))
           (and (fluid? obj)
                (local-set! dst (fluid-ref obj)))))

        ;; Pairs
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

        ;; Vector related operations will slow down compilation time.
        ;; Though current approach required book keeping the contents of
        ;; vector, since there is no way to determine whether vector
        ;; elements are used as callee. Using <sparse-vector> instead of
        ;; vector to manage vectors in locals.

        (('make-vector dst length init)
         (let ((len (local-ref/var length)))
           (and (integer? len)
                (local-set! dst (make-sparse-vector len (local-ref init))))))
        (('make-vector/immediate dst length init)
         (local-set! dst (make-sparse-vector length init)))
        (('vector-ref dst src idx)
         (let ((i (local-ref/var idx)))
           (and (integer? i)
                (local-set! dst (sparse-vector-ref (local-ref src) i)))))
        (('vector-ref/immediate dst src idx)
         (local-set! dst (sparse-vector-ref (local-ref src) idx)))
        (('vector-set! dst idx src)
         (let ((i (local-ref idx)))
           (and (integer? i)
                (sparse-vector-set! (local-ref dst)
                                    i
                                    (local-ref/var src)))))
        (('vector-set!/immediate dst idx src)
         (sparse-vector-set! (local-ref dst)
                             idx
                             (local-ref/var src)))

        (_ *unspecified*)))

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
    ;; (format #t ";;; basm: ~a (~a)~%" name (ensure-program-addr program-or-addr))
    (hashq-set! seen (ensure-program-addr program-or-addr) #t)
    (fold-program-code f
                       (make-basm name args free-vars prim-op?)
                       (ensure-program-addr program-or-addr)
                       #:raw? #t)))
