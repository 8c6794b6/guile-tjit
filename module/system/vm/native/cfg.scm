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

;;; Module containing codes to for converting bytecode to control flow
;;; graph, for further compilation in method JIT VM.

;;; Code:

(define-module (system vm native cfg)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language bytecode)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm disassembler)
  #:use-module (system vm native debug)
  #:use-module (system vm program)
  #:export (procedure->cfg
            make-cfg cfg?
            cfg-last-ip cfg-name
            cfg-ops cfg-labeled-ips

            program->entries entries-ops entries-table
            program->cfg dump-cfg

            fold-primitive-code ensure-program-addr))

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

(define (br-op-dst op)
  (list-ref op (- (length op) 1)))

(define *br-ops*
  '(br
    br-if-nargs-ne br-if-nargs-lt br-if-nargs-gt br-if-npos-gt
    br-if-true br-if-null br-if-nil br-if-pair br-if-struct br-if-char
    br-if-tc7
    br-if-eq br-if-eqv br-if-equal
    br-if-= br-if-< br-if-<= br-if-logtest))

(define *specialized-call-ops*
  '(return-values
    subr-call foreign-call
    compose-continuation tail-apply tail-call/shuffle call/cc))

(define *return-ops*
  (append '(return
            tail-call tail-call-label tail-call/shuffle)
          *specialized-call-ops*))

(define *ignored-ops*
  '(bind-kwargs
    subr-call foreign-call tail-apply
    prompt push-fluid pop-fluid wind unwind
    add add1 sub sub1 mul div quo rem
    string->number string->symbol string->keyword))

(define *label-call-ops*
  '(call-label tail-call-label))


;;;
;;; For entries in bytecode basic blocks
;;;

(define-syntax scm-f-program-is-partial-continuation
  (identifier-syntax #x1000))

(define-syntax scm-f-program-is-foreign
  (identifier-syntax #x2000))

(define (partial-continuation? obj)
  (and (procedure? obj)
       (< 0 (logand (pointer-address (dereference-pointer
                                      (scm->pointer obj)))
                    scm-f-program-is-partial-continuation))))

(define (foreign-procedure? obj)
  (and (procedure? obj)
       (< 0 (logand (pointer-address (dereference-pointer
                                      (scm->pointer obj)))
                    scm-f-program-is-foreign))))

(define disassemble-one
  (@@ (system vm disassembler) disassemble-one))

(define (fold-primitive-code f acc primitive)
  (let ((bv (pointer->bytevector (make-pointer (program-code primitive)) 64))
        (last-ops '(return-values
                    subr-call foreign-call
                    compose-continuation tail-apply tail-call/shuffle
                    call/cc)))
    (let lp ((acc acc) (offset 0))
      (call-with-values
          (lambda () (disassemble-one bv offset))
        (lambda (len elt)
          (cond
           ((eq? (car elt) 'halt)
            acc)
           ((memq (car elt) last-ops)
            (f elt acc))
           (else
            (lp (f elt acc) (+ offset len)))))))))

(define (fold-init-code f acc program-or-addr)
  (let ((bufsize 1024)
        (base (ensure-program-addr program-or-addr))
        (max-vm-op-size 5))
    (let lp ((bv (pointer->bytevector (make-pointer base) bufsize))
             (base base)
             (acc acc)
             (offset 0))
      (call-with-values
          (lambda ()
            (disassemble-one bv offset))
        (lambda (len elt)
          (cond
           ((eq? (car elt) 'return)
            (f elt acc))
           (else
            (let ((offset-byte (* 4 (+ offset len))))
              (if (< (- bufsize (* 4 max-vm-op-size)) offset-byte)
                  (lp (pointer->bytevector (make-pointer (+ base offset-byte))
                                           bufsize)
                      (+ base offset-byte)
                      (f elt acc)
                      0)
                  (lp bv base (f elt acc) (+ offset len)))))))))))

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
       ((memq (car op) *br-ops*)
        (add-entries (+ ip (opsize op)) (+ ip (br-op-dst op))))
       ;; ((or (memq (car op) *br-ops*)
       ;;      (memq (car op) *label-call-ops*))
       ;;  (add-entries (+ ip (br-op-dst op))))
       ((memq (car op) *return-ops*)
        (add-entries (+ ip (opsize op)))))
      (let ((last-ip ip))
        (set! ip (+ ip (opsize op)))
        (cons (cons last-ip op) acc)))
    (define builtin-addrs
      (map program-code
           (list apply values abort-to-prompt call-with-values
                 call-with-current-continuation)))
    (define (maybe-init-code? program-or-addr)
      (not (find-program-debug-info (ensure-program-addr program-or-addr))))
    (let ((ops (cond
                ((or (primitive? program-or-addr)
                     (foreign-procedure? program-or-addr)
                     (partial-continuation? program-or-addr)
                     (memq (ensure-program-addr program-or-addr)
                           builtin-addrs))
                 (fold-primitive-code entries-one '() program-or-addr))
                ((maybe-init-code? program-or-addr)
                 ;; Invoke the thunk from init code without compiling to
                 ;; native code, then return a single VM op `return'.
                 (and (procedure? program-or-addr)
                      (program-or-addr))
                 '((0 return 0)))
                (else
                 (fold-program-code entries-one '() program-or-addr
                                    #:raw? #t)))))
      (make-entries entries (reverse! ops) ip))))


;;;
;;; For control flow graph
;;;

(define-record-type <bb>
  (make-bb entry ops)
  bb?
  ;; Entry IP of this block.
  (entry bb-entry)

  ;; List of operations in this block.
  (ops bb-ops set-bb-ops!))

(define-record-type <cfg>
  (make-cfg name current-bb bbs nenters entries)
  cfg?
  ;; Name of procedure.
  (name cfg-name)
  ;; Current <bb>.
  (current-bb cfg-current-bb set-cfg-current-bb!)
  ;; List of <bb>s.
  (bbs cfg-bbs set-cfg-bbs!)
  ;; Hash table containing key=destination IP, val=number of entry count
  ;; to the key.
  (nenters cfg-nenters)
  ;; The <entries> used by cfg.
  (entries cfg-entries))

(define (program->cfg name program)
  (define entries (program->entries program))
  (define entries-t (entries-table entries))
  (define (add-bb-op ip op bb)
    (cons (cons ip op) (bb-ops bb)))
  (define (new-bb! cfg ip op bb)
    (set-bb-ops! bb (reverse! (add-bb-op ip op bb)))
    (set-cfg-bbs! cfg (cons bb (cfg-bbs cfg)))
    (set-cfg-current-bb! cfg (make-bb (+ ip (opsize op)) '())))
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
      ;; Might remove this `when'.
      (when (memq (car op) *label-call-ops*)
        (push-nenters! cfg bb (+ ip (br-op-dst op))))
      (cond
       ((hashq-ref entries-t next-ip)
        (cond
         ((eq? (car op) 'br)
          (push-nenters! cfg bb (+ ip (br-op-dst op)))
          (new-bb! cfg ip op bb))
         ((memq (car op) *br-ops*)
          (push-nenters! cfg bb next-ip) ; Might remove this line.
          (push-nenters! cfg bb (+ ip (br-op-dst op)))
          (new-bb! cfg ip op bb))
         ((memq (car op) *return-ops*)
          (new-bb! cfg ip op bb))
         (else
          (push-nenters! cfg bb next-ip)
          (new-bb! cfg ip op bb))))
       (else
        (set-bb-ops! bb (add-bb-op ip op bb))))
      cfg))
  (define (make-fresh-cfg)
    (make-cfg name
              (make-bb 0 '())
              '()
              (make-hash-table)
              entries))
  (let ((cfg (let lp ((ops (entries-ops entries)) (acc (make-fresh-cfg)))
               (if (null? ops)
                   acc
                   (lp (cdr ops) (cfg-one (car ops) acc))))))
    (set-cfg-bbs! cfg (reverse! (cfg-bbs cfg)))
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
     (format #t ";;;   <bb>:~%")
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

(define (primitive? proc)
  (and (program? proc)
       (primitive-code? (program-code proc))))

(define (ensure-program-addr program-or-addr)
  (or (and (primitive? program-or-addr)
           (pointer-address (program-free-variable-ref program-or-addr 0)))
      (and (program? program-or-addr)
           (program-code program-or-addr))
      (and (struct? program-or-addr)
           (let ((ref (struct-ref program-or-addr (struct-procedure-index))))
             (and (program? ref)
                  (program-code ref))))
      program-or-addr))

(define (procedure->cfg program-or-addr)
  "Make <cfg> from PROGRAM-OR-ADDR."
  (program->cfg (try-program-name program-or-addr) program-or-addr))

(define (cfg-ops cfg)
  "Returns a list of ip and vm-operation in CFG."
  (entries-ops (cfg-entries cfg)))

(define (cfg-labeled-ips cfg)
  "Returns a list of labeld ips in CFG."
  (hash-fold (lambda (k v acc) (cons k acc))
             '()
             (cfg-nenters cfg)
             ;; (entries-table (cfg-entries cfg))
             ))

(define (cfg-last-ip cfg)
  "Returns last ip of CFG."
  (entries-last-ip (cfg-entries cfg)))
