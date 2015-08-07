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

;;; Entry point of just-in-time compiler for `vm-tjit' engine.

(define-module (system vm native tjit)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps types)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native debug)
  #:use-module (system vm native lightning)
  #:use-module (system vm native tjit assembler)
  #:use-module (system vm native tjit compile-native)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit tlog)
  #:use-module (system vm native tjit variables)
  #:export (tjitc
            init-vm-tjit)
  #:re-export (tjit-stats))


;;;
;;; Showing IR dump
;;;

(define (show-dump ip-x-ops scm locals cps snapshots tlog code code-size)
  (define (mark-call cont)
    (match cont
      (($ $kargs _ _ ($ $continue _ _ ($ $call _ _)))
       "+")
      (_
       " ")))
  (define (mark-branch cont)
    (match cont
      (($ $kargs _ _ ($ $continue _ _ ($ $primcall name _)))
       (case name
         ((%return
           %eq %ne %lt %flt %le %ge %fge
           %guard-fx %guard-fl
           %frame-ref %frame-ref/f)
          ">")
         (else
          " ")))
      (_
       " ")))
  (define (make-indent n)
    (let lp ((n n) (acc '()))
      (if (< 0 n)
          (lp (- n 1) (cons #\. (cons #\space acc)))
          (list->string acc))))
  (define (lowest-level ip-x-ops)
    (let lp ((traces ip-x-ops) (level 0) (lowest 0))
      (match traces
        (((op . _) . traces)
         (case (car op)
           ((call call-label)
            (lp traces (+ level 1) lowest))
           ((return return-values subr-call foreign-call)
            (let ((level (- level 1)))
              (lp traces level (min level lowest))))
           (else
            (lp traces level lowest))))
        (() lowest))))
  (define (call-term? cont)
    (match cont
      (($ $kargs _ _ ($ $continue _ _ ($ $call)))
       #t)
      (_
       #f)))
  (define (pretty-type type)
    (cond
     ((eq? type &exact-integer) (blue "snum"))
     ((eq? type &flonum) (magenta "fnum"))
     ((eq? type &char) (blue "char"))
     ((eq? type &unspecified) (green "unsp"))
     ((eq? type &unbound) (green "unbn"))
     ((eq? type &false) (green "fals"))
     ((eq? type &true) (green "true"))
     ((eq? type &nil) (green "nil"))
     ((eq? type &symbol) (blue "symb"))
     ((eq? type &keyword) (blue "kw"))
     ((eq? type &procedure) (red "proc"))
     ((eq? type &pair) (yellow "pair"))
     ((eq? type &vector) (yellow "vec"))
     ((eq? type &box) (yellow "box"))
     ((eq? type &struct) (yellow "strc"))
     ((dynamic-link? type)
      (string-append "dl:" (number->string (dynamic-link-offset type))))
     ((return-address? type)
      (let ((ra (number->string (pointer-address (return-address-ip type)) 16)))
        (string-append "ra:" ra)))
     (else type)))
  (define (dump-locals locals)
    (map (match-lambda
          ((n . type) (cons n (pretty-type type))))
         locals))
  (define (dump-snapshot cont snapshot-id)
    (when (call-term? cont)
      (let ((snapshot (hashq-ref snapshots snapshot-id)))
        (match snapshot
          (($ $snapshot offset nlocals locals)
           (format #t "----     [snap~3,,,' @a] ~a:~a ~a~%"
                   snapshot-id
                   offset
                   nlocals
                   (dump-locals locals)))
          (_
           (format #t "----     [snap~3,,,' @a] ---~%" snapshot-id))))))
  (define (increment-snapshot-id cont snapshot-id)
    (if (call-term? cont)
        (+ snapshot-id 1)
        snapshot-id))

  (let ((verbosity (lightning-verbosity)))
    (when (<= 2 verbosity)
      (let ((lowest (lowest-level ip-x-ops)))
        (format #t ";;; bytecode: ~a:~a:~a~%"
                (length ip-x-ops) lowest (and locals (sort locals <)))
        (let lp ((traces ip-x-ops) (level (- lowest)))
          (match traces
            (((op ip fp ra locals) . traces)
             (let ((op-val (format #f "~x  ~a~a" ip (make-indent level) op)))
               (if (<= 3 verbosity)
                   (format #t "~40a ; ~a~%" op-val locals)
                   (format #t "~a~%" op-val)))
             (case (car op)
               ((call call-label)
                (lp traces (+ level 1)))
               ((return return-values subr-call foreign-call)
                (lp traces (- level 1)))
               (else
                (lp traces level))))
            (() (values)))))
      (format #t ";;; scm:~%~y" scm)
      (display ";;; cps\n")
      (cond
       ((not cps)
        (display "#f\n"))
       (else
        (let ((kstart (loop-start cps)))
          (let lp ((conts (reverse! (intmap-fold acons cps '())))
                   (snapshot-id 0))
            (match conts
              (((k . cont) . conts)
               (and (eq? k kstart)
                    (format #t "==== loop:~%"))
               (dump-snapshot cont snapshot-id)
               (format #t "~4,,,'0@a  ~a~a ~a~%" k
                       (mark-branch cont)
                       (mark-call cont)
                       (unparse-cps cont))
               (match cont
                 (($ $kargs _ _ ($ $continue knext _ _))
                  (when (< knext k)
                    (format #t "==== ->loop~%")))
                 (_ (values)))
               (lp conts (increment-snapshot-id cont snapshot-id)))
              (() values))))))
      (when (and code (<= 3 verbosity))
        (jit-print)
        (call-with-output-file
            (format #f "/tmp/trace-~x.o" (cadr (car ip-x-ops)))
          (lambda (port)
            (let ((code-copy (make-bytevector code-size)))
              (bytevector-copy! code 0 code-copy 0 code-size)
              (put-bytevector port code-copy))))))))


;;;
;;; Entry point
;;;

;; This procedure is called from C code in "libguile/vm-tjit.c".
(define (tjitc trace-id bytecode-ptr bytecode-len envs
               parent-ip parent-exit-id linked-ip loop?)
  (define disassemble-one
    (@@ (system vm disassembler) disassemble-one))
  (define (traced-ops bytecode-ptr bytecode-len envs)
    (let ((bytecode (pointer->bytevector bytecode-ptr bytecode-len))
          (end (/ bytecode-len 4)))
      (let lp ((acc '())
               (offset 0)
               (envs (reverse! envs)))
        (match envs
          ((env . envs)
           (let-values (((len elt) (disassemble-one bytecode offset)))
             (lp (cons (cons elt env) acc) (+ offset len) envs)))
          (()
           (reverse! acc))))))
  (define-syntax-rule (increment-compilation-failure ip)
    (let ((count (hashq-ref (failed-ip-table) ip 0)))
      (hashq-set! (failed-ip-table) ip (+ count 1))))
  (define-syntax-rule (ip-ptr->source-line addr)
    (and=>
     (find-source-for-addr addr)
     (lambda (source)
       (format #f "~a:~d"
               (or (source-file source) "(unknown file)")
               (source-line-for-user source)))))
  (define-syntax-rule (show-one-line sline tlog code-size)
    (let ((exit-pair (if (< 0 parent-ip)
                         (format #f " (~a:~a)"
                                 (or (and tlog (tlog-id tlog))
                                     (format #f "~x" parent-ip))
                                 parent-exit-id)
                         ""))
          (linked-id (if loop?
                         ""
                         (format #f " -> ~a" (tlog-id (get-tlog linked-ip))))))
      (format #t ";;; trace ~a:~a ~a~a~a~%"
              trace-id sline code-size exit-pair linked-id)))

  (let* ((ip-x-ops (traced-ops bytecode-ptr bytecode-len envs))
         (entry-ip (cadr (car ip-x-ops)))
         (verbosity (lightning-verbosity))
         (tlog (get-tlog parent-ip))
         (sline (ip-ptr->source-line (cadr (car ip-x-ops)))))
    (when (and verbosity (<= 2 verbosity))
      (format #t ";;; tjit.scm:~%")
      (format #t ":;;   entry-ip:       ~x~%" entry-ip)
      (format #t ";;;   parent-ip:      ~x~%" parent-ip)
      (format #t ";;;   linked-ip:      ~x~%" linked-ip)
      (format #t ";;;   parent-exit-id: ~a~%" parent-exit-id)
      (format #t ";;;   loop?:          ~a~%" loop?)
      (and tlog (dump-tlog tlog)))
    (with-jit-state
     (jit-prolog)
     (let-values (((locals snapshots scm cps)
                   (trace->cps tlog parent-exit-id loop? ip-x-ops)))
       (cond
        ((not cps)
         (debug 1 ";;; trace ~a:~a abort~%" trace-id sline)
         (debug 2 ";;; CPS conversion failed~%")
         (show-dump ip-x-ops scm locals cps snapshots tlog #f #f)
         (increment-compilation-failure entry-ip))
        (else
         (let-values
             (((exit-variables
                exit-codes
                trampoline
                loop-label
                loop-locals
                loop-vars
                fp-offset)
               (compile-native cps entry-ip locals snapshots tlog
                               parent-exit-id linked-ip)))
           (let ((epilog-address (jit-label)))
             (jit-patch epilog-address)
             (jit-epilog)
             (jit-realize)
             (let* ((estimated-size (jit-code-size))
                    (code (make-bytevector estimated-size)))
               (jit-set-code (bytevector->pointer code) (imm estimated-size))
               (let* ((ptr (jit-emit))
                      (exit-counts (make-hash-table))
                      (loop-address (and loop-label (jit-address loop-label)))
                      (end-address (or (and tlog (tlog-end-address tlog))
                                       (jit-address epilog-address)))
                      (parent-id (or (and tlog (tlog-id tlog))
                                     0)))
                 (make-bytevector-executable! code)
                 (put-tlog! entry-ip (make-tlog trace-id
                                                code
                                                exit-counts
                                                entry-ip
                                                parent-id
                                                parent-exit-id
                                                loop-address
                                                loop-locals
                                                loop-vars
                                                snapshots
                                                exit-variables
                                                exit-codes
                                                trampoline
                                                fp-offset
                                                end-address))
                 (when (and verbosity (<= 1 verbosity))
                   (let ((code-size (jit-code-size)))
                     (show-one-line sline tlog code-size)
                     (show-dump ip-x-ops scm locals cps snapshots tlog
                                code code-size)))
                 ;; When this trace is a side trace, replace the native code
                 ;; of trampoline in parent tlog.
                 (when tlog
                   (let ((trampoline (tlog-trampoline tlog))
                         (parent-exit-codes (tlog-exit-codes tlog)))
                     (trampoline-set! trampoline parent-exit-id ptr)
                     (hashq-set! parent-exit-codes
                                 parent-exit-id
                                 code)))))))))))))


;;;
;;; Initialization
;;;

(define (init-vm-tjit interactive?)
  "Dummy procedure for @code{autoload}."
  (initialize-tjit-primitives)
  #t)

(load-extension (string-append "libguile-" (effective-version))
                "scm_bootstrap_vm_tjit")
