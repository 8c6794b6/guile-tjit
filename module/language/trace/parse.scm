;;;; Parse bytecode with recorded data

;;;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.
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
;;; Parse bytecode and initialize environment. This is the first phase of the
;;; whole compiler workflow.
;;;
;;; Code:

(define-module (language trace parse)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (system vm native debug)
  #:use-module (language trace error)
  #:use-module (language trace env)
  #:use-module (language trace fragment)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:export (parse-bytecode *scan-procedures*))

;;;
;;; Exported hash tablel
;;;

(define *scan-procedures*
  (make-hash-table 255))

;;; XXX: Not so polite way to refer private procedure.
;;;
;;; Without defining `disassemble-one' as top level variable, trace happened at
;;; the time of module import in beginning of script was failing.  Private
;;; lookup may fail when trace happened in early stage of program execution,
;;; hence defined as top level variable in this module.
(define disassemble-one
  (@@ (system vm disassembler) disassemble-one))


;;;
;;; Parser
;;;

(define (parse-bytecode env bytecode traces)
  "Parse bytecode stored in BYTECODE with TRACES, then initialize ENV.

This procedure parses a bytevector BYTECODE, which containing bytecodes recorded
in C VM interpreter function. The C VM interpreter records accompanying
information as TRACES, which are list of lists, containing corresponding IP,
return address, dynamic link, and locals.

Returns two values, the first value is a list of parsed bytecode operation with
corresponding IP, return address, dynamic link, and locals. The second value is
a success flag, true on success, false otherwise.

After successufl parse, this procedure will update fields in ENV."

  (define initial-sp-offset
    (env-sp-offset env))
  (define initial-fp-offset
    (env-fp-offset env))
  (define (resolve-copies dsts srcs)
    (let ((copies (let lp ((dsts dsts) (acc '()))
                    (match dsts
                      (((dst 'copy . src) . dsts)
                       (lp dsts (cons (cons dst src) acc)))
                      ((_ . dsts)
                       (lp dsts acc))
                      (()
                       acc)))))
      (let lp ((copies copies) (dsts dsts))
        (match copies
          (((dst . src) . copies)
           (lp copies (assq-set! dsts dst (assq-ref srcs src))))
          (_
           dsts)))))
  (define (set-reversed-vector! setter getter)
    (setter env (list->vector (reverse! (getter env)))))
  (define (%nyi op)
    (let ((verbosity (lightning-verbosity)))
      (if (and (number? verbosity) (<= 1 verbosity))
          (lambda (op)
            (debug 1 "NYI: ~a~%" (car op))
            #f)
          (nyi "~a" op))))
  (define (scan-trace env op ip dl locals)
    ;; Compute local indices and stack element types in op.
    ;;
    ;; The stack used by VM interpreter grows down. Lower frame data is saved at
    ;; the time of accumulation.  If one of the guard operation appeared soon
    ;; after bytecode sequence `return' or `receive', snapshot does not know the
    ;; value of locals in lower frame. When recorded bytecode contains `return'
    ;; before `call', snapshot will recover a frame higher than the one used to
    ;; enter the native call.
    ;;
    (debug 2 ";;; [parse] call=~a return=~a op=~a~%"
           (env-call-num env) (env-return-num env) op)
    (let lp ((procs (hashq-ref *scan-procedures* (car op))))
      (match procs
        (((test . work) . procs)
         (if (test op locals)
             (apply work env ip dl locals (cdr op))
             (lp procs)))
        (_ (%nyi op)))))
  (define (go)
    (let lp ((offset 0) (traces traces) (so-far-so-good? #t))
      (match traces
        ((trace . traces)
         (match trace
           (#(_ ip ra dl locals)
            (let*-values
                (((len op) (disassemble-one bytecode offset))
                 ((implemented?)
                  (if so-far-so-good?
                      (let* ((ret (scan-trace env op ip dl locals))
                             (_ (infer-type env op ip dl locals))
                             (ws (let lp ((types (env-inferred-types env))
                                          (acc '()))
                                   (if (null? types)
                                       acc
                                       (lp (cdr types)
                                           (cons (car (car types)) acc)))))
                             (buf (cons ws (env-write-buf env))))
                        (increment-env-call-return-num! env op)
                        (set-env-write-buf! env buf)
                        ret)
                      #f)))
              (vector-set! trace 0 op)
              (lp (+ offset len) traces implemented?)))
           (_ (error "malformed trace" trace))))
        (()
         (let* ((linked-ip (env-linked-ip env))
                (last-sp-offset (env-sp-offset env))
                (linked-fragment
                 (if linked-ip
                     (let ((hint (let lp ((ts (env-inferred-types env))
                                          (acc '()))
                                   (match ts
                                     (((n . t) . ts)
                                      (let ((e (cons (- n last-sp-offset) t)))
                                        (lp ts (cons e acc))))
                                     (() acc)))))
                       (get-root-trace hint linked-ip))
                     #f))
                (origin-id (and=> (get-origin-fragment
                                   (env-parent-fragment env))
                                  fragment-id))
                (linking-roots?
                 ;; Detecting root trace linkage by chasing parent id until it
                 ;; reaches to root trace and compare it with linked trace. This
                 ;; loop could be avoided by saving the origin trace id in
                 ;; fragment record type.
                 (let ((linked-id (and=> linked-fragment fragment-id)))
                   (and origin-id linked-id
                        (not (eq? origin-id linked-id)))))
                (depth (or (and=> (env-parent-snapshot env)
                                  snapshot-inline-depth)
                           0)))
           (when linking-roots?
             (let* ((old-ids (fragment-linked-root-ids linked-fragment))
                    (new-ids (cons origin-id old-ids)))
               (set-fragment-linked-root-ids! linked-fragment new-ids)))
           (set-env-linked-fragment! env linked-fragment)
           (set-env-linking-roots! env linking-roots?)
           (set-env-last-sp-offset! env last-sp-offset)
           (set-env-call-num! env 0)
           (set-env-return-num! env 0)
           (set-env-inline-depth! env depth)
           (set-reversed-vector! set-env-sp-offsets! env-sp-offsets)
           (set-reversed-vector! set-env-fp-offsets! env-fp-offsets)
           (set-reversed-vector! set-env-write-buf! env-write-buf)
           (let* ((entry (env-entry-types env))
                  (inferred (env-inferred-types env)))
             (set-env-entry-types! env (resolve-copies entry entry))
             (set-env-inferred-types! env (resolve-copies inferred entry))
             (set-env-read-indices! env (sort (map car entry) <))
             (set-env-write-indices! env (sort (map car inferred) <)))
           (set-env-sp-offset! env initial-sp-offset)
           (set-env-fp-offset! env initial-fp-offset)
           (set-env-initialized! env #t))
         so-far-so-good?))))

  (catch #t go
    (lambda (x y fmt args . z)
      (debug 2 "parse-bytecode: ~a~%" (apply format #f fmt args))
      #f)))