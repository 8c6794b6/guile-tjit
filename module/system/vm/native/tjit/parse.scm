;;;; Parse bytecode with recorded data

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
;;; Parse bytecode and initialize environment. This is the first phase of the
;;; whole compiler workflow.
;;;
;;; Code:

(define-module (system vm native tjit parse)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:export (parse-bytecode *scan-procedures*))

;;;
;;; Exported hash tablel
;;;

(define *scan-procedures*
  (make-hash-table 255))


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
  (define disassemble-one
    (@@ (system vm disassembler) disassemble-one))
  (define last-locals
    (and (pair? traces) (cadddr (car traces))))
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
    (define-syntax-rule (nyi)
      (begin
        (debug 1 "NYI: ~a~%" (car op))
        #f))
    (debug 2 ";;; [parse] call=~a return=~a op=~a~%"
           (env-call-num env) (env-return-num env) op)
    (match (hashq-ref *scan-procedures* (car op))
      ((? list? procs)
       (let lp ((procs procs))
         (match procs
           (((test . work) . procs)
            (if (apply test (list op locals))
                (apply work env ip dl locals (cdr op))
                (lp procs)))
           (_ (nyi)))))
      (_ (nyi))))
  (define (go)
    (let lp ((acc '()) (offset 0) (traces (reverse! traces))
             (so-far-so-good? #t))
      (match traces
        ((trace . traces)
         (match trace
           ((ip ra dl locals)
            (let*-values
                (((len op) (disassemble-one bytecode offset))
                 ((implemented?)
                  (if so-far-so-good?
                      (let* ((ret (scan-trace env op ip dl locals))
                             (_ (infer-type env op ip dl locals))
                             (ws (map car (env-inferred-types env)))
                             (buf (env-write-buf env))
                             (buf (cons (sort ws <) buf)))
                        (increment-env-call-return-num! env op)
                        (set-env-write-buf! env buf)
                        ret)
                      #f)))
              (lp (cons (cons op trace) acc) (+ offset len) traces
                  implemented?)))
           (_ (error "malformed trace" trace))))
        (()
         (let* ((linked-ip (env-linked-ip env))
                (last-sp-offset (env-sp-offset env))
                (inferred
                 (let lp ((inferred (env-inferred-types env)) (acc '()))
                   (match inferred
                     (((n . t) . inferred)
                      (lp inferred (cons (cons (- n last-sp-offset) t) acc)))
                     (() acc))))
                (linked-fragment
                 (if linked-ip
                     (get-root-trace inferred last-locals linked-ip)
                     #f))
                (linking-roots?
                 ;; Detecting root trace linkage by chasing parent id until it
                 ;; reaches to root trace and compare it with linked trace. This
                 ;; loop could be avoided by saving the origin trace id in
                 ;; fragment record type.
                 (let ((origin-id (and=> (get-origin-fragment
                                          (env-parent-fragment env))
                                         fragment-id))
                       (linked-id (and=> linked-fragment fragment-id)))
                   (and origin-id linked-id
                        (not (eq? origin-id linked-id)))))
                (depth (or (and=> (env-parent-snapshot env)
                                  snapshot-inline-depth)
                           0)))
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
         (values (reverse! acc) so-far-so-good?)))))

  (catch #t go
    (lambda (x y fmt args . z)
      (debug 2 "parse-bytecode: ~a~%" (apply format #f fmt args))
      (values '() #f))))
