;;;; Bytecode to IR compiler

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
;;; Compile list of bytecode operations to intermediate representation (IR) via
;;; Scheme in (almost) ANF.
;;;
;;; One of the main reasons to convert bytecode to ANF is to do floating point
;;; arithmetic efficiently. VM bytecodes uses integer index to refer locals.
;;; Those locals does not distinguish floating point values from other. In ANF
;;; format, it is possible to perform floating point arithmetic directly with
;;; unboxed value in floating point register inside loop.
;;;
;;; Code:

(define-module (system vm native tjit compile-ir)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language scheme spec)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system base compile)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit snapshot)
  #:export (trace->primlist
            trace->scm))


;;;
;;; Scheme ANF compiler
;;;

(define (trace->scm fragment exit-id loop? trace)
  (define-syntax root-trace?
    (identifier-syntax (not fragment)))
  (define (get-initial-snapshot-id)
    ;; For root trace, initial snapshot already added in `make-scm'.
    (if root-trace? 1 0))
  (define (get-initial-offset parent-snapshot)
    ;; Initial offset of root trace is constantly 0. Initial offset of side
    ;; trace is where parent trace left, using offset value from SNAPSHOT.
    (match parent-snapshot
      (($ $snapshot _ offset) offset)
      (_ 0)))

  (let* ((parent-snapshot
          (and fragment
               (hashq-ref (fragment-snapshots fragment) exit-id)))
         (initial-offset (get-initial-offset parent-snapshot))
         (past-frame (accumulate-locals initial-offset trace))
         (local-indices (past-frame-local-indices past-frame))
         (vars (make-vars local-indices))
         (lowest-offset (min initial-offset 0))
         (snapshots (make-hash-table))
         (snapshot-id (get-initial-snapshot-id)))

    (define (take-snapshot! ip dst-offset locals vars)
      (let-values (((ret snapshot)
                    (take-snapshot ip
                                   dst-offset
                                   locals vars
                                   snapshot-id
                                   initial-offset
                                   lowest-offset
                                   parent-snapshot
                                   past-frame)))
        (hashq-set! snapshots snapshot-id snapshot)
        (set! snapshot-id (+ snapshot-id 1))
        ret))

    (define (make-vars-from-parent vars
                                   locals-from-parent
                                   offset-from-parent)
      (let lp ((vars vars) (acc '()))
        (match vars
          (((n . var) . vars)
           ;; When parent-snapshot-offset is negative, side trace entered from a
           ;; side exit which is somewhere in the middle of bytecode of lower
           ;; frame than the beginning of the parent trace.
           (let ((m (if (< offset-from-parent 0)
                        (- n offset-from-parent)
                        n)))
             (if (assq-ref locals-from-parent m)
                 (lp vars (cons (cons n var) acc))
                 (lp vars acc))))
          (()
           (reverse! acc)))))

    (let* ((args (map make-var (reverse local-indices)))
           (initial-ip (cadr (car trace)))
           (initial-locals (list-ref (car trace) 4))
           (initial-nlocals (vector-length initial-locals))
           (parent-snapshot-locals (match parent-snapshot
                                     (($ $snapshot _ _ _ locals) locals)
                                     (_ #f)))
           (vars-from-parent (make-vars-from-parent vars
                                                    parent-snapshot-locals
                                                    initial-offset))
           (args-from-parent (reverse (map cdr vars-from-parent)))
           (local-indices-from-parent (map car vars-from-parent)))

      (define (add-initial-loads exp-body)
        (debug 3 ";;; add-initial-loads:~%")
        (debug 3 ";;;   initial-locals=~a~%" initial-locals)
        (let ((snapshot0 (hashq-ref snapshots 0)))
          (define (type-from-snapshot n)
            (let ((i (- n (snapshot-offset snapshot0))))
              (and (< -1 i)
                   (< i (vector-length initial-locals))
                   (type-of (vector-ref initial-locals i)))))
          (define (type-from-parent n)
            (assq-ref parent-snapshot-locals (if (<= 0 initial-offset)
                                                 n
                                                 (- n initial-offset))))
          (let lp ((vars (reverse vars)))
            (match vars
              (((n . var) . vars)
               (debug 3 ";;;   n:~a~%" n)
               (debug 3 ";;;   var: ~a~%" var)
               (debug 3 ";;;   from parent: ~a~%" (type-from-parent n))
               (debug 3 ";;;   from snapshot: ~a~%" (type-from-snapshot n))
               (cond
                ;; When local was passed from parent and snapshot 0 contained
                ;; the local with same type, no need to load from frame. If type
                ;; does not match, the value passed from parent has different
                ;; was untagged with different type, reload from frame.
                ;;
                ;; When locals index was found in parent snapshot locals and not
                ;; from snapshot 0 of this trace, the local will be passed from
                ;; parent fragment, ignoreing.
                ;;
                ;; If initial offset is positive and local index is negative,
                ;; locals from lower frame won't be passed as argument. Loading
                ;; later with '%fref' or '%fref/f'.
                ;;
                ((let ((parent-type (type-from-parent n))
                       (snapshot-type (type-from-snapshot n)))
                   (or (and parent-type
                            snapshot-type
                            (eq? parent-type snapshot-type))
                       (and (not snapshot-type)
                            parent-type)
                       (and (<= 0 initial-offset)
                            (< n 0))))
                 (lp vars))
                (else
                 (let* ((i (- n (snapshot-offset snapshot0)))
                        (local (if (and (< -1 i)
                                        (< i (vector-length initial-locals)))
                                   (vector-ref initial-locals i)
                                   (make-variable #f)))
                        (type (type-of local)))
                   (debug 3 ";;; add-initial-loads: n=~a~%" n)
                   (debug 3 ";;;   local:          ~a~%" local)
                   (debug 3 ";;;   type:           ~a~%" type)

                   ;; Shift the index when this trace started from negative
                   ;; offset. Skip loading from frame when shifted index is
                   ;; negative, should be loaded explicitly with `%fref' or
                   ;; `%fref/f'.
                   (let ((j (if (< initial-offset 0)
                                (- n initial-offset)
                                n)))
                     (if (< j 0)
                         (lp vars)
                         (with-frame-ref lp vars var type j)))))))
              (()
               exp-body)))))

      (define (make-scm escape trace)
        (let ((emit (lambda ()
                      (compile-ir trace
                                  escape
                                  loop?
                                  snapshot-id
                                  snapshots
                                  parent-snapshot
                                  past-frame
                                  vars
                                  initial-offset))))
          (cond
           (root-trace?
            (let* ((snapshot (make-snapshot 0 0 0 initial-nlocals initial-locals
                                            #f (reverse local-indices)
                                            past-frame initial-ip))
                   (_ (hashq-set! snapshots 0 snapshot))
                   (snap (take-snapshot! *ip-key-set-loop-info!*
                                         0 initial-locals vars)))
              `(letrec ((entry (lambda ()
                                 (let ((_ (%snap 0)))
                                   ,(add-initial-loads
                                     `(let ((_ ,snap))
                                        (loop ,@args))))))
                        (loop (lambda ,args
                                ,(emit))))
                 entry)))
           (loop?
            (let ((args-from-vars (reverse! (map cdr vars)))
                  (snap (take-snapshot! initial-ip 0 initial-locals
                                        vars-from-parent)))
              `(letrec ((entry (lambda ,args-from-parent
                                 (let ((_ ,snap))
                                   ,(add-initial-loads
                                     `(loop ,@args-from-vars)))))
                        (loop (lambda ,args-from-vars
                                ,(emit))))
                 entry)))
           (else
            (let ((snap (take-snapshot! initial-ip 0 initial-locals
                                        vars-from-parent)))
              `(letrec ((patch (lambda ,args-from-parent
                                 (let ((_ ,snap))
                                   ,(add-initial-loads
                                     (emit))))))
                 patch))))))

      (let ((scm (call-with-escape-continuation
                  (lambda (escape)
                    (make-scm escape trace)))))
        (debug 3 ";;; snapshot:~%~{;;;   ~a~%~}"
               (sort (hash-fold acons '() snapshots)
                     (lambda (a b) (< (car a) (car b)))))
        (let ((indices (if root-trace?
                           local-indices
                           local-indices-from-parent)))
          (values indices vars snapshots scm))))))

(define (trace->primlist trace-id fragment exit-id loop? trace)
  "Compiles TRACE to primlist.

If the trace to be compiles is a side trace, expects FRAGMENT as from parent
trace, and EXIT-ID is the hot exit id from the parent trace. LOOP? is a boolean
to indicate whether the trace contains loop or not."
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log trace-id)))
      (set-tjit-time-log-scm! log (get-internal-run-time))))
  (let-values (((locals vars snapshots scm)
                (trace->scm fragment exit-id loop? trace)))
    (when (tjit-dump-time? (tjit-dump-option))
      (let ((log (get-tjit-time-log trace-id)))
        (set-tjit-time-log-ops! log (get-internal-run-time))))
    (let* ((parent-snapshot (and fragment
                                 (hashq-ref (fragment-snapshots fragment)
                                            exit-id)))
           (initial-snapshot (hashq-ref snapshots 0))
           (plist (and scm
                       (anf->primlist parent-snapshot initial-snapshot
                                      vars scm))))
      (values locals snapshots scm plist))))
