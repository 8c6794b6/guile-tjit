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
;;; Compile list of bytecode operations to intermediate representation (IR) in
;;; (almost) A-normal form (ANF).
;;;
;;; One of the main reasons to convert bytecode to ANF is to do floating point
;;; arithmetic efficiently. VM bytecodes uses integer index to refer locals, and
;;; those locals does not distinguish floating point values from other. In ANF
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
  #:use-module (system vm native tjit state)
  #:export (compile-primops))


;;;
;;; Traced bytecode to ANF IR compiler
;;;

(define (trace->ir tj ir traces escape handle-interrupts?)
  (let* ((initial-nlocals (snapshot-nlocals (hashq-ref (ir-snapshots ir) 0)))
         (last-sp-offset (let* ((sp-offsets (past-frame-sp-offsets
                                             (tj-past-frame tj)))
                                (i (- (vector-length sp-offsets) 1)))
                           (vector-ref sp-offsets i)))
         (last-fp-offset (let* ((fp-offsets (past-frame-fp-offsets
                                             (tj-past-frame tj)))
                                (i (- (vector-length fp-offsets) 1)))
                           (vector-ref fp-offsets i))))
    (define (take-snapshot-in-entry! ir ip dst-offset locals sp-offset min-sp)
      (let-values (((ret snapshot)
                    (take-snapshot ip
                                   dst-offset
                                   locals
                                   (ir-vars ir)
                                   (ir-snapshot-id ir)
                                   sp-offset
                                   last-fp-offset
                                   min-sp
                                   (ir-max-sp-offset ir)
                                   (tj-parent-snapshot tj)
                                   (tj-past-frame tj))))
        (let ((old-id (ir-snapshot-id ir)))
          (hashq-set! (ir-snapshots ir) old-id snapshot)
          (set-ir-snapshot-id! ir (+ old-id 1))
          ret)))
    (define (convert-one ir op ip ra locals rest)
      (cond
       ((hashq-ref *ir-procedures* (car op))
        => (lambda (proc)
             (let ((next
                    (lambda ()
                      (let* ((old-index (ir-bytecode-index ir))
                             (sp-offsets (past-frame-sp-offsets
                                          (ir-past-frame ir)))
                             (sp-offset (vector-ref sp-offsets old-index))
                             (fp-offsets (past-frame-fp-offsets
                                          (ir-past-frame ir)))
                             (fp-offset (vector-ref fp-offsets old-index))
                             (nlocals (vector-length locals))
                             (max-offset (get-max-sp-offset sp-offset
                                                            fp-offset
                                                            nlocals)))
                        (set-ir-bytecode-index! ir (+ old-index 1))
                        (when (< sp-offset (ir-min-sp-offset ir))
                          (set-ir-min-sp-offset! ir sp-offset))
                        (when (< (ir-max-sp-offset ir) max-offset)
                          (set-ir-max-sp-offset! ir max-offset)))
                      (convert ir rest))))
               (apply proc
                      ir handle-interrupts? next escape
                      ip ra locals (cdr op)))))
       (else
        (debug 2 "*** IR: NYI ~a~%" (car op))
        (escape #f))))
    (define (convert ir trace)
      ;; Last operation is wrapped in a thunk, to assign snapshot ID
      ;; in last expression after taking snapshots from guards in
      ;; traced operations.
      ;;
      ;; Trace with loop will emit `loop', which is the name of
      ;; procedure for looping the body of Scheme IR emitted in
      ;; `make-scm'.
      ;;
      ;; Side trace or loop-less root trace are capturing variables
      ;; with `take-snapshot!' at the end, so that the machine code
      ;; can pass the register information to linked code.
      ;;
      (define (gen-last-op op ip ra locals)
        (define (dr-locals proc nlocals)
          (let lp ((n 0) (end (vector-length locals)) (acc '()))
            (if (= n nlocals)
                (list->vector acc)
                (let* ((i (- end proc n 1))
                       (e (vector-ref locals i)))
                  (lp (+ n 1) end (cons e acc))))))
        (cond
         ((tj-downrec? tj)
          (match op
            (('call proc nlocals)
             (lambda ()
               (let* ((next-sp (- last-fp-offset proc nlocals))
                      (sp-shift (if (tj-parent-fragment tj)
                                    (length (fragment-loop-locals
                                             (tj-parent-fragment tj)))
                                    initial-nlocals))
                      (next-sp-offset (+ next-sp sp-shift)))
                 `(let ((_ ,(take-snapshot-in-entry! ir
                                                     *ip-key-downrec*
                                                     0
                                                     (dr-locals proc nlocals)
                                                     next-sp-offset
                                                     next-sp-offset)))
                    (loop ,@(reverse (map cdr (ir-vars ir))))))))
            (('call-label . _)
             ;; XXX: TODO.
             (escape #f))
            (_
             (escape #f))))
         ((tj-uprec? tj)
          (match op
            (('return-values n)
             (lambda ()
               (let* ((next-sp-offset last-sp-offset))
                 `(let ((_ ,(take-snapshot-in-entry! ir
                                                     *ip-key-uprec*
                                                     0
                                                     locals
                                                     next-sp-offset
                                                     (ir-min-sp-offset ir))))
                    (loop ,@(reverse (map cdr (ir-vars ir))))))))
            (_
             (escape #f))))
         ((tj-loop? tj)
          (lambda ()
            `(loop ,@(reverse (map cdr (ir-vars ir))))))
         (else
          (lambda ()
            `(let ((_ ,(take-snapshot-in-entry! ir
                                                *ip-key-link*
                                                0
                                                locals
                                                last-sp-offset
                                                (ir-min-sp-offset ir))))
               _)))))
      (match trace
        (((op ip ra locals) . ())
         (let ((last-op (gen-last-op op ip ra locals)))
           (convert-one ir op ip ra locals last-op)))
        (((op ip ra locals) . rest)
         (convert-one ir op ip ra locals rest))
        (last-op
         (or (and (procedure? last-op)
                  (last-op))
             (error "trace->ir: last arg was not a procedure" last-op)))))
    (convert ir traces)))

(define (compile-anf tj trace)
  (define root-trace?
    (not (tj-parent-fragment tj)))
  (define (get-initial-snapshot-id)
    ;; For root trace, initial snapshot already added in `make-anf'.
    (if root-trace? 1 0))
  (let* ((parent-snapshot (tj-parent-snapshot tj))
         (initial-sp-offset (get-initial-sp-offset parent-snapshot))
         (initial-fp-offset (get-initial-fp-offset parent-snapshot))
         (local-indices (past-frame-local-indices (tj-past-frame tj)))
         (vars (make-vars local-indices))
         (lowest-offset (min initial-sp-offset 0))
         (snapshots (make-hash-table))
         (snapshot-id (get-initial-snapshot-id)))

    (define (take-snapshot! ip dst-offset locals vars)
      (let*-values (((ret snapshot)
                     (take-snapshot ip
                                    dst-offset
                                    locals
                                    vars
                                    snapshot-id
                                    initial-sp-offset
                                    initial-fp-offset
                                    lowest-offset
                                    (get-max-sp-offset initial-sp-offset
                                                       initial-fp-offset
                                                       (vector-length locals))
                                    parent-snapshot
                                    (tj-past-frame tj))))
        (hashq-set! snapshots snapshot-id snapshot)
        (set! snapshot-id (+ snapshot-id 1))
        ret))

    (define (make-vars-from-parent vars locals-from-parent
                                   sp-offset-from-parent)
      (let lp ((vars vars) (acc '()))
        (match vars
          (((n . var) . vars)
           (if (assq-ref locals-from-parent n)
               (lp vars (cons (cons n var) acc))
               (lp vars acc)))
          (()
           (reverse! acc)))))

    (let* ((args (map make-var (reverse local-indices)))
           (initial-trace (car trace))
           (initial-ip (cadr initial-trace))
           (initial-locals (cadddr initial-trace))
           (initial-nlocals (vector-length initial-locals))
           (parent-snapshot-locals (match parent-snapshot
                                     (($ $snapshot _ _ _ _ locals) locals)
                                     (_ #f)))
           (vars-from-parent (make-vars-from-parent vars
                                                    parent-snapshot-locals
                                                    initial-sp-offset))
           (args-from-parent (reverse (map cdr vars-from-parent)))
           (local-indices-from-parent (map car vars-from-parent))
           (handle-interrupts? (make-variable #f)))

      (define (add-initial-loads exp-body)
        (debug 3 ";;; add-initial-loads:~%")
        (debug 3 ";;;   initial-locals=~a~%" initial-locals)
        (debug 3 ";;;   parent-snapshot-locals=~a~%" parent-snapshot-locals)
        (let ((snapshot0 (hashq-ref snapshots 0)))
          (define (type-from-snapshot n)
            (let ((i (- n (snapshot-sp-offset snapshot0))))
              (and (< -1 i (vector-length initial-locals))
                   (type-of (vector-ref initial-locals i)))))
          (define (type-from-parent n)
            (assq-ref parent-snapshot-locals n))
          (let lp ((vars (reverse vars)))
            (match vars
              (((n . var) . vars)
               (debug 3 ";;; add-initial-loads: n=~a~%" n)
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
                   (and (not (tj-loop? tj))
                        (or (and parent-type
                                 snapshot-type
                                 (eq? parent-type snapshot-type))
                            (and (not snapshot-type)
                                 parent-type)
                            (and (<= 0 initial-sp-offset)
                                 (< n 0)))))
                 (lp vars))
                ((let ((i (- n (snapshot-sp-offset snapshot0))))
                   (not (< -1 i (vector-length initial-locals))))
                 (lp vars))
                (else
                 (let* ((i (- n (snapshot-sp-offset snapshot0)))
                        (local (vector-ref initial-locals i))
                        (type (type-of local)))
                   (debug 3 ";;;   local:          ~a~%" local)
                   (debug 3 ";;;   type:           ~a~%" type)

                   ;; Shift the index when this trace started from negative
                   ;; offset. Skip loading from frame when shifted index is
                   ;; negative, should be loaded explicitly with `%fref' or
                   ;; `%fref/f'.
                   (let ((j (+ n initial-sp-offset)))
                     (if (< j 0)
                         (lp vars)
                         (with-frame-ref lp vars var type j)))))))
              (()
               exp-body)))))

      (define (make-anf escape)
        (let ((emit (lambda ()
                      (let* ((initial-nlocals
                              (snapshot-nlocals (hashq-ref snapshots 0)))
                             (ir (make-ir snapshots
                                          snapshot-id
                                          (tj-parent-snapshot tj)
                                          vars
                                          (min initial-sp-offset 0)
                                          (get-max-sp-offset initial-sp-offset
                                                             initial-fp-offset
                                                             initial-nlocals)
                                          0
                                          (tj-past-frame tj))))
                        (trace->ir tj ir trace escape handle-interrupts?)))))
          (cond
           (root-trace?
            (let* ((arg-indices (filter (lambda (n)
                                          (if (tj-uprec? tj)
                                              (<= 0 n (- initial-nlocals 1))
                                              (<= 0 n)))
                                        (reverse local-indices)))
                   (snapshot (make-snapshot 0
                                            0
                                            0
                                            initial-nlocals
                                            initial-locals
                                            #f
                                            arg-indices
                                            (tj-past-frame tj)
                                            initial-ip))
                   (_ (hashq-set! snapshots 0 snapshot))
                   (snap (take-snapshot! *ip-key-set-loop-info!*
                                         0
                                         initial-locals
                                         vars)))
              `(letrec ((entry (lambda ()
                                 (let ((_ (%snap 0)))
                                   ,(add-initial-loads
                                     `(let ((_ ,snap))
                                        (loop ,@args))))))
                        (loop (lambda ,args
                                ,(emit))))
                 entry)))
           ((tj-loop? tj)
            (let ((args-from-vars (reverse! (map cdr vars)))
                  (snap (take-snapshot! initial-ip
                                        0
                                        initial-locals
                                        vars-from-parent)))
              `(letrec ((entry (lambda ,args-from-parent
                                 (let ((_ ,snap))
                                   ,(add-initial-loads
                                     `(loop ,@args-from-vars)))))
                        (loop (lambda ,args-from-vars
                                ,(emit))))
                 entry)))
           (else
            (let ((snap (take-snapshot! initial-ip
                                        0
                                        initial-locals
                                        vars-from-parent)))
              `(letrec ((patch (lambda ,args-from-parent
                                 (let ((_ ,snap))
                                   ,(add-initial-loads
                                     (emit))))))
                 patch))))))

      (let ((anf (call-with-escape-continuation make-anf)))
        (debug 3 ";;; snapshot:~%~{;;;   ~a~%~}"
               (sort (hash-fold acons '() snapshots)
                     (lambda (a b) (< (car a) (car b)))))
        (let ((indices (if root-trace?
                           local-indices
                           local-indices-from-parent)))
          (values vars snapshots anf (variable-ref handle-interrupts?)))))))

(define (compile-primops tj trace)
  "Compiles TRACE to primops with TJ and TRACE."
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log (tj-id tj))))
      (set-tjit-time-log-scm! log (get-internal-run-time))))
  (let-values (((vars snapshots anf handle-interrupts?)
                (compile-anf tj trace)))
    (when (tjit-dump-time? (tjit-dump-option))
      (let ((log (get-tjit-time-log (tj-id tj))))
        (set-tjit-time-log-ops! log (get-internal-run-time))))
    (let ((primops (if anf
                       (ir->primops (tj-parent-snapshot tj)
                                    (hashq-ref snapshots 0)
                                    vars
                                    handle-interrupts?
                                    snapshots
                                    anf)
                       #f)))
      (values snapshots anf primops))))
