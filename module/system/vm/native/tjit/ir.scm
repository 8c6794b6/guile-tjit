;;;; Definitions for compiling to IR

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
;;; A module containing definition of operations to compile bytecode to ANF IR
;;; used in vm-tjit.
;;;
;;; Code:

(define-module (system vm native tjit ir)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm program)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables)
  #:export (make-ir
            <ir>
            ir-snapshots
            ir-snapshot-id set-ir-snapshot-id!
            ir-min-sp-offset set-ir-min-sp-offset!
            ir-max-sp-offset set-ir-max-sp-offset!
            ir-bytecode-index set-ir-bytecode-index!
            ir-vars
            ir-return-subr? set-ir-return-subr!

            define-ir
            define-interrupt-ir
            gen-put-element-type
            gen-put-index
            dereference-scm
            take-snapshot!
            scm-ref
            u64-ref
            var-ref
            type-ref
            with-boxing
            with-type-guard
            current-sp-offset
            current-fp-offset
            tj outline ir ip ra dl locals next

            make-var
            make-vars
            get-max-sp-offset
            get-initial-sp-offset
            get-initial-fp-offset
            take-snapshot
            gen-load-thunk
            with-frame-ref

            define-anf

            define-scan
            scan-trace
            push-scan-sp-offset!
            pop-scan-sp-offset!
            push-scan-fp-offset!
            pop-scan-fp-offset!
            set-scan-initial-fields!

            define-ti
            infer-type

            *scan-procedures*
            *ir-procedures*
            *ti-procedures*))

;;;
;;; IR record type
;;;

(define-record-type <ir>
  (make-ir snapshots snapshot-id vars min-sp-offset max-sp-offset
           bytecode-index return-subr?)
  ir?

  ;; Hash table containing snapshots.
  (snapshots ir-snapshots)

  ;; Current snapshot ID.
  (snapshot-id ir-snapshot-id set-ir-snapshot-id!)

  ;; List of symbols for variables.
  (vars ir-vars)

  ;; Current minimum SP offset.
  (min-sp-offset ir-min-sp-offset set-ir-min-sp-offset!)

  ;; Current maximum SP offset.
  (max-sp-offset ir-max-sp-offset set-ir-max-sp-offset!)

  ;; Current bytecode index.
  (bytecode-index ir-bytecode-index set-ir-bytecode-index!)

  ;; Flag for subr call.
  (return-subr? ir-return-subr? set-ir-return-subr!))


;;;
;;; Exported hash tablels
;;;

(define *scan-procedures*
  (make-hash-table 255))

(define *ti-procedures*
  (make-hash-table 255))

(define *ir-procedures*
  (make-hash-table 255))


;;;
;;; Macros for scan
;;;

(define-syntax-rule (define-scan (name args ...) . body)
  (let ((test-proc (lambda (op locals)
                     #t))
        (scan-proc (lambda (%ip %dl %locals %outline args ...)
                     (syntax-parameterize
                         ((ip (identifier-syntax %ip))
                          (dl (identifier-syntax %dl))
                          (locals (identifier-syntax %locals))
                          (outline (identifier-syntax %outline)))
                       . body)
                     #t)))
    (hashq-set! *scan-procedures* 'name (list (cons test-proc scan-proc)))))

(define (scan-trace ol op ip dl locals)
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
  (debug 2 ";;; [scan-trace] op=~a~%" op)
  (match (hashq-ref *scan-procedures* (car op))
    ((? list? procs)
     (let lp ((procs procs))
       (match procs
         (((test . work) . procs)
          (if (apply test (list op locals))
              (apply work ip dl locals ol (cdr op))
              (lp procs)))
         (_ (nyi)))))
    (_ (nyi))))

(define-syntax-rule (push-scan-sp-offset! ol n)
  (set-outline-sp-offset! ol (- (outline-sp-offset ol) n)))

(define-syntax-rule (pop-scan-sp-offset! ol n)
  (set-outline-sp-offset! ol (+ (outline-sp-offset ol) n)))

(define-syntax-rule (push-scan-fp-offset! ol n)
  (set-outline-fp-offset! ol (- (outline-fp-offset ol) n)))

(define-syntax-rule (pop-scan-fp-offset! ol n)
  (set-outline-fp-offset! ol (+ (outline-fp-offset ol) n)))

(define-syntax-rule (set-scan-initial-fields! ol)
  (let ((new-sp-offsets (cons (outline-sp-offset ol)
                              (outline-sp-offsets ol)))
        (new-fp-offsets (cons (outline-fp-offset ol)
                              (outline-fp-offsets ol))))
    (set-outline-sp-offsets! ol new-sp-offsets)
    (set-outline-fp-offsets! ol new-fp-offsets)))


;;;
;;; Macro for ANF
;;;

(define-syntax-rule (define-anf (name arg ...) . body)
  (let ((test-proc (lambda (op locals)
                     #t))
        (anf-proc (lambda (%tj %outline %ir %next %ip %ra %dl %locals arg ...)
                    (syntax-parameterize
                        ((tj (identifier-syntax %tj))
                         (outline (identifier-syntax %outline))
                         (ir (identifier-syntax %ir))
                         (next (identifier-syntax %next))
                         (ip (identifier-syntax %ip))
                         (ra (identifier-syntax %ra))
                         (dl (identifier-syntax %dl))
                         (locals (identifier-syntax %locals)))
                      . body))))
    (hashq-set! *ir-procedures* 'name (list (cons test-proc anf-proc)))))


;;;
;;; Macros for type inference
;;;

(define-syntax-rule (define-ti (name args ...) . body)
  (let ((test-proc (lambda (op locals)
                     #t))
        (ti-proc (lambda (%ip %dl %locals %outline args ...)
                   (syntax-parameterize
                       ((ip (identifier-syntax %ip))
                        (dl (identifier-syntax %dl))
                        (locals (identifier-syntax %locals))
                        (outline (identifier-syntax %outline)))
                     . body))))
    (hashq-set! *ti-procedures* 'name (list (cons test-proc ti-proc)))))

(define (infer-type ol op ip dl locals)
  (match (hashq-ref *ti-procedures* (car op))
    ((? list? procs)
     (let lp ((procs procs))
       (match procs
         (((test . work) . procs)
          (if (apply test (list op locals))
              (apply work ip dl locals ol (cdr op))
              (lp procs)))
         (() (values)))))
    (_ (values))))


;;;
;;; Macros for IR
;;;

(define (make-var index)
  (string->symbol (string-append "v" (number->string index))))

(define (make-vars locals)
  ;; Might better to use other data structure than alist for variables.
  ;; Number of variables won't change after getting the number of locals from
  ;; `accumulate-locals'.
  (map (lambda (n)
         (cons n (make-var n)))
       locals))

(define (get-max-sp-offset sp-offset fp-offset nlocals)
  (max fp-offset
       (- (+ sp-offset nlocals) 1)
       (if (< fp-offset 0)
           (- (+ (- fp-offset) nlocals) 1)
           0)))

(define (get-initial-sp-offset parent-snapshot)
  ;; Initial offset of root trace is constantly 0. Initial offset of side
  ;; trace is where parent trace left, using offset value from SNAPSHOT.
  (match parent-snapshot
    (($ $snapshot _ sp-offset) sp-offset)
    (_ 0)))

(define (get-initial-fp-offset parent-snapshot)
  ;; Initial offset of root trace is constantly 0. Initial offset of side
  ;; trace is where parent trace left, using offset value from SNAPSHOT.
  (match parent-snapshot
    (($ $snapshot _ _ fp-offset) fp-offset)
    (_ 0)))

(define* (take-snapshot ip dst-offset locals vars indices id sp-offset fp-offset
                        min-sp-offset max-sp-offset outline
                        #:optional (refill? #f))
  (let* ((nlocals (vector-length locals))
         (dst-ip (+ ip (* dst-offset 4)))
         (indices (filter (lambda (i)
                            (<= min-sp-offset i max-sp-offset))
                          indices))
         (args (let lp ((vars vars) (acc '()))
                 (match vars
                   (((n . var) . vars)
                    (if (memq n indices)
                        (lp vars (cons var acc))
                        (lp vars acc)))
                   (()
                    (if refill?
                        (append acc (list (make-var (+ sp-offset nlocals))
                                          (make-var (+ sp-offset nlocals 1))))
                        acc)))))
         (snapshot (make-snapshot id sp-offset fp-offset nlocals indices
                                  outline dst-ip refill?)))
    (values `(%snap ,id ,@args) snapshot)))

(define-syntax gen-load-thunk
  (syntax-rules ()
    ((_ proc nlocals skip-var?)
     (let* ((return-subr? (ir-return-subr? ir))
            (stack-size (vector-length locals))
            (sp-offset (current-sp-offset))
            (min-local-index (+ (- stack-size proc 1) sp-offset 2))
            (max-local-index (+ stack-size sp-offset))
            (live-indices (outline-live-indices outline))
            (acc (make-hash-table))
            (load-down-frame
             (lambda ()
               (let lp ((vars (reverse (ir-vars ir))))
                 (match vars
                   (((n . var) . vars)
                    (cond
                     ((or (skip-var? var)
                          (memq n live-indices))
                      (lp vars))
                     ((< (- stack-size nlocals) n (ir-min-sp-offset ir))
                      (if (not (outline-parent-snapshot outline))
                          (nyi "root trace loading down frame")
                          (with-frame-ref vars var #f n lp)))
                     (else
                      (lp vars))))
                   (()
                    (let* ((live-indices
                            (sort (hash-fold (lambda (k v acc)
                                               (if (memq k acc)
                                                   acc
                                                   (cons k acc)))
                                             live-indices
                                             acc)
                                  <)))
                      (debug 2 ";;; [gen-load-thunk] live-indices=~a~%"
                             live-indices)
                      (set-outline-live-indices! outline live-indices)
                      (next)))))))
            (load-up-frame
             (lambda ()
               ;; Ignoring `unspecified' values when loading from previous
               ;; frame. Those values might came from dead slots in stack which
               ;; were overwritten by gc. See `scm_i_vm_mark_stack' in
               ;; "libguile/vm.c".
               ;;
               ;; XXX: Add tests to check that this strategy works with
               ;; explicitly given `unspecified' values.
               ;;
               (let lp ((vars (reverse (ir-vars ir))))
                 (match vars
                   (((n . var) . vars)
                    (debug 2 ";;; [load-up-frame] n=~s" n)
                    (cond
                     ((or (skip-var? var)
                          (memq n live-indices))
                      (debug 2 " skipping~%")
                      (lp vars))
                     ((< min-local-index n max-local-index)
                      (let* ((entries (outline-entry-types outline))
                             (t (assq-ref entries n)))
                        (debug 2 " t=~a~%" (pretty-type t))
                        (if (eq? t &unspecified)
                            (lp vars)
                            (begin
                              (hashq-set! acc n var)
                              (with-frame-ref vars var t n lp)))))
                     (else
                      (debug 2 " skipping~%")
                      (lp vars))))
                   (()
                    (load-down-frame)))))))
       (lambda ()
         (when (and (not (outline-parent-snapshot outline))
                    (< 0 (current-fp-offset)))
           (nyi "root trace with up-frame load"))
         (set-ir-return-subr! ir #f)
         (if (or (<= (current-fp-offset) 0)
                 return-subr?)
             (next)
             (load-up-frame)))))))

(define-syntax-rule (with-frame-ref args var type idx next)
  (cond
   ((dynamic-link? type)
    `(let ((,var ,(dynamic-link-offset type)))
       ,(next args)))
   ((return-address? type)
    `(let ((,var ,(pointer-address (return-address-ip type))))
       ,(next args)))
   ((or (eq? type &flonum)
        (eq? type &f64))
    `(let ((,var (%fref/f ,idx ,type)))
       ,(next args)))
   (else
    `(let ((,var (%fref ,idx ,type)))
       ,(next args)))))

(define-syntax define-ir-syntax-parameters
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define-syntax-parameter name
         (lambda (x)
           'name "uninitialized" x))
       ...))))

(define-ir-syntax-parameters tj outline ir ip ra dl locals next)

(define-syntax-rule (gen-entry-type ol ty arg rest)
  (let ((i (+ arg (outline-sp-offset ol))))
    (set-entry-type! ol i ty)
    (gen-scan-type ol . rest)))

(define-syntax gen-scan-type
  (syntax-rules (scm fixnum flonum pair vector box bytevector u64 f64)
    ((_ ol) (values))
    ((_ ol (scm arg) . rest) (gen-entry-type ol &scm arg rest))
    ((_ ol (fixnum arg) . rest) (gen-entry-type ol &fixnum arg rest))
    ((_ ol (flonum arg) . rest) (gen-entry-type ol &flonum arg rest))
    ((_ ol (pair arg) . rest) (gen-entry-type ol &pair arg rest))
    ((_ ol (vector arg) . rest) (gen-entry-type ol &vector arg rest))
    ((_ ol (box arg) . rest) (gen-entry-type ol &box arg rest))
    ((_ ol (bytevector arg) . rest) (gen-entry-type ol &bytevector arg rest))
    ((_ ol (u64 arg) . rest) (gen-entry-type ol &u64 arg rest))
    ((_ ol (f64 arg) . rest) (gen-entry-type ol &f64 arg rest))
    ((_ ol (other arg) . rest) (gen-scan-type ol . rest))))

(define-syntax gen-infer-type
  (syntax-rules (scm! fixnum! flonum! pair! vector! box! u64! f64!)
    ((_ ol (scm! arg) . rest)
     (set-inferred-type! ol (+ arg (outline-sp-offset ol)) &scm))
    ((_ ol (fixnum! arg) . rest)
     (set-inferred-type! ol (+ arg (outline-sp-offset ol)) &fixnum))
    ((_ ol (flonum! arg) . rest)
     (set-inferred-type! ol (+ arg (outline-sp-offset ol)) &flonum))
    ((_ ol (pair! arg) . rest)
     (set-inferred-type! ol (+ arg (outline-sp-offset ol)) &pair))
    ((_ ol (vector! arg) . rest)
     (set-inferred-type! ol (+ arg (outline-sp-offset ol)) &vector))
    ((_ ol (box! arg) . rest)
     (set-inferred-type! ol (+ arg (outline-sp-offset ol)) &box))
    ((_ ol (u64! arg) . rest)
     (set-inferred-type! ol (+ arg (outline-sp-offset ol)) &u64))
    ((_ ol (f64! arg) . rest)
     (set-inferred-type! ol (+ arg (outline-sp-offset ol)) &f64))
    ((_ ol . other) (values))))

(define-syntax define-ir
  (syntax-rules ()
    "Defines procedure to compile bytecode operation to IR, and optionally
defines procedure for scanning locals and types. E.g:

  (define-ir (add (fixnum! dst) (fixnum a) (fixnum b))
    ...)

will define two procedures: one for IR compilation taking three arguments, and
another procedure for scanning locals and types. The procedure for scanner saves
index referenced by dst, a, and b values at runtime."
    ((_ (name (flag arg) ...) . body)
     (let ((test-proc
            (lambda (op locals)
              (let lp ((flags '(flag ...)) (ns (cdr op)))
                (match (cons flags ns)
                  (((f . flags) . (n . ns))
                   (if (memq f '(fixnum
                                 flonum procedure pair vector
                                 struct string bytevector array))
                       (let* ((v (vector-ref locals n))
                              (t (type-of v)))
                         (if (eq? t (flag->type f))
                             (lp flags ns)
                             #f))
                       (lp flags ns)))
                  (_ #t)))))
           (scan-proc
            (lambda (%ip %dl %locals %outline arg ...)
              (gen-scan-type %outline (flag arg) ...)
              (set-scan-initial-fields! %outline)
              #t))
           (ti-proc
            (lambda (%ip %dl %locals %outline arg ...)
              (gen-infer-type %outline (flag arg) ...)))
           (anf-proc
            (lambda (%tj %outline %ir %next %ip %ra %dl %locals arg ...)
              (syntax-parameterize
                  ((tj (identifier-syntax %tj))
                   (outline (identifier-syntax %outline))
                   (ir (identifier-syntax %ir))
                   (next (identifier-syntax %next))
                   (ip (identifier-syntax %ip))
                   (ra (identifier-syntax %ra))
                   (dl (identifier-syntax %dl))
                   (locals (identifier-syntax %locals)))
                . body))))
       (let ((add-proc! (lambda (tbl proc)
                          (let* ((elem (cons test-proc proc))
                                 (val (cond ((hashq-ref tbl 'name)
                                             => (lambda (found)
                                                  (cons elem found)))
                                            (else (list elem)))))
                            (hashq-set! tbl 'name val)))))
         (add-proc! *scan-procedures* scan-proc)
         (add-proc! *ti-procedures* ti-proc)
         (add-proc! *ir-procedures* anf-proc))))))

(define-syntax define-interrupt-ir
  (syntax-rules ()
    ((_ names-and-args . body)
     (define-ir names-and-args
       (begin
         (set-outline-handle-interrupts! outline #t)
         . body)))))

(define-syntax-rule (dereference-scm addr)
  (pointer->scm (dereference-pointer (make-pointer addr))))

(define-syntax-rule (current-sp-offset)
  (vector-ref (outline-sp-offsets outline) (ir-bytecode-index ir)))

(define-syntax-rule (current-fp-offset)
  (vector-ref (outline-fp-offsets outline) (ir-bytecode-index ir)))

(define-syntax-rule (scm-ref n)
  (vector-ref locals n))

(define-syntax-rule (u64-ref n)
  (pointer-address (scm->pointer (vector-ref locals n))))

(define-syntax-rule (var-ref n)
  (assq-ref (ir-vars ir) (+ n (current-sp-offset))))

(define-syntax-rule (type-ref n)
  (assq-ref (outline-inferred-types outline) (+ n (current-sp-offset))))

(define-syntax take-snapshot!
  (syntax-rules ()
    ((_ ip dst-offset)
     (take-snapshot! ip dst-offset #f))
    ((_ ip dst-offset refill?)
     (let-values (((ret snapshot)
                   (take-snapshot ip dst-offset locals (ir-vars ir)
                                  (if (and (outline-parent-snapshot outline)
                                           (not (outline-loop? outline)))
                                      (vector-ref
                                       (outline-write-buf outline)
                                       (ir-bytecode-index ir))
                                      (outline-write-indices outline))
                                  (ir-snapshot-id ir)
                                  (current-sp-offset) (current-fp-offset)
                                  (ir-min-sp-offset ir) (ir-max-sp-offset ir)
                                  outline refill?)))
       (let ((old-id (ir-snapshot-id ir)))
         (hashq-set! (ir-snapshots ir) old-id snapshot)
         (set-ir-snapshot-id! ir (+ old-id 1)))
       ret))))

(define-syntax-rule (with-boxing type var tmp proc)
  (cond
   ((eq? type &flonum)
    (set-outline-handle-interrupts! outline #t)
    `(let ((,tmp (%d2s ,var)))
       ,(proc tmp)))
   (else
    (proc var))))

(define-syntax-rule (with-type-guard type src thunk)
  `(let ((_ ,(take-snapshot! ip 0)))
     (let ((_ (%typeq ,src ,type)))
       ,(thunk))))

;;; *** The dynamic environment

;; XXX: prompt
;; XXX: wind
;; XXX: unwind
;; XXX: push-fluid
;; XXX: pop-fluid
;; XXX: fluid-ref
;; XXX: fluid-set

;;; *** Strings, symbols, and keywords

;; XXX: string-length
;; XXX: string-ref
;; XXX: string->number
;; XXX: string->symbol
;; XXX: symbol->keyword

;;; *** Structs and GOOPS

;; XXX: struct-vtable
;; XXX: allocate-struct
;; XXX: struct-ref
;; XXX: struct-set!
;; XXX: allocate-struct/immediate
;; XXX: struct-ref/immediate
;; XXX: struct-set!/immediate
;; XXX: class-of
