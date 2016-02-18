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
  #:use-module (system vm native tjit state)
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
            ir-tj
            ir-parent-snapshot
            ir-outline
            ir-loop?

            define-ir
            define-interrupt-ir
            gen-put-element-type
            gen-put-index
            dereference-scm
            take-snapshot!
            scm-ref
            u64-ref
            var-ref
            ty-ref
            with-boxing
            with-unboxing
            current-sp-offset
            current-fp-offset
            ir ip ra dl locals next

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
            push-scan-sp-offset!
            pop-scan-sp-offset!
            push-scan-fp-offset!
            pop-scan-fp-offset!
            set-scan-write!
            set-scan-read!
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
           bytecode-index return-subr? tj)
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
  (return-subr? ir-return-subr? set-ir-return-subr!)

  ;; Tjit state for this ir.
  (tj ir-tj))

(define (ir-parent-snapshot ir)
  (tj-parent-snapshot (ir-tj ir)))

(define (ir-outline ir)
  (tj-outline (ir-tj ir)))

(define (ir-loop? ir)
  (tj-loop? (ir-tj ir)))


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
        (scan-proc (lambda (%ip %dl %locals args ...)
                     (syntax-parameterize
                         ((ip (identifier-syntax %ip))
                          (dl (identifier-syntax %dl))
                          (locals (identifier-syntax %locals)))
                       . body)
                     #t)))
    (hashq-set! *scan-procedures* 'name (list (cons test-proc scan-proc)))))

(define-syntax-rule (push-scan-sp-offset! ol n)
  (unless (outline-initialized? ol)
    (set-outline-sp-offset! ol (- (outline-sp-offset ol) n))))

(define-syntax-rule (pop-scan-sp-offset! ol n)
  (unless (outline-initialized? ol)
    (set-outline-sp-offset! ol (+ (outline-sp-offset ol) n))))

(define-syntax-rule (push-scan-fp-offset! ol n)
  (unless (outline-initialized? ol)
    (set-outline-fp-offset! ol (- (outline-fp-offset ol) n))))

(define-syntax-rule (pop-scan-fp-offset! ol n)
  (unless (outline-initialized? ol)
    (set-outline-fp-offset! ol (+ (outline-fp-offset ol) n))))

(define-syntax-rule (set-scan-write! ol i ...)
  (let* ((sp-offset (outline-sp-offset ol))
         (writes (outline-write-indices ol))
         (writes (if (memq (+ i sp-offset) writes)
                     writes
                     (cons (+ i sp-offset) writes)))
         ...)
    (set-outline-write-indices! ol (sort writes <))))

(define-syntax-rule (set-scan-read! ol i ...)
  (let* ((sp-offset (outline-sp-offset ol))
         (reads (outline-read-indices ol))
         (reads (if (memq (+ i sp-offset) reads)
                    reads
                    (cons (+ i sp-offset) reads)))
         ...)
    (set-outline-read-indices! ol reads)))

(define-syntax-rule (set-scan-initial-fields! ol)
  (unless (outline-initialized? ol)
    (let ((new-sp-offsets (cons (outline-sp-offset ol)
                                (outline-sp-offsets ol)))
          (new-fp-offsets (cons (outline-fp-offset ol)
                                (outline-fp-offsets ol)))
          (writes (outline-write-indices ol))
          (buf (outline-write-buf ol)))
      (set-outline-sp-offsets! ol new-sp-offsets)
      (set-outline-fp-offsets! ol new-fp-offsets)
      (set-outline-write-buf! ol (cons writes buf)))))


;;;
;;; Macro for ANF
;;;

(define-syntax-rule (define-anf (name arg ...) . body)
  (let ((test-proc (lambda (op locals)
                     #t))
        (anf-proc (lambda (%ir %next %ip %ra %dl %locals arg ...)
                    (syntax-parameterize
                        ((ir (identifier-syntax %ir))
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

(define-syntax-rule (define-ti (name ol args ...) . body)
  (let ((test-proc (lambda (op locals)
                     #t))
        (ti-proc (lambda (%ip %dl %locals ol args ...)
                   (syntax-parameterize
                       ((ip (identifier-syntax %ip))
                        (dl (identifier-syntax %dl))
                        (locals (identifier-syntax %locals)))
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
     (gen-load-thunk proc nlocals skip-var? next))
    ((_ proc nlocals skip-var? thunk)
     (let* ((return-subr? (ir-return-subr? ir))
            (stack-size (vector-length locals))
            (sp-offset (current-sp-offset))
            (min-local-index (+ (- stack-size proc 1) sp-offset 2))
            (max-local-index (+ stack-size sp-offset))
            (acc (make-hash-table))
            (load-down-frame
             (lambda ()
               (let lp ((vars (reverse (ir-vars ir))))
                 (match vars
                   (((n . var) . vars)
                    (cond
                     ((skip-var? var)
                      (lp vars))
                     ((< (- stack-size nlocals) n (ir-min-sp-offset ir))
                      (if (not (ir-parent-snapshot ir))
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
                                             (outline-live-indices
                                              (ir-outline ir))
                                             acc)
                                  <)))
                      (debug 1 ";;; [gen-load-thunk] live-indices=~a~%"
                             live-indices)
                      (set-outline-live-indices! (ir-outline ir) live-indices)
                      (thunk)))))))
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
                    (debug 1 ";;; [load-up-frame] n=~s" n)
                    (cond
                     ((skip-var? var)
                      (debug 1 " skipping~%")
                      (lp vars))
                     ((< min-local-index n max-local-index)
                      (let* ((entries (outline-entry-types (ir-outline ir)))
                             (t (assq-ref entries n)))
                        (debug 1 " t=~a~%" (pretty-type t))
                        (if (eq? t &unspecified)
                            (lp vars)
                            (begin
                              (hashq-set! acc n var)
                              (with-frame-ref vars var t n lp)))))
                     (else
                      (debug 1 " skipping~%")
                      (lp vars))))
                   (()
                    (load-down-frame)))))))
       (lambda ()
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

(define-ir-syntax-parameters ir ip ra dl locals next)

(define-syntax-rule (gen-write-index ol arg rest)
  (let* ((i (+ arg (outline-sp-offset ol)))
         (writes (outline-write-indices ol)))
    (unless (memq i writes)
      (set-outline-write-indices! ol (cons i writes)))
    (gen-put-index ol . rest)))

(define-syntax gen-put-index
  (syntax-rules (const scm! fixnum! flonum! pair! vector! box! u64! f64!)
    ((_ ol)
     (set-outline-write-indices! ol (sort (outline-write-indices ol) <)))
    ((_ ol (const arg) . rest) (gen-put-index ol . rest))
    ((_ ol (scm! arg) . rest) (gen-write-index ol arg rest))
    ((_ ol (fixnum! arg) . rest) (gen-write-index ol arg rest))
    ((_ ol (flonum! arg) . rest) (gen-write-index ol arg rest))
    ((_ ol (pair! arg) . rest) (gen-write-index ol arg rest))
    ((_ ol (vector! arg) . rest) (gen-write-index ol arg rest))
    ((_ ol (box! arg) . rest) (gen-write-index ol arg rest))
    ((_ ol (u64! arg) . rest) (gen-write-index ol arg rest))
    ((_ ol (f64! arg) . rest) (gen-write-index ol arg rest))
    ((_ ol (other arg) . rest)
     (let* ((i (+ arg (outline-sp-offset ol)))
            (reads (outline-read-indices ol)))
       (unless (memq i reads)
         (set-outline-read-indices! ol (cons i reads)))
       (gen-put-index ol . rest)))))

(define-syntax-rule (gen-expected ol sty ty arg rest)
  (let* ((i (+ arg (outline-sp-offset ol))))
    (gen-put-element-type ol . rest)
    (unless (outline-initialized? ol)
      (set-entry-type! ol i ty))
    (set-expected-type! ol i ty)))

(define-syntax gen-put-element-type
  (syntax-rules (scm fixnum flonum pair vector box bytevector u64 f64)
    ((_ ol) (values))
    ((_ ol (scm arg) . rest) (gen-expected ol 'scm &scm arg rest))
    ((_ ol (fixnum arg) . rest) (gen-expected ol 'scm &fixnum arg rest))
    ((_ ol (flonum arg) . rest) (gen-expected ol 'scm &flonum arg rest))
    ((_ ol (pair arg) . rest) (gen-expected ol 'scm &pair arg rest))
    ((_ ol (vector arg) . rest) (gen-expected ol 'scm &vector arg rest))
    ((_ ol (box arg) . rest) (gen-expected ol 'scm &box arg rest))
    ((_ ol (bytevector arg) . rest) (gen-expected ol 'scm &bytevector arg rest))
    ((_ ol (u64 arg) . rest) (gen-expected ol 'u64 &u64 arg rest))
    ((_ ol (f64 arg) . rest) (gen-expected ol 'f64 &f64 arg rest))
    ((_ ol (other arg) . rest) (gen-put-element-type ol . rest))))

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
                       (let* ((v (pointer->scm (vector-ref locals n)))
                              (t (type-of v)))
                         (if (eq? t (flag->type f))
                             (lp flags ns)
                             #f))
                       (lp flags ns)))
                  (_ #t)))))
           (scan-proc
            (lambda (%ip %dl %locals ol arg ...)
              (gen-put-element-type ol (flag arg) ...)
              (unless (outline-initialized? ol)
                (gen-put-index ol (flag arg) ...))
              (set-scan-initial-fields! ol)
              #t))
           (ti-proc
            (lambda (%ip %dl %locals ol arg ...)
              (gen-infer-type ol (flag arg) ...)))
           (anf-proc
            (lambda (%ir %next %ip %ra %dl %locals arg ...)
              (syntax-parameterize
                  ((ir (identifier-syntax %ir))
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
         (set-tj-handle-interrupts! (ir-tj ir) #t)
         . body)))))

(define-syntax-rule (dereference-scm addr)
  (pointer->scm (dereference-pointer (make-pointer addr))))

(define-syntax-rule (current-sp-offset)
  (vector-ref (outline-sp-offsets (ir-outline ir)) (ir-bytecode-index ir)))

(define-syntax-rule (current-fp-offset)
  (vector-ref (outline-fp-offsets (ir-outline ir)) (ir-bytecode-index ir)))

(define-syntax-rule (scm-ref n)
  (pointer->scm (vector-ref locals n)))

(define-syntax-rule (u64-ref n)
  (pointer-address (vector-ref locals n)))

(define-syntax-rule (var-ref n)
  (assq-ref (ir-vars ir) (+ n (current-sp-offset))))

(define-syntax-rule (ty-ref n)
  (assq-ref (outline-inferred-types (ir-outline ir)) (+ n (current-sp-offset))))

(define-syntax take-snapshot!
  (syntax-rules ()
    ((_ ip dst-offset)
     (take-snapshot! ip dst-offset #f))
    ((_ ip dst-offset refill?)
     (let-values (((ret snapshot)
                   (take-snapshot ip dst-offset locals (ir-vars ir)
                                  (if (and (ir-parent-snapshot ir)
                                           (not (ir-loop? ir)))
                                      (vector-ref
                                       (outline-write-buf (ir-outline ir))
                                       (ir-bytecode-index ir))
                                      (outline-write-indices (ir-outline ir)))
                                  (ir-snapshot-id ir)
                                  (current-sp-offset) (current-fp-offset)
                                  (ir-min-sp-offset ir) (ir-max-sp-offset ir)
                                  (ir-outline ir) refill?)))
       (let ((old-id (ir-snapshot-id ir)))
         (hashq-set! (ir-snapshots ir) old-id snapshot)
         (set-ir-snapshot-id! ir (+ old-id 1)))
       ret))))

(define-syntax-rule (with-boxing type var tmp proc)
  (cond
   ((eq? type &flonum)
    (set-tj-handle-interrupts! (ir-tj ir) #t)
    `(let ((,tmp (%d2s ,var)))
       ,(proc tmp)))
   ;; XXX: Add more types.
   ((memq type (list &fixnum
                     ;; &complex
                     ;; &fraction
                     &char
                     ;; &unspecified
                     ;; &unbound
                     &false
                     ;; &true
                     ;; &nil
                     &null
                     &symbol
                     ;; &keyword
                     &procedure
                     ;; &pointer
                     &pair
                     ;; &fluid
                     &vector
                     ;; &box
                     &struct
                     &string
                     ;; &bytevector
                     ;; &bitvector
                     ;; &array
                     &hash-table
                     &port
                     &undefined))
    (proc var))
   (else
    (nyi "with-boxing: ~a ~s ~s" (pretty-type type) var tmp))))

(define-syntax-rule (with-unboxing type dst src thunk)
  (let ((tmp (if (equal? dst (make-tmpvar 2))
                 (make-tmpvar 1)
                 (make-tmpvar 2))))
    (letrec-syntax
        ((guard-const
          (syntax-rules ()
            ((_ val)
             `(let ((_ ,(take-snapshot! ip 0)))
                (let ((_ (%eq ,src val)))
                  ,(thunk))))))
         (gen-guard-imm
          (syntax-rules ()
            ((_ mask tcx)
             `(let ((_ ,(take-snapshot! ip 0)))
                (let ((,tmp (%band ,src ,mask)))
                  (let ((_ (%eq ,tmp ,tcx)))
                    ,(thunk)))))))
         (gen-guard-cell
          (syntax-rules ()
            ((_ mask tcx expr)
             `(let ((_ ,(take-snapshot! ip 0)))
                (let ((,tmp (%band ,src 6)))
                  (let ((_ (%eq ,tmp 0)))
                    (let ((,tmp (%cref ,src 0)))
                      (let ((,tmp (%band ,tmp mask)))
                        (let ((_ (%eq ,tmp ,tcx)))
                          expr)))))))
            ((_ mask tcx)
             (gen-guard-cell mask tcx ,(thunk)))))
         (guard-tc1
          (syntax-rules ()
            ((_ tag) (gen-guard-cell #x1 tag))))
         (guard-tc2
          (syntax-rules ()
            ((_ tag) (gen-guard-imm #x2 tag))))
         (guard-tc3
          (syntax-rules ()
            ((_ tag) (gen-guard-cell #x7 tag))))
         (guard-tc7
          (syntax-rules ()
            ((_ tag) (gen-guard-cell #x7f tag))))
         (guard-tc8
          (syntax-rules ()
            ((_ tag) (gen-guard-imm #xff tag))))
         (guard-tc16/f
          (syntax-rules ()
            ((_ tag) (gen-guard-cell #xffff tag
                                     (let ((,dst (%cref/f ,src 2)))
                                       ,(thunk)))))))
      (cond
       ((eq? type &fixnum) (guard-tc2 %tc2-int))
       ((eq? type &flonum) (guard-tc16/f %tc16-real))
       ((eq? type &char) (guard-tc8 %tc8-char))
       ((eq? type &unspecified) (guard-const *unspecified*))
       ((eq? type &unbound) (guard-const *unbound*))

       ;; Guard for true and false disabled for now. Perhaps more proper way is
       ;; to call `with-unboxing' only when necessary, e.g.: detect unmatch
       ;; between inferred type and expected type and skip `with-unboxing' if
       ;; matched.
       ;;
       ((eq? type &false) (thunk))
       ((eq? type &true) (thunk))

       ((eq? type &nil) (guard-const #nil))
       ((eq? type &null) (thunk))
       ((eq? type &symbol) (guard-tc7 %tc7-symbol))
       ((eq? type &keyword) (guard-tc7 %tc7-keyword))
       ((eq? type &procedure) (guard-tc7 %tc7-program))
       ((eq? type &pointer) (guard-tc7 %tc7-pointer))
       ((eq? type &pair) (thunk))
       ((eq? type &fluid) (guard-tc7 %tc7-fluid))
       ((eq? type &vector) (guard-tc7 %tc7-vector))
       ((eq? type &box) (guard-tc7 %tc7-variable))
       ((eq? type &struct) (guard-tc3 %tc3-struct))
       ((eq? type &string) (guard-tc7 %tc7-string))
       ((eq? type &bytevector) (guard-tc7 %tc7-bytevector))
       ((eq? type &bitvector) (guard-tc7 %tc7-bitvector))
       ((eq? type &array) (guard-tc7 %tc7-array))
       ((eq? type &hash-table) (guard-tc7 %tc7-hashtable))
       ((eq? type &port) (guard-tc7 %tc7-port))
       ;; XXX: Add more numbers: bignum, complex, rational.
       (else
        (nyi "with-unboxing: ~a ~a" (pretty-type type) src))))))

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
