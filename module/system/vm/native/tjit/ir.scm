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
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
  #:export (make-ir
            <ir>
            ir-snapshots set-ir-snapshots!
            ir-snapshot-id set-ir-snapshot-id!
            ir-min-sp-offset set-ir-min-sp-offset!
            ir-max-sp-offset set-ir-max-sp-offset!
            ir-bytecode-index set-ir-bytecode-index!
            ir-vars
            ir-outline
            ir-handle-interrupts?
            ir-return-subr? set-ir-return-subr!

            define-ir
            define-interrupt-ir
            gen-put-element-type
            gen-put-index
            dereference-scm
            take-snapshot!
            var-ref
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
            with-frame-ref
            *ir-procedures*
            *scan-procedures*)
  #:replace (local-ref))

;;;
;;; Auxiliary, exported
;;;

(define-record-type <ir>
  (make-ir snapshots snapshot-id parent-snapshot vars
           min-sp-offset max-sp-offset bytecode-index
           outline handle-interrupts? return-subr?)
  ir?

  ;; Hash table containing snapshots.
  (snapshots ir-snapshots set-ir-snapshots!)

  ;; Current snapshot ID.
  (snapshot-id ir-snapshot-id set-ir-snapshot-id!)

  ;; Snapshot from parent trace, if any.
  (parent-snapshot ir-parent-snapshot)

  ;; List of symbols for variables.
  (vars ir-vars)

  ;; Current minimum SP offset.
  (min-sp-offset ir-min-sp-offset set-ir-min-sp-offset!)

  ;; Current maximum SP offset.
  (max-sp-offset ir-max-sp-offset set-ir-max-sp-offset!)

  ;; Current bytecode index.
  (bytecode-index ir-bytecode-index set-ir-bytecode-index!)

  ;; Past frame.
  (outline ir-outline)

  ;; Flag for handle interrupts
  (handle-interrupts? ir-handle-interrupts? set-ir-handle-interrupts!)

  ;; Flag for subr call.
  (return-subr? ir-return-subr? set-ir-return-subr!))


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

(define (take-snapshot ip dst-offset locals vars id
                       sp-offset fp-offset min-sp-offset max-sp-offset
                       parent-snapshot outline)
  (let* ((nlocals (vector-length locals))
         (dst-ip (+ ip (* dst-offset 4)))
         (args-and-indices
          (let lp ((vars vars) (args '()) (indices '()))
            (match vars
              (((n . var) . vars)
               (if (<= min-sp-offset n max-sp-offset)
                   (lp vars (cons var args) (cons n indices))
                   (lp vars args indices)))
              (()
               (cons args indices)))))
         (args (car args-and-indices))
         (indices (cdr args-and-indices))
         (snapshot (make-snapshot id
                                  sp-offset
                                  fp-offset
                                  nlocals
                                  locals
                                  parent-snapshot
                                  indices
                                  outline
                                  dst-ip)))
    (values `(%snap ,id ,@args) snapshot)))

(define-syntax-rule (with-frame-ref next args var type idx)
  (cond
   ((not type)
    (debug 1 "XXX: with-frame-ref: var=~a type=~a~%" var type)
    `(let ((,var #f))
       ,(next args)))
   ((dynamic-link? type)
    `(let ((,var ,(dynamic-link-offset type)))
       ,(next args)))
   ((return-address? type)
    `(let ((,var ,(pointer-address (return-address-ip type))))
       ,(next args)))
   ((or (= type &flonum)
        (= type &f64))
    `(let ((,var (%fref/f ,idx ,type)))
       ,(next args)))
   (else
    `(let ((,var (%fref ,idx ,type)))
       ,(next args)))))

(define *ir-procedures*
  (make-hash-table 255))


;;;
;;; Auxiliary, internal
;;;

(define *scan-procedures*
  (make-hash-table 255))

(define-syntax define-ir-syntax-parameters
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define-syntax-parameter name
         (lambda (x)
           'name "uninitialized" x))
       ...))))

(define-ir-syntax-parameters ir ip ra dl locals next)

(define-syntax gen-put-index
  (syntax-rules (const)
    ((_ ol)
     ol)
    ((_ ol (const arg) . rest)
     (gen-put-index ol . rest))
    ((_ ol (other arg) . rest)
     (let ((indices (assq-set! (outline-local-indices ol)
                               (+ arg (outline-sp-offset ol)) #t)))
       (set-outline-local-indices! ol indices)
       (gen-put-index ol . rest)))))

(define-syntax gen-put-element-type
  (syntax-rules (scm u64 f64 const)
    ((_ ol)
     ol)
    ((_ ol (const arg) . rest)
     (gen-put-element-type ol . rest))
    ((_ ol (other arg) . rest)
     (let ((types (assq-set! (outline-types ol)
                             (+ arg (outline-sp-offset ol)) 'other)))
       (set-outline-types! ol types)
       (gen-put-element-type ol . rest)))))

(define-syntax define-ir
  (syntax-rules ()
    "Defines procedure to compile VM operation to IR, and optionally local
accumulator when arguments in definition are lists. E.g:

  (define-ir (add1 (local dst) (local src))
    ...)

will define two procedures: one for IR compilation taking two arguments, and
another procedure for scanner. The procedure for scanner takes two arguments and
saves index referenced by dst and src values at runtime."
    ((_ (name) . body)
     (let ((proc
            (lambda (%ir %next %ip %ra %dl %locals)
              (syntax-parameterize
                  ((ir (identifier-syntax %ir))
                   (next (identifier-syntax %next))
                   (ip (identifier-syntax %ip))
                   (ra (identifier-syntax %ra))
                   (dl (identifier-syntax %dl))
                   (locals (identifier-syntax %locals)))
                . body))))
       (hashq-set! *ir-procedures* 'name proc)))
    ((_ (name (flag arg) ...) . body)
     (let ((scan-proc (lambda (ol initialized? arg ...)
                        (unless initialized?
                          (gen-put-index ol (flag arg) ...))
                        (gen-put-element-type ol (flag arg) ...))))
       (hashq-set! *scan-procedures* 'name scan-proc)
       (define-ir (name arg ...) . body)))
    ((_ (name arg ...) . body)
     (let ((proc
            (lambda (%ir %next %ip %ra %dl %locals arg ...)
              (syntax-parameterize
                  ((ir (identifier-syntax %ir))
                   (next (identifier-syntax %next))
                   (ip (identifier-syntax %ip))
                   (ra (identifier-syntax %ra))
                   (dl (identifier-syntax %dl))
                   (locals (identifier-syntax %locals)))
                . body))))
       (hashq-set! *ir-procedures* 'name proc)))))

(define-syntax define-interrupt-ir
  (syntax-rules ()
    ((_ names-and-args . body)
     (define-ir names-and-args
       (begin
         (set-ir-handle-interrupts! ir #t)
         . body)))))

(define-syntax-rule (to-double scm)
  `(%cref/f ,scm 2))

(define-syntax-rule (dereference-scm addr)
  (pointer->scm (dereference-pointer (make-pointer addr))))

(define-syntax-rule (current-sp-offset)
  (vector-ref (outline-sp-offsets (ir-outline ir)) (ir-bytecode-index ir)))

(define-syntax-rule (current-fp-offset)
  (vector-ref (outline-fp-offsets (ir-outline ir)) (ir-bytecode-index ir)))

(define-syntax-rule (local-ref n)
  (let ((t (outline-type-ref (ir-outline ir) (+ n (current-sp-offset)))))
    (stack-element locals n t)))

(define-syntax-rule (var-ref n)
  (assq-ref (ir-vars ir) (+ n (current-sp-offset))))

(define-syntax-rule (take-snapshot! ip dst-offset)
  (let-values (((ret snapshot)
                (take-snapshot ip
                               dst-offset
                               locals
                               (ir-vars ir)
                               (ir-snapshot-id ir)
                               (current-sp-offset)
                               (current-fp-offset)
                               (ir-min-sp-offset ir)
                               (ir-max-sp-offset ir)
                               (ir-parent-snapshot ir)
                               (ir-outline ir))))
    (let ((old-id (ir-snapshot-id ir)))
      (hashq-set! (ir-snapshots ir) old-id snapshot)
      (set-ir-snapshot-id! ir (+ old-id 1)))
    ret))

(define-syntax-rule (with-boxing type var tmp proc)
  (cond
   ((eq? type &flonum)
    (set-ir-handle-interrupts! ir #t)
    `(let ((,tmp (%d2s ,var)))
       ,(proc tmp)))
   ;; XXX: Add more types.
   ((memq type (list &exact-integer &char &false &undefined &symbol &pair
                     &vector &string &struct &hash-table))
    (proc var))
   (else
    (nyi "with-boxing: ~a ~s ~s" (pretty-type type) var tmp))))

(define-syntax-rule (with-unboxing type var thunk)
  (let ((tmp (if (equal? var (make-tmpvar 2))
                 (make-tmpvar 1)
                 (make-tmpvar 2))))
    (letrec-syntax
        ((guard-const
          (syntax-rules ()
            ((_ val)
             `(let ((_ ,(take-snapshot! ip 0)))
                (let ((_ (%eq ,var val)))
                  ,(thunk))))))
         (gen-guard-imm
          (syntax-rules ()
            ((_ mask tcx)
             `(let ((_ ,(take-snapshot! ip 0)))
                (let ((,tmp (%band ,var ,mask)))
                  (let ((_ (%eq ,tmp ,tcx)))
                    ,(thunk)))))))
         (gen-guard-cell
          (syntax-rules ()
            ((_ mask tcx expr)
             `(let ((_ ,(take-snapshot! ip 0)))
                (let ((,tmp (%band ,var 6)))
                  (let ((_ (%eq ,tmp 0)))
                    (let ((,tmp (%cref ,var 0)))
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
                                      (let ((,var (%cref/f ,var 2)))
                                        ,(thunk)))))))
      (cond
       ((eq? type &exact-integer) (guard-tc2 %tc2-int))
       ((eq? type &flonum) (guard-tc16/f %tc16-real))
       ((eq? type &char) (guard-tc8 %tc8-char))
       ((eq? type &unspecified) (guard-const *unspecified*))
       ((eq? type &unbound) (guard-const *unbound*))
       ((eq? type &false) (guard-const #f))
       ((eq? type &true) (guard-const #t))
       ((eq? type &nil) (guard-const #nil))
       ((eq? type &null) (guard-const ()))
       ((eq? type &symbol) (guard-tc7 %tc7-symbol))
       ((eq? type &keyword) (guard-tc7 %tc7-keyword))
       ((eq? type &procedure) (guard-tc7 %tc7-program))
       ((eq? type &pointer) (guard-tc7 %tc7-pointer))
       ((eq? type &pair) (guard-tc1 %tc3-cons))
       ((eq? type &fluid) (guard-tc7 %tc7-fluid))
       ((eq? type &vector) (guard-tc7 %tc7-vector))
       ((eq? type &box) (guard-tc7 %tc7-variable))
       ((eq? type &struct) (guard-tc3 %tc3-struct))
       ((eq? type &string) (guard-tc7 %tc7-string))
       ((eq? type &bytevector) (guard-tc7 %tc7-bytevector))
       ((eq? type &bitvector) (guard-tc7 %tc7-bitvector))
       ((eq? type &array) (guard-tc7 %tc7-array))
       ((eq? type &hash-table) (guard-tc7 %tc7-hashtable))
       ;; XXX: Add more numbers: bignum, complex, rational.
       (else
        (nyi "with-unboxing: ~a ~a" (pretty-type type) var))))))


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

;;; *** Arrays, packed uniform arrays, and bytevectors

;; XXX: load-typed-array
;; XXX: make-array
;; XXX: bv-u8-ref
;; XXX: bv-s8-ref
;; XXX: bv-u16-ref
;; XXX: bv-s16-ref
;; XXX: bv-u32-ref
;; XXX: bv-s32-ref
;; XXX: bv-u64-ref
;; XXX: bv-s64-ref
;; XXX: bv-f32-ref
;; XXX: bv-f64-ref
;; XXX: bv-u8-set!
;; XXX: bv-s8-set!
;; XXX: bv-u16-set!
;; XXX: bv-s16-set!
;; XXX: bv-u32-set!
;; XXX: bv-s32-set!
;; XXX: bv-u64-set!
;; XXX: bv-s64-set!
;; XXX: bv-f32-set!
;; XXX: bv-f64-set!
