;;; Effects analysis on CPS

;; Copyright (C) 2011, 2012, 2013, 2014 Free Software Foundation, Inc.

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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; A helper module to compute the set of effects that an expression
;;; depends on and causes.  This information is useful when writing
;;; algorithms that move code around, while preserving the semantics of
;;; an input program.
;;;
;;; The effects set is represented by a bitfield, as a fixnum.  The set
;;; of possible effects is modelled rather coarsely.  For example, a
;;; "car" call modelled as depending on the &car effect, and causing a
;;; &type-check effect.  If any intervening code sets the car of any
;;; pair, that will block motion of the "car" call.
;;;
;;; For each effect, two bits are reserved: one to indicate that an
;;; expression depends on the effect, and the other to indicate that an
;;; expression causes the effect.
;;;
;;; Since we have more bits in a fixnum on 64-bit systems, we can be
;;; more precise without losing efficiency.  On a 32-bit system, some of
;;; the more precise effects map to fewer bits.
;;;
;;; Code:

(define-module (language cps effects-analysis)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (ice-9 match)
  #:export (expression-effects
            compute-effects
            synthesize-definition-effects!

            &fluid
            &fluid-environment
            &prompt
            &allocation
            &car
            &cdr
            &vector
            &box
            &module
            &struct
            &string
            &bytevector
            &type-check

            &no-effects
            &all-effects

            effects-commute?
            exclude-effects
            effect-free?
            constant?
            depends-on-effects?
            causes-effects?
            causes-all-effects?))

(define-syntax define-effects
  (lambda (x)
    (syntax-case x ()
      ((_ all name ...)
       (with-syntax (((n ...) (iota (length #'(name ...)))))
         #'(begin
             (define-syntax name (identifier-syntax (ash 1 (* n 2))))
             ...
             (define-syntax all (identifier-syntax (logior name ...)))))))))

(define-syntax compile-time-cond
  (lambda (x)
    (syntax-case x (else)
      ((_ (else body ...))
       #'(begin body ...))
      ((_ (exp body ...) clause ...)
       (if (eval (syntax->datum #'exp) (current-module))
           #'(begin body ...)
           #'(compile-time-cond clause ...))))))

;; Here we define the effects, indicating the meaning of the effect.
;;
;; Effects that are described in a "depends on" sense can also be used
;; in the "causes" sense.
;;
;; Effects that are described as causing an effect are not usually used
;; in a "depends-on" sense.  Although the "depends-on" sense is used
;; when checking for the existence of the "causes" effect, the effects
;; analyzer will not associate the "depends-on" sense of these effects
;; with any expression.
;;
(compile-time-cond
 ((>= (logcount most-positive-fixnum) 60)
  (define-effects &all-effects
    ;; Indicates that an expression depends on the value of a fluid
    ;; variable.
    &fluid

    ;; Indicates that an expression depends on the current fluid environment.
    &fluid-environment

    ;; Indicates that an expression depends on the current prompt
    ;; stack.
    &prompt

    ;; Indicates that an expression may return a fresh object -- a
    ;; "causes" effect.
    &allocation

    ;; Indicates that an expression depends on the value of the car of a
    ;; pair.
    &car

    ;; Indicates that an expression depends on the value of the cdr of a
    ;; pair.
    &cdr

    ;; Indicates that an expression depends on the value of a vector
    ;; field.  We cannot be more precise, as vectors may alias other
    ;; vectors.
    &vector

    ;; Indicates that an expression depends on the value of a variable
    ;; cell.
    &box

    ;; Indicates that an expression depends on the current module.
    &module

    ;; Indicates that an expression depends on the value of a particular
    ;; struct field.
    &struct-0 &struct-1 &struct-2 &struct-3 &struct-4 &struct-5 &struct-6+

    ;; Indicates that an expression depends on the contents of a string.
    &string

    ;; Indicates that an expression depends on the contents of a
    ;; bytevector.  We cannot be more precise, as bytevectors may alias
    ;; other bytevectors.
    &bytevector

    ;; Indicates that an expression may cause a type check.  A type check,
    ;; for the purposes of this analysis, is the possibility of throwing
    ;; an exception the first time an expression is evaluated.  If the
    ;; expression did not cause an exception to be thrown, users can
    ;; assume that evaluating the expression again will not cause an
    ;; exception to be thrown.
    ;;
    ;; For example, (+ x y) might throw if X or Y are not numbers.  But if
    ;; it doesn't throw, it should be safe to elide a dominated, common
    ;; subexpression (+ x y).
    &type-check)

  ;; Indicates that an expression depends on the contents of an unknown
  ;; struct field.
  (define-syntax &struct
    (identifier-syntax
     (logior &struct-1 &struct-2 &struct-3 &struct-4 &struct-5 &struct-6+))))

 (else
  ;; For systems with smaller fixnums, be less precise regarding struct
  ;; fields.
  (define-effects &all-effects
    &fluid
    &prompt
    &allocation
    &car
    &cdr
    &vector
    &box
    &module
    &struct
    &string
    &bytevector
    &type-check)
  (define-syntax &fluid-environment (identifier-syntax &fluid))
  (define-syntax &struct-0 (identifier-syntax &struct))
  (define-syntax &struct-1 (identifier-syntax &struct))
  (define-syntax &struct-2 (identifier-syntax &struct))
  (define-syntax &struct-3 (identifier-syntax &struct))
  (define-syntax &struct-4 (identifier-syntax &struct))
  (define-syntax &struct-5 (identifier-syntax &struct))
  (define-syntax &struct-6+ (identifier-syntax &struct))))

(define-syntax &no-effects (identifier-syntax 0))

(define-inlinable (cause effect)
  (ash effect 1))

(define-inlinable (&depends-on a)
  (logand a &all-effects))
(define-inlinable (&causes a)
  (logand a (cause &all-effects)))

(define-inlinable (exclude-effects effects exclude)
  (logand effects (lognot (cause exclude))))
(define-inlinable (effect-free? effects)
  (zero? (&causes effects)))
(define-inlinable (constant? effects)
  (zero? effects))
(define-inlinable (effects-clobber effects)
  (ash (&causes effects) -1))

(define-inlinable (depends-on-effects? x effects)
  (not (zero? (logand (&depends-on x) effects))))
(define-inlinable (causes-effects? x effects)
  (not (zero? (logand (&causes x) (cause effects)))))
(define-inlinable (causes-all-effects? x effects)
  (= (logand (&causes x) (cause effects)) (cause effects)))

(define-inlinable (effects-commute? a b)
  (and (not (causes-effects? a (&depends-on b)))
       (not (causes-effects? b (&depends-on a)))))

(define (lookup-constant-index sym dfg)
  (call-with-values (lambda () (find-constant-value sym dfg))
    (lambda (has-const? val)
      (and has-const? (integer? val) (exact? val) (<= 0 val) val))))

(define *primitive-effects* (make-hash-table))

(define-syntax-rule (define-primitive-effects* dfg ((name . args) effects) ...)
  (begin
    (hashq-set! *primitive-effects* 'name
                (case-lambda* ((dfg . args) effects)
                              (_ (logior &all-effects (cause &all-effects)))))
    ...))

(define-syntax-rule (define-primitive-effects ((name . args) effects) ...)
  (define-primitive-effects* dfg ((name . args) effects) ...))

;; Miscellaneous.
(define-primitive-effects
  ((values . _) &no-effects)
  ((not arg) &no-effects))

;; Generic predicates.
(define-primitive-effects
  ((eq? . _) &no-effects)
  ((eqv? . _) &no-effects)
  ((equal? . _) &no-effects)
  ((pair? arg) &no-effects)
  ((null? arg) &no-effects)
  ((nil? arg ) &no-effects)
  ((symbol? arg) &no-effects)
  ((variable? arg) &no-effects)
  ((vector? arg) &no-effects)
  ((struct? arg) &no-effects)
  ((string? arg) &no-effects)
  ((number? arg) &no-effects)
  ((char? arg) &no-effects)
  ((procedure? arg) &no-effects)
  ((thunk? arg) &no-effects))

;; Fluids.
(define-primitive-effects
  ((fluid-ref f)
   (logior (cause &type-check) &fluid &fluid-environment))
  ((fluid-set! f v)
   (logior (cause &type-check) (cause &fluid) &fluid-environment))
  ((push-fluid f v)
   (logior (cause &type-check) (cause &fluid-environment)))
  ((pop-fluid)
   (logior (cause &fluid-environment))))

;; Prompts.
(define-primitive-effects
  ((make-prompt-tag #:optional arg) (cause &allocation)))

;; Pairs.
(define-primitive-effects
  ((cons a b) (cause &allocation))
  ((list . _) (cause &allocation))
  ((car x) (logior (cause &type-check) &car))
  ((set-car! x y) (logior (cause &type-check) (cause &car)))
  ((cdr x) (logior (cause &type-check) &cdr))
  ((set-cdr! x y) (logior (cause &type-check) (cause &cdr)))
  ((memq x y) (logior (cause &type-check) &car &cdr))
  ((memv x y) (logior (cause &type-check) &car &cdr))
  ((list? arg) &cdr)
  ((length l) (logior (cause &type-check) &cdr)))

;; Vectors.
(define-primitive-effects
  ((vector . _) (cause &allocation))
  ((make-vector n init) (logior (cause &type-check) (cause &allocation)))
  ((make-vector/immediate n init) (cause &allocation))
  ((vector-ref v n) (logior (cause &type-check) &vector))
  ((vector-ref/immediate v n) (logior (cause &type-check) &vector))
  ((vector-set! v n x) (logior (cause &type-check) (cause &vector)))
  ((vector-set!/immediate v n x) (logior (cause &type-check) (cause &vector)))
  ((vector-length v) (cause &type-check)))

;; Variables.
(define-primitive-effects
  ((box v) (cause &allocation))
  ((box-ref v) (logior (cause &type-check) &box))
  ((box-set! v x) (logior (cause &type-check) (cause &box))))

;; Structs.
(define-primitive-effects* dfg
  ((allocate-struct vtable nfields)
   (logior (cause &type-check) (cause &allocation)))
  ((allocate-struct/immediate vtable nfields)
   (logior (cause &type-check) (cause &allocation)))
  ((make-struct vtable ntail . args)
   (logior (cause &type-check) (cause &allocation)))
  ((make-struct/no-tail vtable . args)
   (logior (cause &type-check) (cause &allocation)))
  ((struct-ref s n)
   (logior (cause &type-check)
           (match (lookup-constant-index n dfg)
             (#f &struct)
             (0 &struct-0)
             (1 &struct-1)
             (2 &struct-2)
             (3 &struct-3)
             (4 &struct-4)
             (5 &struct-5)
             (_ &struct-6+))))
  ((struct-ref/immediate s n)
   (logior (cause &type-check)
           (match (lookup-constant-index n dfg)
             (#f &struct)
             (0 &struct-0)
             (1 &struct-1)
             (2 &struct-2)
             (3 &struct-3)
             (4 &struct-4)
             (5 &struct-5)
             (_ &struct-6+))))
  ((struct-set! s n x)
   (logior (cause &type-check)
           (match (lookup-constant-index n dfg)
             (#f (cause &struct))
             (0 (cause &struct-0))
             (1 (cause &struct-1))
             (2 (cause &struct-2))
             (3 (cause &struct-3))
             (4 (cause &struct-4))
             (5 (cause &struct-5))
             (_ (cause &struct-6+)))))
  ((struct-set!/immediate s n x)
   (logior (cause &type-check)
           (match (lookup-constant-index n dfg)
             (#f (cause &struct))
             (0 (cause &struct-0))
             (1 (cause &struct-1))
             (2 (cause &struct-2))
             (3 (cause &struct-3))
             (4 (cause &struct-4))
             (5 (cause &struct-5))
             (_ (cause &struct-6+)))))
  ((struct-vtable s) (cause &type-check)))

;; Strings.
(define-primitive-effects
  ((string-ref s n) (logior (cause &type-check) &string))
  ((string-set! s n c) (logior (cause &type-check) (cause &string)))
  ((number->string _) (cause &type-check))
  ((string->number _) (logior (cause &type-check) &string))
  ((string-length s) (cause &type-check)))

;; Bytevectors.
(define-primitive-effects
  ((bytevector-length _) (cause &type-check))

  ((bv-u8-ref bv n) (logior (cause &type-check) &bytevector))
  ((bv-s8-ref bv n) (logior (cause &type-check) &bytevector))
  ((bv-u16-ref bv n) (logior (cause &type-check) &bytevector))
  ((bv-s16-ref bv n) (logior (cause &type-check) &bytevector))
  ((bv-u32-ref bv n) (logior (cause &type-check) &bytevector))
  ((bv-s32-ref bv n) (logior (cause &type-check) &bytevector))
  ((bv-u64-ref bv n) (logior (cause &type-check) &bytevector))
  ((bv-s64-ref bv n) (logior (cause &type-check) &bytevector))
  ((bv-f32-ref bv n) (logior (cause &type-check) &bytevector))
  ((bv-f64-ref bv n) (logior (cause &type-check) &bytevector))
  
  ((bv-u8-set! bv n x) (logior (cause &type-check) (cause &bytevector)))
  ((bv-s8-set! bv n x) (logior (cause &type-check) (cause &bytevector)))
  ((bv-u16-set! bv n x) (logior (cause &type-check) (cause &bytevector)))
  ((bv-s16-set! bv n x) (logior (cause &type-check) (cause &bytevector)))
  ((bv-u32-set! bv n x) (logior (cause &type-check) (cause &bytevector)))
  ((bv-s32-set! bv n x) (logior (cause &type-check) (cause &bytevector)))
  ((bv-u64-set! bv n x) (logior (cause &type-check) (cause &bytevector)))
  ((bv-s64-set! bv n x) (logior (cause &type-check) (cause &bytevector)))
  ((bv-f32-set! bv n x) (logior (cause &type-check) (cause &bytevector)))
  ((bv-f64-set! bv n x) (logior (cause &type-check) (cause &bytevector))))

;; Numbers.
(define-primitive-effects
  ((= . _) (cause &type-check))
  ((< . _) (cause &type-check))
  ((> . _) (cause &type-check))
  ((<= . _) (cause &type-check))
  ((>= . _) (cause &type-check))
  ((zero? . _) (cause &type-check))
  ((add . _) (cause &type-check))
  ((mul . _) (cause &type-check))
  ((sub . _) (cause &type-check))
  ((div . _) (cause &type-check))
  ((sub1 . _) (cause &type-check))
  ((add1 . _) (cause &type-check))
  ((quo . _) (cause &type-check))
  ((rem . _) (cause &type-check))
  ((mod . _) (cause &type-check))
  ((complex? _) (cause &type-check))
  ((real? _) (cause &type-check))
  ((rational? _) (cause &type-check))
  ((inf? _) (cause &type-check))
  ((nan? _) (cause &type-check))
  ((integer? _) (cause &type-check))
  ((exact? _) (cause &type-check))
  ((inexact? _) (cause &type-check))
  ((even? _) (cause &type-check))
  ((odd? _) (cause &type-check))
  ((ash n m) (cause &type-check))
  ((logand . _) (cause &type-check))
  ((logior . _) (cause &type-check))
  ((logior . _) (cause &type-check))
  ((lognot . _) (cause &type-check))
  ((sqrt _) (cause &type-check))
  ((abs _) (cause &type-check)))

;; Characters.
(define-primitive-effects
  ((char<? . _) (cause &type-check))
  ((char<=? . _) (cause &type-check))
  ((char>=? . _) (cause &type-check))
  ((char>? . _) (cause &type-check))
  ((integer->char _) (cause &type-check))
  ((char->integer _) (cause &type-check)))

;; Modules.
(define-primitive-effects
  ((current-module) &module)
  ((cache-current-module! mod scope) (cause &box))
  ((resolve name bound?) (logior &module (cause &type-check)))
  ((cached-toplevel-box scope name bound?) (cause &type-check))
  ((cached-module-box mod name public? bound?) (cause &type-check))
  ((define! name val) (logior &module (cause &box))))

(define (primitive-effects dfg name args)
  (let ((proc (hashq-ref *primitive-effects* name)))
    (if proc
        (apply proc dfg args)
        (logior &all-effects (cause &all-effects)))))

(define (expression-effects exp dfg)
  (match exp
    ((or ($ $void) ($ $const) ($ $prim) ($ $values))
     &no-effects)
    (($ $fun)
     (cause &allocation))
    (($ $prompt)
     (cause &prompt))
    ((or ($ $call) ($ $callk))
     (logior &all-effects (cause &all-effects)))
    (($ $primcall name args)
     (primitive-effects dfg name args))))

(define* (compute-effects dfg #:optional (min-label (dfg-min-label dfg))
                          (label-count (dfg-label-count dfg)))
  (let ((effects (make-vector label-count &no-effects)))
    (define (idx->label idx) (+ idx min-label))
    (let lp ((n 0))
      (when (< n label-count)
        (vector-set!
         effects
         n
         (match (lookup-cont (idx->label n) dfg)
           (($ $kargs names syms body)
            (expression-effects (find-expression body) dfg))
           (($ $kreceive arity kargs)
            (match arity
              (($ $arity _ () #f () #f) (cause &type-check))
              (($ $arity () () _ () #f) (cause &allocation))
              (($ $arity _ () _ () #f) (logior (cause &allocation)
                                               (cause &type-check)))))
           (($ $kif) &no-effects)
           (($ $kfun) (cause &type-check))
           (($ $kclause) (cause &type-check))
           (($ $ktail) &no-effects)))
        (lp (1+ n))))
    effects))

;; There is a way to abuse effects analysis in CSE to also do scalar
;; replacement, effectively adding `car' and `cdr' expressions to `cons'
;; expressions, and likewise with other constructors and setters.  This
;; routine adds appropriate effects to `cons' and `set-car!' and the
;; like.
;;
;; This doesn't affect CSE's ability to eliminate expressions, given
;; that allocations aren't eliminated anyway, and the new effects will
;; just cause the allocations not to commute with e.g. set-car!  which
;; is what we want anyway.
(define* (synthesize-definition-effects! effects dfg min-label #:optional
                                         (label-count (vector-length effects)))
  (define (label->idx label) (- label min-label))
  (let lp ((label min-label))
    (when (< label (+ min-label label-count))
      (let* ((lidx (label->idx label))
             (fx (vector-ref effects lidx)))
        (define (add-deps! deps)
          (vector-set! effects lidx (logior fx deps)))
        (match (lookup-cont label dfg)
          (($ $kargs _ _ term)
           (match (find-expression term)
             (($ $primcall 'cons)
              (add-deps! (logior &car &cdr)))
             (($ $primcall (or 'make-vector 'make-vector/immediate))
              (add-deps! &vector))
             (($ $primcall (or 'allocate-struct 'allocate-struct/immediate
                               'make-struct/no-tail 'make-struct))
              (add-deps! &struct))
             (($ $primcall 'box)
              (add-deps! &box))
             (_
              (add-deps! (effects-clobber
                          (logior fx &car &cdr &vector &struct &box)))
              #t)))
          (_ #t))
        (lp (1+ label))))))
