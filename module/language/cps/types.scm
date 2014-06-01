;;; Type analysis on CPS
;;; Copyright (C) 2014 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Type analysis computes the possible types and ranges that values may
;;; have at all program positions.  This analysis can help to prove that
;;; a primcall has no side-effects, if its arguments have the
;;; appropriate type and range.  It can also enable constant folding of
;;; type predicates and, in the future, enable the compiler to choose
;;; untagged, unboxed representations for numbers.
;;;
;;; For the purposes of this analysis, a "type" is an aspect of a value
;;; that will not change.  Guile's CPS intermediate language does not
;;; carry manifest type information that asserts properties about given
;;; values; instead, we recover this information via flow analysis,
;;; garnering properties from type predicates, constant literals,
;;; primcall results, and primcalls that assert that their arguments are
;;; of particular types.
;;;
;;; A range denotes a subset of the set of values in a type, bounded by
;;; a minimum and a maximum.  The precise meaning of a range depends on
;;; the type.  For real numbers, the range indicates an inclusive lower
;;; and upper bound on the integer value of a type.  For vectors, the
;;; range indicates the length of the vector.  The range is limited to a
;;; signed 32-bit value, with the smallest and largest values indicating
;;; -inf.0 and +inf.0, respectively.  For some types, like pairs, the
;;; concept of "range" makes no sense.  In these cases we consider the
;;; range to be -inf.0 to +inf.0.
;;;
;;; Types are represented as a bitfield.  Fewer bits means a more precise
;;; type.  Although normally only values that have a single type will
;;; have an associated range, this is not enforced.  The range applies
;;; to all types in the bitfield.  When control flow meets, the types and
;;; ranges meet with the union operator.
;;;
;;; It is not practical to precisely compute value ranges in all cases.
;;; For example, in the following case:
;;;
;;;   (let lp ((n 0)) (when (foo) (lp (1+ n))))
;;;
;;; The first time that range analysis visits the program, N is
;;; determined to be the exact integer 0.  The second time, it is an
;;; exact integer in the range [0, 1]; the third, [0, 2]; and so on.
;;; This analysis will terminate, but only after the positive half of
;;; the 32-bit range has been fully explored and we decide that the
;;; range of N is [0, +inf.0].  At the same time, we want to do range
;;; analysis and type analysis at the same time, as there are
;;; interactions between them, notably in the case of `sqrt' which
;;; returns a complex number if its argument cannot be proven to be
;;; non-negative.  So what we do is, once the types reach a fixed point,
;;; we cause control-flow joins that would expand the range of a value
;;; to saturate that range towards positive or infinity (as
;;; appropriate).
;;;
;;; We represent the set of types and ranges of value at a given
;;; program point as a bytevector that is N * 12 bytes long, where N is
;;; the number of variables.  Each 12-byte value indicates the type,
;;; minimum, and maximum of the value.  This gives an overall time and
;;; space complexity of the algorithm of O(label-count *
;;; variable-count).  Perhaps with a different representation for the
;;; types we could decrease this, sharing space between typesets and
;;; requiring fewer "meet" operations.
;;;
;;; Code:

(define-module (language cps types)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (rnrs bytevectors)
  #:export (;; Specific types.
            &exact-integer
            &flonum
            &complex
            &fraction

            &char
            &unspecified
            &unbound
            &boolean
            &nil
            &null
            &symbol
            &keyword

            &procedure

            &pointer
            &fluid
            &pair
            &vector
            &box
            &struct
            &string
            &bytevector
            &bitvector
            &array
            &hash-table

            ;; Union types.
            &number &real

            infer-types
            lookup-pre-type
            lookup-post-type
            primcall-types-check?))

(define-syntax define-flags
  (lambda (x)
    (syntax-case x ()
      ((_ all shift name ...)
       (let ((count (length #'(name ...))))
         (with-syntax (((n ...) (iota count))
                       (count count))
           #'(begin
               (define-syntax name (identifier-syntax (ash 1 n)))
               ...
               (define-syntax all (identifier-syntax (1- (ash 1 count))))
               (define-syntax shift (identifier-syntax count)))))))))

;; More precise types have fewer bits.
(define-flags &all-types &type-bits
  &exact-integer
  &flonum
  &complex
  &fraction

  &char
  &unspecified
  &unbound
  &boolean
  &nil
  &null
  &symbol
  &keyword

  &procedure

  &pointer
  &fluid
  &pair
  &vector
  &box
  &struct
  &string
  &bytevector
  &bitvector
  &array
  &hash-table)

(define-syntax &no-type (identifier-syntax 0))

(define-syntax &number
  (identifier-syntax (logior &exact-integer &flonum &complex &fraction)))
(define-syntax &real
  (identifier-syntax (logior &exact-integer &flonum &fraction)))

(define-syntax *max-s32* (identifier-syntax (- (ash 1 31) 1)))
(define-syntax *min-s32* (identifier-syntax (- 0 (ash 1 31))))

;; Versions of min and max that do not coerce exact numbers to become
;; inexact.
(define min
  (case-lambda
    ((a b) (if (< a b) a b))
    ((a b c) (min (min a b) c))
    ((a b c d) (min (min a b) c d))))
(define max
  (case-lambda
    ((a b) (if (> a b) a b))
    ((a b c) (max (max a b) c))
    ((a b c d) (max (max a b) c d))))

(define (constant-type val)
  "Compute the type and range of VAL.  Return three values: the type,
minimum, and maximum."
  (define (return type val)
    (if val
        (values type val val)
        (values type -inf.0 +inf.0)))
  (cond
   ((number? val)
    (cond
     ((exact-integer? val) (return &exact-integer val))
     ((eqv? (imag-part val) 0)
      (values (if (exact? val) &fraction &flonum)
              (if (rational? val) (inexact->exact (floor val)) val)
              (if (rational? val) (inexact->exact (ceiling val)) val)))
     (else (return &complex #f))))
   ((eq? val '()) (return &null #f))
   ((eq? val #nil) (return &nil #f))
   ((char? val) (return &char (char->integer val)))
   ((eqv? val *unspecified*) (return &unspecified #f))
   ((boolean? val) (return &boolean (if val 1 0)))
   ((symbol? val) (return &symbol #f))
   ((keyword? val) (return &keyword #f))
   ((pair? val) (return &pair #f))
   ((vector? val) (return &vector (vector-length val)))
   ((string? val) (return &string (string-length val)))
   ((bytevector? val) (return &bytevector (bytevector-length val)))
   ((bitvector? val) (return &bitvector (bitvector-length val)))
   ((array? val) (return &array (array-rank val)))
   ((not (variable-bound? (make-variable val))) (return &unbound #f))

   (else (error "unhandled constant" val))))

(define-syntax-rule (var-type bv var)
  (bytevector-u32-native-ref bv (* var 12)))
(define-syntax-rule (var-clamped-min bv var)
  (bytevector-s32-native-ref bv (+ (* var 12) 4)))
(define-syntax-rule (var-clamped-max bv var)
  (bytevector-s32-native-ref bv (+ (* var 12) 8)))
(define-syntax-rule (var-min bv var)
  (let ((min (var-clamped-min bv var)))
    (if (= min *min-s32*)
        -inf.0
        min)))
(define-syntax-rule (var-max bv var)
  (let ((max (var-clamped-max bv var)))
    (if (= max *max-s32*)
        +inf.0
        max)))

(define-inlinable (clamp-range val)
  (cond
   ((< val *min-s32*) *min-s32*)
   ((< *max-s32* val) *max-s32*)
   (else val)))
(define-syntax-rule (set-var-type! bv var val)
  (bytevector-u32-native-set! bv (* var 12) val))
(define-syntax-rule (set-var-clamped-min! bv var val)
  (bytevector-s32-native-set! bv (+ (* var 12) 4) val))
(define-syntax-rule (set-var-clamped-max! bv var val)
  (bytevector-s32-native-set! bv (+ (* var 12) 8) val))
(define-syntax-rule (set-var-min! bv var val)
  (set-var-clamped-min! bv var (clamp-range val)))
(define-syntax-rule (set-var-max! bv var val)
  (set-var-clamped-max! bv var (clamp-range val)))

(define-inlinable (extend-var-type! bv var type)
  (set-var-type! bv var (logior (var-type bv var) type)))
(define-inlinable (restrict-var-type! bv var type)
  (set-var-type! bv var (logand (var-type bv var) type)))
(define-inlinable (extend-var-range! bv var min max)
  (let ((old-min (var-clamped-min bv var))
        (old-max (var-clamped-max bv var))
        (min (clamp-range min))
        (max (clamp-range max)))
    (when (< min old-min)
      (set-var-clamped-min! bv var min))
    (when (< old-max max)
      (set-var-clamped-max! bv var max))))
(define-inlinable (restrict-var-range! bv var min max)
  (let ((old-min (var-clamped-min bv var))
        (old-max (var-clamped-max bv var))
        (min (clamp-range min))
        (max (clamp-range max)))
    (when (< old-min min)
      (set-var-clamped-min! bv var min))
    (when (< max old-max)
      (set-var-clamped-max! bv var max))))

(define *type-checkers* (make-hash-table))
(define *type-inferrers* (make-hash-table))
(define *predicate-inferrers* (make-hash-table))

(define-syntax-rule (define-type-helper name)
  (define-syntax-parameter name
    (lambda (stx)
      (syntax-violation 'name
                        "macro used outside of define-type"
                        stx))))
(define-type-helper define!)
(define-type-helper restrict!)
(define-type-helper &type)
(define-type-helper &min)
(define-type-helper &max)

(define-syntax-rule (define-type-checker (name arg ...) body ...)
  (hashq-set!
   *type-checkers*
   'name
   (lambda (in arg ...)
     (syntax-parameterize
         ((&type (syntax-rules () ((_ val) (var-type in val))))
          (&min  (syntax-rules () ((_ val) (var-min in val))))
          (&max  (syntax-rules () ((_ val) (var-max in val)))))
       body ...))))

(define-syntax-rule (check-type arg type min max)
  ;; If the arg is negative, it is a closure variable.
  (and (>= arg 0)
       (zero? (logand (lognot type) (&type arg)))
       (<= min (&min arg))
       (<= (&max arg) max)))

(define-syntax-rule (define-type-inferrer (name var ...) body ...)
  (hashq-set!
   *type-inferrers*
   'name
   (lambda (out var ...)
     (syntax-parameterize
         ((define!
           (syntax-rules ()
             ((_ val type min max)
              (begin
                (extend-var-type! out val type)
                (extend-var-range! out val min max)))))
          (restrict!
           (syntax-rules ()
             ((_ val type min max)
              (when (>= val 0)
                (restrict-var-type! out val type)
                (restrict-var-range! out val min max)))))
          ;; Negative vals are closure variables.
          (&type (syntax-rules ()
                   ((_ val) (if (< val 0) &all-types (var-type out val)))))
          (&min  (syntax-rules ()
                   ((_ val) (if (< val 0) -inf.0 (var-min out val)))))
          (&max  (syntax-rules ()
                   ((_ val) (if (< val 0) +inf.0 (var-max out val))))))
       body ...
       (values)))))

(define-syntax-rule (define-predicate-inferrer (name var ... true?) body ...)
  (hashq-set!
   *predicate-inferrers*
   'name
   (lambda (out var ... true?)
     (syntax-parameterize
         ((restrict!
           (syntax-rules ()
             ((_ val type min max)
              (when (>= val 0)
                (restrict-var-type! out val type)
                (restrict-var-range! out val min max)))))
          ;; Negative vals are closure variables.
          (&type (syntax-rules ()
                   ((_ val) (if (< val 0) &all-types (var-type out val)))))
          (&min  (syntax-rules ()
                   ((_ val) (if (< val 0) -inf.0 (var-min out val)))))
          (&max  (syntax-rules ()
                   ((_ val) (if (< val 0) +inf.0 (var-max out val))))))
       body ...
       (values)))))

(define-syntax define-simple-type-checker
  (lambda (x)
    (define (parse-spec l)
      (syntax-case l ()
        (() '())
        (((type min max) . l) (cons #'(type min max) (parse-spec #'l)))
        (((type min+max) . l) (cons #'(type min+max min+max) (parse-spec #'l)))
        ((type . l) (cons #'(type -inf.0 +inf.0) (parse-spec #'l)))))
    (syntax-case x ()
      ((_ (name arg-spec ...) result-spec ...)
       (with-syntax
           (((arg ...) (generate-temporaries #'(arg-spec ...)))
            (((arg-type arg-min arg-max) ...) (parse-spec #'(arg-spec ...))))
         #'(define-type-checker (name arg ...)
             (and (check-type arg arg-type arg-min arg-max)
                  ...)))))))

(define-syntax define-simple-type-inferrer
  (lambda (x)
    (define (parse-spec l)
      (syntax-case l ()
        (() '())
        (((type min max) . l) (cons #'(type min max) (parse-spec #'l)))
        (((type min+max) . l) (cons #'(type min+max min+max) (parse-spec #'l)))
        ((type . l) (cons #'(type -inf.0 +inf.0) (parse-spec #'l)))))
    (syntax-case x ()
      ((_ (name arg-spec ...) result-spec ...)
       (with-syntax
           (((arg ...) (generate-temporaries #'(arg-spec ...)))
            (((arg-type arg-min arg-max) ...) (parse-spec #'(arg-spec ...)))
            ((res ...) (generate-temporaries #'(result-spec ...)))
            (((res-type res-min res-max) ...) (parse-spec #'(result-spec ...))))
         #'(define-type-inferrer (name arg ... res ...)
             (restrict! arg arg-type arg-min arg-max)
             ...
             (define! res res-type res-min res-max)
             ...))))))

(define-syntax-rule (define-simple-type (name arg-spec ...) result-spec ...)
  (begin
    (define-simple-type-checker (name arg-spec ...))
    (define-simple-type-inferrer (name arg-spec ...) result-spec ...)))

(define-syntax-rule (define-simple-types
                      ((name arg-spec ...) result-spec ...)
                      ...)
  (begin
    (define-simple-type (name arg-spec ...) result-spec ...)
    ...))

(define-syntax-rule (define-type-checker-aliases orig alias ...)
  (let ((check (hashq-ref *type-checkers* 'orig)))
    (hashq-set! *type-checkers* 'alias check)
    ...))
(define-syntax-rule (define-type-inferrer-aliases orig alias ...)
  (let ((check (hashq-ref *type-inferrers* 'orig)))
    (hashq-set! *type-inferrers* 'alias check)
    ...))
(define-syntax-rule (define-type-aliases orig alias ...)
  (begin
    (define-type-checker-aliases orig alias ...)
    (define-type-inferrer-aliases orig alias ...)))




;;; This list of primcall type definitions follows the order of
;;; effects-analysis.scm; please keep it in a similar order.
;;;
;;; There is no need to add checker definitions for expressions that do
;;; not exhibit the &type-check effect, as callers should not ask if
;;; such an expression does or does not type-check.  For those that do
;;; exhibit &type-check, you should define a type inferrer unless the
;;; primcall will never typecheck.
;;;
;;; Likewise there is no need to define inferrers for primcalls which
;;; return &all-types values and which never raise exceptions from which
;;; we can infer the types of incoming values.




;;;
;;; Miscellaneous.
;;;

(define-simple-type-checker (not &all-types))
(define-type-inferrer (not val result)
  (cond
   ((and (eqv? (&type val) &boolean)
         (eqv? (&min val) (&max val)))
    (let ((val (if (zero? (&min val)) 1 0)))
      (define! result &boolean val val)))
   (else
    (define! result &boolean 0 1))))




;;;
;;; Generic effect-free predicates.
;;;

(define-predicate-inferrer (eq? a b true?)
  ;; We can only propagate information down the true leg.
  (when true?
    (let ((type (logand (&type a) (&type b)))
          (min (max (&min a) (&min b)))
          (max (min (&max a) (&max b))))
      (restrict! a type min max)
      (restrict! b type min max))))
;; FIXME!!!!!
(define-type-inferrer-aliases eq? eqv? equal?)

(define-syntax-rule (define-simple-predicate-inferrer predicate type)
  (define-predicate-inferrer (predicate val true?)
    (let ((type (if true?
                    type
                    (logand (&type val) (lognot type)))))
      (restrict! val type -inf.0 +inf.0))))
(define-simple-predicate-inferrer pair? &pair)
(define-simple-predicate-inferrer null? &null)
(define-simple-predicate-inferrer nil? &nil)
(define-simple-predicate-inferrer symbol? &symbol)
(define-simple-predicate-inferrer variable? &box)
(define-simple-predicate-inferrer vector? &vector)
(define-simple-predicate-inferrer struct? &struct)
(define-simple-predicate-inferrer string? &string)
(define-simple-predicate-inferrer number? &number)
(define-simple-predicate-inferrer char? &char)
(define-simple-predicate-inferrer procedure? &procedure)
(define-simple-predicate-inferrer thunk? &procedure)



;;;
;;; Fluids.  Note that we can't track bound-ness of fluids, as pop-fluid
;;; can change boundness.
;;;

(define-simple-types
  ((fluid-ref (&fluid 1)) &all-types)
  ((fluid-set! (&fluid 0 1) &all-types))
  ((push-fluid (&fluid 0 1) &all-types))
  ((pop-fluid)))




;;;
;;; Prompts.  (Nothing to do.)
;;;




;;;
;;; Pairs.
;;;

(define-simple-types
  ((cons &all-types &all-types) &pair)
  ((car &pair) &all-types)
  ((set-car! &pair &all-types))
  ((cdr &pair) &all-types)
  ((set-cdr! &pair &all-types)))




;;;
;;; Variables.
;;;

(define-simple-types
  ((box &all-types) (&box 1))
  ((box-ref (&box 1)) &all-types))

(define-simple-type-checker (box-set! (&box 0 1) &all-types))
(define-type-inferrer (box-set! box val)
  (restrict! box &box 1 1))




;;;
;;; Vectors.
;;;

;; This max-vector-len computation is a hack.
(define *max-vector-len* (ash most-positive-fixnum -5))

(define-simple-type-checker (make-vector (&exact-integer 0 *max-vector-len*)
                                         &all-types))
(define-type-inferrer (make-vector size init result)
  (restrict! size &exact-integer 0 *max-vector-len*)
  (define! result &vector (&min size) (&max size)))

(define-type-checker (vector-ref v idx)
  (and (check-type v &vector 0 *max-vector-len*)
       (check-type idx &exact-integer 0 (1- (&min v)))))
(define-type-inferrer (vector-ref v idx result)
  (restrict! v &vector (1+ (&min idx)) +inf.0)
  (restrict! idx &exact-integer 0 (1- (&max v)))
  (define! result &all-types -inf.0 +inf.0))

(define-type-checker (vector-set! v idx val)
  (and (check-type v &vector 0 *max-vector-len*)
       (check-type idx &exact-integer 0 (1- (&min v)))))
(define-type-inferrer (vector-set! v idx val)
  (restrict! v &vector (1+ (&min idx)) +inf.0)
  (restrict! idx &exact-integer 0 (1- (&max v))))

(define-type-aliases make-vector make-vector/immediate)
(define-type-aliases vector-ref vector-ref/immediate)
(define-type-aliases vector-set! vector-set!/immediate)

(define-simple-type-checker (vector-length &vector))
(define-type-inferrer (vector-length v result)
  (restrict! v &vector 0 *max-vector-len*)
  (define! result &exact-integer (max (&min v) 0) (&max v)))




;;;
;;; Structs.
;;;

;; No type-checker for allocate-struct, as we can't currently check that
;; vt is actually a vtable.
(define-type-inferrer (allocate-struct vt size result)
  (restrict! vt &struct vtable-offset-user +inf.0)
  (restrict! size &exact-integer 0 +inf.0)
  (define! result &struct (max (&min size) 0) (&max size)))

(define-type-checker (struct-ref s idx)
  (and (check-type s &struct 0 +inf.0)
       (check-type idx &exact-integer 0 +inf.0)
       ;; FIXME: is the field readable?
       (< (&max idx) (&min s))))
(define-type-inferrer (struct-ref s idx result)
  (restrict! s &struct (1+ (&min idx)) +inf.0)
  (restrict! idx &exact-integer 0 (1- (&max s)))
  (define! result &all-types -inf.0 +inf.0))

(define-type-checker (struct-set! s idx val)
  (and (check-type s &struct 0 +inf.0)
       (check-type idx &exact-integer 0 +inf.0)
       ;; FIXME: is the field writable?
       (< (&max idx) (&min s))))
(define-type-inferrer (struct-set! s idx val)
  (restrict! s &struct (1+ (&min idx)) +inf.0)
  (restrict! idx &exact-integer 0 (1- (&max s))))

(define-type-aliases allocate-struct allocate-struct/immediate)
(define-type-aliases struct-ref struct-ref/immediate)
(define-type-aliases struct-set! struct-set!/immediate)

(define-simple-type (struct-vtable (&struct 0 +inf.0))
  (&struct vtable-offset-user +inf.0))




;;;
;;; Strings.
;;;

(define *max-char* (1- (ash 1 24)))

(define-type-checker (string-ref s idx)
  (and (check-type s &string 0 +inf.0)
       (check-type idx &exact-integer 0 +inf.0)
       (< (&max idx) (&min s))))
(define-type-inferrer (string-ref s idx result)
  (restrict! s &string (1+ (&min idx)) +inf.0)
  (restrict! idx &exact-integer 0 (1- (&max s)))
  (define! result &char 0 *max-char*))

(define-type-checker (string-set! s idx val)
  (and (check-type s &string 0 +inf.0)
       (check-type idx &exact-integer 0 +inf.0)
       (check-type val &char 0 *max-char*)
       (< (&max idx) (&min s))))
(define-type-inferrer (string-set! s idx val)
  (restrict! s &string (1+ (&min idx)) +inf.0)
  (restrict! idx &exact-integer 0 (1- (&max s)))
  (restrict! val &char 0 *max-char*))

(define-simple-type-checker (string-length &string))
(define-type-inferrer (string-length s result)
  (restrict! s &string 0 +inf.0)
  (define! result &exact-integer (max (&min s) 0) (&max s)))

(define-simple-type (number->string &number) (&string 0 +inf.0))
(define-simple-type (string->number (&string 0 +inf.0))
  ((logior &number &boolean) -inf.0 +inf.0))




;;;
;;; Bytevectors.
;;;

(define-simple-type-checker (bytevector-length &bytevector))
(define-type-inferrer (bytevector-length bv result)
  (restrict! bv &bytevector 0 +inf.0)
  (define! result &exact-integer (max (&min bv) 0) (&max bv)))

(define-syntax-rule (define-bytevector-accessors ref set type size min max)
  (begin
    (define-type-checker (ref bv idx)
      (and (check-type bv &bytevector 0 +inf.0)
           (check-type idx &exact-integer 0 +inf.0)
           (< (&max idx) (- (&min bv) size))))
    (define-type-inferrer (ref bv idx result)
      (restrict! bv &bytevector (+ (&min idx) size) +inf.0)
      (restrict! idx &exact-integer 0 (- (&max bv) size))
      (define! result type min max))
    (define-type-checker (set bv idx val)
      (and (check-type bv &bytevector 0 +inf.0)
           (check-type idx &exact-integer 0 +inf.0)
           (check-type val type min max)
           (< (&max idx) (- (&min bv) size))))
    (define-type-inferrer (set! bv idx val)
      (restrict! bv &bytevector (+ (&min idx) size) +inf.0)
      (restrict! idx &exact-integer 0 (- (&max bv) size))
      (restrict! val type min max))))

(define-syntax-rule (define-short-bytevector-accessors ref set size signed?)
  (define-bytevector-accessors ref set &exact-integer size
    (if signed? (- (ash 1 (1- (* size 8)))) 0)
    (1- (ash 1 (if signed? (1- (* size 8)) (* size 8))))))

(define-short-bytevector-accessors bv-u8-ref bv-u8-set! 1 #f)
(define-short-bytevector-accessors bv-s8-ref bv-s8-set! 1 #t)
(define-short-bytevector-accessors bv-u16-ref bv-u16-set! 2 #f)
(define-short-bytevector-accessors bv-s16-ref bv-s16-set! 2 #t)

;; The range analysis only works on signed 32-bit values, so some limits
;; are out of range.
(define-bytevector-accessors bv-u32-ref bv-u32-set! &exact-integer 4 0 +inf.0)
(define-bytevector-accessors bv-s32-ref bv-s32-set! &exact-integer 4 -inf.0 +inf.0)
(define-bytevector-accessors bv-u64-ref bv-u64-set! &exact-integer 8 0 +inf.0)
(define-bytevector-accessors bv-s64-ref bv-s64-set! &exact-integer 8 -inf.0 +inf.0)
(define-bytevector-accessors bv-f32-ref bv-f32-set! &real 4 -inf.0 +inf.0)
(define-bytevector-accessors bv-f64-ref bv-f64-set! &real 8 -inf.0 +inf.0)




;;;
;;; Numbers.
;;;

;; First, branching primitives with no results.
(define-simple-type-checker (= &number &number))
(define-predicate-inferrer (= a b true?)
  (when (and true?
             (zero? (logand (logior (&type a) (&type b)) (lognot &number))))
    (let ((min (max (&min a) (&min b)))
          (max (min (&max a) (&max b))))
      (restrict! a &number min max)
      (restrict! b &number min max))))

(define-simple-type-checker (< &real &real))
(define-predicate-inferrer (< a b true?)
  (when (zero? (logand (logior (&type a) (&type b)) (lognot &number)))
    (restrict! a &real -inf.0 +inf.0)
    (restrict! b &real -inf.0 +inf.0)))
;; FIXME!!!
(define-type-aliases < <= > >=)

;; Arithmetic.
(define-syntax-rule (define-unary-result! a result min max)
  (let ((min* min)
        (max* max)
        (type (logand (&type a) &number)))
    (cond
     ((not (= type (&type a)))
      ;; Not a number.  Punt and do nothing.
      (define! result &all-types -inf.0 +inf.0))
     ;; Complex numbers don't have a range.
     ((eqv? type &complex)
      (define! result &complex -inf.0 +inf.0))
     (else
      (define! result type min* max*)))))

(define-syntax-rule (define-binary-result! a b result closed? min max)
  (let ((min* min)
        (max* max)
        (a-type (logand (&type a) &number))
        (b-type (logand (&type b) &number)))
    (cond
     ((or (not (= a-type (&type a))) (not (= b-type (&type b))))
      ;; One input not a number.  Perhaps we end up dispatching to
      ;; GOOPS.
      (define! result &all-types -inf.0 +inf.0))
     ;; Complex and floating-point numbers are contagious.
     ((or (eqv? a-type &complex) (eqv? b-type &complex))
      (define! result &complex -inf.0 +inf.0))
     ((or (eqv? a-type &flonum) (eqv? b-type &flonum))
      (define! result &flonum min* max*))
     ;; Exact integers are closed under some operations.
     ((and closed? (eqv? a-type &exact-integer) (eqv? b-type &exact-integer))
      (define! result &exact-integer min* max*))
     (else
      ;; Fractions may become integers.
      (let ((type (logior a-type b-type)))
        (define! result
                 (if (zero? (logand type &fraction))
                     type
                     (logior type &exact-integer))
                 min* max*))))))

(define-simple-type-checker (add &number &number))
(define-type-inferrer (add a b result)
  (define-binary-result! a b result #t
                         (+ (&min a) (&min b))
                         (+ (&max a) (&max b))))

(define-simple-type-checker (sub &number &number))
(define-type-inferrer (sub a b result)
  (define-binary-result! a b result #t
                         (- (&min a) (&max b))
                         (- (&max a) (&min b))))

(define-simple-type-checker (mul &number &number))
(define-type-inferrer (mul a b result)
  (let ((min-a (&min a)) (max-a (&max a))
        (min-b (&min b)) (max-b (&max b)))
    (let ((-- (* min-a min-b))
          (-+ (* min-a max-b))
          (++ (* max-a max-b))
          (+- (* max-a min-b)))
      (define-binary-result! a b result #t
                             (if (eqv? a b) 0 (min -- -+ ++ +-))
                             (max -- -+ ++ +-)))))

(define-type-checker (div a b)
  (and (check-type a &number -inf.0 +inf.0)
       (check-type b &number -inf.0 +inf.0)
       ;; We only know that there will not be an exception if b is not
       ;; zero.
       (not (<= (&min b) 0 (&max b)))))
(define-type-inferrer (div a b result)
  (let ((min-a (&min a)) (max-a (&max a))
        (min-b (&min b)) (max-b (&max b)))
    (call-with-values
        (lambda ()
          (if (<= min-b 0 max-b)
              ;; If the range of the divisor crosses 0, the result spans
              ;; the whole range.
              (values -inf.0 +inf.0)
              ;; Otherwise min-b and max-b have the same sign, and cannot both
              ;; be infinity.
              (let ((-- (if (inf? min-b) 0 (* min-a min-b)))
                    (-+ (if (inf? max-b) 0 (* min-a max-b)))
                    (++ (if (inf? max-b) 0 (* max-a max-b)))
                    (+- (if (inf? min-b) 0 (* max-a min-b))))
                (values (min -- -+ ++ +-)
                        (max -- -+ ++ +-)))))
      (lambda (min max)
        (define-binary-result! a b result #f min max)))))

(define-simple-type-checker (add1 &number))
(define-type-inferrer (add1 a result)
  (define-unary-result! a result (1+ (&min a)) (1+ (&max a))))

(define-simple-type-checker (sub1 &number))
(define-type-inferrer (sub1 a result)
  (define-unary-result! a result (1- (&min a)) (1- (&max a))))

(define-type-checker (quo a b)
  (and (check-type a &exact-integer -inf.0 +inf.0)
       (check-type b &exact-integer -inf.0 +inf.0)
       ;; We only know that there will not be an exception if b is not
       ;; zero.
       (not (<= (&min b) 0 (&max b)))))
(define-type-inferrer (quo a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  (define! result &exact-integer -inf.0 +inf.0))

(define-type-checker-aliases quo rem)
(define-type-inferrer (rem a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  ;; Same sign as A.
  (let ((max-abs-rem (1- (max (abs (&min b)) (abs (&max b))))))
    (cond
     ((< (&min a) 0)
      (if (< 0 (&max a))
          (define! result &exact-integer (- max-abs-rem) max-abs-rem)
          (define! result &exact-integer (- max-abs-rem) 0)))
     (else
      (define! result &exact-integer 0 max-abs-rem)))))

(define-type-checker-aliases quo mod)
(define-type-inferrer (mod a b result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  ;; Same sign as B.
  (let ((max-abs-mod (1- (max (abs (&min b)) (abs (&max b))))))
    (cond
     ((< (&min b) 0)
      (if (< 0 (&max b))
          (define! result &exact-integer (- max-abs-mod) max-abs-mod)
          (define! result &exact-integer (- max-abs-mod) 0)))
     (else
      (define! result &exact-integer 0 max-abs-mod)))))

;; Predicates.
(define-syntax-rule (define-number-kind-predicate-inferrer name type)
  (define-type-inferrer (name val result)
    (cond
     ((zero? (logand (&type val) type))
      (define! result &boolean 0 0))
     ((zero? (logand (&type val) (lognot type)))
      (define! result &boolean 1 1))
     (else
      (define! result &boolean 0 1)))))
(define-number-kind-predicate-inferrer complex? &number)
(define-number-kind-predicate-inferrer real? &real)
(define-number-kind-predicate-inferrer rational?
  (logior &exact-integer &fraction))
(define-number-kind-predicate-inferrer integer?
  (logior &exact-integer &flonum))
(define-number-kind-predicate-inferrer exact-integer?
  &exact-integer)

(define-simple-type-checker (exact? &number))
(define-type-inferrer (exact? val result)
  (restrict! val &number -inf.0 +inf.0)
  (cond
   ((zero? (logand (&type val) (logior &exact-integer &fraction)))
    (define! result &boolean 0 0))
   ((zero? (logand (&type val) (lognot (logior &exact-integer &fraction))))
    (define! result &boolean 1 1))
   (else
    (define! result &boolean 0 1))))

(define-simple-type-checker (inexact? &number))
(define-type-inferrer (inexact? val result)
  (restrict! val &number -inf.0 +inf.0)
  (cond
   ((zero? (logand (&type val) (logior &flonum &complex)))
    (define! result &boolean 0 0))
   ((zero? (logand (&type val) (lognot (logior &flonum &complex))))
    (define! result &boolean 1 1))
   (else
    (define! result &boolean 0 1))))

(define-simple-type-checker (inf? &real))
(define-type-inferrer (inf? val result)
  (restrict! val &real -inf.0 +inf.0)
  (cond
   ((or (zero? (logand (&type val) (logior &flonum &complex)))
        (and (not (inf? (&min val))) (not (inf? (&max val)))))
    (define! result &boolean 0 0))
   (else
    (define! result &boolean 0 1))))

(define-type-aliases inf? nan?)

(define-simple-type (even? &exact-integer) (&boolean 0 1))
(define-type-aliases even? odd?)

;; Bit operations.
(define-simple-type-checker (ash &exact-integer &exact-integer))
(define-type-inferrer (ash val count result)
  (define (ash* val count)
    ;; As we can only represent a 32-bit range, don't bother inferring
    ;; shifts that might exceed that range.
    (cond
     ((inf? val) val) ; Preserves sign.
     ((< -32 count 32) (ash val count))
     ((zero? val) 0)
     ((positive? val) +inf.0)
     (else -inf.0)))
  (restrict! val &exact-integer -inf.0 +inf.0)
  (restrict! count &exact-integer -inf.0 +inf.0)
  (let ((-- (ash* (&min val) (&min count)))
        (-+ (ash* (&min val) (&max count)))
        (++ (ash* (&max val) (&max count)))
        (+- (ash* (&max val) (&min count))))
    (define! result &exact-integer
             (min -- -+ ++ +-)
             (max -- -+ ++ +-))))

(define (next-power-of-two n)
  (let lp ((out 1))
    (if (< n out)
        out
        (lp (ash out 1)))))

(define-simple-type-checker (logand &exact-integer &exact-integer))
(define-type-inferrer (logand a b result)
  (define (logand-min a b)
    (if (< a b 0)
        (min a b)
        0))
  (define (logand-max a b)
    (if (< a b 0)
        0
        (max a b)))
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  (define! result &exact-integer
           (logand-min (&min a) (&min b))
           (logand-max (&max a) (&max b))))

(define-simple-type-checker (logior &exact-integer &exact-integer))
(define-type-inferrer (logior a b result)
  ;; Saturate all bits of val.
  (define (saturate val)
    (1- (next-power-of-two val)))
  (define (logior-min a b)
    (cond ((and (< a 0) (<= 0 b)) a)
          ((and (< b 0) (<= 0 a)) b)
          (else (max a b))))
  (define (logior-max a b)
    ;; If either operand is negative, just assume the max is -1.
    (cond
     ((or (< a 0) (< b 0)) -1)
     ((or (inf? a) (inf? b)) +inf.0)
     (else (saturate (logior a b)))))
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0)
  (define! result &exact-integer
           (logior-min (&min a) (&min b))
           (logior-max (&max a) (&max b))))

;; For our purposes, treat logxor the same as logior.
(define-type-aliases logior logxor)

(define-simple-type-checker (lognot &exact-integer))
(define-type-inferrer (lognot a result)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (define! result &exact-integer
           (- -1 (&max a))
           (- -1 (&min a))))

;; Flonums.
(define-simple-type-checker (sqrt &number))
(define-type-inferrer (sqrt x result)
  (restrict! x &number -inf.0 +inf.0)
  (let ((type (&type x)))
    (cond
     ((and (zero? (logand type &complex)) (<= 0 (&min x)))
      (define! result
               (logior type &flonum)
               (inexact->exact (floor (sqrt (&min x))))
               (if (inf? (&max x))
                   +inf.0
                   (inexact->exact (ceiling (sqrt (&max x)))))))
     (else
      (define! result (logior type &flonum &complex) -inf.0 +inf.0)))))

(define-simple-type-checker (abs &real))
(define-type-inferrer (abs x result)
  (restrict! x &real -inf.0 +inf.0)
  (define! result (logior (logand (&type x) (lognot &number))
                          (logand (&type x) &real))
           (min (abs (&min x)) (abs (&max x)))
           (max (abs (&min x)) (abs (&max x)))))




;;;
;;; Characters.
;;;

(define-simple-type (char<? &char &char) (&boolean 0 1))
(define-type-aliases char<? char<=? char>=? char>?)

(define-simple-type-checker (integer->char (&exact-integer 0 #x10ffff)))
(define-type-inferrer (integer->char i result)
  (restrict! i &exact-integer 0 #x10ffff)
  (define! result &char (&min i) (&max i)))

(define-simple-type-checker (char->integer &char))
(define-type-inferrer (char->integer c result)
  (restrict! c &char 0 #x10ffff)
  (define! result &exact-integer (&min c) (&max c)))




;;;
;;; Type flow analysis: the meet (ahem) of the algorithm.
;;;

(define (infer-types* dfg min-label label-count min-var var-count)
  "Compute types for all variables in @var{fun}.  Returns a hash table
mapping symbols to types."
  (let* ((typev (make-vector (* 2 label-count) #f))
         (changed (make-bitvector var-count #f))
         (changed-types (make-bitvector var-count #f))
         (changed-ranges (make-bitvector var-count #f))
         (revisit-labels (make-bitvector label-count #f))
         (tmp (make-bytevector (* var-count 12) 0))
         (tmp2 (make-bytevector (* var-count 12) 0))
         (saturate? #f))
    (define (var->idx var) (- var min-var))
    (define (idx->var idx) (+ idx min-var))
    (define (label->idx label) (- label min-label))
    (define (idx->label idx) (+ idx min-label))

    (define (get-pre-types label)
      (vector-ref typev (* (label->idx label) 2)))
    (define (get-post-types label)
      (vector-ref typev (1+ (* (label->idx label) 2))))

    (define (define! bv val type min max)
      (extend-var-type! bv val type)
      (extend-var-range! bv val min max))

    (define (restrict! bv val type min max)
      (when (>= val 0)
        (restrict-var-type! bv val type)
        (restrict-var-range! bv val min max)))

    (define (infer-primcall! out name args result)
      (let lp ((args args))
        (match args
          ((arg . args)
           ;; Primcall operands can originate outside the function.
           (when (<= 0 arg)
             (bitvector-set! changed arg #t))
           (lp args))
          (_ #f)))
      (when result
        (bitvector-set! changed result #t))
      (let ((inferrer (hashq-ref *type-inferrers* name)))
        (if inferrer
            ;; FIXME: remove the apply?
            (apply inferrer out
                   (if result
                       (append args (list result))
                       args))
            (when result
              (define! out result &all-types -inf.0 +inf.0)))))

    (define (infer-predicate! out name args true?)
      (let ((pred-inferrer (hashq-ref *predicate-inferrers* name)))
        (when pred-inferrer
          ;; FIXME: remove the apply?
          (apply pred-inferrer out (append args (list true?))))))

    (define (propagate-types! k in)
      (match (lookup-predecessors k dfg)
        ((_)
         ;; Fast path: we dominate the successor.  Just copy; there's no
         ;; need to set bits in the "revisit-labels" set because we'll
         ;; reach the successor in this iteration anyway.
         (let ((out (get-pre-types k)))
           (bytevector-copy! in 0 out 0 (* var-count 12))
           out))
        (_
         (propagate-types/slow! k in))))

    (define (propagate-types/slow! k in)
      (let ((out (get-pre-types k)))
        ;; Slow path: union.
        (let lp ((n 0))
          (let ((n (bit-position #t changed-types n)))
            (when n
              (let ((in-type (var-type in n))
                    (out-type (var-type out n)))
                (let ((type (logior in-type out-type)))
                  (unless (= type out-type)
                    (bitvector-set! revisit-labels (label->idx k) #t)
                    (set-var-type! out n type))))
              (lp (1+ n)))))
        (let lp ((n 0))
          (let ((n (bit-position #t changed-ranges n)))
            (when n
              (let ((in-min (var-clamped-min in n))
                    (in-max (var-clamped-max in n))
                    (out-min (var-clamped-min out n))
                    (out-max (var-clamped-max out n)))
                (let ((min (min in-min out-min)))
                  (unless (= min out-min)
                    (bitvector-set! revisit-labels (label->idx k) #t)
                    (set-var-min! out n (if saturate? *min-s32* min))))
                (let ((max (max in-max out-max)))
                  (unless (= max out-max)
                    (bitvector-set! revisit-labels (label->idx k) #t)
                    (set-var-max! out n (if saturate? *max-s32* max)))))
              (lp (1+ n)))))))

    ;; Initialize "tmp" as a template.
    (let lp ((n 0))
      (when (< n var-count)
        (set-var-min! tmp n +inf.0)
        (set-var-max! tmp n -inf.0)
        (lp (1+ n))))

    ;; Initial state: invalid range, no types.
    (let lp ((n 0))
      (define (make-fresh-type-vector var-count)
        (let ((bv (make-bytevector (* var-count 12) 0)))
          (bytevector-copy! tmp 0 bv 0 (* var-count 12))
          bv))
      (when (< n label-count)
        (vector-set! typev (* n 2) (make-fresh-type-vector var-count))
        (vector-set! typev (1+ (* n 2)) (make-fresh-type-vector var-count))
        (lp (1+ n))))

    ;; Iterate over all labels in the function.  When visiting a label
    ;; N, we first propagate N's types to the continuation, then refine
    ;; those types in place (at the continuation).  This is consistent
    ;; with an interpretation that the types at a labelled expression
    ;; describe the values before the expression is evaluated, i.e., the
    ;; types that flow into a label.
    (let lp ((label min-label))
      (cond
       ((< label (+ min-label label-count))
        (let ((pre (get-pre-types label))
              (post (get-post-types label)))
          ;; First, clear the "changed" bitvector and save a copy of the
          ;; "post" set, so we can detect what changes in this
          ;; expression.
          (let ((revisit? (bitvector-ref revisit-labels (label->idx label))))
            ;; Check all variables for changes in expressions that we
            ;; are revisiting because of a changed incoming type or
            ;; range on a control-flow join.
            (bitvector-fill! changed revisit?))
          (bitvector-set! revisit-labels (label->idx label) #f)
          (bytevector-copy! post 0 tmp 0 (bytevector-length post))

          ;; Now copy the incoming types to the outgoing types.
          (bytevector-copy! pre 0 post 0 (bytevector-length post))

          ;; Add types for new definitions, and restrict types of
          ;; existing variables due to side effects.
          (match (lookup-cont label dfg)
            (($ $kargs names vars term)
             (let visit-term ((term term))
               (match term
                 (($ $letrec names vars funs term)
                  (let lp ((vars vars))
                    (match vars
                      ((var . vars)
                       (let ((def (var->idx var)))
                         (bitvector-set! changed def #t)
                         (define! post def &procedure -inf.0 +inf.0)
                         (lp vars)))
                      (_ (visit-term term)))))
                 (($ $letk conts term)
                  (visit-term term))
                 (($ $continue k src exp)
                  (match exp
                    (($ $branch kt exp)
                     ;; The "normal" continuation is the #f branch.
                     ;; For the #t branch we need to roll our own
                     ;; "changed" logic.  This will be refactored
                     ;; in the future.
                     (let ((kt-out tmp2))
                       (bytevector-copy! pre 0 kt-out 0 (bytevector-length pre))
                       (match exp
                         (($ $values (arg))
                          (let ((arg (var->idx arg)))
                            (unless (< arg 0)
                              (bitvector-set! changed arg #t)
                              (restrict! post arg (logior &boolean &nil) 0 0))
                            ;; No additional information on the #t branch,
                            ;; as there's no way currently to remove #f
                            ;; from the typeset (because it would remove
                            ;; #t as well: they are both &boolean).
                            ))
                         (($ $primcall name args)
                          (let ((args (map var->idx args)))
                            ;; For the #t branch we need to roll our own
                            ;; "changed" logic.  This will be refactored
                            ;; in the future.
                            (define (update-changelist! k from var)
                              (let ((to (get-pre-types k)))
                                (unless (or (< var 0)
                                            (bitvector-ref changed-types var)
                                            (= (logior (var-type from var)
                                                       (var-type to var))
                                               (var-type to var)))
                                  (bitvector-set! changed-types var #t))
                                (unless (or (< var 0)
                                            (bitvector-ref changed-ranges var)
                                            (and
                                             (<= (var-min to var) (var-min from var))
                                             (<= (var-max from var) (var-max to var))))
                                  (bitvector-set! changed-ranges var #t))))
                            ;; The "normal" continuation is the #f branch.
                            (infer-predicate! post name args #f)
                            (infer-predicate! kt-out name args #t)
                            (let lp ((args args))
                              (match args
                                ((arg . args)
                                 ;; Primcall operands can originate
                                 ;; outside the function.
                                 (when (<= 0 arg)
                                   ;; `out' will be scanned below.
                                   (bitvector-set! changed arg #t)
                                   ;; But we need to manually scan
                                   ;; kt-out.
                                   (update-changelist! kt kt-out arg))
                                 (lp args))
                                (_ #f))))))
                       ;; Manually propagate the kt branch.
                       (propagate-types! kt kt-out)))
                    (($ $primcall name args)
                     (match (lookup-cont k dfg)
                       (($ $kargs (_) (var))
                        (let ((def (var->idx var)))
                          (infer-primcall! post name (map var->idx args) def)))
                       (($ $kargs ())
                        (infer-primcall! post name (map var->idx args) #f))
                       (_ #f)))
                    (($ $values args)
                     (match (lookup-cont k dfg)
                       (($ $kargs _ defs)
                        (let lp ((defs defs) (args args))
                          (match (cons defs args)
                            ((() . ()) #f)
                            (((def . defs) . (arg . args))
                             (let ((def (var->idx def)) (arg (var->idx arg)))
                               (bitvector-set! changed def #t)
                               (if (< arg 0)
                                   (define! post def &all-types -inf.0 +inf.0)
                                   (define! post def (var-type post arg)
                                     (var-min post arg) (var-max post arg))))
                             (lp defs args)))))
                       (_ #f)))
                    ((or ($ $call) ($ $callk) ($ $prompt))
                     ;; Nothing to do.
                     #t)
                    (_
                     (call-with-values
                         (lambda ()
                           (match exp
                             (($ $void)
                              (values &unspecified -inf.0 +inf.0))
                             (($ $const val)
                              (constant-type val))
                             ((or ($ $prim) ($ $fun) ($ $closure))
                              ;; Could be more precise here.
                              (values &procedure -inf.0 +inf.0))))
                       (lambda (type min max)
                         (match (lookup-cont k dfg)
                           (($ $kargs (_) (var))
                            (let ((def (var->idx var)))
                              (bitvector-set! changed def #t)
                              (define! post def type min max))))))))))))
            (cont
             (let lp ((vars (match cont
                              (($ $kreceive arity k*)
                               (match (lookup-cont k* dfg)
                                 (($ $kargs names vars) vars)))
                              (($ $kfun src meta self)
                               (list self))
                              (($ $kclause arity ($ $cont kbody))
                               (match (lookup-cont kbody dfg)
                                 (($ $kargs names vars) vars)))
                              (_ '()))))
               (match vars
                 (() #t)
                 ((var . vars)
                  (bitvector-set! changed (var->idx var) #t)
                  (define! post (var->idx var) &all-types -inf.0 +inf.0)
                  (lp vars))))))

          ;; Now determine the set of changed variables.
          (let lp ((n 0))
            (let ((n (bit-position #t changed n)))
              (when n
                (unless (eqv? (var-type tmp n) (var-type post n))
                  (bitvector-set! changed-types n #t))
                (unless (and (eqv? (var-clamped-min tmp n)
                                   (var-clamped-min post n))
                             (eqv? (var-clamped-max tmp n)
                                   (var-clamped-max post n)))
                  (bitvector-set! changed-ranges n #t))
                (lp (1+ n)))))
          
          ;; Propagate outgoing types to successors.
          (match (lookup-cont label dfg)
            (($ $kargs names vars term)
             (match (find-call term)
               (($ $continue k src exp)
                (propagate-types! k post)
                (match exp
                  (($ $prompt escape? tag handler)
                   (propagate-types! handler post))
                  (_ #f)))))
            (($ $kreceive arity k*)
             (propagate-types! k* post))
            (($ $kfun src meta self tail clause)
             (let lp ((clause clause))
               (match clause
                 (#f #f)
                 (($ $cont k ($ $kclause arity body alternate))
                  (propagate-types! k post)
                  (lp alternate)))))
            (($ $kclause arity ($ $cont kbody))
             (propagate-types! kbody post))
            (_ #f)))

        ;; And loop.
        (lp (1+ label)))

       ;; Iterate until the types reach a fixed point.
       ((bit-position #t changed-types 0)
        (bitvector-fill! changed-types #f)
        (bitvector-fill! changed-ranges #f)
        (lp min-label))

       ;; Once the types have a fixed point, iterate until ranges also
       ;; reach a fixed point, saturating ranges to accelerate
       ;; convergence.
       ((or (bit-position #t changed-ranges 0)
            (bit-position #t revisit-labels 0))
        (bitvector-fill! changed-ranges #f)
        (set! saturate? #t)
        (lp min-label))

       ;; All done!  Return the computed types.
       (else typev)))))

(define* (infer-types fun dfg #:key (max-label-count +inf.0))
  ;; Fun must be renumbered.
  (match fun
    (($ $cont min-label ($ $kfun _ _ min-var))
     (call-with-values
         (lambda ()
           ((make-local-cont-folder label-count var-count)
            (lambda (k cont label-count var-count)
              (define (min* var vars)
                (match vars
                  ((var* . vars)
                   (min* (min var var*) vars))
                  (_ var)))
              (let ((label-count (1+ label-count)))
                (match cont
                  (($ $kargs names vars body)
                   (let lp ((body body)
                            (var-count (+ var-count (length vars))))
                     (match body
                       (($ $letrec names vars funs body)
                        (lp body
                            (+ var-count (length vars))))
                       (($ $letk conts body)
                        (lp body var-count))
                       (_ (values label-count var-count)))))
                  (($ $kfun src meta self)
                   (values label-count (1+ var-count)))
                  (_
                   (values label-count var-count)))))
            fun 0 0))
       (lambda (label-count var-count)
         (and (< label-count max-label-count)
              (infer-types* dfg min-label label-count min-var var-count)))))))

(define (lookup-pre-type typev label def)
  (if (< def 0)
      (values &all-types -inf.0 +inf.0)
      (let ((types (vector-ref typev (* label 2))))
        (values (var-type types def)
                (var-min types def)
                (var-max types def)))))

(define (lookup-post-type typev label def)
  (if (< def 0)
      (values &all-types -inf.0 +inf.0)
      (let ((types (vector-ref typev (1+ (* label 2)))))
        (values (var-type types def)
                (var-min types def)
                (var-max types def)))))

(define (primcall-types-check? label-idx typev name arg-idxs)
  (let ((checker (hashq-ref *type-checkers* name)))
    (and checker
         (apply checker (vector-ref typev (* label-idx 2)) arg-idxs))))
