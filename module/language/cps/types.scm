;;; Type analysis on CPS
;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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
;;; non-negative.  So what we do instead is to precisely propagate types
;;; and ranges when propagating forward, but after the first backwards
;;; branch is seen, we cause backward branches that would expand the
;;; range of a value to saturate that range towards positive or negative
;;; infinity (as appropriate).
;;;
;;; A naive approach to type analysis would build up a table that has
;;; entries for all variables at all program points, but this has
;;; N-squared complexity and quickly grows unmanageable.  Instead, we
;;; use _intmaps_ from (language cps intmap) to share state between
;;; connected program points.
;;;
;;; Code:

(define-module (language cps types)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:export (;; Specific types.
            &exact-integer
            &flonum
            &complex
            &fraction

            &char
            &unspecified
            &unbound
            &false
            &true
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
  &false
  &true
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



(define-syntax-rule (define-compile-time-value name val)
  (define-syntax name
    (make-variable-transformer
     (lambda (x)
       (syntax-case x (set!)
         (var (identifier? #'var)
              (datum->syntax #'var val)))))))

(define-compile-time-value min-fixnum most-negative-fixnum)
(define-compile-time-value max-fixnum most-positive-fixnum)

(define-inlinable (make-unclamped-type-entry type min max)
  (vector type min max))
(define-inlinable (type-entry-type tentry)
  (vector-ref tentry 0))
(define-inlinable (type-entry-clamped-min tentry)
  (vector-ref tentry 1))
(define-inlinable (type-entry-clamped-max tentry)
  (vector-ref tentry 2))

(define-syntax-rule (clamp-range val)
  (cond
   ((< val min-fixnum) min-fixnum)
   ((< max-fixnum val) max-fixnum)
   (else val)))

(define-inlinable (make-type-entry type min max)
  (vector type (clamp-range min) (clamp-range max)))
(define-inlinable (type-entry-min tentry)
  (let ((min (type-entry-clamped-min tentry)))
    (if (eq? min min-fixnum) -inf.0 min)))
(define-inlinable (type-entry-max tentry)
  (let ((max (type-entry-clamped-max tentry)))
    (if (eq? max max-fixnum) +inf.0 max)))

(define all-types-entry (make-type-entry &all-types -inf.0 +inf.0))

(define* (var-type-entry typeset var #:optional (default all-types-entry))
  (intmap-ref typeset var (lambda (_) default)))

(define (var-type typeset var)
  (type-entry-type (var-type-entry typeset var)))
(define (var-min typeset var)
  (type-entry-min (var-type-entry typeset var)))
(define (var-max typeset var)
  (type-entry-max (var-type-entry typeset var)))

;; Is the type entry A contained entirely within B?
(define (type-entry<=? a b)
  (match (cons a b)
    ((#(a-type a-min a-max) . #(b-type b-min b-max))
     (and (eqv? b-type (logior a-type b-type))
          (<= b-min a-min)
          (>= b-max a-max)))))

(define (type-entry-union a b)
  (cond
   ((type-entry<=? b a) a)
   ((type-entry<=? a b) b)
   (else (make-type-entry
          (logior (type-entry-type a) (type-entry-type b))
          (min (type-entry-clamped-min a) (type-entry-clamped-min b))
          (max (type-entry-clamped-max a) (type-entry-clamped-max b))))))

(define (type-entry-saturating-union a b)
  (cond
   ((type-entry<=? b a) a)
   (else
    (make-type-entry
     (logior (type-entry-type a) (type-entry-type b))
     (let ((a-min (type-entry-clamped-min a))
           (b-min (type-entry-clamped-min b)))
       (if (< b-min a-min) min-fixnum a-min))
     (let ((a-max (type-entry-clamped-max a))
           (b-max (type-entry-clamped-max b)))
       (if (> b-max a-max) max-fixnum a-max))))))

(define (type-entry-intersection a b)
  (cond
   ((type-entry<=? a b) a)
   ((type-entry<=? b a) b)
   (else (make-type-entry
          (logand (type-entry-type a) (type-entry-type b))
          (max (type-entry-clamped-min a) (type-entry-clamped-min b))
          (min (type-entry-clamped-max a) (type-entry-clamped-max b))))))

(define (adjoin-var typeset var entry)
  (intmap-add typeset var entry type-entry-union))

(define (restrict-var typeset var entry)
  (intmap-add typeset var entry type-entry-intersection))

(define (constant-type val)
  "Compute the type and range of VAL.  Return three values: the type,
minimum, and maximum."
  (define (return type val)
    (if val
        (make-type-entry type val val)
        (make-type-entry type -inf.0 +inf.0)))
  (cond
   ((number? val)
    (cond
     ((exact-integer? val) (return &exact-integer val))
     ((eqv? (imag-part val) 0)
      (if (nan? val)
          (make-type-entry &flonum -inf.0 +inf.0)
          (make-type-entry
           (if (exact? val) &fraction &flonum)
           (if (rational? val) (inexact->exact (floor val)) val)
           (if (rational? val) (inexact->exact (ceiling val)) val))))
     (else (return &complex #f))))
   ((eq? val '()) (return &null #f))
   ((eq? val #nil) (return &nil #f))
   ((eq? val #t) (return &true #f))
   ((eq? val #f) (return &false #f))
   ((char? val) (return &char (char->integer val)))
   ((eqv? val *unspecified*) (return &unspecified #f))
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

(define *type-checkers* (make-hash-table))
(define *type-inferrers* (make-hash-table))

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
   (lambda (typeset arg ...)
     (syntax-parameterize
         ((&type (syntax-rules () ((_ val) (var-type typeset val))))
          (&min  (syntax-rules () ((_ val) (var-min typeset val))))
          (&max  (syntax-rules () ((_ val) (var-max typeset val)))))
       body ...))))

(define-syntax-rule (check-type arg type min max)
  ;; If the arg is negative, it is a closure variable.
  (and (>= arg 0)
       (zero? (logand (lognot type) (&type arg)))
       (<= min (&min arg))
       (<= (&max arg) max)))

(define-syntax-rule (define-type-inferrer* (name succ var ...) body ...)
  (hashq-set!
   *type-inferrers*
   'name
   (lambda (in succ var ...)
     (let ((out in))
       (syntax-parameterize
           ((define!
              (syntax-rules ()
                ((_ val type min max)
                 (set! out (adjoin-var out val
                                       (make-type-entry type min max))))))
            (restrict!
             (syntax-rules ()
               ((_ val type min max)
                (set! out (restrict-var out val
                                        (make-type-entry type min max))))))
            (&type (syntax-rules () ((_ val) (var-type in val))))
            (&min  (syntax-rules () ((_ val) (var-min in val))))
            (&max  (syntax-rules () ((_ val) (var-max in val)))))
         body ...
         out)))))

(define-syntax-rule (define-type-inferrer (name arg ...) body ...)
  (define-type-inferrer* (name succ arg ...) body ...))

(define-syntax-rule (define-predicate-inferrer (name arg ... true?) body ...)
  (define-type-inferrer* (name succ arg ...)
    (let ((true? (not (zero? succ))))
      body ...)))

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
(define-simple-predicate-inferrer bytevector? &bytevector)
(define-simple-predicate-inferrer bitvector? &bitvector)
(define-simple-predicate-inferrer keyword? &keyword)
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
  (define! result &vector (max (&min size) 0) (&max size)))

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
  (define! result &exact-integer (max (&min v) 0)
    (min (&max v) *max-vector-len*)))




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
  ((logior &number &false) -inf.0 +inf.0))




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

(define (restricted-comparison-ranges op type0 min0 max0 type1 min1 max1)
  (define (infer-integer-ranges)
    (match op
      ('< (values min0 (min max0 (1- max1)) (max (1+ min0) min1) max1))
      ('<= (values min0 (min max0 max1) (max min0 min1) max1))
      ('>= (values (max min0 min1) max0 min1 (min max0 max1)))
      ('> (values (max min0 (1+ min1)) max0 min1 (min (1- max0) max1)))))
  (define (infer-real-ranges)
    (match op
      ((or '< '<=) (values min0 (min max0 max1) (max min0 min1) max1))
      ((or '> '>=) (values (max min0 min1) max0 min1 (min max0 max1)))))
  (if (= (logior type0 type1) &exact-integer)
      (infer-integer-ranges)
      (infer-real-ranges)))

(define-syntax-rule (define-comparison-inferrer (op inverse))
  (define-predicate-inferrer (op a b true?)
    (when (zero? (logand (logior (&type a) (&type b)) (lognot &number)))
      (call-with-values
          (lambda ()
            (restricted-comparison-ranges (if true? 'op 'inverse)
                                          (&type a) (&min a) (&max a)
                                          (&type b) (&min b) (&max b)))
        (lambda (min0 max0 min1 max1)
          (restrict! a &real min0 max0)
          (restrict! b &real min1 max1))))))

(define-simple-type-checker (< &real &real))
(define-comparison-inferrer (< >=))

(define-simple-type-checker (<= &real &real))
(define-comparison-inferrer (<= >))

(define-simple-type-checker (>= &real &real))
(define-comparison-inferrer (>= <))

(define-simple-type-checker (> &real &real))
(define-comparison-inferrer (> <=))

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
        (min-b (&min b)) (max-b (&max b))
        ;; We only really get +inf.0 at runtime for flonums and
        ;; compnums.  If we have inferred that the arguments are not
        ;; flonums and not compnums, then the result of (* +inf.0 0) at
        ;; range inference time is 0 and not +nan.0.
        (nan-impossible? (not (logtest (logior (&type a) (&type b))
                                       (logior &flonum &complex)))))
    (define (nan* a b)
      (if (and (or (and (inf? a) (zero? b))
                   (and (zero? a) (inf? b)))
               nan-impossible?)
          0 
          (* a b)))
    (let ((-- (nan* min-a min-b))
          (-+ (nan* min-a max-b))
          (++ (nan* max-a max-b))
          (+- (nan* max-a min-b)))
      (let ((has-nan? (or (nan? --) (nan? -+) (nan? ++) (nan? +-))))
        (define-binary-result! a b result #t
                               (cond
                                ((eqv? a b) 0)
                                (has-nan? -inf.0)
                                (else (min -- -+ ++ +-)))
                               (if has-nan?
                                   +inf.0
                                   (max -- -+ ++ +-)))))))

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
              (let ((--- (if (inf? min-b) 0 (floor/ min-a min-b)))
                    (-+- (if (inf? max-b) 0 (floor/ min-a max-b)))
                    (++- (if (inf? max-b) 0 (floor/ max-a max-b)))
                    (+-- (if (inf? min-b) 0 (floor/ max-a min-b)))
                    (--+ (if (inf? min-b) 0 (ceiling/ min-a min-b)))
                    (-++ (if (inf? max-b) 0 (ceiling/ min-a max-b)))
                    (+++ (if (inf? max-b) 0 (ceiling/ max-a max-b)))
                    (+-+ (if (inf? min-b) 0 (ceiling/ max-a min-b))))
                (values (min (min --- -+- ++- +--)
                             (min --+ -++ +++ +-+))
                        (max (max --- -+- ++- +--)
                             (max --+ -++ +++ +-+))))))
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
      (define! result &false 0 0))
     ((zero? (logand (&type val) (lognot type)))
      (define! result &true 0 0))
     (else
      (define! result (logior &true &false) 0 0)))))
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
    (define! result &false 0 0))
   ((zero? (logand (&type val) (lognot (logior &exact-integer &fraction))))
    (define! result &true 0 0))
   (else
    (define! result (logior &true &false) 0 0))))

(define-simple-type-checker (inexact? &number))
(define-type-inferrer (inexact? val result)
  (restrict! val &number -inf.0 +inf.0)
  (cond
   ((zero? (logand (&type val) (logior &flonum &complex)))
    (define! result &false 0 0))
   ((zero? (logand (&type val) (logand &number
                                       (lognot (logior &flonum &complex)))))
    (define! result &true 0 0))
   (else
    (define! result (logior &true &false) 0 0))))

(define-simple-type-checker (inf? &real))
(define-type-inferrer (inf? val result)
  (restrict! val &real -inf.0 +inf.0)
  (cond
   ((or (zero? (logand (&type val) (logior &flonum &complex)))
        (and (not (inf? (&min val))) (not (inf? (&max val)))))
    (define! result &false 0 0))
   (else
    (define! result (logior &true &false) 0 0))))

(define-type-aliases inf? nan?)

(define-simple-type (even? &exact-integer)
  ((logior &true &false) 0 0))
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
    (if (and (negative? a) (negative? b))
        (min a b)
        0))
  (define (logand-max a b)
    (if (and (positive? a) (positive? b))
        (min a b)
        0))
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

(define-simple-type-checker (logtest &exact-integer &exact-integer))
(define-predicate-inferrer (logtest a b true?)
  (restrict! a &exact-integer -inf.0 +inf.0)
  (restrict! b &exact-integer -inf.0 +inf.0))

(define-simple-type-checker (logbit? (&exact-integer 0 +inf.0) &exact-integer))
(define-type-inferrer (logbit? a b result)
  (let ((a-min (&min a))
        (a-max (&max a))
        (b-min (&min b))
        (b-max (&max b)))
    (if (and (eqv? a-min a-max) (>= a-min 0) (not (inf? a-min))
             (eqv? b-min b-max) (>= b-min 0) (not (inf? b-min)))
        (let ((type (if (logbit? a-min b-min) &true &false)))
          (define! result type 0 0))
        (define! result (logior &true &false) 0 0))))

;; Flonums.
(define-simple-type-checker (sqrt &number))
(define-type-inferrer (sqrt x result)
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
  (let ((type (&type x)))
    (cond
     ((eqv? type (logand type &number))
      (restrict! x &real -inf.0 +inf.0)
      (define! result (logand type &real)
        (min (abs (&min x)) (abs (&max x)))
        (max (abs (&min x)) (abs (&max x)))))
     (else
      (define! result (logior (logand (&type x) (lognot &number))
                              (logand (&type x) &real))
        (max (&min x) 0)
        (max (abs (&min x)) (abs (&max x))))))))




;;;
;;; Characters.
;;;

(define-simple-type (char<? &char &char)
  ((logior &true &false) 0 0))
(define-type-aliases char<? char<=? char>=? char>?)

(define-simple-type-checker (integer->char (&exact-integer 0 #x10ffff)))
(define-type-inferrer (integer->char i result)
  (restrict! i &exact-integer 0 #x10ffff)
  (define! result &char (max (&min i) 0) (min (&max i) #x10ffff)))

(define-simple-type-checker (char->integer &char))
(define-type-inferrer (char->integer c result)
  (restrict! c &char 0 #x10ffff)
  (define! result &exact-integer (max (&min c) 0) (min (&max c) #x10ffff)))




;;;
;;; Type flow analysis: the meet (ahem) of the algorithm.
;;;

(define (successor-count cont)
  (match cont
    (($ $kargs _ _ ($ $continue k src exp))
     (match exp
       ((or ($ $branch) ($ $prompt)) 2)
       (_ 1)))
    (($ $kfun src meta self tail clause) (if clause 1 0))
    (($ $kclause arity body alt) (if alt 2 1))
    (($ $kreceive) 1)
    (($ $ktail) 0)))

(define (intset-pop set)
  (match (intset-next set)
    (#f (values set #f))
    (i (values (intset-remove set i) i))))

(define-syntax-rule (make-worklist-folder* seed ...)
  (lambda (f worklist seed ...)
    (let lp ((worklist worklist) (seed seed) ...)
      (call-with-values (lambda () (intset-pop worklist))
        (lambda (worklist i)
          (if i
              (call-with-values (lambda () (f i seed ...))
                (lambda (i* seed ...)
                  (let add ((i* i*) (worklist worklist))
                    (match i*
                      (() (lp worklist seed ...))
                      ((i . i*) (add i* (intset-add worklist i)))))))
              (values seed ...)))))))

(define worklist-fold*
  (case-lambda
    ((f worklist seed)
     ((make-worklist-folder* seed) f worklist seed))))

(define intmap-ensure
  (let* ((*absent* (list 'absent))
         (not-found (lambda (i) *absent*)))
    (lambda (map i ensure)
      (let ((val (intmap-ref map i not-found)))
        (if (eq? val *absent*)
            (let ((val (ensure i)))
              (values (intmap-add map i val) val))
            (values map val))))))

;; For best results, the labels in the function starting should be
;; topologically sorted (renumbered).  Otherwise the backward branch
;; detection mentioned in the module commentary will trigger for
;; ordinary forward branches.
(define (infer-types conts kfun)
  "Compute types for all variables bound in the function labelled
@var{kfun}, from @var{conts}.  Returns an intmap mapping labels to type
entries.

A type entry is a vector that describes the types of the values that
flow into and out of a labelled expressoin.  The first slot in the type
entry vector corresponds to the types that flow in, and the rest of the
slots correspond to the types that flow out.  Each element of the type
entry vector is an intmap mapping variable name to the variable's
inferred type.  An inferred type is a 3-vector of type, minimum, and
maximum, where type is a bitset as a fixnum."
  (define (get-entry typev label) (intmap-ref typev label))
  (define (entry-not-found label)
    (make-vector (1+ (successor-count (intmap-ref conts label))) #f))
  (define (ensure-entry typev label)
    (intmap-ensure typev label entry-not-found))

  (define (compute-initial-state)
    (let ((entry (entry-not-found kfun)))
      ;; Nothing flows in to the first label.
      (vector-set! entry 0 empty-intmap)
      (intmap-add empty-intmap kfun entry)))

  (define (adjoin-vars types vars entry)
    (match vars
      (() types)
      ((var . vars)
       (adjoin-vars (adjoin-var types var entry) vars entry))))

  (define (infer-primcall types succ name args result)
    (cond
     ((hashq-ref *type-inferrers* name)
      => (lambda (inferrer)
           ;; FIXME: remove the apply?
           ;; (pk 'primcall name args result)
           (apply inferrer types succ
                  (if result
                      (append args (list result))
                      args))))
     (result
      (adjoin-var types result all-types-entry))
     (else
      types)))

  (define (vector-replace vec idx val)
    (let ((vec (vector-copy vec)))
      (vector-set! vec idx val)
      vec))

  (define (update-out-types label typev types succ-idx)
    (let* ((entry (get-entry typev label))
           (old-types (vector-ref entry (1+ succ-idx))))
      (if (eq? types old-types)
          (values typev #f)
          (let ((entry (vector-replace entry (1+ succ-idx) types))
                (first? (not old-types)))
            (values (intmap-replace typev label entry) first?)))))

  (define (update-in-types label typev types saturate?)
    (let*-values (((typev entry) (ensure-entry typev label))
                  ((old-types) (vector-ref entry 0))
                  ;; TODO: If the label has only one predecessor, we can
                  ;; avoid the meet.
                  ((types) (if (not old-types)
                               types
                               (let ((meet (if saturate?
                                               type-entry-saturating-union
                                               type-entry-union)))
                                 (intmap-intersect old-types types meet)))))
      (if (eq? old-types types)
          (values typev #f)
          (let ((entry (vector-replace entry 0 types)))
            (values (intmap-replace typev label entry) #t)))))

  (define (propagate-types label typev succ-idx succ-label types)
    (let*-values
        (((typev first?) (update-out-types label typev types succ-idx))
         ((saturate?) (and (not first?) (<= succ-label label)))
         ((typev changed?) (update-in-types succ-label typev types saturate?)))
      (values (if changed? (list succ-label) '()) typev)))

  (define (visit-exp label typev k types exp)
    (define (propagate1 succ-label types)
      (propagate-types label typev 0 succ-label types))
    (define (propagate2 succ0-label types0 succ1-label types1)
      (let*-values (((changed0 typev)
                     (propagate-types label typev 0 succ0-label types0))
                    ((changed1 typev)
                     (propagate-types label typev 1 succ1-label types1)))
        (values (append changed0 changed1) typev)))
    ;; Each of these branches must propagate to its successors.
    (match exp
      (($ $branch kt ($ $values (arg)))
       ;; The "normal" continuation is the #f branch.
       (let ((kf-types (restrict-var types arg
                                     (make-type-entry (logior &false &nil)
                                                      0
                                                      0)))
             (kt-types (restrict-var types arg
                                     (make-type-entry
                                      (logand &all-types 
                                              (lognot (logior &false &nil)))
                                      -inf.0 +inf.0))))
         (propagate2 k kf-types kt kt-types)))
      (($ $branch kt ($ $primcall name args))
       ;; The "normal" continuation is the #f branch.
       (let ((kf-types (infer-primcall types 0 name args #f))
             (kt-types (infer-primcall types 1 name args #f)))
         (propagate2 k kf-types kt kt-types)))
      (($ $prompt escape? tag handler)
       ;; The "normal" continuation enters the prompt.
       (propagate2 k types handler types))
      (($ $primcall name args)
       (propagate1 k
                   (match (intmap-ref conts k)
                     (($ $kargs _ defs)
                      (infer-primcall types 0 name args
                                      (match defs ((var) var) (() #f))))
                     (_
                      ;; (pk 'warning-no-restrictions name)
                      types))))
      (($ $values args)
       (match (intmap-ref conts k)
         (($ $kargs _ defs)
          (let ((in types))
            (let lp ((defs defs) (args args) (out types))
              (match (cons defs args)
                ((() . ())
                 (propagate1 k out))
                (((def . defs) . (arg . args))
                 (lp defs args
                     (adjoin-var out def (var-type-entry in arg))))))))
         (_
          (propagate1 k types))))
      ((or ($ $call) ($ $callk))
       (propagate1 k types))
      (($ $rec names vars funs)
       (let ((proc-type (make-type-entry &procedure -inf.0 +inf.0)))
         (propagate1 k (adjoin-vars types vars proc-type))))
      (_
       (match (intmap-ref conts k)
         (($ $kargs (_) (var))
          (let ((entry (match exp
                         (($ $const val)
                          (constant-type val))
                         ((or ($ $prim) ($ $fun) ($ $closure))
                          ;; Could be more precise here.
                          (make-type-entry &procedure -inf.0 +inf.0)))))
            (propagate1 k (adjoin-var types var entry))))))))

  (define (visit-cont label typev)
    (let ((types (vector-ref (intmap-ref typev label) 0)))
      (define (propagate0)
        (values '() typev))
      (define (propagate1 succ-label types)
        (propagate-types label typev 0 succ-label types))
      (define (propagate2 succ0-label types0 succ1-label types1)
        (let*-values (((changed0 typev)
                       (propagate-types label typev 0 succ0-label types0))
                      ((changed1 typev)
                       (propagate-types label typev 1 succ1-label types1)))
          (values (append changed0 changed1) typev)))
      
      ;; Add types for new definitions, and restrict types of
      ;; existing variables due to side effects.
      (match (intmap-ref conts label)
        (($ $kargs names vars ($ $continue k src exp))
         (visit-exp label typev k types exp))
        (($ $kreceive arity k)
         (match (intmap-ref conts k)
           (($ $kargs names vars)
            (propagate1 k (adjoin-vars types vars all-types-entry)))))
        (($ $kfun src meta self tail clause)
         (if clause
             (propagate1 clause (adjoin-var types self all-types-entry))
             (propagate0)))
        (($ $kclause arity kbody kalt)
         (match (intmap-ref conts kbody)
           (($ $kargs _ defs)
            (let ((body-types (adjoin-vars types defs all-types-entry)))
              (if kalt
                  (propagate2 kbody body-types kalt types)
                  (propagate1 kbody body-types))))))
        (($ $ktail) (propagate0)))))

  (worklist-fold* visit-cont
                  (intset-add empty-intset kfun)
                  (compute-initial-state)))

(define (lookup-pre-type types label def)
  (let* ((entry (intmap-ref types label))
         (tentry (var-type-entry (vector-ref entry 0) def)))
    (values (type-entry-type tentry)
            (type-entry-min tentry)
            (type-entry-max tentry))))

(define (lookup-post-type types label def succ-idx)
  (let* ((entry (intmap-ref types label))
         (tentry (var-type-entry (vector-ref entry (1+ succ-idx)) def)))
    (values (type-entry-type tentry)
            (type-entry-min tentry)
            (type-entry-max tentry))))

(define (primcall-types-check? types label name args)
  (match (hashq-ref *type-checkers* name)
    (#f #f)
    (checker
     (let ((entry (intmap-ref types label)))
       (apply checker (vector-ref entry 0) args)))))
