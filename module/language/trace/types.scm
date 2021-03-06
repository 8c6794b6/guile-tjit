;;;; Types used in tracing JIT compile

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
;;; This module contains types used in tracing JIT compiler. Most of the types
;;; are borrowed from (language cps types) and (system base types), merely
;;; re-exporting them.
;;;
;;; Code:

(define-module (language trace types)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps types)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (language trace error)
  #:export ($return-address
            make-return-address
            return-address?
            return-address-ip

            $dynamic-link
            make-dynamic-link
            dynamic-link?
            dynamic-link-offset

            $constant
            make-constant
            constant?
            constant-value

            &fixnum &undefined &scm &any

            flonum? fraction? unbound? true? false? undefined? unbound?

            *scm-false* *scm-true* *scm-nil* *scm-null*
            *scm-unbound* *scm-unspecified* *scm-undefined* *scm-unbound*

            %tc2-int

            %tc3-imm24 %tc3-cons %tc3-struct

            %tc7-symbol %tc7-variable %tc7-vector %tc7-wvect %tc7-string
            %tc7-number %tc7-hashtable %tc7-pointer %tc7-fluid
            %tc7-stringbuf %tc7-keyword %tc7-program %tc7-bytevector
            %tc7-array %tc7-bitvector %tc7-port

            %tc8-char %tc8-flag

            %tc16-bignum %tc16-real %tc16-complex %tc16-fraction

            type-of pretty-type gen-type-checker)
  #:re-export (&flonum
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
               &pair
               &fluid
               &vector
               &box
               &struct
               &string
               &bytevector
               &bitvector
               &array
               &hash-table
               &f64
               &u64
               &s64
               %word-size))


;;;; Record types

;; Record type to represent return address in frame.
(define-record-type $return-address
  (make-return-address ip)
  return-address?
  (ip return-address-ip))

;; Record type to represent dynamic link in frame.
(define-record-type $dynamic-link
  (make-dynamic-link offset)
  dynamic-link?
  (offset dynamic-link-offset))

;; Record type to represent constant value in frame.
(define-record-type $constant
  (make-constant value)
  constant?
  (value constant-value))


;;;; Tags

(define-syntax define-tcx
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define name
         (@@ (system base types) name))
       ...))))

(define-tcx
  ;; Immediate values.
  %tc2-int %tc3-imm24

  %tc3-cons %tc3-int1 %tc3-int2

  %tc8-char %tc8-flag

  ;; Cell types.
  %tc3-struct %tc7-symbol %tc7-variable %tc7-vector %tc7-wvect %tc7-string
  %tc7-number %tc7-hashtable %tc7-pointer %tc7-fluid %tc7-stringbuf
  %tc7-dynamic-state %tc7-frame %tc7-keyword %tc7-program %tc7-vm-continuation
  %tc7-bytevector %tc7-weak-set %tc7-weak-table %tc7-array %tc7-bitvector
  %tc7-port %tc7-smob

  %tc16-bignum %tc16-real %tc16-complex %tc16-fraction)


;;;; Extra types

;; XXX: Any better number to use ...?
(define &undefined 0)

(define-syntax &fixnum
  (identifier-syntax &exact-integer))

(define-syntax &scm
  (identifier-syntax (@@ (language cps types) &all-types)))

(define-syntax &any
  (identifier-syntax most-positive-fixnum))


;;;; Type checker based on runtime values

(define-syntax-rule (inline-fixnum? val)
  (not (= 0 (logand 2 (object-address val)))))

(define-inlinable (flonum? val)
  (and (real? val) (inexact? val)))

(define-inlinable (undefined? x)
  (= (object-address x) #x904))

(define-inlinable (unbound? x)
  (= (object-address x) #xb04))

(define-inlinable (false? x)
  (not x))

(define-inlinable (true? x)
  (eq? x #t))

(define-inlinable (fraction? x)
  (let* ((ptr (scm->pointer x))
         (addr (pointer-address ptr)))
    (and (zero? (logand addr 6))
         (= (logand #xffff (pointer-address (dereference-pointer ptr)))
            %tc16-fraction))))


;;;; Scheme constants

(define-syntax-rule (define-inline name val)
  (define-syntax name (identifier-syntax val)))

(define-inline *scm-false* #x4)

(define-inline *scm-true* #x404)

(define-inline *scm-nil* #x104)

(define-inline *scm-unspecified* #x804)

(define-inline *scm-undefined* #x904)

(define-inline *scm-unbound* #xb04)

(define-inline *scm-null* #x304)

;; Property to hold the type of an object.
(define type-property (make-object-property))

(define (type-of obj)
  "Return the type of OBJ."
  (or (type-property obj)
      (let ((type (cond
                   ;; From (@ language cps types)
                   ((inline-fixnum? obj) &fixnum)
                   ((flonum? obj) &flonum)
                   ((fraction? obj) &fraction)
                   ((number? obj) &number)
                   ((char? obj) &char)
                   ((unspecified? obj) &unspecified)
                   ((false? obj) &false)
                   ((true? obj) &true)
                   ((null? obj) &null)
                   ((symbol? obj) &symbol)
                   ((keyword? obj) &keyword)
                   ((procedure? obj) &procedure)
                   ((pointer? obj) &pointer)
                   ((fluid? obj) &fluid)
                   ((pair? obj) &pair)
                   ((vector? obj) &vector)
                   ((variable? obj) &box)
                   ((struct? obj) &struct)
                   ((string? obj) &string)
                   ((bytevector? obj) &bytevector)
                   ((bitvector? obj) &bitvector)
                   ((array? obj) &array)
                   ((hash-table? obj) &hash-table)
                   ;; Not from (@ language cps types)
                   ((undefined? obj) &undefined)
                   (else &scm))))
        (set! (type-property obj) type)
        type)))

(define (dump-debug arg-types id hint n t)
  (let ((f (lambda (ts)
             (map (lambda (nt)
                    (cons (car nt) (pretty-type (cdr nt))))
                  (sort ts (lambda (a b)
                             (< (car a) (car b))))))))
    (debug 2 ";;; trace ~a: types=~a~%" id (f arg-types))
    (debug 2 ";;; trace ~a: hint=~a~%" id
           (cond
            ((vector? hint)
             (do ((v (make-vector (vector-length hint)))
                  (i (- (vector-length hint) 1) (- i 1)))
                 ((< i 0) v)
               (vector-set! v i (scm->pointer (vector-ref hint i)))))
            ((pair? hint) (f hint))
            (else hint)))
    (debug 2 ";;; trace ~a: local ~a expect ~a, got ~a from ~a~%"
           id n
           (pretty-type t)
           (pretty-type
            (if (vector? hint)
                (and (<= 0 n (- (vector-length hint) 1))
                     (type-of (vector-ref hint n)))
                (assq-ref hint n)))
           (if (vector? hint) 'vector 'list))))

(define (gen-type-checker arg-types id)
  "Returns a procedure for checking types.

Takes assoc list ARG-TYPES, with its keys being local index and values being
type value. Returns a procedure taking an argument HINT.  HINT could be a vector
containing stack elements, or an assoc list of index and type pairs. The
returned procedure will return true if all of the types in ARG-TYPES matched
with HINT, otherwise return false."
  (define ignored (list #f &scm &u64 &f64 &s64 &any))
  (define types-to-check
    (let lp ((arg-types arg-types) (acc '()))
      (match arg-types
        ((arg-type . arg-types)
         (lp arg-types (if (memq (cdr arg-type) ignored)
                           acc
                           (cons arg-type acc))))
        (_ acc))))
  (lambda (hint)
    (let ((checker (cond
                    ((vector? hint)
                     (lambda (n t)
                       (eq? t (type-of (vector-ref hint n)))))
                    ((null? hint)
                     (lambda (n t)
                       #t))
                    ((pair? hint)
                     (lambda (n t)
                       (let ((inferred (assq-ref hint n)))
                         (or (memq inferred ignored)
                             (eq? t inferred)
                             (constant? inferred)
                             (and (pair? inferred)
                                  (eq? 'copy (car inferred)))))))
                    (else
                     (failure 'type-checker "unknown hint ~a" hint)))))
      (let lp ((types types-to-check))
        (match types
          (((n . t) . types) (and (checker n t) (lp types)))
          (_ #t))))))


;;;; Auxiliary

(define (pretty-type type)
  "Show string representation of TYPE."
  (cond
   ;; From (@ language cps types)
   ((eq? type &scm) "scm")
   ((eq? type &fixnum) (blue "fixn"))
   ((eq? type &flonum) (magenta "flon"))
   ((eq? type &fraction) (yellow "frac"))
   ((eq? type &char) (blue "char"))
   ((eq? type &unspecified) (green "uspc"))
   ((eq? type &unbound) (green "ubnd"))
   ((eq? type &false) (green "fals"))
   ((eq? type &true) (green "true"))
   ((eq? type &nil) (green "nil"))
   ((eq? type &null) (green "null"))
   ((eq? type &symbol) (blue "symb"))
   ((eq? type &keyword) (blue "keyw"))
   ((eq? type &procedure) (red "proc"))
   ((eq? type &pointer) (yellow "ptr"))
   ((eq? type &fluid) (yellow "fld"))
   ((eq? type &pair) (yellow "pair"))
   ((eq? type &vector) (yellow "vect"))
   ((eq? type &box) (yellow "box"))
   ((eq? type &struct) (yellow "stru"))
   ((eq? type &string) (yellow "stri"))
   ((eq? type &bytevector) (yellow "bytv"))
   ((eq? type &bitvector) (yellow "bitv"))
   ((eq? type &array) (yellow "arry"))
   ((eq? type &hash-table) (yellow "htbl"))
   ((eq? type &f64) "f64")
   ((eq? type &u64) "u64")
   ((eq? type &s64) "s64")
   ;; Not from (@ language cps types)
   ((eq? type &undefined) (green "udef"))
   ((eq? type &any) "any")
   ((dynamic-link? type)
    (let ((diff (number->string (dynamic-link-offset type))))
      (string-append "d:" (cyan diff))))
   ((return-address? type)
    (let* ((addr (return-address-ip type))
           (hex-ip (number->string addr 16)))
      (string-append "r:" (cyan hex-ip))))
   ((constant? type)
    (string-append "c:" (cyan (number->string (constant-value type)))))
   (else type)))
