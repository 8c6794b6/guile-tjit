;;;; Types used in tracing JIT compile

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
;;; This module contains types used in tracing JIT compiler. Most of the types
;;; are borrowed from (language cps types) and (system base types), merely
;;; re-exporting them.
;;;
;;; Code:

(define-module (system vm native tjit types)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps types)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module ((system base types) #:select (%word-size))
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:export ($return-address
            make-return-address
            return-address?
            return-address-ip

            $dynamic-link
            make-dynamic-link
            dynamic-link?
            dynamic-link-offset

            &fixnum
            &undefined
            &scm
            &any

            flonum?
            fraction?
            unbound?
            true?
            false?
            undefined?
            eof?
            unbound?
            type-of
            pretty-type
            *unbound*
            flag->type

            %tc2-int
            %tc3-imm24
            %tc3-cons
            %tc8-char
            %tc3-struct
            %tc7-symbol
            %tc7-variable
            %tc7-vector
            %tc7-wvect
            %tc7-string
            %tc7-number
            %tc7-hashtable
            %tc7-pointer
            %tc7-fluid
            %tc7-stringbuf
            %tc7-keyword
            %tc7-program
            %tc7-bytevector
            %tc7-array
            %tc7-bitvector
            %tc7-port
            %tc16-real
            %tc16-fraction

            *ti-procedures*
            infer-type
            gen-type-checker
            type->stack-element-type)
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

;;;
;;; Record types
;;;

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


;;;
;;; Tags
;;;

(define-syntax define-tcx
  (syntax-rules ()
    ((_ name ...)
     (begin
       (define name
         (@@ (system base types) name))
       ...))))

(define-tcx
  %tc2-int
  %tc3-imm24
  %tc3-cons
  %tc8-char
  %tc3-struct
  %tc7-symbol
  %tc7-variable
  %tc7-vector
  %tc7-wvect
  %tc7-string
  %tc7-number
  %tc7-hashtable
  %tc7-pointer
  %tc7-fluid
  %tc7-stringbuf
  %tc7-keyword
  %tc7-program
  %tc7-bytevector
  %tc7-array
  %tc7-bitvector
  %tc7-port
  %tc16-real
  %tc16-fraction)

;;;
;;; Exported hash table
;;;

(define *ti-procedures*
  (make-hash-table 255))


;;;
;;; Lookup procedure
;;;

(define (infer-type env op ip dl locals)
  (let lp ((procs (hashq-ref *ti-procedures* (car op))))
    (match procs
      (((test . work) . procs)
       (if (test op locals)
           (apply work env ip dl locals (cdr op))
           (lp procs)))
      (_ (values)))))

;;;
;;; Extra types
;;;

;; XXX: Any better number to use ...?
(define &undefined 0)

(define-syntax &fixnum
  (identifier-syntax &exact-integer))

(define-syntax &scm
  (identifier-syntax (@@ (language cps types) &all-types)))

(define-syntax &any
  (identifier-syntax most-positive-fixnum))


;;;
;;; Type checker based on runtime values
;;;

(define-syntax-rule (inline-fixnum? val)
  (not (= 0 (logand 2 (object-address val)))))

(define (flonum? val)
  (and (real? val) (inexact? val)))

(define (undefined? x)
  (= (object-address x) #x904))

(define (eof? x)
  (= (object-address x) #xa04))

(define (unbound? x)
  (= (object-address x) #xb04))

(define (false? x)
  (not x))

(define (true? x)
  (eq? x #t))

(define (fraction? x)
  (let* ((ptr (scm->pointer x))
         (addr (pointer-address ptr)))
    (and (zero? (logand addr 6))
         (= (logand #xffff (pointer-address (dereference-pointer ptr)))
            %tc16-fraction))))

(define *unbound*
  (pointer->scm (make-pointer #xb04)))

;;;
;;; Auxiliary
;;;

(define (gen-type-checker arg-types id)
  "Returns a procedure for checking types.

Takes assoc list ARG-TYPES, with its keys being local index and values being
type values. Returns a procedure taking two argument INFERRED-TYPES and LOCALS.
LOCALS is a vector containing stack elements. The returned procedure will return
true if all of the types in ARG-TYPES matched with LOCALS, otherwise return
false."
  (lambda (hint)
    (let* ((ignored-types (list #f &scm &u64 &f64 &s64 &any))
           (checker
            (cond
             ((vector? hint)
              (lambda (n t)
                (let ((tr (and (<= 0 n (- (vector-length hint) 1))
                               (type-of (vector-ref hint n)))))
                  (eq? t tr))))
             ((null? hint)
              (lambda (n t)
                #t))
             ((pair? hint)
              (lambda (n t)
                (let ((ti (assq-ref hint n)))
                  (or (not ti)
                      (memq ti ignored-types)
                      (eq? t ti)
                      (and (pair? ti) (eq? 'copy (car ti)))))))
             (else
              (failure 'type-checker "unknown hint ~a" hint))))
           (dump-debug
            (lambda (n t)
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
                           (vector-set! v i (scm->pointer
                                             (vector-ref hint i)))))
                        ((pair? hint)
                         (f hint))
                        (else hint)))
                (debug 2 ";;; trace ~a: local ~a expect ~a, got ~a from ~a~%"
                       id n
                       (pretty-type t)
                       (pretty-type
                        (if (vector? hint)
                            (and (<= 0 n (- (vector-length hint) 1))
                                 (type-of (vector-ref hint n)))
                            (assq-ref hint n)))
                       (if (vector? hint) 'vector 'list))))))
      (let lp ((types arg-types))
        (match types
          (((n . t) . types)
           (if (or (memq t ignored-types)
                   (checker n t))
               (lp types)
               (begin
                 (dump-debug n t)
                 #f)))
          (() #t))))))

(define (type->stack-element-type type)
  (cond
   ((eq? type &f64) 'f64)
   ((eq? type &u64) 'u64)
   ((eq? type &s64) 's64)
   (else 'scm)))

(define (type-of obj)
  (cond
   ;; Non-scm object.
   ((zero? (object-address obj)) #f)
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
   (else &scm)))

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
      (string-append "dl:" (cyan diff))))
   ((return-address? type)
    (let* ((addr (pointer-address (return-address-ip type)))
           (hex-ip (number->string addr 16)))
      (string-append "ra:" (cyan hex-ip))))
   (else type)))

(define (flag->type flag)
  (cond
   ((eq? flag 'scm) &scm)
   ((eq? flag 'fixnum) &fixnum)
   ((eq? flag 'flonum) &flonum)
   ((eq? flag 'fraction) &fraction)
   ((eq? flag 'char) &char)
   ((eq? flag 'procedure) &procedure)
   ((eq? flag 'pair) &pair)
   ((eq? flag 'vector) &vector)
   ((eq? flag 'struct) &struct)
   ((eq? flag 'string) &string)
   ((eq? flag 'box) &box)
   ((eq? flag 'bytevector) &bytevector)
   ((eq? flag 'array) &array)
   (else
    (failure 'flag->type "~s" flag))))
