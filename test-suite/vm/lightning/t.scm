;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA

;;; Commentary:

;;; Tests for vm-lightning

;;; Code:

(use-modules (srfi srfi-9)
             (srfi srfi-64)
             (system vm program)
             (system vm lightning)
             (system vm vm))

(define-syntax-rule (with-lightning proc . args)
  (dynamic-wind
    (lambda () (set-vm-engine! 'lightning))
    (lambda () (call-with-vm proc . args))
    (lambda () (set-vm-engine! 'regular))))

;; Define and run a procedure with guile vm and lightning, and compare the
;; results with test-equal.
(define-syntax define-test
  (syntax-rules ()
    ((_ (name . vars) args body ...)
     (begin
       (define (name . vars)
         body ...)
       (format #t ";;; Test: ~a ...~%" '(name . vars))
       (test-equal (symbol->string (procedure-name name))
         (name . args)
         ;; (call-lightning name . args)
         (with-lightning name . args))))))

(lightning-verbosity 0)

;;;
;;; VM operation
;;;

(test-begin "vm-lightning-test")


;;; Call and return

(define (callee x)
  (+ 1 x))

(define-test (t-call n) (98)
  (+ 1 (callee n)))

(define-test (t-call-simple-prim x y) ('(a b c) '(d e f g h))
  (+ (length x) (length y)))

(define-test (t-call-rest-prim x y) ('(a b c) '(d e f g h))
  (append x y))

(define-test (t-call-opt-rest-prim-1 a) (#\a)
  (char=? a))

(define-test (t-call-opt-rest-prim-2a a b) (#\a #\a)
  (char=? a b))

(define-test (t-call-opt-rest-prim-2b a b) (#\a #\b)
  (char=? a b))

(define-test (t-call-opt-rest-prim-3a a b c) (#\a #\a #\a)
  (char=? a b c))

(define-test (t-call-opt-rest-prim-3b a b c) (#\a #\a #\b)
  (char=? a b c))

(define-test (t-call-opt-rest-prim-4 a b c d) (#\a #\a #\a #\a)
  (char=? a b c d))

(define-test (t-call-string->list str) ("foo-bar-buzz")
  (string->list str))

(define-test (t-call-string-append str1 str2) ("foo" "bar")
  (string-append str1 str2))

(define-test (t-call-make-string n fill) (113 #\a)
  (make-string n fill))

(define-test (t-call-arg0 f x) ((lambda (a) (+ a 100)) 23)
  (f x))

(define (add-one x)
  (+ x 1))

(define (mul-two x)
  (* x 2))

(define-test (t-my-map f xs) (add-one '(1 2 3))
  (let lp ((xs xs))
    (if (null? xs)
        '()
        (cons (f (car xs)) (t-my-map f (cdr xs))))))

(define (my-map f xs)
  (let lp ((xs xs))
    (if (null? xs)
        '()
        (cons (f (car xs)) (my-map f (cdr xs))))))

(define-test (call-my-map xs) ('(1 2 3 4 5))
  (my-map add-one xs))

(define-test (call-my-map-twice xs) ('(1 2 3 4 5))
  (append (my-map add-one xs)
          (my-map mul-two xs)))

(define (make-applicable-struct)
  (make-procedure-with-setter
   (lambda (obj)
     (+ obj 1))
   (lambda (obj val)
     (list obj val))))

(define applicable-struct (make-applicable-struct))

;; XXX: Getting segfault.
;; (define-test (call-applicable-struct n) (99)
;;   (applicable-struct n))

(define-test (call-branched-a x) (#f)
  ((or x *) 20 30))

(define-test (call-branched-b x) (+)
  ((or x *) 20 30))

(define (call-branched x)
  ((or x *) 20 30))

(define-test (call-branched-c x) (#f)
  (call-branched x))

(define-test (call-branched-d x) (+)
  (call-branched x))

(define (call-branched-prim x)
  ((if (even? x) + *) 20 30))

(define-test (call-branched-prim-a x) (1)
  (call-branched-prim x))

(define-test (call-branched-prim-b x) (2)
  (call-branched-prim x))

(define (sum-and-product x y)
  (values (+ x y) (* x y)))

(define-test (call-sum-and-product x y) (12 34)
  (call-with-values
      (lambda ()
        (sum-and-product x y))
    (lambda (a b)
      (cons a b))))

(define (two-values x)
  (if (null? x)
      (values #f x)
      (car x)))

(define-test (call-two-values-true x) ('())
  (if (two-values x) 'true-branch 'false-branch))

(define-test (call-two-values-false x) ('(1 2 3))
  (if (two-values x) 'true-branch 'false-branch))


;;; Specialized call stubs

(define-test (return-builtin-apply) ()
  apply)

(define-test (t-apply-a f a b rest) (+ 1 2 '(3 4 5))
  (+ 100 (apply f a b rest)))

(define-test (t-apply-b f a b rest) (+ 1 2 '())
  (+ 100 (apply f a b rest)))

(define-test (t-apply-c f rest) (+ '(1 2 3 4 5))
  (+ 100 (apply f rest)))

(define-test (t-apply-tail-a f a b rest) (+ 1 2 '(3 4 5))
  (apply f a b rest))

(define-test (t-apply-tail-b f a b rest) (+ 1 2 '())
  (apply f a b rest))

(define-test (t-apply-tail-c f rest) (+ '(1 2 3 4 5))
  (apply f rest))

(define-test (return-builtin-values) ()
  values)

(define-test (return-builtin-abort) ()
  abort-to-prompt)

(define-test (return-builtin-call-with-values) ()
  call-with-values)

(define-test (return-builtin-call-with-current-continuation) ()
  call-with-current-continuation)

;;; Function prologues

(define-test (t-identity x) (12345)
  x)

(define-test (t-bind-rest x . rest) ('foo 'bar 'buzz)
  (cons x rest))

(define (args-rest a b . rest)
  (cons (+ a b) rest))

(define-test (t-bind-rest-without-rest a b) (1 2)
  (args-rest a b))

(define-test (t-bind-rest-with-rest a b c d e) (1 2 3 4 5)
  (args-rest a b c d e))

(define-test (t-call-rest-0 x . rest) ('foo + *)
  ((car rest) 2 3))

(define-test (t-call-rest-1 x . rest) ('foo + *)
  ((cadr rest) 2 3))

(define* (sum-optional x #:optional (y 0) (z 0))
  (+ x y z))

(test-equal "t-sum-optional-one"
            (sum-optional 1)
            (with-lightning sum-optional 1))

(test-equal "t-sum-optional-two"
            (sum-optional 1 2)
            (with-lightning sum-optional 1 2))

(test-equal "t-sum-optional-two"
            (sum-optional 1 2 3)
            (with-lightning sum-optional 1 2 3))

(define br-nargs-1
  (case-lambda
    (() 100)
    ((x) (cons x 100))))

(test-equal "t-br-if-nargs-1-0"
            (br-nargs-1)
            (with-lightning br-nargs-1))

(test-equal "t-br-if-nargs-1-1"
            (br-nargs-1 99)
            (with-lightning br-nargs-1 99))

(define* (call-bind-kwargs-01 #:key (x 100))
  x)

(define-test (t-bind-kwargs-01-default) ()
  (call-bind-kwargs-01))

(define-test (t-bind-kwargs-01-with-x x) (123)
  (call-bind-kwargs-01 #:x x))

(define* (call-bind-kwargs-02 a #:key (b 2) (c 3))
  (+ (* a b) c))

(define-test (t-bind-kwargs-02-default x) (10)
  (call-bind-kwargs-02 x))

(define-test (t-bind-kwargs-02-with-b x) (10)
  (call-bind-kwargs-02 x #:b x))

(define-test (t-bind-kwargs-02-with-c x) (10)
  (call-bind-kwargs-02 x #:c x))

(define-test (t-bind-kwargs-02-with-b-and-c x) (10)
  (call-bind-kwargs-02 x #:b x #:c x))

(define* (call-bind-kwargs-03 a #:key (b 2) . rest)
  (+ (* a (length rest)) b))

(define-test (t-bind-kwargs-03 x) (100)
  (call-bind-kwargs-03 x 'foo 'bar 'buzz))

(define-test (t-bind-kwargs-03-with-b x) (100)
  (call-bind-kwargs-03 x #:b 23 'foo 'bar 'buzz))


;;; Branching instructions

(define-test (t-if-true x) (#t)
  (if x 100 200))

(define-test (t-if-true-invert x) (#t)
  (if (not x) 100 200))

(define-test (t-if-null x) ('(1 2 3))
  (null? x))

(define-test (t-if-null-non-list x) (#(1 2 3))
  (null? x))

(define-test (t-if-pair x) ('(1 2 3))
  (pair? x))

(define-test (t-if-pair-immediate x) (123456)
  (pair? x))

(define-test (t-if-pair-non-immediate x) ((lambda () 'no))
  (pair? x))

(define-test (t-if-struct x) ((make-object-property))
  (struct? x))

(define-test (t-if-struct-immediate x) (1)
  (struct? x))

(define-test (t-if-struct-heap-object x) ((vector 1 2 3))
  (struct? x))

(define-test (t-if-char x) (#\a)
  (char? x))

(define-test (t-if-char-immediate x) (103)
  (char? x))

(define-test (t-if-char-heap-object x) ((vector 1 2 3))
  (char? x))

(define v01 (vector 1 2 3))

(define-test (t-if-tc7-vector x) (v01)
  (vector? x))

(define-test (t-if-tc7-vector-immediate x) (123)
  (vector? x))

(define-test (t-if-tc7-vector-heap-object x) ('(1 2 3))
  (vector? x))

(define-test (t-if-tc7-string x) ("foo bar buzz")
  (string? x))

(define-test (t-if-tc7-string-immediate x) (123)
  (string? x))

(define-test (t-if-tc7-string-heap-object x) ('(1 2 3))
  (string? x))

(define-test (t-eqv?-1 a b) (2.13 2.13)
  (eqv? a b))

(define-test (t-eqv?-2 a b) (2.14 2.13)
  (eqv? a b))

(define-test (t-equal?-1 a b) ('(1 2 3) '(1 2 3))
  (equal? a b))

(define-test (t-equal?-2 a b) ('(1 2 3) '(1 2 3.3))
  (equal? a b))

(define-test (t-if-zero x) (0)
  (zero? x))


;;; Lexical binding instructions

(define-test (t-box-set! n) (20)
  (let ((result 0))
    (if (< n 100)
        (set! result n)
        (set! result 0))
    result))

(define (closure01 x)
  (lambda (y)
    (+ x y)))

(test-equal "closure01-program-code"
  (program-code (closure01 100))
  (program-code (call-lightning closure01 100)))

(let ((closure01-vm (closure01 23))
      (closure01-lightning (call-lightning closure01 23)))

  (test-equal "closure01-call-01"
    (closure01-vm 100)
    (call-lightning closure01-vm 100))

  (test-equal "closure01-call-02"
    (closure01-vm 100)
    (call-lightning closure01-lightning 100))

  (test-equal "closure01-call-03"
    (closure01-vm 100)
    (closure01-lightning 100)))

(define-test (closure02 x) (23)
  ((closure01 x) 100))

(define (closure03 n)
  (lambda ()
    (+ n 1)))

(define-test (call-closure03 x) (23)
  ((closure03 (+ ((closure03 x))
                 ((closure03 10))))))

(define-test (call-closure03-b x) (12345)
  (cons ((closure03 (+ x 100))) 23))

(define (closure04 n)
  (lambda ()
    (let lp ((n n) (acc 0))
      (if (< n 0)
          acc
          (lp (- n 1) (+ acc n))))))

(define-test (call-closure04 n) (100)
  ((closure04 n)))

(define-test (call-closure05 x) (10)
  (let ((l (list (closure03 x)
                 (call-closure03 x)
                 (call-closure03-b x))))
    ((car l))))

(define (addk x y k)
  (k (+ x y)))

(define (mulk x y k)
  (k (* x y)))

(define-test (muladdk x y z k) (3 4 5 (lambda (a) a))
  (mulk x y
        (lambda (xy)
          (addk xy z k))))

(define-test (pythk2 x y k) (3 4 (lambda (a) a))
  (mulk x x
        (lambda (x2)
          (mulk y y
                (lambda (y2)
                  (addk x2 y2 k))))))

;;; Immediates and statically allocated non-immediates

(define-test (t-make-short-immediate) () ;; no args.
  100)

(define-test (t-long-long-immediate) ()
  -12345)

(define-test (t-non-immediate) ()
  "non-immediate string.")

(define-test (t-static-ref) ()
  0.5)

;;; Mutable top-level bindings

(define-test (t-current-module) ()
  (current-module))

(define a-toplevel-ref 123)

(define-test (t-toplevel-box x) (321)
  (+ x a-toplevel-ref))

(define (add-toplevel-ref x)
  (+ x a-toplevel-ref))

;; Call a function with toplevel-box twice. In first call, variable need to be
;; resolved.  In second call, variable should be already stored. Writing test
;; without using define-test macro. The macro runs bytecode procedure first, and
;; variable get cached when bytecode procedure runs.
(test-equal "resolve-toplevel-var"
  (call-lightning add-toplevel-ref 100)
  (call-lightning add-toplevel-ref 100))

(define-test (t-module-box) ()
  length)

(define (get-from-module-box)
  length)

(test-equal "resolve-module-box-var"
  'length
  (procedure-name (call-lightning get-from-module-box)))

;;; The dynamic environment

(define f01 (make-fluid 100))
(fluid-set! f01 12345)

(define f02 (make-fluid 123))

(define-test (t-fluid-ref fluid) (f01)
  (fluid-ref fluid))

(define-test (t-fluid-ref-undefine fluid) (f02)
  (fluid-ref fluid))

(define-test (t-fluid-push-pop-a) ()
  (with-fluid* f01 456
               (lambda ()
                 (fluid-ref f01))))

(define-test (t-fluid-push-pop-b) ()
  (let ((a (with-fluid* f01 456
                        (lambda ()
                          (fluid-ref f01)))))
    (list (fluid-ref f01) a)))

(test-skip 1)
(define-test (t-wind-unwind x) (100)
  (let ((ret '()))
    (dynamic-wind
      (lambda () (set! ret (cons 'pre ret)))
      (lambda () (set! ret (cons x ret)))
      (lambda () (set! ret (cons 'post ret))))
    ret))

;;; String, symbols, and keywords

(define-test (t-string-length str) ("foo-bar-buzz")
  (string-length str))

(define-test (t-string-ref k) (8)
  (string-ref "foo-bar-buzz" k))

(define-test (t-string->number-fixnum n) ("12345")
  (string->number n))

(define-test (t-string->number-flonum n) ("1.2345")
  (string->number n))

(define-test (t-string->number-gmp n) ("123456789012345678901234567890")
  (string->number n))

(define-test (t-string->symbol str) ("foo")
  (string->symbol str))

(define-test (t-symbol->keyword sym) ('foo)
  (symbol->keyword sym))


;;; Pairs

(define-test (t-car x) ('(foo bar buzz))
  (car x))

(define-test (t-cdr x) ('(foo bar buzz))
  (cdr x))

(define-test (t-cons x y) (100 200)
  (cons x y))

(define-test (t-set-car! lst x) ('(1 2 3) 123)
  (set-car! lst x)
  lst)

(define-test (t-set-cdr! lst x) ('(1 2 3) '(998 999 1000))
  (set-cdr! lst x)
  lst)


;;; Numeric operations

(define-test (t-add1 x) (99)
  (+ 1 x))

(define-test (t-add-fx-fx x y) (27 73)
  (+ x y))

(define-test (t-add-fx-fl x y) (3 0.456)
  (+ x y))

(define-test (t-add-fl-fx x y) (0.345 4)
  (+ x y))

(define-test (t-add-fl-fl x y) (0.125 0.775)
  (+ x y))

(define-test (t-add-fx-gmp x y)
  (9999999999999999999999999999999 999999999999999999999999999999)
  (+ x y))

(define-test (t-add-overflow x y) ((- (expt 2 61) 1) 100)
  (+ x y))

(define-test (t-sub x y) (127 27)
  (- x y))

(define-test (t-mul x y) (123 321)
  (* x y))

(define-test (t-mul-fx-fl x y) (10 1.23)
  (* x y))

(define-test (t-mul-fl-fx x y) (1.23 10)
  (* x y))

(define-test (t-mul-fl-fl x y) (1.23 0.12)
  (* x y))

(define-test (t-mul-gmp x y) (1.23 9999999999999999999999999999)
  (* x y))

(define-test (t-div x y) (32 8)
  (/ x y))

(define-test (t-div-fx-fl x y) (10 1.23)
  (/ x y))

(define-test (t-div-fl-fx x y) (1.23 10)
  (/ x y))

(define-test (t-div-fl-fl x y) (1.23 0.12)
  (/ x y))

(define-test (t-div-gmp x y) (1.23 9999999999999999999999999999)
  (/ x y))

(define-test (t-make-vector len fill) (16 'foo)
  (make-vector len fill))

(define-test (t-make-vector-immediate fill) ('foo)
  (make-vector 10 fill))

(define-test (t-vector-length v) (#(1 2 3 4 5))
  (vector-length v))

(define-test (t-vector-ref v idx) (#(1 2 3 4 5) 3)
  (vector-ref v idx))

(define-test (t-vector-ref-min v idx) (#(1 2 3 4 5) 0)
  (vector-ref v idx))

(define-test (t-vector-ref-max v idx) (#(1 2 3 4 5) 4)
  (vector-ref v idx))

(define-test (t-vector-ref-immediate v) (#(1 2 3 4 5))
  (vector-ref v 3))

(define-test (t-vector-ref-immediate-min v) (#(1 2 3 4 5))
  (vector-ref v 0))

(define-test (t-vector-ref-immediate-max v) (#(1 2 3 4 5))
  (vector-ref v 4))

(define-test (t-vector-set! v idx) (#(1 2 3 4 5) 3)
  (vector-set! v idx 999)
  v)

(define-test (t-vector-set!-min v idx) (#(1 2 3 4 5) 0)
  (vector-set! v idx 999)
  v)

(define-test (t-vector-set!-max v idx) (#(1 2 3 4 5) 4)
  (vector-set! v idx 999)
  v)

(define-test (t-vector-set!-immediate v) (#(1 2 3 4 5))
  (vector-set! v 3 999)
  v)

(define-test (t-vector-set!-immediate-min v) (#(1 2 3 4 5))
  (vector-set! v 0 999)
  v)

(define-test (t-vector-set!-immediate-max v) (#(1 2 3 4 5))
  (vector-set! v 4 999)
  v)


;;; Structs and GOOPS

(define-record-type <r01>
  (make-r01)
  r01?)

(define-test (t-allocate-struct) ()
  (make-r01))

(define-record-type <r02>
  (make-r02 x y)
  r02?
  (x r01-x set-r02-x!)
  (y r01-y set-r02-y!))

(define r02-a (make-r02 123 456))

(define-test (t-struct-set-immediate x y) (1 2)
  (make-r02 x y))

(define-test (t-struct-ref-immediate r) (r02-a)
  (struct-ref r 0))

(define-test (t-struct-ref x) (<r02>)
  (record-type-name x))

;;; Arrays, packed uniform arrays, and bytevectors

;;;
;;; Simple procedures
;;;

(define-test (t-lp n) (#e1e7)
  (let lp ((n n))
    (if (< 0 n) (lp (- n 1)) n)))

(define-test (sum-tail-call x) (1000)
  (let lp ((n x) (acc 0))
    (if (< n 0)
        acc
        (lp (- n 1) (+ acc n)))))

(define-test (sum-non-tail-call x) (1000)
  (let lp ((n x))
    (if (< n 0)
        0
        (+ n (lp (- n 1))))))

(define-test (sum-toplevel n acc) (1000 0)
  (if (= n 0)
      acc
      (sum-toplevel (- n 1) (+ n acc))))

(define-test (sum-cps n k) (10 (lambda (a) a))
  (if (< n 0)
      (k 0)
      (sum-cps (- n 1)
               (lambda (s)
                 (k (+ s n))))))

;;; XXX: CPS style sum soon reaches to stack limit, running with n=1000
;;; causing "Unwind-only `stack-overflow'.
;; (test-skip 1)
;; (define-test (sum-cps-1000 n k) (1000 (lambda (a) a))
;;   (if (< n 0)
;;       (k 0)
;;       (sum-cps (- n 1)
;;                (lambda (s)
;;                  (k (+ s n))))))

(define-test (fib1 n) (30)
  (let lp ((n n))
    (if (< n 2)
        n
        (+ (lp (- n 1))
           (lp (- n 2))))))

(define-test (fib2 n) (30)
  (if (< n 2)
      n
      (+ (fib2 (- n 1))
         (fib2 (- n 2)))))

(define-test (call-map1 xs) ('(1 2 3))
  (map (lambda (x) (+ x 1)) xs))

(define-test (call-map2 xs ys) ('(1 2 3) '(10 20 30))
  (map (lambda (x y) (+ x y)) xs ys))

(define-test (call-map3 xs ys zs) ('(1 2 3) '(10 20 30) '(100 200 300))
  (map + xs ys zs))

(test-skip 1)
(define-test (call-map3-lambda xs ys zs) ('(1 2 3) '(10 20 30) '(100 200 300))
  (map (lambda (x y z) (+ x y z)) xs ys zs))

(define-test (nqueens n) (8)
  (define (one-to n)
    (let loop ((i n) (l '()))
      (if (= i 0) l (loop (- i 1) (cons i l)))))
  (define (ok? row dist placed)
    (if (null? placed)
        #t
        (and (not (= (car placed) (+ row dist)))
             (not (= (car placed) (- row dist)))
             (ok? row (+ dist 1) (cdr placed)))))
  (define (try-it x y z)
    (if (null? x)
        (if (null? y) 1 0)
        (+ (if (ok? (car x) 1 z)
               (try-it (append (cdr x) y) '() (cons (car x) z))
               0)
           (try-it (cdr x) (cons (car x) y) z))))
  (try-it (one-to n) '() '()))

(define-test (tak x y z) (18 12 6)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define-test (sieve n) (500)
  (let ((root (round (sqrt n)))
        (a (make-vector n #f)))
    (define (cross-out t to dt)
      (cond ((> t to) 0)
            (else
             (vector-set! a t #t)
             (cross-out (+ t dt) to dt))))
    (define (iter i result)
      (cond ((>= i n)
             (reverse result))
            (else
             (when (< i root)
               (cross-out (* i i) (- n 1) (+ i i)))
             (iter (+ i 2) (if (vector-ref a i)
                               result
                               (cons i result))))))
    (iter 3 (list 2))))

(define-test (deriv a) ('(+ (* 3 x x) (* a x x) (* b x) 5))
  (define (deriv-aux a) (list '/ (deriv a) a))
  (cond
   ((not (pair? a))
    (cond ((eq? a 'x) 1) (else 0)))
   ((eq? (car a) '+)
    (cons '+ (map deriv (cdr a))))
   ((eq? (car a) '-)
    (cons '- (map deriv (cdr a))))
   ((eq? (car a) '*)
    (list '*
          a
          (cons '+ (map deriv-aux (cdr a)))))
   ((eq? (car a) '/)
    (list '-
          (list '/
                (deriv (cadr a))
                (caddr a))
          (list '/
                (cadr a)
                (list '*
                      (caddr a)
                      (caddr a)
                      (deriv (caddr a))))))
   (else 'error)))

(test-end "vm-lightning-test")
