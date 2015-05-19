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

(use-modules (ice-9 binary-ports)
             (ice-9 format)
             (oop goops)
             (rnrs bytevectors)
             (srfi srfi-9)
             (srfi srfi-11)
             (srfi srfi-64)
             (system foreign)
             (system vm program)
             (system vm lightning)
             (system vm loader)
             (system vm vm))

;; Define and run a procedure with guile vm and lightning, and compare the
;; results with test-equal.
(define-syntax define-test
  (syntax-rules ()
    ((_ (name . vars) args body ...)
     (begin
       (define (name . vars)
         body ...)
       (format #t ";;; Test: ~a~%" '(name . vars))
       (test-equal
        (symbol->string (procedure-name name))
        (call-with-values (lambda () (name . args))
          (lambda vs vs))
        (call-with-values (lambda () (call-lightning name . args))
          (lambda vs vs)))))))

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

(define-test (t-call-my-map xs) ('(1 2 3 4 5))
  (my-map add-one xs))

(define-test (t-call-my-map-twice xs) ('(1 2 3 4 5))
  (append (my-map add-one xs)
          (my-map mul-two xs)))

(define param01 (make-parameter 100))

(define-test (t-call-parameter x) (100)
  (param01))

(define-test (t-apply-parameter f xs) (param01 '())
  (apply f xs))

(define procedure-with-setter
  (make-procedure-with-setter
   (lambda (obj)
     (+ obj 1))
   (lambda (obj val)
     (list obj val))))

(define-test (t-call-procedure-with-setter n) (99)
  (procedure-with-setter n))

(define g01 (make-guardian))

(define-test (t-call-guardian) ()
  (g01))

(define-test (t-call-smob0 x) ('apply-smob/0)
  (let-values (((names-port get-name-bv) (open-bytevector-output-port)))
    (values x (get-name-bv))))

(define-test (t-call-branched-a x) (#f)
  ((or x *) 20 30))

(define-test (t-call-branched-b x) (+)
  ((or x *) 20 30))

(define (call-branched x)
  ((or x *) 20 30))

(define-test (t-call-branched-c x) (#f)
  (call-branched x))

(define-test (t-call-branched-d x) (+)
  (call-branched x))

(define (call-branched-prim x)
  ((if (even? x) + *) 20 30))

(define-test (t-call-branched-prim-a x) (1)
  (call-branched-prim x))

(define-test (t-call-branched-prim-b x) (2)
  (call-branched-prim x))

(define (sum-and-product x y)
  (values (+ x y) (* x y)))

(define-test (t-call-sum-and-product x y) (12 34)
  (call-with-values
      (lambda ()
        (sum-and-product x y))
    (lambda (a b)
      (cons a b))))

(define-test (t-call-sum-and-product-again x y) (12 34)
  (call-with-values
      (lambda ()
        (sum-and-product x y))
    (lambda (a b)
      (cons a b))))

(test-equal "t-tail-call-shuffle"
            (call-with-values
                (lambda () (values 1 2 3))
              (lambda (a b c) (+ a b c)))
            (call-lightning call-with-values
                            (lambda () (values 1 2 3))
                            (lambda (a b c) (+ a b c))))

(define (two-values x)
  (if (null? x)
      (values #f x)
      (car x)))

(define-test (t-call-two-values-true x) ('())
  (if (two-values x) 'true-branch 'false-branch))

(define-test (t-call-two-values-false x) ('(1 2 3))
  (if (two-values x) 'true-branch 'false-branch))

(define fluid-tmp (make-fluid))

(define-test (t-recursive-values-apply n acc) (10 '())
  (with-fluids ((fluid-tmp 123))
    (if (< 0 n)
        (call-with-values (lambda () (values n))
          (lambda (a)
            (t-recursive-values-apply (- n 1) (cons a acc))))
        (values n acc))))

(define-test (t-recursive-values-apply-single x) (10)
  (define (lp x acc)
    (if (< 0 x)
        (cons x (lp (- x 1) acc))
        acc))
  (with-fluids ((fluid-tmp 123))
    (lp x '())))

(define-test (t-return-no-values) ()
  (values))


;;; Specialized call stubs

(define-test (t-subr-call-length x y) ('(a b c) '(d e f g h))
  (+ (length x) (length y)))

(define-test (t-subr-call-append x y) ('(a b c) '(d e f g h))
  (append x y))

(define-test (t-subr-call-opt-rest-1 a) (#\a)
  (char=? a))

(define-test (t-subr-call-opt-rest-2a a b) (#\a #\a)
  (char=? a b))

(define-test (t-subr-call-opt-rest-2b a b) (#\a #\b)
  (char=? a b))

(define-test (t-subr-call-opt-rest-3a a b c) (#\a #\a #\a)
  (char=? a b c))

(define-test (t-subr-call-opt-rest-3b a b c) (#\a #\a #\b)
  (char=? a b c))

(define-test (t-subr-call-opt-rest-4 a b c d) (#\a #\a #\a #\a)
  (char=? a b c d))

(define-test (t-subr-call-string->list str) ("foo-bar-buzz")
  (string->list str))

(define-test (t-subr-call-string-append str1 str2) ("foo" "bar")
  (string-append str1 str2))

(define-test (t-subr-call-make-string n fill) (113 #\a)
  (make-string n fill))

(define (scm-sum x y)
  (define %scm-sum
    (pointer->procedure '*
                        (dynamic-func "scm_sum" (dynamic-link))
                        '(* *)))
  (pointer->scm (%scm-sum (scm->pointer x) (scm->pointer y))))

(define-test (t-foreigncall-scm-sum x y) (100 200)
  (scm-sum x y))

(define j0
  (pointer->procedure double
                      (dynamic-func "j0" (dynamic-link "libm"))
                      (list double)))

(define-test (t-foreign-call-j0 x) (1.234)
  (j0 x))

(define fmod
  (pointer->procedure double
                      (dynamic-func "fmod" (dynamic-link "libm"))
                      (list double double)))

(define-test (t-foreign-call-fmod x y) (1.2 0.25)
  (fmod x y))

(define-test (t-return-builtin-apply) ()
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

(define-test (t-apply-empty-list-to-values f g) (apply values)
  (f g '()))

(define (my-call-ec proc)
  (call-with-prompt
   'foo
   (lambda ()
     (proc (lambda () (abort-to-prompt 'foo))))
   (lambda (k) 'aborted)))

(define-test (t-abort-negative-1 x) (1)
  (my-call-ec
   (lambda (return)
     (if (< 0 x) x (return)))))

(define-test (t-abort-negative-2 x) (-1)
  (my-call-ec
   (lambda (return)
     (if (< 0 x) x (return)))))

(define-test (t-abort-and-return-arg2 proc x) ((lambda (re) (re)) 'bar)
  (call-with-prompt
   'foo
   (lambda ()
     (proc (lambda () (abort-to-prompt 'foo))))
   (lambda (k) x)))

(define-test (t-abort-arg1 x) ('bar)
  (call-with-prompt
   'foo
   (lambda ()
     (abort-to-prompt 'foo x))
   (lambda (_ arg1) arg1)))

(define-test (t-abort-rest a b c) (1 2 3)
  (call-with-prompt
   'foo
   (lambda ()
     (abort-to-prompt 'foo a b c))
   (lambda (_ . rest) (apply + rest))))

(define-test (t-prompt-capture x) (-56)
  (+ (call-with-prompt 'foo
                       (lambda ()
                         (if (< x 0)
                             (abort-to-prompt 'foo)
                             123))
                       (lambda (k)
                         (k 456)))
     x))

(define-test (t-return-builtin-values) ()
  values)

(define-test (t-return-builtin-abort) ()
  abort-to-prompt)

(define-test (t-return-builtin-call-with-values) ()
  call-with-values)

(define-test (t-return-builtin-call-with-current-continuation) ()
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
            (call-lightning sum-optional 1))

(test-equal "t-sum-optional-two"
            (sum-optional 1 2)
            (call-lightning sum-optional 1 2))

(test-equal "t-sum-optional-two"
            (sum-optional 1 2 3)
            (call-lightning sum-optional 1 2 3))

(define br-nargs-1
  (case-lambda
    (() 100)
    ((x) (cons x 100))))

(test-equal "t-br-if-nargs-1-0"
            (br-nargs-1)
            (call-lightning br-nargs-1))

(test-equal "t-br-if-nargs-1-1"
            (br-nargs-1 99)
            (call-lightning br-nargs-1 99))

(define* (bind-kwargs-01 #:key (x 100))
  x)

(define-test (t-bind-kwargs-01-default) ()
  (bind-kwargs-01))

(define-test (t-bind-kwargs-01-with-x x) (123)
  (bind-kwargs-01 #:x x))

(define* (bind-kwargs-02 a #:key (b 2) (c 3))
  (+ (* a b) c))

(define-test (t-bind-kwargs-02-default x) (10)
  (bind-kwargs-02 x))

(define-test (t-bind-kwargs-02-with-b x) (10)
  (bind-kwargs-02 x #:b x))

(define-test (t-bind-kwargs-02-with-c x) (10)
  (bind-kwargs-02 x #:c x))

(define-test (t-bind-kwargs-02-with-b-and-c x) (10)
  (bind-kwargs-02 x #:b x #:c x))

(define* (bind-kwargs-03 a #:key (b 2) . rest)
  (+ (* a (length rest)) b))

(define-test (t-bind-kwargs-03 x) (100)
  (bind-kwargs-03 x 'foo 'bar 'buzz))

(define-test (t-bind-kwargs-03-with-b x) (100)
  (bind-kwargs-03 x #:b 23 'foo 'bar 'buzz))

(define* (bind-kwargs-04 a #:key (b 2) (c 3) (d 4) (e 5) (f 6))
  (+ a b c d e f))

(define-test (t-bind-kwargs-04-with-b x) (100)
  (bind-kwargs-04 1 #:b x))

(define-test (t-bind-kwargs-04-with-c x) (100)
  (bind-kwargs-04 1 #:c x))

(define-test (t-bind-kwargs-04-with-d x) (100)
  (bind-kwargs-04 1 #:d x))

(define-test (t-bind-kwargs-04-with-e x) (100)
  (bind-kwargs-04 1 #:e x))

(define-test (t-bind-kwargs-04-with-f x) (100)
  (bind-kwargs-04 1 #:f x))


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

(define-test (t-if-logtest-true a b) (#b1011 #b0101)
  (logtest a b))

(define-test (t-if-logtest-false a b) (#b1011 #b0100)
  (logtest a b))

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

(define-test (t-closure02 x) (23)
  ((closure01 x) 100))

(define (closure03 n)
  (lambda ()
    (+ n 1)))

(define-test (t-call-closure03 x) (23)
  ((closure03 (+ ((closure03 x))
                 ((closure03 10))))))

(define-test (t-call-closure03-b x) (12345)
  (cons ((closure03 (+ x 100))) 23))

(define (closure04 n)
  (lambda ()
    (let lp ((n n) (acc 0))
      (if (< n 0)
          acc
          (lp (- n 1) (+ acc n))))))

(define-test (t-call-closure04 n) (100)
  ((closure04 n)))

(define-test (t-call-closure05 x) (10)
  (let ((l (list (closure03 x)
                 (t-call-closure03 x)
                 (t-call-closure03-b x))))
    ((car l))))

(define (addk x y k)
  (k (+ x y)))

(define (mulk x y k)
  (k (* x y)))

(define-test (t-muladdk x y z k) (3 4 5 (lambda (a) a))
  (mulk x y
        (lambda (xy)
          (addk xy z k))))

(define-test (t-pythk2 x y k) (3 4 (lambda (a) a))
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

;; Testing `static-set!' and `static-patch!' with `load' procedure.
;; Init section in compiled file contains these VM ops.
(define source-file
  (let ((port (mkstemp! (string-copy "/tmp/ccXXXXXX"))))
    (with-output-to-port port
      (lambda ()
        (write '(define (blah a b)
                  (* a b)))
        (write '(blah 123 456))
        (display #\newline)))
    (port-filename port)))

(define compiled-file (compile-file source-file))

(define-test (t-static-xxx! path) (compiled-file)
  (let ((thunk (load-thunk-from-file path)))
    (thunk)))

(define (my-define! sym val)
  (define! sym val))

(begin
  (call-lightning my-define! 'var1 123)
  (test-equal "t-define!" 123 var1))

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

(define-test (t-quo-pos-pos a b) (100 3)
  (quotient a b))

(define-test (t-quo-neg-pos a b) (-100 3)
  (quotient a b))

(define-test (t-quo-pos-neg a b) (100 -3)
  (quotient a b))

(define-test (t-quo-neg-neg a b) (-100 -3)
  (quotient a b))

(define-test (t-rem-pos-pos a b) (100 3)
  (remainder a b))

(define-test (t-rem-neg-pos a b) (-100 3)
  (remainder a b))

(define-test (t-rem-pos-neg a b) (100 -3)
  (remainder a b))

(define-test (t-rem-neg-neg a b) (-100 -3)
  (remainder a b))

(define-test (t-mod-pos-pos a b) (100 3)
  (modulo a b))

(define-test (t-mod-neg-pos a b) (-100 3)
  (modulo a b))

(define-test (t-mod-pos-neg a b) (100 -3)
  (modulo a b))

(define-test (t-mod-neg-neg a b) (-100 -3)
  (modulo a b))

(define-test (t-mod-zero a b) (100 5)
  (modulo a b))

(define-test (t-ash-r-pos a b) (8192 24)
  (ash a b))

(define-test (t-ash-r-neg a b) (-8192 24)
  (ash a b))

(define-test (t-ash-r-shift-1024 a b) (1 1024)
  (ash a b))

(define-test (t-ash-r-most-positive a b) (most-positive-fixnum 1)
  (ash a b))

(define-test (t-ash-r-most-negative a b) (most-negative-fixnum 1)
  (ash a b))

(define-test (t-ash-l-pos a b) (8192 -24)
  (ash a b))

(define-test (t-ash-l-neg a b) (-8192 -24)
  (ash a b))

(define-test (t-ash-l-shift-1024 a b) (1 -1024)
  (ash a b))

(define-test (t-ash-l-most-positive a b) (most-positive-fixnum -1)
  (ash a b))

(define-test (t-ash-l-most-negative a b) (most-negative-fixnum -1)
  (ash a b))

(define-test (t-logandr-fx-fx a b) (256 128)
  (logand a b))

(define-test (t-logandr-bignum-fx a b)
  (340282366920938463463374607431768211456 128)
  (logand a b))

(define-test (t-logandr-fx-bignum a b)
  (128 340282366920938463463374607431768211456)
  (logand a b))

(define-test (t-logandr-bignum-bignum a b)
  (170141183460469231731687303715884105728
   340282366920938463463374607431768211456)
  (logand a b))

(define-test (t-logior-fx-fx a b) (256 128)
  (logior a b))

(define-test (t-logior-bignum-fx a b)
  (340282366920938463463374607431768211456 128)
  (logior a b))

(define-test (t-logior-fx-bignum a b)
  (128 340282366920938463463374607431768211456)
  (logior a b))

(define-test (t-logior-bignum-bignum a b)
  (170141183460469231731687303715884105728
   340282366920938463463374607431768211456)
  (logior a b))

(define-test (t-logxor-fx-fx a b) (256 128)
  (logxor a b))

(define-test (t-logxor-bignum-fx a b)
  (340282366920938463463374607431768211456 128)
  (logxor a b))

(define-test (t-logxor-fx-bignum a b)
  (128 340282366920938463463374607431768211456)
  (logxor a b))

(define-test (t-logxor-bignum-bignum a b)
  (170141183460469231731687303715884105728
   340282366920938463463374607431768211456)
  (logxor a b))

(define-test (t-make-vector len fill) (16 'foo)
  (make-vector len fill))

(define-test (t-make-vector-immediate fill) ('foo)
  (make-vector 10 fill))

(define (make-vec)
  (list->vector '(1 2 3 4 5)))

(define-test (t-vector-length v) ((make-vec))
  (vector-length v))

(define-test (t-vector-ref v idx) ((make-vec) 3)
  (vector-ref v idx))

(define-test (t-vector-ref-min v idx) ((make-vec) 0)
  (vector-ref v idx))

(define-test (t-vector-ref-max v idx) ((make-vec) 4)
  (vector-ref v idx))

(define-test (t-vector-ref-immediate v) ((make-vec))
  (vector-ref v 3))

(define-test (t-vector-ref-immediate-min v) ((make-vec))
  (vector-ref v 0))

(define-test (t-vector-ref-immediate-max v) ((make-vec))
  (vector-ref v 4))

(define-test (t-vector-set! v idx) ((make-vec) 3)
  (vector-set! v idx 999)
  v)

(define-test (t-vector-set!-min v idx) ((make-vec) 0)
  (vector-set! v idx 999)
  v)

(define-test (t-vector-set!-max v idx) ((make-vec) 4)
  (vector-set! v idx 999)
  v)

(define-test (t-vector-set!-immediate v) ((make-vec))
  (vector-set! v 3 999)
  v)

(define-test (t-vector-set!-immediate-min v) ((make-vec))
  (vector-set! v 0 999)
  v)

(define-test (t-vector-set!-immediate-max v) ((make-vec))
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

(define-test (t-struct-ref x) (<r02>)
  (record-type-name x))

(define-test (t-struct-ref-immediate r) (r02-a)
  (struct-ref r 0))

(define-test (t-struct-set! x pos val) (r02-a 0 'foo)
  (struct-set! x pos val))

(define-test (t-struct-set-immediate x y) (1 2)
  (make-r02 x y))

(define-test (t-class-of-real x) (0.125)
  (class-of x))

(define-test (t-class-of-vector x) (0.125)
  (class-of x))

(define-test (t-class-of-class-of x) (class-of)
  (class-of x))

(define-class <class01> ()
  x y)

(define instance01 (make <class01>))

(define-test (t-class-of-instance x) (instance01)
  (class-of x))


;;; Arrays, packed uniform arrays, and bytevectors

(define bv01
  (u8-list->bytevector
   (map (lambda (x) (+ x 128))
        '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))))

(define-test (t-u8-ref bv) (bv01)
  (cons (bytevector-u8-ref bv 0)
        (bytevector-u8-ref bv 1)))

(define-test (t-s8-ref bv) (bv01)
  (cons (bytevector-s8-ref bv 0)
        (bytevector-s8-ref bv 1)))

(define-test (t-u16-ref bv) (bv01)
  (cons (bytevector-u16-native-ref bv 0)
        (bytevector-u16-native-ref bv 1)))

(define-test (t-s16-ref bv) (bv01)
  (cons (bytevector-s16-native-ref bv 0)
        (bytevector-s16-native-ref bv 1)))

(define-test (t-u32-ref bv) (bv01)
  (cons (bytevector-u32-native-ref bv 0)
        (bytevector-u32-native-ref bv 1)))

(define-test (t-s32-ref bv) (bv01)
  (cons (bytevector-s32-native-ref bv 0)
        (bytevector-s32-native-ref bv 1)))

(define-test (t-u64-ref bv) (bv01)
  (cons (bytevector-u32-native-ref bv 0)
        (bytevector-u32-native-ref bv 1)))

(define-test (t-s64-ref bv) (bv01)
  (cons (bytevector-s32-native-ref bv 0)
        (bytevector-s32-native-ref bv 1)))

(define-test (t-f32-ref bv) (bv01)
  (cons (bytevector-ieee-single-native-ref bv 0)
        (bytevector-ieee-single-native-ref bv 1)))

(define-test (t-f64-ref bv) (bv01)
  (cons (bytevector-ieee-double-native-ref bv 0)
        (bytevector-ieee-double-native-ref bv 1)))

(define (make-bv)
  (u8-list->bytevector '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))

(define-test (t-u8-set! bv v) ((make-bv) 255)
  (bytevector-u8-set! bv 0 v)
  (bytevector-u8-set! bv 1 v)
  bv)

(define-test (t-s8-set! bv v) ((make-bv) -1)
  (bytevector-s8-set! bv 0 v)
  (bytevector-s8-set! bv 1 v)
  bv)

(define-test (t-u16-set! bv v) ((make-bv) 65280)
  (bytevector-u16-native-set! bv 0 v)
  (bytevector-u16-native-set! bv 2 v)
  bv)

(define-test (t-s16-set! bv v) ((make-bv) -32768)
  (bytevector-s16-native-set! bv 0 v)
  (bytevector-s16-native-set! bv 2 v)
  bv)

(define-test (t-u32-set! bv v) ((make-bv) 65535)
  (bytevector-u32-native-set! bv 0 v)
  (bytevector-u32-native-set! bv 4 v)
  bv)

(define-test (t-s32-set! bv v) ((make-bv) -65536)
  (bytevector-s32-native-set! bv 0 v)
  (bytevector-s32-native-set! bv 4 v)
  bv)

(define-test (t-u64-set! bv v) ((make-bv) (expt 2 61))
  (bytevector-u64-native-set! bv 0 v)
  (bytevector-u64-native-set! bv 4 v)
  bv)

(define-test (t-s64-set! bv v) ((make-bv) (expt 2 61))
  (bytevector-s64-native-set! bv 0 v)
  (bytevector-s64-native-set! bv 4 v)
  bv)

(define-test (t-f32-set! bv v) ((make-bv) -1.52587890625e-5)
  (bytevector-ieee-single-native-set! bv 0 v)
  (bytevector-ieee-single-native-set! bv 4 v)
  bv)

(define-test (t-f64-set! bv v) ((make-bv) -1.52587890625e-5)
  (bytevector-ieee-double-native-set! bv 0 v)
  (bytevector-ieee-double-native-set! bv 4 v)
  bv)


;;;
;;; Simple procedures
;;;

(define-test (t-lp n) (#e1e7)
  (let lp ((n n))
    (if (< 0 n) (lp (- n 1)) n)))

(define-test (t-sum-tail-call x) (1000)
  (let lp ((n x) (acc 0))
    (if (< n 0)
        acc
        (lp (- n 1) (+ acc n)))))

(define-test (t-sum-non-tail-call x) (1000)
  (let lp ((n x))
    (if (< n 0)
        0
        (+ n (lp (- n 1))))))

(define-test (t-sum-toplevel n acc) (1000 0)
  (if (= n 0)
      acc
      (t-sum-toplevel (- n 1) (+ n acc))))

(define-test (t-sum-cps n k) (10 (lambda (a) a))
  (if (< n 0)
      (k 0)
      (t-sum-cps (- n 1)
                 (lambda (s)
                   (k (+ s n))))))

(define-test (t-sum-cps-1000 n k) (1000 (lambda (a) a))
  (if (< n 0)
      (k 0)
      (t-sum-cps (- n 1)
                 (lambda (s)
                   (k (+ s n))))))

(define-test (t-fib1 n) (30)
  (let lp ((n n))
    (if (< n 2)
        n
        (+ (lp (- n 1))
           (lp (- n 2))))))

(define-test (t-fib2 n) (30)
  (if (< n 2)
      n
      (+ (t-fib2 (- n 1))
         (t-fib2 (- n 2)))))

(define-test (t-nqueens n) (8)
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

(define-test (t-tak x y z) (18 12 6)
  (if (not (< y x))
      z
      (t-tak (t-tak (- x 1) y z)
             (t-tak (- y 1) z x)
             (t-tak (- z 1) x y))))

(define-test (t-sieve n) (500)
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

(define-test (t-deriv a) ('(+ (* 3 x x) (* a x x) (* b x) 5))
  (define (deriv-aux a) (list '/ (t-deriv a) a))
  (cond
   ((not (pair? a))
    (cond ((eq? a 'x) 1) (else 0)))
   ((eq? (car a) '+)
    (cons '+ (map t-deriv (cdr a))))
   ((eq? (car a) '-)
    (cons '- (map t-deriv (cdr a))))
   ((eq? (car a) '*)
    (list '*
          a
          (cons '+ (map deriv-aux (cdr a)))))
   ((eq? (car a) '/)
    (list '-
          (list '/
                (t-deriv (cadr a))
                (caddr a))
          (list '/
                (cadr a)
                (list '*
                      (caddr a)
                      (caddr a)
                      (t-deriv (caddr a))))))
   (else 'error)))


;;;
;;; Procedures found in module (guile)
;;;

(define-test (t-gc) ()
  (gc))

(define-test (t-call-map1 xs) ('(1 2 3))
  (map (lambda (x) (+ x 1)) xs))

(define-test (t-call-map2 xs ys) ('(1 2 3) '(10 20 30))
  (map (lambda (x y) (+ x y)) xs ys))

(define-test (t-call-map3 xs ys zs) ('(1 2 3) '(10 20 30) '(100 200 300))
  (map + xs ys zs))

(define-test (t-call-map3-lambda xs ys zs)
  ('(1 2 3) '(10 20 30) '(100 200 300))
  (map (lambda (x y z) (+ x y z)) xs ys zs))

(define-test (t-format-1 str arg1 arg2) ("~x is ~a~%" 100 #f)
  (format #f str arg1 arg2))

(define-test (t-macroexpand-1 expr) ('(+ 1 2 3))
  (macroexpand expr))

(define-syntax my-syntax-01
  (syntax-rules ()
    ((_)
     (my-syntax-01 100 100))
    ((_ x)
     (my-syntax-01 x 100))
    ((_ x y)
     (list x y))))

(define-test (t-macroexpand-2 expr) ('(my-syntax))
  (macroexpand expr))

(define-test (t-macroexpand-3 expr) ('(my-syntax 99))
  (macroexpand expr))

(define-test (t-macroexpand-4 expr) ('(my-syntax 98 99))
  (macroexpand expr))

(define-test (t-primitive-eval-1 expr) ('(+ 1 (* 2 3)))
  (primitive-eval expr))

(define letrec-fib-expr
  '(letrec ((fib (lambda (n)
                   (if (< n 2)
                       n
                       (+ (fib (- n 1))
                          (fib (- n 2)))))))
     (fib 10)))

(define-test (t-primitive-eval-2 expr) (letrec-fib-expr)
  (primitive-eval expr))

(define-test (t-compile-tree-il expr) ('(+ 1 2 3))
  (compile expr #:to 'tree-il))

(define-test (t-compile-cps-1 expr) ('(+ 1 2 3))
  (compile expr #:to 'cps))

(define-test (t-compile-cps-2 expr) (letrec-fib-expr)
  (compile expr #:to 'cps))

(define-test (t-compile-value-1 expr) ('(+ 1 2 3))
  (compile expr #:to 'value))

(define-test (t-compile-value-1 expr) (letrec-fib-expr)
  (compile expr #:to 'value))


;;;
;;; GC tests
;;;

(define-test (t-list-copy-loop n a) (#e1e7 '(1 2 3 4 5))
  (let lp ((n n) (v '()))
    (if (< n 0)
        v
        (lp (- n 1) (list-copy a)))))

(define-test (t-cons-loop n a b) (#e1e7 'foo 'bar)
  (let lp ((n n) (v #f))
    (if (< n 0)
        v
        (lp (- n 1) (cons a b)))))

(define (binder . rest)
  rest)

(define-test (t-binder-loop n a b) (#e1e7 'foo 'bar)
  (let lp ((n n) (v #f))
    (if (< n 0)
        v
        (lp (- n 1) (binder a b)))))

;; Apply procedure `f' to `a' and `b', compare the result with
;; `expected' with `equal?'.  Test to see that GC will not overwrite the
;; result during loops.
(define (equal?-loop f a b expected)
  (let lp ((n 0) (v #f) (nincorrect 0))
    (if (< #e1e6 n)
        nincorrect
        (let ((result (f a b)))
          (if (equal? result expected)
              (lp (+ n 1) result nincorrect)
              (lp (+ n 1) result (+ nincorrect 1)))))))

(test-skip 1)
(define-test (t-cons-equal-loop a b expected) (12 34 '(12 . 34))
  (equal?-loop cons a b expected))

(test-skip 1)
(define-test (t-append-equal-loop a b expected)
  ('(1 2 3) '(4 5 6) '(1 2 3 4 5 6))
  (equal?-loop append a b expected))

(test-skip 1)
(define-test (t-macroexpand-loop n expr) (#e1e4 letrec-fib-expr)
  (let lp ((n n) (v #f))
    (if (< n 0)
        v
        (lp (- n 1) (macroexpand expr)))))

(test-end "vm-lightning-test")
