;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014, 2015 Free Software Foundation, Inc.
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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;



;;; Commentary:

;;; Scheme eval, written in Scheme.
;;;
;;; Expressions are first expanded, by the syntax expander (i.e.
;;; psyntax), then memoized into internal forms. The evaluator itself
;;; only operates on the internal forms ("memoized expressions").
;;;
;;; Environments are represented as a chain of vectors, linked through
;;; their first elements.  The terminal element of an environment is the
;;; module that was current when the outer lexical environment was
;;; entered.
;;;

;;; Code:



(define (primitive-eval exp)
  "Evaluate @var{exp} in the current module."
  (define-syntax env-toplevel
    (syntax-rules ()
      ((_ env)
       (let lp ((e env))
         (if (vector? e)
             (lp (vector-ref e 0))
             e)))))

  (define-syntax make-env
    (syntax-rules ()
      ((_ n init next)
       (let ((v (make-vector (1+ n) init)))
         (vector-set! v 0 next)
         v))))

  (define-syntax make-env*
    (syntax-rules ()
      ((_ next init ...)
       (vector next init ...))))

  (define-syntax env-ref
    (syntax-rules ()
      ((_ env depth width)
       (let lp ((e env) (d depth))
         (if (zero? d)
             (vector-ref e (1+ width))
             (lp (vector-ref e 0) (1- d)))))))

  (define-syntax env-set!
    (syntax-rules ()
      ((_ env depth width val)
       (let lp ((e env) (d depth))
         (if (zero? d)
             (vector-set! e (1+ width) val)
             (lp (vector-ref e 0) (1- d)))))))

  ;; This is a modified version of Oleg Kiselyov's "pmatch".
  (define-syntax-rule (match e cs ...)
    (let ((v e)) (expand-clauses v cs ...)))

  (define-syntax expand-clauses
    (syntax-rules ()
      ((_ v) ((error "unreachable")))
      ((_ v (pat e0 e ...) cs ...)
       (let ((fk (lambda () (expand-clauses v cs ...))))
         (expand-pattern v pat (let () e0 e ...) (fk))))))

  (define-syntax expand-pattern
    (syntax-rules (_ quote unquote ?)
      ((_ v _ kt kf) kt)
      ((_ v () kt kf) (if (null? v) kt kf))
      ((_ v (quote lit) kt kf)
       (if (equal? v (quote lit)) kt kf))
      ((_ v (unquote exp) kt kf)
       (if (equal? v exp) kt kf))
      ((_ v (x . y) kt kf)
       (if (pair? v)
           (let ((vx (car v)) (vy (cdr v)))
             (expand-pattern vx x (expand-pattern vy y kt kf) kf))
           kf))
      ((_ v (? pred var) kt kf)
       (if (pred v) (let ((var v)) kt) kf))
      ((_ v #f kt kf) (if (eqv? v #f) kt kf))
      ((_ v var kt kf) (let ((var v)) kt))))

  (define-syntax typecode
    (lambda (x)
      (syntax-case x ()
        ((_ type)
         (or (memoized-typecode (syntax->datum #'type))
             (error "not a typecode" (syntax->datum #'type)))))))

  (define (compile-lexical-ref depth width)
    (lambda (env)
      (env-ref env depth width)))

  (define (compile-call f args)
    (let ((f (compile f)))
      (match args
        (() (lambda (env) ((f env))))
        ((a)
         (let ((a (compile a)))
           (lambda (env) ((f env) (a env)))))
        ((a b)
         (let ((a (compile a))
               (b (compile b)))
           (lambda (env) ((f env) (a env) (b env)))))
        ((a b c)
         (let ((a (compile a))
               (b (compile b))
               (c (compile c)))
           (lambda (env) ((f env) (a env) (b env) (c env)))))
        ((a b c . args)
         (let ((a (compile a))
               (b (compile b))
               (c (compile c))
               (args (let lp ((args args))
                       (if (null? args)
                           '()
                           (cons (compile (car args)) (lp (cdr args)))))))
           (lambda (env)
             (apply (f env) (a env) (b env) (c env)
                    (let lp ((args args))
                      (if (null? args)
                          '()
                          (cons ((car args) env) (lp (cdr args))))))))))))

  (define (compile-box-ref box)
    (match box
      ((,(typecode resolve) . var-or-loc)
       (lambda (env)
         (cond
          ((variable? var-or-loc) (variable-ref var-or-loc))
          (else
           (set! var-or-loc
                 (%resolve-variable var-or-loc (env-toplevel env)))
           (variable-ref var-or-loc)))))
      ((,(typecode lexical-ref) depth . width)
       (lambda (env)
         (variable-ref (env-ref env depth width))))
      (_
       (let ((box (compile box)))
         (lambda (env)
           (variable-ref (box env)))))))

  (define (compile-resolve var-or-loc)
    (lambda (env)
      (cond
       ((variable? var-or-loc) var-or-loc)
       (else
        (set! var-or-loc (%resolve-variable var-or-loc (env-toplevel env)))
        var-or-loc))))

  (define (compile-if test consequent alternate)
    (let ((test (compile test))
          (consequent (compile consequent))
          (alternate (compile alternate)))
      (lambda (env)
        (if (test env) (consequent env) (alternate env)))))

  (define (compile-quote x)
    (lambda (env) x))

  (define (compile-let inits body)
    (let ((body (compile body))
          (width (vector-length inits)))
      (case width
        ((0) (lambda (env)
               (body (make-env* env))))
        ((1)
         (let ((a (compile (vector-ref inits 0))))
           (lambda (env)
             (body (make-env* env (a env))))))
        ((2)
         (let ((a (compile (vector-ref inits 0)))
               (b (compile (vector-ref inits 1))))
           (lambda (env)
             (body (make-env* env (a env) (b env))))))
        ((3)
         (let ((a (compile (vector-ref inits 0)))
               (b (compile (vector-ref inits 1)))
               (c (compile (vector-ref inits 2))))
           (lambda (env)
             (body (make-env* env (a env) (b env) (c env))))))
        ((4)
         (let ((a (compile (vector-ref inits 0)))
               (b (compile (vector-ref inits 1)))
               (c (compile (vector-ref inits 2)))
               (d (compile (vector-ref inits 3))))
           (lambda (env)
             (body (make-env* env (a env) (b env) (c env) (d env))))))
        (else
         (let lp ((n width)
                  (k (lambda (env)
                       (make-env width #f env))))
           (if (zero? n)
               (lambda (env)
                 (body (k env)))
               (lp (1- n)
                   (let ((init (compile (vector-ref inits (1- n)))))
                     (lambda (env)
                       (let* ((x (init env))
                              (new-env (k env)))
                         (env-set! new-env 0 (1- n) x)
                         new-env))))))))))

  (define (compile-fixed-lambda body nreq)
    (case nreq
      ((0) (lambda (env)
             (lambda ()
               (body (make-env* env)))))
      ((1) (lambda (env)
             (lambda (a)
               (body (make-env* env a)))))
      ((2) (lambda (env)
             (lambda (a b)
               (body (make-env* env a b)))))
      ((3) (lambda (env)
             (lambda (a b c)
               (body (make-env* env a b c)))))
      ((4) (lambda (env)
             (lambda (a b c d)
               (body (make-env* env a b c d)))))
      ((5) (lambda (env)
             (lambda (a b c d e)
               (body (make-env* env a b c d e)))))
      ((6) (lambda (env)
             (lambda (a b c d e f)
               (body (make-env* env a b c d e f)))))
      ((7) (lambda (env)
             (lambda (a b c d e f g)
               (body (make-env* env a b c d e f g)))))
      (else
       (lambda (env)
         (lambda (a b c d e f g . more)
           (let ((env (make-env nreq #f env)))
             (env-set! env 0 0 a)
             (env-set! env 0 1 b)
             (env-set! env 0 2 c)
             (env-set! env 0 3 d)
             (env-set! env 0 4 e)
             (env-set! env 0 5 f)
             (env-set! env 0 6 g)
             (let lp ((n 7) (args more))
               (cond
                ((= n nreq)
                 (unless (null? args)
                   (scm-error 'wrong-number-of-args
                              "eval" "Wrong number of arguments"
                              '() #f))
                 (body env))
                ((null? args)
                 (scm-error 'wrong-number-of-args
                            "eval" "Wrong number of arguments"
                            '() #f))
                (else
                 (env-set! env 0 n (car args))
                 (lp (1+ n) (cdr args)))))))))))

  (define (compile-rest-lambda body nreq rest?)
    (case nreq
      ((0) (lambda (env)
             (lambda rest
               (body (make-env* env rest)))))
      ((1) (lambda (env)
             (lambda (a . rest)
               (body (make-env* env a rest)))))
      ((2) (lambda (env)
             (lambda (a b . rest)
               (body (make-env* env a b rest)))))
      ((3) (lambda (env)
             (lambda (a b c . rest)
               (body (make-env* env a b c rest)))))
      (else
       (lambda (env)
         (lambda (a b c . more)
           (let ((env (make-env (1+ nreq) #f env)))
             (env-set! env 0 0 a)
             (env-set! env 0 1 b)
             (env-set! env 0 2 c)
             (let lp ((n 3) (args more))
               (cond
                ((= n nreq)
                 (env-set! env 0 n args)
                 (body env))
                ((null? args)
                 (scm-error 'wrong-number-of-args
                            "eval" "Wrong number of arguments"
                            '() #f))
                (else
                 (env-set! env 0 n (car args))
                 (lp (1+ n) (cdr args)))))))))))

  (define (compile-opt-lambda body nreq rest? nopt ninits unbound make-alt)
    (lambda (env)
      (define alt (and make-alt (make-alt env)))
      (lambda args
        (let ((nargs (length args)))
          (cond
           ((or (< nargs nreq) (and (not rest?) (> nargs (+ nreq nopt))))
            (if alt
                (apply alt args)
                ((scm-error 'wrong-number-of-args
                            "eval" "Wrong number of arguments"
                            '() #f))))
           (else
            (let* ((nvals (+ nreq (if rest? 1 0) ninits))
                   (env (make-env nvals unbound env)))
              (define (bind-req args)
                (let lp ((i 0) (args args))
                  (cond
                   ((< i nreq)
                    ;; Bind required arguments.
                    (env-set! env 0 i (car args))
                    (lp (1+ i) (cdr args)))
                   (else
                    (bind-opt args)))))
              (define (bind-opt args)
                (let lp ((i nreq) (args args))
                  (cond
                   ((and (< i (+ nreq nopt)) (< i nargs))
                    (env-set! env 0 i (car args))
                    (lp (1+ i) (cdr args)))
                   (else
                    (bind-rest args)))))
              (define (bind-rest args)
                (when rest?
                  (env-set! env 0 (+ nreq nopt) args))
                (body env))
              (bind-req args))))))))

  (define (compile-kw-lambda body nreq rest? nopt kw ninits unbound make-alt)
    (define allow-other-keys? (car kw))
    (define keywords (cdr kw))
    (lambda (env)
      (define alt (and make-alt (make-alt env)))
      (lambda args
        (define (npositional args)
          (let lp ((n 0) (args args))
            (if (or (null? args)
                    (and (>= n nreq) (keyword? (car args))))
                n
                (lp (1+ n) (cdr args)))))
        (let ((nargs (length args)))
          (cond
           ((or (< nargs nreq)
                (and alt (not rest?) (> (npositional args) (+ nreq nopt))))
            (if alt
                (apply alt args)
                ((scm-error 'wrong-number-of-args
                            "eval" "Wrong number of arguments"
                            '() #f))))
           (else
            (let* ((nvals (+ nreq (if rest? 1 0) ninits))
                   (env (make-env nvals unbound env)))
              (define (bind-req args)
                (let lp ((i 0) (args args))
                  (cond
                   ((< i nreq)
                    ;; Bind required arguments.
                    (env-set! env 0 i (car args))
                    (lp (1+ i) (cdr args)))
                   (else
                    (bind-opt args)))))
              (define (bind-opt args)
                (let lp ((i nreq) (args args))
                  (cond
                   ((and (< i (+ nreq nopt)) (< i nargs)
                         (not (keyword? (car args))))
                    (env-set! env 0 i (car args))
                    (lp (1+ i) (cdr args)))
                   (else
                    (bind-rest args)))))
              (define (bind-rest args)
                (when rest?
                  (env-set! env 0 (+ nreq nopt) args))
                (bind-kw args))
              (define (bind-kw args)
                (let lp ((args args))
                  (cond
                   ((and (pair? args) (pair? (cdr args))
                         (keyword? (car args)))
                    (let ((kw-pair (assq (car args) keywords))
                          (v (cadr args)))
                      (if kw-pair
                          ;; Found a known keyword; set its value.
                          (env-set! env 0 (cdr kw-pair) v)
                          ;; Unknown keyword.
                          (if (not allow-other-keys?)
                              ((scm-error
                                'keyword-argument-error
                                "eval" "Unrecognized keyword"
                                '() (list (car args))))))
                      (lp (cddr args))))
                   ((pair? args)
                    (if rest?
                        ;; Be lenient parsing rest args.
                        (lp (cdr args))
                        ((scm-error 'keyword-argument-error
                                    "eval" "Invalid keyword"
                                    '() (list (car args))))))
                   (else
                    (body env)))))
              (bind-req args))))))))

  (define (compute-arity alt nreq rest? nopt kw)
    (let lp ((alt alt) (nreq nreq) (nopt nopt) (rest? rest?))
      (if (not alt)
          (let ((arglist (list nreq
                               nopt
                               (if kw (cdr kw) '())
                               (and kw (car kw))
                               (and rest? '_))))
            (values arglist nreq nopt rest?))
          (let* ((spec (cddr alt))
                 (nreq* (car spec))
                 (rest?* (if (null? (cdr spec)) #f (cadr spec)))
                 (tail (and (pair? (cdr spec)) (pair? (cddr spec)) (cddr spec)))
                 (nopt* (if tail (car tail) 0))
                 (alt* (and tail (car (cddddr tail)))))
            (if (or (< nreq* nreq)
                    (and (= nreq* nreq)
                         (if rest?
                             (and rest?* (> nopt* nopt))
                             (or rest?* (> nopt* nopt)))))
                (lp alt* nreq* nopt* rest?*)
                (lp alt* nreq nopt rest?))))))

  (define (compile-general-lambda body nreq rest? nopt kw ninits unbound alt)
    (call-with-values
        (lambda ()
          (compute-arity alt nreq rest? nopt kw))
      (lambda (arglist min-nreq min-nopt min-rest?)
        (define make-alt
          (match alt
            (#f #f)
            ((body meta nreq . tail)
             (compile-lambda body meta nreq tail))))
        (define make-closure
          (if kw
              (compile-kw-lambda body nreq rest? nopt kw ninits unbound make-alt)
              (compile-opt-lambda body nreq rest? nopt ninits unbound make-alt)))
        (lambda (env)
          (let ((proc (make-closure env)))
            (set-procedure-property! proc 'arglist arglist)
            (set-procedure-minimum-arity! proc min-nreq min-nopt min-rest?)
            proc)))))

  (define (compile-lambda body meta nreq tail)
    (define (set-procedure-meta meta proc)
      (match meta
        (() proc)
        (((prop . val) . meta)
         (set-procedure-meta meta
                             (lambda (env)
                               (let ((proc (proc env)))
                                 (set-procedure-property! proc prop val)
                                 proc))))))
    (let ((body (compile body)))
      (set-procedure-meta
       meta
       (match tail
         (() (compile-fixed-lambda body nreq))
         ((rest? . tail)
          (match tail
            (() (compile-rest-lambda body nreq rest?))
            ((nopt kw ninits unbound alt)
             (compile-general-lambda body nreq rest? nopt kw
                                     ninits unbound alt))))))))

  (define (compile-capture-env locs body)
    (let ((body (compile body)))
      (lambda (env)
        (let* ((len (vector-length locs))
               (new-env (make-env len #f (env-toplevel env))))
          (let lp ((n 0))
            (when (< n len)
              (match (vector-ref locs n)
                ((depth . width)
                 (env-set! new-env 0 n (env-ref env depth width))))
              (lp (1+ n))))
          (body new-env)))))

  (define (compile-seq head tail)
    (let ((head (compile head))
          (tail (compile tail)))
      (lambda (env)
        (head env)
        (tail env))))

  (define (compile-box-set! box val)
    (let ((box (compile box))
          (val (compile val)))
      (lambda (env)
        (let ((val (val env)))
          (variable-set! (box env) val)))))

  (define (compile-lexical-set! depth width x)
    (let ((x (compile x)))
      (lambda (env)
        (env-set! env depth width (x env)))))

  (define (compile-call-with-values producer consumer)
    (let ((producer (compile producer))
          (consumer (compile consumer)))
      (lambda (env)
        (call-with-values (producer env)
          (consumer env)))))

  (define (compile-apply f args)
    (let ((f (compile f))
          (args (compile args)))
      (lambda (env)
        (apply (f env) (args env)))))

  (define (compile-capture-module x)
    (let ((x (compile x)))
      (lambda (env)
        (x (current-module)))))

  (define (compile-call-with-prompt tag thunk handler)
    (let ((tag (compile tag))
          (thunk (compile thunk))
          (handler (compile handler)))
      (lambda (env)
        (call-with-prompt (tag env) (thunk env) (handler env)))))

  (define (compile-call/cc proc)
    (let ((proc (compile proc)))
      (lambda (env)
        (call/cc (proc env)))))

  (define (compile exp)
    (match exp
      ((,(typecode lexical-ref) depth . width)
       (compile-lexical-ref depth width))
      
      ((,(typecode call) f . args)
       (compile-call f args))
      
      ((,(typecode box-ref) . box)
       (compile-box-ref box))

      ((,(typecode resolve) . var-or-loc)
       (compile-resolve var-or-loc))

      ((,(typecode if) test consequent . alternate)
       (compile-if test consequent alternate))

      ((,(typecode quote) . x)
       (compile-quote x))

      ((,(typecode let) inits . body)
       (compile-let inits body))

      ((,(typecode lambda) body meta nreq . tail)
       (compile-lambda body meta nreq tail))

      ((,(typecode capture-env) locs . body)
       (compile-capture-env locs body))

      ((,(typecode seq) head . tail)
       (compile-seq head tail))
      
      ((,(typecode box-set!) box . val)
       (compile-box-set! box val))

      ((,(typecode lexical-set!) (depth . width) . x)
       (compile-lexical-set! depth width x))
      
      ((,(typecode call-with-values) producer . consumer)
       (compile-call-with-values producer consumer))

      ((,(typecode apply) f args)
       (compile-apply f args))

      ((,(typecode capture-module) . x)
       (compile-capture-module x))

      ((,(typecode call-with-prompt) tag thunk . handler)
       (compile-call-with-prompt tag thunk handler))
      
      ((,(typecode call/cc) . proc)
       (compile-call/cc proc))))

  (let ((proc (compile
               (memoize-expression 
                (if (macroexpanded? exp)
                    exp
                    ((module-transformer (current-module)) exp)))))
        (env #f))
    (proc env)))
