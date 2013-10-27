;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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
;;; Environments are represented as linked lists of the form (VAL ... .
;;; MOD). If MOD is #f, it means the environment was captured before
;;; modules were booted. If MOD is the literal value '(), we are
;;; evaluating at the top level, and so should track changes to the
;;; current module.
;;;
;;; Evaluate this in Emacs to make code indentation work right:
;;;
;;;    (put 'memoized-expression-case 'scheme-indent-function 1)
;;;

;;; Code:



(eval-when (compile)
  (define-syntax capture-env
    (syntax-rules ()
      ((_ (exp ...))
       (let ((env (exp ...)))
         (capture-env env)))
      ((_ env)
       (if (null? env)
           (current-module)
           (if (not env)
               ;; the and current-module checks that modules are booted,
               ;; and thus the-root-module is defined
               (and (current-module) the-root-module)
               env)))))

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

  ;; Fast case for procedures with fixed arities.
  (define-syntax make-fixed-closure
    (lambda (x)
      (define *max-static-argument-count* 8)
      (define (make-formals n)
        (map (lambda (i)
               (datum->syntax
                x
                (string->symbol
                 (string (integer->char (+ (char->integer #\a) i))))))
             (iota n)))
      (syntax-case x ()
        ((_ eval nreq body env) (not (identifier? #'env))
         #'(let ((e env))
             (make-fixed-closure eval nreq body e)))
        ((_ eval nreq body env)
         #`(case nreq
             #,@(map (lambda (nreq)
                       (let ((formals (make-formals nreq)))
                         #`((#,nreq)
                            (lambda (#,@formals)
                              (eval body
                                    (make-env* env #,@formals))))))
                     (iota *max-static-argument-count*))
             (else
              #,(let ((formals (make-formals *max-static-argument-count*)))
                  #`(lambda (#,@formals . more)
                      (let ((env (make-env nreq #f env)))
                        #,@(map (lambda (formal n)
                                  #`(env-set! env 0 #,n #,formal))
                                formals (iota (length formals)))
                        (let lp ((i #,*max-static-argument-count*)
                                 (args more))
                          (cond
                           ((= i nreq)
                            (eval body
                                  (if (null? args)
                                      env
                                      (scm-error 'wrong-number-of-args
                                                 "eval" "Wrong number of arguments"
                                                 '() #f))))
                           ((null? args)
                            (scm-error 'wrong-number-of-args
                                       "eval" "Wrong number of arguments"
                                       '() #f))
                           (else
                            (env-set! env 0 i (car args))
                            (lp (1+ i) (cdr args))))))))))))))

  ;; Fast case for procedures with fixed arities and a rest argument.
  (define-syntax make-rest-closure
    (lambda (x)
      (define *max-static-argument-count* 3)
      (define (make-formals n)
        (map (lambda (i)
               (datum->syntax
                x
                (string->symbol
                 (string (integer->char (+ (char->integer #\a) i))))))
             (iota n)))
      (syntax-case x ()
        ((_ eval nreq body env) (not (identifier? #'env))
         #'(let ((e env))
             (make-rest-closure eval nreq body e)))
        ((_ eval nreq body env)
         #`(case nreq
             #,@(map (lambda (nreq)
                       (let ((formals (make-formals nreq)))
                         #`((#,nreq)
                            (lambda (#,@formals . rest)
                              (eval body
                                    (make-env* env #,@formals rest))))))
                     (iota *max-static-argument-count*))
             (else
              #,(let ((formals (make-formals *max-static-argument-count*)))
                  #`(lambda (#,@formals . more)
                      (let ((env (make-env (1+ nreq) #f env)))
                        #,@(map (lambda (formal n)
                                  #`(env-set! env 0 #,n #,formal))
                                formals (iota (length formals)))
                        (let lp ((i #,*max-static-argument-count*)
                                 (args more))
                          (cond
                           ((= i nreq)
                            (env-set! env 0 nreq args)
                            (eval body env))
                           ((null? args)
                            (scm-error 'wrong-number-of-args
                                       "eval" "Wrong number of arguments"
                                       '() #f))
                           (else
                            (env-set! env 0 i (car args))
                            (lp (1+ i) (cdr args))))))))))))))

  (define-syntax call
    (lambda (x)
      (define *max-static-call-count* 4)
      (syntax-case x ()
        ((_ eval proc nargs args env) (identifier? #'env)
         #`(case nargs
             #,@(map (lambda (nargs)
                       #`((#,nargs)
                          (proc
                           #,@(map
                               (lambda (n)
                                 (let lp ((n n) (args #'args))
                                   (if (zero? n)
                                       #`(eval (car #,args) env)
                                       (lp (1- n) #`(cdr #,args)))))
                               (iota nargs)))))
                     (iota *max-static-call-count*))
             (else
              (apply proc
                     #,@(map
                         (lambda (n)
                           (let lp ((n n) (args #'args))
                             (if (zero? n)
                                 #`(eval (car #,args) env)
                                 (lp (1- n) #`(cdr #,args)))))
                         (iota *max-static-call-count*))
                     (let lp ((exps #,(let lp ((n *max-static-call-count*)
                                               (args #'args))
                                        (if (zero? n)
                                            args
                                            (lp (1- n) #`(cdr #,args)))))
                              (args '()))
                       (if (null? exps)
                           (reverse args)
                           (lp (cdr exps)
                               (cons (eval (car exps) env) args)))))))))))

  ;; This macro could be more straightforward if the compiler had better
  ;; copy propagation. As it is we do some copy propagation by hand.
  (define-syntax mx-bind
    (lambda (x)
      (syntax-case x ()
        ((_ data () body)
         #'body)
        ((_ data (a . b) body) (and (identifier? #'a) (identifier? #'b))
         #'(let ((a (car data))
                 (b (cdr data)))
             body))
        ((_ data (a . b) body) (identifier? #'a)
         #'(let ((a (car data))
                 (xb (cdr data)))
             (mx-bind xb b body)))
        ((_ data (a . b) body) 
         #'(let ((xa (car data))
                 (xb (cdr data)))
             (mx-bind xa a (mx-bind xb b body))))
        ((_ data v body) (identifier? #'v)
         #'(let ((v data))
             body)))))
  
  ;; The resulting nested if statements will be an O(n) dispatch. Once
  ;; we compile `case' effectively, this situation will improve.
  (define-syntax mx-match
    (lambda (x)
      (syntax-case x (quote)
        ((_ mx data tag)
         #'(error "what" mx))
        ((_ mx data tag (('type pat) body) c* ...)
         #`(if (eqv? tag #,(or (memoized-typecode (syntax->datum #'type))
                               (error "not a typecode" #'type)))
               (mx-bind data pat body)
               (mx-match mx data tag c* ...))))))

  (define-syntax memoized-expression-case
    (lambda (x)
      (syntax-case x ()
        ((_ mx c ...)
         #'(let ((tag (car mx))
                 (data (cdr mx)))
             (mx-match mx data tag c ...)))))))


;;;
;;; On 18 Feb 2010, I did a profile of how often the various memoized expression
;;; types occur when getting to a prompt on a fresh build. Here are the numbers
;;; I got:
;;;
;;;      lexical-ref: 32933054
;;;             call: 20281547
;;;     toplevel-ref: 13228724
;;;               if: 9156156
;;;            quote: 6610137
;;;              let: 2619707
;;;           lambda: 1010921
;;;            begin: 948945
;;;      lexical-set: 509862
;;; call-with-values: 139668
;;;            apply: 49402
;;;       module-ref: 14468
;;;           define: 1259
;;;     toplevel-set: 328
;;;          call/cc: 0
;;;       module-set: 0
;;;
;;; So until we compile `case' into a computed goto, we'll order the clauses in
;;; `eval' in this order, to put the most frequent cases first.
;;;

(define primitive-eval
  (let ()
    ;; We pre-generate procedures with fixed arities, up to some number
    ;; of arguments, and some rest arities; see make-fixed-closure and
    ;; make-rest-closure above.

    ;; A unique marker for unbound keywords.
    (define unbound-arg (list 'unbound-arg))

    ;; Procedures with rest, optional, or keyword arguments, potentially with
    ;; multiple arities, as with case-lambda.
    (define (make-general-closure env body nreq rest? nopt kw inits alt)
      (define alt-proc
        (and alt                        ; (body meta nreq ...)
             (let* ((body (car alt))
                    (spec (cddr alt))
                    (nreq (car spec))
                    (rest (if (null? (cdr spec)) #f (cadr spec)))
                    (tail (and (pair? (cdr spec)) (pair? (cddr spec)) (cddr spec)))
                    (nopt (if tail (car tail) 0))
                    (kw (and tail (cadr tail)))
                    (inits (if tail (caddr tail) '()))
                    (alt (and tail (cadddr tail))))
               (make-general-closure env body nreq rest nopt kw inits alt))))
      (define (set-procedure-arity! proc)
        (let lp ((alt alt) (nreq nreq) (nopt nopt) (rest? rest?))
          (if (not alt)
              (begin
                (set-procedure-property! proc 'arglist
                                         (list nreq
                                               nopt
                                               (if kw (cdr kw) '())
                                               (and kw (car kw))
                                               (and rest? '_)))
                (set-procedure-minimum-arity! proc nreq nopt rest?))
              (let* ((spec (cddr alt))
                     (nreq* (car spec))
                     (rest?* (if (null? (cdr spec)) #f (cadr spec)))
                     (tail (and (pair? (cdr spec)) (pair? (cddr spec)) (cddr spec)))
                     (nopt* (if tail (car tail) 0))
                     (alt* (and tail (cadddr tail))))
                (if (or (< nreq* nreq)
                        (and (= nreq* nreq)
                             (if rest?
                                 (and rest?* (> nopt* nopt))
                                 (or rest?* (> nopt* nopt)))))
                    (lp alt* nreq* nopt* rest?*)
                    (lp alt* nreq nopt rest?)))))
        proc)
      (set-procedure-arity!
       (lambda %args
         (define (npositional args)
           (let lp ((n 0) (args args))
             (if (or (null? args)
                     (and (>= n nreq) (keyword? (car args))))
                 n
                 (lp (1+ n) (cdr args)))))
         (let ((nargs (length %args)))
           (cond
            ((or (< nargs nreq)
                 (and (not kw) (not rest?) (> nargs (+ nreq nopt)))
                 (and kw (not rest?) (> (npositional %args) (+ nreq nopt))))
             (if alt
                 (apply alt-proc %args)
                 ((scm-error 'wrong-number-of-args
                             "eval" "Wrong number of arguments"
                             '() #f))))
            (else
             (let* ((nvals (+ nreq (if rest? 1 0) (length inits)))
                    (env (make-env nvals unbound-arg env)))
               (let lp ((i 0) (args %args))
                 (cond
                  ((< i nreq)
                   ;; Bind required arguments.
                   (env-set! env 0 i (car args))
                   (lp (1+ i) (cdr args)))
                  ((not kw)
                   ;; Optional args (possibly), but no keyword args.
                   (let lp ((i i) (args args) (inits inits))
                     (cond
                      ((< i (+ nreq nopt))
                       (cond
                        ((< i nargs)
                         (env-set! env 0 i (car args))
                         (lp (1+ i) (cdr args) (cdr inits)))
                        (else
                         (env-set! env 0 i (eval (car inits) env))
                         (lp (1+ i) args (cdr inits)))))
                      (else
                       (when rest?
                         (env-set! env 0 i args))
                       (eval body env)))))
                  (else
                   ;; Optional args.  As before, but stop at the first
                   ;; keyword.
                   (let lp ((i i) (args args) (inits inits))
                     (cond
                      ((< i (+ nreq nopt))
                       (cond
                        ((and (< i nargs) (not (keyword? (car args))))
                         (env-set! env 0 i (car args))
                         (lp (1+ i) (cdr args) (cdr inits)))
                        (else
                         (env-set! env 0 i (eval (car inits) env))
                         (lp (1+ i) args (cdr inits)))))
                      (else
                       (when rest?
                         (env-set! env 0 i args))
                       (let ((aok (car kw))
                             (kw (cdr kw))
                             (kw-base (if rest? (1+ i) i)))
                         ;; Now scan args for keywords.
                         (let lp ((args args))
                           (cond
                            ((and (pair? args) (pair? (cdr args))
                                  (keyword? (car args)))
                             (let ((kw-pair (assq (car args) kw))
                                   (v (cadr args)))
                               (if kw-pair
                                   ;; Found a known keyword; set its value.
                                   (env-set! env 0 (cdr kw-pair) v)
                                   ;; Unknown keyword.
                                   (if (not aok)
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
                             ;; Finished parsing keywords. Fill in
                             ;; uninitialized kwargs by evalling init
                             ;; expressions in their appropriate
                             ;; environment.
                             (let lp ((i kw-base) (inits inits))
                               (cond
                                ((pair? inits)
                                 (when (eq? (env-ref env 0 i) unbound-arg)
                                   (env-set! env 0 i (eval (car inits) env)))
                                 (lp (1+ i) (cdr inits)))
                                (else
                                 ;; Finally, eval the body.
                                 (eval body env)))))))))))))))))))))

    ;; The "engine". EXP is a memoized expression.
    (define (eval exp env)
      (memoized-expression-case exp
        (('lexical-ref (depth . width))
         (env-ref env depth width))
        
        (('call (f nargs . args))
         (let ((proc (eval f env)))
           (call eval proc nargs args env)))
        
        (('toplevel-ref var-or-sym)
         (variable-ref
          (if (variable? var-or-sym)
              var-or-sym
              (memoize-variable-access! exp
                                        (capture-env (env-toplevel env))))))

        (('if (test consequent . alternate))
         (if (eval test env)
             (eval consequent env)
             (eval alternate env)))
      
        (('quote x)
         x)

        (('let (inits . body))
         (let* ((width (vector-length inits))
                (new-env (make-env width #f (capture-env env))))
           (let lp ((i 0))
             (when (< i width)
               (env-set! new-env 0 i (eval (vector-ref inits i) env))
               (lp (1+ i))))
           (eval body new-env)))

        (('lambda (body meta nreq . tail))
         (let ((proc
                (if (null? tail)
                    (make-fixed-closure eval nreq body (capture-env env))
                    (if (null? (cdr tail))
                        (make-rest-closure eval nreq body (capture-env env))
                        (apply make-general-closure (capture-env env)
                               body nreq tail)))))
           (let lp ((meta meta))
             (unless (null? meta)
               (set-procedure-property! proc (caar meta) (cdar meta))
               (lp (cdr meta))))
           proc))

        (('seq (head . tail))
         (begin
           (eval head env)
           (eval tail env)))
        
        (('lexical-set! ((depth . width) . x))
         (env-set! env depth width (eval x env)))
        
        (('call-with-values (producer . consumer))
         (call-with-values (eval producer env)
           (eval consumer env)))

        (('apply (f args))
         (apply (eval f env) (eval args env)))

        (('module-ref var-or-spec)
         (variable-ref
          (if (variable? var-or-spec)
              var-or-spec
              (memoize-variable-access! exp #f))))

        (('define (name . x))
         (begin
           (define! name (eval x env))
           (if #f #f)))
      
        (('toplevel-set! (var-or-sym . x))
         (variable-set!
          (if (variable? var-or-sym)
              var-or-sym
              (memoize-variable-access! exp
                                        (capture-env (env-toplevel env))))
          (eval x env)))
      
        (('call-with-prompt (tag thunk . handler))
         (call-with-prompt
          (eval tag env)
          (eval thunk env)
          (eval handler env)))
        
        (('call/cc proc)
         (call/cc (eval proc env)))

        (('module-set! (x . var-or-spec))
         (variable-set!
          (if (variable? var-or-spec)
              var-or-spec
              (memoize-variable-access! exp #f))
          (eval x env)))))
  
    ;; primitive-eval
    (lambda (exp)
      "Evaluate @var{exp} in the current module."
      (eval 
       (memoize-expression 
        (if (macroexpanded? exp)
            exp
            ((module-transformer (current-module)) exp)))
       '()))))
