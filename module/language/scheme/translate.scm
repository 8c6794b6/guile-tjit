;;; Guile Scheme specification

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (language scheme translate)
  :use-module (system base language)
  :use-module (system il ghil)
  :use-module (ice-9 match)
  :use-module (ice-9 receive)
  :use-module (srfi srfi-39)
  :use-module ((system base compile) :select (syntax-error))
  :export (translate))


;; Module in which compile-time code (macros) is evaluated.
(define &compile-time-module (make-parameter #f))

(define (eval-at-compile-time exp)
  "Evaluate @var{exp} in the current compile-time module."
  (catch #t
    (lambda ()
      (save-module-excursion
       (lambda ()
	 (eval exp (&compile-time-module)))))
    (lambda (key . args)
      (syntax-error #f
		    (format #f "~a: compile-time evaluation failed" exp)
		    (cons key args)))))

(define (translate x e)
  (parameterize ((&compile-time-module (make-module)))

    ;; Import only core bindings in the macro module.
    (module-use! (&compile-time-module) the-root-module)

    (call-with-ghil-environment (make-ghil-mod e) '()
      (lambda (env vars)
	(make-ghil-lambda env #f vars #f (trans env #f x))))))


;;;
;;; Macro tricks
;;;

(define (expand-macro e)
  ;; Similar to `macroexpand' in `boot-9.scm' except that it does not expand
  ;; `define-macro' and `defmacro'.
  (cond
   ((pair? e)
    (let* ((head (car e))
	   (val (and (symbol? head)
		     (false-if-exception
		      (module-ref (&compile-time-module) head)))))
      (case head
	((defmacro define-macro)
	 ;; Normally, these are expanded as `defmacro:transformer' but we
	 ;; don't want it to happen since they are handled by `trans-pair'.
	 e)

	((use-syntax)
	 ;; `use-syntax' is used to express a compile-time dependency
	 ;; (because we use a macro from that module, or because one of our
	 ;; macros uses bindings from that module).  Thus, we arrange to get
	 ;; the current compile-time module to use it.
	 (let* ((module-name (cadr e))
		(module (false-if-exception (resolve-module module-name))))
	   (if (module? module)
	       (let ((public-if (module-public-interface module)))
		 (module-use! (&compile-time-module) public-if))
	       (syntax-error #f "invalid `use-syntax' form" e)))
	 '(void))

	((begin let let* letrec lambda quote quasiquote if and or
		set! cond case eval-case define do)
	 ;; All these built-in macros should not be expanded.
	 e)

	(else
	 ;; Look for a macro.
	 (let ((ref (false-if-exception
		     (module-ref (&compile-time-module) head))))
	   (if (macro? ref)
	       (expand-macro
		(save-module-excursion
		 (lambda ()
		   (let ((transformer (macro-transformer ref))
			 (syntax-error syntax-error))
		     (set-current-module (&compile-time-module))
		     (catch #t
		       (lambda ()
			 (transformer (copy-tree e) (current-module)))
		       (lambda (key . args)
			 (syntax-error #f
				       (format #f "~a: macro transformer failed"
					       head)
				       (cons key args))))))))
	       e))))))

   (#t e)))


;;;
;;; Translator
;;;

(define %scheme-primitives
  '(not null? eq? eqv? equal? pair? list? cons car cdr set-car! set-cdr!))

(define %forbidden-primitives
  ;; Guile's `procedure->macro' family is evil because it crosses the
  ;; compilation boundary.  One solution might be to evaluate calls to
  ;; `procedure->memoizing-macro' at compilation time, but it may be more
  ;; compicated than that.
  '(procedure->syntax procedure->macro procedure->memoizing-macro))

(define (trans e l x)
  (cond ((pair? x)
	 (let ((y (expand-macro x)))
	   (if (eq? x y)
	       (trans-pair e (or (location x) l) (car x) (cdr x))
	       (trans e l y))))
	((symbol? x)
	 (let ((y (symbol-expand x)))
	   (if (symbol? y)
	       (make-ghil-ref e l (ghil-lookup e y))
	       (trans e l y))))
	(else (make-ghil-quote e l x))))

(define (symbol-expand x)
  (let loop ((s (symbol->string x)))
    (let ((i (string-rindex s #\.)))
      (if i
	(let ((sym (string->symbol (substring s (1+ i)))))
	  `(slot ,(loop (substring s 0 i)) (quote ,sym)))
	(string->symbol s)))))

(define (trans-pair e l head tail)
  (define (trans:x x) (trans e l x))
  (define (trans:pair x) (trans-pair e l (car x) (cdr x)))
  (define (trans:body body) (trans-body e l body))
  (define (make:void) (make-ghil-void e l))
  (define (bad-syntax)
    (syntax-error l (format #f "bad ~A" head) (cons head tail)))
  (case head
    ;; (void)
    ((void)
     (match tail
       (() (make:void))
       (else (bad-syntax))))

    ;; (quote OBJ)
    ((quote)
     (match tail
       ((obj) (make-ghil-quote e l obj))
       (else (bad-syntax))))

    ;; (quasiquote OBJ)
    ((quasiquote)
     (match tail
       ((obj) (make-ghil-quasiquote e l (trans-quasiquote e l obj)))
       (else (bad-syntax))))

    ((define define-private)
     (match tail
       ;; (define NAME VAL)
       (((? symbol? name) val)
	(make-ghil-define e l (ghil-lookup e name) (trans:x val)))

       ;; (define (NAME FORMALS...) BODY...)
       ((((? symbol? name) . formals) . body)
	;; -> (define NAME (lambda FORMALS BODY...))
	(let ((val (trans:x `(lambda ,formals ,@body))))
	  (make-ghil-define e l (ghil-lookup e name) val)))

       (else (bad-syntax))))

    ;; simple macros
    ((defmacro define-macro)
     ;; Evaluate the macro definition in the current compile-time module.
     (eval-at-compile-time (cons head tail))

     ;; FIXME: We need to evaluate them in the runtime module as well.
     (make:void))

    ((set!)
     (match tail
       ;; (set! NAME VAL)
       (((? symbol? name) val)
	(make-ghil-set e l (ghil-lookup e name) (trans:x val)))

       ;; (set! (NAME ARGS...) VAL)
       ((((? symbol? name) . args) val)
	;; -> ((setter NAME) ARGS... VAL)
	(trans:pair `((setter ,name) . (,@args ,val))))

       (else (bad-syntax))))

    ;; (if TEST THEN [ELSE])
    ((if)
     (match tail
       ((test then)
	(make-ghil-if e l (trans:x test) (trans:x then) (make:void)))
       ((test then else)
	(make-ghil-if e l (trans:x test) (trans:x then) (trans:x else)))
       (else (bad-syntax))))

    ;; (and EXPS...)
    ((and)
     (make-ghil-and e l (map trans:x tail)))

    ;; (or EXPS...)
    ((or)
     (make-ghil-or e l (map trans:x tail)))

    ;; (begin EXPS...)
    ((begin)
     (make-ghil-begin e l (map trans:x tail)))

    ((let)
     (match tail
       ;; (let NAME ((SYM VAL) ...) BODY...)
       (((? symbol? name) (((? symbol? sym) val) ...) body ...)
	;; -> (letrec ((NAME (lambda (SYM...) BODY...))) (NAME VAL...))
	(trans:pair `(letrec ((,name (lambda ,sym ,@body))) (,name ,@val))))

       ;; (let () BODY...)
       ((() body ...)
	;; NOTE: This differs from `begin'
	(make-ghil-begin e l (list (trans:body body))))

       ;; (let ((SYM VAL) ...) BODY...)
       (((((? symbol? sym) val) ...) body ...)
	(let ((vals (map trans:x val)))
	  (call-with-ghil-bindings e sym
	    (lambda (vars)
	      (make-ghil-bind e l vars vals (trans:body body))))))

       (else (bad-syntax))))

    ;; (let* ((SYM VAL) ...) BODY...)
    ((let*)
     (match tail
       (((def ...) body ...)
	(if (null? def)
	    (trans:pair `(let () ,@body))
	    (trans:pair `(let (,(car def)) (let* ,(cdr def) ,@body)))))
       (else (bad-syntax))))

    ;; (letrec ((SYM VAL) ...) BODY...)
    ((letrec)
     (match tail
       (((((? symbol? sym) val) ...) body ...)
	(call-with-ghil-bindings e sym
	  (lambda (vars)
	    (let ((vals (map trans:x val)))
	      (make-ghil-bind e l vars vals (trans:body body))))))
       (else (bad-syntax))))

    ;; (cond (CLAUSE BODY...) ...)
    ((cond)
     (match tail
       (() (make:void))
       ((('else . body)) (trans:body body))
       (((test) . rest) (trans:pair `(or ,test (cond ,@rest))))
       (((test '=> proc) . rest)
	(trans:pair `(let ((_t ,test)) (if _t (,proc _t) (cond ,@rest)))))
       (((test . body) . rest)
	(trans:pair `(if ,test (begin ,@body) (cond ,@rest))))
       (else (bad-syntax))))

    ;; (case EXP ((KEY...) BODY...) ...)
    ((case)
     (match tail
       ((exp . clauses)
	(trans:pair
	 `(let ((_t ,exp))
	    ,(let loop ((ls clauses))
	       (cond ((null? ls) '(void))
		     ((eq? (caar ls) 'else) `(begin ,@(cdar ls)))
		     (else `(if (memv _t ',(caar ls))
				(begin ,@(cdar ls))
				,(loop (cdr ls)))))))))
       (else (bad-syntax))))

    ;; (do ((SYM VAL [UPDATE]) ...) (TEST RESULT...) BODY...)
    ((do)
     (let ()
       (define (next s x) (if (pair? x) (car x) s))
       (match tail
	 ((((sym init . update) ...) (test . result) body ...)
	  (trans:pair
	   `(letrec ((_l (lambda ,sym
			   (if ,test
			       (let () (void) ,@result)
			       (let () (void) ,@body
				    (_l ,@(map next sym update)))))))
	      (_l ,@init))))
	 (else (bad-syntax)))))

    ;; (lambda FORMALS BODY...)
    ((lambda)
     (match tail
       ((formals body ...)
	(receive (syms rest) (parse-formals formals)
	  (call-with-ghil-environment e syms
	    (lambda (env vars)
	      (make-ghil-lambda env l vars rest (trans-body env l body))))))
       (else (bad-syntax))))

    ((eval-case)
     (let loop ((x tail))
       (match x
	 (() (make:void))
	 ((('else . body)) (trans:pair `(begin ,@body)))
	 (((((? symbol? key) ...) body ...) rest ...)
	  (if (memq 'load-toplevel key)
	      (begin
		(primitive-eval `(begin ,@(copy-tree body)))
		(trans:pair `(begin ,@body)))
	      (loop rest)))
	 (else (bad-syntax)))))

    (else
     (if (memq head %scheme-primitives)
	 (make-ghil-inline e l head (map trans:x tail))
	 (if (memq head %forbidden-primitives)
	     (syntax-error l (format #f "`~a' is forbidden" head)
			   (cons head tail))
	     (make-ghil-call e l (trans:x head) (map trans:x tail)))))))

(define (trans-quasiquote e l x)
  (cond ((not (pair? x)) x)
	((memq (car x) '(unquote unquote-splicing))
	 (let ((l (location x)))
	   (match (cdr x)
	     ((obj)
	      (if (eq? (car x) 'unquote)
		  (make-ghil-unquote e l (trans e l obj))
		  (make-ghil-unquote-splicing e l (trans e l obj))))
	     (else (syntax-error l (format #f "bad ~A" (car x)) x)))))
	(else (cons (trans-quasiquote e l (car x))
		    (trans-quasiquote e l (cdr x))))))

(define (trans-body e l body)
  (define (define->binding df)
    (match (cdr df)
      (((? symbol? name) val) (list name val))
      ((((? symbol? name) . formals) . body)
       (list name `(lambda ,formals ,@body)))
      (else (syntax-error (location df) "bad define" df))))
  ;; main
  (let loop ((ls body) (ds '()))
    (cond ((null? ls) (syntax-error l "bad body" body))
	  ((and (pair? (car ls)) (eq? (caar ls) 'define))
	   (loop (cdr ls) (cons (car ls) ds)))
	  (else
	   (if (null? ds)
	       (trans-pair e l 'begin ls)
	       (trans-pair e l 'letrec (cons (map define->binding ds) ls)))))))

(define (parse-formals formals)
  (cond
   ;; (lambda x ...)
   ((symbol? formals) (values (list formals) #t))
   ;; (lambda (x y z) ...)
   ((list? formals) (values formals #f))
   ;; (lambda (x y . z) ...)
   ((pair? formals)
    (let loop ((l formals) (v '()))
      (if (pair? l)
	  (loop (cdr l) (cons (car l) v))
	  (values (reverse! (cons l v)) #t))))
   (else (syntax-error (location formals) "bad formals" formals))))

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (not (null? props))
	      (cons (assq-ref props 'line) (assq-ref props 'column))))))
