;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010
;;;; Free Software Foundation, Inc.
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

;;; This file is the first thing loaded into Guile.  It adds many mundane
;;; definitions and a few that are interesting.
;;;
;;; The module system (hence the hierarchical namespace) are defined in this
;;; file.
;;;

;;; Code:



;; Before compiling, make sure any symbols are resolved in the (guile)
;; module, the primary location of those symbols, rather than in
;; (guile-user), the default module that we compile in.

(eval-when (compile)
  (set-current-module (resolve-module '(guile))))



;;; {Error handling}
;;;

;; Define delimited continuation operators, and implement catch and throw in
;; terms of them.

(define (make-prompt-tag . stem)
  (gensym (if (pair? stem) (car stem) "prompt")))
(define default-prompt-tag
  ;; not sure if we should expose this to the user as a fluid
  (let ((%default-prompt-tag (make-prompt-tag)))
    (lambda ()
      %default-prompt-tag)))

(define (call-with-prompt tag thunk handler)
  (@prompt tag (thunk) handler))
(define (abort-to-prompt tag . args)
  (@abort tag args))


;; Define catch and with-throw-handler, using some common helper routines and a
;; shared fluid. Hide the helpers in a lexical contour.

(let ()
  ;; Ideally we'd like to be able to give these default values for all threads,
  ;; even threads not created by Guile; but alack, that does not currently seem
  ;; possible. So wrap the getters in thunks.
  (define %running-exception-handlers (make-fluid))
  (define %exception-handler (make-fluid))

  (define (running-exception-handlers)
    (or (fluid-ref %running-exception-handlers)
        (begin
          (fluid-set! %running-exception-handlers '())
          '())))
  (define (exception-handler)
    (or (fluid-ref %exception-handler)
        (begin
          (fluid-set! %exception-handler default-exception-handler)
          default-exception-handler)))

  (define (default-exception-handler k . args)
    (cond
     ((eq? k 'quit)
      (primitive-exit (cond
                       ((not (pair? args)) 0)
                       ((integer? (car args)) (car args))
                       ((not (car args)) 1)
                       (else 0))))
     (else
      (format (current-error-port) "guile: uncaught throw to ~a: ~a\n" k args)
      (primitive-exit 1))))

  (define (default-throw-handler prompt-tag catch-k)
    (let ((prev (exception-handler)))
      (lambda (thrown-k . args)
        (if (or (eq? thrown-k catch-k) (eqv? catch-k #t))
            (apply abort-to-prompt prompt-tag thrown-k args)
            (apply prev thrown-k args)))))

  (define (custom-throw-handler prompt-tag catch-k pre)
    (let ((prev (exception-handler)))
      (lambda (thrown-k . args)
        (if (or (eq? thrown-k catch-k) (eqv? catch-k #t))
            (let ((running (running-exception-handlers)))
              (with-fluids ((%running-exception-handlers (cons pre running)))
                (if (not (memq pre running))
                    (apply pre thrown-k args))
                ;; fall through
                (if prompt-tag
                    (apply abort-to-prompt prompt-tag thrown-k args)
                    (apply prev thrown-k args))))
            (apply prev thrown-k args)))))

  (define! 'catch
    ;; Until we get optargs support into Guile's C evaluator, we have to fake it
    ;; here.
    (lambda (k thunk handler . pre-unwind-handler)
      "Invoke @var{thunk} in the dynamic context of @var{handler} for
exceptions matching @var{key}.  If thunk throws to the symbol
@var{key}, then @var{handler} is invoked this way:
@lisp
 (handler key args ...)
@end lisp

@var{key} is a symbol or @code{#t}.

@var{thunk} takes no arguments.  If @var{thunk} returns
normally, that is the return value of @code{catch}.

Handler is invoked outside the scope of its own @code{catch}.
If @var{handler} again throws to the same key, a new handler
from further up the call chain is invoked.

If the key is @code{#t}, then a throw to @emph{any} symbol will
match this call to @code{catch}.

If a @var{pre-unwind-handler} is given and @var{thunk} throws
an exception that matches @var{key}, Guile calls the
@var{pre-unwind-handler} before unwinding the dynamic state and
invoking the main @var{handler}.  @var{pre-unwind-handler} should
be a procedure with the same signature as @var{handler}, that
is @code{(lambda (key . args))}.  It is typically used to save
the stack at the point where the exception occurred, but can also
query other parts of the dynamic state at that point, such as
fluid values.

A @var{pre-unwind-handler} can exit either normally or non-locally.
If it exits normally, Guile unwinds the stack and dynamic context
and then calls the normal (third argument) handler.  If it exits
non-locally, that exit determines the continuation."
      (if (not (or (symbol? k) (eqv? k #t)))
          (scm-error "catch" 'wrong-type-arg
                     "Wrong type argument in position ~a: ~a"
                     (list 1 k) (list k)))
      (let ((tag (make-prompt-tag "catch")))
        (call-with-prompt
         tag
         (lambda ()
           (with-fluids
               ((%exception-handler
                 (if (null? pre-unwind-handler)
                     (default-throw-handler tag k)
                     (custom-throw-handler tag k
                                           (car pre-unwind-handler)))))
             (thunk)))
         (lambda (cont k . args)
           (apply handler k args))))))

  (define! 'with-throw-handler
    (lambda (k thunk pre-unwind-handler)
      "Add @var{handler} to the dynamic context as a throw handler
for key @var{key}, then invoke @var{thunk}."
      (if (not (or (symbol? k) (eqv? k #t)))
          (scm-error "with-throw-handler" 'wrong-type-arg
                     "Wrong type argument in position ~a: ~a"
                     (list 1 k) (list k)))
      (with-fluids ((%exception-handler
                     (custom-throw-handler #f k pre-unwind-handler)))
        (thunk))))

  (define! 'throw
    (lambda (key . args)
      "Invoke the catch form matching @var{key}, passing @var{args} to the
@var{handler}.

@var{key} is a symbol. It will match catches of the same symbol or of @code{#t}.

If there is no handler at all, Guile prints an error and then exits."
      (if (not (symbol? key))
          ((exception-handler) 'wrong-type-arg "throw"
           "Wrong type argument in position ~a: ~a" (list 1 key) (list key))
          (apply (exception-handler) key args)))))




;;; {R4RS compliance}
;;;

(primitive-load-path "ice-9/r4rs")



;;; {Simple Debugging Tools}
;;;

;; peek takes any number of arguments, writes them to the
;; current ouput port, and returns the last argument.
;; It is handy to wrap around an expression to look at
;; a value each time is evaluated, e.g.:
;;
;;      (+ 10 (troublesome-fn))
;;      => (+ 10 (pk 'troublesome-fn-returned (troublesome-fn)))
;;

(define (peek . stuff)
  (newline)
  (display ";;; ")
  (write stuff)
  (newline)
  (car (last-pair stuff)))

(define pk peek)


(define (warn . stuff)
  (with-output-to-port (current-error-port)
    (lambda ()
      (newline)
      (display ";;; WARNING ")
      (display stuff)
      (newline)
      (car (last-pair stuff)))))



;;; {Features}
;;;

(define (provide sym)
  (if (not (memq sym *features*))
      (set! *features* (cons sym *features*))))

;; Return #t iff FEATURE is available to this Guile interpreter.  In SLIB,
;; provided? also checks to see if the module is available.  We should do that
;; too, but don't.

(define (provided? feature)
  (and (memq feature *features*) #t))



;;; {and-map and or-map}
;;;
;;; (and-map fn lst) is like (and (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;; (or-map fn lst) is like (or (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;;

;; and-map f l
;;
;; Apply f to successive elements of l until exhaustion or f returns #f.
;; If returning early, return #f.  Otherwise, return the last value returned
;; by f.  If f has never been called because l is empty, return #t.
;;
(define (and-map f lst)
  (let loop ((result #t)
             (l lst))
    (and result
         (or (and (null? l)
                  result)
             (loop (f (car l)) (cdr l))))))

;; or-map f l
;;
;; Apply f to successive elements of l until exhaustion or while f returns #f.
;; If returning early, return the return value of f.
;;
(define (or-map f lst)
  (let loop ((result #f)
             (l lst))
    (or result
        (and (not (null? l))
             (loop (f (car l)) (cdr l))))))



;; let format alias simple-format until the more complete version is loaded

(define format simple-format)

;; this is scheme wrapping the C code so the final pred call is a tail call,
;; per SRFI-13 spec
(define (string-any char_pred s . rest)
  (let ((start (if (null? rest)
                   0 (car rest)))
        (end   (if (or (null? rest) (null? (cdr rest)))
                   (string-length s) (cadr rest))))
    (if (and (procedure? char_pred)
             (> end start)
             (<= end (string-length s))) ;; let c-code handle range error
        (or (string-any-c-code char_pred s start (1- end))
            (char_pred (string-ref s (1- end))))
        (string-any-c-code char_pred s start end))))

;; this is scheme wrapping the C code so the final pred call is a tail call,
;; per SRFI-13 spec
(define (string-every char_pred s . rest)
  (let ((start (if (null? rest)
                   0 (car rest)))
        (end   (if (or (null? rest) (null? (cdr rest)))
                   (string-length s) (cadr rest))))
    (if (and (procedure? char_pred)
             (> end start)
             (<= end (string-length s))) ;; let c-code handle range error
        (and (string-every-c-code char_pred s start (1- end))
             (char_pred (string-ref s (1- end))))
        (string-every-c-code char_pred s start end))))

;; A variant of string-fill! that we keep for compatability
;;
(define (substring-fill! str start end fill)
  (string-fill! str fill start end))



;; Define a minimal stub of the module API for psyntax, before modules
;; have booted.
(define (module-name x)
  '(guile))
(define (module-define! module sym val)
  (let ((v (hashq-ref (%get-pre-modules-obarray) sym)))
    (if v
        (variable-set! v val)
        (hashq-set! (%get-pre-modules-obarray) sym
                    (make-variable val)))))
(define (module-ref module sym)
  (let ((v (module-variable module sym)))
    (if v (variable-ref v) (error "badness!" (pk module) (pk sym)))))
(define (resolve-module . args)
  #f)

;; Input hook to syncase -- so that we might be able to pass annotated
;; expressions in. Currently disabled. Maybe we should just use
;; source-properties directly.
(define (annotation? x) #f)

;; API provided by psyntax
(define syntax-violation #f)
(define datum->syntax #f)
(define syntax->datum #f)
(define identifier? #f)
(define generate-temporaries #f)
(define bound-identifier=? #f)
(define free-identifier=? #f)
(define macroexpand #f)

;; $sc-dispatch is an implementation detail of psyntax. It is used by
;; expanded macros, to dispatch an input against a set of patterns.
(define $sc-dispatch #f)

;; Load it up!
(primitive-load-path "ice-9/psyntax-pp")

;; %pre-modules-transformer is the Scheme expander from now until the
;; module system has booted up.
(define %pre-modules-transformer macroexpand)

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ((_ x y ...) (if x (and y ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x y ...) (let ((t x)) (if t t (or y ...))))))

;; The "maybe-more" bits are something of a hack, so that we can support
;; SRFI-61. Rewrites into a standalone syntax-case macro would be
;; appreciated.
(define-syntax cond
  (syntax-rules (=> else)
    ((_ "maybe-more" test consequent)
     (if test consequent))

    ((_ "maybe-more" test consequent clause ...)
     (if test consequent (cond clause ...)))

    ((_ (else else1 else2 ...))
     (begin else1 else2 ...))

    ((_ (test => receiver) more-clause ...)
     (let ((t test))
       (cond "maybe-more" t (receiver t) more-clause ...)))

    ((_ (generator guard => receiver) more-clause ...)
     (call-with-values (lambda () generator)
       (lambda t
         (cond "maybe-more"
               (apply guard t) (apply receiver t) more-clause ...))))

    ((_ (test => receiver ...) more-clause ...)
     (syntax-violation 'cond "wrong number of receiver expressions"
                       '(test => receiver ...)))
    ((_ (generator guard => receiver ...) more-clause ...)
     (syntax-violation 'cond "wrong number of receiver expressions"
                       '(generator guard => receiver ...)))
    
    ((_ (test) more-clause ...)
     (let ((t test))
       (cond "maybe-more" t t more-clause ...)))

    ((_ (test body1 body2 ...) more-clause ...)
     (cond "maybe-more"
           test (begin body1 body2 ...) more-clause ...))))

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
         command ...)
     (letrec
       ((loop
         (lambda (var ...)
           (if test
               (begin
                 (if #f #f)
                 expr ...)
               (begin
                 command
                 ...
                 (loop (do "step" var step ...)
                       ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (make-promise (lambda () exp)))))

(include-from-path "ice-9/quasisyntax")



;;; {Defmacros}
;;;

(define-syntax define-macro
  (lambda (x)
    "Define a defmacro."
    (syntax-case x ()
      ((_ (macro . args) doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ (macro . args) body ...)
       #'(define-macro macro #f (lambda args body ...)))
      ((_ macro doc transformer)
       (or (string? (syntax->datum #'doc))
           (not (syntax->datum #'doc)))
       #'(define-syntax macro
           (lambda (y)
             doc
             #((macro-type . defmacro)
               (defmacro-args args))
             (syntax-case y ()
               ((_ . args)
                (let ((v (syntax->datum #'args)))
                  (datum->syntax y (apply transformer v)))))))))))

(define-syntax defmacro
  (lambda (x)
    "Define a defmacro, with the old lispy defun syntax."
    (syntax-case x ()
      ((_ macro args doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ macro args body ...)
       #'(define-macro macro #f (lambda args body ...))))))

(provide 'defmacro)



;;; {Deprecation}
;;;
;;; Depends on: defmacro
;;;

(defmacro begin-deprecated forms
  (if (include-deprecated-features)
      `(begin ,@forms)
      `(begin)))



;;; {Trivial Functions}
;;;

(define (identity x) x)
(define (and=> value procedure) (and value (procedure value)))
(define call/cc call-with-current-continuation)

;;; apply-to-args is functionally redundant with apply and, worse,
;;; is less general than apply since it only takes two arguments.
;;;
;;; On the other hand, apply-to-args is a syntacticly convenient way to
;;; perform binding in many circumstances when the "let" family of
;;; of forms don't cut it.  E.g.:
;;;
;;;     (apply-to-args (return-3d-mouse-coords)
;;;       (lambda (x y z)
;;;             ...))
;;;

(define (apply-to-args args fn) (apply fn args))

(defmacro false-if-exception (expr)
  `(catch #t
     (lambda ()
       ;; avoid saving backtraces inside false-if-exception
       (with-fluids ((the-last-stack (fluid-ref the-last-stack)))
         ,expr))
     (lambda args #f)))



;;; {General Properties}
;;;

;; This is a more modern interface to properties.  It will replace all
;; other property-like things eventually.

(define (make-object-property)
  (let ((prop (primitive-make-property #f)))
    (make-procedure-with-setter
     (lambda (obj) (primitive-property-ref prop obj))
     (lambda (obj val) (primitive-property-set! prop obj val)))))



;;; {Symbol Properties}
;;;

(define (symbol-property sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (and pair (cdr pair))))

(define (set-symbol-property! sym prop val)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
        (set-cdr! pair val)
        (symbol-pset! sym (acons prop val (symbol-pref sym))))))

(define (symbol-property-remove! sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
        (symbol-pset! sym (delq! pair (symbol-pref sym))))))



;;; {Arrays}
;;;

(define (array-shape a)
  (map (lambda (ind) (if (number? ind) (list 0 (+ -1 ind)) ind))
       (array-dimensions a)))



;;; {Keywords}
;;;

(define (kw-arg-ref args kw)
  (let ((rem (member kw args)))
    (and rem (pair? (cdr rem)) (cadr rem))))



;;; {Structs}
;;;

(define (struct-layout s)
  (struct-ref (struct-vtable s) vtable-index-layout))



;;; {Records}
;;;

;; Printing records: by default, records are printed as
;;
;;   #<type-name field1: val1 field2: val2 ...>
;;
;; You can change that by giving a custom printing function to
;; MAKE-RECORD-TYPE (after the list of field symbols).  This function
;; will be called like
;;
;;   (<printer> object port)
;;
;; It should print OBJECT to PORT.

(define (inherit-print-state old-port new-port)
  (if (get-print-state old-port)
      (port-with-print-state new-port (get-print-state old-port))
      new-port))

;; 0: type-name, 1: fields, 2: constructor
(define record-type-vtable
  ;; FIXME: This should just call make-vtable, not make-vtable-vtable; but for
  ;; that we need to expose the bare vtable-vtable to Scheme.
  (make-vtable-vtable "prprpw" 0
                      (lambda (s p)
                        (cond ((eq? s record-type-vtable)
                               (display "#<record-type-vtable>" p))
                              (else
                               (display "#<record-type " p)
                               (display (record-type-name s) p)
                               (display ">" p))))))

(define (record-type? obj)
  (and (struct? obj) (eq? record-type-vtable (struct-vtable obj))))

(define (make-record-type type-name fields . opt)
  ;; Pre-generate constructors for nfields < 20.
  (define-syntax make-constructor
    (lambda (x)
      (define *max-static-argument-count* 20)
      (define (make-formals n)
        (let lp ((i 0))
          (if (< i n)
              (cons (datum->syntax
                     x 
                     (string->symbol
                      (string (integer->char (+ (char->integer #\a) i)))))
                    (lp (1+ i)))
              '())))
      (syntax-case x ()
        ((_ rtd exp) (not (identifier? #'exp))
         #'(let ((n exp))
             (make-constructor rtd n)))
        ((_ rtd nfields)
         #`(case nfields
             #,@(let lp ((n 0))
                  (if (< n *max-static-argument-count*)
                      (cons (with-syntax (((formal ...) (make-formals n))
                                          (n n))
                              #'((n)
                                 (lambda (formal ...)
                                   (make-struct rtd 0 formal ...))))
                            (lp (1+ n)))
                      '()))
             (else
              (lambda args
                (if (= (length args) nfields)
                    (apply make-struct rtd 0 args)
                    (scm-error 'wrong-number-of-args
                               (format #f "make-~a" type-name)
                               "Wrong number of arguments" '() #f)))))))))

  (define (default-record-printer s p)
    (display "#<" p)
    (display (record-type-name (record-type-descriptor s)) p)
    (let loop ((fields (record-type-fields (record-type-descriptor s)))
               (off 0))
      (cond
       ((not (null? fields))
        (display " " p)
        (display (car fields) p)
        (display ": " p)
        (display (struct-ref s off) p)
        (loop (cdr fields) (+ 1 off)))))
    (display ">" p))

  (let ((rtd (make-struct record-type-vtable 0
                          (make-struct-layout
                           (apply string-append
                                  (map (lambda (f) "pw") fields)))
                          (or (and (pair? opt) (car opt))
                              default-record-printer)
                          type-name
                          (copy-tree fields))))
    (struct-set! rtd (+ vtable-offset-user 2)
                 (make-constructor rtd (length fields)))
    ;; Temporary solution: Associate a name to the record type descriptor
    ;; so that the object system can create a wrapper class for it.
    (set-struct-vtable-name! rtd (if (symbol? type-name)
                                     type-name
                                     (string->symbol type-name)))
    rtd))

(define (record-type-name obj)
  (if (record-type? obj)
      (struct-ref obj vtable-offset-user)
      (error 'not-a-record-type obj)))

(define (record-type-fields obj)
  (if (record-type? obj)
      (struct-ref obj (+ 1 vtable-offset-user))
      (error 'not-a-record-type obj)))

(define (record-constructor rtd . opt)
  (if (null? opt)
      (struct-ref rtd (+ 2 vtable-offset-user))
      (let ((field-names (car opt)))
        (primitive-eval
         `(lambda ,field-names
            (make-struct ',rtd 0 ,@(map (lambda (f)
                                          (if (memq f field-names)
                                              f
                                              #f))
                                        (record-type-fields rtd))))))))
          
(define (record-predicate rtd)
  (lambda (obj) (and (struct? obj) (eq? rtd (struct-vtable obj)))))

(define (%record-type-error rtd obj)  ;; private helper
  (or (eq? rtd (record-type-descriptor obj))
      (scm-error 'wrong-type-arg "%record-type-check"
                 "Wrong type record (want `~S'): ~S"
                 (list (record-type-name rtd) obj)
                 #f)))

(define (record-accessor rtd field-name)
  (let ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
        (error 'no-such-field field-name))
    (lambda (obj)
      (if (eq? (struct-vtable obj) rtd)
          (struct-ref obj pos)
          (%record-type-error rtd obj)))))

(define (record-modifier rtd field-name)
  (let ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
        (error 'no-such-field field-name))
    (lambda (obj val)
      (if (eq? (struct-vtable obj) rtd)
          (struct-set! obj pos val)
          (%record-type-error rtd obj)))))

(define (record? obj)
  (and (struct? obj) (record-type? (struct-vtable obj))))

(define (record-type-descriptor obj)
  (if (struct? obj)
      (struct-vtable obj)
      (error 'not-a-record obj)))

(provide 'record)



;;; {Booleans}
;;;

(define (->bool x) (not (not x)))



;;; {Symbols}
;;;

(define (symbol-append . args)
  (string->symbol (apply string-append (map symbol->string args))))

(define (list->symbol . args)
  (string->symbol (apply list->string args)))

(define (symbol . args)
  (string->symbol (apply string args)))



;;; {Lists}
;;;

(define (list-index l k)
  (let loop ((n 0)
             (l l))
    (and (not (null? l))
         (if (eq? (car l) k)
             n
             (loop (+ n 1) (cdr l))))))



(if (provided? 'posix)
    (primitive-load-path "ice-9/posix"))

(if (provided? 'socket)
    (primitive-load-path "ice-9/networking"))

;; For reference, Emacs file-exists-p uses stat in this same way.
(define file-exists?
  (if (provided? 'posix)
      (lambda (str)
        (->bool (stat str #f)))
      (lambda (str)
        (let ((port (catch 'system-error (lambda () (open-file str OPEN_READ))
                           (lambda args #f))))
          (if port (begin (close-port port) #t)
              #f)))))

(define file-is-directory?
  (if (provided? 'posix)
      (lambda (str)
        (eq? (stat:type (stat str)) 'directory))
      (lambda (str)
        (let ((port (catch 'system-error
                           (lambda () (open-file (string-append str "/.")
                                                 OPEN_READ))
                           (lambda args #f))))
          (if port (begin (close-port port) #t)
              #f)))))

(define (has-suffix? str suffix)
  (string-suffix? suffix str))

(define (system-error-errno args)
  (if (eq? (car args) 'system-error)
      (car (list-ref args 4))
      #f))



;;; {Error Handling}
;;;

(define (error . args)
  (save-stack)
  (if (null? args)
      (scm-error 'misc-error #f "?" #f #f)
      (let loop ((msg "~A")
                 (rest (cdr args)))
        (if (not (null? rest))
            (loop (string-append msg " ~S")
                  (cdr rest))
            (scm-error 'misc-error #f msg args #f)))))

;; bad-throw is the hook that is called upon a throw to a an unhandled
;; key (unless the throw has four arguments, in which case
;; it's usually interpreted as an error throw.)
;; If the key has a default handler (a throw-handler-default property),
;; it is applied to the throw.
;;
(define (bad-throw key . args)
  (let ((default (symbol-property key 'throw-handler-default)))
    (or (and default (apply default key args))
        (apply error "unhandled-exception:" key args))))



(define (tm:sec obj) (vector-ref obj 0))
(define (tm:min obj) (vector-ref obj 1))
(define (tm:hour obj) (vector-ref obj 2))
(define (tm:mday obj) (vector-ref obj 3))
(define (tm:mon obj) (vector-ref obj 4))
(define (tm:year obj) (vector-ref obj 5))
(define (tm:wday obj) (vector-ref obj 6))
(define (tm:yday obj) (vector-ref obj 7))
(define (tm:isdst obj) (vector-ref obj 8))
(define (tm:gmtoff obj) (vector-ref obj 9))
(define (tm:zone obj) (vector-ref obj 10))

(define (set-tm:sec obj val) (vector-set! obj 0 val))
(define (set-tm:min obj val) (vector-set! obj 1 val))
(define (set-tm:hour obj val) (vector-set! obj 2 val))
(define (set-tm:mday obj val) (vector-set! obj 3 val))
(define (set-tm:mon obj val) (vector-set! obj 4 val))
(define (set-tm:year obj val) (vector-set! obj 5 val))
(define (set-tm:wday obj val) (vector-set! obj 6 val))
(define (set-tm:yday obj val) (vector-set! obj 7 val))
(define (set-tm:isdst obj val) (vector-set! obj 8 val))
(define (set-tm:gmtoff obj val) (vector-set! obj 9 val))
(define (set-tm:zone obj val) (vector-set! obj 10 val))

(define (tms:clock obj) (vector-ref obj 0))
(define (tms:utime obj) (vector-ref obj 1))
(define (tms:stime obj) (vector-ref obj 2))
(define (tms:cutime obj) (vector-ref obj 3))
(define (tms:cstime obj) (vector-ref obj 4))

(define file-position ftell)
(define (file-set-position port offset . whence)
  (let ((whence (if (eq? whence '()) SEEK_SET (car whence))))
    (seek port offset whence)))

(define (move->fdes fd/port fd)
  (cond ((integer? fd/port)
         (dup->fdes fd/port fd)
         (close fd/port)
         fd)
        (else
         (primitive-move->fdes fd/port fd)
         (set-port-revealed! fd/port 1)
         fd/port)))

(define (release-port-handle port)
  (let ((revealed (port-revealed port)))
    (if (> revealed 0)
        (set-port-revealed! port (- revealed 1)))))

(define (dup->port port/fd mode . maybe-fd)
  (let ((port (fdopen (apply dup->fdes port/fd maybe-fd)
                      mode)))
    (if (pair? maybe-fd)
        (set-port-revealed! port 1))
    port))

(define (dup->inport port/fd . maybe-fd)
  (apply dup->port port/fd "r" maybe-fd))

(define (dup->outport port/fd . maybe-fd)
  (apply dup->port port/fd "w" maybe-fd))

(define (dup port/fd . maybe-fd)
  (if (integer? port/fd)
      (apply dup->fdes port/fd maybe-fd)
      (apply dup->port port/fd (port-mode port/fd) maybe-fd)))

(define (duplicate-port port modes)
  (dup->port port modes))

(define (fdes->inport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
           (let ((result (fdopen fdes "r")))
             (set-port-revealed! result 1)
             result))
          ((input-port? (car rest-ports))
           (set-port-revealed! (car rest-ports)
                               (+ (port-revealed (car rest-ports)) 1))
           (car rest-ports))
          (else
           (loop (cdr rest-ports))))))

(define (fdes->outport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
           (let ((result (fdopen fdes "w")))
             (set-port-revealed! result 1)
             result))
          ((output-port? (car rest-ports))
           (set-port-revealed! (car rest-ports)
                               (+ (port-revealed (car rest-ports)) 1))
           (car rest-ports))
          (else
           (loop (cdr rest-ports))))))

(define (port->fdes port)
  (set-port-revealed! port (+ (port-revealed port) 1))
  (fileno port))

(define (setenv name value)
  (if value
      (putenv (string-append name "=" value))
      (putenv name)))

(define (unsetenv name)
  "Remove the entry for NAME from the environment."
  (putenv name))



;;; {Load Paths}
;;;

;;; Here for backward compatability
;;
(define scheme-file-suffix (lambda () ".scm"))

(define (in-vicinity vicinity file)
  (let ((tail (let ((len (string-length vicinity)))
                (if (zero? len)
                    #f
                    (string-ref vicinity (- len 1))))))
    (string-append vicinity
                   (if (or (not tail)
                           (eq? tail #\/))
                       ""
                       "/")
                   file)))



;;; {Help for scm_shell}
;;;
;;; The argument-processing code used by Guile-based shells generates
;;; Scheme code based on the argument list.  This page contains help
;;; functions for the code it generates.
;;;

(define (command-line) (program-arguments))

;; This is mostly for the internal use of the code generated by
;; scm_compile_shell_switches.

(define (turn-on-debugging)
  (debug-enable 'debug)
  (debug-enable 'backtrace)
  (read-enable 'positions))

(define (load-user-init)
  (let* ((home (or (getenv "HOME")
                   (false-if-exception (passwd:dir (getpwuid (getuid))))
                   "/"))  ;; fallback for cygwin etc.
         (init-file (in-vicinity home ".guile")))
    (if (file-exists? init-file)
        (primitive-load init-file))))



;;; {The interpreter stack}
;;;

;; %stacks defined in stacks.c
(define (%start-stack tag thunk)
  (let ((prompt-tag (make-prompt-tag "start-stack")))
    (call-with-prompt
     prompt-tag
     (lambda ()
       (with-fluids ((%stacks (acons tag prompt-tag
                                     (or (fluid-ref %stacks) '()))))
         (thunk)))
     (lambda (k . args)
              (%start-stack tag (lambda () (apply k args)))))))
(define-syntax start-stack
  (syntax-rules ()
    ((_ tag exp)
     (%start-stack tag (lambda () exp)))))



;;; {Loading by paths}
;;;

;;; Load a Scheme source file named NAME, searching for it in the
;;; directories listed in %load-path, and applying each of the file
;;; name extensions listed in %load-extensions.
(define (load-from-path name)
  (start-stack 'load-stack
               (primitive-load-path name)))

(define %load-verbosely #f)
(define (assert-load-verbosity v) (set! %load-verbosely v))

(define (%load-announce file)
  (if %load-verbosely
      (with-output-to-port (current-error-port)
        (lambda ()
          (display ";;; ")
          (display "loading ")
          (display file)
          (newline)
          (force-output)))))

(set! %load-hook %load-announce)

(define (load name . reader)
  ;; Returns the .go file corresponding to `name'. Does not search load
  ;; paths, only the fallback path. If the .go file is missing or out of
  ;; date, and autocompilation is enabled, will try autocompilation, just
  ;; as primitive-load-path does internally. primitive-load is
  ;; unaffected. Returns #f if autocompilation failed or was disabled.
  ;;
  ;; NB: Unless we need to compile the file, this function should not cause
  ;; (system base compile) to be loaded up. For that reason compiled-file-name
  ;; partially duplicates functionality from (system base compile).
  (define (compiled-file-name canon-path)
    (and %compile-fallback-path
         (string-append
          %compile-fallback-path
          ;; no need for '/' separator here, canon-path is absolute
          canon-path
          (cond ((or (null? %load-compiled-extensions)
                     (string-null? (car %load-compiled-extensions)))
                 (warn "invalid %load-compiled-extensions"
                       %load-compiled-extensions)
                 ".go")
                (else (car %load-compiled-extensions))))))
  (define (fresh-compiled-file-name go-path)
    (catch #t
      (lambda ()
        (let* ((scmstat (stat name))
               (gostat (stat go-path #f)))
          (if (and gostat (= (stat:mtime gostat) (stat:mtime scmstat)))
              go-path
              (begin
                (if gostat
                    (format (current-error-port)
                            ";;; note: source file ~a\n;;;       newer than compiled ~a\n"
                            name go-path))
                (cond
                 (%load-should-autocompile
                  (%warn-autocompilation-enabled)
                  (format (current-error-port) ";;; compiling ~a\n" name)
                  (let ((cfn ((@ (system base compile) compile-file) name
                              #:env (current-module))))
                    (format (current-error-port) ";;; compiled ~a\n" cfn)
                    cfn))
                 (else #f))))))
      (lambda (k . args)
        (format (current-error-port)
                ";;; WARNING: compilation of ~a failed:\n;;; key ~a, throw_args ~s\n"
                name k args)
        #f)))
  (with-fluids ((current-reader (and (pair? reader) (car reader))))
    (let ((cfn (and=> (and=> (false-if-exception (canonicalize-path name))
                             compiled-file-name)
                      fresh-compiled-file-name)))
      (if cfn
          (load-compiled cfn)
          (start-stack 'load-stack
                       (primitive-load name))))))



;;; {Reader Extensions}
;;;
;;; Reader code for various "#c" forms.
;;;

(define read-eval? (make-fluid))
(fluid-set! read-eval? #f)
(read-hash-extend #\.
                  (lambda (c port)
                    (if (fluid-ref read-eval?)
                        (eval (read port) (interaction-environment))
                        (error
                         "#. read expansion found and read-eval? is #f."))))



;;; {Command Line Options}
;;;

(define (get-option argv kw-opts kw-args return)
  (cond
   ((null? argv)
    (return #f #f argv))

   ((or (not (eq? #\- (string-ref (car argv) 0)))
        (eq? (string-length (car argv)) 1))
    (return 'normal-arg (car argv) (cdr argv)))

   ((eq? #\- (string-ref (car argv) 1))
    (let* ((kw-arg-pos (or (string-index (car argv) #\=)
                           (string-length (car argv))))
           (kw (symbol->keyword (substring (car argv) 2 kw-arg-pos)))
           (kw-opt? (member kw kw-opts))
           (kw-arg? (member kw kw-args))
           (arg (or (and (not (eq? kw-arg-pos (string-length (car argv))))
                         (substring (car argv)
                                    (+ kw-arg-pos 1)
                                    (string-length (car argv))))
                    (and kw-arg?
                         (begin (set! argv (cdr argv)) (car argv))))))
      (if (or kw-opt? kw-arg?)
          (return kw arg (cdr argv))
          (return 'usage-error kw (cdr argv)))))

   (else
    (let* ((char (substring (car argv) 1 2))
           (kw (symbol->keyword char)))
      (cond

       ((member kw kw-opts)
        (let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
               (new-argv (if (= 0 (string-length rest-car))
                             (cdr argv)
                             (cons (string-append "-" rest-car) (cdr argv)))))
          (return kw #f new-argv)))

       ((member kw kw-args)
        (let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
               (arg (if (= 0 (string-length rest-car))
                        (cadr argv)
                        rest-car))
               (new-argv (if (= 0 (string-length rest-car))
                             (cddr argv)
                             (cdr argv))))
          (return kw arg new-argv)))

       (else (return 'usage-error kw argv)))))))

(define (for-next-option proc argv kw-opts kw-args)
  (let loop ((argv argv))
    (get-option argv kw-opts kw-args
                (lambda (opt opt-arg argv)
                  (and opt (proc opt opt-arg argv loop))))))

(define (display-usage-report kw-desc)
  (for-each
   (lambda (kw)
     (or (eq? (car kw) #t)
         (eq? (car kw) 'else)
         (let* ((opt-desc kw)
                (help (cadr opt-desc))
                (opts (car opt-desc))
                (opts-proper (if (string? (car opts)) (cdr opts) opts))
                (arg-name (if (string? (car opts))
                              (string-append "<" (car opts) ">")
                              ""))
                (left-part (string-append
                            (with-output-to-string
                              (lambda ()
                                (map (lambda (x) (display (keyword->symbol x)) (display " "))
                                     opts-proper)))
                            arg-name))
                (middle-part (if (and (< (string-length left-part) 30)
                                      (< (string-length help) 40))
                                 (make-string (- 30 (string-length left-part)) #\ )
                                 "\n\t")))
           (display left-part)
           (display middle-part)
           (display help)
           (newline))))
   kw-desc))



(define (transform-usage-lambda cases)
  (let* ((raw-usage (delq! 'else (map car cases)))
         (usage-sans-specials (map (lambda (x)
                                    (or (and (not (list? x)) x)
                                        (and (symbol? (car x)) #t)
                                        (and (boolean? (car x)) #t)
                                        x))
                                  raw-usage))
         (usage-desc (delq! #t usage-sans-specials))
         (kw-desc (map car usage-desc))
         (kw-opts (apply append (map (lambda (x) (and (not (string? (car x))) x)) kw-desc)))
         (kw-args (apply append (map (lambda (x) (and (string? (car x)) (cdr x))) kw-desc)))
         (transmogrified-cases (map (lambda (case)
                                      (cons (let ((opts (car case)))
                                              (if (or (boolean? opts) (eq? 'else opts))
                                                  opts
                                                  (cond
                                                   ((symbol? (car opts))  opts)
                                                   ((boolean? (car opts)) opts)
                                                   ((string? (caar opts)) (cdar opts))
                                                   (else (car opts)))))
                                            (cdr case)))
                                    cases)))
    `(let ((%display-usage (lambda () (display-usage-report ',usage-desc))))
       (lambda (%argv)
         (let %next-arg ((%argv %argv))
           (get-option %argv
                       ',kw-opts
                       ',kw-args
                       (lambda (%opt %arg %new-argv)
                         (case %opt
                           ,@ transmogrified-cases))))))))




;;; {Low Level Modules}
;;;
;;; These are the low level data structures for modules.
;;;
;;; Every module object is of the type 'module-type', which is a record
;;; consisting of the following members:
;;;
;;; - eval-closure: the function that defines for its module the strategy that
;;;   shall be followed when looking up symbols in the module.
;;;
;;;   An eval-closure is a function taking two arguments: the symbol to be
;;;   looked up and a boolean value telling whether a binding for the symbol
;;;   should be created if it does not exist yet.  If the symbol lookup
;;;   succeeded (either because an existing binding was found or because a new
;;;   binding was created), a variable object representing the binding is
;;;   returned.  Otherwise, the value #f is returned.  Note that the eval
;;;   closure does not take the module to be searched as an argument: During
;;;   construction of the eval-closure, the eval-closure has to store the
;;;   module it belongs to in its environment.  This means, that any
;;;   eval-closure can belong to only one module.
;;;
;;;   The eval-closure of a module can be defined arbitrarily.  However, three
;;;   special cases of eval-closures are to be distinguished: During startup
;;;   the module system is not yet activated.  In this phase, no modules are
;;;   defined and all bindings are automatically stored by the system in the
;;;   pre-modules-obarray.  Since no eval-closures exist at this time, the
;;;   functions which require an eval-closure as their argument need to be
;;;   passed the value #f.
;;;
;;;   The other two special cases of eval-closures are the
;;;   standard-eval-closure and the standard-interface-eval-closure.  Both
;;;   behave equally for the case that no new binding is to be created.  The
;;;   difference between the two comes in, when the boolean argument to the
;;;   eval-closure indicates that a new binding shall be created if it is not
;;;   found.
;;;
;;;   Given that no new binding shall be created, both standard eval-closures
;;;   define the following standard strategy of searching bindings in the
;;;   module: First, the module's obarray is searched for the symbol.  Second,
;;;   if no binding for the symbol was found in the module's obarray, the
;;;   module's binder procedure is exececuted.  If this procedure did not
;;;   return a binding for the symbol, the modules referenced in the module's
;;;   uses list are recursively searched for a binding of the symbol.  If the
;;;   binding can not be found in these modules also, the symbol lookup has
;;;   failed.
;;;
;;;   If a new binding shall be created, the standard-interface-eval-closure
;;;   immediately returns indicating failure.  That is, it does not even try
;;;   to look up the symbol.  In contrast, the standard-eval-closure would
;;;   first search the obarray, and if no binding was found there, would
;;;   create a new binding in the obarray, therefore not calling the binder
;;;   procedure or searching the modules in the uses list.
;;;
;;;   The explanation of the following members obarray, binder and uses
;;;   assumes that the symbol lookup follows the strategy that is defined in
;;;   the standard-eval-closure and the standard-interface-eval-closure.
;;;
;;; - obarray: a hash table that maps symbols to variable objects.  In this
;;;   hash table, the definitions are found that are local to the module (that
;;;   is, not imported from other modules).  When looking up bindings in the
;;;   module, this hash table is searched first.
;;;
;;; - binder: either #f or a function taking a module and a symbol argument.
;;;   If it is a function it is called after the obarray has been
;;;   unsuccessfully searched for a binding.  It then can provide bindings
;;;   that would otherwise not be found locally in the module.
;;;
;;; - uses: a list of modules from which non-local bindings can be inherited.
;;;   These modules are the third place queried for bindings after the obarray
;;;   has been unsuccessfully searched and the binder function did not deliver
;;;   a result either.
;;;
;;; - transformer: either #f or a function taking a scheme expression as
;;;   delivered by read.  If it is a function, it will be called to perform
;;;   syntax transformations (e. g. makro expansion) on the given scheme
;;;   expression. The output of the transformer function will then be passed
;;;   to Guile's internal memoizer.  This means that the output must be valid
;;;   scheme code.  The only exception is, that the output may make use of the
;;;   syntax extensions provided to identify the modules that a binding
;;;   belongs to.
;;;
;;; - name: the name of the module.  This is used for all kinds of printing
;;;   outputs.  In certain places the module name also serves as a way of
;;;   identification.  When adding a module to the uses list of another
;;;   module, it is made sure that the new uses list will not contain two
;;;   modules of the same name.
;;;
;;; - kind: classification of the kind of module.  The value is (currently?)
;;;   only used for printing.  It has no influence on how a module is treated.
;;;   Currently the following values are used when setting the module kind:
;;;   'module, 'directory, 'interface, 'custom-interface.  If no explicit kind
;;;   is set, it defaults to 'module.
;;;
;;; - duplicates-handlers: a list of procedures that get called to make a
;;;   choice between two duplicate bindings when name clashes occur.  See the
;;;   `duplicate-handlers' global variable below.
;;;
;;; - observers: a list of procedures that get called when the module is
;;;   modified.
;;;
;;; - weak-observers: a weak-key hash table of procedures that get called
;;;   when the module is modified.  See `module-observe-weak' for details.
;;;
;;; In addition, the module may (must?) contain a binding for
;;; `%module-public-interface'.  This variable should be bound to a module
;;; representing the exported interface of a module.  See the
;;; `module-public-interface' and `module-export!' procedures.
;;;
;;; !!! warning: The interface to lazy binder procedures is going
;;; to be changed in an incompatible way to permit all the basic
;;; module ops to be virtualized.
;;;
;;; (make-module size use-list lazy-binding-proc) => module
;;; module-{obarray,uses,binder}[|-set!]
;;; (module? obj) => [#t|#f]
;;; (module-locally-bound? module symbol) => [#t|#f]
;;; (module-bound? module symbol) => [#t|#f]
;;; (module-symbol-locally-interned? module symbol) => [#t|#f]
;;; (module-symbol-interned? module symbol) => [#t|#f]
;;; (module-local-variable module symbol) => [#<variable ...> | #f]
;;; (module-variable module symbol) => [#<variable ...> | #f]
;;; (module-symbol-binding module symbol opt-value)
;;;             => [ <obj> | opt-value | an error occurs ]
;;; (module-make-local-var! module symbol) => #<variable...>
;;; (module-add! module symbol var) => unspecified
;;; (module-remove! module symbol) =>  unspecified
;;; (module-for-each proc module) => unspecified
;;; (make-scm-module) => module ; a lazy copy of the symhash module
;;; (set-current-module module) => unspecified
;;; (current-module) => #<module...>
;;;
;;;



;;; {Printing Modules}
;;;

;; This is how modules are printed.  You can re-define it.
(define (%print-module mod port)
  (display "#<" port)
  (display (or (module-kind mod) "module") port)
  (display " " port)
  (display (module-name mod) port)
  (display " " port)
  (display (number->string (object-address mod) 16) port)
  (display ">" port))

(letrec-syntax
     ;; Locally extend the syntax to allow record accessors to be defined at
     ;; compile-time. Cache the rtd locally to the constructor, the getters and
     ;; the setters, in order to allow for redefinition of the record type; not
     ;; relevant in the case of modules, but perhaps if we make this public, it
     ;; could matter.

    ((define-record-type
       (lambda (x)
         (define (make-id scope . fragments)
           (datum->syntax #'scope
                          (apply symbol-append
                                 (map (lambda (x)
                                        (if (symbol? x) x (syntax->datum x)))
                                      fragments))))
         
         (define (getter rtd type-name field slot)
           #`(define #,(make-id rtd type-name '- field)
               (let ((rtd #,rtd))
                 (lambda (#,type-name)
                   (if (eq? (struct-vtable #,type-name) rtd)
                       (struct-ref #,type-name #,slot)
                       (%record-type-error rtd #,type-name))))))

         (define (setter rtd type-name field slot)
           #`(define #,(make-id rtd 'set- type-name '- field '!)
               (let ((rtd #,rtd))
                 (lambda (#,type-name val)
                   (if (eq? (struct-vtable #,type-name) rtd)
                       (struct-set! #,type-name #,slot val)
                       (%record-type-error rtd #,type-name))))))

         (define (accessors rtd type-name fields n exp)
           (syntax-case fields ()
             (() exp)
             (((field #:no-accessors) field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         exp))
             (((field #:no-setter) field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         #`(begin #,exp
                                  #,(getter rtd type-name #'field n))))
             (((field #:no-getter) field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         #`(begin #,exp
                                  #,(setter rtd type-name #'field n))))
             ((field field* ...) (identifier? #'field)
              (accessors rtd type-name #'(field* ...) (1+ n)
                         #`(begin #,exp
                                  #,(getter rtd type-name #'field n)
                                  #,(setter rtd type-name #'field n))))))

         (define (predicate rtd type-name fields exp)
           (accessors
            rtd type-name fields 0
            #`(begin
                #,exp
                (define (#,(make-id rtd type-name '?) obj)
                  (and (struct? obj) (eq? (struct-vtable obj) #,rtd))))))

         (define (field-list fields)
           (syntax-case fields ()
             (() '())
             (((f . opts) . rest) (identifier? #'f)
              (cons #'f (field-list #'rest)))
             ((f . rest) (identifier? #'f)
              (cons #'f (field-list #'rest)))))

         (define (constructor rtd type-name fields exp)
           (let ((ctor (make-id rtd type-name '-constructor))
                 (args (field-list fields)))
             (predicate rtd type-name fields
                        #`(begin #,exp
                                 (define #,ctor
                                   (let ((rtd #,rtd))
                                     (lambda #,args
                                       (make-struct rtd 0 #,@args))))
                                 (struct-set! #,rtd (+ vtable-offset-user 2)
                                              #,ctor)))))

         (define (type type-name printer fields)
           (define (make-layout)
             (let lp ((fields fields) (slots '()))
               (syntax-case fields ()
                 (() (datum->syntax #'here
                                    (make-struct-layout
                                     (apply string-append slots))))
                 ((_ . rest) (lp #'rest (cons "pw" slots))))))

           (let ((rtd (make-id type-name type-name '-type)))
             (constructor rtd type-name fields
                          #`(begin
                              (define #,rtd
                                (make-struct record-type-vtable 0
                                             '#,(make-layout)
                                             #,printer
                                             '#,type-name
                                             '#,(field-list fields)))
                              (set-struct-vtable-name! #,rtd '#,type-name)))))

         (syntax-case x ()
           ((_ type-name printer (field ...))
            (type #'type-name #'printer #'(field ...)))))))

  ;; module-type
  ;;
  ;; A module is characterized by an obarray in which local symbols
  ;; are interned, a list of modules, "uses", from which non-local
  ;; bindings can be inherited, and an optional lazy-binder which
  ;; is a (CLOSURE module symbol) which, as a last resort, can provide
  ;; bindings that would otherwise not be found locally in the module.
  ;;
  ;; NOTE: If you change the set of fields or their order, you also need to
  ;; change the constants in libguile/modules.h.
  ;;
  ;; NOTE: The getter `module-eval-closure' is used in libguile/modules.c.
  ;; NOTE: The getter `module-transfomer' is defined libguile/modules.c.
  ;; NOTE: The getter `module-name' is defined later, due to boot reasons.
  ;;
  (define-record-type module
    (lambda (obj port) (%print-module obj port))
    (obarray
     uses
     binder
     eval-closure
     (transformer #:no-getter)
     (name #:no-getter)
     kind
     duplicates-handlers
     (import-obarray #:no-setter)
     observers
     (weak-observers #:no-setter)
     version)))


;; make-module &opt size uses binder
;;
;; Create a new module, perhaps with a particular size of obarray,
;; initial uses list, or binding procedure.
;;
(define make-module
    (lambda args

      (define (parse-arg index default)
        (if (> (length args) index)
            (list-ref args index)
            default))

      (define %default-import-size
        ;; Typical number of imported bindings actually used by a module.
        600)

      (if (> (length args) 3)
          (error "Too many args to make-module." args))

      (let ((size (parse-arg 0 31))
            (uses (parse-arg 1 '()))
            (binder (parse-arg 2 #f)))

        (if (not (integer? size))
            (error "Illegal size to make-module." size))
        (if (not (and (list? uses)
                      (and-map module? uses)))
            (error "Incorrect use list." uses))
        (if (and binder (not (procedure? binder)))
            (error
             "Lazy-binder expected to be a procedure or #f." binder))

        (let ((module (module-constructor (make-hash-table size)
                                          uses binder #f %pre-modules-transformer
                                          #f #f #f
                                          (make-hash-table %default-import-size)
                                          '()
                                          (make-weak-key-hash-table 31) #f)))

          ;; We can't pass this as an argument to module-constructor,
          ;; because we need it to close over a pointer to the module
          ;; itself.
          (set-module-eval-closure! module (standard-eval-closure module))

          module))))




;;; {Observer protocol}
;;;

(define (module-observe module proc)
  (set-module-observers! module (cons proc (module-observers module)))
  (cons module proc))

(define (module-observe-weak module observer-id . proc)
  ;; Register PROC as an observer of MODULE under name OBSERVER-ID (which can
  ;; be any Scheme object).  PROC is invoked and passed MODULE any time
  ;; MODULE is modified.  PROC gets unregistered when OBSERVER-ID gets GC'd
  ;; (thus, it is never unregistered if OBSERVER-ID is an immediate value,
  ;; for instance).

  ;; The two-argument version is kept for backward compatibility: when called
  ;; with two arguments, the observer gets unregistered when closure PROC
  ;; gets GC'd (making it impossible to use an anonymous lambda for PROC).

  (let ((proc (if (null? proc) observer-id (car proc))))
    (hashq-set! (module-weak-observers module) observer-id proc)))

(define (module-unobserve token)
  (let ((module (car token))
        (id (cdr token)))
    (if (integer? id)
        (hash-remove! (module-weak-observers module) id)
        (set-module-observers! module (delq1! id (module-observers module)))))
  *unspecified*)

(define module-defer-observers #f)
(define module-defer-observers-mutex (make-mutex 'recursive))
(define module-defer-observers-table (make-hash-table))

(define (module-modified m)
  (if module-defer-observers
      (hash-set! module-defer-observers-table m #t)
      (module-call-observers m)))

;;; This function can be used to delay calls to observers so that they
;;; can be called once only in the face of massive updating of modules.
;;;
(define (call-with-deferred-observers thunk)
  (dynamic-wind
      (lambda ()
        (lock-mutex module-defer-observers-mutex)
        (set! module-defer-observers #t))
      thunk
      (lambda ()
        (set! module-defer-observers #f)
        (hash-for-each (lambda (m dummy)
                         (module-call-observers m))
                       module-defer-observers-table)
        (hash-clear! module-defer-observers-table)
        (unlock-mutex module-defer-observers-mutex))))

(define (module-call-observers m)
  (for-each (lambda (proc) (proc m)) (module-observers m))

  ;; We assume that weak observers don't (un)register themselves as they are
  ;; called since this would preclude proper iteration over the hash table
  ;; elements.
  (hash-for-each (lambda (id proc) (proc m)) (module-weak-observers m)))



;;; {Module Searching in General}
;;;
;;; We sometimes want to look for properties of a symbol
;;; just within the obarray of one module.  If the property
;;; holds, then it is said to hold ``locally'' as in, ``The symbol
;;; DISPLAY is locally rebound in the module `safe-guile'.''
;;;
;;;
;;; Other times, we want to test for a symbol property in the obarray
;;; of M and, if it is not found there, try each of the modules in the
;;; uses list of M.  This is the normal way of testing for some
;;; property, so we state these properties without qualification as
;;; in: ``The symbol 'fnord is interned in module M because it is
;;; interned locally in module M2 which is a member of the uses list
;;; of M.''
;;;

;; module-search fn m
;;
;; return the first non-#f result of FN applied to M and then to
;; the modules in the uses of m, and so on recursively.  If all applications
;; return #f, then so does this function.
;;
(define (module-search fn m v)
  (define (loop pos)
    (and (pair? pos)
         (or (module-search fn (car pos) v)
             (loop (cdr pos)))))
  (or (fn m v)
      (loop (module-uses m))))


;;; {Is a symbol bound in a module?}
;;;
;;; Symbol S in Module M is bound if S is interned in M and if the binding
;;; of S in M has been set to some well-defined value.
;;;

;; module-locally-bound? module symbol
;;
;; Is a symbol bound (interned and defined) locally in a given module?
;;
(define (module-locally-bound? m v)
  (let ((var (module-local-variable m v)))
    (and var
         (variable-bound? var))))

;; module-bound? module symbol
;;
;; Is a symbol bound (interned and defined) anywhere in a given module
;; or its uses?
;;
(define (module-bound? m v)
  (let ((var (module-variable m v)))
    (and var
         (variable-bound? var))))

;;; {Is a symbol interned in a module?}
;;;
;;; Symbol S in Module M is interned if S occurs in
;;; of S in M has been set to some well-defined value.
;;;
;;; It is possible to intern a symbol in a module without providing
;;; an initial binding for the corresponding variable.  This is done
;;; with:
;;;       (module-add! module symbol (make-undefined-variable))
;;;
;;; In that case, the symbol is interned in the module, but not
;;; bound there.  The unbound symbol shadows any binding for that
;;; symbol that might otherwise be inherited from a member of the uses list.
;;;

(define (module-obarray-get-handle ob key)
  ((if (symbol? key) hashq-get-handle hash-get-handle) ob key))

(define (module-obarray-ref ob key)
  ((if (symbol? key) hashq-ref hash-ref) ob key))

(define (module-obarray-set! ob key val)
  ((if (symbol? key) hashq-set! hash-set!) ob key val))

(define (module-obarray-remove! ob key)
  ((if (symbol? key) hashq-remove! hash-remove!) ob key))

;; module-symbol-locally-interned? module symbol
;;
;; is a symbol interned (not neccessarily defined) locally in a given module
;; or its uses?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-locally-interned? m v)
  (not (not (module-obarray-get-handle (module-obarray m) v))))

;; module-symbol-interned? module symbol
;;
;; is a symbol interned (not neccessarily defined) anywhere in a given module
;; or its uses?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-interned? m v)
  (module-search module-symbol-locally-interned? m v))


;;; {Mapping modules x symbols --> variables}
;;;

;; module-local-variable module symbol
;; return the local variable associated with a MODULE and SYMBOL.
;;
;;; This function is very important. It is the only function that can
;;; return a variable from a module other than the mutators that store
;;; new variables in modules.  Therefore, this function is the location
;;; of the "lazy binder" hack.
;;;
;;; If symbol is defined in MODULE, and if the definition binds symbol
;;; to a variable, return that variable object.
;;;
;;; If the symbols is not found at first, but the module has a lazy binder,
;;; then try the binder.
;;;
;;; If the symbol is not found at all, return #f.
;;;
;;; (This is now written in C, see `modules.c'.)
;;;

;;; {Mapping modules x symbols --> bindings}
;;;
;;; These are similar to the mapping to variables, except that the
;;; variable is dereferenced.
;;;

;; module-symbol-binding module symbol opt-value
;;
;; return the binding of a variable specified by name within
;; a given module, signalling an error if the variable is unbound.
;; If the OPT-VALUE is passed, then instead of signalling an error,
;; return OPT-VALUE.
;;
(define (module-symbol-local-binding m v . opt-val)
  (let ((var (module-local-variable m v)))
    (if (and var (variable-bound? var))
        (variable-ref var)
        (if (not (null? opt-val))
            (car opt-val)
            (error "Locally unbound variable." v)))))

;; module-symbol-binding module symbol opt-value
;;
;; return the binding of a variable specified by name within
;; a given module, signalling an error if the variable is unbound.
;; If the OPT-VALUE is passed, then instead of signalling an error,
;; return OPT-VALUE.
;;
(define (module-symbol-binding m v . opt-val)
  (let ((var (module-variable m v)))
    (if (and var (variable-bound? var))
        (variable-ref var)
        (if (not (null? opt-val))
            (car opt-val)
            (error "Unbound variable." v)))))




;;; {Adding Variables to Modules}
;;;

;; module-make-local-var! module symbol
;;
;; ensure a variable for V in the local namespace of M.
;; If no variable was already there, then create a new and uninitialzied
;; variable.
;;
;; This function is used in modules.c.
;;
(define (module-make-local-var! m v)
  (or (let ((b (module-obarray-ref (module-obarray m) v)))
        (and (variable? b)
             (begin
               ;; Mark as modified since this function is called when
               ;; the standard eval closure defines a binding
               (module-modified m)
               b)))

      ;; Create a new local variable.
      (let ((local-var (make-undefined-variable)))
        (module-add! m v local-var)
        local-var)))

;; module-ensure-local-variable! module symbol
;;
;; Ensure that there is a local variable in MODULE for SYMBOL.  If
;; there is no binding for SYMBOL, create a new uninitialized
;; variable.  Return the local variable.
;;
(define (module-ensure-local-variable! module symbol)
  (or (module-local-variable module symbol)
      (let ((var (make-undefined-variable)))
        (module-add! module symbol var)
        var)))

;; module-add! module symbol var
;;
;; ensure a particular variable for V in the local namespace of M.
;;
(define (module-add! m v var)
  (if (not (variable? var))
      (error "Bad variable to module-add!" var))
  (module-obarray-set! (module-obarray m) v var)
  (module-modified m))

;; module-remove!
;;
;; make sure that a symbol is undefined in the local namespace of M.
;;
(define (module-remove! m v)
  (module-obarray-remove! (module-obarray m) v)
  (module-modified m))

(define (module-clear! m)
  (hash-clear! (module-obarray m))
  (module-modified m))

;; MODULE-FOR-EACH -- exported
;;
;; Call PROC on each symbol in MODULE, with arguments of (SYMBOL VARIABLE).
;;
(define (module-for-each proc module)
  (hash-for-each proc (module-obarray module)))

(define (module-map proc module)
  (hash-map->list proc (module-obarray module)))



;;; {Low Level Bootstrapping}
;;;

;; make-root-module

;; A root module uses the pre-modules-obarray as its obarray.  This
;; special obarray accumulates all bindings that have been established
;; before the module system is fully booted.
;;
;; (The obarray continues to be used by code that has been closed over
;;  before the module system has been booted.)

(define (make-root-module)
  (let ((m (make-module 0)))
    (set-module-obarray! m (%get-pre-modules-obarray))
    m))

;; make-scm-module

;; The root interface is a module that uses the same obarray as the
;; root module.  It does not allow new definitions, tho.

(define (make-scm-module)
  (let ((m (make-module 0)))
    (set-module-obarray! m (%get-pre-modules-obarray))
    (set-module-eval-closure! m (standard-interface-eval-closure m))
    m))




;;; {Module-based Loading}
;;;

(define (save-module-excursion thunk)
  (let ((inner-module (current-module))
        (outer-module #f))
    (dynamic-wind (lambda ()
                    (set! outer-module (current-module))
                    (set-current-module inner-module)
                    (set! inner-module #f))
                  thunk
                  (lambda ()
                    (set! inner-module (current-module))
                    (set-current-module outer-module)
                    (set! outer-module #f)))))

(define basic-load load)

(define (load-module filename . reader)
  (save-module-excursion
   (lambda ()
     (let ((oldname (and (current-load-port)
                         (port-filename (current-load-port)))))
       (apply basic-load
              (if (and oldname
                       (> (string-length filename) 0)
                       (not (char=? (string-ref filename 0) #\/))
                       (not (string=? (dirname oldname) ".")))
                  (string-append (dirname oldname) "/" filename)
                  filename)
              reader)))))




;;; {MODULE-REF -- exported}
;;;

;; Returns the value of a variable called NAME in MODULE or any of its
;; used modules.  If there is no such variable, then if the optional third
;; argument DEFAULT is present, it is returned; otherwise an error is signaled.
;;
(define (module-ref module name . rest)
  (let ((variable (module-variable module name)))
    (if (and variable (variable-bound? variable))
        (variable-ref variable)
        (if (null? rest)
            (error "No variable named" name 'in module)
            (car rest)                  ; default value
            ))))

;; MODULE-SET! -- exported
;;
;; Sets the variable called NAME in MODULE (or in a module that MODULE uses)
;; to VALUE; if there is no such variable, an error is signaled.
;;
(define (module-set! module name value)
  (let ((variable (module-variable module name)))
    (if variable
        (variable-set! variable value)
        (error "No variable named" name 'in module))))

;; MODULE-DEFINE! -- exported
;;
;; Sets the variable called NAME in MODULE to VALUE; if there is no such
;; variable, it is added first.
;;
(define (module-define! module name value)
  (let ((variable (module-local-variable module name)))
    (if variable
        (begin
          (variable-set! variable value)
          (module-modified module))
        (let ((variable (make-variable value)))
          (module-add! module name variable)))))

;; MODULE-DEFINED? -- exported
;;
;; Return #t iff NAME is defined in MODULE (or in a module that MODULE
;; uses)
;;
(define (module-defined? module name)
  (let ((variable (module-variable module name)))
    (and variable (variable-bound? variable))))

;; MODULE-USE! module interface
;;
;; Add INTERFACE to the list of interfaces used by MODULE.
;;
(define (module-use! module interface)
  (if (not (or (eq? module interface)
               (memq interface (module-uses module))))
      (begin
        ;; Newly used modules must be appended rather than consed, so that
        ;; `module-variable' traverses the use list starting from the first
        ;; used module.
        (set-module-uses! module
                          (append (filter (lambda (m)
                                            (not
                                             (equal? (module-name m)
                                                     (module-name interface))))
                                          (module-uses module))
                                  (list interface)))
        (hash-clear! (module-import-obarray module))
        (module-modified module))))

;; MODULE-USE-INTERFACES! module interfaces
;;
;; Same as MODULE-USE! but add multiple interfaces and check for duplicates
;;
(define (module-use-interfaces! module interfaces)
  (set-module-uses! module
                    (append (module-uses module) interfaces))
  (hash-clear! (module-import-obarray module))
  (module-modified module))



;;; {Recursive Namespaces}
;;;
;;; A hierarchical namespace emerges if we consider some module to be
;;; root, and variables bound to modules as nested namespaces.
;;;
;;; The routines in this file manage variable names in hierarchical namespace.
;;; Each variable name is a list of elements, looked up in successively nested
;;; modules.
;;;
;;;             (nested-ref some-root-module '(foo bar baz))
;;;             => <value of a variable named baz in the module bound to bar in
;;;                 the module bound to foo in some-root-module>
;;;
;;;
;;; There are:
;;;
;;;     ;; a-root is a module
;;;     ;; name is a list of symbols
;;;
;;;     nested-ref a-root name
;;;     nested-set! a-root name val
;;;     nested-define! a-root name val
;;;     nested-remove! a-root name
;;;
;;;
;;; (current-module) is a natural choice for a-root so for convenience there are
;;; also:
;;;
;;;     local-ref name          ==      nested-ref (current-module) name
;;;     local-set! name val     ==      nested-set! (current-module) name val
;;;     local-define name val   ==      nested-define! (current-module) name val
;;;     local-remove name       ==      nested-remove! (current-module) name
;;;


(define (nested-ref root names)
  (let loop ((cur root)
             (elts names))
    (cond
     ((null? elts)              cur)
     ((not (module? cur))       #f)
     (else (loop (module-ref cur (car elts) #f) (cdr elts))))))

(define (nested-set! root names val)
  (let loop ((cur root)
             (elts names))
    (if (null? (cdr elts))
        (module-set! cur (car elts) val)
        (loop (module-ref cur (car elts)) (cdr elts)))))

(define (nested-define! root names val)
  (let loop ((cur root)
             (elts names))
    (if (null? (cdr elts))
        (module-define! cur (car elts) val)
        (loop (module-ref cur (car elts)) (cdr elts)))))

(define (nested-remove! root names)
  (let loop ((cur root)
             (elts names))
    (if (null? (cdr elts))
        (module-remove! cur (car elts))
        (loop (module-ref cur (car elts)) (cdr elts)))))

(define (local-ref names) (nested-ref (current-module) names))
(define (local-set! names val) (nested-set! (current-module) names val))
(define (local-define names val) (nested-define! (current-module) names val))
(define (local-remove names) (nested-remove! (current-module) names))




;;; {The (%app) module}
;;;
;;; The root of conventionally named objects not directly in the top level.
;;;
;;; (%app modules)
;;; (%app modules guile)
;;;
;;; The directory of all modules and the standard root module.
;;;

;; module-public-interface is defined in C.
(define (set-module-public-interface! m i)
  (module-define! m '%module-public-interface i))
(define (set-system-module! m s)
  (set-procedure-property! (module-eval-closure m) 'system-module s))
(define the-root-module (make-root-module))
(define the-scm-module (make-scm-module))
(set-module-public-interface! the-root-module the-scm-module)
(set-module-name! the-root-module '(guile))
(set-module-name! the-scm-module '(guile))
(set-module-kind! the-scm-module 'interface)
(set-system-module! the-root-module #t)
(set-system-module! the-scm-module #t)

;; NOTE: This binding is used in libguile/modules.c.
;;
(define (make-modules-in module name)
  (if (null? name)
      module
      (make-modules-in
       (let* ((var (module-local-variable module (car name)))
              (val (and var (variable-bound? var) (variable-ref var))))
         (if (module? val)
             val
             (let ((m (make-module 31)))
               (set-module-kind! m 'directory)
               (set-module-name! m (append (module-name module)
                                           (list (car name))))
               (module-define! module (car name) m)
               m)))
       (cdr name))))

(define (beautify-user-module! module)
  (let ((interface (module-public-interface module)))
    (if (or (not interface)
            (eq? interface module))
        (let ((interface (make-module 31)))
          (set-module-name! interface (module-name module))
          (set-module-version! interface (module-version module))
          (set-module-kind! interface 'interface)
          (set-module-public-interface! module interface))))
  (if (and (not (memq the-scm-module (module-uses module)))
           (not (eq? module the-root-module)))
      ;; Import the default set of bindings (from the SCM module) in MODULE.
      (module-use! module the-scm-module)))

(define (version-matches? version-ref target)
  (define (any pred lst)
    (and (not (null? lst)) (or (pred (car lst)) (any pred (cdr lst)))))
  (define (every pred lst) 
    (or (null? lst) (and (pred (car lst)) (every pred (cdr lst)))))
  (define (sub-versions-match? v-refs t)
    (define (sub-version-matches? v-ref t)
      (define (curried-sub-version-matches? v)
        (sub-version-matches? v t))
      (cond ((number? v-ref) (eqv? v-ref t))
            ((list? v-ref)
             (let ((cv (car v-ref)))
               (cond ((eq? cv '>=) (>= t (cadr v-ref)))
                     ((eq? cv '<=) (<= t (cadr v-ref)))
                     ((eq? cv 'and) 
                      (every curried-sub-version-matches? (cdr v-ref)))
                     ((eq? cv 'or)
                      (any curried-sub-version-matches? (cdr v-ref)))
                     ((eq? cv 'not) (not (sub-version-matches? (cadr v-ref) t)))
                     (else (error "Incompatible sub-version reference" cv)))))
            (else (error "Incompatible sub-version reference" v-ref))))
    (or (null? v-refs)
        (and (not (null? t))
             (sub-version-matches? (car v-refs) (car t))
             (sub-versions-match? (cdr v-refs) (cdr t)))))
  (define (curried-version-matches? v)
    (version-matches? v target))
  (or (null? version-ref)
      (let ((cv (car version-ref)))
        (cond ((eq? cv 'and) (every curried-version-matches? (cdr version-ref)))
              ((eq? cv 'or) (any curried-version-matches? (cdr version-ref)))
              ((eq? cv 'not) (not (version-matches? (cadr version-ref) target)))
              (else (sub-versions-match? version-ref target))))))

(define (find-versioned-module dir-hint name version-ref roots)
  (define (subdir-pair-less pair1 pair2)
    (define (numlist-less lst1 lst2)
      (or (null? lst2) 
          (and (not (null? lst1))
               (cond ((> (car lst1) (car lst2)) #t)
                     ((< (car lst1) (car lst2)) #f)
                     (else (numlist-less (cdr lst1) (cdr lst2)))))))
    (numlist-less (car pair1) (car pair2)))
  (define (match-version-and-file pair)
    (and (version-matches? version-ref (car pair))
         (let ((filenames                            
                (filter (lambda (file)
                          (let ((s (false-if-exception (stat file))))
                            (and s (eq? (stat:type s) 'regular))))
                        (map (lambda (ext)
                               (string-append (cdr pair) "/" name ext))
                             %load-extensions))))
           (and (not (null? filenames))
                (cons (car pair) (car filenames))))))
    
  (define (match-version-recursive root-pairs leaf-pairs)
    (define (filter-subdirs root-pairs ret)
      (define (filter-subdir root-pair dstrm subdir-pairs)
        (let ((entry (readdir dstrm)))
          (if (eof-object? entry)
              subdir-pairs
              (let* ((subdir (string-append (cdr root-pair) "/" entry))
                     (num (string->number entry))
                     (num (and num (append (car root-pair) (list num)))))
                (if (and num (eq? (stat:type (stat subdir)) 'directory))
                    (filter-subdir 
                     root-pair dstrm (cons (cons num subdir) subdir-pairs))
                    (filter-subdir root-pair dstrm subdir-pairs))))))
      
      (or (and (null? root-pairs) ret)
          (let* ((rp (car root-pairs))
                 (dstrm (false-if-exception (opendir (cdr rp)))))
            (if dstrm
                (let ((subdir-pairs (filter-subdir rp dstrm '())))
                  (closedir dstrm)
                  (filter-subdirs (cdr root-pairs) 
                                  (or (and (null? subdir-pairs) ret)
                                      (append ret subdir-pairs))))
                (filter-subdirs (cdr root-pairs) ret)))))
    
    (or (and (null? root-pairs) leaf-pairs)
        (let ((matching-subdir-pairs (filter-subdirs root-pairs '())))
          (match-version-recursive
           matching-subdir-pairs
           (append leaf-pairs (filter pair? (map match-version-and-file 
                                                 matching-subdir-pairs)))))))
  (define (make-root-pair root)
    (cons '() (string-append root "/" dir-hint)))

  (let* ((root-pairs (map make-root-pair roots))
         (matches (if (null? version-ref) 
                      (filter pair? (map match-version-and-file root-pairs))
                      '()))
         (matches (append matches (match-version-recursive root-pairs '()))))
    (and (null? matches) (error "No matching modules found."))
    (cdar (sort matches subdir-pair-less))))

(define (make-fresh-user-module)
  (let ((m (make-module)))
    (beautify-user-module! m)
    m))

;; NOTE: This binding is used in libguile/modules.c.
;;
(define resolve-module
  (let ((the-root-module the-root-module))
    (lambda (name . args)
      (if (equal? name '(guile))
          the-root-module
          (let ((full-name (append '(%app modules) name)))
            (let* ((already (nested-ref the-root-module full-name))
                   (numargs (length args))
                   (autoload (or (= numargs 0) (car args)))
                   (version (and (> numargs 1) (cadr args))))
              (cond
               ((and already (module? already)
                     (or (not autoload) (module-public-interface already)))
                ;; A hit, a palpable hit.
                (if (and version 
                         (not (version-matches? version (module-version already))))
                    (error "incompatible module version already loaded" name))
                already)
               (autoload
                ;; Try to autoload the module, and recurse.
                (try-load-module name version)
                (resolve-module name #f))
               (else
                ;; A module is not bound (but maybe something else is),
                ;; we're not autoloading -- here's the weird semantics,
                ;; we create an empty module.
                (make-modules-in the-root-module full-name)))))))))

;; Cheat.  These bindings are needed by modules.c, but we don't want
;; to move their real definition here because that would be unnatural.
;;
(define try-module-autoload #f)
(define process-define-module #f)
(define process-use-modules #f)
(define module-export! #f)
(define default-duplicate-binding-procedures #f)

(define %app (make-module 31))
(set-module-name! %app '(%app))

(let ((m (make-module 31)))
  (set-module-name! m '())
  (local-define '(%app modules) m))
(local-define '(%app modules guile) the-root-module)

;; This boots the module system.  All bindings needed by modules.c
;; must have been defined by now.
;;
(set-current-module the-root-module)
;; definition deferred for syncase's benefit.
(define module-name
  (let ((accessor (record-accessor module-type 'name)))
    (lambda (mod)
      (or (accessor mod)
          (let ((name (list (gensym))))
            ;; Name MOD and bind it in THE-ROOT-MODULE so that it's visible
            ;; to `resolve-module'.  This is important as `psyntax' stores
            ;; module names and relies on being able to `resolve-module'
            ;; them.
            (set-module-name! mod name)
            (nested-define! the-root-module `(%app modules ,@name) mod)
            (accessor mod))))))

;; (define-special-value '(%app modules new-ws) (lambda () (make-scm-module)))

(define (try-load-module name version)
  (try-module-autoload name version))

(define (purify-module! module)
  "Removes bindings in MODULE which are inherited from the (guile) module."
  (let ((use-list (module-uses module)))
    (if (and (pair? use-list)
             (eq? (car (last-pair use-list)) the-scm-module))
        (set-module-uses! module (reverse (cdr (reverse use-list)))))))

;; Return a module that is an interface to the module designated by
;; NAME.
;;
;; `resolve-interface' takes four keyword arguments:
;;
;;   #:select SELECTION
;;
;; SELECTION is a list of binding-specs to be imported; A binding-spec
;; is either a symbol or a pair of symbols (ORIG . SEEN), where ORIG
;; is the name in the used module and SEEN is the name in the using
;; module.  Note that SEEN is also passed through RENAMER, below.  The
;; default is to select all bindings.  If you specify no selection but
;; a renamer, only the bindings that already exist in the used module
;; are made available in the interface.  Bindings that are added later
;; are not picked up.
;;
;;   #:hide BINDINGS
;;
;; BINDINGS is a list of bindings which should not be imported.
;;
;;   #:prefix PREFIX
;;
;; PREFIX is a symbol that will be appended to each exported name.
;; The default is to not perform any renaming.
;;
;;   #:renamer RENAMER
;;
;; RENAMER is a procedure that takes a symbol and returns its new
;; name.  The default is not perform any renaming.
;;
;; Signal "no code for module" error if module name is not resolvable
;; or its public interface is not available.  Signal "no binding"
;; error if selected binding does not exist in the used module.
;;
(define (resolve-interface name . args)

  (define (get-keyword-arg args kw def)
    (cond ((memq kw args)
           => (lambda (kw-arg)
                (if (null? (cdr kw-arg))
                    (error "keyword without value: " kw))
                (cadr kw-arg)))
          (else
           def)))

  (let* ((select (get-keyword-arg args #:select #f))
         (hide (get-keyword-arg args #:hide '()))
         (renamer (or (get-keyword-arg args #:renamer #f)
                      (let ((prefix (get-keyword-arg args #:prefix #f)))
                        (and prefix (symbol-prefix-proc prefix)))
                      identity))
         (version (get-keyword-arg args #:version #f))
         (module (resolve-module name #t version))
         (public-i (and module (module-public-interface module))))
    (and (or (not module) (not public-i))
         (error "no code for module" name))
    (if (and (not select) (null? hide) (eq? renamer identity))
        public-i
        (let ((selection (or select (module-map (lambda (sym var) sym)
                                                public-i)))
              (custom-i (make-module 31)))
          (set-module-kind! custom-i 'custom-interface)
          (set-module-name! custom-i name)
          ;; XXX - should use a lazy binder so that changes to the
          ;; used module are picked up automatically.
          (for-each (lambda (bspec)
                      (let* ((direct? (symbol? bspec))
                             (orig (if direct? bspec (car bspec)))
                             (seen (if direct? bspec (cdr bspec)))
                             (var (or (module-local-variable public-i orig)
                                      (module-local-variable module orig)
                                      (error
                                       ;; fixme: format manually for now
                                       (simple-format
                                        #f "no binding `~A' in module ~A"
                                        orig name)))))
                        (if (memq orig hide)
                            (set! hide (delq! orig hide))
                            (module-add! custom-i
                                         (renamer seen)
                                         var))))
                    selection)
          ;; Check that we are not hiding bindings which don't exist
          (for-each (lambda (binding)
                      (if (not (module-local-variable public-i binding))
                          (error
                           (simple-format
                            #f "no binding `~A' to hide in module ~A"
                            binding name))))
                    hide)
          custom-i))))

(define (symbol-prefix-proc prefix)
  (lambda (symbol)
    (symbol-append prefix symbol)))

;; This function is called from "modules.c".  If you change it, be
;; sure to update "modules.c" as well.

(define (process-define-module args)
  (let* ((module-id (car args))
         (module (resolve-module module-id #f))
         (kws (cdr args))
         (unrecognized (lambda (arg)
                         (error "unrecognized define-module argument" arg))))
    (beautify-user-module! module)
    (let loop ((kws kws)
               (reversed-interfaces '())
               (exports '())
               (re-exports '())
               (replacements '())
               (autoloads '()))

      (if (null? kws)
          (call-with-deferred-observers
           (lambda ()
             (module-use-interfaces! module (reverse reversed-interfaces))
             (module-export! module exports)
             (module-replace! module replacements)
             (module-re-export! module re-exports)
             (if (not (null? autoloads))
                 (apply module-autoload! module autoloads))))
          (case (car kws)
            ((#:use-module #:use-syntax)
             (or (pair? (cdr kws))
                 (unrecognized kws))
             (cond
              ((equal? (caadr kws) '(ice-9 syncase))
               (issue-deprecation-warning
                "(ice-9 syncase) is deprecated. Support for syntax-case is now in Guile core.")
               (loop (cddr kws)
                     reversed-interfaces
                     exports
                     re-exports
                     replacements
                     autoloads))
              (else
               (let* ((interface-args (cadr kws))
                      (interface (apply resolve-interface interface-args)))
                 (and (eq? (car kws) #:use-syntax)
                      (or (symbol? (caar interface-args))
                          (error "invalid module name for use-syntax"
                                 (car interface-args)))
                      (set-module-transformer!
                       module
                       (module-ref interface
                                   (car (last-pair (car interface-args)))
                                   #f)))
                 (loop (cddr kws)
                       (cons interface reversed-interfaces)
                       exports
                       re-exports
                       replacements
                       autoloads)))))
            ((#:autoload)
             (or (and (pair? (cdr kws)) (pair? (cddr kws)))
                 (unrecognized kws))
             (loop (cdddr kws)
                   reversed-interfaces
                   exports
                   re-exports
                   replacements
                   (let ((name (cadr kws))
                         (bindings (caddr kws)))
                     (cons* name bindings autoloads))))
            ((#:no-backtrace)
             (set-system-module! module #t)
             (loop (cdr kws) reversed-interfaces exports re-exports
                   replacements autoloads))
            ((#:pure)
             (purify-module! module)
             (loop (cdr kws) reversed-interfaces exports re-exports
                   replacements autoloads))
            ((#:version)
             (or (pair? (cdr kws))
                 (unrecognized kws))
             (let ((version (cadr kws)))
               (set-module-version! module version)
               (set-module-version! (module-public-interface module) version))
             (loop (cddr kws) reversed-interfaces exports re-exports
                   replacements autoloads))
            ((#:duplicates)
             (if (not (pair? (cdr kws)))
                 (unrecognized kws))
             (set-module-duplicates-handlers!
              module
              (lookup-duplicates-handlers (cadr kws)))
             (loop (cddr kws) reversed-interfaces exports re-exports
                   replacements autoloads))
            ((#:export #:export-syntax)
             (or (pair? (cdr kws))
                 (unrecognized kws))
             (loop (cddr kws)
                   reversed-interfaces
                   (append (cadr kws) exports)
                   re-exports
                   replacements
                   autoloads))
            ((#:re-export #:re-export-syntax)
             (or (pair? (cdr kws))
                 (unrecognized kws))
             (loop (cddr kws)
                   reversed-interfaces
                   exports
                   (append (cadr kws) re-exports)
                   replacements
                   autoloads))
            ((#:replace #:replace-syntax)
             (or (pair? (cdr kws))
                 (unrecognized kws))
             (loop (cddr kws)
                   reversed-interfaces
                   exports
                   re-exports
                   (append (cadr kws) replacements)
                   autoloads))
            (else
             (unrecognized kws)))))
    (run-hook module-defined-hook module)
    module))

;; `module-defined-hook' is a hook that is run whenever a new module
;; is defined.  Its members are called with one argument, the new
;; module.
(define module-defined-hook (make-hook 1))



;;; {Autoload}
;;;

(define (make-autoload-interface module name bindings)
  (let ((b (lambda (a sym definep)
             (and (memq sym bindings)
                  (let ((i (module-public-interface (resolve-module name))))
                    (if (not i)
                        (error "missing interface for module" name))
                    (let ((autoload (memq a (module-uses module))))
                      ;; Replace autoload-interface with actual interface if
                      ;; that has not happened yet.
                      (if (pair? autoload)
                          (set-car! autoload i)))
                    (module-local-variable i sym))))))
    (module-constructor (make-hash-table 0) '() b #f #f name 'autoload #f
                        (make-hash-table 0) '() (make-weak-value-hash-table 31) #f)))

(define (module-autoload! module . args)
  "Have @var{module} automatically load the module named @var{name} when one
of the symbols listed in @var{bindings} is looked up.  @var{args} should be a
list of module-name/binding-list pairs, e.g., as in @code{(module-autoload!
module '(ice-9 q) '(make-q q-length))}."
  (let loop ((args args))
    (cond ((null? args)
           #t)
          ((null? (cdr args))
           (error "invalid name+binding autoload list" args))
          (else
           (let ((name     (car args))
                 (bindings (cadr args)))
             (module-use! module (make-autoload-interface module
                                                          name bindings))
             (loop (cddr args)))))))




;;; {Autoloading modules}
;;;

(define autoloads-in-progress '())

;; This function is called from "modules.c".  If you change it, be
;; sure to update "modules.c" as well.

(define (try-module-autoload module-name . args)
  (let* ((reverse-name (reverse module-name))
         (name (symbol->string (car reverse-name)))
         (version (and (not (null? args)) (car args)))
         (dir-hint-module-name (reverse (cdr reverse-name)))
         (dir-hint (apply string-append
                          (map (lambda (elt)
                                 (string-append (symbol->string elt) "/"))
                               dir-hint-module-name))))
    (resolve-module dir-hint-module-name #f)
    (and (not (autoload-done-or-in-progress? dir-hint name))
         (let ((didit #f))
           (dynamic-wind
            (lambda () (autoload-in-progress! dir-hint name))
            (lambda ()
              (with-fluids ((current-reader #f))
                (save-module-excursion
                 (lambda () 
                   (if version
                       (load (find-versioned-module
                              dir-hint name version %load-path))
                       (primitive-load-path (in-vicinity dir-hint name) #f))
                   (set! didit #t)))))
            (lambda () (set-autoloaded! dir-hint name didit)))
           didit))))



;;; {Dynamic linking of modules}
;;;

(define autoloads-done '((guile . guile)))

(define (autoload-done-or-in-progress? p m)
  (let ((n (cons p m)))
    (->bool (or (member n autoloads-done)
                (member n autoloads-in-progress)))))

(define (autoload-done! p m)
  (let ((n (cons p m)))
    (set! autoloads-in-progress
          (delete! n autoloads-in-progress))
    (or (member n autoloads-done)
        (set! autoloads-done (cons n autoloads-done)))))

(define (autoload-in-progress! p m)
  (let ((n (cons p m)))
    (set! autoloads-done
          (delete! n autoloads-done))
    (set! autoloads-in-progress (cons n autoloads-in-progress))))

(define (set-autoloaded! p m done?)
  (if done?
      (autoload-done! p m)
      (let ((n (cons p m)))
        (set! autoloads-done (delete! n autoloads-done))
        (set! autoloads-in-progress (delete! n autoloads-in-progress)))))



;;; {Run-time options}
;;;

(defmacro define-option-interface (option-group)
  (let* ((option-name 'car)
         (option-value 'cadr)
         (option-documentation 'caddr)

         ;; Below follow the macros defining the run-time option interfaces.

         (make-options (lambda (interface)
                         `(lambda args
                            (cond ((null? args) (,interface))
                                  ((list? (car args))
                                   (,interface (car args)) (,interface))
                                  (else (for-each
                                         (lambda (option)
                                           (display (,option-name option))
                                           (if (< (string-length
                                                   (symbol->string (,option-name option)))
                                                  8)
                                               (display #\tab))
                                           (display #\tab)
                                           (display (,option-value option))
                                           (display #\tab)
                                           (display (,option-documentation option))
                                           (newline))
                                         (,interface #t)))))))

         (make-enable (lambda (interface)
                        `(lambda flags
                           (,interface (append flags (,interface)))
                           (,interface))))

         (make-disable (lambda (interface)
                         `(lambda flags
                            (let ((options (,interface)))
                              (for-each (lambda (flag)
                                          (set! options (delq! flag options)))
                                        flags)
                              (,interface options)
                              (,interface))))))
    (let* ((interface (car option-group))
           (options/enable/disable (cadr option-group)))
      `(begin
         (define ,(car options/enable/disable)
           ,(make-options interface))
         (define ,(cadr options/enable/disable)
           ,(make-enable interface))
         (define ,(caddr options/enable/disable)
           ,(make-disable interface))
         (defmacro ,(caaddr option-group) (opt val)
           `(,',(car options/enable/disable)
             (append (,',(car options/enable/disable))
                     (list ',opt ,val))))))))

(define-option-interface
  (eval-options-interface
   (eval-options eval-enable eval-disable)
   (eval-set!)))

(define-option-interface
  (debug-options-interface
   (debug-options debug-enable debug-disable)
   (debug-set!)))

(define-option-interface
  (evaluator-traps-interface
   (traps trap-enable trap-disable)
   (trap-set!)))

(define-option-interface
  (read-options-interface
   (read-options read-enable read-disable)
   (read-set!)))

(define-option-interface
  (print-options-interface
   (print-options print-enable print-disable)
   (print-set!)))



;;; {Running Repls}
;;;

(define (repl read evaler print)
  (let loop ((source (read (current-input-port))))
    (print (evaler source))
    (loop (read (current-input-port)))))

;; A provisional repl that acts like the SCM repl:
;;
(define scm-repl-silent #f)
(define (assert-repl-silence v) (set! scm-repl-silent v))

(define *unspecified* (if #f #f))
(define (unspecified? v) (eq? v *unspecified*))

(define scm-repl-print-unspecified #f)
(define (assert-repl-print-unspecified v) (set! scm-repl-print-unspecified v))

(define scm-repl-verbose #f)
(define (assert-repl-verbosity v) (set! scm-repl-verbose v))

(define scm-repl-prompt "guile> ")

(define (set-repl-prompt! v) (set! scm-repl-prompt v))

(define (default-pre-unwind-handler key . args)
  ;; Narrow by two more frames: this one, and the throw handler.
  (save-stack 2)
  (apply throw key args))

(begin-deprecated
 (define (pre-unwind-handler-dispatch key . args)
   (apply default-pre-unwind-handler key args)))

(define abort-hook (make-hook))

;; these definitions are used if running a script.
;; otherwise redefined in error-catching-loop.
(define (set-batch-mode?! arg) #t)
(define (batch-mode?) #t)

(define (error-catching-loop thunk)
  (let ((status #f)
        (interactive #t))
    (define (loop first)
      (let ((next
             (catch #t

                    (lambda ()
                      (call-with-unblocked-asyncs
                       (lambda ()
                         (with-traps
                          (lambda ()
                            (first)

                            ;; This line is needed because mark
                            ;; doesn't do closures quite right.
                            ;; Unreferenced locals should be
                            ;; collected.
                            (set! first #f)
                            (let loop ((v (thunk)))
                              (loop (thunk)))
                            #f)))))

                    (lambda (key . args)
                      (case key
                        ((quit)
                         (set! status args)
                         #f)

                        ((switch-repl)
                         (apply throw 'switch-repl args))

                        ((abort)
                         ;; This is one of the closures that require
                         ;; (set! first #f) above
                         ;;
                         (lambda ()
                           (run-hook abort-hook)
                           (force-output (current-output-port))
                           (display "ABORT: "  (current-error-port))
                           (write args (current-error-port))
                           (newline (current-error-port))
                           (if interactive
                               (begin
                                 (if (and
                                      (not has-shown-debugger-hint?)
                                      (not (memq 'backtrace
                                                 (debug-options-interface)))
                                      (stack? (fluid-ref the-last-stack)))
                                     (begin
                                       (newline (current-error-port))
                                       (display
                                        "Type \"(backtrace)\" to get more information or \"(debug)\" to enter the debugger.\n"
                                        (current-error-port))
                                       (set! has-shown-debugger-hint? #t)))
                                 (force-output (current-error-port)))
                               (begin
                                 (primitive-exit 1)))
                           (set! stack-saved? #f)))

                        (else
                         ;; This is the other cons-leak closure...
                         (lambda ()
                           (cond ((= (length args) 4)
                                  (apply handle-system-error key args))
                                 (else
                                  (apply bad-throw key args)))))))

                    default-pre-unwind-handler)))

        (if next (loop next) status)))
    (set! set-batch-mode?! (lambda (arg)
                             (cond (arg
                                    (set! interactive #f)
                                    (restore-signals))
                                   (#t
                                    (error "sorry, not implemented")))))
    (set! batch-mode? (lambda () (not interactive)))
    (call-with-blocked-asyncs
     (lambda () (loop (lambda () #t))))))

;;(define the-last-stack (make-fluid)) Defined by scm_init_backtrace ()
(define before-signal-stack (make-fluid))
;; FIXME: stack-saved? is broken in the presence of threads.
(define stack-saved? #f)

(define (save-stack . narrowing)
  (if (not stack-saved?)
      (begin
        (let ((stacks (fluid-ref %stacks)))
          (fluid-set! the-last-stack
                      ;; (make-stack obj inner outer inner outer ...)
                      ;;
                      ;; In this case, cut away the make-stack frame, the
                      ;; save-stack frame, and then narrow as specified by the
                      ;; user, delimited by the nearest start-stack invocation,
                      ;; if any.
                      (apply make-stack #t
                             2
                             (if (pair? stacks) (cdar stacks) 0)
                             narrowing)))
        (set! stack-saved? #t))))

(define before-error-hook (make-hook))
(define after-error-hook (make-hook))
(define before-backtrace-hook (make-hook))
(define after-backtrace-hook (make-hook))

(define has-shown-debugger-hint? #f)

(define (handle-system-error key . args)
  (let ((cep (current-error-port)))
    (cond ((not (stack? (fluid-ref the-last-stack))))
          ((memq 'backtrace (debug-options-interface))
           (let ((highlights (if (or (eq? key 'wrong-type-arg)
                                     (eq? key 'out-of-range))
                                 (list-ref args 3)
                                 '())))
             (run-hook before-backtrace-hook)
             (newline cep)
             (display "Backtrace:\n")
             (display-backtrace (fluid-ref the-last-stack) cep
                                #f #f highlights)
             (newline cep)
             (run-hook after-backtrace-hook))))
    (run-hook before-error-hook)
    (apply display-error (fluid-ref the-last-stack) cep args)
    (run-hook after-error-hook)
    (force-output cep)
    (throw 'abort key)))

(define (quit . args)
  (apply throw 'quit args))

(define exit quit)

;;(define has-shown-backtrace-hint? #f) Defined by scm_init_backtrace ()

;; Replaced by C code:
;;(define (backtrace)
;;  (if (fluid-ref the-last-stack)
;;      (begin
;;      (newline)
;;      (display-backtrace (fluid-ref the-last-stack) (current-output-port))
;;      (newline)
;;      (if (and (not has-shown-backtrace-hint?)
;;               (not (memq 'backtrace (debug-options-interface))))
;;          (begin
;;            (display
;;"Type \"(debug-enable 'backtrace)\" if you would like a backtrace
;;automatically if an error occurs in the future.\n")
;;            (set! has-shown-backtrace-hint? #t))))
;;      (display "No backtrace available.\n")))

(define (error-catching-repl r e p)
  (error-catching-loop
   (lambda ()
     (call-with-values (lambda () (e (r)))
       (lambda the-values (for-each p the-values))))))

(define (gc-run-time)
  (cdr (assq 'gc-time-taken (gc-stats))))

(define before-read-hook (make-hook))
(define after-read-hook (make-hook))
(define before-eval-hook (make-hook 1))
(define after-eval-hook (make-hook 1))
(define before-print-hook (make-hook 1))
(define after-print-hook (make-hook 1))

;;; The default repl-reader function.  We may override this if we've
;;; the readline library.
(define repl-reader
  (lambda (prompt . reader)
    (if (not (char-ready?))
        (display (if (string? prompt) prompt (prompt))))
    (force-output)
    (run-hook before-read-hook)
    ((or (and (pair? reader) (car reader))
         (fluid-ref current-reader)
         read)
     (current-input-port))))

(define (scm-style-repl)

  (letrec (
           (start-gc-rt #f)
           (start-rt #f)
           (repl-report-start-timing (lambda ()
                                       (set! start-gc-rt (gc-run-time))
                                       (set! start-rt (get-internal-run-time))))
           (repl-report (lambda ()
                          (display ";;; ")
                          (display (inexact->exact
                                    (* 1000 (/ (- (get-internal-run-time) start-rt)
                                               internal-time-units-per-second))))
                          (display "  msec  (")
                          (display  (inexact->exact
                                     (* 1000 (/ (- (gc-run-time) start-gc-rt)
                                                internal-time-units-per-second))))
                          (display " msec in gc)\n")))

           (consume-trailing-whitespace
            (lambda ()
              (let ((ch (peek-char)))
                (cond
                 ((eof-object? ch))
                 ((or (char=? ch #\space) (char=? ch #\tab))
                  (read-char)
                  (consume-trailing-whitespace))
                 ((char=? ch #\newline)
                  (read-char))))))
           (-read (lambda ()
                    (let ((val
                           (let ((prompt (cond ((string? scm-repl-prompt)
                                                scm-repl-prompt)
                                               ((thunk? scm-repl-prompt)
                                                (scm-repl-prompt))
                                               (scm-repl-prompt "> ")
                                               (else ""))))
                             (repl-reader prompt))))

                      ;; As described in R4RS, the READ procedure updates the
                      ;; port to point to the first character past the end of
                      ;; the external representation of the object.  This
                      ;; means that it doesn't consume the newline typically
                      ;; found after an expression.  This means that, when
                      ;; debugging Guile with GDB, GDB gets the newline, which
                      ;; it often interprets as a "continue" command, making
                      ;; breakpoints kind of useless.  So, consume any
                      ;; trailing newline here, as well as any whitespace
                      ;; before it.
                      ;; But not if EOF, for control-D.
                      (if (not (eof-object? val))
                          (consume-trailing-whitespace))
                      (run-hook after-read-hook)
                      (if (eof-object? val)
                          (begin
                            (repl-report-start-timing)
                            (if scm-repl-verbose
                                (begin
                                  (newline)
                                  (display ";;; EOF -- quitting")
                                  (newline)))
                            (quit 0)))
                      val)))

           (-eval (lambda (sourc)
                    (repl-report-start-timing)
                    (run-hook before-eval-hook sourc)
                    (let ((val (start-stack 'repl-stack
                                            ;; If you change this procedure
                                            ;; (primitive-eval), please also
                                            ;; modify the repl-stack case in
                                            ;; save-stack so that stack cutting
                                            ;; continues to work.
                                            (primitive-eval sourc))))
                      (run-hook after-eval-hook sourc)
                      val)))


           (-print (let ((maybe-print (lambda (result)
                                        (if (or scm-repl-print-unspecified
                                                (not (unspecified? result)))
                                            (begin
                                              (write result)
                                              (newline))))))
                     (lambda (result)
                       (if (not scm-repl-silent)
                           (begin
                             (run-hook before-print-hook result)
                             (maybe-print result)
                             (run-hook after-print-hook result)
                             (if scm-repl-verbose
                                 (repl-report))
                             (force-output))))))

           (-quit (lambda (args)
                    (if scm-repl-verbose
                        (begin
                          (display ";;; QUIT executed, repl exitting")
                          (newline)
                          (repl-report)))
                    args)))

    (let ((status (error-catching-repl -read
                                       -eval
                                       -print)))
      (-quit status))))




;;; {IOTA functions: generating lists of numbers}
;;;

(define (iota n)
  (let loop ((count (1- n)) (result '()))
    (if (< count 0) result
        (loop (1- count) (cons count result)))))



;;; {collect}
;;;
;;; Similar to `begin' but returns a list of the results of all constituent
;;; forms instead of the result of the last form.
;;; (The definition relies on the current left-to-right
;;;  order of evaluation of operands in applications.)
;;;

(defmacro collect forms
  (cons 'list forms))



;;; {While}
;;;
;;; with `continue' and `break'.
;;;

;; The inner `do' loop avoids re-establishing a catch every iteration,
;; that's only necessary if continue is actually used.  A new key is
;; generated every time, so break and continue apply to their originating
;; `while' even when recursing.
;;
;; FIXME: This macro is unintentionally unhygienic with respect to let,
;; make-symbol, do, throw, catch, lambda, and not.
;;
(define-macro (while cond . body)
  (let ((keyvar (make-symbol "while-keyvar")))
    `(let ((,keyvar (make-symbol "while-key")))
       (do ()
           ((catch ,keyvar
                   (lambda ()
                     (let ((break (lambda () (throw ,keyvar #t)))
                           (continue (lambda () (throw ,keyvar #f))))
                       (do ()
                           ((not ,cond))
                         ,@body)
                       #t))
                   (lambda (key arg)
                     arg)))))))




;;; {Module System Macros}
;;;

;; Return a list of expressions that evaluate to the appropriate
;; arguments for resolve-interface according to SPEC.

(eval-when
 (compile)
 (if (memq 'prefix (read-options))
     (error "boot-9 must be compiled with #:kw, not :kw")))

(define (compile-interface-spec spec)
  (define (make-keyarg sym key quote?)
    (cond ((or (memq sym spec)
               (memq key spec))
           => (lambda (rest)
                (if quote?
                    (list key (list 'quote (cadr rest)))
                    (list key (cadr rest)))))
          (else
           '())))
  (define (map-apply func list)
    (map (lambda (args) (apply func args)) list))
  (define keys
    ;; sym     key      quote?
    '((:select #:select #t)
      (:hide   #:hide   #t)
      (:prefix #:prefix #t)
      (:renamer #:renamer #f)
      (:version #:version #t)))
  (if (not (pair? (car spec)))
      `(',spec)
      `(',(car spec)
        ,@(apply append (map-apply make-keyarg keys)))))

(define (keyword-like-symbol->keyword sym)
  (symbol->keyword (string->symbol (substring (symbol->string sym) 1))))

(define (compile-define-module-args args)
  ;; Just quote everything except #:use-module and #:use-syntax.  We
  ;; need to know about all arguments regardless since we want to turn
  ;; symbols that look like keywords into real keywords, and the
  ;; keyword args in a define-module form are not regular
  ;; (i.e. no-backtrace doesn't take a value).
  (let loop ((compiled-args `((quote ,(car args))))
             (args (cdr args)))
    (cond ((null? args)
           (reverse! compiled-args))
          ;; symbol in keyword position
          ((symbol? (car args))
           (loop compiled-args
                 (cons (keyword-like-symbol->keyword (car args)) (cdr args))))
          ((memq (car args) '(#:no-backtrace #:pure))
           (loop (cons (car args) compiled-args)
                 (cdr args)))
          ((null? (cdr args))
           (error "keyword without value:" (car args)))
          ((memq (car args) '(#:use-module #:use-syntax))
           (loop (cons* `(list ,@(compile-interface-spec (cadr args)))
                        (car args)
                        compiled-args)
                 (cddr args)))
          ((eq? (car args) #:autoload)
           (loop (cons* `(quote ,(caddr args))
                        `(quote ,(cadr args))
                        (car args)
                        compiled-args)
                 (cdddr args)))
          (else
           (loop (cons* `(quote ,(cadr args))
                        (car args)
                        compiled-args)
                 (cddr args))))))

(defmacro define-module args
  `(eval-when
    (eval load compile)
    (let ((m (process-define-module
              (list ,@(compile-define-module-args args)))))
      (set-current-module m)
      m)))

;; The guts of the use-modules macro.  Add the interfaces of the named
;; modules to the use-list of the current module, in order.

;; This function is called by "modules.c".  If you change it, be sure
;; to change scm_c_use_module as well.

(define (process-use-modules module-interface-args)
  (let ((interfaces (map (lambda (mif-args)
                           (or (apply resolve-interface mif-args)
                               (error "no such module" mif-args)))
                         module-interface-args)))
    (call-with-deferred-observers
     (lambda ()
       (module-use-interfaces! (current-module) interfaces)))))

(defmacro use-modules modules
  `(eval-when
    (eval load compile)
    (process-use-modules
     (list ,@(map (lambda (m)
                    `(list ,@(compile-interface-spec m)))
                  modules)))
    *unspecified*))

(defmacro use-syntax (spec)
  `(eval-when
    (eval load compile)
    (issue-deprecation-warning
     "`use-syntax' is deprecated. Please contact guile-devel for more info.")
    (process-use-modules (list (list ,@(compile-interface-spec spec))))
    *unspecified*))

(define-syntax define-private
  (syntax-rules ()
    ((_ foo bar)
     (define foo bar))))

(define-syntax define-public
  (syntax-rules ()
    ((_ (name . args) . body)
     (define-public name (lambda args . body)))
    ((_ name val)
     (begin
       (define name val)
       (export name)))))

(define-syntax defmacro-public
  (syntax-rules ()
    ((_ name args . body)
     (begin
       (defmacro name args . body)
       (export-syntax name)))))

;; And now for the most important macro.
(define-syntax λ
  (syntax-rules ()
    ((_ formals body ...)
     (lambda formals body ...))))


;; Export a local variable

;; This function is called from "modules.c".  If you change it, be
;; sure to update "modules.c" as well.

(define (module-export! m names)
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-ensure-local-variable! m internal-name)))
                  (module-add! public-i external-name var)))
              names)))

(define (module-replace! m names)
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-ensure-local-variable! m internal-name)))
                  (set-object-property! var 'replace #t)
                  (module-add! public-i external-name var)))
              names)))

;; Re-export a imported variable
;;
(define (module-re-export! m names)
  (let ((public-i (module-public-interface m)))
    (for-each (lambda (name)
                (let* ((internal-name (if (pair? name) (car name) name))
                       (external-name (if (pair? name) (cdr name) name))
                       (var (module-variable m internal-name)))
                  (cond ((not var)
                         (error "Undefined variable:" internal-name))
                        ((eq? var (module-local-variable m internal-name))
                         (error "re-exporting local variable:" internal-name))
                        (else
                         (module-add! public-i external-name var)))))
              names)))

(defmacro export names
  `(eval-when (eval load compile)
     (call-with-deferred-observers
      (lambda ()
        (module-export! (current-module) ',names)))))

(defmacro re-export names
  `(eval-when (eval load compile)
     (call-with-deferred-observers
       (lambda ()
         (module-re-export! (current-module) ',names)))))

(defmacro export-syntax names
  `(export ,@names))

(defmacro re-export-syntax names
  `(re-export ,@names))

(define load load-module)



;;; {Parameters}
;;;

(define make-mutable-parameter
  (let ((make (lambda (fluid converter)
                (lambda args
                  (if (null? args)
                      (fluid-ref fluid)
                      (fluid-set! fluid (converter (car args))))))))
    (lambda (init . converter)
      (let ((fluid (make-fluid))
            (converter (if (null? converter)
                           identity
                           (car converter))))
        (fluid-set! fluid (converter init))
        (make fluid converter)))))



;;; {Handling of duplicate imported bindings}
;;;

;; Duplicate handlers take the following arguments:
;;
;; module  importing module
;; name    conflicting name
;; int1    old interface where name occurs
;; val1    value of binding in old interface
;; int2    new interface where name occurs
;; val2    value of binding in new interface
;; var     previous resolution or #f
;; val     value of previous resolution
;;
;; A duplicate handler can take three alternative actions:
;;
;; 1. return #f => leave responsibility to next handler
;; 2. exit with an error
;; 3. return a variable resolving the conflict
;;

(define duplicate-handlers
  (let ((m (make-module 7)))
    
    (define (check module name int1 val1 int2 val2 var val)
      (scm-error 'misc-error
                 #f
                 "~A: `~A' imported from both ~A and ~A"
                 (list (module-name module)
                       name
                       (module-name int1)
                       (module-name int2))
                 #f))
    
    (define (warn module name int1 val1 int2 val2 var val)
      (format (current-error-port)
              "WARNING: ~A: `~A' imported from both ~A and ~A\n"
              (module-name module)
              name
              (module-name int1)
              (module-name int2))
      #f)
     
    (define (replace module name int1 val1 int2 val2 var val)
      (let ((old (or (and var (object-property var 'replace) var)
                     (module-variable int1 name)))
            (new (module-variable int2 name)))
        (if (object-property old 'replace)
            (and (or (eq? old new)
                     (not (object-property new 'replace)))
                 old)
            (and (object-property new 'replace)
                 new))))
    
    (define (warn-override-core module name int1 val1 int2 val2 var val)
      (and (eq? int1 the-scm-module)
           (begin
             (format (current-error-port)
                     "WARNING: ~A: imported module ~A overrides core binding `~A'\n"
                     (module-name module)
                     (module-name int2)
                     name)
             (module-local-variable int2 name))))
     
    (define (first module name int1 val1 int2 val2 var val)
      (or var (module-local-variable int1 name)))
     
    (define (last module name int1 val1 int2 val2 var val)
      (module-local-variable int2 name))
     
    (define (noop module name int1 val1 int2 val2 var val)
      #f)
    
    (set-module-name! m 'duplicate-handlers)
    (set-module-kind! m 'interface)
    (module-define! m 'check check)
    (module-define! m 'warn warn)
    (module-define! m 'replace replace)
    (module-define! m 'warn-override-core warn-override-core)
    (module-define! m 'first first)
    (module-define! m 'last last)
    (module-define! m 'merge-generics noop)
    (module-define! m 'merge-accessors noop)
    m))

(define (lookup-duplicates-handlers handler-names)
  (and handler-names
       (map (lambda (handler-name)
              (or (module-symbol-local-binding
                   duplicate-handlers handler-name #f)
                  (error "invalid duplicate handler name:"
                         handler-name)))
            (if (list? handler-names)
                handler-names
                (list handler-names)))))

(define default-duplicate-binding-procedures
  (make-mutable-parameter #f))

(define default-duplicate-binding-handler
  (make-mutable-parameter '(replace warn-override-core warn last)
                          (lambda (handler-names)
                            (default-duplicate-binding-procedures
                              (lookup-duplicates-handlers handler-names))
                            handler-names)))



;;; {`cond-expand' for SRFI-0 support.}
;;;
;;; This syntactic form expands into different commands or
;;; definitions, depending on the features provided by the Scheme
;;; implementation.
;;;
;;; Syntax:
;;;
;;; <cond-expand>
;;;   --> (cond-expand <cond-expand-clause>+)
;;;     | (cond-expand <cond-expand-clause>* (else <command-or-definition>))
;;; <cond-expand-clause>
;;;   --> (<feature-requirement> <command-or-definition>*)
;;; <feature-requirement>
;;;   --> <feature-identifier>
;;;     | (and <feature-requirement>*)
;;;     | (or <feature-requirement>*)
;;;     | (not <feature-requirement>)
;;; <feature-identifier>
;;;   --> <a symbol which is the name or alias of a SRFI>
;;;
;;; Additionally, this implementation provides the
;;; <feature-identifier>s `guile' and `r5rs', so that programs can
;;; determine the implementation type and the supported standard.
;;;
;;; Currently, the following feature identifiers are supported:
;;;
;;;   guile r5rs srfi-0 srfi-4 srfi-6 srfi-13 srfi-14 srfi-55 srfi-61
;;;
;;; Remember to update the features list when adding more SRFIs.
;;;

(define %cond-expand-features
  ;; Adjust the above comment when changing this.
  '(guile
    guile-2
    r5rs
    srfi-0   ;; cond-expand itself
    srfi-4   ;; homogenous numeric vectors
    srfi-6   ;; open-input-string etc, in the guile core
    srfi-13  ;; string library
    srfi-14  ;; character sets
    srfi-55  ;; require-extension
    srfi-61  ;; general cond clause
    ))

;; This table maps module public interfaces to the list of features.
;;
(define %cond-expand-table (make-hash-table 31))

;; Add one or more features to the `cond-expand' feature list of the
;; module `module'.
;;
(define (cond-expand-provide module features)
  (let ((mod (module-public-interface module)))
    (and mod
         (hashq-set! %cond-expand-table mod
                     (append (hashq-ref %cond-expand-table mod '())
                             features)))))

(define-macro (cond-expand . clauses)
  (let ((syntax-error (lambda (cl)
                        (error "invalid clause in `cond-expand'" cl))))
    (letrec
        ((test-clause
          (lambda (clause)
            (cond
             ((symbol? clause)
              (or (memq clause %cond-expand-features)
                  (let lp ((uses (module-uses (current-module))))
                    (if (pair? uses)
                        (or (memq clause
                                  (hashq-ref %cond-expand-table
                                             (car uses) '()))
                            (lp (cdr uses)))
                        #f))))
             ((pair? clause)
              (cond
               ((eq? 'and (car clause))
                (let lp ((l (cdr clause)))
                  (cond ((null? l)
                         #t)
                        ((pair? l)
                         (and (test-clause (car l)) (lp (cdr l))))
                        (else
                         (syntax-error clause)))))
               ((eq? 'or (car clause))
                (let lp ((l (cdr clause)))
                  (cond ((null? l)
                         #f)
                        ((pair? l)
                         (or (test-clause (car l)) (lp (cdr l))))
                        (else
                         (syntax-error clause)))))
               ((eq? 'not (car clause))
                (cond ((not (pair? (cdr clause)))
                       (syntax-error clause))
                      ((pair? (cddr clause))
                       ((syntax-error clause))))
                (not (test-clause (cadr clause))))
               (else
                (syntax-error clause))))
             (else
              (syntax-error clause))))))
      (let lp ((c clauses))
        (cond
         ((null? c)
          (error "Unfulfilled `cond-expand'"))
         ((not (pair? c))
          (syntax-error c))
         ((not (pair? (car c)))
          (syntax-error (car c)))
         ((test-clause (caar c))
          `(begin ,@(cdar c)))
         ((eq? (caar c) 'else)
          (if (pair? (cdr c))
              (syntax-error c))
          `(begin ,@(cdar c)))
         (else
          (lp (cdr c))))))))

;; This procedure gets called from the startup code with a list of
;; numbers, which are the numbers of the SRFIs to be loaded on startup.
;;
(define (use-srfis srfis)
  (process-use-modules
   (map (lambda (num)
          (list (list 'srfi (string->symbol
                             (string-append "srfi-" (number->string num))))))
        srfis)))



;;; srfi-55: require-extension
;;;

(define-macro (require-extension extension-spec)
  ;; This macro only handles the srfi extension, which, at present, is
  ;; the only one defined by the standard.
  (if (not (pair? extension-spec))
      (scm-error 'wrong-type-arg "require-extension"
                 "Not an extension: ~S" (list extension-spec) #f))
  (let ((extension (car extension-spec))
        (extension-args (cdr extension-spec)))
    (case extension
      ((srfi)
       (let ((use-list '()))
         (for-each
          (lambda (i)
            (if (not (integer? i))
                (scm-error 'wrong-type-arg "require-extension"
                           "Invalid srfi name: ~S" (list i) #f))
            (let ((srfi-sym (string->symbol
                             (string-append "srfi-" (number->string i)))))
              (if (not (memq srfi-sym %cond-expand-features))
                  (set! use-list (cons `(use-modules (srfi ,srfi-sym))
                                       use-list)))))
          extension-args)
         (if (pair? use-list)
             ;; i.e. (begin (use-modules x) (use-modules y) (use-modules z))
             `(begin ,@(reverse! use-list)))))
      (else
       (scm-error
        'wrong-type-arg "require-extension"
        "Not a recognized extension type: ~S" (list extension) #f)))))



;;; {Load emacs interface support if emacs option is given.}
;;;

(define (named-module-use! user usee)
  (module-use! (resolve-module user) (resolve-interface usee)))

(define (load-emacs-interface)
  (and (provided? 'debug-extensions)
       (debug-enable 'backtrace))
  (named-module-use! '(guile-user) '(ice-9 emacs)))



(define using-readline?
  (let ((using-readline? (make-fluid)))
     (make-procedure-with-setter
      (lambda () (fluid-ref using-readline?))
      (lambda (v) (fluid-set! using-readline? v)))))

(define (top-repl)
  (let ((guile-user-module (resolve-module '(guile-user))))

    ;; Load emacs interface support if emacs option is given.
    (if (and (module-defined? guile-user-module 'use-emacs-interface)
             (module-ref guile-user-module 'use-emacs-interface))
        (load-emacs-interface))

    ;; Use some convenient modules (in reverse order)

    (set-current-module guile-user-module)
    (process-use-modules 
     (append
      '(((ice-9 r5rs))
        ((ice-9 session))
        ((ice-9 debug)))
      (if (provided? 'regex)
          '(((ice-9 regex)))
          '())
      (if (provided? 'threads)
          '(((ice-9 threads)))
          '())))
    ;; load debugger on demand
    (module-autoload! guile-user-module '(system vm debug) '(debug))

    ;; Note: SIGFPE, SIGSEGV and SIGBUS are actually "query-only" (see
    ;; scmsigs.c scm_sigaction_for_thread), so the handlers setup here have
    ;; no effect.
    (let ((old-handlers #f)
          (start-repl (module-ref (resolve-interface '(system repl repl))
                                  'start-repl))
          (signals (if (provided? 'posix)
                       `((,SIGINT . "User interrupt")
                         (,SIGFPE . "Arithmetic error")
                         (,SIGSEGV
                          . "Bad memory access (Segmentation violation)"))
                       '())))
      ;; no SIGBUS on mingw
      (if (defined? 'SIGBUS)
          (set! signals (acons SIGBUS "Bad memory access (bus error)"
                               signals)))

      (dynamic-wind

          ;; call at entry
          (lambda ()
            (let ((make-handler (lambda (msg)
                                  (lambda (sig)
                                    ;; Make a backup copy of the stack
                                    (fluid-set! before-signal-stack
                                                (fluid-ref the-last-stack))
                                    (save-stack 2)
                                    (scm-error 'signal
                                               #f
                                               msg
                                               #f
                                               (list sig))))))
              (set! old-handlers
                    (map (lambda (sig-msg)
                           (sigaction (car sig-msg)
                                      (make-handler (cdr sig-msg))))
                         signals))))

          ;; the protected thunk.
          (lambda ()
            (let ((status (start-repl 'scheme)))
              (run-hook exit-hook)
              status))

          ;; call at exit.
          (lambda ()
            (map (lambda (sig-msg old-handler)
                   (if (not (car old-handler))
                       ;; restore original C handler.
                       (sigaction (car sig-msg) #f)
                       ;; restore Scheme handler, SIG_IGN or SIG_DFL.
                       (sigaction (car sig-msg)
                                  (car old-handler)
                                  (cdr old-handler))))
                 signals old-handlers))))))

;;; This hook is run at the very end of an interactive session.
;;;
(define exit-hook (make-hook))



;;; {Deprecated stuff}
;;;

(begin-deprecated
 (module-use! the-scm-module (resolve-interface '(ice-9 deprecated))))



;;; Place the user in the guile-user module.
;;;

;;; FIXME: annotate ?
;; (define (syncase exp)
;;   (with-fluids ((expansion-eval-closure
;;               (module-eval-closure (current-module))))
;;     (deannotate/source-properties (macroexpand (annotate exp)))))

;; FIXME:
(module-use! the-scm-module (resolve-interface '(srfi srfi-4)))

(define-module (guile-user)
  #:autoload (system base compile) (compile))

;; Remain in the `(guile)' module at compilation-time so that the
;; `-Wunused-toplevel' warning works as expected.
(eval-when (compile) (set-current-module the-root-module))

;;; boot-9.scm ends here
