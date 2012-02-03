;;; Guile Emacs Lisp

;; Copyright (C) 2009, 2010 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(define-module (language elisp compile-tree-il)
  #:use-module (language elisp bindings)
  #:use-module (language elisp runtime)
  #:use-module (language tree-il)
  #:use-module (system base pmatch)
  #:use-module (system base compile)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (compile-tree-il
            compile-progn
            compile-eval-when-compile
            compile-if
            compile-defconst
            compile-defvar
            compile-setq
            compile-let
            compile-flet
            compile-labels
            compile-let*
            compile-guile-ref
            compile-guile-primitive
            compile-function
            compile-defmacro
            compile-defun
            #{compile-`}#
            compile-quote
            compile-%funcall
            compile-%set-lexical-binding-mode))

;;; Certain common parameters (like the bindings data structure or
;;; compiler options) are not always passed around but accessed using
;;; fluids to simulate dynamic binding (hey, this is about elisp).

;;; The bindings data structure to keep track of symbol binding related
;;; data.

(define bindings-data (make-fluid))

(define lexical-binding (make-fluid))

;;; Find the source properties of some parsed expression if there are
;;; any associated with it.

(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
         (and (not (null? props))
              props))))

;;; Values to use for Elisp's nil and t.

(define (nil-value loc)
  (make-const loc (@ (language elisp runtime) nil-value)))

(define (t-value loc)
  (make-const loc (@ (language elisp runtime) t-value)))

;;; Modules that contain the value and function slot bindings.

(define runtime '(language elisp runtime))

(define value-slot (@ (language elisp runtime) value-slot-module))

(define function-slot (@ (language elisp runtime) function-slot-module))

;;; The backquoting works the same as quasiquotes in Scheme, but the
;;; forms are named differently; to make easy adaptions, we define these
;;; predicates checking for a symbol being the car of an
;;; unquote/unquote-splicing/backquote form.

(define (unquote? sym)
  (and (symbol? sym) (eq? sym '#{,}#)))

(define (unquote-splicing? sym)
  (and (symbol? sym) (eq? sym '#{,@}#)))

;;; Build a call to a primitive procedure nicely.

(define (call-primitive loc sym . args)
  (make-application loc (make-primitive-ref loc sym) args))

;;; Error reporting routine for syntax/compilation problems or build
;;; code for a runtime-error output.

(define (report-error loc . args)
  (apply error args))

;;; Generate code to ensure a global symbol is there for further use of
;;; a given symbol.  In general during the compilation, those needed are
;;; only tracked with the bindings data structure.  Afterwards, however,
;;; for all those needed symbols the globals are really generated with
;;; this routine.

(define (generate-ensure-global loc sym module)
  (make-application loc
                    (make-module-ref loc runtime 'ensure-fluid! #t)
                    (list (make-const loc module)
                          (make-const loc sym))))

(define (ensuring-globals loc bindings body)
  (make-sequence
   loc
   `(,@(map-globals (fluid-ref bindings)
                    (lambda (mod sym)
                      (generate-ensure-global loc sym mod)))
     ,body)))

;;; Build a construct that establishes dynamic bindings for certain
;;; variables.  We may want to choose between binding with fluids and
;;; with-fluids* and using just ordinary module symbols and
;;; setting/reverting their values with a dynamic-wind.

(define (let-dynamic loc syms module vals body)
  (call-primitive
   loc
   'with-fluids*
   (make-application loc
                     (make-primitive-ref loc 'list)
                     (map (lambda (sym)
                            (make-module-ref loc module sym #t))
                          syms))
   (make-application loc (make-primitive-ref loc 'list) vals)
   (make-lambda loc
                '()
                (make-lambda-case #f '() #f #f #f '() '() body #f))))

;;; Handle access to a variable (reference/setting) correctly depending
;;; on whether it is currently lexically or dynamically bound.  lexical
;;; access is done only for references to the value-slot module!

(define (access-variable loc
                         sym
                         module
                         handle-global
                         handle-lexical
                         handle-dynamic)
  (let ((lexical (get-lexical-binding (fluid-ref bindings-data) sym)))
    (cond
     (lexical (handle-lexical lexical))
     ((equal? module function-slot) (handle-global))
     (else (handle-dynamic)))))

;;; Generate code to reference a variable.  For references in the
;;; value-slot module, we may want to generate a lexical reference
;;; instead if the variable has a lexical binding.

(define (reference-variable loc sym module)
  (access-variable
   loc
   sym
   module
   (lambda () (make-module-ref loc module sym #t))
   (lambda (lexical) (make-lexical-ref loc lexical lexical))
   (lambda ()
     (mark-global! (fluid-ref bindings-data) sym module)
     (call-primitive loc
                     'fluid-ref
                     (make-module-ref loc module sym #t)))))

;;; Generate code to set a variable.  Just as with reference-variable, in
;;; case of a reference to value-slot, we want to generate a lexical set
;;; when the variable has a lexical binding.

(define (set-variable! loc sym module value)
  (access-variable
   loc
   sym
   module
   (lambda ()
     (make-application
      loc
      (make-module-ref loc runtime 'set-symbol-function! #t) ;++ fix
      (list (make-const loc sym) value)))
   (lambda (lexical) (make-lexical-set loc lexical lexical value))
   (lambda ()
     (mark-global! (fluid-ref bindings-data) sym module)
     (call-primitive loc
                     'fluid-set!
                     (make-module-ref loc module sym #t)
                     value))))

(define (bind-lexically? sym module decls)
  (or (eq? module function-slot)
      (let ((decl (assq-ref decls sym)))
        (and (equal? module value-slot)
             (or
              (eq? decl 'lexical)
              (and
               (fluid-ref lexical-binding)
               (not (global? (fluid-ref bindings-data) sym module))))))))

(define (parse-let-binding loc binding)
  (pmatch binding
    ((unquote var)
     (guard (symbol? var))
     (cons var #nil))
    ((,var)
     (guard (symbol? var))
     (cons var #nil))
    ((,var ,val)
     (guard (symbol? var))
     (cons var val))
    (else
     (report-error loc "malformed variable binding" binding))))

(define (parse-flet-binding loc binding)
  (pmatch binding
    ((,var ,args . ,body)
     (guard (symbol? var))
     (cons var `(function (lambda ,args ,@body))))
    (else
     (report-error loc "malformed function binding" binding))))

(define (parse-declaration expr)
  (pmatch expr
    ((lexical . ,vars)
     (map (cut cons <> 'lexical) vars))
    (else
     '())))

(define (parse-body-1 body lambda?)
  (let loop ((lst body)
             (decls '())
             (intspec #f)
             (doc #f))
    (pmatch lst
      (((declare . ,x) . ,tail)
       (loop tail (append-reverse x decls) intspec doc))
      (((interactive . ,x) . ,tail)
       (guard lambda? (not intspec))
       (loop tail decls x doc))
      ((,x . ,tail)
       (guard lambda? (string? x) (not doc) (not (null? tail)))
       (loop tail decls intspec x))
      (else
       (values (append-map parse-declaration decls)
               intspec
               doc
               lst)))))

(define (parse-lambda-body body)
  (parse-body-1 body #t))

(define (parse-body body)
  (receive (decls intspec doc body) (parse-body-1 body #f)
    (values decls body)))

;;; Let is done with a single call to let-dynamic binding them locally
;;; to new values all "at once".  If there is at least one variable to
;;; bind lexically among the bindings, we first do a let for all of them
;;; to evaluate all values before any bindings take place, and then call
;;; let-dynamic for the variables to bind dynamically.

(define (generate-let loc module bindings body)
  (receive (decls forms) (parse-body body)
    (receive (lexical dynamic)
             (partition (compose (cut bind-lexically? <> module decls)
                                 car)
                        bindings)
      (for-each (lambda (sym)
                  (mark-global! (fluid-ref bindings-data)
                                sym
                                module))
                (map car dynamic))
      (let ((make-values (lambda (for)
                           (map (lambda (el) (compile-expr (cdr el)))
                                for)))
            (make-body (lambda () (compile-expr `(progn ,@forms)))))
        (if (null? lexical)
            (let-dynamic loc (map car dynamic) module
                         (make-values dynamic) (make-body))
            (let* ((lexical-syms (map (lambda (el) (gensym)) lexical))
                   (dynamic-syms (map (lambda (el) (gensym)) dynamic))
                   (all-syms (append lexical-syms dynamic-syms))
                   (vals (append (make-values lexical)
                                 (make-values dynamic))))
              (make-let loc
                        all-syms
                        all-syms
                        vals
                        (with-lexical-bindings
                         (fluid-ref bindings-data)
                         (map car lexical) lexical-syms
                         (lambda ()
                           (if (null? dynamic)
                               (make-body)
                               (let-dynamic loc
                                            (map car dynamic)
                                            module
                                            (map
                                             (lambda (sym)
                                               (make-lexical-ref loc
                                                                 sym
                                                                 sym))
                                             dynamic-syms)
                                            (make-body))))))))))))

;;; Let* is compiled to a cascaded set of "small lets" for each binding
;;; in turn so that each one already sees the preceding bindings.

(define (generate-let* loc module bindings body)
  (receive (decls forms) (parse-body body)
    (begin
      (for-each (lambda (sym)
                  (if (not (bind-lexically? sym module decls))
                      (mark-global! (fluid-ref bindings-data)
                                    sym
                                    module)))
                (map car bindings))
      (let iterate ((tail bindings))
        (if (null? tail)
            (compile-expr `(progn ,@forms))
            (let ((sym (caar tail))
                  (value (compile-expr (cdar tail))))
              (if (bind-lexically? sym module decls)
                  (let ((target (gensym)))
                    (make-let loc
                              `(,target)
                              `(,target)
                              `(,value)
                              (with-lexical-bindings
                               (fluid-ref bindings-data)
                               `(,sym)
                               `(,target)
                               (lambda () (iterate (cdr tail))))))
                  (let-dynamic loc
                               `(,(caar tail))
                               module
                               `(,value)
                               (iterate (cdr tail))))))))))

;;; Partition the argument list of a lambda expression into required,
;;; optional and rest arguments.

(define (parse-lambda-list lst)
  (define (%match lst null optional rest symbol)
    (pmatch lst
      (() (null))
      ((&optional . ,tail) (optional tail))
      ((&rest . ,tail) (rest tail))
      ((,arg . ,tail) (guard (symbol? arg)) (symbol arg tail))
      (else (fail))))
  (define (return rreq ropt rest)
    (values #t (reverse rreq) (reverse ropt) rest))
  (define (fail)
    (values #f #f #f #f))
  (define (parse-req lst rreq)
    (%match lst
            (lambda () (return rreq '() #f))
            (lambda (tail) (parse-opt tail rreq '()))
            (lambda (tail) (parse-rest tail rreq '()))
            (lambda (arg tail) (parse-req tail (cons arg rreq)))))
  (define (parse-opt lst rreq ropt)
    (%match lst
            (lambda () (return rreq ropt #f))
            (lambda (tail) (fail))
            (lambda (tail) (parse-rest tail rreq ropt))
            (lambda (arg tail) (parse-opt tail rreq (cons arg ropt)))))
  (define (parse-rest lst rreq ropt)
    (%match lst
            (lambda () (fail))
            (lambda (tail) (fail))
            (lambda (tail) (fail))
            (lambda (arg tail) (parse-post-rest tail rreq ropt arg))))
  (define (parse-post-rest lst rreq ropt rest)
    (%match lst
            (lambda () (return rreq ropt rest))
            (lambda () (fail))
            (lambda () (fail))
            (lambda (arg tail) (fail))))
  (parse-req lst '()))

(define (make-simple-lambda loc meta req opt init rest vars body)
  (make-lambda loc
               meta
               (make-lambda-case #f req opt rest #f init vars body #f)))

(define (compile-lambda loc meta args body)
  (receive (valid? req-ids opt-ids rest-id)
           (parse-lambda-list args)
    (if valid?
        (let* ((all-ids (append req-ids
                                opt-ids
                                (or (and=> rest-id list) '())))
               (all-vars (map (lambda (ignore) (gensym)) all-ids)))
          (let*-values (((decls intspec doc forms)
                         (parse-lambda-body body))
                        ((lexical dynamic)
                         (partition
                          (compose (cut bind-lexically? <> value-slot decls)
                                   car)
                          (map list all-ids all-vars)))
                        ((lexical-ids lexical-vars) (unzip2 lexical))
                        ((dynamic-ids dynamic-vars) (unzip2 dynamic)))
            (with-dynamic-bindings
             (fluid-ref bindings-data)
             dynamic-ids
             (lambda ()
               (with-lexical-bindings
                (fluid-ref bindings-data)
                lexical-ids
                lexical-vars
                (lambda ()
                  (let* ((tree-il (compile-expr `(progn ,@forms)))
                         (full-body
                          (if (null? dynamic)
                              tree-il
                              (let-dynamic loc
                                           dynamic-ids
                                           value-slot
                                           (map (cut make-lexical-ref
                                                     loc
                                                     <>
                                                     <>)
                                                dynamic-ids
                                                dynamic-vars)
                                           tree-il))))
                    (make-simple-lambda loc
                                        meta
                                        req-ids
                                        opt-ids
                                        (map (const (nil-value loc))
                                             opt-ids)
                                        rest-id
                                        all-vars
                                        full-body))))))))
        (report-error "invalid function" `(lambda ,args ,@body)))))

;;; Handle the common part of defconst and defvar, that is, checking for
;;; a correct doc string and arguments as well as maybe in the future
;;; handling the docstring somehow.

(define (handle-var-def loc sym doc)
  (cond
   ((not (symbol? sym)) (report-error loc "expected symbol, got" sym))
   ((> (length doc) 1) (report-error loc "too many arguments to defvar"))
   ((and (not (null? doc)) (not (string? (car doc))))
    (report-error loc "expected string as third argument of defvar, got"
                  (car doc)))
   ;; TODO: Handle doc string if present.
   (else #t)))

;;; Handle macro and special operator bindings.

(define (find-operator name type)
  (and
   (symbol? name)
   (module-defined? (resolve-interface function-slot) name)
   (let ((op (module-ref (resolve-module function-slot) name)))
     (if (and (pair? op) (eq? (car op) type))
         (cdr op)
         #f))))

;;; See if a (backquoted) expression contains any unquotes.

(define (contains-unquotes? expr)
  (if (pair? expr)
      (if (or (unquote? (car expr)) (unquote-splicing? (car expr)))
          #t
          (or (contains-unquotes? (car expr))
              (contains-unquotes? (cdr expr))))
      #f))

;;; Process a backquoted expression by building up the needed
;;; cons/append calls.  For splicing, it is assumed that the expression
;;; spliced in evaluates to a list.  The emacs manual does not really
;;; state either it has to or what to do if it does not, but Scheme
;;; explicitly forbids it and this seems reasonable also for elisp.

(define (unquote-cell? expr)
  (and (list? expr) (= (length expr) 2) (unquote? (car expr))))

(define (unquote-splicing-cell? expr)
  (and (list? expr) (= (length expr) 2) (unquote-splicing? (car expr))))

(define (process-backquote loc expr)
  (if (contains-unquotes? expr)
      (if (pair? expr)
          (if (or (unquote-cell? expr) (unquote-splicing-cell? expr))
              (compile-expr (cadr expr))
              (let* ((head (car expr))
                     (processed-tail (process-backquote loc (cdr expr)))
                     (head-is-list-2 (and (list? head)
                                          (= (length head) 2)))
                     (head-unquote (and head-is-list-2
                                        (unquote? (car head))))
                     (head-unquote-splicing (and head-is-list-2
                                                 (unquote-splicing?
                                                  (car head)))))
                (if head-unquote-splicing
                    (call-primitive loc
                                    'append
                                    (compile-expr (cadr head))
                                    processed-tail)
                    (call-primitive loc 'cons
                                    (if head-unquote
                                        (compile-expr (cadr head))
                                        (process-backquote loc head))
                                    processed-tail))))
          (report-error loc
                        "non-pair expression contains unquotes"
                        expr))
      (make-const loc expr)))

;;; Special operators

(defspecial progn (loc args)
  (make-sequence loc
                 (if (null? args)
                     (list (nil-value loc))
                     (map compile-expr args))))

(defspecial eval-when-compile (loc args)
  (make-const loc (compile `(progn ,@args) #:from 'elisp #:to 'value)))

(defspecial if (loc args)
  (pmatch args
    ((,cond ,then . ,else)
     (make-conditional
      loc
      (call-primitive loc 'not
       (call-primitive loc 'nil? (compile-expr cond)))
      (compile-expr then)
      (compile-expr `(progn ,@else))))))

(defspecial defconst (loc args)
  (pmatch args
    ((,sym ,value . ,doc)
     (if (handle-var-def loc sym doc)
         (make-sequence loc
                        (list (set-variable! loc
                                             sym
                                             value-slot
                                             (compile-expr value))
                              (make-const loc sym)))))))

(defspecial defvar (loc args)
  (pmatch args
    ((,sym) (make-const loc sym))
    ((,sym ,value . ,doc)
     (if (handle-var-def loc sym doc)
         (make-sequence
          loc
          (list
           (make-conditional
            loc
            (make-conditional
             loc
             (call-primitive
              loc
              'module-bound?
              (call-primitive loc
                              'resolve-interface
                              (make-const loc value-slot))
              (make-const loc sym))
             (call-primitive loc
                             'fluid-bound?
                             (make-module-ref loc value-slot sym #t))
             (make-const loc #f))
            (make-void loc)
            (set-variable! loc sym value-slot (compile-expr value)))
           (make-const loc sym)))))))

(defspecial setq (loc args)
  (define (car* x) (if (null? x) '() (car x)))
  (define (cdr* x) (if (null? x) '() (cdr x)))
  (define (cadr* x) (car* (cdr* x)))
  (define (cddr* x) (cdr* (cdr* x)))
  (make-sequence
   loc
   (let loop ((args args) (last (nil-value loc)))
     (if (null? args)
         (list last)
         (let ((sym (car args))
               (val (compile-expr (cadr* args))))
           (if (not (symbol? sym))
               (report-error loc "expected symbol in setq")
               (cons
                (set-variable! loc sym value-slot val)
                (loop (cddr* args)
                      (reference-variable loc sym value-slot)))))))))
  
(defspecial let (loc args)
  (pmatch args
    ((,bindings . ,body)
     (generate-let loc
                   value-slot
                   (map (cut parse-let-binding loc <>) bindings)
                   body))))

(defspecial flet (loc args)
  (pmatch args
    ((,bindings . ,body)
     (let ((names+vals (map (cut parse-flet-binding loc <>) bindings)))
       (receive (decls forms) (parse-body body)
         (let ((names (map car names+vals))
               (vals (map cdr names+vals))
               (gensyms (map (lambda (x) (gensym)) names+vals)))
           (with-lexical-bindings
            (fluid-ref bindings-data)
            names
            gensyms
            (lambda ()
              (make-let loc
                        names
                        gensyms
                        (map compile-expr vals)
                        (compile-expr `(progn ,@forms)))))))))))

(defspecial labels (loc args)
  (pmatch args
    ((,bindings . ,body)
     (let ((names+vals (map (cut parse-flet-binding loc <>) bindings)))
       (receive (decls forms) (parse-body body)
         (let ((names (map car names+vals))
               (vals (map cdr names+vals))
               (gensyms (map (lambda (x) (gensym)) names+vals)))
           (with-lexical-bindings
            (fluid-ref bindings-data)
            names
            gensyms
            (lambda ()
              (make-letrec #f
                           loc
                           names
                           gensyms
                           (map compile-expr vals)
                           (compile-expr `(progn ,@forms)))))))))))

(defspecial let* (loc args)
  (pmatch args
    ((,bindings . ,body)
     (generate-let* loc
                    value-slot
                    (map (cut parse-let-binding loc <>) bindings)
                    body))))

;;; guile-ref allows building TreeIL's module references from within
;;; elisp as a way to access data within the Guile universe.  The module
;;; and symbol referenced are static values, just like (@ module symbol)
;;; does!

(defspecial guile-ref (loc args)
  (pmatch args
    ((,module ,sym) (guard (and (list? module) (symbol? sym)))
     (make-module-ref loc module sym #t))))

;;; guile-primitive allows to create primitive references, which are
;;; still a little faster.

(defspecial guile-primitive (loc args)
  (pmatch args
    ((,sym)
     (make-primitive-ref loc sym))))

(defspecial function (loc args)
  (pmatch args
    (((lambda ,args . ,body))
     (compile-lambda loc '() args body))
    ((,sym) (guard (symbol? sym))
     (reference-variable loc sym function-slot))))

(defspecial defmacro (loc args)
  (pmatch args
    ((,name ,args . ,body)
     (if (not (symbol? name))
         (report-error loc "expected symbol as macro name" name)
         (let* ((tree-il
                 (make-sequence
                  loc
                  (list
                   (set-variable!
                    loc
                    name
                    function-slot
                    (make-application
                     loc
                     (make-module-ref loc '(guile) 'cons #t)
                     (list (make-const loc 'macro)
                           (compile-lambda loc
                                           `((name . ,name))
                                           args
                                           body))))
                   (make-const loc name)))))
           (compile (ensuring-globals loc bindings-data tree-il)
                    #:from 'tree-il
                    #:to 'value)
           tree-il)))))

(defspecial defun (loc args)
  (pmatch args
    ((,name ,args . ,body)
     (if (not (symbol? name))
         (report-error loc "expected symbol as function name" name)
         (make-sequence loc
                        (list (set-variable! loc
                                             name
                                             function-slot
                                             (compile-lambda loc
                                                             `((name . ,name))
                                                             args
                                                             body))
                              (make-const loc name)))))))

(defspecial #{`}# (loc args)
  (pmatch args
    ((,val)
     (process-backquote loc val))))

(defspecial quote (loc args)
  (pmatch args
    ((,val)
     (make-const loc val))))

(defspecial %funcall (loc args)
  (pmatch args
    ((,function . ,arguments)
     (make-application loc
                       (compile-expr function)
                       (map compile-expr arguments)))))

(defspecial %set-lexical-binding-mode (loc args)
  (pmatch args
    ((,val)
     (fluid-set! lexical-binding val)
     (make-void loc))))

;;; Compile a compound expression to Tree-IL.

(define (compile-pair loc expr)
  (let ((operator (car expr))
        (arguments (cdr expr)))
    (cond
     ((find-operator operator 'special-operator)
      => (lambda (special-operator-function)
           (special-operator-function loc arguments)))
     ((find-operator operator 'macro)
      => (lambda (macro-function)
           (compile-expr (apply macro-function arguments))))
     (else
      (compile-expr `(%funcall (function ,operator) ,@arguments))))))

;;; Compile a symbol expression.  This is a variable reference or maybe
;;; some special value like nil.

(define (compile-symbol loc sym)
  (case sym
    ((nil) (nil-value loc))
    ((t) (t-value loc))
    (else (reference-variable loc sym value-slot))))

;;; Compile a single expression to TreeIL.

(define (compile-expr expr)
  (let ((loc (location expr)))
    (cond
     ((symbol? expr)
      (compile-symbol loc expr))
     ((pair? expr)
      (compile-pair loc expr))
     (else (make-const loc expr)))))

;;; Process the compiler options.
;;; FIXME: Why is '(()) passed as options by the REPL?

(define (valid-symbol-list-arg? value)
  (or (eq? value 'all)
      (and (list? value) (and-map symbol? value))))

(define (process-options! opt)
  (if (and (not (null? opt))
           (not (equal? opt '(()))))
      (if (null? (cdr opt))
          (report-error #f "Invalid compiler options" opt)
          (let ((key (car opt))
                (value (cadr opt)))
            (case key
              ((#:warnings)             ; ignore
               #f)
              (else (report-error #f
                                  "Invalid compiler option"
                                  key)))))))

;;; Entry point for compilation to TreeIL.  This creates the bindings
;;; data structure, and after compiling the main expression we need to
;;; make sure all globals for symbols used during the compilation are
;;; created using the generate-ensure-global function.

(define (compile-tree-il expr env opts)
  (values
   (with-fluids ((bindings-data (make-bindings)))
     (process-options! opts)
     (let ((compiled (compile-expr expr)))
       (ensuring-globals (location expr) bindings-data compiled)))
   env
   env))
