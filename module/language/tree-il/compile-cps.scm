;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

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

;;; Commentary:
;;;
;;; This pass converts Tree-IL to the continuation-passing style (CPS)
;;; language.
;;;
;;; CPS is a lower-level representation than Tree-IL.  Converting to
;;; CPS, beyond adding names for all control points and all values,
;;; simplifies expressions in the following ways, among others:
;;;
;;;   * Fixing the order of evaluation.
;;;
;;;   * Converting assigned variables to boxed variables.
;;;
;;;   * Requiring that Scheme's <letrec> has already been lowered to
;;;     <fix>.
;;;
;;;   * Inlining default-value initializers into lambda-case
;;;     expressions.
;;;
;;;   * Inlining prompt bodies.
;;;
;;;   * Turning toplevel and module references into primcalls.  This
;;;     involves explicitly modelling the "scope" of toplevel lookups
;;;     (indicating the module with respect to which toplevel bindings
;;;     are resolved).
;;;
;;; The utility of CPS is that it gives a name to everything: every
;;; intermediate value, and every control point (continuation).  As such
;;; it is more verbose than Tree-IL, but at the same time more simple as
;;; the number of concepts is reduced.
;;;
;;; Code:

(define-module (language tree-il compile-cps)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold fold-right filter-map))
  #:use-module (srfi srfi-26)
  #:use-module ((system foreign) #:select (make-pointer pointer->scm))
  #:use-module (language cps)
  #:use-module (language cps primitives)
  #:use-module (language tree-il analyze)
  #:use-module (language tree-il optimize)
  #:use-module (language tree-il)
  #:export (compile-cps))

;;; Guile's semantics are that a toplevel lambda captures a reference on
;;; the current module, and that all contained lambdas use that module
;;; to resolve toplevel variables.  This parameter tracks whether or not
;;; we are in a toplevel lambda.  If we are in a lambda, the parameter
;;; is bound to a fresh name identifying the module that was current
;;; when the toplevel lambda is defined.
;;;
;;; This is more complicated than it need be.  Ideally we should resolve
;;; all toplevel bindings to bindings from specific modules, unless the
;;; binding is unbound.  This is always valid if the compilation unit
;;; sets the module explicitly, as when compiling a module, but it
;;; doesn't work for files auto-compiled for use with `load'.
;;;
(define current-topbox-scope (make-parameter #f))

(define (toplevel-box src name bound? val-proc)
  (let-fresh (kbox) (name-sym bound?-sym box)
    (build-cps-term
      ($letconst (('name name-sym name)
                  ('bound? bound?-sym bound?))
        ($letk ((kbox ($kargs ('box) (box) ,(val-proc box))))
          ,(match (current-topbox-scope)
             (#f
              (build-cps-term
                ($continue kbox src
                  ($primcall 'resolve
                             (name-sym bound?-sym)))))
             (scope
              (let-fresh () (scope-sym)
                (build-cps-term
                  ($letconst (('scope scope-sym scope))
                    ($continue kbox src
                      ($primcall 'cached-toplevel-box
                                 (scope-sym name-sym bound?-sym)))))))))))))

(define (module-box src module name public? bound? val-proc)
  (let-fresh (kbox) (module-sym name-sym public?-sym bound?-sym box)
    (build-cps-term
      ($letconst (('module module-sym module)
                  ('name name-sym name)
                  ('public? public?-sym public?)
                  ('bound? bound?-sym bound?))
        ($letk ((kbox ($kargs ('box) (box) ,(val-proc box))))
          ($continue kbox src
            ($primcall 'cached-module-box
                       (module-sym name-sym public?-sym bound?-sym))))))))

(define (capture-toplevel-scope src scope k)
  (let-fresh (kmodule) (module scope-sym)
    (build-cps-term
      ($letconst (('scope scope-sym scope))
        ($letk ((kmodule ($kargs ('module) (module)
                           ($continue k src
                             ($primcall 'cache-current-module!
                                        (module scope-sym))))))
          ($continue kmodule src
            ($primcall 'current-module ())))))))

(define (fold-formals proc seed arity gensyms inits)
  (match arity
    (($ $arity req opt rest kw allow-other-keys?)
     (let ()
       (define (fold-req names gensyms seed)
         (match names
           (() (fold-opt opt gensyms inits seed))
           ((name . names)
            (proc name (car gensyms) #f
                  (fold-req names (cdr gensyms) seed)))))
       (define (fold-opt names gensyms inits seed)
         (match names
           (() (fold-rest rest gensyms inits seed))
           ((name . names)
            (proc name (car gensyms) (car inits)
                  (fold-opt names (cdr gensyms) (cdr inits) seed)))))
       (define (fold-rest rest gensyms inits seed)
         (match rest
           (#f (fold-kw kw gensyms inits seed))
           (name (proc name (car gensyms) #f
                       (fold-kw kw (cdr gensyms) inits seed)))))
       (define (fold-kw kw gensyms inits seed)
         (match kw
           (()
            (unless (null? gensyms)
              (error "too many gensyms"))
            (unless (null? inits)
              (error "too many inits"))
            seed)
           (((key name var) . kw)
            ;; Could be that var is not a gensym any more.
            (when (symbol? var)
              (unless (eq? var (car gensyms))
                (error "unexpected keyword arg order")))
            (proc name (car gensyms) (car inits)
                  (fold-kw kw (cdr gensyms) (cdr inits) seed)))))
       (fold-req req gensyms seed)))))

(define (unbound? src var kt kf)
  (define tc8-iflag 4)
  (define unbound-val 9)
  (define unbound-bits (logior (ash unbound-val 8) tc8-iflag))
  (let-fresh (ktest) (unbound)
    (build-cps-term
      ($letconst (('unbound unbound
                            (pointer->scm (make-pointer unbound-bits))))
        ($letk ((ktest ($kif kt kf)))
          ($continue ktest src
            ($primcall 'eq? (var unbound))))))))

(define (init-default-value name sym subst init body)
  (match (hashq-ref subst sym)
    ((orig-var subst-var box?)
     (let ((src (tree-il-src init)))
       (define (maybe-box k make-body)
         (if box?
             (let-fresh (kbox) (phi)
               (build-cps-term
                 ($letk ((kbox ($kargs (name) (phi)
                                 ($continue k src ($primcall 'box (phi))))))
                   ,(make-body kbox))))
             (make-body k)))
       (let-fresh (knext kbound kunbound kreceive krest) (val rest)
         (build-cps-term
           ($letk ((knext ($kargs (name) (subst-var) ,body)))
             ,(maybe-box
               knext
               (lambda (k)
                 (build-cps-term
                   ($letk ((kbound ($kargs () () ($continue k src
                                                   ($values (orig-var)))))
                           (krest ($kargs (name 'rest) (val rest)
                                    ($continue k src ($values (val)))))
                           (kreceive ($kreceive (list name) 'rest krest))
                           (kunbound ($kargs () ()
                                       ,(convert init kreceive subst))))
                     ,(unbound? src orig-var kunbound kbound))))))))))))

;; exp k-name alist -> term
(define (convert exp k subst)
  ;; exp (v-name -> term) -> term
  (define (convert-arg exp k)
    (match exp
      (($ <lexical-ref> src name sym)
       (match (hashq-ref subst sym)
         ((orig-var box #t)
          (let-fresh (kunboxed) (unboxed)
            (build-cps-term
              ($letk ((kunboxed ($kargs ('unboxed) (unboxed) ,(k unboxed))))
                ($continue kunboxed src ($primcall 'box-ref (box)))))))
         ((orig-var subst-var #f) (k subst-var))
         (var (k var))))
      (else
       (let-fresh (kreceive karg) (arg rest)
         (build-cps-term
           ($letk ((karg ($kargs ('arg 'rest) (arg rest) ,(k arg)))
                   (kreceive ($kreceive '(arg) 'rest karg)))
             ,(convert exp kreceive subst)))))))
  ;; (exp ...) ((v-name ...) -> term) -> term
  (define (convert-args exps k)
    (match exps
      (() (k '()))
      ((exp . exps)
       (convert-arg exp
         (lambda (name)
           (convert-args exps
             (lambda (names)
               (k (cons name names)))))))))
  (define (box-bound-var name sym body)
    (match (hashq-ref subst sym)
      ((orig-var subst-var #t)
       (let-fresh (k) ()
         (build-cps-term
           ($letk ((k ($kargs (name) (subst-var) ,body)))
             ($continue k #f ($primcall 'box (orig-var)))))))
      (else body)))
  (define (bound-var sym)
    (match (hashq-ref subst sym)
      ((var . _) var)
      ((? exact-integer? var) var)))

  (match exp
    (($ <lexical-ref> src name sym)
     (rewrite-cps-term (hashq-ref subst sym)
       ((orig-var box #t) ($continue k src ($primcall 'box-ref (box))))
       ((orig-var subst-var #f) ($continue k src ($values (subst-var))))
       (var ($continue k src ($values (var))))))

    (($ <void> src)
     (build-cps-term ($continue k src ($void))))

    (($ <const> src exp)
     (build-cps-term ($continue k src ($const exp))))

    (($ <primitive-ref> src name)
     (build-cps-term ($continue k src ($prim name))))

    (($ <lambda> fun-src meta body)
     (let ()
       (define (convert-clauses body ktail)
         (match body
           (#f '())
           (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
            (let* ((arity (make-$arity req (or opt '()) rest
                                       (map (match-lambda
                                             ((kw name sym) 
                                              (list kw name (bound-var sym))))
                                            (if kw (cdr kw) '()))
                                       (and kw (car kw))))
                   (names (fold-formals (lambda (name sym init names)
                                          (cons name names))
                                        '()
                                        arity gensyms inits)))
              (cons
               (let ((bound-vars (map bound-var gensyms)))
                 (let-fresh (kclause kargs) ()
                   (build-cps-cont
                     (kclause
                      ($kclause ,arity
                        (kargs
                         ($kargs names bound-vars
                           ,(fold-formals
                             (lambda (name sym init body)
                               (if init
                                   (init-default-value name sym subst init body)
                                   (box-bound-var name sym body)))
                             (convert body ktail subst)
                             arity gensyms inits))))))))
               (convert-clauses alternate ktail))))))
       (if (current-topbox-scope)
           (let-fresh (kentry ktail) (self)
             (build-cps-term
               ($continue k fun-src
                 ($fun fun-src meta '()
                       (kentry ($kentry self (ktail ($ktail))
                                 ,(convert-clauses body ktail)))))))
           (let-fresh (kscope) (scope)
             (build-cps-term
               ($letk ((kscope ($kargs () ()
                                 ,(parameterize ((current-topbox-scope scope))
                                    (convert exp k subst)))))
                 ,(capture-toplevel-scope fun-src scope kscope)))))))

    (($ <module-ref> src mod name public?)
     (module-box
      src mod name public? #t
      (lambda (box)
        (build-cps-term ($continue k src ($primcall 'box-ref (box)))))))

    (($ <module-set> src mod name public? exp)
     (convert-arg exp
       (lambda (val)
         (module-box
          src mod name public? #f
          (lambda (box)
            (build-cps-term
              ($continue k src ($primcall 'box-set! (box val)))))))))

    (($ <toplevel-ref> src name)
     (toplevel-box
      src name #t
      (lambda (box)
        (build-cps-term ($continue k src ($primcall 'box-ref (box)))))))

    (($ <toplevel-set> src name exp)
     (convert-arg exp
       (lambda (val)
         (toplevel-box
          src name #f
          (lambda (box)
            (build-cps-term
              ($continue k src ($primcall 'box-set! (box val)))))))))

    (($ <toplevel-define> src name exp)
     (convert-arg exp
       (lambda (val)
         (let-fresh (kname) (name-sym)
           (build-cps-term
             ($letconst (('name name-sym name))
               ($continue k src ($primcall 'define! (name-sym val)))))))))

    (($ <call> src proc args)
     (convert-args (cons proc args)
       (match-lambda
        ((proc . args)
         (build-cps-term ($continue k src ($call proc args)))))))

    (($ <primcall> src name args)
     (cond
      ((branching-primitive? name)
       (convert-args args
         (lambda (args)
           (let-fresh (kt kf kif) ()
             (build-cps-term
               ($letk ((kt ($kargs () () ($continue k src ($const #t))))
                       (kf ($kargs () () ($continue k src ($const #f))))
                       (kif ($kif kt kf)))
                 ($continue kif src ($primcall name args))))))))
      ((and (eq? name 'list)
            (and-map (match-lambda
                      ((or ($ <const>)
                           ($ <void>)
                           ($ <lambda>)
                           ($ <lexical-ref>)) #t)
                      (_ #f))
                     args))
       ;; See note below in `canonicalize' about `vector'.  The same
       ;; thing applies to `list'.
       (let lp ((args args) (k k))
         (match args
           (()
            (build-cps-term
              ($continue k src ($const '()))))
           ((arg . args)
            (let-fresh (ktail) (tail)
              (build-cps-term
                ($letk ((ktail ($kargs ('tail) (tail)
                                 ,(convert-arg arg
                                    (lambda (head)
                                      (build-cps-term
                                        ($continue k src
                                          ($primcall 'cons (head tail)))))))))
                  ,(lp args ktail))))))))
      (else
       (convert-args args
         (lambda (args)
           (build-cps-term ($continue k src ($primcall name args))))))))

    ;; Prompts with inline handlers.
    (($ <prompt> src escape-only? tag body
        ($ <lambda> hsrc hmeta
           ($ <lambda-case> _ hreq #f hrest #f () hsyms hbody #f)))
     ;; Handler:
     ;;   khargs: check args returned to handler, -> khbody
     ;;   khbody: the handler, -> k
     ;;
     ;; Post-body:
     ;;   krest: collect return vals from body to list, -> kpop
     ;;   kpop: pop the prompt, -> kprim
     ;;   kprim: load the values primitive, -> kret
     ;;   kret: (apply values rvals), -> k
     ;;
     ;; Escape prompts evaluate the body with the continuation of krest.
     ;; Otherwise we do a no-inline call to body, continuing to krest.
     (convert-arg tag
       (lambda (tag)
         (let ((hnames (append hreq (if hrest (list hrest) '())))
               (bound-vars (map bound-var hsyms)))
           (let-fresh (khargs khbody kret kprim kpop krest kbody) (prim vals)
             (build-cps-term
               ;; FIXME: Attach hsrc to $kreceive.
               ($letk* ((khbody ($kargs hnames bound-vars
                                  ,(fold box-bound-var
                                         (convert hbody k subst)
                                         hnames hsyms)))
                        (khargs ($kreceive hreq hrest khbody))
                        (kpop ($kargs ('rest) (vals)
                                ($letk ((kret
                                         ($kargs () ()
                                           ($letk ((kprim
                                                    ($kargs ('prim) (prim)
                                                      ($continue k src
                                                        ($primcall 'apply
                                                                   (prim vals))))))
                                             ($continue kprim src
                                               ($prim 'values))))))
                                  ($continue kret src
                                    ($primcall 'unwind ())))))
                        (krest ($kreceive '() 'rest kpop)))
                 ,(if escape-only?
                      (build-cps-term
                        ($letk ((kbody ($kargs () ()
                                         ,(convert body krest subst))))
                          ($continue kbody src ($prompt #t tag khargs))))
                      (convert-arg body
                        (lambda (thunk)
                          (build-cps-term
                            ($letk ((kbody ($kargs () ()
                                             ($continue krest (tree-il-src body)
                                               ($primcall 'call-thunk/no-inline
                                                          (thunk))))))
                              ($continue kbody (tree-il-src body)
                                ($prompt #f tag khargs))))))))))))))

    (($ <abort> src tag args ($ <const> _ ()))
     (convert-args (cons tag args)
       (lambda (args*)
         (build-cps-term
           ($continue k src
             ($primcall 'abort-to-prompt args*))))))

    (($ <abort> src tag args tail)
     (convert-args (append (list (make-primitive-ref #f 'abort-to-prompt)
                                 tag)
                           args
                           (list tail))
       (lambda (args*)
         (build-cps-term
           ($continue k src ($primcall 'apply args*))))))

    (($ <conditional> src test consequent alternate)
     (let-fresh (kif kt kf) ()
       (build-cps-term
         ($letk* ((kt ($kargs () () ,(convert consequent k subst)))
                  (kf ($kargs () () ,(convert alternate k subst)))
                  (kif ($kif kt kf)))
           ,(match test
              (($ <primcall> src (? branching-primitive? name) args)
               (convert-args args
                 (lambda (args)
                   (build-cps-term
                     ($continue kif src ($primcall name args))))))
              (_ (convert-arg test
                   (lambda (test)
                     (build-cps-term
                       ($continue kif src ($values (test))))))))))))

    (($ <lexical-set> src name gensym exp)
     (convert-arg exp
       (lambda (exp)
         (match (hashq-ref subst gensym)
           ((orig-var box #t)
            (build-cps-term
              ($continue k src ($primcall 'box-set! (box exp)))))))))

    (($ <seq> src head tail)
     (let-fresh (kreceive kseq) (vals)
       (build-cps-term
         ($letk* ((kseq ($kargs ('vals) (vals)
                          ,(convert tail k subst)))
                  (kreceive ($kreceive '() 'vals kseq)))
           ,(convert head kreceive subst)))))

    (($ <let> src names syms vals body)
     (let lp ((names names) (syms syms) (vals vals))
       (match (list names syms vals)
         ((() () ()) (convert body k subst))
         (((name . names) (sym . syms) (val . vals))
          (let-fresh (kreceive klet) (rest)
            (build-cps-term
              ($letk* ((klet ($kargs (name 'rest) ((bound-var sym) rest)
                               ,(box-bound-var name sym
                                               (lp names syms vals))))
                       (kreceive ($kreceive (list name) 'rest klet)))
                ,(convert val kreceive subst))))))))

    (($ <fix> src names gensyms funs body)
     ;; Some letrecs can be contified; that happens later.
     (if (current-topbox-scope)
         (let-fresh () (self)
           (build-cps-term
             ($letrec names
                      (map bound-var gensyms)
                      (map (lambda (fun)
                             (match (convert fun k subst)
                               (($ $continue _ _ (and fun ($ $fun)))
                                fun)))
                           funs)
                      ,(convert body k subst))))
         (let-fresh (kscope) (scope)
           (build-cps-term
             ($letk ((kscope ($kargs () ()
                               ,(parameterize ((current-topbox-scope scope))
                                  (convert exp k subst)))))
               ,(capture-toplevel-scope src scope kscope))))))

    (($ <let-values> src exp
        ($ <lambda-case> lsrc req #f rest #f () syms body #f))
     (let ((names (append req (if rest (list rest) '())))
           (bound-vars (map bound-var syms)))
       (let-fresh (kreceive kargs) ()
         (build-cps-term
           ($letk* ((kargs ($kargs names bound-vars
                             ,(fold box-bound-var
                                    (convert body k subst)
                                    names syms)))
                    (kreceive ($kreceive req rest kargs)))
             ,(convert exp kreceive subst))))))))

(define (build-subst exp)
  "Compute a mapping from lexical gensyms to CPS variable indexes.  CPS
uses small integers to identify variables, instead of gensyms.

This subst table serves an additional purpose of mapping variables to
replacements.  The usual reason to replace one variable by another is
assignment conversion.  Default argument values is the other reason.

The result is a hash table mapping symbols to substitutions (in the case
that a variable is substituted) or to indexes.  A substitution is a list
of the form:

  (ORIG-INDEX SUBST-INDEX BOXED?)

A true value for BOXED?  indicates that the replacement variable is in a
box.  If a variable is not substituted, the mapped value is a small
integer."
  (let ((table (make-hash-table)))
    (define (down exp)
      (match exp
        (($ <lexical-set> src name sym exp)
         (match (hashq-ref table sym)
           ((orig subst #t) #t)
           ((orig subst #f) (hashq-set! table sym (list orig subst #t)))
           ((? number? idx) (hashq-set! table sym (list idx (fresh-var) #t)))))
        (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
         (fold-formals (lambda (name sym init seed)
                         (hashq-set! table sym
                                     (if init
                                         (list (fresh-var) (fresh-var) #f)
                                         (fresh-var))))
                       #f
                       (make-$arity req (or opt '()) rest
                                    (if kw (cdr kw) '()) (and kw (car kw)))
                       gensyms
                       inits))
        (($ <let> src names gensyms vals body)
         (for-each (lambda (sym)
                     (hashq-set! table sym (fresh-var)))
                   gensyms))
        (($ <fix> src names gensyms vals body)
         (for-each (lambda (sym)
                     (hashq-set! table sym (fresh-var)))
                   gensyms))
        (_ #t))
      (values))
    (define (up exp) (values))
    ((make-tree-il-folder) exp down up)
    table))

(define (cps-convert/thunk exp)
  (parameterize ((label-counter 0)
                 (var-counter 0))
    (let ((src (tree-il-src exp)))
      (let-fresh (kinit ktail kclause kbody) (init)
        (build-cps-exp
          ($fun src '() '()
                (kinit ($kentry init
                           (ktail ($ktail))
                         ((kclause
                           ($kclause ('() '() #f '() #f)
                             (kbody ($kargs () ()
                                      ,(convert exp ktail
                                                (build-subst exp)))))))))))))))

(define *comp-module* (make-fluid))

(define %warning-passes
  `((unused-variable     . ,unused-variable-analysis)
    (unused-toplevel     . ,unused-toplevel-analysis)
    (unbound-variable    . ,unbound-variable-analysis)
    (arity-mismatch      . ,arity-analysis)
    (format              . ,format-analysis)))

(define (optimize-tree-il x e opts)
  (define warnings
    (or (and=> (memq #:warnings opts) cadr)
        '()))

  ;; Go through the warning passes.
  (let ((analyses (filter-map (lambda (kind)
                                (assoc-ref %warning-passes kind))
                              warnings)))
    (analyze-tree analyses x e))

  (optimize x e opts))

(define (canonicalize exp)
  (post-order
   (lambda (exp)
     (match exp
       (($ <primcall> src 'vector
           (and args
                ((or ($ <const>) ($ <void>) ($ <lambda>) ($ <lexical-ref>))
                 ...)))
        ;; Some macros generate calls to "vector" with like 300
        ;; arguments.  Since we eventually compile to make-vector and
        ;; vector-set!, it reduces live variable pressure to allocate the
        ;; vector first, then set values as they are produced, if we can
        ;; prove that no value can capture the continuation.  (More on
        ;; that caveat here:
        ;; http://wingolog.org/archives/2013/11/02/scheme-quiz-time).
        ;;
        ;; Normally we would do this transformation in the compiler, but
        ;; it's quite tricky there and quite easy here, so hold your nose
        ;; while we drop some smelly code.
        (let ((len (length args))
              (v (gensym "v ")))
          (make-let src
                    (list 'v)
                    (list v)
                    (list (make-primcall src 'make-vector
                                         (list (make-const #f len)
                                               (make-const #f #f))))
                    (fold (lambda (arg n tail)
                            (make-seq
                             src
                             (make-primcall
                              src 'vector-set!
                              (list (make-lexical-ref src 'v v)
                                    (make-const #f n)
                                    arg))
                             tail))
                          (make-lexical-ref src 'v v)
                          (reverse args) (reverse (iota len))))))

       (($ <prompt> src escape-only? tag body
           ($ <lambda> hsrc hmeta
              ($ <lambda-case> _ hreq #f hrest #f () hsyms hbody #f)))
        exp)

       ;; Eta-convert prompts without inline handlers.
       (($ <prompt> src escape-only? tag body handler)
        (let ((h (gensym "h "))
              (args (gensym "args ")))
          (make-let
           src (list 'h) (list h) (list handler)
           (make-seq
            src
            (make-conditional
             src
             (make-primcall src 'procedure? (list (make-lexical-ref #f 'h h)))
             (make-void src)
             (make-primcall
              src 'scm-error
              (list
               (make-const #f 'wrong-type-arg)
               (make-const #f "call-with-prompt")
               (make-const #f "Wrong type (expecting procedure): ~S")
               (make-primcall #f 'list (list (make-lexical-ref #f 'h h)))
               (make-primcall #f 'list (list (make-lexical-ref #f 'h h))))))
            (make-prompt
             src escape-only? tag body
             (make-lambda
              src '()
              (make-lambda-case
               src '() #f 'args #f '() (list args)
               (make-primcall
                src 'apply
                (list (make-lexical-ref #f 'h h)
                      (make-lexical-ref #f 'args args)))
               #f)))))))
       (_ exp)))
   exp))

(define (compile-cps exp env opts)
  (values (cps-convert/thunk
           (canonicalize (optimize-tree-il exp env opts)))
          env
          env))

;;; Local Variables:
;;; eval: (put 'convert-arg 'scheme-indent-function 1)
;;; eval: (put 'convert-args 'scheme-indent-function 1)
;;; End:
