;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013 Free Software Foundation, Inc.

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
  #:use-module ((language tree-il) #:hide (let-gensyms))
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
  (let-gensyms (name-sym bound?-sym kbox box)
    (build-cps-term
      ($letconst (('name name-sym name)
                  ('bound? bound?-sym bound?))
        ($letk ((kbox src ($kargs ('box) (box) ,(val-proc box))))
          ,(match (current-topbox-scope)
             (#f
              (build-cps-term
                ($continue kbox
                  ($primcall 'resolve
                             (name-sym bound?-sym)))))
             (scope
              (let-gensyms (scope-sym)
                (build-cps-term
                  ($letconst (('scope scope-sym scope))
                    ($continue kbox
                      ($primcall 'cached-toplevel-box
                                 (scope-sym name-sym bound?-sym)))))))))))))

(define (module-box src module name public? bound? val-proc)
  (let-gensyms (module-sym name-sym public?-sym bound?-sym kbox box)
    (build-cps-term
      ($letconst (('module module-sym module)
                  ('name name-sym name)
                  ('public? public?-sym public?)
                  ('bound? bound?-sym bound?))
        ($letk ((kbox src ($kargs ('box) (box) ,(val-proc box))))
          ($continue kbox
            ($primcall 'cached-module-box
                       (module-sym name-sym public?-sym bound?-sym))))))))

(define (capture-toplevel-scope src scope k)
  (let-gensyms (module scope-sym kmodule)
    (build-cps-term
      ($letconst (('scope scope-sym scope))
        ($letk ((kmodule src ($kargs ('module) (module)
                               ($continue k
                                 ($primcall 'cache-current-module!
                                            (module scope-sym))))))
          ($continue kmodule
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
            (unless (eq? var (car gensyms))
              (error "unexpected keyword arg order"))
            (proc name var (car inits)
                  (fold-kw kw (cdr gensyms) (cdr inits) seed)))))
       (fold-req req gensyms seed)))))

(define (unbound? src sym kt kf)
  (define tc8-iflag 4)
  (define unbound-val 9)
  (define unbound-bits (logior (ash unbound-val 8) tc8-iflag))
  (let-gensyms (unbound ktest)
    (build-cps-term
      ($letconst (('unbound unbound (pointer->scm (make-pointer unbound-bits))))
        ($letk ((ktest src ($kif kt kf)))
          ($continue ktest
            ($primcall 'eq? (sym unbound))))))))

(define (init-default-value name sym subst init body)
  (match (assq-ref subst sym)
    ((subst-sym box?)
     (let ((src (tree-il-src init)))
       (define (maybe-box k make-body)
         (if box?
             (let-gensyms (kbox phi)
               (build-cps-term
                 ($letk ((kbox src ($kargs (name) (phi)
                                     ($continue k ($primcall 'box (phi))))))
                   ,(make-body kbox))))
             (make-body k)))
       (let-gensyms (knext kbound kunbound)
         (build-cps-term
           ($letk ((knext src ($kargs (name) (subst-sym) ,body)))
             ,(maybe-box
               knext
               (lambda (k)
                 (build-cps-term
                   ($letk ((kbound src ($kargs () () ($continue k ($var sym))))
                           (kunbound src ($kargs () () ,(convert init k subst))))
                     ,(unbound? src sym kunbound kbound))))))))))))

;; exp k-name alist -> term
(define (convert exp k subst)
  ;; exp (v-name -> term) -> term
  (define (convert-arg exp k)
    (match exp
      (($ <lexical-ref> src name sym)
       (match (assq-ref subst sym)
         ((box #t)
          (let-gensyms (kunboxed unboxed)
            (build-cps-term
              ($letk ((kunboxed src ($kargs ('unboxed) (unboxed) ,(k unboxed))))
                ($continue kunboxed ($primcall 'box-ref (box)))))))
         ((subst #f) (k subst))
         (#f (k sym))))
      (else
       (let ((src (tree-il-src exp)))
         (let-gensyms (karg arg)
           (build-cps-term
             ($letk ((karg src ($kargs ('arg) (arg) ,(k arg))))
               ,(convert exp karg subst))))))))
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
    (match (assq-ref subst sym)
      ((box #t)
       (let-gensyms (k)
         (build-cps-term
           ($letk ((k #f ($kargs (name) (box) ,body)))
             ($continue k ($primcall 'box (sym)))))))
      (else body)))

  (match exp
    (($ <lexical-ref> src name sym)
     (match (assq-ref subst sym)
       ((box #t) (build-cps-term ($continue k ($primcall 'box-ref (box)))))
       ((subst #f) (build-cps-term ($continue k ($var subst))))
       (#f (build-cps-term ($continue k ($var sym))))))

    (($ <void> src)
     (build-cps-term ($continue k ($void))))

    (($ <const> src exp)
     (build-cps-term ($continue k ($const exp))))

    (($ <primitive-ref> src name)
     (build-cps-term ($continue k ($prim name))))

    (($ <lambda> fun-src meta body)
     (let ()
       (define (convert-clauses body ktail)
         (match body
           (#f '())
           (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
            (let* ((arity (make-$arity req (or opt '()) rest
                                       (if kw (cdr kw) '()) (and kw (car kw))))
                   (names (fold-formals (lambda (name sym init names)
                                          (cons name names))
                                        '()
                                        arity gensyms inits)))
              (cons
               (let-gensyms (kclause kargs)
                 (build-cps-cont
                   (kclause
                    src
                    ($kclause ,arity
                      (kargs
                       src
                       ($kargs names gensyms
                         ,(fold-formals
                           (lambda (name sym init body)
                             (if init
                                 (init-default-value name sym subst init body)
                                 (box-bound-var name sym body)))
                           (convert body ktail subst)
                           arity gensyms inits)))))))
               (convert-clauses alternate ktail))))))
       (if (current-topbox-scope)
           (let-gensyms (kentry self ktail)
             (build-cps-term
               ($continue k
                 ($fun meta '()
                   (kentry fun-src
                           ($kentry self (ktail #f ($ktail))
                                    ,(convert-clauses body ktail)))))))
           (let-gensyms (scope kscope)
             (build-cps-term
               ($letk ((kscope fun-src
                               ($kargs () ()
                                 ,(parameterize ((current-topbox-scope scope))
                                    (convert exp k subst)))))
                 ,(capture-toplevel-scope fun-src scope kscope)))))))

    (($ <module-ref> src mod name public?)
     (module-box
      src mod name public? #t
      (lambda (box)
        (build-cps-term ($continue k ($primcall 'box-ref (box)))))))

    (($ <module-set> src mod name public? exp)
     (convert-arg exp
       (lambda (val)
         (module-box
          src mod name public? #f
          (lambda (box)
            (build-cps-term ($continue k ($primcall 'box-set! (box val)))))))))

    (($ <toplevel-ref> src name)
     (toplevel-box
      src name #t
      (lambda (box)
        (build-cps-term ($continue k ($primcall 'box-ref (box)))))))

    (($ <toplevel-set> src name exp)
     (convert-arg exp
       (lambda (val)
         (toplevel-box
          src name #f
          (lambda (box)
            (build-cps-term ($continue k ($primcall 'box-set! (box val)))))))))

    (($ <toplevel-define> src name exp)
     (convert-arg exp
       (lambda (val)
         (let-gensyms (kname name-sym)
           (build-cps-term
             ($letconst (('name name-sym name))
               ($continue k ($primcall 'define! (name-sym val)))))))))

    (($ <call> src proc args)
     (convert-args (cons proc args)
       (match-lambda
        ((proc . args)
         (build-cps-term ($continue k ($call proc args)))))))

    (($ <primcall> src name args)
     (cond
      ((branching-primitive? name)
       (convert (make-conditional src exp (make-const #f #t)
                                  (make-const #f #f))
                k subst))
      ((and (eq? name 'vector)
            (and-map (match-lambda
                      ((or ($ <const>)
                           ($ <void>)
                           ($ <lambda>)
                           ($ <lexical-ref>)) #t)
                      (_ #f))
                     args))
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
       (convert (let ((len (length args)))
                  (let-gensyms (v)
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
        k subst))
      (else
       (convert-args args
         (lambda (args)
           (build-cps-term ($continue k ($primcall name args))))))))

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
         (let ((hnames (append hreq (if hrest (list hrest) '()))))
           (let-gensyms (khargs khbody kret kprim prim kpop krest vals kbody)
             (build-cps-term
               ($letk* ((khbody hsrc ($kargs hnames hsyms
                                       ,(fold box-bound-var
                                              (convert hbody k subst)
                                              hnames hsyms)))
                        (khargs hsrc ($ktrunc hreq hrest khbody))
                        (kpop src
                              ($kargs ('rest) (vals)
                                ($letk ((kret
                                         src
                                         ($kargs () ()
                                           ($letk ((kprim
                                                    src
                                                    ($kargs ('prim) (prim)
                                                      ($continue k
                                                        ($primcall 'apply
                                                                   (prim vals))))))
                                             ($continue kprim
                                               ($prim 'values))))))
                                  ($continue kret
                                    ($primcall 'unwind ())))))
                        (krest src ($ktrunc '() 'rest kpop)))
                 ,(if escape-only?
                      (build-cps-term
                        ($letk ((kbody (tree-il-src body) 
                                       ($kargs () ()
                                         ,(convert body krest subst))))
                          ($continue kbody ($prompt #t tag khargs kpop))))
                      (convert-arg body
                        (lambda (thunk)
                          (build-cps-term
                            ($letk ((kbody (tree-il-src body) 
                                           ($kargs () ()
                                             ($continue krest
                                               ($primcall 'call-thunk/no-inline
                                                          (thunk))))))
                              ($continue kbody
                                ($prompt #f tag khargs kpop))))))))))))))

    ;; Eta-convert prompts without inline handlers.
    (($ <prompt> src escape-only? tag body handler)
     (let-gensyms (h args)
       (convert
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
             #f)))))
        k
        subst)))

    (($ <abort> src tag args ($ <const> _ ()))
     (convert-args (cons tag args)
       (lambda (args*)
         (build-cps-term
           ($continue k ($primcall 'abort-to-prompt args*))))))

    (($ <abort> src tag args tail)
     (convert-args (append (list (make-primitive-ref #f 'abort-to-prompt)
                                 tag)
                           args
                           (list tail))
       (lambda (args*)
         (build-cps-term
           ($continue k ($primcall 'apply args*))))))

    (($ <conditional> src test consequent alternate)
     (let-gensyms (kif kt kf)
       (build-cps-term
         ($letk* ((kt (tree-il-src consequent) ($kargs () ()
                                                 ,(convert consequent k subst)))
                  (kf (tree-il-src alternate) ($kargs () ()
                                                ,(convert alternate k subst)))
                  (kif src ($kif kt kf)))
           ,(match test
              (($ <primcall> src (? branching-primitive? name) args)
               (convert-args args
                 (lambda (args)
                   (build-cps-term ($continue kif ($primcall name args))))))
              (_ (convert-arg test
                   (lambda (test)
                     (build-cps-term ($continue kif ($var test)))))))))))

    (($ <lexical-set> src name gensym exp)
     (convert-arg exp
       (lambda (exp)
         (match (assq-ref subst gensym)
           ((box #t)
            (build-cps-term
              ($continue k ($primcall 'box-set! (box exp)))))))))

    (($ <seq> src head tail)
     (let-gensyms (ktrunc kseq)
       (build-cps-term
         ($letk* ((kseq (tree-il-src tail) ($kargs () ()
                                             ,(convert tail k subst)))
                  (ktrunc src ($ktrunc '() #f kseq)))
           ,(convert head ktrunc subst)))))

    (($ <let> src names syms vals body)
     (let lp ((names names) (syms syms) (vals vals))
       (match (list names syms vals)
         ((() () ()) (convert body k subst))
         (((name . names) (sym . syms) (val . vals))
          (let-gensyms (klet)
            (build-cps-term
              ($letk ((klet src ($kargs (name) (sym)
                                  ,(box-bound-var name sym
                                                  (lp names syms vals)))))
                ,(convert val klet subst))))))))

    (($ <fix> src names gensyms funs body)
     ;; Some letrecs can be contified; that happens later.
     (if (current-topbox-scope)
         (let-gensyms (self)
           (build-cps-term
             ($letrec names
                      gensyms
                      (map (lambda (fun)
                             (match (convert fun k subst)
                               (($ $continue _ (and fun ($ $fun)))
                                fun)))
                           funs)
                      ,(convert body k subst))))
         (let-gensyms (scope kscope)
           (build-cps-term
             ($letk ((kscope src ($kargs () ()
                                   ,(parameterize ((current-topbox-scope scope))
                                      (convert exp k subst)))))
               ,(capture-toplevel-scope src scope kscope))))))

    (($ <let-values> src exp
        ($ <lambda-case> lsrc req #f rest #f () syms body #f))
     (let ((names (append req (if rest (list rest) '()))))
       (let-gensyms (ktrunc kargs)
         (build-cps-term
           ($letk* ((kargs src ($kargs names syms
                                 ,(fold box-bound-var
                                        (convert body k subst)
                                        names syms)))
                    (ktrunc src ($ktrunc req rest kargs)))
             ,(convert exp ktrunc subst))))))))

(define (build-subst exp)
  "Compute a mapping from lexical gensyms to substituted gensyms.  The
usual reason to replace one variable by another is assignment
conversion.  Default argument values is the other reason.

Returns a list of (ORIG-SYM SUBST-SYM BOXED?).  A true value for BOXED?
indicates that the replacement variable is in a box."
  (define (box-set-vars exp subst)
    (match exp
      (($ <lexical-set> src name sym exp)
       (if (assq sym subst)
           subst
           (cons (list sym (gensym "b") #t) subst)))
      (_ subst)))
  (define (default-args exp subst)
    (match exp
      (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
       (fold-formals (lambda (name sym init subst)
                       (if init
                           (let ((box? (match (assq-ref subst sym)
                                         ((box #t) #t)
                                         (#f #f)))
                                 (subst-sym (gensym (symbol->string name))))
                             (cons (list sym subst-sym box?) subst))
                           subst))
                     subst
                     (make-$arity req (or opt '()) rest
                                  (if kw (cdr kw) '()) (and kw (car kw)))
                     gensyms
                     inits))
      (_ subst)))
  (tree-il-fold box-set-vars default-args '() exp))

(define (cps-convert/thunk exp)
  (let ((src (tree-il-src exp)))
    (let-gensyms (kinit init ktail kclause kbody)
      (build-cps-exp
        ($fun '() '()
          (kinit src
                 ($kentry init
                   (ktail #f ($ktail))
                   ((kclause src
                            ($kclause ('() '() #f '() #f)
                              (kbody src
                                     ($kargs () ()
                                       ,(convert exp ktail
                                                 (build-subst exp))))))))))))))

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

(define (compile-cps exp env opts)
  (values (cps-convert/thunk (optimize-tree-il exp env opts))
          env
          env))

;;; Local Variables:
;;; eval: (put 'convert-arg 'scheme-indent-function 1)
;;; eval: (put 'convert-args 'scheme-indent-function 1)
;;; End:
