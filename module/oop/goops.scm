;;; installed-scm-file

;;;; Copyright (C) 1998,1999,2000-2003,2006,2009-2011,2013-2015 Free Software Foundation, Inc.
;;;; Copyright (C) 1993-1998 Erick Gallesio - I3S-CNRS/ESSI <eg@unice.fr>
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


;;;;
;;;; This file was based upon stklos.stk from the STk distribution
;;;; version 4.0.1 by Erick Gallesio <eg@unice.fr>.
;;;;

(define-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (oop goops util)
  #:use-module (system base target)
  #:export-syntax (define-class class standard-define-class
                    define-generic define-accessor define-method
                    define-extended-generic define-extended-generics
                    method)
  #:export ( ;; The root of everything.
            <top>
            <class> <object>

            ;; Slot types.
            <foreign-slot> <protected-slot> <hidden-slot> <opaque-slot>
            <read-only-slot> <self-slot> <protected-opaque-slot>
            <protected-hidden-slot> <protected-read-only-slot>
            <scm-slot> <int-slot> <float-slot> <double-slot>

            ;; Methods are implementations of generic functions.
            <method> <accessor-method>

            ;; Applicable objects, either procedures or applicable structs.
            <procedure-class> <applicable>
            <procedure> <primitive-generic>

            ;; Applicable structs.
            <applicable-struct-class> <applicable-struct-with-setter-class>
            <applicable-struct> <applicable-struct-with-setter>
            <generic> <extended-generic>
            <generic-with-setter> <extended-generic-with-setter>
            <accessor> <extended-accessor>

            ;; Types with their own allocated typecodes.
            <boolean> <char> <list> <pair> <null> <string> <symbol>
            <vector> <bytevector> <uvec> <foreign> <hashtable>
            <fluid> <dynamic-state> <frame> <vm> <vm-continuation>
            <keyword>

            ;; Numbers.
            <number> <complex> <real> <integer> <fraction>

            ;; Unknown.
            <unknown>

            ;; Particular SMOB data types.  All SMOB types have
            ;; corresponding classes, which may be obtained via class-of,
            ;; once you have an instance.  Perhaps FIXME to provide a
            ;; smob-type-name->class procedure.
            <arbiter> <promise> <thread> <mutex> <condition-variable>
            <regexp> <hook> <bitvector> <random-state> <async>
            <directory> <array> <character-set>
            <dynamic-object> <guardian> <macro>

            ;; Modules.
            <module>

            ;; Ports.
            <port> <input-port> <output-port> <input-output-port>

            ;; Like SMOB types, all port types have their own classes,
            ;; which can be accessed via `class-of' once you have an
            ;; instance.  Here we export bindings just for file ports.
            <file-port>
            <file-input-port> <file-output-port> <file-input-output-port>

            is-a? class-of
            ensure-metaclass ensure-metaclass-with-supers
            make-class
            make-generic ensure-generic
            make-extended-generic
            make-accessor ensure-accessor
            add-method!
            class-slot-ref class-slot-set! slot-unbound slot-missing 
            slot-definition-name  slot-definition-options
            slot-definition-allocation

            slot-definition-getter slot-definition-setter
            slot-definition-accessor
            slot-definition-init-value slot-definition-init-form
            slot-definition-init-thunk slot-definition-init-keyword 
            slot-init-function class-slot-definition
            method-source
            compute-cpl compute-std-cpl compute-get-n-set compute-slots
            compute-getter-method compute-setter-method
            allocate-instance initialize make-instance make
            no-next-method  no-applicable-method no-method
            change-class update-instance-for-different-class
            shallow-clone deep-clone
            class-redefinition
            apply-generic apply-method apply-methods
            compute-applicable-methods %compute-applicable-methods
            method-more-specific? sort-applicable-methods
            class-subclasses class-methods
            goops-error
            min-fixnum max-fixnum
           
;;; *fixme* Should go into goops.c
            instance?  slot-ref-using-class
            slot-set-using-class! slot-bound-using-class?
            slot-exists-using-class? slot-ref slot-set! slot-bound?
            class-name class-direct-supers class-direct-subclasses
            class-direct-methods class-direct-slots class-precedence-list
            class-slots
            generic-function-name
            generic-function-methods method-generic-function
            method-specializers method-formals
            primitive-generic-generic enable-primitive-generic!
            method-procedure accessor-method-slot-definition
            slot-exists? make find-method get-keyword)
  #:no-backtrace)

;; XXX FIXME: figure out why the 'eval-when's in this file must use
;; 'compile' and must avoid 'expand', but only in 2.2, and only when
;; compiling something that imports goops, e.g. (ice-9 occam-channel),
;; before (oop goops) itself has been compiled.

;; First initialize the builtin part of GOOPS
(eval-when (compile load eval)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_goops_builtins"))

(eval-when (compile load eval)
  (use-modules ((language tree-il primitives) :select (add-interesting-primitive!)))
  (add-interesting-primitive! 'class-of))

;;; The standard class precedence list computation algorithm
;;;
;;; Correct behaviour:
;;;
;;; (define-class food ())
;;; (define-class fruit (food))
;;; (define-class spice (food))
;;; (define-class apple (fruit))
;;; (define-class cinnamon (spice))
;;; (define-class pie (apple cinnamon))
;;; => cpl (pie) = pie apple fruit cinnamon spice food object top
;;;
;;; (define-class d ())
;;; (define-class e ())
;;; (define-class f ())
;;; (define-class b (d e))
;;; (define-class c (e f))
;;; (define-class a (b c))
;;; => cpl (a) = a b d c e f object top
;;;

(define (compute-std-cpl c get-direct-supers)
  (define (only-non-null lst)
    (filter (lambda (l) (not (null? l))) lst))

  (define (merge-lists reversed-partial-result inputs)
    (cond
     ((every null? inputs)
      (reverse! reversed-partial-result))
     (else
      (let* ((candidate (lambda (c)
                          (and (not (any (lambda (l)
                                           (memq c (cdr l)))
                                         inputs))
                               c)))
             (candidate-car (lambda (l)
                              (and (not (null? l))
                                   (candidate (car l)))))
             (next (any candidate-car inputs)))
        (if (not next)
            (goops-error "merge-lists: Inconsistent precedence graph"))
        (let ((remove-next (lambda (l)
                             (if (eq? (car l) next)
                                 (cdr l)
                                 l))))
          (merge-lists (cons next reversed-partial-result)
                       (only-non-null (map remove-next inputs))))))))
  (let ((c-direct-supers (get-direct-supers c)))
    (merge-lists (list c)
                 (only-non-null (append (map class-precedence-list
                                             c-direct-supers)
                                        (list c-direct-supers))))))

;; Bootstrap version.
(define (compute-cpl class)
  (compute-std-cpl class class-direct-supers))

(define-syntax macro-fold-right
  (syntax-rules ()
    ((_ folder seed ()) seed)
    ((_ folder seed (head . tail))
     (folder head (macro-fold-right folder seed tail)))))

(define-syntax-rule (fold-<class>-slots fold visit seed)
  (fold visit seed
        ((layout <protected-read-only-slot>)
         (flags <hidden-slot>)
         (self <self-slot>)
         (instance-finalizer <hidden-slot>)
         (print)
         (name <protected-hidden-slot>)
         (reserved-0 <hidden-slot>)
         (reserved-1 <hidden-slot>)
         (redefined)
         (direct-supers)
         (direct-slots)
         (direct-subclasses)
         (direct-methods)
         (cpl)
         (slots)
         (getters-n-setters)
         (nfields))))

(define (build-slots-list dslots cpl)
  (define (check-cpl slots class-slots)
    (when (or-map (lambda (slot-def) (assq (car slot-def) slots))
                  class-slots)
      (scm-error 'misc-error #f
                 "a predefined <class> inherited field cannot be redefined"
                 '() '())))
  (define (remove-duplicate-slots slots)
    (let lp ((slots (reverse slots)) (res '()) (seen '()))
      (cond
       ((null? slots) res)
       ((memq (caar slots) seen)
        (lp (cdr slots) res seen))
       (else
        (lp (cdr slots) (cons (car slots) res) (cons (caar slots) seen))))))
  (let* ((class-slots (and (memq <class> cpl) (slot-ref <class> 'slots))))
    (when class-slots
      (check-cpl dslots class-slots))
    (let lp ((cpl (cdr cpl)) (res dslots) (class-slots '()))
      (if (null? cpl)
          (remove-duplicate-slots (append class-slots res))
          (let* ((head (car cpl))
                 (cpl (cdr cpl))
                 (new-slots (slot-ref head 'direct-slots)))
            (cond
             ((not class-slots)
              (lp cpl (append new-slots res) class-slots))
             ((eq? head <class>)
              ;; Move class slots to the head of the list.
              (lp cpl res new-slots))
             (else
              (check-cpl new-slots class-slots)
              (lp cpl (append new-slots res) class-slots))))))))

(define (%compute-getters-n-setters slots)
  (define (compute-init-thunk options)
    (cond
     ((kw-arg-ref options #:init-value) => (lambda (val) (lambda () val)))
     ((kw-arg-ref options #:init-thunk))
     (else #f)))
  (let lp ((slots slots) (n 0))
    (match slots
      (() '())
      (((name . options) . slots)
       (cons (cons name (cons (compute-init-thunk options) n))
             (lp slots (1+ n)))))))

(define (%compute-layout slots getters-n-setters nfields is-class?)
  (define (instance-allocated? g-n-s)
    (match g-n-s
      ((name init-thunk . (? exact-integer? index)) #t)
      ((name init-thunk getter setter index size) #t)
      (_ #f)))

  (define (allocated-index g-n-s)
    (match g-n-s
      ((name init-thunk . (? exact-integer? index)) index)
      ((name init-thunk getter setter index size) index)))

  (define (allocated-size g-n-s)
    (match g-n-s
      ((name init-thunk . (? exact-integer? index)) 1)
      ((name init-thunk getter setter index size) size)))

  (define (slot-protection-and-kind options)
    (define (subclass? class parent)
      (memq parent (class-precedence-list class)))
    (let ((type (kw-arg-ref options #:class)))
      (if (and type (subclass? type <foreign-slot>))
          (values (cond
                   ((subclass? type <self-slot>) #\s)
                   ((subclass? type <protected-slot>) #\p)
                   (else #\u))
                  (cond
                   ((subclass? type <opaque-slot>) #\o)
                   ((subclass? type <read-only-slot>) #\r)
                   ((subclass? type <hidden-slot>) #\h)
                   (else #\w)))
          (values #\p #\w))))

  (let ((layout (make-string (* nfields 2))))
    (let lp ((n 0) (slots slots) (getters-n-setters getters-n-setters))
      (match getters-n-setters
        (()
         (unless (= n nfields) (error "bad nfields"))
         (unless (null? slots) (error "inconsistent g-n-s/slots"))
         (when is-class?
           (let ((class-layout (symbol->string (slot-ref <class> 'layout))))
             (unless (string-prefix? class-layout layout)
               (error "bad layout for class"))))
         layout)
        ((g-n-s . getters-n-setters)
         (match slots
           (((name . options) . slots)
            (cond
             ((instance-allocated? g-n-s)
              (unless (< n nfields) (error "bad nfields"))
              (unless (= n (allocated-index g-n-s)) (error "bad allocation"))
              (call-with-values (lambda () (slot-protection-and-kind options))
                (lambda (protection kind)
                  (let init ((n n) (size (allocated-size g-n-s)))
                    (cond
                     ((zero? size) (lp n slots getters-n-setters))
                     (else
                      (string-set! layout (* n 2) protection)
                      (string-set! layout (1+ (* n 2)) kind)
                      (init (1+ n) (1- size))))))))
             (else
              (lp n slots getters-n-setters))))))))))

(define (%prep-layout! class)
  (let* ((is-class? (and (memq <class> (slot-ref class 'cpl)) #t))
         (layout (%compute-layout (slot-ref class 'slots)
                                  (slot-ref class 'getters-n-setters)
                                  (slot-ref class 'nfields)
                                  is-class?)))
    (%init-layout! class layout)))

(define (make-standard-class class name dsupers dslots)
  (let ((z (make-struct/no-tail class)))
    (slot-set! z 'direct-supers dsupers)
    (let* ((cpl (compute-cpl z))
           (dslots (map (lambda (slot)
                          (if (pair? slot) slot (list slot)))
                        dslots))
           (slots (build-slots-list dslots cpl))
           (nfields (length slots))
           (g-n-s (%compute-getters-n-setters slots)))
      (slot-set! z 'name name)
      (slot-set! z 'direct-slots dslots)
      (slot-set! z 'direct-subclasses '())
      (slot-set! z 'direct-methods '())
      (slot-set! z 'cpl cpl)
      (slot-set! z 'slots slots)
      (slot-set! z 'nfields nfields)
      (slot-set! z 'getters-n-setters g-n-s)
      (slot-set! z 'redefined #f)
      (for-each (lambda (super)
                  (let ((subclasses (slot-ref super 'direct-subclasses)))
                    (slot-set! super 'direct-subclasses (cons z subclasses))))
                dsupers)
      (%prep-layout! z)
      (%inherit-magic! z dsupers)
      z)))

(define <class>
  (let-syntax ((visit
                ;; The specialized slot classes have not been defined
                ;; yet; initialize <class> with unspecialized slots.
                (syntax-rules ()
                  ((_ (name) tail)       (cons (list 'name) tail))
                  ((_ (name class) tail) (cons (list 'name) tail)))))
    (let ((dslots (fold-<class>-slots macro-fold-right visit '())))
      (%make-root-class '<class> dslots (%compute-getters-n-setters dslots)))))

(define-syntax define-standard-class
  (syntax-rules ()
    ((define-standard-class name (super ...) #:metaclass meta slot ...)
     (define name
       (make-standard-class meta 'name (list super ...) '(slot ...))))
    ((define-standard-class name (super ...) slot ...)
     (define-standard-class name (super ...) #:metaclass <class> slot ...))))

(define-standard-class <top> ())
(define-standard-class <object> (<top>))

;; <top>, <object>, and <class> were partially initialized.  Correct
;; them here.
(slot-set! <object> 'direct-subclasses (list <class>))
(slot-set! <class> 'direct-supers (list <object>))
(slot-set! <class> 'cpl (list <class> <object> <top>))

(define-standard-class <foreign-slot> (<top>))
(define-standard-class <protected-slot> (<foreign-slot>))
(define-standard-class <hidden-slot> (<foreign-slot>))
(define-standard-class <opaque-slot> (<foreign-slot>))
(define-standard-class <read-only-slot> (<foreign-slot>))
(define-standard-class <self-slot> (<read-only-slot>))
(define-standard-class <protected-opaque-slot> (<protected-slot>
                                                <opaque-slot>))
(define-standard-class <protected-hidden-slot> (<protected-slot>
                                                <hidden-slot>))
(define-standard-class <protected-read-only-slot> (<protected-slot>
                                                   <read-only-slot>))
(define-standard-class <scm-slot> (<protected-slot>))
(define-standard-class <int-slot> (<foreign-slot>))
(define-standard-class <float-slot> (<foreign-slot>))
(define-standard-class <double-slot> (<foreign-slot>))

;; Finish initialization of <class> with specialized slots.
(let-syntax ((visit
              (syntax-rules ()
                ((_ (name) tail)
                 (cons (list 'name) tail))
                ((_ (name class) tail)
                 (cons (list 'name #:class class) tail)))))
  (let ((dslots (fold-<class>-slots macro-fold-right visit '())))
    (slot-set! <class> 'direct-slots dslots)
    (slot-set! <class> 'slots dslots)
    (slot-set! <class> 'getters-n-setters (%compute-getters-n-setters dslots))))

;; Applicables and their classes.
(define-standard-class <procedure-class> (<class>))
(define-standard-class <applicable-struct-class>
  (<procedure-class>))
(define-standard-class <applicable-struct-with-setter-class>
  (<applicable-struct-class>))
(%bless-applicable-struct-vtables! <applicable-struct-class>
                                   <applicable-struct-with-setter-class>)

(define-standard-class <applicable> (<top>))
(define-standard-class <applicable-struct> (<object> <applicable>)
  #:metaclass <applicable-struct-class>
  procedure)
(define-standard-class <applicable-struct-with-setter> (<applicable-struct>)
  #:metaclass <applicable-struct-with-setter-class>
  setter)
(define-standard-class <generic> (<applicable-struct>)
  #:metaclass <applicable-struct-class>
  methods
  (n-specialized #:init-value 0)
  (extended-by #:init-value ())
  effective-methods)
(%bless-pure-generic-vtable! <generic>)
(define-standard-class <extended-generic> (<generic>)
  #:metaclass <applicable-struct-class>
  (extends #:init-value ()))
(%bless-pure-generic-vtable! <extended-generic>)
(define-standard-class <generic-with-setter> (<generic>
                                              <applicable-struct-with-setter>)
  #:metaclass <applicable-struct-with-setter-class>)
(%bless-pure-generic-vtable! <generic-with-setter>)
(define-standard-class <accessor> (<generic-with-setter>)
  #:metaclass <applicable-struct-with-setter-class>)
(%bless-pure-generic-vtable! <accessor>)
(define-standard-class <extended-generic-with-setter> (<extended-generic>
                                                       <generic-with-setter>)
  #:metaclass <applicable-struct-with-setter-class>)
(%bless-pure-generic-vtable! <extended-generic-with-setter>)
(define-standard-class <extended-accessor> (<accessor>
                                            <extended-generic-with-setter>)
  #:metaclass <applicable-struct-with-setter-class>)
(%bless-pure-generic-vtable! <extended-accessor>)

;; Methods
(define-standard-class <method> (<object>)
  generic-function
  specializers
  procedure
  formals
  body
  make-procedure)
(define-standard-class <accessor-method> (<method>)
  (slot-definition #:init-keyword #:slot-definition))

;; Primitive types classes
(define-standard-class <boolean> (<top>))
(define-standard-class <char> (<top>))
(define-standard-class <list> (<top>))
(define-standard-class <pair> (<list>))
(define-standard-class <null> (<list>))
(define-standard-class <string> (<top>))
(define-standard-class <symbol> (<top>))
(define-standard-class <vector> (<top>))
(define-standard-class <foreign> (<top>))
(define-standard-class <hashtable> (<top>))
(define-standard-class <fluid> (<top>))
(define-standard-class <dynamic-state> (<top>))
(define-standard-class <frame> (<top>))
(define-standard-class <vm-continuation> (<top>))
(define-standard-class <bytevector> (<top>))
(define-standard-class <uvec> (<bytevector>))
(define-standard-class <array> (<top>))
(define-standard-class <bitvector> (<top>))
(define-standard-class <number> (<top>))
(define-standard-class <complex> (<number>))
(define-standard-class <real> (<complex>))
(define-standard-class <integer> (<real>))
(define-standard-class <fraction> (<real>))
(define-standard-class <keyword> (<top>))
(define-standard-class <unknown> (<top>))
(define-standard-class <procedure> (<applicable>)
  #:metaclass <procedure-class>)
(define-standard-class <primitive-generic> (<procedure>)
  #:metaclass <procedure-class>)
(define-standard-class <port> (<top>))
(define-standard-class <input-port> (<port>))
(define-standard-class <output-port> (<port>))
(define-standard-class <input-output-port> (<input-port> <output-port>))

(define (%invalidate-method-cache! gf)
  (slot-set! gf 'procedure (delayed-compile gf))
  (slot-set! gf 'effective-methods '()))

;; Boot definition.
(define (invalidate-method-cache! gf)
  (%invalidate-method-cache! gf))

;; A simple make which will be redefined later.  This version handles
;; only creation of gf, methods and classes (no instances).
;;
;; Since this code will disappear when Goops will be fully booted,
;; no precaution is taken to be efficient.
;;
(define (make class . args)
  (cond
   ((or (eq? class <generic>) (eq? class <accessor>))
    (let ((z (make-struct/no-tail class #f '() 0 '())))
      (set-procedure-property! z 'name (get-keyword #:name args #f))
      (invalidate-method-cache! z)
      (when (eq? class <accessor>)
        (let ((setter (get-keyword #:setter args #f)))
          (when setter
            (slot-set! z 'setter setter))))
      z))
   (else
    (let ((z (%allocate-instance class args)))
      (cond
       ((or (eq? class <method>) (eq? class <accessor-method>))
        (for-each (match-lambda
                   ((kw slot default)
                    (slot-set! z slot (get-keyword kw args default))))
                  '((#:generic-function generic-function #f)
                    (#:specializers specializers ())
                    (#:procedure procedure #f)
                    (#:formals formals ())
                    (#:body body ())
                    (#:make-procedure make-procedure #f))))
       ((memq <class> (class-precedence-list class))
        (for-each (match-lambda
                   ((kw slot default)
                    (slot-set! z slot (get-keyword kw args default))))
                  '((#:name name ???)
                    (#:dsupers direct-supers ())
                    (#:slots direct-slots ())
                    )))
       (else
        (error "boot `make' does not support this class" class)))
      z))))

(define *dispatch-module* (current-module))

;;;
;;; Generic functions have an applicable-methods cache associated with
;;; them. Every distinct set of types that is dispatched through a
;;; generic adds an entry to the cache. This cache gets compiled out to
;;; a dispatch procedure. In steady-state, this dispatch procedure is
;;; never recompiled; but during warm-up there is some churn, both to
;;; the cache and to the dispatch procedure.
;;;
;;; So what is the deal if warm-up happens in a multithreaded context?
;;; There is indeed a window between missing the cache for a certain set
;;; of arguments, and then updating the cache with the newly computed
;;; applicable methods. One of the updaters is liable to lose their new
;;; entry.
;;;
;;; This is actually OK though, because a subsequent cache miss for the
;;; race loser will just cause memoization to try again. The cache will
;;; eventually be consistent. We're not mutating the old part of the
;;; cache, just consing on the new entry.
;;;
;;; It doesn't even matter if the dispatch procedure and the cache are
;;; inconsistent -- most likely the type-set that lost the dispatch
;;; procedure race will simply re-trigger a memoization, but since the
;;; winner isn't in the effective-methods cache, it will likely also
;;; re-trigger a memoization, and the cache will finally be consistent.
;;; As you can see there is a possibility for ping-pong effects, but
;;; it's unlikely given the shortness of the window between slot-set!
;;; invocations. We could add a mutex, but it is strictly unnecessary,
;;; and would add runtime cost and complexity.
;;;

(define (emit-linear-dispatch gf-sym nargs methods free rest?)
  (define (gen-syms n stem)
    (let lp ((n (1- n)) (syms '()))
      (if (< n 0)
          syms
          (lp (1- n) (cons (gensym stem) syms)))))
  (let* ((args (gen-syms nargs "a"))
         (types (gen-syms nargs "t")))
    (let lp ((methods methods)
             (free free)
             (exp `(cache-miss ,gf-sym
                               ,(if rest?
                                    `(cons* ,@args rest)
                                    `(list ,@args)))))
      (cond
       ((null? methods)
        (values `(,(if rest? `(,@args . rest) args)
                  (let ,(map (lambda (t a)
                               `(,t (class-of ,a)))
                             types args)
                    ,exp))
                free))
       (else
        ;; jeez
        (let preddy ((free free)
                     (types types)
                     (specs (vector-ref (car methods) 1))
                     (checks '()))
          (if (null? types)
              (let ((m-sym (gensym "p")))
                (lp (cdr methods)
                    (acons (vector-ref (car methods) 3)
                           m-sym
                           free)
                    `(if (and . ,checks)
                         ,(if rest?
                              `(apply ,m-sym ,@args rest)
                              `(,m-sym . ,args))
                         ,exp)))
              (let ((var (assq-ref free (car specs))))
                (if var
                    (preddy free
                            (cdr types)
                            (cdr specs)
                            (cons `(eq? ,(car types) ,var)
                                  checks))
                    (let ((var (gensym "c")))
                      (preddy (acons (car specs) var free)
                              (cdr types)
                              (cdr specs)
                              (cons `(eq? ,(car types) ,var)
                                    checks))))))))))))

(define (compute-dispatch-procedure gf cache)
  (define (scan)
    (let lp ((ls cache) (nreq -1) (nrest -1))
      (cond
       ((null? ls)
        (collate (make-vector (1+ nreq) '())
                 (make-vector (1+ nrest) '())))
       ((vector-ref (car ls) 2)         ; rest
        (lp (cdr ls) nreq (max nrest (vector-ref (car ls) 0))))
       (else                            ; req
        (lp (cdr ls) (max nreq (vector-ref (car ls) 0)) nrest)))))
  (define (collate req rest)
    (let lp ((ls cache))
      (cond
       ((null? ls)
        (emit req rest))
       ((vector-ref (car ls) 2)         ; rest
        (let ((n (vector-ref (car ls) 0)))
          (vector-set! rest n (cons (car ls) (vector-ref rest n)))
          (lp (cdr ls))))
       (else                            ; req
        (let ((n (vector-ref (car ls) 0)))
          (vector-set! req n (cons (car ls) (vector-ref req n)))
          (lp (cdr ls)))))))
  (define (emit req rest)
    (let ((gf-sym (gensym "g")))
      (define (emit-rest n clauses free)
        (if (< n (vector-length rest))
            (let ((methods (vector-ref rest n)))
              (cond
               ((null? methods)
                (emit-rest (1+ n) clauses free))
               ;; FIXME: hash dispatch
               (else
                (call-with-values
                    (lambda ()
                      (emit-linear-dispatch gf-sym n methods free #t))
                  (lambda (clause free)
                    (emit-rest (1+ n) (cons clause clauses) free))))))
            (emit-req (1- (vector-length req)) clauses free)))
      (define (emit-req n clauses free)
        (if (< n 0)
            (comp `(lambda ,(map cdr free)
                     (case-lambda ,@clauses))
                  (map car free))
            (let ((methods (vector-ref req n)))
              (cond
               ((null? methods)
                (emit-req (1- n) clauses free))
               ;; FIXME: hash dispatch
               (else
                (call-with-values
                    (lambda ()
                      (emit-linear-dispatch gf-sym n methods free #f))
                  (lambda (clause free)
                    (emit-req (1- n) (cons clause clauses) free))))))))

      (emit-rest 0
                 (if (or (zero? (vector-length rest))
                         (null? (vector-ref rest 0)))
                     (list `(args (cache-miss ,gf-sym args)))
                     '())
                 (acons gf gf-sym '()))))
  (define (comp exp vals)
    ;; When cross-compiling Guile itself, the native Guile must generate
    ;; code for the host.
    (with-target %host-type
      (lambda ()
        (let ((p ((@ (system base compile) compile) exp
                  #:env *dispatch-module*
                  #:from 'scheme
                  #:opts '(#:partial-eval? #f #:cse? #f))))
          (apply p vals)))))

  ;; kick it.
  (scan))

;; o/~  ten, nine, eight
;;        sometimes that's just how it goes
;;          three, two, one
;;
;;            get out before it blows    o/~
;;
(define timer-init 30)
(define (delayed-compile gf)
  (let ((timer timer-init))
    (lambda args
      (set! timer (1- timer))
      (cond
       ((zero? timer)
        (let ((dispatch (compute-dispatch-procedure
                         gf (slot-ref gf 'effective-methods))))
          (slot-set! gf 'procedure dispatch)
          (apply dispatch args)))
       (else
        ;; interestingly, this catches recursive compilation attempts as
        ;; well; in that case, timer is negative
        (cache-dispatch gf args))))))

(define (cache-dispatch gf args)
  (define (map-until n f ls)
    (if (or (zero? n) (null? ls))
        '()
        (cons (f (car ls)) (map-until (1- n) f (cdr ls)))))
  (define (equal? x y) ; can't use the stock equal? because it's a generic...
    (cond ((pair? x) (and (pair? y)
                          (eq? (car x) (car y))
                          (equal? (cdr x) (cdr y))))
          ((null? x) (null? y))
          (else #f)))
  (if (slot-ref gf 'n-specialized)
      (let ((types (map-until (slot-ref gf 'n-specialized) class-of args)))
        (let lp ((cache (slot-ref gf 'effective-methods)))
          (cond ((null? cache)
                 (cache-miss gf args))
                ((equal? (vector-ref (car cache) 1) types)
                 (apply (vector-ref (car cache) 3) args))
                (else (lp (cdr cache))))))
      (cache-miss gf args)))

(define (cache-miss gf args)
  (apply (memoize-method! gf args) args))

(define (memoize-effective-method! gf args applicable)
  (define (first-n ls n)
    (if (or (zero? n) (null? ls))
        '()
        (cons (car ls) (first-n (cdr ls) (- n 1)))))
  (define (parse n ls)
    (cond ((null? ls)
           (memoize n #f (map class-of args)))
          ((= n (slot-ref gf 'n-specialized))
           (memoize n #t (map class-of (first-n args n))))
          (else
           (parse (1+ n) (cdr ls)))))
  (define (memoize len rest? types)
    (let* ((cmethod (compute-cmethod applicable types))
           (cache (cons (vector len types rest? cmethod)
                        (slot-ref gf 'effective-methods))))
      (slot-set! gf 'effective-methods cache)
      (slot-set! gf 'procedure (delayed-compile gf))
      cmethod))
  (parse 0 args))

;;;
;;; Compiling next methods into method bodies
;;;

;;; So, for the reader: there basic idea is that, given that the
;;; semantics of `next-method' depend on the concrete types being
;;; dispatched, why not compile a specific procedure to handle each type
;;; combination that we see at runtime.
;;;
;;; In theory we can do much better than a bytecode compilation, because
;;; we know the *exact* types of the arguments. It's ideal for native
;;; compilation. A task for the future.
;;;
;;; I think this whole generic application mess would benefit from a
;;; strict MOP.

(define (compute-cmethod methods types)
  (let ((make-procedure (slot-ref (car methods) 'make-procedure)))
    (if make-procedure
        (make-procedure
         (if (null? (cdr methods))
             (lambda args
               (no-next-method (method-generic-function (car methods)) args))
             (compute-cmethod (cdr methods) types)))
        (method-procedure (car methods)))))

;;;
;;; Memoization
;;;

(define (memoize-method! gf args)
  (let ((applicable ((if (eq? gf compute-applicable-methods)
			 %compute-applicable-methods
			 compute-applicable-methods)
		     gf args)))
    (cond (applicable
           (memoize-effective-method! gf args applicable))
	  (else
	   (no-applicable-method gf args)))))

(set-procedure-property! memoize-method! 'system-procedure #t)

(define no-applicable-method
  (make <generic> #:name 'no-applicable-method))

(%goops-early-init)

;; Then load the rest of GOOPS


;; FIXME: deprecate.
(define min-fixnum (- (expt 2 29)))
(define max-fixnum (- (expt 2 29) 1))

;;
;; goops-error
;;
(define (goops-error format-string . args)
  (scm-error 'goops-error #f format-string args '()))

;;
;; is-a?
;;
(define (is-a? obj class)
  (and (memq class (class-precedence-list (class-of obj))) #t))


;;;
;;; {Meta classes}
;;;

(define ensure-metaclass-with-supers
  (let ((table-of-metas '()))
    (lambda (meta-supers)
      (let ((entry (assoc meta-supers table-of-metas)))
	(if entry
	    ;; Found a previously created metaclass
	    (cdr entry)
	    ;; Create a new meta-class which inherit from "meta-supers"
	    (let ((new (make <class> #:dsupers meta-supers
			             #:slots   '()
				     #:name   (gensym "metaclass"))))
	      (set! table-of-metas (cons (cons meta-supers new) table-of-metas))
	      new))))))

(define (ensure-metaclass supers)
  (if (null? supers)
      <class>
      (let* ((all-metas (map (lambda (x) (class-of x)) supers))
	     (all-cpls  (append-map (lambda (m)
                                      (cdr (class-precedence-list m))) 
                                    all-metas))
	     (needed-metas '()))
	;; Find the most specific metaclasses.  The new metaclass will be
	;; a subclass of these.
	(for-each
	 (lambda (meta)
	   (if (and (not (member meta all-cpls))
		      (not (member meta needed-metas)))
	     (set! needed-metas (append needed-metas (list meta)))))
	 all-metas)
	;; Now return a subclass of the metaclasses we found.
	(if (null? (cdr needed-metas))
	    (car needed-metas)  ; If there's only one, just use it.
	    (ensure-metaclass-with-supers needed-metas)))))

;;;
;;; {Classes}
;;;

;;; (define-class NAME (SUPER ...) SLOT-DEFINITION ... OPTION ...)
;;;
;;;   SLOT-DEFINITION ::= SLOT-NAME | (SLOT-NAME OPTION ...)
;;;   OPTION ::= KEYWORD VALUE
;;;

(define (make-class supers slots . options)
  (let* ((name (get-keyword #:name options (make-unbound)))
         (supers (if (not (or-map (lambda (class)
                                    (memq <object>
                                          (class-precedence-list class)))
                                  supers))
                     (append supers (list <object>))
                     supers))
         (metaclass (or (get-keyword #:metaclass options #f)
                        (ensure-metaclass supers))))

    ;; Verify that all direct slots are different and that we don't inherit
    ;; several time from the same class
    (let ((tmp1 (find-duplicate supers))
          (tmp2 (find-duplicate (map slot-definition-name slots))))
      (if tmp1
          (goops-error "make-class: super class ~S is duplicate in class ~S"
                       tmp1 name))
      (if tmp2
          (goops-error "make-class: slot ~S is duplicate in class ~S"
                       tmp2 name)))

    ;; Everything seems correct, build the class
    (apply make metaclass
           #:dsupers supers
           #:slots slots 
           #:name name
           options)))

;;; (class (SUPER ...) SLOT-DEFINITION ... OPTION ...)
;;;
;;;   SLOT-DEFINITION ::= SLOT-NAME | (SLOT-NAME OPTION ...)
;;;   OPTION ::= KEYWORD VALUE
;;;
(define-syntax class
  (lambda (x)
    (define (parse-options options)
      (syntax-case options ()
        (() #'())
        ((kw arg . options) (keyword? (syntax->datum #'kw))
         (with-syntax ((options (parse-options #'options)))
           (syntax-case #'kw ()
             (#:init-form
              #'(kw 'arg #:init-thunk (lambda () arg) . options))
             (_
              #'(kw arg . options)))))))
    (define (check-valid-kwargs args)
      (syntax-case args ()
        (() #'())
        ((kw arg . args) (keyword? (syntax->datum #'kw))
         #`(kw arg . #,(check-valid-kwargs #'args)))))
    (define (parse-slots-and-kwargs args)
      (syntax-case args ()
        (()
         #'(() ()))
        ((kw . _) (keyword? (syntax->datum #'kw))
         #`(() #,(check-valid-kwargs args)))
        (((name option ...) args ...)
         (with-syntax (((slots kwargs) (parse-slots-and-kwargs #'(args ...)))
                       ((option ...) (parse-options #'(option ...))))
           #'(((list 'name option ...) . slots) kwargs)))
        ((name args ...) (symbol? (syntax->datum #'name))
         (with-syntax (((slots kwargs) (parse-slots-and-kwargs #'(args ...))))
           #'(('(name) . slots) kwargs)))))
    (syntax-case x ()
      ((class (super ...) arg ...)
       (with-syntax ((((slot-def ...) (option ...))
                      (parse-slots-and-kwargs #'(arg ...))))
         #'(make-class (list super ...)
                       (list slot-def ...)
                       option ...))))))

(define-syntax define-class-pre-definition
  (lambda (x)
    (syntax-case x ()
      ((_ (k arg rest ...) out ...)
       (keyword? (syntax->datum #'k))
       (case (syntax->datum #'k)
         ((#:getter #:setter)
          #'(define-class-pre-definition (rest ...)
              out ...
              (if (or (not (defined? 'arg))
                      (not (is-a? arg <generic>)))
                  (toplevel-define!
                   'arg
                   (ensure-generic (if (defined? 'arg) arg #f) 'arg)))))
         ((#:accessor)
          #'(define-class-pre-definition (rest ...)
              out ...
              (if (or (not (defined? 'arg))
                      (not (is-a? arg <accessor>)))
                  (toplevel-define!
                   'arg
                   (ensure-accessor (if (defined? 'arg) arg #f) 'arg)))))
         (else
          #'(define-class-pre-definition (rest ...) out ...))))
      ((_ () out ...)
       #'(begin out ...)))))
       
;; Some slot options require extra definitions to be made. In
;; particular, we want to make sure that the generic function objects
;; which represent accessors exist before `make-class' tries to add
;; methods to them.
(define-syntax define-class-pre-definitions
  (lambda (x)
    (syntax-case x ()
      ((_ () out ...)
       #'(begin out ...))
      ((_ (slot rest ...) out ...)
       (keyword? (syntax->datum #'slot))
       #'(begin out ...))
      ((_ (slot rest ...) out ...)
       (identifier? #'slot)
       #'(define-class-pre-definitions (rest ...)
         out ...))
      ((_ ((slotname slotopt ...) rest ...) out ...)
       #'(define-class-pre-definitions (rest ...) 
         out ... (define-class-pre-definition (slotopt ...)))))))

(define-syntax-rule (define-class name supers slot ...)
  (begin
    (define-class-pre-definitions (slot ...))
    (if (and (defined? 'name)
             (is-a? name <class>)
             (memq <object> (class-precedence-list name)))
        (class-redefinition name
                            (class supers slot ... #:name 'name))
        (toplevel-define! 'name (class supers slot ... #:name 'name)))))
       
(define-syntax-rule (standard-define-class arg ...)
  (define-class arg ...))

;;;
;;; {Generic functions and accessors}
;;;

;; Apparently the desired semantics are that we extend previous
;; procedural definitions, but that if `name' was already a generic, we
;; overwrite its definition.
(define-syntax define-generic
  (lambda (x)
    (syntax-case x ()
      ((define-generic name) (symbol? (syntax->datum #'name))
       #'(define name
           (if (and (defined? 'name) (is-a? name <generic>))
               (make <generic> #:name 'name)
               (ensure-generic (if (defined? 'name) name #f) 'name)))))))

(define-syntax define-extended-generic
  (lambda (x)
    (syntax-case x ()
      ((define-extended-generic name val) (symbol? (syntax->datum #'name))
       #'(define name (make-extended-generic val 'name))))))

(define-syntax define-extended-generics
  (lambda (x)
    (define (id-append ctx a b)
      (datum->syntax ctx (symbol-append (syntax->datum a) (syntax->datum b))))
    (syntax-case x ()
      ((define-extended-generic (name ...) #:prefix (prefix ...))
       (and (and-map symbol? (syntax->datum #'(name ...)))
            (and-map symbol? (syntax->datum #'(prefix ...))))
       (with-syntax ((((val ...)) (map (lambda (name)
                                         (map (lambda (prefix)
                                                (id-append name prefix name))
                                              #'(prefix ...)))
                                       #'(name ...))))
         #'(begin
             (define-extended-generic name (list val ...))
             ...))))))

(define* (make-generic #:optional name)
  (make <generic> #:name name))

(define* (make-extended-generic gfs #:optional name)
  (let* ((gfs (if (list? gfs) gfs (list gfs)))
	 (gws? (any (lambda (gf) (is-a? gf <generic-with-setter>)) gfs)))
    (let ((ans (if gws?
		   (let* ((sname (and name (make-setter-name name)))
			  (setters
			   (append-map (lambda (gf)
					 (if (is-a? gf <generic-with-setter>)
					     (list (ensure-generic (setter gf)
								   sname))
					     '()))
				       gfs))
			  (es (make <extended-generic-with-setter>
				#:name name
				#:extends gfs
				#:setter (make <extended-generic>
					   #:name sname
					   #:extends setters))))
		     (extended-by! setters (setter es))
		     es)
		   (make <extended-generic>
		     #:name name
		     #:extends gfs))))
      (extended-by! gfs ans)
      ans)))

(define (extended-by! gfs eg)
  (for-each (lambda (gf)
	      (slot-set! gf 'extended-by
			 (cons eg (slot-ref gf 'extended-by))))
	    gfs)
  (invalidate-method-cache! eg))

(define (not-extended-by! gfs eg)
  (for-each (lambda (gf)
	      (slot-set! gf 'extended-by
			 (delq! eg (slot-ref gf 'extended-by))))
	    gfs)
  (invalidate-method-cache! eg))

(define* (ensure-generic old-definition #:optional name)
  (cond ((is-a? old-definition <generic>) old-definition)
        ((procedure-with-setter? old-definition)
         (make <generic-with-setter>
           #:name name
           #:default (procedure old-definition)
           #:setter (setter old-definition)))
        ((procedure? old-definition)
         (if (generic-capability? old-definition) old-definition
             (make <generic> #:name name #:default old-definition)))
        (else (make <generic> #:name name))))

;; same semantics as <generic>
(define-syntax-rule (define-accessor name)
  (define name
    (cond ((not (defined? 'name))  (ensure-accessor #f 'name))
          ((is-a? name <accessor>) (make <accessor> #:name 'name))
          (else                    (ensure-accessor name 'name)))))

(define (make-setter-name name)
  (string->symbol (string-append "setter:" (symbol->string name))))

(define* (make-accessor #:optional name)
  (make <accessor>
    #:name name
    #:setter (make <generic>
               #:name (and name (make-setter-name name)))))

(define* (ensure-accessor proc #:optional name)
  (cond ((and (is-a? proc <accessor>)
              (is-a? (setter proc) <generic>))
         proc)
        ((is-a? proc <generic-with-setter>)
         (upgrade-accessor proc (setter proc)))
        ((is-a? proc <generic>)
         (upgrade-accessor proc (make-generic name)))
        ((procedure-with-setter? proc)
         (make <accessor>
           #:name name
           #:default (procedure proc)
           #:setter (ensure-generic (setter proc) name)))
        ((procedure? proc)
         (ensure-accessor (if (generic-capability? proc)
                              (make <generic> #:name name #:default proc)
                              (ensure-generic proc name))
                          name))
        (else
         (make-accessor name))))

(define (upgrade-accessor generic setter)
  (let ((methods (slot-ref generic 'methods))
	(gws (make (if (is-a? generic <extended-generic>)
		       <extended-generic-with-setter>
		       <accessor>)
		   #:name (generic-function-name generic)
		   #:extended-by (slot-ref generic 'extended-by)
		   #:setter setter)))
    (if (is-a? generic <extended-generic>)
	(let ((gfs (slot-ref generic 'extends)))
	  (not-extended-by! gfs generic)
	  (slot-set! gws 'extends gfs)
	  (extended-by! gfs gws)))
    ;; Steal old methods
    (for-each (lambda (method)
		(slot-set! method 'generic-function gws))
	      methods)
    (slot-set! gws 'methods methods)
    (invalidate-method-cache! gws)
    gws))

;;;
;;; {Methods}
;;;

;; Note: `a' and `b' can have unequal lengths (i.e. one can be one
;; element longer than the other when we have a dotted parameter
;; list). For instance, with the call
;;
;;   (M 1)
;;
;; with
;;
;;   (define-method M (a . l) ....)
;;   (define-method M (a) ....)
;; 
;; we consider that the second method is more specific.
;; 
;; Precondition: `a' and `b' are methods and are applicable to `types'.
(define (%method-more-specific? a b types)
  (let lp ((a-specializers (method-specializers a))
           (b-specializers (method-specializers b))
           (types types))
    (cond
     ;; (a) less specific than (a b ...) or (a . b)
     ((null? a-specializers) #t)
     ;; (a b ...) or (a . b) less specific than (a)
     ((null? b-specializers) #f)
     ;; (a . b) less specific than (a b ...)
     ((not (pair? a-specializers)) #f)
     ;; (a b ...) more specific than (a . b)
     ((not (pair? b-specializers)) #t)
     (else
      (let ((a-specializer (car a-specializers))
            (b-specializer (car b-specializers))
            (a-specializers (cdr a-specializers))
            (b-specializers (cdr b-specializers))
            (type (car types))
            (types (cdr types)))
        (if (eq? a-specializer b-specializer)
            (lp a-specializers b-specializers types)
            (let lp ((cpl (class-precedence-list type)))
              (let ((elt (car cpl)))
                (cond
                 ((eq? a-specializer elt) #t)
                 ((eq? b-specializer elt) #f)
                 (else (lp (cdr cpl))))))))))))

(define (%sort-applicable-methods methods types)
  (sort methods (lambda (a b) (%method-more-specific? a b types))))

(define (%compute-applicable-methods gf args)
  (define (method-applicable? m types)
    (let lp ((specs (method-specializers m)) (types types))
      (cond
       ((null? specs) (null? types))
       ((not (pair? specs)) #t)
       ((null? types) #f)
       (else
        (and (memq (car specs) (class-precedence-list (car types)))
             (lp (cdr specs) (cdr types)))))))
  (let ((n (length args))
        (types (map class-of args)))
    (let lp ((methods (generic-function-methods gf))
             (applicable '()))
      (if (null? methods)
          (and (not (null? applicable))
               (%sort-applicable-methods applicable types))
          (let ((m (car methods)))
            (lp (cdr methods)
                (if (method-applicable? m types)
                    (cons m applicable)
                    applicable)))))))

(define compute-applicable-methods %compute-applicable-methods)

(define (toplevel-define! name val)
  (module-define! (current-module) name val))

(define-syntax define-method
  (syntax-rules (setter)
    ((_ ((setter name) . args) body ...)
     (begin
       (if (or (not (defined? 'name))
               (not (is-a? name <accessor>)))
           (toplevel-define! 'name
                             (ensure-accessor
                              (if (defined? 'name) name #f) 'name)))
       (add-method! (setter name) (method args body ...))))
    ((_ (name . args) body ...)
     (begin
       ;; FIXME: this code is how it always was, but it's quite cracky:
       ;; it will only define the generic function if it was undefined
       ;; before (ok), or *was defined to #f*. The latter is crack. But
       ;; there are bootstrap issues about fixing this -- change it to
       ;; (is-a? name <generic>) and see.
       (if (or (not (defined? 'name))
               (not name))
           (toplevel-define! 'name (make <generic> #:name 'name)))
       (add-method! name (method args body ...))))))

(define-syntax method
  (lambda (x)
    (define (parse-args args)
      (let lp ((ls args) (formals '()) (specializers '()))
        (syntax-case ls ()
          (((f s) . rest)
           (and (identifier? #'f) (identifier? #'s))
           (lp #'rest
               (cons #'f formals)
               (cons #'s specializers)))
          ((f . rest)
           (identifier? #'f)
           (lp #'rest
               (cons #'f formals)
               (cons #'<top> specializers)))
          (()
           (list (reverse formals)
                 (reverse (cons #''() specializers))))
          (tail
           (identifier? #'tail)
           (list (append (reverse formals) #'tail)
                 (reverse (cons #'<top> specializers)))))))

    (define (find-free-id exp referent)
      (syntax-case exp ()
        ((x . y)
         (or (find-free-id #'x referent)
             (find-free-id #'y referent)))
        (x
         (identifier? #'x)
         (let ((id (datum->syntax #'x referent)))
           (and (free-identifier=? #'x id) id)))
        (_ #f)))

    (define (compute-procedure formals body)
      (syntax-case body ()
        ((body0 ...)
         (with-syntax ((formals formals))
           #'(lambda formals body0 ...)))))

    (define (->proper args)
      (let lp ((ls args) (out '()))
        (syntax-case ls ()
          ((x . xs)        (lp #'xs (cons #'x out)))
          (()              (reverse out))
          (tail            (reverse (cons #'tail out))))))

    (define (compute-make-procedure formals body next-method)
      (syntax-case body ()
        ((body ...)
         (with-syntax ((next-method next-method))
           (syntax-case formals ()
             ((formal ...)
              #'(lambda (real-next-method)
                  (lambda (formal ...)
                    (let ((next-method (lambda args
                                         (if (null? args)
                                             (real-next-method formal ...)
                                             (apply real-next-method args)))))
                      body ...))))
             (formals
              (with-syntax (((formal ...) (->proper #'formals)))
                #'(lambda (real-next-method)
                    (lambda formals
                      (let ((next-method (lambda args
                                           (if (null? args)
                                               (apply real-next-method formal ...)
                                               (apply real-next-method args)))))
                        body ...))))))))))

    (define (compute-procedures formals body)
      ;; So, our use of this is broken, because it operates on the
      ;; pre-expansion source code. It's equivalent to just searching
      ;; for referent in the datums. Ah well.
      (let ((id (find-free-id body 'next-method)))
        (if id
            ;; return a make-procedure
            (values #'#f
                    (compute-make-procedure formals body id))
            (values (compute-procedure formals body)
                    #'#f))))

    (syntax-case x ()
      ((_ args) #'(method args (if #f #f)))
      ((_ args body0 body1 ...)
       (with-syntax (((formals (specializer ...)) (parse-args #'args)))
         (call-with-values
             (lambda ()
               (compute-procedures #'formals #'(body0 body1 ...)))
           (lambda (procedure make-procedure)
             (with-syntax ((procedure procedure)
                           (make-procedure make-procedure))
               #'(make <method>
                   #:specializers (cons* specializer ...)
                   #:formals 'formals
                   #:body '(body0 body1 ...)
                   #:make-procedure make-procedure
                   #:procedure procedure)))))))))

;;;
;;; {add-method!}
;;;

(define (add-method-in-classes! m)
  ;; Add method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
	       (let ((dm (class-direct-methods x)))
		 (if (not (memq m dm))
		     (slot-set! x 'direct-methods (cons m dm)))))
	     (method-specializers m)))

(define (remove-method-in-classes! m)
  ;; Remove method in all the classes which appears in its specializers list
  (for-each* (lambda (x)
	       (slot-set! x
			  'direct-methods
			  (delv! m (class-direct-methods x))))
	     (method-specializers m)))

(define (compute-new-list-of-methods gf new)
  (let ((new-spec (method-specializers new))
	(methods  (slot-ref gf 'methods)))
    (let loop ((l methods))
      (if (null? l)
	  (cons new methods)
	  (if (equal? (method-specializers (car l)) new-spec)
	      (begin 
		;; This spec. list already exists. Remove old method from dependents
		(remove-method-in-classes! (car l))
		(set-car! l new) 
		methods)
	      (loop (cdr l)))))))

(define (method-n-specializers m)
  (length* (slot-ref m 'specializers)))

(define (calculate-n-specialized gf)
  (fold (lambda (m n) (max n (method-n-specializers m)))
        0
        (generic-function-methods gf)))

(define (invalidate-method-cache! gf)
  (%invalidate-method-cache! gf)
  (slot-set! gf 'n-specialized (calculate-n-specialized gf))
  (for-each (lambda (gf) (invalidate-method-cache! gf))
            (slot-ref gf 'extended-by)))

(define internal-add-method!
  (method ((gf <generic>) (m <method>))
    (slot-set! m  'generic-function gf)
    (slot-set! gf 'methods (compute-new-list-of-methods gf m))
    (invalidate-method-cache! gf)
    (add-method-in-classes! m)
    *unspecified*))

(define-generic add-method!)

((method-procedure internal-add-method!) add-method! internal-add-method!)

(define-method (add-method! (proc <procedure>) (m <method>))
  (if (generic-capability? proc)
      (begin
	(enable-primitive-generic! proc)
	(add-method! proc m))
      (next-method)))

(define-method (add-method! (pg <primitive-generic>) (m <method>))
  (add-method! (primitive-generic-generic pg) m))

(define-method (add-method! obj (m <method>))
  (goops-error "~S is not a valid generic function" obj))

;;;
;;; {Access to meta objects}
;;;

;;;
;;; Methods
;;;
(define-method (method-source (m <method>))
  (let* ((spec (map* class-name (slot-ref m 'specializers)))
	 (src (procedure-source (slot-ref m 'procedure))))
    (and src
         (let ((args (cadr src))
               (body (cddr src)))
           (cons 'method
                 (cons (map* list args spec)
                       body))))))

(define-method (method-formals (m <method>))
  (slot-ref m 'formals))

;;;
;;; Slots
;;;
(define slot-definition-name car)

(define slot-definition-options cdr)

(define (slot-definition-allocation s)
  (get-keyword #:allocation (cdr s) #:instance))

(define (slot-definition-getter s)
  (get-keyword #:getter (cdr s) #f))

(define (slot-definition-setter s)
  (get-keyword #:setter (cdr s) #f))

(define (slot-definition-accessor s)
  (get-keyword #:accessor (cdr s) #f))

(define (slot-definition-init-value s)
  ;; can be #f, so we can't use #f as non-value
  (get-keyword #:init-value (cdr s) (make-unbound)))

(define (slot-definition-init-form s)
  (get-keyword #:init-form (cdr s) (make-unbound)))

(define (slot-definition-init-thunk s)
  (get-keyword #:init-thunk (cdr s) #f))

(define (slot-definition-init-keyword s)
  (get-keyword #:init-keyword (cdr s) #f))

(define (class-slot-definition class slot-name)
  (assq slot-name (class-slots class)))

(define (slot-init-function class slot-name)
  (cadr (assq slot-name (slot-ref class 'getters-n-setters))))

(define (accessor-method-slot-definition obj)
  "Return the slot definition of the accessor @var{obj}."
  (slot-ref obj 'slot-definition))


;;;
;;; {Standard methods used by the C runtime}
;;;

;;; Methods to compare objects
;;;

;; Have to do this in a strange order because equal? is used in the
;; add-method! implementation; we need to make sure that when the
;; primitive is extended, that the generic has a method. =
(define g-equal? (make-generic 'equal?))
;; When this generic gets called, we will have already checked eq? and
;; eqv? -- the purpose of this generic is to extend equality. So by
;; default, there is no extension, thus the #f return.
(add-method! g-equal? (method (x y) #f)) 
(set-primitive-generic! equal? g-equal?)

;;;
;;; methods to display/write an object
;;;

;     Code for writing objects must test that the slots they use are
;     bound. Otherwise a slot-unbound method will be called and will 
;     conduct to an infinite loop.

;; Write
(define (display-address o file)
  (display (number->string (object-address o) 16) file))

(define-method (write o file)
  (display "#<instance " file)
  (display-address o file)
  (display #\> file))

(define write-object (primitive-generic-generic write))

(define-method (write (o <object>) file)
  (let ((class (class-of o)))
    (if (slot-bound? class 'name)
	(begin
	  (display "#<" file)
	  (display (class-name class) file)
	  (display #\space file)
	  (display-address o file)
	  (display #\> file))
	(next-method))))

(define-method (write (class <class>) file)
  (let ((meta (class-of class)))
    (if (and (slot-bound? class 'name)
	     (slot-bound? meta 'name))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (display #\space file)
	  (display (class-name class) file)
	  (display #\space file)
	  (display-address class file)
	  (display #\> file))
	(next-method))))

(define-method (write (gf <generic>) file)
  (let ((meta (class-of gf)))
    (if (and (slot-bound? meta 'name)
	     (slot-bound? gf 'methods))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (let ((name (generic-function-name gf)))
	    (if name
		(begin
		  (display #\space file)
		  (display name file))))
	  (display " (" file)
	  (display (length (generic-function-methods gf)) file)
	  (display ")>" file))
	(next-method))))

(define-method (write (o <method>) file)
  (let ((meta (class-of o)))
    (if (and (slot-bound? meta 'name)
	     (slot-bound? o 'specializers))
	(begin
	  (display "#<" file)
	  (display (class-name meta) file)
	  (display #\space file)
	  (display (map* (lambda (spec)
			   (if (slot-bound? spec 'name)
			       (slot-ref spec 'name)
			       spec))
			 (method-specializers o))
		   file)
	  (display #\space file)
	  (display-address o file)
	  (display #\> file))
	(next-method))))

;; Display (do the same thing as write by default)
(define-method (display o file) 
  (write-object o file))

;;;
;;; Handling of duplicate bindings in the module system
;;;

(define (find-subclass super name)
  (let lp ((classes (class-direct-subclasses super)))
    (cond
     ((null? classes)
      (error "class not found" name))
     ((and (slot-bound? (car classes) 'name)
           (eq? (class-name (car classes)) name))
      (car classes))
     (else
      (lp (cdr classes))))))

;; A record type.
(define <module> (find-subclass <top> '<module>))

(define-method (merge-generics (module <module>)
			       (name <symbol>)
			       (int1 <module>)
			       (val1 <top>)
			       (int2 <module>)
			       (val2 <top>)
			       (var <top>)
			       (val <top>))
  #f)

(define-method (merge-generics (module <module>)
			       (name <symbol>)
			       (int1 <module>)
			       (val1 <generic>)
			       (int2 <module>)
			       (val2 <generic>)
			       (var <top>)
			       (val <boolean>))
  (and (not (eq? val1 val2))
       (make-variable (make-extended-generic (list val2 val1) name))))

(define-method (merge-generics (module <module>)
			       (name <symbol>)
			       (int1 <module>)
			       (val1 <generic>)
			       (int2 <module>)
			       (val2 <generic>)
			       (var <top>)
			       (gf <extended-generic>))
  (and (not (memq val2 (slot-ref gf 'extends)))
       (begin
	 (slot-set! gf
		    'extends
		    (cons val2 (delq! val2 (slot-ref gf 'extends))))
	 (slot-set! val2
		    'extended-by
		    (cons gf (delq! gf (slot-ref val2 'extended-by))))
         (invalidate-method-cache! gf)
	 var)))

(module-define! duplicate-handlers 'merge-generics merge-generics)

(define-method (merge-accessors (module <module>)
				(name <symbol>)
				(int1 <module>)
				(val1 <top>)
				(int2 <module>)
				(val2 <top>)
				(var <top>)
				(val <top>))
  #f)

(define-method (merge-accessors (module <module>)
				(name <symbol>)
				(int1 <module>)
				(val1 <accessor>)
				(int2 <module>)
				(val2 <accessor>)
				(var <top>)
				(val <top>))
  (merge-generics module name int1 val1 int2 val2 var val))

(module-define! duplicate-handlers 'merge-accessors merge-accessors)

;;;
;;; slot access
;;;

(define (class-slot-g-n-s class slot-name)
  (let* ((this-slot (assq slot-name (slot-ref class 'slots)))
	 (g-n-s (cddr (or (assq slot-name (slot-ref class 'getters-n-setters))
			  (slot-missing class slot-name)))))
    (if (not (memq (slot-definition-allocation this-slot)
		   '(#:class #:each-subclass)))
	(slot-missing class slot-name))
    g-n-s))

(define (class-slot-ref class slot)
  (let ((x ((car (class-slot-g-n-s class slot)) #f)))
    (if (unbound? x)
	(slot-unbound class slot)
	x)))

(define (class-slot-set! class slot value)
  ((cadr (class-slot-g-n-s class slot)) #f value))

(define-method (slot-unbound (c <class>) (o <object>) s)
  (goops-error "Slot `~S' is unbound in object ~S" s o))

(define-method (slot-unbound (c <class>) s)
  (goops-error "Slot `~S' is unbound in class ~S" s c))

(define-method (slot-unbound (o <object>))
  (goops-error "Unbound slot in object ~S" o))

(define-method (slot-missing (c <class>) (o <object>) s)
  (goops-error "No slot with name `~S' in object ~S" s o))
  
(define-method (slot-missing (c <class>) s)
  (goops-error "No class slot with name `~S' in class ~S" s c))
  

(define-method (slot-missing (c <class>) (o <object>) s value)
  (slot-missing c o s))

;;; Methods for the possible error we can encounter when calling a gf

(define-method (no-next-method (gf <generic>) args)
  (goops-error "No next method when calling ~S\nwith arguments ~S" gf args))

(define-method (no-applicable-method (gf <generic>) args)
  (goops-error "No applicable method for ~S in call ~S"
	       gf (cons (generic-function-name gf) args)))

(define-method (no-method (gf <generic>) args)
  (goops-error "No method defined for ~S"  gf))

;;;
;;; {Cloning functions (from rdeline@CS.CMU.EDU)}
;;;

(define-method (shallow-clone (self <object>))
  (let ((clone (%allocate-instance (class-of self) '()))
	(slots (map slot-definition-name
		    (class-slots (class-of self)))))
    (for-each (lambda (slot)
		(if (slot-bound? self slot)
		    (slot-set! clone slot (slot-ref self slot))))
	      slots)
    clone))

(define-method (deep-clone  (self <object>))
  (let ((clone (%allocate-instance (class-of self) '()))
	(slots (map slot-definition-name
		    (class-slots (class-of self)))))
    (for-each (lambda (slot)
		(if (slot-bound? self slot)
		    (slot-set! clone slot
			       (let ((value (slot-ref self slot)))
				 (if (instance? value)
				     (deep-clone value)
				     value)))))
	      slots)
    clone))

;;;
;;; {Class redefinition utilities}
;;;

;;; (class-redefinition OLD NEW)
;;;

;;; Has correct the following conditions:

;;; Methods
;;; 
;;; 1. New accessor specializers refer to new header
;;; 
;;; Classes
;;; 
;;; 1. New class cpl refers to the new class header
;;; 2. Old class header exists on old super classes direct-subclass lists
;;; 3. New class header exists on new super classes direct-subclass lists

(define-method (class-redefinition (old <class>) (new <class>))
  ;; Work on direct methods:
  ;;		1. Remove accessor methods from the old class 
  ;;		2. Patch the occurences of new in the specializers by old
  ;;		3. Displace the methods from old to new
  (remove-class-accessors! old)					;; -1-
  (let ((methods (class-direct-methods new)))
    (for-each (lambda (m)
     	         (update-direct-method! m new old))	;; -2-
              methods)
    (slot-set! new
	       'direct-methods
	       (append methods (class-direct-methods old))))

  ;; Substitute old for new in new cpl
  (set-car! (slot-ref new 'cpl) old)
  
  ;; Remove the old class from the direct-subclasses list of its super classes
  (for-each (lambda (c) (slot-set! c 'direct-subclasses
				   (delv! old (class-direct-subclasses c))))
	    (class-direct-supers old))

  ;; Replace the new class with the old in the direct-subclasses of the supers
  (for-each (lambda (c)
	      (slot-set! c 'direct-subclasses
			 (cons old (delv! new (class-direct-subclasses c)))))
	    (class-direct-supers new))

  ;; Swap object headers
  (%modify-class old new)

  ;; Now old is NEW!

  ;; Redefine all the subclasses of old to take into account modification
  (for-each 
       (lambda (c)
	 (update-direct-subclass! c new old))
       (class-direct-subclasses new))

  ;; Invalidate class so that subsequent instances slot accesses invoke
  ;; change-object-class
  (slot-set! new 'redefined old)
  (%invalidate-class new) ;must come after slot-set!

  old)

;;;
;;; remove-class-accessors!
;;;

(define-method (remove-class-accessors! (c <class>))
  (for-each (lambda (m)
	      (if (is-a? m <accessor-method>)
		  (let ((gf (slot-ref m 'generic-function)))
		    ;; remove the method from its GF
		    (slot-set! gf 'methods
			       (delq1! m (slot-ref gf 'methods)))
		    (invalidate-method-cache! gf)
		    ;; remove the method from its specializers
		    (remove-method-in-classes! m))))
	    (class-direct-methods c)))

;;;
;;; update-direct-method!
;;;

(define-method (update-direct-method! (m  <method>)
				      (old <class>)
				      (new <class>))
  (let loop ((l (method-specializers m)))
    ;; Note: the <top> in dotted list is never used. 
    ;; So we can work as if we had only proper lists.
    (if (pair? l)       	  
	(begin
	  (if (eqv? (car l) old)  
	      (set-car! l new))
	  (loop (cdr l))))))

;;;
;;; update-direct-subclass!
;;;

(define-method (update-direct-subclass! (c <class>)
					(old <class>)
					(new <class>))
  (class-redefinition c
		      (make-class (class-direct-supers c)
				  (class-direct-slots c)
				  #:name (class-name c)
				  #:metaclass (class-of c))))

;;;
;;; {Utilities for INITIALIZE methods}
;;;

;;; compute-slot-accessors
;;;
(define (compute-slot-accessors class slots)
  (for-each
      (lambda (s g-n-s)
	(let ((getter-function (slot-definition-getter   s))
	      (setter-function (slot-definition-setter   s))
	      (accessor        (slot-definition-accessor s)))
	  (if getter-function
	      (add-method! getter-function
			   (compute-getter-method class g-n-s)))
	  (if setter-function
	      (add-method! setter-function
			   (compute-setter-method class g-n-s)))
	  (if accessor
	      (begin
		(add-method! accessor
			     (compute-getter-method class g-n-s))
		(add-method! (setter accessor)
			     (compute-setter-method class g-n-s))))))
      slots (slot-ref class 'getters-n-setters)))

(define-method (compute-getter-method (class <class>) slotdef)
  (let ((init-thunk (cadr slotdef))
	(g-n-s (cddr slotdef)))
    (make <accessor-method>
          #:specializers (list class)
	  #:procedure (cond ((pair? g-n-s)
			     (make-generic-bound-check-getter (car g-n-s)))
			    (init-thunk
			     (standard-get g-n-s))
			    (else
			     (bound-check-get g-n-s)))
	  #:slot-definition slotdef)))

(define-method (compute-setter-method (class <class>) slotdef)
  (let ((g-n-s (cddr slotdef)))
    (make <accessor-method>
          #:specializers (list class <top>)
	  #:procedure (if (pair? g-n-s)
			  (cadr g-n-s)
			  (standard-set g-n-s))
	  #:slot-definition slotdef)))

(define (make-generic-bound-check-getter proc)
  (lambda (o)
    (let ((val (proc o)))
      (if (unbound? val)
          (slot-unbound o)
          val))))

;;; Pre-generate getters and setters for the first 20 slots.
(define-syntax define-standard-accessor-method
  (lambda (stx)
    (define num-standard-pre-cache 20)
    (syntax-case stx ()
      ((_ ((proc n) arg ...) body)
       #`(define proc
           (let ((cache (vector #,@(map (lambda (n*)
                                          #`(lambda (arg ...)
                                              (let ((n #,n*))
                                                body)))
                                        (iota num-standard-pre-cache)))))
             (lambda (n)
               (if (< n #,num-standard-pre-cache)
                   (vector-ref cache n)
                   (lambda (arg ...) body)))))))))

(define-standard-accessor-method ((bound-check-get n) o)
  (let ((x (struct-ref o n)))
    (if (unbound? x)
        (slot-unbound o)
        x)))

(define-standard-accessor-method ((standard-get n) o)
  (struct-ref o n))

(define-standard-accessor-method ((standard-set n) o v)
  (struct-set! o n v))

;;; compute-getters-n-setters
;;;
(define (compute-getters-n-setters class slots)

  (define (compute-slot-init-function name s)
    (or (let ((thunk (slot-definition-init-thunk s)))
	  (and thunk
	       (if (thunk? thunk)
                   thunk
                   (goops-error "Bad init-thunk for slot `~S' in ~S: ~S"
                                name class thunk))))
	(let ((init (slot-definition-init-value s)))
	  (and (not (unbound? init))
	       (lambda () init)))))

  (define (verify-accessors slot l)
    (cond ((integer? l))
	  ((not (and (list? l) (= (length l) 2)))
	   (goops-error "Bad getter and setter for slot `~S' in ~S: ~S"
			slot class l))
	  (else
	   (let ((get (car l)) 
		 (set (cadr l)))
	     (if (not (procedure? get))
                 (goops-error "Bad getter closure for slot `~S' in ~S: ~S"
			      slot class get))
	     (if (not (procedure? set))
                 (goops-error "Bad setter closure for slot `~S' in ~S: ~S"
			      slot class set))))))

  (map (lambda (s)
	 ;; The strange treatment of nfields is due to backward compatibility.
	 (let* ((index (slot-ref class 'nfields))
		(g-n-s (compute-get-n-set class s))
		(size (- (slot-ref class 'nfields) index))
		(name  (slot-definition-name s)))
	   ;; NOTE: The following is interdependent with C macros
	   ;; defined above goops.c:scm_sys_prep_layout_x.
	   ;;
	   ;; For simple instance slots, we have the simplest form
	   ;; '(name init-function . index)
	   ;; For other slots we have
	   ;; '(name init-function getter setter . alloc)
	   ;; where alloc is:
	   ;;   '(index size) for instance allocated slots
	   ;;   '() for other slots
	   (verify-accessors name g-n-s)
           (case (slot-definition-allocation s)
             ((#:each-subclass #:class)
              (unless (and (zero? size) (pair? g-n-s))
                (error "Class-allocated slots should not reserve fields"))
              ;; Don't initialize the slot; that's handled when the slot
              ;; is allocated, in compute-get-n-set.
              (cons name (cons #f g-n-s)))
             (else
              (cons name
                    (cons (compute-slot-init-function name s)
                          (if (or (integer? g-n-s)
                                  (zero? size))
                              g-n-s
                              (append g-n-s (list index size)))))))))
       slots))

;;; compute-cpl
;;;

;; Replace the bootstrap compute-cpl with this definition.
(define compute-cpl
  (make <generic> #:name 'compute-cpl))

(define-method (compute-cpl (class <class>))
  (compute-std-cpl class class-direct-supers))

;;; compute-get-n-set
;;;
(define-method (compute-get-n-set (class <class>) s)
  (define (class-slot-init-value)
    (let ((thunk (slot-definition-init-thunk s)))
      (if thunk
          (thunk)
          (slot-definition-init-value s))))

  (case (slot-definition-allocation s)
    ((#:instance) ;; Instance slot
     ;; get-n-set is just its offset
     (let ((already-allocated (slot-ref class 'nfields)))
       (slot-set! class 'nfields (+ already-allocated 1))
       already-allocated))

    ((#:class)  ;; Class slot
     ;; Class-slots accessors are implemented as 2 closures around 
     ;; a Scheme variable. As instance slots, class slots must be
     ;; unbound at init time.
     (let ((name (slot-definition-name s)))
       (if (memq name (map slot-definition-name (class-direct-slots class)))
	   ;; This slot is direct; create a new shared variable
	   (make-closure-variable class (class-slot-init-value))
	   ;; Slot is inherited. Find its definition in superclass
	   (let loop ((l (cdr (class-precedence-list class))))
	     (let ((r (assoc name (slot-ref (car l) 'getters-n-setters))))
	       (if r
		   (cddr r)
		   (loop (cdr l))))))))

    ((#:each-subclass) ;; slot shared by instances of direct subclass.
     ;; (Thomas Buerger, April 1998)
     (make-closure-variable class (class-slot-init-value)))

    ((#:virtual) ;; No allocation
     ;; slot-ref and slot-set! function must be given by the user
     (let ((get (get-keyword #:slot-ref  (slot-definition-options s) #f))
	   (set (get-keyword #:slot-set! (slot-definition-options s) #f)))
       (if (not (and get set))
	   (goops-error "You must supply a #:slot-ref and a #:slot-set! in ~S"
			s))
       (list get set)))
    (else    (next-method))))

(define (make-closure-variable class value)
  (list (lambda (o) value)
        (lambda (o v) (set! value v))))

(define-method (compute-get-n-set (o <object>) s)
  (goops-error "Allocation \"~S\" is unknown" (slot-definition-allocation s)))

(define-method (compute-slots (class <class>))
  (build-slots-list (class-direct-slots class)
                    (class-precedence-list class)))

;;;
;;; {Initialize}
;;;

(define-method (initialize (object <object>) initargs)
  (%initialize-object object initargs))

(define-method (initialize (class <class>) initargs)
  (next-method)
  (let ((dslots (get-keyword #:slots initargs '()))
	(supers (get-keyword #:dsupers	  initargs '())))
    (slot-set! class 'name	  	(get-keyword #:name initargs '???))
    (slot-set! class 'direct-supers 	supers)
    (slot-set! class 'direct-slots  	dslots)
    (slot-set! class 'direct-subclasses '())
    (slot-set! class 'direct-methods    '())
    (slot-set! class 'cpl		(compute-cpl class))
    (slot-set! class 'redefined		#f)
    (let ((slots (compute-slots class)))
      (slot-set! class 'slots	  	  slots)
      (slot-set! class 'nfields	  	  0)
      (slot-set! class 'getters-n-setters (compute-getters-n-setters class 
								     slots))
      ;; Build getters - setters - accessors
      (compute-slot-accessors class slots))

    ;; Update the "direct-subclasses" of each inherited classes
    (for-each (lambda (x)
		(slot-set! x
			   'direct-subclasses 
			   (cons class (slot-ref x 'direct-subclasses))))
	      supers)

    ;; Support for the underlying structs:
    
    ;; Set the layout slot
    (%prep-layout! class)
    ;; Inherit class flags (invisible on scheme level) from supers
    (%inherit-magic! class supers)))

(define (initialize-object-procedure object initargs)
  (let ((proc (get-keyword #:procedure initargs #f)))
    (cond ((not proc))
	  ((pair? proc)
	   (apply slot-set! object 'procedure proc))
	  (else
           (slot-set! object 'procedure proc)))))

(define-method (initialize (applicable-struct <applicable-struct>) initargs)
  (next-method)
  (initialize-object-procedure applicable-struct initargs))

(define-method (initialize (applicable-struct <applicable-struct-with-setter>)
                           initargs)
  (next-method)
  (slot-set! applicable-struct 'setter (get-keyword #:setter initargs #f)))

(define-method (initialize (generic <generic>) initargs)
  (let ((previous-definition (get-keyword #:default initargs #f))
	(name (get-keyword #:name initargs #f)))
    (next-method)
    (slot-set! generic 'methods (if (is-a? previous-definition <procedure>)
				    (list (method args
                                            (apply previous-definition args)))
				    '()))
    (if name
	(set-procedure-property! generic 'name name))
    (invalidate-method-cache! generic)))

(define-method (initialize (eg <extended-generic>) initargs)
  (next-method)
  (slot-set! eg 'extends (get-keyword #:extends initargs '())))

(define dummy-procedure (lambda args *unspecified*))

(define-method (initialize (method <method>) initargs)
  (next-method)
  (slot-set! method 'generic-function (get-keyword #:generic-function initargs #f))
  (slot-set! method 'specializers (get-keyword #:specializers initargs '()))
  (slot-set! method 'procedure
	     (get-keyword #:procedure initargs #f))
  (slot-set! method 'formals (get-keyword #:formals initargs '()))
  (slot-set! method 'body (get-keyword #:body initargs '()))
  (slot-set! method 'make-procedure (get-keyword #:make-procedure initargs #f)))
             

;;;
;;; {Change-class}
;;;

(define (change-object-class old-instance old-class new-class)
  (let ((new-instance (allocate-instance new-class '())))
    ;; Initialize the slots of the new instance
    (for-each (lambda (slot)
		(if (and (slot-exists-using-class? old-class old-instance slot)
			 (eq? (slot-definition-allocation
			       (class-slot-definition old-class slot))
			      #:instance)
			 (slot-bound-using-class? old-class old-instance slot))
		    ;; Slot was present and allocated in old instance; copy it 
		    (slot-set-using-class!
		     new-class 
		     new-instance 
		     slot 
		     (slot-ref-using-class old-class old-instance slot))
		    ;; slot was absent; initialize it with its default value
		    (let ((init (slot-init-function new-class slot)))
		      (if init
			  (slot-set-using-class!
			       new-class 
			       new-instance 
			       slot
			       (apply init '()))))))
	      (map slot-definition-name (class-slots new-class)))
    ;; Exchange old and new instance in place to keep pointers valid
    (%modify-instance old-instance new-instance)
    ;; Allow class specific updates of instances (which now are swapped)
    (update-instance-for-different-class new-instance old-instance)
    old-instance))


(define-method (update-instance-for-different-class (old-instance <object>)
						    (new-instance
						     <object>))
  ;;not really important what we do, we just need a default method
  new-instance)

(define-method (change-class (old-instance <object>) (new-class <class>))
  (change-object-class old-instance (class-of old-instance) new-class))

;;;
;;; {make}
;;;
;;; A new definition which overwrites the previous one which was built-in
;;;

(define-method (allocate-instance (class <class>) initargs)
  (%allocate-instance class initargs))

(define-method (make-instance (class <class>) . initargs)
  (let ((instance (allocate-instance class initargs)))
    (initialize instance initargs)
    instance))

(define make make-instance)

;;;
;;; {apply-generic}
;;;
;;; Protocol for calling standard generic functions.  This protocol is
;;; not used for real <generic> functions (in this case we use a
;;; completely C hard-coded protocol).  Apply-generic is used by
;;; goops for calls to subclasses of <generic> and <generic-with-setter>.
;;; The code below is similar to the first MOP described in AMOP. In
;;; particular, it doesn't used the currified approach to gf
;;; call. There are 2 reasons for that:
;;;   - the protocol below is exposed to mimic completely the one written in C
;;;   - the currified protocol would be imho inefficient in C.
;;;

(define-method (apply-generic (gf <generic>) args)
  (if (null? (slot-ref gf 'methods))
      (no-method gf args))
  (let ((methods (compute-applicable-methods gf args)))
    (if methods
	(apply-methods gf (sort-applicable-methods gf methods args) args)
	(no-applicable-method gf args))))

;; compute-applicable-methods is bound to %compute-applicable-methods.
;; *fixme* use let
(define %%compute-applicable-methods
  (make <generic> #:name 'compute-applicable-methods))

(define-method (%%compute-applicable-methods (gf <generic>) args)
  (%compute-applicable-methods gf args))

(set! compute-applicable-methods %%compute-applicable-methods)

(define-method (sort-applicable-methods (gf <generic>) methods args)
  (%sort-applicable-methods methods (map class-of args)))

(define-method (method-more-specific? (m1 <method>) (m2 <method>) targs)
  (%method-more-specific? m1 m2 targs))

(define-method (apply-method (gf <generic>) methods build-next args)
  (apply (method-procedure (car methods))
	 (build-next (cdr methods) args)
	 args))

(define-method (apply-methods (gf <generic>) (l <list>) args)
  (letrec ((next (lambda (procs args)
		   (lambda new-args
		     (let ((a (if (null? new-args) args new-args)))
		       (if (null? procs)
			   (no-next-method gf a)
			   (apply-method gf procs next a)))))))
    (apply-method gf l next args)))

;; We don't want the following procedure to turn up in backtraces:
(for-each (lambda (proc)
	    (set-procedure-property! proc 'system-procedure #t))
	  (list slot-unbound
		slot-missing
		no-next-method
		no-applicable-method
		no-method
		))

;;;
;;; {<composite-metaclass> and <active-metaclass>}
;;;

;(autoload "active-slot"    <active-metaclass>)
;(autoload "composite-slot" <composite-metaclass>)
;(export <composite-metaclass> <active-metaclass>)

;;;
;;; {Tools}
;;;

;; list2set
;;
;; duplicate the standard list->set function but using eq instead of
;; eqv which really sucks a lot, uselessly here
;;
(define (list2set l)	       
  (let loop ((l l)
	     (res '()))
    (cond		       
     ((null? l) res)
     ((memq (car l) res) (loop (cdr l) res))
     (else (loop (cdr l) (cons (car l) res))))))

(define (class-subclasses c)
  (letrec ((allsubs (lambda (c)
		      (cons c (mapappend allsubs
					 (class-direct-subclasses c))))))
    (list2set (cdr (allsubs c)))))

(define (class-methods c)
  (list2set (mapappend class-direct-methods
		       (cons c (class-subclasses c)))))

;;;
;;; {Final initialization}
;;;

;; Tell C code that the main bulk of Goops has been loaded
(%goops-loaded)




;;;
;;; {SMOB and port classes}
;;;

(define <arbiter> (find-subclass <top> '<arbiter>))
(define <promise> (find-subclass <top> '<promise>))
(define <thread> (find-subclass <top> '<thread>))
(define <mutex> (find-subclass <top> '<mutex>))
(define <condition-variable> (find-subclass <top> '<condition-variable>))
(define <regexp> (find-subclass <top> '<regexp>))
(define <hook> (find-subclass <top> '<hook>))
(define <bitvector> (find-subclass <top> '<bitvector>))
(define <random-state> (find-subclass <top> '<random-state>))
(define <async> (find-subclass <top> '<async>))
(define <directory> (find-subclass <top> '<directory>))
(define <array> (find-subclass <top> '<array>))
(define <character-set> (find-subclass <top> '<character-set>))
(define <dynamic-object> (find-subclass <top> '<dynamic-object>))
(define <guardian> (find-subclass <applicable> '<guardian>))
(define <macro> (find-subclass <top> '<macro>))

(define (define-class-subtree class)
  (define! (class-name class) class)
  (for-each define-class-subtree (class-direct-subclasses class)))

(define-class-subtree (find-subclass <port> '<file-port>))
