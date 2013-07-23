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
;;; This is the continuation-passing style (CPS) intermediate language
;;; (IL) for Guile.
;;;
;;; There are two kinds of terms in CPS: terms that bind continuations,
;;; and terms that call continuations.
;;;
;;; $letk binds a set of mutually recursive continuations, each one an
;;; instance of $cont.  A $cont declares the name and source of a
;;; continuation, and then contains as a subterm the particular
;;; continuation instance: $kif for test continuations, $kargs for
;;; continuations that bind values, etc.
;;;
;;; $continue nodes call continuations.  The expression contained in the
;;; $continue node determines the value or values that are passed to the
;;; target continuation: $const to pass a constant value, $values to
;;; pass multiple named values, etc.
;;;
;;; Additionally there is $letrec, a term that binds mutually recursive
;;; functions.  The contification pass will turn $letrec into $letk if
;;; it can do so.  Otherwise, the closure conversion pass will desugar
;;; $letrec into an equivalent sequence of make-closure primcalls and
;;; subsequent initializations of the captured variables of the
;;; closures.  You can think of $letrec as pertaining to "high CPS",
;;; whereas later passes will only see "low CPS", which does not have
;;; $letrec.
;;;
;;; This particular formulation of CPS was inspired by Andrew Kennedy's
;;; 2007 paper, "Compiling with Continuations, Continued".  All Guile
;;; hackers should read that excellent paper!  As in Kennedy's paper,
;;; continuations are second-class, and may be thought of as basic block
;;; labels.  All values are bound to variables using continuation calls:
;;; even constants!
;;;
;;; There are some Guile-specific quirks as well:
;;;
;;;   - $ktrunc represents a continuation that receives multiple values,
;;;     but which truncates them to some number of required values,
;;;     possibly with a rest list.
;;;
;;;   - $kentry labels an entry point for a $fun (a function), and
;;;     contains a $ktail representing the formal argument which is the
;;;     function's continuation.
;;;
;;;   - $kentry also contains $kclause continuations, corresponding to
;;;     the case-lambda clauses of the function.  $kclause actually
;;;     contains the clause body.  This is because the $kclause
;;;     logically matches or doesn't match a given set of actual
;;;     arguments against a formal arity, then proceeds to a "body"
;;;     continuation (which is a $kargs).
;;;
;;;     That's to say that a $fun can be matched like this:
;;;
;;;     (match f
;;;       (($ $fun meta free
;;;           ($ $cont kentry src
;;;              ($ $kentry self ($ $cont ktail _ ($ $ktail))
;;;                 (($ $kclause arity
;;;                     ($ $cont kbody _ ($ $kargs names syms body)))
;;;                  ...))))
;;;         #t))
;;;
;;;     A $continue to ktail is in tail position.  $kentry, $kclause,
;;;     and $ktail will never be seen elsewhere in a CPS term.
;;;
;;;   - $prompt continues to the body of the prompt, having pushed on a
;;;     prompt whose handler will continue at its "handler"
;;;     continuation.  The continuation of the prompt is responsible for
;;;     popping the prompt.
;;;
;;; In summary:
;;;
;;;   - $letk, $letrec, and $continue are terms.
;;;
;;;   - $cont is a continuation, containing a continuation body ($kargs,
;;;     $kif, etc).
;;;
;;;   - $continue terms contain an expression ($call, $const, $fun,
;;;     etc).
;;;
;;; See (language tree-il compile-cps) for details on how Tree-IL
;;; converts to CPS.
;;;
;;; Code:

(define-module (language cps)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (;; Helper.
            $arity
            make-$arity

            ;; Terms.
            $letk $continue $letrec

            ;; Continuations.
            $cont

            ;; Continuation bodies.
            $kif $ktrunc $kargs $kentry $ktail $kclause

            ;; Expressions.
            $var $void $const $prim $fun $call $primcall $values $prompt

            ;; Building macros.
            let-gensyms
            build-cps-term build-cps-cont build-cps-exp
            rewrite-cps-term rewrite-cps-cont rewrite-cps-exp

            ;; Misc.
            parse-cps unparse-cps
            fold-conts fold-local-conts))

;; FIXME: Use SRFI-99, when Guile adds it.
(define-syntax define-record-type*
  (lambda (x)
    (define (id-append ctx . syms)
      (datum->syntax ctx (apply symbol-append (map syntax->datum syms))))
    (syntax-case x ()
      ((_ name field ...)
       (and (identifier? #'name) (and-map identifier? #'(field ...)))
       (with-syntax ((cons (id-append #'name #'make- #'name))
                     (pred (id-append #'name #'name #'?))
                     ((getter ...) (map (lambda (f)
                                          (id-append f #'name #'- f))
                                        #'(field ...))))
         #'(define-record-type name
             (cons field ...)
             pred
             (field getter)
             ...))))))

(define-syntax-rule (define-cps-type name field ...)
  (begin
    (define-record-type* name field ...)
    (set-record-type-printer! name print-cps)))

(define (print-cps exp port)
  (format port "#<cps ~S>" (unparse-cps exp)))

;; Helper.
(define-record-type* $arity req opt rest kw allow-other-keys?)

;; Terms.
(define-cps-type $letk conts body)
(define-cps-type $continue k exp)
(define-cps-type $letrec names syms funs body)

;; Continuations
(define-cps-type $cont k src cont)
(define-cps-type $kif kt kf)
(define-cps-type $ktrunc arity k)
(define-cps-type $kargs names syms body)
(define-cps-type $kentry self tail clauses)
(define-cps-type $ktail)
(define-cps-type $kclause arity cont)

;; Expressions.
(define-cps-type $var sym)
(define-cps-type $void)
(define-cps-type $const val)
(define-cps-type $prim name)
(define-cps-type $fun meta free body)
(define-cps-type $call proc args)
(define-cps-type $primcall name args)
(define-cps-type $values args)
(define-cps-type $prompt escape? tag handler)

(define-syntax let-gensyms
  (syntax-rules ()
    ((_ (sym ...) body body* ...)
     (let ((sym (gensym (symbol->string 'sym))) ...)
       body body* ...))))

(define-syntax build-arity
  (syntax-rules (unquote)
    ((_ (unquote exp)) exp)
    ((_ (req opt rest kw allow-other-keys?))
     (make-$arity req opt rest kw allow-other-keys?))))

(define-syntax build-cont-body
  (syntax-rules (unquote $kif $ktrunc $kargs $kentry $ktail $kclause)
    ((_ (unquote exp))
     exp)
    ((_ ($kif kt kf))
     (make-$kif kt kf))
    ((_ ($ktrunc req rest kargs))
     (make-$ktrunc (make-$arity req '() rest '() #f) kargs))
    ((_ ($kargs (name ...) (sym ...) body))
     (make-$kargs (list name ...) (list sym ...) (build-cps-term body)))
    ((_ ($kargs names syms body))
     (make-$kargs names syms (build-cps-term body)))
    ((_ ($kentry self tail (unquote clauses)))
     (make-$kentry self (build-cps-cont tail) clauses))
    ((_ ($kentry self tail (clause ...)))
     (make-$kentry self (build-cps-cont tail) (list (build-cps-cont clause) ...)))
    ((_ ($ktail))
     (make-$ktail))
    ((_ ($kclause arity cont))
     (make-$kclause (build-arity arity) (build-cps-cont cont)))))

(define-syntax build-cps-cont
  (syntax-rules (unquote)
    ((_ (unquote exp)) exp)
    ((_ (k src cont)) (make-$cont k src (build-cont-body cont)))))

(define-syntax build-cps-exp
  (syntax-rules (unquote
                 $var $void $const $prim $fun $call $primcall $values $prompt)
    ((_ (unquote exp)) exp)
    ((_ ($var sym)) (make-$var sym))
    ((_ ($void)) (make-$void))
    ((_ ($const val)) (make-$const val))
    ((_ ($prim name)) (make-$prim name))
    ((_ ($fun meta free body)) (make-$fun meta free (build-cps-cont body)))
    ((_ ($call proc (arg ...))) (make-$call proc (list arg ...)))
    ((_ ($call proc args)) (make-$call proc args))
    ((_ ($primcall name (arg ...))) (make-$primcall name (list arg ...)))
    ((_ ($primcall name args)) (make-$primcall name args))
    ((_ ($values (arg ...))) (make-$values (list arg ...)))
    ((_ ($values args)) (make-$values args))
    ((_ ($prompt escape? tag handler)) (make-$prompt escape? tag handler))))

(define-syntax build-cps-term
  (syntax-rules (unquote $letk $letk* $letconst $letrec $continue)
    ((_ (unquote exp))
     exp)
    ((_ ($letk (unquote conts) body))
     (make-$letk conts (build-cps-term body)))
    ((_ ($letk (cont ...) body))
     (make-$letk (list (build-cps-cont cont) ...)
                 (build-cps-term body)))
    ((_ ($letk* () body))
     (build-cps-term body))
    ((_ ($letk* (cont conts ...) body))
     (build-cps-term ($letk (cont) ($letk* (conts ...) body))))
    ((_ ($letconst () body))
     (build-cps-term body))
    ((_ ($letconst ((name sym val) tail ...) body))
     (let-gensyms (kconst)
       (build-cps-term
         ($letk ((kconst #f ($kargs (name) (sym) ($letconst (tail ...) body))))
           ($continue kconst ($const val))))))
    ((_ ($letrec names gensyms funs body))
     (make-$letrec names gensyms funs (build-cps-term body)))
    ((_ ($continue k exp))
     (make-$continue k (build-cps-exp exp)))))

(define-syntax-rule (rewrite-cps-term x (pat body) ...)
  (match x
    (pat (build-cps-term body)) ...))
(define-syntax-rule (rewrite-cps-cont x (pat body) ...)
  (match x
    (pat (build-cps-cont body)) ...))
(define-syntax-rule (rewrite-cps-exp x (pat body) ...)
  (match x
    (pat (build-cps-exp body)) ...))

(define (parse-cps exp)
  (define (src exp)
    (let ((props (source-properties exp)))
      (and (pair? props) props)))
  (match exp
    ;; Continuations.
    (('letconst k (name sym c) body)
     (build-cps-term
       ($letk ((k (src exp) ($kargs (name) (sym)
                              ,(parse-cps body))))
         ($continue k ($const c)))))
    (('let k (name sym val) body)
     (build-cps-term
      ($letk ((k (src exp) ($kargs (name) (sym)
                             ,(parse-cps body))))
        ,(parse-cps val))))
    (('letk (cont ...) body)
     (build-cps-term
       ($letk ,(map parse-cps cont) ,(parse-cps body))))
    (('k sym body)
     (build-cps-cont
       (sym (src exp) ,(parse-cps body))))
    (('kif kt kf)
     (build-cont-body ($kif kt kf)))
    (('ktrunc req rest k)
     (build-cont-body ($ktrunc req rest k)))
    (('kargs names syms body)
     (build-cont-body ($kargs names syms ,(parse-cps body))))
    (('kentry self tail clauses)
     (build-cont-body
      ($kentry self ,(parse-cps tail) ,(map parse-cps clauses))))
    (('ktail)
     (build-cont-body
      ($ktail)))
    (('kclause (req opt rest kw allow-other-keys?) body)
     (build-cont-body
      ($kclause (req opt rest kw allow-other-keys?)
        ,(parse-cps body))))
    (('kseq body)
     (build-cont-body ($kargs () () ,(parse-cps body))))

    ;; Calls.
    (('continue k exp)
     (build-cps-term ($continue k ,(parse-cps exp))))
    (('var sym)
     (build-cps-exp ($var sym)))
    (('void)
     (build-cps-exp ($void)))
    (('const exp)
     (build-cps-exp ($const exp)))
    (('prim name)
     (build-cps-exp ($prim name)))
    (('fun meta free body)
     (build-cps-exp ($fun meta free ,(parse-cps body))))
    (('letrec ((name sym fun) ...) body)
     (build-cps-term
       ($letrec name sym (map parse-cps fun) ,(parse-cps body))))
    (('call proc arg ...)
     (build-cps-exp ($call proc arg)))
    (('primcall name arg ...)
     (build-cps-exp ($primcall name arg)))
    (('values arg ...)
     (build-cps-exp ($values arg)))
    (('prompt escape? tag handler)
     (build-cps-exp ($prompt escape? tag handler)))
    (_
     (error "unexpected cps" exp))))

(define (unparse-cps exp)
  (match exp
    ;; Continuations.
    (($ $letk (($ $cont k src ($ $kargs (name) (sym) body)))
        ($ $continue k ($ $const c)))
     `(letconst ,k (,name ,sym ,c)
                ,(unparse-cps body)))
    (($ $letk (($ $cont k src ($ $kargs (name) (sym) body))) val)
     `(let ,k (,name ,sym ,(unparse-cps val))
           ,(unparse-cps body)))
    (($ $letk conts body)
     `(letk ,(map unparse-cps conts) ,(unparse-cps body)))
    (($ $cont sym src body)
     `(k ,sym ,(unparse-cps body)))
    (($ $kif kt kf)
     `(kif ,kt ,kf))
    (($ $ktrunc ($ $arity req () rest '() #f) k)
     `(ktrunc ,req ,rest ,k))
    (($ $kargs () () body)
     `(kseq ,(unparse-cps body)))
    (($ $kargs names syms body)
     `(kargs ,names ,syms ,(unparse-cps body)))
    (($ $kentry self tail clauses)
     `(kentry ,self ,(unparse-cps tail) ,(map unparse-cps clauses)))
    (($ $ktail)
     `(ktail))
    (($ $kclause ($ $arity req opt rest kw allow-other-keys?) body)
     `(kclause (,req ,opt ,rest ,kw ,allow-other-keys?) ,(unparse-cps body)))

    ;; Calls.
    (($ $continue k exp)
     `(continue ,k ,(unparse-cps exp)))
    (($ $var sym)
     `(var ,sym))
    (($ $void)
     `(void))
    (($ $const val)
     `(const ,val))
    (($ $prim name)
     `(prim ,name))
    (($ $fun meta free body)
     `(fun ,meta ,free ,(unparse-cps body)))
    (($ $letrec names syms funs body)
     `(letrec ,(map (lambda (name sym fun)
                      (list name sym (unparse-cps fun)))
                    names syms funs)
        ,(unparse-cps body)))
    (($ $call proc args)
     `(call ,proc ,@args))
    (($ $primcall name args)
     `(primcall ,name ,@args))
    (($ $values args)
     `(values ,@args))
    (($ $prompt escape? tag handler)
     `(prompt ,escape? ,tag ,handler))
    (_
     (error "unexpected cps" exp))))

(define (fold-conts proc seed fun)
  (define (cont-folder cont seed)
    (match cont
      (($ $cont k src cont)
       (let ((seed (proc k src cont seed)))
         (match cont
           (($ $kargs names syms body)
            (term-folder body seed))

           (($ $kentry self tail clauses)
            (fold cont-folder (cont-folder tail seed) clauses))

           (($ $kclause arity body)
            (cont-folder body seed))

           (_ seed))))))

  (define (fun-folder fun seed)
    (match fun
      (($ $fun meta free body)
       (cont-folder body seed))))

  (define (term-folder term seed)
    (match term
      (($ $letk conts body)
       (fold cont-folder (term-folder body seed) conts))

      (($ $continue k exp)
       (match exp
         (($ $fun) (fun-folder exp seed))
         (_ seed)))

      (($ $letrec names syms funs body)
       (fold fun-folder (term-folder body seed) funs))))

  (fun-folder fun seed))

(define (fold-local-conts proc seed cont)
  (define (cont-folder cont seed)
    (match cont
      (($ $cont k src cont)
       (let ((seed (proc k src cont seed)))
         (match cont
           (($ $kargs names syms body)
            (term-folder body seed))

           (($ $kentry self tail clauses)
            (fold cont-folder (cont-folder tail seed) clauses))

           (($ $kclause arity body)
            (cont-folder body seed))

           (_ seed))))))

  (define (term-folder term seed)
    (match term
      (($ $letk conts body)
       (fold cont-folder (term-folder body seed) conts))

      (($ $continue) seed)

      (($ $letrec names syms funs body) (term-folder body seed))))

  (cont-folder cont seed))
