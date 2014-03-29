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
;;; This is the continuation-passing style (CPS) intermediate language
;;; (IL) for Guile.
;;;
;;; There are two kinds of terms in CPS: terms that bind continuations,
;;; and terms that call continuations.
;;;
;;; $letk binds a set of mutually recursive continuations, each one an
;;; instance of $cont.  A $cont declares the name of a continuation, and
;;; then contains as a subterm the particular continuation instance:
;;; $kif for test continuations, $kargs for continuations that bind
;;; values, etc.
;;;
;;; $continue nodes call continuations.  The expression contained in the
;;; $continue node determines the value or values that are passed to the
;;; target continuation: $const to pass a constant value, $values to
;;; pass multiple named values, etc.  $continue nodes also record the source at which 
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
;;;   - $kreceive represents a continuation that receives multiple values,
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
;;;       (($ $fun src meta free
;;;           ($ $cont kentry
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
  #:use-module (srfi srfi-11)
  #:export (;; Helper.
            $arity
            make-$arity

            ;; Terms.
            $letk $continue $letrec

            ;; Continuations.
            $cont

            ;; Continuation bodies.
            $kif $kreceive $kargs $kentry $ktail $kclause

            ;; Expressions.
            $void $const $prim $fun $call $callk $primcall $values $prompt

            ;; Fresh names.
            label-counter var-counter
            fresh-label fresh-var
            with-fresh-name-state compute-max-label-and-var
            let-fresh

            ;; Building macros.
            build-cps-term build-cps-cont build-cps-exp
            rewrite-cps-term rewrite-cps-cont rewrite-cps-exp

            ;; Misc.
            parse-cps unparse-cps
            make-cont-folder fold-conts fold-local-conts))

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
(define-cps-type $continue k src exp)
(define-cps-type $letrec names syms funs body)

;; Continuations
(define-cps-type $cont k cont)
(define-cps-type $kif kt kf)
(define-cps-type $kreceive arity k)
(define-cps-type $kargs names syms body)
(define-cps-type $kentry self tail clauses)
(define-cps-type $ktail)
(define-cps-type $kclause arity cont)

;; Expressions.
(define-cps-type $void)
(define-cps-type $const val)
(define-cps-type $prim name)
(define-cps-type $fun src meta free body)
(define-cps-type $call proc args)
(define-cps-type $callk k proc args)
(define-cps-type $primcall name args)
(define-cps-type $values args)
(define-cps-type $prompt escape? tag handler)

(define label-counter (make-parameter #f))
(define var-counter (make-parameter #f))

(define (fresh-label)
  (let ((count (or (label-counter)
                   (error "fresh-label outside with-fresh-name-state"))))
    (label-counter (1+ count))
    count))

(define (fresh-var)
  (let ((count (or (var-counter)
                   (error "fresh-var outside with-fresh-name-state"))))
    (var-counter (1+ count))
    count))

(define-syntax-rule (let-fresh (label ...) (var ...) body ...)
  (let ((label (fresh-label)) ...
        (var (fresh-var)) ...)
    body ...))

(define-syntax-rule (with-fresh-name-state fun body ...)
  (begin
    (when (or (label-counter) (var-counter))
      (error "with-fresh-name-state should not be called recursively"))
    (call-with-values (lambda ()
                        (compute-max-label-and-var fun))
      (lambda (max-label max-var)
        (parameterize ((label-counter (1+ max-label))
                       (var-counter (1+ max-var)))
          body ...)))))

(define-syntax build-arity
  (syntax-rules (unquote)
    ((_ (unquote exp)) exp)
    ((_ (req opt rest kw allow-other-keys?))
     (make-$arity req opt rest kw allow-other-keys?))))

(define-syntax build-cont-body
  (syntax-rules (unquote $kif $kreceive $kargs $kentry $ktail $kclause)
    ((_ (unquote exp))
     exp)
    ((_ ($kif kt kf))
     (make-$kif kt kf))
    ((_ ($kreceive req rest kargs))
     (make-$kreceive (make-$arity req '() rest '() #f) kargs))
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
    ((_ (k cont)) (make-$cont k (build-cont-body cont)))))

(define-syntax build-cps-exp
  (syntax-rules (unquote
                 $void $const $prim $fun $call $callk $primcall $values $prompt)
    ((_ (unquote exp)) exp)
    ((_ ($void)) (make-$void))
    ((_ ($const val)) (make-$const val))
    ((_ ($prim name)) (make-$prim name))
    ((_ ($fun src meta free body))
     (make-$fun src meta free (build-cps-cont body)))
    ((_ ($call proc (arg ...))) (make-$call proc (list arg ...)))
    ((_ ($call proc args)) (make-$call proc args))
    ((_ ($callk k proc (arg ...))) (make-$callk k proc (list arg ...)))
    ((_ ($callk k proc args)) (make-$callk k proc args))
    ((_ ($primcall name (arg ...))) (make-$primcall name (list arg ...)))
    ((_ ($primcall name args)) (make-$primcall name args))
    ((_ ($values (arg ...))) (make-$values (list arg ...)))
    ((_ ($values args)) (make-$values args))
    ((_ ($prompt escape? tag handler))
     (make-$prompt escape? tag handler))))

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
     (let-fresh (kconst) ()
       (build-cps-term
         ($letk ((kconst ($kargs (name) (sym) ($letconst (tail ...) body))))
           ($continue kconst (let ((props (source-properties val)))
                               (and (pair? props) props))
             ($const val))))))
    ((_ ($letrec names gensyms funs body))
     (make-$letrec names gensyms funs (build-cps-term body)))
    ((_ ($continue k src exp))
     (make-$continue k src (build-cps-exp exp)))))

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
       ($letk ((k ($kargs (name) (sym)
                    ,(parse-cps body))))
         ($continue k (src exp) ($const c)))))
    (('let k (name sym val) body)
     (build-cps-term
      ($letk ((k ($kargs (name) (sym)
                   ,(parse-cps body))))
        ,(parse-cps val))))
    (('letk (cont ...) body)
     (build-cps-term
       ($letk ,(map parse-cps cont) ,(parse-cps body))))
    (('k sym body)
     (build-cps-cont
       (sym ,(parse-cps body))))
    (('kif kt kf)
     (build-cont-body ($kif kt kf)))
    (('kreceive req rest k)
     (build-cont-body ($kreceive req rest k)))
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
     (build-cps-term ($continue k (src exp) ,(parse-cps exp))))
    (('void)
     (build-cps-exp ($void)))
    (('const exp)
     (build-cps-exp ($const exp)))
    (('prim name)
     (build-cps-exp ($prim name)))
    (('fun meta free body)
     (build-cps-exp ($fun (src exp) meta free ,(parse-cps body))))
    (('letrec ((name sym fun) ...) body)
     (build-cps-term
       ($letrec name sym (map parse-cps fun) ,(parse-cps body))))
    (('call proc arg ...)
     (build-cps-exp ($call proc arg)))
    (('callk k proc arg ...)
     (build-cps-exp ($callk k proc arg)))
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
    (($ $letk (($ $cont k ($ $kargs (name) (sym) body)))
        ($ $continue k src ($ $const c)))
     `(letconst ,k (,name ,sym ,c)
                ,(unparse-cps body)))
    (($ $letk (($ $cont k ($ $kargs (name) (sym) body))) val)
     `(let ,k (,name ,sym ,(unparse-cps val))
           ,(unparse-cps body)))
    (($ $letk conts body)
     `(letk ,(map unparse-cps conts) ,(unparse-cps body)))
    (($ $cont sym body)
     `(k ,sym ,(unparse-cps body)))
    (($ $kif kt kf)
     `(kif ,kt ,kf))
    (($ $kreceive ($ $arity req () rest '() #f) k)
     `(kreceive ,req ,rest ,k))
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
    (($ $continue k src exp)
     `(continue ,k ,(unparse-cps exp)))
    (($ $void)
     `(void))
    (($ $const val)
     `(const ,val))
    (($ $prim name)
     `(prim ,name))
    (($ $fun src meta free body)
     `(fun ,meta ,free ,(unparse-cps body)))
    (($ $letrec names syms funs body)
     `(letrec ,(map (lambda (name sym fun)
                      (list name sym (unparse-cps fun)))
                    names syms funs)
        ,(unparse-cps body)))
    (($ $call proc args)
     `(call ,proc ,@args))
    (($ $callk k proc args)
     `(callk ,k ,proc ,@args))
    (($ $primcall name args)
     `(primcall ,name ,@args))
    (($ $values args)
     `(values ,@args))
    (($ $prompt escape? tag handler)
     `(prompt ,escape? ,tag ,handler))
    (_
     (error "unexpected cps" exp))))

(define-syntax-rule (make-cont-folder global? seed ...)
  (lambda (proc fun seed ...)
    (define (fold-values proc in seed ...)
      (if (null? in)
          (values seed ...)
          (let-values (((seed ...) (proc (car in) seed ...)))
            (fold-values proc (cdr in) seed ...))))

    (define (cont-folder cont seed ...)
      (match cont
        (($ $cont k cont)
         (let-values (((seed ...) (proc k cont seed ...)))
           (match cont
             (($ $kargs names syms body)
              (term-folder body seed ...))

             (($ $kentry self tail clauses)
              (let-values (((seed ...) (cont-folder tail seed ...)))
                (fold-values cont-folder clauses seed ...)))

             (($ $kclause arity body)
              (cont-folder body seed ...))

             (_ (values seed ...)))))))

    (define (fun-folder fun seed ...)
      (match fun
        (($ $fun src meta free body)
         (cont-folder body seed ...))))

    (define (term-folder term seed ...)
      (match term
        (($ $letk conts body)
         (let-values (((seed ...) (term-folder body seed ...)))
           (fold-values cont-folder conts seed ...)))

        (($ $continue k src exp)
         (match exp
           (($ $fun)
            (if global?
                (fun-folder exp seed ...)
                (values seed ...)))
           (_ (values seed ...))))

        (($ $letrec names syms funs body)
         (let-values (((seed ...) (term-folder body seed ...)))
           (if global?
               (fold-values fun-folder funs seed ...)
               (values seed ...))))))

    (fun-folder fun seed ...)))

(define (compute-max-label-and-var fun)
  ((make-cont-folder #t max-label max-var)
   (lambda (label cont max-label max-var)
     (values (max label max-label)
             (match cont
               (($ $kargs names vars)
                (fold max max-var vars))
               (($ $kentry self)
                (max self max-var))
               (_ max-var))))
   fun
   -1
   -1))

(define (fold-conts proc seed fun)
  ((make-cont-folder #t seed) proc fun seed))

(define (fold-local-conts proc seed fun)
  ((make-cont-folder #f seed) proc fun seed))
