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
;;; $kargs for continuations that bind values, $ktail for the tail
;;; continuation, etc.
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
;;;   - $kfun labels an entry point for a $fun (a function), and
;;;     contains a $ktail representing the formal argument which is the
;;;     function's continuation.
;;;
;;;   - $kfun also contain a $kclause continuation, corresponding to
;;;     the first case-lambda clause of the function.  $kclause actually
;;;     contains the clause body, and the subsequent clause (if any).
;;;     This is because the $kclause logically matches or doesn't match
;;;     a given set of actual arguments against a formal arity, then
;;;     proceeds to a "body" continuation (which is a $kargs).
;;;
;;;     That's to say that a $fun can be matched like this:
;;;
;;;     (match f
;;;       (($ $fun free
;;;           ($ $cont kfun
;;;              ($ $kfun src meta self ($ $cont ktail ($ $ktail))
;;;                 ($ $kclause arity
;;;                    ($ $cont kbody ($ $kargs names syms body))
;;;                    alternate))))
;;;         #t))
;;;
;;;     A $continue to ktail is in tail position.  $kfun, $kclause,
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
;;;     $ktail, etc).
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
            $kreceive $kargs $kfun $ktail $kclause

            ;; Expressions.
            $void $const $prim $fun $closure $branch
            $call $callk $primcall $values $prompt

            ;; First-order CPS root.
            $program

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
            make-global-cont-folder make-local-cont-folder
            fold-conts fold-local-conts
            visit-cont-successors))

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
(define-cps-type $letrec names syms funs body) ; Higher-order.

;; Continuations
(define-cps-type $cont k cont)
(define-cps-type $kreceive arity k)
(define-cps-type $kargs names syms body)
(define-cps-type $kfun src meta self tail clause)
(define-cps-type $ktail)
(define-cps-type $kclause arity cont alternate)

;; Expressions.
(define-cps-type $void)
(define-cps-type $const val)
(define-cps-type $prim name)
(define-cps-type $fun free body) ; Higher-order.
(define-cps-type $closure label nfree) ; First-order.
(define-cps-type $branch k exp)
(define-cps-type $call proc args)
(define-cps-type $callk k proc args) ; First-order.
(define-cps-type $primcall name args)
(define-cps-type $values args)
(define-cps-type $prompt escape? tag handler)

;; The root of a higher-order CPS term is $cont containing a $kfun.  The
;; root of a first-order CPS term is a $program.
(define-cps-type $program funs)

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
  (call-with-values (lambda () (compute-max-label-and-var fun))
    (lambda (max-label max-var)
      (parameterize ((label-counter (1+ max-label))
                     (var-counter (1+ max-var)))
        body ...))))

(define-syntax build-arity
  (syntax-rules (unquote)
    ((_ (unquote exp)) exp)
    ((_ (req opt rest kw allow-other-keys?))
     (make-$arity req opt rest kw allow-other-keys?))))

(define-syntax build-cont-body
  (syntax-rules (unquote $kreceive $kargs $kfun $ktail $kclause)
    ((_ (unquote exp))
     exp)
    ((_ ($kreceive req rest kargs))
     (make-$kreceive (make-$arity req '() rest '() #f) kargs))
    ((_ ($kargs (name ...) (unquote syms) body))
     (make-$kargs (list name ...) syms (build-cps-term body)))
    ((_ ($kargs (name ...) (sym ...) body))
     (make-$kargs (list name ...) (list sym ...) (build-cps-term body)))
    ((_ ($kargs names syms body))
     (make-$kargs names syms (build-cps-term body)))
    ((_ ($kfun src meta self tail clause))
     (make-$kfun src meta self (build-cps-cont tail) (build-cps-cont clause)))
    ((_ ($ktail))
     (make-$ktail))
    ((_ ($kclause arity cont alternate))
     (make-$kclause (build-arity arity) (build-cps-cont cont)
                    (build-cps-cont alternate)))))

(define-syntax build-cps-cont
  (syntax-rules (unquote)
    ((_ (unquote exp)) exp)
    ((_ (k cont)) (make-$cont k (build-cont-body cont)))))

(define-syntax build-cps-exp
  (syntax-rules (unquote
                 $void $const $prim $fun $closure $branch
                 $call $callk $primcall $values $prompt)
    ((_ (unquote exp)) exp)
    ((_ ($void)) (make-$void))
    ((_ ($const val)) (make-$const val))
    ((_ ($prim name)) (make-$prim name))
    ((_ ($fun free body)) (make-$fun free (build-cps-cont body)))
    ((_ ($closure k nfree)) (make-$closure k nfree))
    ((_ ($call proc (unquote args))) (make-$call proc args))
    ((_ ($call proc (arg ...))) (make-$call proc (list arg ...)))
    ((_ ($call proc args)) (make-$call proc args))
    ((_ ($callk k proc (unquote args))) (make-$callk k proc args))
    ((_ ($callk k proc (arg ...))) (make-$callk k proc (list arg ...)))
    ((_ ($callk k proc args)) (make-$callk k proc args))
    ((_ ($primcall name (unquote args))) (make-$primcall name args))
    ((_ ($primcall name (arg ...))) (make-$primcall name (list arg ...)))
    ((_ ($primcall name args)) (make-$primcall name args))
    ((_ ($values (unquote args))) (make-$values args))
    ((_ ($values (arg ...))) (make-$values (list arg ...)))
    ((_ ($values args)) (make-$values args))
    ((_ ($branch k exp)) (make-$branch k (build-cps-exp exp)))
    ((_ ($prompt escape? tag handler))
     (make-$prompt escape? tag handler))))

(define-syntax build-cps-term
  (syntax-rules (unquote $letk $letk* $letconst $letrec $program $continue)
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
    ((_ ($program (unquote conts)))
     (make-$program conts))
    ((_ ($program (cont ...)))
     (make-$program (list (build-cps-cont cont) ...)))
    ((_ ($program conts))
     (make-$program conts))
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
    (('kreceive req rest k)
     (build-cont-body ($kreceive req rest k)))
    (('kargs names syms body)
     (build-cont-body ($kargs names syms ,(parse-cps body))))
    (('kfun src meta self tail clause)
     (build-cont-body
      ($kfun (src exp) meta self ,(parse-cps tail)
        ,(and=> clause parse-cps))))
    (('ktail)
     (build-cont-body
      ($ktail)))
    (('kclause (req opt rest kw allow-other-keys?) body)
     (build-cont-body
      ($kclause (req opt rest kw allow-other-keys?)
        ,(parse-cps body)
        ,#f)))
    (('kclause (req opt rest kw allow-other-keys?) body alternate)
     (build-cont-body
      ($kclause (req opt rest kw allow-other-keys?)
        ,(parse-cps body)
        ,(parse-cps alternate))))
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
    (('fun free body)
     (build-cps-exp ($fun free ,(parse-cps body))))
    (('closure k nfree)
     (build-cps-exp ($closure k nfree)))
    (('letrec ((name sym fun) ...) body)
     (build-cps-term
       ($letrec name sym (map parse-cps fun) ,(parse-cps body))))
    (('program (cont ...))
     (build-cps-term ($program ,(map parse-cps cont))))
    (('call proc arg ...)
     (build-cps-exp ($call proc arg)))
    (('callk k proc arg ...)
     (build-cps-exp ($callk k proc arg)))
    (('primcall name arg ...)
     (build-cps-exp ($primcall name arg)))
    (('branch k exp)
     (build-cps-exp ($branch k ,(parse-cps exp))))
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
    (($ $kreceive ($ $arity req () rest '() #f) k)
     `(kreceive ,req ,rest ,k))
    (($ $kargs () () body)
     `(kseq ,(unparse-cps body)))
    (($ $kargs names syms body)
     `(kargs ,names ,syms ,(unparse-cps body)))
    (($ $kfun src meta self tail clause)
     `(kfun ,meta ,self ,(unparse-cps tail) ,(unparse-cps clause)))
    (($ $ktail)
     `(ktail))
    (($ $kclause ($ $arity req opt rest kw allow-other-keys?) body alternate)
     `(kclause (,req ,opt ,rest ,kw ,allow-other-keys?) ,(unparse-cps body)
               . ,(if alternate (list (unparse-cps alternate)) '())))

    ;; Calls.
    (($ $continue k src exp)
     `(continue ,k ,(unparse-cps exp)))
    (($ $void)
     `(void))
    (($ $const val)
     `(const ,val))
    (($ $prim name)
     `(prim ,name))
    (($ $fun free body)
     `(fun ,free ,(unparse-cps body)))
    (($ $closure k nfree)
     `(closure ,k ,nfree))
    (($ $letrec names syms funs body)
     `(letrec ,(map (lambda (name sym fun)
                      (list name sym (unparse-cps fun)))
                    names syms funs)
        ,(unparse-cps body)))
    (($ $program conts)
     `(program ,(map unparse-cps conts)))
    (($ $call proc args)
     `(call ,proc ,@args))
    (($ $callk k proc args)
     `(callk ,k ,proc ,@args))
    (($ $primcall name args)
     `(primcall ,name ,@args))
    (($ $branch k exp)
     `(branch ,k ,(unparse-cps exp)))
    (($ $values args)
     `(values ,@args))
    (($ $prompt escape? tag handler)
     `(prompt ,escape? ,tag ,handler))
    (_
     (error "unexpected cps" exp))))

(define-syntax-rule (make-global-cont-folder seed ...)
  (lambda (proc cont seed ...)
    (define (cont-folder cont seed ...)
      (match cont
        (($ $cont k cont)
         (let-values (((seed ...) (proc k cont seed ...)))
           (match cont
             (($ $kargs names syms body)
              (term-folder body seed ...))

             (($ $kfun src meta self tail clause)
              (let-values (((seed ...) (cont-folder tail seed ...)))
                (if clause
                    (cont-folder clause seed ...)
                    (values seed ...))))

             (($ $kclause arity body alternate)
              (let-values (((seed ...) (cont-folder body seed ...)))
                (if alternate
                    (cont-folder alternate seed ...)
                    (values seed ...))))

             (_ (values seed ...)))))))

    (define (fun-folder fun seed ...)
      (match fun
        (($ $fun free body)
         (cont-folder body seed ...))))

    (define (term-folder term seed ...)
      (match term
        (($ $letk conts body)
         (let-values (((seed ...) (term-folder body seed ...)))
           (let lp ((conts conts) (seed seed) ...)
             (if (null? conts)
                 (values seed ...)
                 (let-values (((seed ...) (cont-folder (car conts) seed ...)))
                   (lp (cdr conts) seed ...))))))

        (($ $continue k src exp)
         (match exp
           (($ $fun) (fun-folder exp seed ...))
           (_ (values seed ...))))

        (($ $letrec names syms funs body)
         (let-values (((seed ...) (term-folder body seed ...)))
           (let lp ((funs funs) (seed seed) ...)
             (if (null? funs)
                 (values seed ...)
                 (let-values (((seed ...) (fun-folder (car funs) seed ...)))
                   (lp (cdr funs) seed ...))))))))

    (cont-folder cont seed ...)))

(define-syntax-rule (make-local-cont-folder seed ...)
  (lambda (proc cont seed ...)
    (define (cont-folder cont seed ...)
      (match cont
        (($ $cont k (and cont ($ $kargs names syms body)))
         (let-values (((seed ...) (proc k cont seed ...)))
           (term-folder body seed ...)))
        (($ $cont k cont)
         (proc k cont seed ...))))
    (define (term-folder term seed ...)
      (match term
        (($ $letk conts body)
         (let-values (((seed ...) (term-folder body seed ...)))
           (let lp ((conts conts) (seed seed) ...)
             (match conts
               (() (values seed ...))
               ((cont) (cont-folder cont seed ...))
               ((cont . conts)
                (let-values (((seed ...) (cont-folder cont seed ...)))
                  (lp conts seed ...)))))))
        (($ $letrec names syms funs body) (term-folder body seed ...))
        (_ (values seed ...))))
    (define (clause-folder clause seed ...)
      (match clause
        (($ $cont k (and cont ($ $kclause arity body alternate)))
         (let-values (((seed ...) (proc k cont seed ...)))
           (if alternate
               (let-values (((seed ...) (cont-folder body seed ...)))
                 (clause-folder alternate seed ...))
               (cont-folder body seed ...))))))
    (match cont
      (($ $cont k (and cont ($ $kfun src meta self tail clause)))
       (let*-values (((seed ...) (proc k cont seed ...))
                     ((seed ...) (if clause
                                     (clause-folder clause seed ...)
                                     (values seed ...))))
         (cont-folder tail seed ...))))))

(define (compute-max-label-and-var fun)
  (match fun
    (($ $cont)
     ((make-global-cont-folder max-label max-var)
      (lambda (label cont max-label max-var)
        (values (max label max-label)
                (match cont
                  (($ $kargs names vars body)
                   (let lp ((body body) (max-var (fold max max-var vars)))
                     (match body
                       (($ $letk conts body) (lp body max-var))
                       (($ $letrec names vars funs body)
                        (lp body (fold max max-var vars)))
                       (_ max-var))))
                  (($ $kfun src meta self)
                   (max self max-var))
                  (_ max-var))))
      fun -1 -1))
    (($ $program conts)
     (define (fold/2 proc in s0 s1)
      (if (null? in)
          (values s0 s1)
          (let-values (((s0 s1) (proc (car in) s0 s1)))
            (fold/2 proc (cdr in) s0 s1))))
     (let lp ((conts conts) (max-label -1) (max-var -1))
       (if (null? conts)
           (values max-label max-var)
           (call-with-values (lambda ()
                               ((make-local-cont-folder max-label max-var)
                                (lambda (label cont max-label max-var)
                                  (values (max label max-label)
                                          (match cont
                                            (($ $kargs names vars body)
                                             (fold max max-var vars))
                                            (($ $kfun src meta self)
                                             (max self max-var))
                                            (_ max-var))))
                                (car conts) max-label max-var))
             (lambda (max-label max-var)
               (lp (cdr conts) max-label max-var))))))))

(define (fold-conts proc seed fun)
  ((make-global-cont-folder seed) proc fun seed))

(define (fold-local-conts proc seed fun)
  ((make-local-cont-folder seed) proc fun seed))

(define (visit-cont-successors proc cont)
  (match cont
    (($ $kargs names syms body)
     (let lp ((body body))
       (match body
         (($ $letk conts body) (lp body))
         (($ $letrec names vars funs body) (lp body))
         (($ $continue k src exp)
          (match exp
            (($ $prompt escape? tag handler) (proc k handler))
            (($ $branch kt) (proc k kt))
            (_ (proc k)))))))

    (($ $kreceive arity k) (proc k))

    (($ $kclause arity ($ $cont kbody) #f) (proc kbody))

    (($ $kclause arity ($ $cont kbody) ($ $cont kalt)) (proc kbody kalt))

    (($ $kfun src meta self tail ($ $cont clause)) (proc clause))

    (($ $kfun src meta self tail #f) (proc))

    (($ $ktail) (proc))))
