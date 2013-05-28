;;;; 	Copyright (C) 2009, 2010, 2011, 2012, 2013 Free Software Foundation, Inc.
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


(define-module (language tree-il)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (system base syntax)
  #:export (tree-il-src

            <void> void? make-void void-src
            <const> const? make-const const-src const-exp
            <primitive-ref> primitive-ref? make-primitive-ref primitive-ref-src primitive-ref-name
            <lexical-ref> lexical-ref? make-lexical-ref lexical-ref-src lexical-ref-name lexical-ref-gensym
            <lexical-set> lexical-set? make-lexical-set lexical-set-src lexical-set-name lexical-set-gensym lexical-set-exp
            <module-ref> module-ref? make-module-ref module-ref-src module-ref-mod module-ref-name module-ref-public?
            <module-set> module-set? make-module-set module-set-src module-set-mod module-set-name module-set-public? module-set-exp
            <toplevel-ref> toplevel-ref? make-toplevel-ref toplevel-ref-src toplevel-ref-name
            <toplevel-set> toplevel-set? make-toplevel-set toplevel-set-src toplevel-set-name toplevel-set-exp
            <toplevel-define> toplevel-define? make-toplevel-define toplevel-define-src toplevel-define-name toplevel-define-exp
            <conditional> conditional? make-conditional conditional-src conditional-test conditional-consequent conditional-alternate
            <call> call? make-call call-src call-proc call-args
            <primcall> primcall? make-primcall primcall-src primcall-name primcall-args
            <seq> seq? make-seq seq-src seq-head seq-tail
            <lambda> lambda? make-lambda lambda-src lambda-meta lambda-body
            <lambda-case> lambda-case? make-lambda-case lambda-case-src
                          lambda-case-req lambda-case-opt lambda-case-rest lambda-case-kw
                          lambda-case-inits lambda-case-gensyms
                          lambda-case-body lambda-case-alternate
            <let> let? make-let let-src let-names let-gensyms let-vals let-body
            <letrec> letrec? make-letrec letrec-src letrec-in-order? letrec-names letrec-gensyms letrec-vals letrec-body
            <fix> fix? make-fix fix-src fix-names fix-gensyms fix-vals fix-body
            <let-values> let-values? make-let-values let-values-src let-values-exp let-values-body
            <dynwind> dynwind? make-dynwind dynwind-src dynwind-winder dynwind-pre dynwind-body dynwind-post dynwind-unwinder
            <dynlet> dynlet? make-dynlet dynlet-src dynlet-fluids dynlet-vals dynlet-body
            <dynref> dynref? make-dynref dynref-src dynref-fluid
            <dynset> dynset? make-dynset dynset-src dynset-fluid dynset-exp
            <prompt> prompt? make-prompt prompt-src prompt-tag prompt-body prompt-handler
            <abort> abort? make-abort abort-src abort-tag abort-args abort-tail

            list->seq

            parse-tree-il
            unparse-tree-il
            tree-il->scheme

            tree-il-fold
            make-tree-il-folder
            post-order
            pre-order

            tree-il=?
            tree-il-hash))

(define (print-tree-il exp port)
  (format port "#<tree-il ~S>" (unparse-tree-il exp)))

(define-syntax borrow-core-vtables
  (lambda (x)
    (syntax-case x ()
      ((_)
       (let lp ((n 0) (out '()))
         (if (< n (vector-length %expanded-vtables))
             (lp (1+ n)
                 (let* ((vtable (vector-ref %expanded-vtables n))
                        (stem (struct-ref vtable (+ vtable-offset-user 0)))
                        (fields (struct-ref vtable (+ vtable-offset-user 2)))
                        (sfields (map
                                  (lambda (f) (datum->syntax x f))
                                  fields))
                        (type (datum->syntax x (symbol-append '< stem '>)))
                        (ctor (datum->syntax x (symbol-append 'make- stem)))
                        (pred (datum->syntax x (symbol-append stem '?))))
                   (let lp ((n 0) (fields fields)
                            (out (cons*
                                  #`(define (#,ctor #,@sfields)
                                      (make-struct #,type 0 #,@sfields))
                                  #`(define (#,pred x)
                                      (and (struct? x)
                                           (eq? (struct-vtable x) #,type)))
                                  #`(struct-set! #,type vtable-index-printer
                                                 print-tree-il)
                                  #`(define #,type
                                           (vector-ref %expanded-vtables #,n))
                                       out)))
                     (if (null? fields)
                         out
                         (lp (1+ n)
                             (cdr fields)
                             (let ((acc (datum->syntax
                                         x (symbol-append stem '- (car fields)))))
                               (cons #`(define #,acc
                                         (make-procedure-with-setter
                                          (lambda (x) (struct-ref x #,n))
                                          (lambda (x v) (struct-set! x #,n v))))
                                     out)))))))
             #`(begin #,@(reverse out))))))))

(borrow-core-vtables)

  ;; (<void>)
  ;; (<const> exp)
  ;; (<primitive-ref> name)
  ;; (<lexical-ref> name gensym)
  ;; (<lexical-set> name gensym exp)
  ;; (<module-ref> mod name public?)
  ;; (<module-set> mod name public? exp)
  ;; (<toplevel-ref> name)
  ;; (<toplevel-set> name exp)
  ;; (<toplevel-define> name exp)
  ;; (<conditional> test consequent alternate)
  ;; (<call> proc args)
  ;; (<primcall> name args)
  ;; (<seq> head tail)
  ;; (<lambda> meta body)
  ;; (<lambda-case> req opt rest kw inits gensyms body alternate)
  ;; (<let> names gensyms vals body)
  ;; (<letrec> in-order? names gensyms vals body)
  ;; (<dynlet> fluids vals body)

(define-type (<tree-il> #:common-slots (src) #:printer print-tree-il)
  (<fix> names gensyms vals body)
  (<let-values> exp body)
  (<dynwind> winder pre body post unwinder)
  (<dynref> fluid)
  (<dynset> fluid exp)
  (<prompt> tag body handler)
  (<abort> tag args tail))



;; A helper.
(define (list->seq loc exps)
  (if (null? (cdr exps))
      (car exps)
      (make-seq loc (car exps) (list->seq #f (cdr exps)))))



(define (location x)
  (and (pair? x)
       (let ((props (source-properties x)))
	 (and (pair? props) props))))

(define (parse-tree-il exp)
  (let ((loc (location exp))
        (retrans (lambda (x) (parse-tree-il x))))
    (match exp
     (('void)
      (make-void loc))

     (('call proc . args)
      (make-call loc (retrans proc) (map retrans args)))

     (('primcall name . args)
      (make-primcall loc name (map retrans args)))

     (('if test consequent alternate)
      (make-conditional loc (retrans test) (retrans consequent) (retrans alternate)))

     (('primitive (and name (? symbol?)))
      (make-primitive-ref loc name))

     (('lexical (and name (? symbol?)))
      (make-lexical-ref loc name name))

     (('lexical (and name (? symbol?)) (and sym (? symbol?)))
      (make-lexical-ref loc name sym))

     (('set! ('lexical (and name (? symbol?))) exp)
      (make-lexical-set loc name name (retrans exp)))

     (('set! ('lexical (and name (? symbol?)) (and sym (? symbol?))) exp)
      (make-lexical-set loc name sym (retrans exp)))

     (('@ ((and mod (? symbol?)) ...) (and name (? symbol?)))
      (make-module-ref loc mod name #t))

     (('set! ('@ ((and mod (? symbol?)) ...) (and name (? symbol?))) exp)
      (make-module-set loc mod name #t (retrans exp)))

     (('@@ ((and mod (? symbol?)) ...) (and name (? symbol?)))
      (make-module-ref loc mod name #f))

     (('set! ('@@ ((and mod (? symbol?)) ...) (and name (? symbol?))) exp)
      (make-module-set loc mod name #f (retrans exp)))

     (('toplevel (and name (? symbol?)))
      (make-toplevel-ref loc name))

     (('set! ('toplevel (and name (? symbol?))) exp)
      (make-toplevel-set loc name (retrans exp)))

     (('define (and name (? symbol?)) exp)
      (make-toplevel-define loc name (retrans exp)))

     (('lambda meta body)
      (make-lambda loc meta (retrans body)))

     (('lambda-case ((req opt rest kw inits gensyms) body) alternate)
      (make-lambda-case loc req opt rest kw
                        (map retrans inits) gensyms
                        (retrans body)
                        (and=> alternate retrans)))

     (('lambda-case ((req opt rest kw inits gensyms) body))
      (make-lambda-case loc req opt rest kw
                        (map retrans inits) gensyms
                        (retrans body)
                        #f))

     (('const exp)
      (make-const loc exp))

     (('seq head tail)
      (make-seq loc (retrans head) (retrans tail)))

     ;; Convenience.
     (('begin . exps)
      (list->seq loc (map retrans exps)))

     (('let names gensyms vals body)
      (make-let loc names gensyms (map retrans vals) (retrans body)))

     (('letrec names gensyms vals body)
      (make-letrec loc #f names gensyms (map retrans vals) (retrans body)))

     (('letrec* names gensyms vals body)
      (make-letrec loc #t names gensyms (map retrans vals) (retrans body)))

     (('fix names gensyms vals body)
      (make-fix loc names gensyms (map retrans vals) (retrans body)))

     (('let-values exp body)
      (make-let-values loc (retrans exp) (retrans body)))

     (('dynwind winder pre body post unwinder)
      (make-dynwind loc (retrans winder) (retrans pre)
                    (retrans body)
                    (retrans post) (retrans unwinder)))

     (('dynlet fluids vals body)
      (make-dynlet loc (map retrans fluids) (map retrans vals) (retrans body)))

     (('dynref fluid)
      (make-dynref loc (retrans fluid)))

     (('dynset fluid exp)
      (make-dynset loc (retrans fluid) (retrans exp)))

     (('prompt tag body handler)
      (make-prompt loc (retrans tag) (retrans body) (retrans handler)))
     
     (('abort tag args tail)
      (make-abort loc (retrans tag) (map retrans args) (retrans tail)))

     (else
      (error "unrecognized tree-il" exp)))))

(define (unparse-tree-il tree-il)
  (match tree-il
    (($ <void> src)
     '(void))

    (($ <call> src proc args)
     `(call ,(unparse-tree-il proc) ,@(map unparse-tree-il args)))

    (($ <primcall> src name args)
     `(primcall ,name ,@(map unparse-tree-il args)))

    (($ <conditional> src test consequent alternate)
     `(if ,(unparse-tree-il test)
          ,(unparse-tree-il consequent)
          ,(unparse-tree-il alternate)))

    (($ <primitive-ref> src name)
     `(primitive ,name))

    (($ <lexical-ref> src name gensym)
     `(lexical ,name ,gensym))

    (($ <lexical-set> src name gensym exp)
     `(set! (lexical ,name ,gensym) ,(unparse-tree-il exp)))

    (($ <module-ref> src mod name public?)
     `(,(if public? '@ '@@) ,mod ,name))

    (($ <module-set> src mod name public? exp)
     `(set! (,(if public? '@ '@@) ,mod ,name) ,(unparse-tree-il exp)))

    (($ <toplevel-ref> src name)
     `(toplevel ,name))

    (($ <toplevel-set> src name exp)
     `(set! (toplevel ,name) ,(unparse-tree-il exp)))

    (($ <toplevel-define> src name exp)
     `(define ,name ,(unparse-tree-il exp)))

    (($ <lambda> src meta body)
     (if body
         `(lambda ,meta ,(unparse-tree-il body))
         `(lambda ,meta (lambda-case))))

    (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
     `(lambda-case ((,req ,opt ,rest ,kw ,(map unparse-tree-il inits) ,gensyms)
                    ,(unparse-tree-il body))
                   . ,(if alternate (list (unparse-tree-il alternate)) '())))

    (($ <const> src exp)
     `(const ,exp))

    (($ <seq> src head tail)
     `(seq ,(unparse-tree-il head) ,(unparse-tree-il tail)))
    
    (($ <let> src names gensyms vals body)
     `(let ,names ,gensyms ,(map unparse-tree-il vals) ,(unparse-tree-il body)))

    (($ <letrec> src in-order? names gensyms vals body)
     `(,(if in-order? 'letrec* 'letrec) ,names ,gensyms
       ,(map unparse-tree-il vals) ,(unparse-tree-il body)))

    (($ <fix> src names gensyms vals body)
     `(fix ,names ,gensyms ,(map unparse-tree-il vals) ,(unparse-tree-il body)))

    (($ <let-values> src exp body)
     `(let-values ,(unparse-tree-il exp) ,(unparse-tree-il body)))

    (($ <dynwind> src winder pre body post unwinder)
     `(dynwind ,(unparse-tree-il winder) ,(unparse-tree-il pre)
               ,(unparse-tree-il body)
               ,(unparse-tree-il post) ,(unparse-tree-il unwinder)))

    (($ <dynlet> src fluids vals body)
     `(dynlet ,(map unparse-tree-il fluids) ,(map unparse-tree-il vals)
              ,(unparse-tree-il body)))

    (($ <dynref> src fluid)
     `(dynref ,(unparse-tree-il fluid)))

    (($ <dynset> src fluid exp)
     `(dynset ,(unparse-tree-il fluid) ,(unparse-tree-il exp)))

    (($ <prompt> src tag body handler)
     `(prompt ,(unparse-tree-il tag)
              ,(unparse-tree-il body)
              ,(unparse-tree-il handler)))

    (($ <abort> src tag args tail)
     `(abort ,(unparse-tree-il tag) ,(map unparse-tree-il args)
             ,(unparse-tree-il tail)))))

(define* (tree-il->scheme e #:optional (env #f) (opts '()))
  (values ((@ (language scheme decompile-tree-il)
              decompile-tree-il)
           e env opts)))


(define-syntax-rule (make-tree-il-folder seed ...)
  (lambda (tree down up seed ...)
    (define (fold-values proc exps seed ...)
      (if (null? exps)
          (values seed ...)
          (let-values (((seed ...) (proc (car exps) seed ...)))
            (fold-values proc (cdr exps) seed ...))))
    (let foldts ((tree tree) (seed seed) ...)
      (let*-values
          (((seed ...) (down tree seed ...))
           ((seed ...)
            (match tree
              (($ <lexical-set> src name gensym exp)
               (foldts exp seed ...))
              (($ <module-set> src mod name public? exp)
               (foldts exp seed ...))
              (($ <toplevel-set> src name exp)
               (foldts exp seed ...))
              (($ <toplevel-define> src name exp)
               (foldts exp seed ...))
              (($ <conditional> src test consequent alternate)
               (let*-values (((seed ...) (foldts test seed ...))
                             ((seed ...) (foldts consequent seed ...)))
                 (foldts alternate seed ...)))
              (($ <call> src proc args)
               (let-values (((seed ...) (foldts proc seed ...)))
                 (fold-values foldts args seed ...)))
              (($ <primcall> src name args)
               (fold-values foldts args seed ...))
              (($ <seq> src head tail)
               (let-values (((seed ...) (foldts head seed ...)))
                 (foldts tail seed ...)))
              (($ <lambda> src meta body)
               (if body
                   (foldts body seed ...)
                   (values seed ...)))
              (($ <lambda-case> src req opt rest kw inits gensyms body
                              alternate)
               (let-values (((seed ...) (fold-values foldts inits seed ...)))
                 (if alternate
                     (let-values (((seed ...) (foldts body seed ...)))
                       (foldts alternate seed ...))
                     (foldts body seed ...))))
              (($ <let> src names gensyms vals body)
               (let*-values (((seed ...) (fold-values foldts vals seed ...)))
                 (foldts body seed ...)))
              (($ <letrec> src in-order? names gensyms vals body)
               (let*-values (((seed ...) (fold-values foldts vals seed ...)))
                 (foldts body seed ...)))
              (($ <fix> src names gensyms vals body)
               (let*-values (((seed ...) (fold-values foldts vals seed ...)))
                 (foldts body seed ...)))
              (($ <let-values> src exp body)
               (let*-values (((seed ...) (foldts exp seed ...)))
                 (foldts body seed ...)))
              (($ <dynwind> src winder pre body post unwinder)
               (let*-values (((seed ...) (foldts winder seed ...))
                             ((seed ...) (foldts pre seed ...))
                             ((seed ...) (foldts body seed ...))
                             ((seed ...) (foldts post seed ...)))
                 (foldts unwinder seed ...)))
              (($ <dynlet> src fluids vals body)
               (let*-values (((seed ...) (fold-values foldts fluids seed ...))
                             ((seed ...) (fold-values foldts vals seed ...)))
                 (foldts body seed ...)))
              (($ <dynref> src fluid)
               (foldts fluid seed ...))
              (($ <dynset> src fluid exp)
               (let*-values (((seed ...) (foldts fluid seed ...)))
                 (foldts exp seed ...)))
              (($ <prompt> src tag body handler)
               (let*-values (((seed ...) (foldts tag seed ...))
                             ((seed ...) (foldts body seed ...)))
                 (foldts handler seed ...)))
              (($ <abort> src tag args tail)
               (let*-values (((seed ...) (foldts tag seed ...))
                             ((seed ...) (fold-values foldts args seed ...)))
                 (foldts tail seed ...)))
              (_
               (values seed ...)))))
        (up tree seed ...)))))

(define (tree-il-fold down up seed tree)
  "Traverse TREE, calling DOWN before visiting a sub-tree, and UP when
after visiting it.  Each of these procedures is invoked as `(PROC TREE
SEED)', where TREE is the sub-tree considered and SEED is the current
result, intially seeded with SEED.

This is an implementation of `foldts' as described by Andy Wingo in
``Applications of fold to XML transformation''."
  ;; Multi-valued fold naturally puts the seeds at the end, whereas
  ;; normal fold puts the traversable at the end.  Adapt to the expected
  ;; argument order.
  ((make-tree-il-folder tree) tree down up seed))

(define (pre-post-order pre post x)
  (let lp ((x x))
    (post
     (match (pre x)
       (($ <void> src)
        (make-void src))

       (($ <const> src exp)
        (make-const src exp))

       (($ <primitive-ref> src name)
        (make-primitive-ref src name))

       (($ <lexical-ref> src name gensym)
        (make-lexical-ref src name gensym))

       (($ <lexical-set> src name gensym exp)
        (make-lexical-set src name gensym (lp exp)))

       (($ <module-ref> src mod name public?)
        (make-module-ref src mod name public?))

       (($ <module-set> src mod name public? exp)
        (make-module-set src mod name public? (lp exp)))

       (($ <toplevel-ref> src name)
        (make-toplevel-ref src name))

       (($ <toplevel-set> src name exp)
        (make-toplevel-set src name (lp exp)))

       (($ <toplevel-define> src name exp)
        (make-toplevel-define src name (lp exp)))

       (($ <conditional> src test consequent alternate)
        (make-conditional src (lp test) (lp consequent) (lp alternate)))

       (($ <call> src proc args)
        (make-call src (lp proc) (map lp args)))

       (($ <primcall> src name args)
        (make-primcall src name (map lp args)))

       (($ <seq> src head tail)
        (make-seq src (lp head) (lp tail)))
      
       (($ <lambda> src meta body)
        (make-lambda src meta (and body (lp body))))

       (($ <lambda-case> src req opt rest kw inits gensyms body alternate)
        (make-lambda-case src req opt rest kw (map lp inits) gensyms (lp body)
                          (and alternate (lp alternate))))

       (($ <let> src names gensyms vals body)
        (make-let src names gensyms (map lp vals) (lp body)))

       (($ <letrec> src in-order? names gensyms vals body)
        (make-letrec src in-order? names gensyms (map lp vals) (lp body)))

       (($ <fix> src names gensyms vals body)
        (make-fix src names gensyms (map lp vals) (lp body)))

       (($ <let-values> src exp body)
        (make-let-values src (lp exp) (lp body)))

       (($ <dynwind> src winder pre body post unwinder)
        (make-dynwind src
                      (lp winder) (lp pre) (lp body) (lp post) (lp unwinder)))

       (($ <dynlet> src fluids vals body)
        (make-dynlet src (map lp fluids) (map lp vals) (lp body)))

       (($ <dynref> src fluid)
        (make-dynref src (lp fluid)))

       (($ <dynset> src fluid exp)
        (make-dynset src (lp fluid) (lp exp)))

       (($ <prompt> src tag body handler)
        (make-prompt src (lp tag) (lp body) (lp handler)))

       (($ <abort> src tag args tail)
        (make-abort src (lp tag) (map lp args) (lp tail)))))))

(define (post-order f x)
  (pre-post-order (lambda (x) x) f x))

(define (pre-order f x)
  (pre-post-order f (lambda (x) x) x))

;; FIXME: We should have a better primitive than this.
(define (struct-nfields x)
  (/ (string-length (symbol->string (struct-layout x))) 2))

(define (tree-il=? a b)
  (cond
   ((struct? a)
    (and (struct? b)
         (eq? (struct-vtable a) (struct-vtable b))
         ;; Assume that all structs are tree-il, so we skip over the
         ;; src slot.
         (let lp ((n (1- (struct-nfields a))))
           (or (zero? n)
               (and (tree-il=? (struct-ref a n) (struct-ref b n))
                    (lp (1- n)))))))
   ((pair? a)
    (and (pair? b)
         (tree-il=? (car a) (car b))
         (tree-il=? (cdr a) (cdr b))))
   (else
    (equal? a b))))

(define-syntax hash-bits
  (make-variable-transformer
   (lambda (x)
     (syntax-case x ()
       (var
        (identifier? #'var)
        (logcount most-positive-fixnum))))))

(define (tree-il-hash exp)
  (let ((hash-depth 4)
        (hash-width 3))
    (define (hash-exp exp depth)
      (define (rotate x bits)
        (logior (ash x (- bits))
                (ash (logand x (1- (ash 1 bits))) (- hash-bits bits))))
      (define (mix h1 h2)
        (logxor h1 (rotate h2 8)))
      (define (hash-struct s)
        (let ((len (struct-nfields s))
              (h (hashq (struct-vtable s) most-positive-fixnum)))
          (if (zero? depth)
              h
              (let lp ((i (max (- len hash-width) 1)) (h h))
                (if (< i len)
                    (lp (1+ i) (mix (hash-exp (struct-ref s i) (1+ depth)) h))
                    h)))))
      (define (hash-list l)
        (let ((h (hashq 'list most-positive-fixnum)))
          (if (zero? depth)
              h
              (let lp ((l l) (width 0) (h h))
                (if (< width hash-width)
                    (lp (cdr l) (1+ width)
                        (mix (hash-exp (car l) (1+ depth)) h))
                    h)))))
      (cond
       ((struct? exp) (hash-struct exp))
       ((list? exp) (hash-list exp))
       (else (hash exp most-positive-fixnum))))

    (hash-exp exp 0)))
