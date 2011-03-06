;;;; codegen.scm --- code generation for composable parsers
;;;;
;;;; 	Copyright (C) 2011 Free Software Foundation, Inc.
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

(define-module (ice-9 peg codegen)
  #:export (peg-sexp-compile)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 peg string-peg)
  #:use-module (ice-9 pretty-print)
  #:use-module (system base pmatch))

(define-syntax single?
  (syntax-rules ()
    "Return #t if X is a list of one element."
    ((_ x)
     (pmatch x
       ((_) #t)
       (else #f)))))

(define-syntax single-filter
  (syntax-rules ()
    "If EXP is a list of one element, return the element.  Otherwise
return EXP."
    ((_ exp)
     (pmatch exp
       ((,elt) elt)
       (,elts elts)))))

(define-syntax push-not-null!
  (syntax-rules ()
    "If OBJ is non-null, push it onto LST, otherwise do nothing."
    ((_ lst obj)
     (if (not (null? obj))
         (push! lst obj)))))

(define-syntax push!
  (syntax-rules ()
    "Push an object onto a list."
    ((_ lst obj)
     (set! lst (cons obj lst)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; CODE GENERATORS
;; These functions generate scheme code for parsing PEGs.
;; Conventions:
;;   accum: (all name body none)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code we generate will have a certain return structure depending on how we're
;; accumulating (the ACCUM variable).
(define (cg-generic-ret accum name body-uneval at)
  ;; name, body-uneval and at are syntax
  #`(let ((body #,body-uneval))
     #,(cond
        ((and (eq? accum 'all) name)
         #`(list #,at
                 (cond
                  ((not (list? body)) (list '#,name body))
                  ((null? body) '#,name)
                  ((symbol? (car body)) (list '#,name body))
                  (else (cons '#,name body)))))
        ((eq? accum 'name)
         #`(list #,at '#,name))
        ((eq? accum 'body)
         #`(list #,at
                 (cond
                  ((single? body) (car body))
                  (else body))))
        ((eq? accum 'none)
         #`(list #,at '()))
        (else
         (begin
           (pretty-print `(cg-generic-ret-error ,accum ,name ,body-uneval ,at))
           (pretty-print "Defaulting to accum of none.\n")
           #`(list #,at '()))))))

;; The short name makes the formatting below much easier to read.
(define cggr cg-generic-ret)

;; Generates code that matches a particular string.
;; E.g.: (cg-string syntax "abc" 'body)
(define (cg-string pat accum)
  (let ((plen (string-length pat)))
    #`(lambda (str len pos)
        (let ((end (+ pos #,plen)))
          (and (<= end len)
               (string= str #,pat pos end)
               #,(case accum
                   ((all) #`(list end (list 'cg-string #,pat)))
                   ((name) #`(list end 'cg-string))
                   ((body) #`(list end #,pat))
                   ((none) #`(list end '()))
                   (else (error "bad accum" accum))))))))

;; Generates code for matching any character.
;; E.g.: (cg-peg-any syntax 'body)
(define (cg-peg-any accum)
  #`(lambda (str len pos)
      (and (< pos len)
           #,(case accum
               ((all) #`(list (1+ pos)
                              (list 'cg-peg-any (substring str pos (1+ pos)))))
               ((name) #`(list (1+ pos) 'cg-peg-any))
               ((body) #`(list (1+ pos) (substring str pos (1+ pos))))
               ((none) #`(list (1+ pos) '()))
               (else (error "bad accum" accum))))))

;; Generates code for matching a range of characters between start and end.
;; E.g.: (cg-range syntax #\a #\z 'body)
(define (cg-range start end accum)
  #`(lambda (str len pos)
      (and (< pos len)
           (let ((c (string-ref str pos)))
             (and (char>=? c #,start)
                  (char<=? c #,end)
                  #,(case accum
                      ((all) #`(list (1+ pos) (list 'cg-range (string c))))
                      ((name) #`(list (1+ pos) 'cg-range))
                      ((body) #`(list (1+ pos) (string c)))
                      ((none) #`(list (1+ pos) '()))
                      (else (error "bad accum" accum))))))))

;; Filters the accum argument to peg-sexp-compile for buildings like string
;; literals (since we don't want to tag them with their name if we're doing an
;; "all" accum).
(define (builtin-accum-filter accum)
  (cond
   ((eq? accum 'all) 'body)
   ((eq? accum 'name) 'name)
   ((eq? accum 'body) 'body)
   ((eq? accum 'none) 'none)))
(define baf builtin-accum-filter)

;; Takes an arbitrary expressions and accumulation variable, then parses it.
;; E.g.: (peg-sexp-compile syntax '(and "abc" (or "-" (range #\a #\z))) 'all)
(define (peg-sexp-compile pat accum)
  (syntax-case pat (peg-any range ignore capture peg and or body)
    (peg-any
     (cg-peg-any (baf accum)))
    (sym (identifier? #'sym) ;; nonterminal
     #'sym)
    (str (string? (syntax->datum #'str)) ;; literal string
     (cg-string (syntax->datum #'str) (baf accum)))
    ((range start end) ;; range of characters (e.g. [a-z])
     (and (char? (syntax->datum #'start)) (char? (syntax->datum #'end)))
     (cg-range (syntax->datum #'start) (syntax->datum #'end) (baf accum)))
    ((ignore pat) ;; match but don't parse
     (peg-sexp-compile #'pat 'none))
    ((capture pat) ;; parse
     (peg-sexp-compile #'pat 'body))
    ((peg pat)  ;; embedded PEG string
     (string? (syntax->datum #'pat))
     (peg-string-compile #'pat (baf accum)))
    ((and pat ...)
     (cg-and #'(pat ...) (baf accum)))
    ((or pat ...)
     (cg-or #'(pat ...) (baf accum)))
    ((body type pat num)
     (cg-body (baf accum) #'type #'pat #'num))))

;; Top-level function builder for AND.  Reduces to a call to CG-AND-INT.
(define (cg-and clauses accum)
  #`(lambda (str len pos)
      (let ((body '()))
        #,(cg-and-int clauses accum #'str #'len #'pos #'body))))

;; Internal function builder for AND (calls itself).
(define (cg-and-int clauses accum str strlen at body)
  (syntax-case clauses ()
    (()
     (cggr accum 'cg-and #`(reverse #,body) at))
    ((first rest ...)
     #`(let ((res (#,(peg-sexp-compile #'first accum) #,str #,strlen #,at)))
         (and res 
              ;; update AT and BODY then recurse
              (let ((newat (car res))
                    (newbody (cadr res)))
                (set! #,at newat)
                (push-not-null! #,body (single-filter newbody))
                #,(cg-and-int #'(rest ...) accum str strlen at body)))))))

;; Top-level function builder for OR.  Reduces to a call to CG-OR-INT.
(define (cg-or clauses accum)
  #`(lambda (str len pos)
      #,(cg-or-int clauses accum #'str #'len #'pos)))

;; Internal function builder for OR (calls itself).
(define (cg-or-int clauses accum str strlen at)
  (syntax-case clauses ()
    (()
     #f)
    ((first rest ...)
     #`(or (#,(peg-sexp-compile #'first accum) #,str #,strlen #,at)
           #,(cg-or-int #'(rest ...) accum str strlen at)))))

;; Returns a function that parses a BODY element.
(define (cg-body accum type pat num)
  #`(lambda (str strlen at)
      (let ((body '()))
        (let lp ((end at) (count 0))
          (let* ((match (#,(peg-sexp-compile pat accum) str strlen end))
                 (new-end (if match (car match) end))
                 (count (if (> new-end end) (1+ count) count)))
            (if (> new-end end)
                (push-not-null! body (single-filter (cadr match))))
            (if (and (> new-end end)
                     #,(syntax-case num (+ * ?)
                         (n (number? (syntax->datum #'n))
                            #'(< count n))
                         (+ #t)
                         (* #t)
                         (? #'(< count 1))))
                (lp new-end count)
                (let ((success #,(syntax-case num (+ * ?)
                                   (n (number? (syntax->datum #'n))
                                      #'(= count n))
                                   (+ #'(>= count 1))
                                   (* #t)
                                   (? #t))))
                  #,(syntax-case type (! & lit)
                      (!
                       #`(if success
                             #f
                             #,(cggr accum 'cg-body #''() #'at)))
                      (&
                       #`(and success
                              #,(cggr accum 'cg-body #''() #'at)))
                      (lit
                       #`(and success
                              #,(cggr accum 'cg-body #'(reverse body) #'new-end)))))))))))
