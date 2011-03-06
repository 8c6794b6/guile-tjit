;;;; string-peg.scm --- representing PEG grammars as strings
;;;;
;;;; 	Copyright (C) 2010, 2011 Free Software Foundation, Inc.
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

(define-module (ice-9 peg string-peg)
  #:export (peg-string-compile
            peg-as-peg
            define-grammar
            define-grammar-f
            define-nonterm
            peg-grammar)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 peg codegen))

;; The results of parsing using a nonterminal are cached.  Think of it like a
;; hash with no conflict resolution.  Process for deciding on the cache size
;; wasn't very scientific; just ran the benchmarks and stopped a little after
;; the point of diminishing returns on my box.
(define *cache-size* 512)

;; Gets the left-hand depth of a list.
(define (depth lst)
  (if (or (not (list? lst)) (null? lst))
      0
      (+ 1 (depth (car lst)))))

(eval-when (compile load eval)
(define (syntax-for-non-cache-case for-syntax matchf-syn accumsym s-syn)
;  (let ((matchf-syn (datum->syntax for-syntax matchf)))
   #`(lambda (str strlen at)
      (let ((res (#,matchf-syn str strlen at)))
        ;; Try to match the nonterminal.
        (if res
            ;; If we matched, do some post-processing to figure out
            ;; what data to propagate upward.
            (let ((at (car res))
                  (body (cadr res)))
              #,(cond
                 ((eq? accumsym 'name)
                  #`(list at '#,s-syn))
                 ((eq? accumsym 'all)
                  #`(list (car res)
                          (cond
                           ((not (list? body))
                            (list '#,s-syn body))
                           ((null? body) '#,s-syn)
                           ((symbol? (car body))
                            (list '#,s-syn body))
                           (else (cons '#,s-syn body)))))
                 ((eq? accumsym 'none) #`(list (car res) '()))
                 (else #`(begin res))))
            ;; If we didn't match, just return false.
            #f))))
)

;; Defines a new nonterminal symbol accumulating with ACCUM.
(define-syntax define-nonterm
  (lambda (x)
    (syntax-case x ()
      ((_ sym accum pat)
       (let ((matchf (peg-sexp-compile #'pat (syntax->datum #'accum)))
             (accumsym (syntax->datum #'accum))
             (c (datum->syntax x (gensym))));; the cache
         ;; CODE is the code to parse the string if the result isn't cached.
         (let ((syn (syntax-for-non-cache-case x matchf accumsym #'sym)))
           #`(begin
               (define #,c (make-vector *cache-size* #f));; the cache
               (define (sym str strlen at)
                 (let* ((vref (vector-ref #,c (modulo at *cache-size*))))
                   ;; Check to see whether the value is cached.
                   (if (and vref (eq? (car vref) str) (= (cadr vref) at))
                       (caddr vref);; If it is return it.
                       (let ((fres ;; Else calculate it and cache it.
                              (#,syn str strlen at)))
                         (vector-set! #,c (modulo at *cache-size*)
                                      (list str at fres))
                         fres)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parse string PEGs using sexp PEGs.
;; See the variable PEG-AS-PEG for an easier-to-read syntax.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Grammar for PEGs in PEG grammar.
(define peg-as-peg
"grammar <-- (nonterminal ('<--' / '<-' / '<') sp pattern)+
pattern <-- alternative (SLASH sp alternative)*
alternative <-- ([!&]? sp suffix)+
suffix <-- primary ([*+?] sp)*
primary <-- '(' sp pattern ')' sp / '.' sp / literal / charclass / nonterminal !'<'
literal <-- ['] (!['] .)* ['] sp
charclass <-- LB (!']' (CCrange / CCsingle))* RB sp
CCrange <-- . '-' .
CCsingle <-- .
nonterminal <-- [a-zA-Z0-9-]+ sp
sp < [ \t\n]*
SLASH < '/'
LB < '['
RB < ']'
")

(define-nonterm peg-grammar all
  (body lit (and peg-nonterminal (or "<--" "<-" "<") peg-sp peg-pattern) +))
(define-nonterm peg-pattern all
  (and peg-alternative
       (body lit (and (ignore "/") peg-sp peg-alternative) *)))
(define-nonterm peg-alternative all
  (body lit (and (body lit (or "!" "&") ?) peg-sp peg-suffix) +))
(define-nonterm peg-suffix all
  (and peg-primary (body lit (and (or "*" "+" "?") peg-sp) *)))
(define-nonterm peg-primary all
  (or (and "(" peg-sp peg-pattern ")" peg-sp)
      (and "." peg-sp)
      peg-literal
      peg-charclass
      (and peg-nonterminal (body ! "<" 1))))
(define-nonterm peg-literal all
  (and "'" (body lit (and (body ! "'" 1) peg-any) *) "'" peg-sp))
(define-nonterm peg-charclass all
  (and (ignore "[")
       (body lit (and (body ! "]" 1)
                      (or charclass-range charclass-single)) *)
       (ignore "]")
       peg-sp))
(define-nonterm charclass-range all (and peg-any "-" peg-any))
(define-nonterm charclass-single all peg-any)
(define-nonterm peg-nonterminal all
  (and (body lit (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9) "-") +) peg-sp))
(define-nonterm peg-sp none
  (body lit (or " " "\t" "\n") *))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; PARSE STRING PEGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pakes a string representing a PEG grammar and defines all the nonterminals in
;; it as the associated PEGs.
(define (peg-parser str for-syntax)
  (let ((parsed (peg-parse peg-grammar str)))
    (if (not parsed)
        (begin
          ;; (display "Invalid PEG grammar!\n")
          #f)
        (let ((lst (peg:tree parsed)))
          (cond
           ((or (not (list? lst)) (null? lst))
            lst)
           ((eq? (car lst) 'peg-grammar)
            #`(begin
                #,@(map (lambda (x) (peg-nonterm->defn x for-syntax))
                        (context-flatten (lambda (lst) (<= (depth lst) 2))
                                         (cdr lst))))))))))

;; Macro wrapper for PEG-PARSER.  Parses PEG grammars expressed as strings and
;; defines all the appropriate nonterminals.
(define-syntax define-grammar
  (lambda (x)
    (syntax-case x ()
      ((_ str)
       (peg-parser (syntax->datum #'str) x)))))
(define define-grammar-f peg-parser)

;; Parse a nonterminal and pattern listed in LST.
(define (peg-nonterm->defn lst for-syntax)
  (let* ((nonterm (car lst))
         (grabber (cadr lst))
         (pattern (caddr lst))
         (nonterm-name (datum->syntax for-syntax
                                      (string->symbol (cadr nonterm)))))
    #`(define-nonterm #,nonterm-name
       #,(cond
          ((string=? grabber "<--") (datum->syntax for-syntax 'all))
          ((string=? grabber "<-") (datum->syntax for-syntax 'body))
          (else (datum->syntax for-syntax 'none)))
       #,(compressor (peg-pattern->defn pattern for-syntax) for-syntax))))

;; Parse a pattern.
(define (peg-pattern->defn lst for-syntax)
  #`(or #,@(map (lambda (x) (peg-alternative->defn x for-syntax))
                (context-flatten (lambda (x) (eq? (car x) 'peg-alternative))
                                 (cdr lst)))))

;; Parse an alternative.
(define (peg-alternative->defn lst for-syntax)
  #`(and #,@(map (lambda (x) (peg-body->defn x for-syntax))
                 (context-flatten (lambda (x) (or (string? (car x))
                                             (eq? (car x) 'peg-suffix)))
                                  (cdr lst)))))

;; Parse a body.
(define (peg-body->defn lst for-syntax)
  (let ((suffix '())
        (front (datum->syntax for-syntax 'lit)))
    (cond
     ((eq? (car lst) 'peg-suffix)
      (set! suffix lst))
     ((string? (car lst))
      (begin (set! front (datum->syntax for-syntax
                                        (string->symbol (car lst))))
             (set! suffix (cadr lst))))
     (else `(peg-parse-body-fail ,lst)))
    #`(body #,front #,@(peg-suffix->defn
                        suffix
                        for-syntax))))

;; Parse a suffix.
(define (peg-suffix->defn lst for-syntax)
  #`(#,(peg-primary->defn (cadr lst) for-syntax)
     #,(if (null? (cddr lst))
           1
           (datum->syntax for-syntax (string->symbol (caddr lst))))))

;; Parse a primary.
(define (peg-primary->defn lst for-syntax)
  (let ((el (cadr lst)))
  (cond
   ((list? el)
    (cond
     ((eq? (car el) 'peg-literal)
      (peg-literal->defn el for-syntax))
     ((eq? (car el) 'peg-charclass)
      (peg-charclass->defn el for-syntax))
     ((eq? (car el) 'peg-nonterminal)
      (datum->syntax for-syntax (string->symbol (cadr el))))))
   ((string? el)
    (cond
     ((equal? el "(")
      (peg-pattern->defn (caddr lst) for-syntax))
     ((equal? el ".")
      (datum->syntax for-syntax 'peg-any))
     (else (datum->syntax for-syntax
                          `(peg-parse-any unknown-string ,lst)))))
   (else (datum->syntax for-syntax
                        `(peg-parse-any unknown-el ,lst))))))

;; Trims characters off the front and end of STR.
;; (trim-1chars "'ab'") -> "ab"
(define (trim-1chars str) (substring str 1 (- (string-length str) 1)))

;; Parses a literal.
(define (peg-literal->defn lst for-syntax)
  (datum->syntax for-syntax (trim-1chars (cadr lst))))

;; Parses a charclass.
(define (peg-charclass->defn lst for-syntax)
  #`(or
     #,@(map
         (lambda (cc)
           (cond
            ((eq? (car cc) 'charclass-range)
             #`(range #,(datum->syntax
                         for-syntax
                         (string-ref (cadr cc) 0))
                      #,(datum->syntax
                         for-syntax
                         (string-ref (cadr cc) 2))))
            ((eq? (car cc) 'charclass-single)
             (datum->syntax for-syntax (cadr cc)))))
         (context-flatten
          (lambda (x) (or (eq? (car x) 'charclass-range)
                          (eq? (car x) 'charclass-single)))
          (cdr lst)))))

;; Compresses a list to save the optimizer work.
;; e.g. (or (and a)) -> a
(define (compressor-core lst)
  (if (or (not (list? lst)) (null? lst))
      lst
      (cond
       ((and (or (eq? (car lst) 'or) (eq? (car lst) 'and))
             (null? (cddr lst)))
        (compressor-core (cadr lst)))
       ((and (eq? (car lst) 'body)
             (eq? (cadr lst) 'lit)
             (eq? (cadddr lst) 1))
        (compressor-core (caddr lst)))
       (else (map compressor-core lst)))))

(define (compressor syn for-syntax)
  (datum->syntax for-syntax
                 (compressor-core (syntax->datum syn))))

;; Builds a lambda-expressions for the pattern STR using accum.
(define (peg-string-compile str-stx accum)
  (peg-sexp-compile
   (compressor
    (peg-pattern->defn
     (peg:tree (peg-parse peg-pattern (syntax->datum str-stx))) str-stx)
    str-stx)
   accum))
