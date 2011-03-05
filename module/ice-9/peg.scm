;;;; peg.scm --- Parsing Expression Grammar (PEG) parser generator
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

(define-module (ice-9 peg)
  #:export (peg-string-compile
            context-flatten
            peg-parse
            define-nonterm
            define-nonterm-f
            peg-match
            define-grammar
            define-grammar-f
            peg:start
            peg:end
            peg:string
            peg:tree
            peg:substring
            peg-record?
            keyword-flatten)
  #:use-module (ice-9 peg codegen)
  #:re-export (peg-sexp-compile)
  #:use-module (system base pmatch))

;;;
;;; Helper Macros
;;;

(define-syntax until
  (syntax-rules ()
    "Evaluate TEST.  If it is true, return its value.  Otherwise,
execute the STMTs and try again."
    ((_ test stmt stmt* ...)
     (let lp ()
       (or test
           (begin stmt stmt* ... (lp)))))))

(define-syntax single?
  (syntax-rules ()
    "Return #t if X is a list of one element."
    ((_ x)
     (pmatch x
       ((_) #t)
       (else #f)))))

(eval-when (compile load eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; FOR DEFINING AND USING NONTERMINALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The results of parsing using a nonterminal are cached.  Think of it like a
;; hash with no conflict resolution.  Process for deciding on the cache size
;; wasn't very scientific; just ran the benchmarks and stopped a little after
;; the point of diminishing returns on my box.
(define *cache-size* 512)

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

;; Parses STRING using NONTERM
(define (peg-parse nonterm string)
  ;; We copy the string before using it because it might have been modified
  ;; in-place since the last time it was parsed, which would invalidate the
  ;; cache.  Guile uses copy-on-write for strings, so this is fast.
  (let ((res (nonterm (string-copy string) (string-length string) 0)))
    (if (not res)
        #f
        (make-prec 0 (car res) string (string-collapse (cadr res))))))

;; Searches through STRING for something that parses to PEG-MATCHER.  Think
;; regexp search.
(define-syntax peg-match
  (lambda (x)
    (syntax-case x ()
      ((_ pattern string-uncopied)
       (let ((pmsym (syntax->datum #'pattern)))
         (let ((matcher (if (string? (syntax->datum #'pattern))
                            (peg-string-compile #'pattern 'body)
                            (peg-sexp-compile #'pattern 'body))))
           ;; We copy the string before using it because it might have been
           ;; modified in-place since the last time it was parsed, which would
           ;; invalidate the cache.  Guile uses copy-on-write for strings, so
           ;; this is fast.
           #`(let ((string (string-copy string-uncopied))
                   (strlen (string-length string-uncopied))
                   (at 0))
               (let ((ret (until (or (>= at strlen)
                                     (#,matcher string strlen at))
                                 (set! at (+ at 1)))))
                 (if (eq? ret #t) ;; (>= at strlen) succeeded
                     #f
                     (let ((end (car ret))
                           (match (cadr ret)))
                       (make-prec
                        at end string
                        (string-collapse match))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; POST-PROCESSING FUNCTIONS (TO CANONICALIZE MATCH TREES)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Is everything in LST true?
(define (andlst lst)
  (or (null? lst)
      (and (car lst) (andlst (cdr lst)))))

;; Is LST a list of strings?
(define (string-list? lst)
  (and (list? lst) (not (null? lst))
       (andlst (map string? lst))))

;; Groups all strings that are next to each other in LST.  Used in
;; STRING-COLLAPSE.
(define (string-group lst)
  (if (not (list? lst))
      lst
      (if (null? lst)
          '()
          (let ((next (string-group (cdr lst))))
            (if (not (string? (car lst)))
                (cons (car lst) next)
                (if (and (not (null? next))
                         (list? (car next))
                         (string? (caar next)))
                    (cons (cons (car lst) (car next)) (cdr next))
                    (cons (list (car lst)) next)))))))


;; Collapses all the string in LST.
;; ("a" "b" (c d) "e" "f") -> ("ab" (c d) "ef")
(define (string-collapse lst)
  (if (list? lst)
      (let ((res (map (lambda (x) (if (string-list? x)
                                      (apply string-append x)
                                      x))
                      (string-group (map string-collapse lst)))))
        (if (single? res) (car res) res))
      lst))

;; If LST is an atom, return (list LST), else return LST.
(define (mklst lst)
  (if (not (list? lst)) (list lst) lst))

;; Takes a list and "flattens" it, using the predicate TST to know when to stop
;; instead of terminating on atoms (see tutorial).
(define (context-flatten tst lst)
  (if (or (not (list? lst)) (null? lst))
      lst
      (if (tst lst)
          (list lst)
          (apply append
                 (map (lambda (x) (mklst (context-flatten tst x)))
                      lst)))))

;; Takes a list and "flattens" it, using the list of keywords KEYWORD-LST to
;; know when to stop at (see tutorial).
(define (keyword-flatten keyword-lst lst)
  (context-flatten
   (lambda (x)
     (if (or (not (list? x)) (null? x))
         #t
         (member (car x) keyword-lst)))
   lst))

;; Gets the left-hand depth of a list.
(define (depth lst)
  (if (or (not (list? lst)) (null? lst))
      0
      (+ 1 (depth (car lst)))))

;; Trims characters off the front and end of STR.
;; (trim-1chars "'ab'") -> "ab"
(define (trim-1chars str) (substring str 1 (- (string-length str) 1)))

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
(define (peg-parser str)
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
            (cons 'begin (map (lambda (x) (peg-parse-nonterm x))
                              (context-flatten (lambda (lst) (<= (depth lst) 2))
                                          (cdr lst))))))))))

;; Macro wrapper for PEG-PARSER.  Parses PEG grammars expressed as strings and
;; defines all the appropriate nonterminals.
(define-syntax define-grammar
  (lambda (x)
    (syntax-case x ()
      ((_ str)
       (datum->syntax x (peg-parser (syntax->datum #'str)))))))
(define define-grammar-f peg-parser)

;; Parse a nonterminal and pattern listed in LST.
(define (peg-parse-nonterm lst)
  (let ((nonterm (car lst))
        (grabber (cadr lst))
        (pattern (caddr lst)))
    `(define-nonterm ,(string->symbol (cadr nonterm))
       ,(cond
         ((string=? grabber "<--") 'all)
         ((string=? grabber "<-") 'body)
         (else 'none))
       ,(compressor (peg-parse-pattern pattern)))))

;; Parse a pattern.
(define (peg-parse-pattern lst)
  (cons 'or (map peg-parse-alternative
                 (context-flatten (lambda (x) (eq? (car x) 'peg-alternative))
                             (cdr lst)))))

;; Parse an alternative.
(define (peg-parse-alternative lst)
  (cons 'and (map peg-parse-body
                  (context-flatten (lambda (x) (or (string? (car x))
                                              (eq? (car x) 'peg-suffix)))
                              (cdr lst)))))

;; Parse a body.
(define (peg-parse-body lst)
  (let ((suffix '())
        (front 'lit))
    (cond
     ((eq? (car lst) 'peg-suffix)
      (set! suffix lst))
     ((string? (car lst))
      (begin (set! front (string->symbol (car lst)))
             (set! suffix (cadr lst))))
     (else `(peg-parse-body-fail ,lst)))
    `(body ,front ,@(peg-parse-suffix suffix))))

;; Parse a suffix.
(define (peg-parse-suffix lst)
  (list (peg-parse-primary (cadr lst))
        (if (null? (cddr lst))
            1
            (string->symbol (caddr lst)))))

;; Parse a primary.
(define (peg-parse-primary lst)
  (let ((el (cadr lst)))
  (cond
   ((list? el)
    (cond
     ((eq? (car el) 'peg-literal)
      (peg-parse-literal el))
     ((eq? (car el) 'peg-charclass)
      (peg-parse-charclass el))
     ((eq? (car el) 'peg-nonterminal)
      (string->symbol (cadr el)))))
   ((string? el)
    (cond
     ((equal? el "(")
      (peg-parse-pattern (caddr lst)))
     ((equal? el ".")
      'peg-any)
     (else `(peg-parse-any unknown-string ,lst))))
   (else `(peg-parse-any unknown-el ,lst)))))

;; Parses a literal.
(define (peg-parse-literal lst) (trim-1chars (cadr lst)))

;; Parses a charclass.
(define (peg-parse-charclass lst)
  (cons 'or
        (map
         (lambda (cc)
           (cond
            ((eq? (car cc) 'charclass-range)
             `(range ,(string-ref (cadr cc) 0) ,(string-ref (cadr cc) 2)))
            ((eq? (car cc) 'charclass-single)
             (cadr cc))))
         (context-flatten
          (lambda (x) (or (eq? (car x) 'charclass-range)
                          (eq? (car x) 'charclass-single)))
          (cdr lst)))))

;; Compresses a list to save the optimizer work.
;; e.g. (or (and a)) -> a
(define (compressor lst)
  (if (or (not (list? lst)) (null? lst))
      lst
      (cond
       ((and (or (eq? (car lst) 'or) (eq? (car lst) 'and))
             (null? (cddr lst)))
        (compressor (cadr lst)))
       ((and (eq? (car lst) 'body)
             (eq? (cadr lst) 'lit)
             (eq? (cadddr lst) 1))
        (compressor (caddr lst)))
       (else (map compressor lst)))))

;; Builds a lambda-expressions for the pattern STR using accum.
(define (peg-string-compile str-stx accum)
  (peg-sexp-compile
   (datum->syntax
    str-stx
    (compressor
     (peg-parse-pattern
      (peg:tree (peg-parse peg-pattern (syntax->datum str-stx))))))
   accum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; PMATCH STRUCTURE MUNGING
;; Pretty self-explanatory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define prec
  (make-record-type "peg" '(start end string tree)))
(define make-prec
  (record-constructor prec '(start end string tree)))
(define (peg:start pm)
  (if pm ((record-accessor prec 'start) pm) #f))
(define (peg:end pm)
  (if pm ((record-accessor prec 'end) pm) #f))
(define (peg:string pm)
  (if pm ((record-accessor prec 'string) pm) #f))
(define (peg:tree pm)
  (if pm ((record-accessor prec 'tree) pm) #f))
(define (peg:substring pm)
  (if pm (substring (peg:string pm) (peg:start pm) (peg:end pm)) #f))
(define peg-record? (record-predicate prec))

)

