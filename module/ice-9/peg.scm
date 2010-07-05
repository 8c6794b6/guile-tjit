(define-module (ice-9 peg)
  :export (peg-sexp-compile peg-string-compile context-flatten peg-parse define-nonterm define-nonterm-f peg-match get-code define-grammar define-grammar-f peg:start peg:end peg:string peg:tree peg:substring peg-record? keyword-flatten)
  :autoload (ice-9 pretty-print) (peg-sexp-compile peg-string-compile context-flatten peg-parse define-nonterm define-nonterm-f peg-match get-code define-grammar define-grammar-f keyword-flatten)
  :use-module (ice-9 pretty-print))

(use-modules (ice-9 pretty-print))

(eval-when (compile load eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; CONVENIENCE MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eeval exp)
  (eval exp (interaction-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; MACRO BUILDERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Safe-bind helps to bind macros safely.
;; e.g.:
;; (safe-bind
;;  (a b)
;;  `(,a ,b))
;; gives:
;; (#<uninterned-symbol a cc608d0> #<uninterned-symbol b cc608a0>)
(define-syntax safe-bind
  (lambda (x)
    (syntax-case x ()
      ((_ vals . actions)
       (datum->syntax x (apply safe-bind-f
                               (cons
                                (syntax->datum #'vals)
                                (syntax->datum #'actions))))))))
;; (define-macro (safe-bind vals . actions)
;;   (apply safe-bind-f (cons vals actions)))
(define (safe-bind-f vals . actions)
  `(let ,(map (lambda (val) `(,val (make-symbol ,(symbol->string val)))) vals)
     ,@actions))

;; Unsafe-bind is like safe-bind but uses symbols that are easier to read while
;; debugging rather than safe ones.  Currently unused.
;; (define-macro (unsafe-bind vals . actions)
;;   (apply unsafe-bind-f (cons vals actions)))
;; (define (unsafe-bind-f vals . actions)
;;   `(let ,(map (lambda (val) `(,val ',val)) vals)
;;      ,@actions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; LOOPING CONSTRUCTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perform ACTION. If it succeeded, return its return value.  If it failed, run
;; IF_FAILS and try again
(define-syntax until-works
  (lambda (x)
    (syntax-case x ()
      ((_ action if-fails)
       #'(let ((retval action))
           (while (not retval)
                  if-fails
                  (set! retval action))
           retval)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; GENERIC LIST-PROCESSING MACROS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return #t if the list has only one element (calling length all the time on
;; potentially long lists was really slow).
(define-syntax single?
  (lambda (x)
    (syntax-case x ()
      ((_ lst)
       #'(and (list? lst) (not (null? lst)) (null? (cdr lst)))))))

;; Push an object onto a list.
(define-syntax push!
  (lambda (x)
    (syntax-case x ()
      ((_ lst obj)
       #'(set! lst (cons obj lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; CODE GENERATORS
;; These functions generate scheme code for parsing PEGs.
;; Conventions:
;;   accum: (all name body none)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Code we generate will be defined in a function, and always has to test
;; whether it's beyond the bounds of the string before it executes.
(define (cg-generic-lambda str strlen at code)
  `(lambda (,str ,strlen ,at)
     (if (>= ,at ,strlen)
         #f
         ,code)))
;; The short name makes the formatting below much easier to read.
(define cggl cg-generic-lambda)

;; Optimizations for CG-GENERIC-RET below...
(define *op-known-single-body* '(cg-string cg-peg-any cg-range))
;; ...done with optimizations (could use more of these).

;; Code we generate will have a certain return structure depending on how we're
;; accumulating (the ACCUM variable).
(define (cg-generic-ret accum name body-uneval at)
  (safe-bind
   (body)
   `(let ((,body ,body-uneval))
      ,(cond
        ((and (eq? accum 'all) name body)
         `(list ,at
                (cond
                 ((not (list? ,body)) (list ',name ,body))
                 ((null? ,body) ',name)
                 ((symbol? (car ,body)) (list ',name ,body))
                 (#t (cons ',name ,body)))))
        ((and (eq? accum 'name) name)
         `(list ,at ',name))
        ((and (eq? accum 'body) body)
         (cond
          ((member name *op-known-single-body*)
           `(list ,at ,body))
          (#t `(list ,at
                     (cond
                      (((@@ (ice-9 peg) single?) ,body) (car ,body))
                      (#t ,body))))))
        ((eq? accum 'none)
         `(list ,at '()))
        (#t
         (begin
           (pretty-print `(cg-generic-ret-error ,accum ,name ,body-uneval ,at))
           (pretty-print "Defaulting to accum of none.\n")
           `(list ,at '())))))))
;; The short name makes the formatting below much easier to read.
(define cggr cg-generic-ret)

;; Generates code that matches a particular string.
;; E.g.: (cg-string "abc" 'body)
(define (cg-string match accum)
  (safe-bind
   (str strlen at)
   (let ((len (string-length match)))
     (cggl str strlen at
           `(if (string=? (substring ,str ,at (min (+ ,at ,len) ,strlen))
                          ,match)
                ,(cggr accum 'cg-string match `(+ ,at ,len))
                #f)))))

;; Generates code for matching any character.
;; E.g.: (cg-peg-any 'body)
(define (cg-peg-any accum)
  (safe-bind
   (str strlen at)
   (cggl str strlen at
         (cggr accum 'cg-peg-any `(substring ,str ,at (+ ,at 1)) `(+ ,at 1)))))

;; Generates code for matching a range of characters between start and end.
;; E.g.: (cg-range #\a #\z 'body)
(define (cg-range start end accum)
  (safe-bind
   (str strlen at c)
   (cggl str strlen at
         `(let ((,c (string-ref ,str ,at)))
            (if (and
                 (char>=? ,c ,start)
                 (char<=? ,c ,end))
                ,(cggr accum 'cg-range `(string ,c) `(+ ,at 1))
                #f)))))

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

;; Takes a value, prints some debug output, and returns it.
(define (error-val val)
  (begin
    (pretty-print val)
    (pretty-print "Inserting into code for debugging.\n")
    val))

;; Takes an arbitrary expressions and accumulation variable, then parses it.
;; E.g.: (peg-sexp-compile '(and "abc" (or "-" (range #\a #\z))) 'all)
(define (peg-sexp-compile match accum)
   (cond
    ((string? match) (cg-string match (baf accum)))
    ((symbol? match) ;; either peg-any or a nonterminal
     (cond
      ((eq? match 'peg-any) (cg-peg-any (baf accum)))
      ;; if match is any other symbol it's a nonterminal, so just return it
      (#t match)))
    ((or (not (list? match)) (null? match))
     ;; anything besides a string, symbol, or list is an error
     (error-val `(peg-sexp-compile-error-1 ,match ,accum)))
    
    ((eq? (car match) 'range) ;; range of characters (e.g. [a-z])
     (cg-range (cadr match) (caddr match) (baf accum)))
    ((eq? (car match) 'ignore) ;; match but don't parse
     (peg-sexp-compile (cadr match) 'none))
    ((eq? (car match) 'capture) ;; parse
     (peg-sexp-compile (cadr match) 'body))
    ((eq? (car match) 'peg) ;; embedded PEG string
     (peg-string-compile (cadr match) (baf accum)))
    ((eq? (car match) 'and) (cg-and (cdr match) (baf accum)))
    ((eq? (car match) 'or) (cg-or (cdr match) (baf accum)))
    ((eq? (car match) 'body)
     (if (not (= (length match) 4))
         (error-val `(peg-sexp-compile-error-2 ,match ,accum))
         (apply cg-body (cons (baf accum) (cdr match)))))
    (#t (error-val `(peg-sexp-compile-error-3 ,match ,accum)))))

;;;;; Convenience macros for making sure things come out in a readable form.
;; If SYM is a list of one element, return (car SYM), else return SYM.
(define-syntax single-filter
  (lambda (x)
    (syntax-case x ()
      ((_ sym)
       #'(if (single? sym) (car sym) sym)))))
;; If OBJ is non-null, push it onto LST, otherwise do nothing.
(define-syntax push-not-null!
  (lambda (x)
    (syntax-case x ()
      ((_ lst obj)
       #'(if (not (null? obj)) (push! lst obj))))))

;; Top-level function builder for AND.  Reduces to a call to CG-AND-INT.
(define (cg-and arglst accum)
  (safe-bind
   (str strlen at body)
   `(lambda (,str ,strlen ,at)
      (let ((,body '()))
        ,(cg-and-int arglst accum str strlen at body)))))

;; Internal function builder for AND (calls itself).
(define (cg-and-int arglst accum str strlen at body)
  (safe-bind
   (res newat newbody)
   (if (null? arglst)
       (cggr accum 'cg-and `(reverse ,body) at) ;; base case
       (let ((mf (peg-sexp-compile (car arglst) accum))) ;; match function
         `(let ((,res (,mf ,str ,strlen ,at)))
            (if (not ,res) 
                #f ;; if the match failed, the and failed
                ;; otherwise update AT and BODY then recurse
                (let ((,newat (car ,res))
                      (,newbody (cadr ,res)))
                  (set! ,at ,newat)
                  ((@@ (ice-9 peg) push-not-null!) ,body ((@@ (ice-9 peg) single-filter) ,newbody))
                  ,(cg-and-int (cdr arglst) accum str strlen at body))))))))

;; Top-level function builder for OR.  Reduces to a call to CG-OR-INT.
(define (cg-or arglst accum)
  (safe-bind
   (str strlen at body)
   `(lambda (,str ,strlen ,at)
      ,(cg-or-int arglst accum str strlen at body))))

;; Internal function builder for OR (calls itself).
(define (cg-or-int arglst accum str strlen at body)
  (safe-bind
   (res)
   (if (null? arglst)
       #f ;; base case
       (let ((mf (peg-sexp-compile (car arglst) accum)))
         `(let ((,res (,mf ,str ,strlen ,at)))
            (if ,res ;; if the match succeeds, we're done
                ,(cggr accum 'cg-or `(cadr ,res) `(car ,res))
                ,(cg-or-int (cdr arglst) accum str strlen at body)))))))

;; Returns a block of code that tries to match MATCH, and on success updates AT
;; and BODY, return #f on failure and #t on success.
(define (cg-body-test match accum str strlen at body)
  (safe-bind
   (at2-body2 at2 body2)
   (let ((mf (peg-sexp-compile match accum)))
     `(let ((,at2-body2 (,mf ,str ,strlen ,at)))
        (if (or (not ,at2-body2) (= ,at (car ,at2-body2)))
            #f
            (let ((,at2 (car ,at2-body2))
                  (,body2 (cadr ,at2-body2)))
              (set! ,at ,at2)
              ((@@ (ice-9 peg) push-not-null!)
               ,body
               ((@@ (ice-9 peg) single-filter) ,body2))
              #t))))))

;; Returns a block of code that sees whether NUM wants us to try and match more
;; given that we've already matched COUNT.
(define (cg-body-more num count)
  (cond ((number? num) `(< ,count ,num))
        ((eq? num '+) #t)
        ((eq? num '*) #t)
        ((eq? num '?) `(< ,count 1))
        (#t (error-val `(cg-body-more-error ,num ,count)))))

;; Returns a function that takes a paramter indicating whether or not the match
;; was succesful and returns what the body expression should return.
(define (cg-body-ret accum type name body at at2)
  (safe-bind
   (success)
   `(lambda (,success)
      ,(cond ((eq? type '!) `(if ,success #f ,(cggr accum name ''() at)))
             ((eq? type '&) `(if ,success ,(cggr accum name ''() at) #f))
             ((eq? type 'lit)
              `(if ,success ,(cggr accum name `(reverse ,body) at2) #f))
             (#t (error-val
                  `(cg-body-ret-error ,type ,accum ,name ,body ,at ,at2)))))))

;; Returns a block of code that sees whether COUNT satisfies the constraints of
;; NUM.
(define (cg-body-success num count)
  (cond ((number? num) `(= ,count ,num))
        ((eq? num '+) `(>= ,count 1))
        ((eq? num '*) #t)
        ((eq? num '?) `(<= ,count 1))
        (#t `(cg-body-success-error ,num))))

;; Returns a function that parses a BODY element.
(define (cg-body accum type match num)
  (safe-bind
   (str strlen at at2 count body)
   `(lambda (,str ,strlen ,at)
      (let ((,at2 ,at) (,count 0) (,body '()))
        (while (and ,(cg-body-test match accum str strlen at2 body)
                    (set! ,count (+ ,count 1))
                    ,(cg-body-more num count)))
        (,(cg-body-ret accum type 'cg-body body at at2)
         ,(cg-body-success num count))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; FOR DEFINING AND USING NONTERMINALS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The results of parsing using a nonterminal are cached.  Think of it like a
;; hash with no conflict resolution.  Process for deciding on the cache size
;; wasn't very scientific; just ran the benchmarks and stopped a little after
;; the point of diminishing returns on my box.
(define *cache-size* 512)

;; Defines a new nonterminal symbol accumulating with ACCUM.
(define-syntax define-nonterm
  (lambda (x)
    (syntax-case x ()
      ((_ sym accum match)
       (let ((matchf (peg-sexp-compile (syntax->datum #'match)
                                    (syntax->datum #'accum)))
             (symsym (syntax->datum #'sym))
             (accumsym (syntax->datum #'accum))
             (c (datum->syntax x (gensym))));; the cache
         ;; CODE is the code to parse the string if the result isn't cached.
         (let ((code
                (safe-bind
                 (str strlen at res body)
                `(lambda (,str ,strlen ,at)
                   (let ((,res (,matchf ,str ,strlen ,at)))
                     ;; Try to match the nonterminal.
                     (if ,res
                         ;; If we matched, do some post-processing to figure out
                         ;; what data to propagate upward.
                         (let ((,at (car ,res))
                               (,body (cadr ,res)))
                           ,(cond
                             ((eq? accumsym 'name)
                              `(list ,at ',symsym))
                             ((eq? accumsym 'all)
                              `(list (car ,res)
                                     (cond
                                      ((not (list? ,body))
                                       (list ',symsym ,body))
                                      ((null? ,body) ',symsym)
                                      ((symbol? (car ,body))
                                       (list ',symsym ,body))
                                      (#t (cons ',symsym ,body)))))
                             ((eq? accumsym 'none) `(list (car ,res) '()))
                             (#t (begin res))))
                         ;; If we didn't match, just return false.
                         #f))))))
           #`(begin
               (define #,c (make-vector *cache-size* #f));; the cache
               (define (sym str strlen at)
                 (let* ((vref (vector-ref #,c (modulo at *cache-size*))))
                   ;; Check to see whether the value is cached.
                   (if (and vref (eq? (car vref) str) (= (cadr vref) at))
                       (caddr vref);; If it is return it.
                       (let ((fres ;; Else calculate it and cache it.
                              (#,(datum->syntax x code) str strlen at)))
                         (vector-set! #,c (modulo at *cache-size*)
                                      (list str at fres))
                         fres))))

               ;; Store the code in case people want to debug.
               (set-symbol-property!
                'sym 'code #,(datum->syntax x (list 'quote code)))
               sym)))))))

;; Gets the code corresponding to NONTERM
(define-syntax get-code
  (lambda (x)
    (syntax-case x ()
      ((_ nonterm)
       #`(pretty-print (symbol-property 'nonterm 'code))))))

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
      ((_ peg-matcher string-uncopied)
       (let ((pmsym (syntax->datum #'peg-matcher)))
         (let ((peg-sexp-compile
                (if (string? pmsym)
                    (peg-string-compile pmsym 'body)
                    (peg-sexp-compile pmsym 'body))))
           ;; We copy the string before using it because it might have been
           ;; modified in-place since the last time it was parsed, which would
           ;; invalidate the cache.  Guile uses copy-on-write for strings, so
           ;; this is fast.
           #`(let ((string (string-copy string-uncopied))
                   (strlen (string-length string-uncopied))
                   (at 0))
               (let ((ret ((@@ (ice-9 peg) until-works)
                           (or (>= at strlen)
                               (#,(datum->syntax x peg-sexp-compile)
                                string strlen at))
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
          ;; (pretty-print "Invalid PEG grammar!\n")
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
         (#t 'none))
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
     (#t `(peg-parse-body-fail ,lst)))
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
     (#t `(peg-parse-any unknown-string ,lst))))
   (#t `(peg-parse-any unknown-el ,lst)))))

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
       (#t (map compressor lst)))))

;; Builds a lambda-expressions for the pattern STR using accum.
(define (peg-string-compile str accum)
  (peg-sexp-compile
   (compressor (peg-parse-pattern (peg:tree (peg-parse peg-pattern str))))
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

