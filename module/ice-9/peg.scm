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
  #:export (context-flatten
            peg-parse
;            define-nonterm
;            define-nonterm-f
            peg-match
            peg:start
            peg:end
            peg:string
            peg:tree
            peg:substring
            peg-record?
            keyword-flatten)
;  #:export-syntax (define-nonterm)
  #:use-module (ice-9 peg codegen)
  #:use-module (ice-9 peg string-peg)
  #:re-export (peg-sexp-compile
               define-grammar
               define-grammar-f
               define-nonterm)
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

