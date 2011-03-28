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
  #:export (peg-parse
            define-nonterm
;            define-nonterm-f
            peg-match)
;  #:export-syntax (define-nonterm)
  #:use-module (ice-9 peg codegen)
  #:use-module (ice-9 peg string-peg)
  #:use-module (ice-9 peg simplify-tree)
  #:use-module (ice-9 peg match-record)
  #:re-export (peg-sexp-compile
               define-grammar
               define-grammar-f
;               define-nonterm
               keyword-flatten
               context-flatten
               peg:start
               peg:end
               peg:string
               peg:tree
               peg:substring
               peg-record?))

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

(define (peg-extended-compile pattern accum)
  (syntax-case pattern (peg)
    ((peg str)
     (string? (syntax->datum #'str))
     (peg-string-compile #'str (if (eq? accum 'all) 'body accum)))
    (else (peg-sexp-compile pattern accum))))

;; The results of parsing using a nonterminal are cached.  Think of it like a
;; hash with no conflict resolution.  Process for deciding on the cache size
;; wasn't very scientific; just ran the benchmarks and stopped a little after
;; the point of diminishing returns on my box.
(define *cache-size* 512)

;; Defines a new nonterminal symbol accumulating with ACCUM.
(define-syntax define-nonterm
  (lambda (x)
    (syntax-case x ()
      ((_ sym accum pat)
       (let ((matchf (peg-extended-compile #'pat (syntax->datum #'accum)))
             (accumsym (syntax->datum #'accum))
             (c (datum->syntax x (gensym))));; the cache
         ;; CODE is the code to parse the string if the result isn't cached.
         (let ((syn (wrap-parser-for-users x matchf accumsym #'sym)))
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
