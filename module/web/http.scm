;;; HTTP messages

;; Copyright (C)  2010, 2011, 2012, 2013, 2014 Free Software Foundation, Inc.

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:
;;;
;;; This module has a number of routines to parse textual
;;; representations of HTTP data into native Scheme data structures.
;;;
;;; It tries to follow RFCs fairly strictly---the road to perdition
;;; being paved with compatibility hacks---though some allowances are
;;; made for not-too-divergent texts (like a quality of .2 which should
;;; be 0.2, etc).
;;;
;;; Code:

(define-module (web http)
  #:use-module ((srfi srfi-1) #:select (append-map! map!))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 q)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (web uri)
  #:export (string->header
            header->string

            declare-header!
            declare-opaque-header!
            known-header?
            header-parser
            header-validator
            header-writer

            read-header
            parse-header
            valid-header?
            write-header

            read-headers
            write-headers

            parse-http-method
            parse-http-version
            parse-request-uri

            read-request-line
            write-request-line
            read-response-line
            write-response-line

            make-chunked-input-port
            make-chunked-output-port

            http-proxy-port?
            set-http-proxy-port?!))


(define (string->header name)
  "Parse NAME to a symbolic header name."
  (string->symbol (string-downcase name)))

(define-record-type <header-decl>
  (make-header-decl name parser validator writer multiple?)
  header-decl?
  (name header-decl-name)
  (parser header-decl-parser)
  (validator header-decl-validator)
  (writer header-decl-writer)
  (multiple? header-decl-multiple?))

;; sym -> header
(define *declared-headers* (make-hash-table))

(define (lookup-header-decl sym)
  (hashq-ref *declared-headers* sym))

(define* (declare-header! name
                          parser
                          validator
                          writer
                          #:key multiple?)
  "Declare a parser, validator, and writer for a given header."
  (if (and (string? name) parser validator writer)
      (let ((decl (make-header-decl name parser validator writer multiple?)))
        (hashq-set! *declared-headers* (string->header name) decl)
        decl)
      (error "bad header decl" name parser validator writer multiple?)))

(define (header->string sym)
  "Return the string form for the header named SYM."
  (let ((decl (lookup-header-decl sym)))
    (if decl
        (header-decl-name decl)
        (string-titlecase (symbol->string sym)))))

(define (known-header? sym)
  "Return ‘#t’ iff SYM is a known header, with associated
parsers and serialization procedures."
  (and (lookup-header-decl sym) #t))

(define (header-parser sym)
  "Return the value parser for headers named SYM.  The result is a
procedure that takes one argument, a string, and returns the parsed
value.  If the header isn't known to Guile, a default parser is returned
that passes through the string unchanged."
  (let ((decl (lookup-header-decl sym)))
    (if decl
        (header-decl-parser decl)
        (lambda (x) x))))

(define (header-validator sym)
  "Return a predicate which returns ‘#t’ if the given value is valid
for headers named SYM.  The default validator for unknown headers
is ‘string?’."
  (let ((decl (lookup-header-decl sym)))
    (if decl
        (header-decl-validator decl)
        string?)))

(define (header-writer sym)
  "Return a procedure that writes values for headers named SYM to a
port.  The resulting procedure takes two arguments: a value and a port.
The default writer is ‘display’."
  (let ((decl (lookup-header-decl sym)))
    (if decl
        (header-decl-writer decl)
        display)))

(define (read-line* port)
  (let* ((pair (%read-line port))
         (line (car pair))
         (delim (cdr pair)))
    (if (and (string? line) (char? delim))
        (let ((orig-len (string-length line)))
          (let lp ((len orig-len))
            (if (and (> len 0)
                     (char-whitespace? (string-ref line (1- len))))
                (lp (1- len))
                (if (= len orig-len)
                    line
                    (substring line 0 len)))))
        (bad-header '%read line))))

(define (read-continuation-line port val)
  (if (or (eqv? (peek-char port) #\space)
          (eqv? (peek-char port) #\tab))
      (read-continuation-line port
                              (string-append val
                                             (begin
                                               (read-line* port))))
      val))

(define *eof* (call-with-input-string "" read))

(define (read-header port)
  "Read one HTTP header from PORT. Return two values: the header
name and the parsed Scheme value. May raise an exception if the header
was known but the value was invalid.

Returns the end-of-file object for both values if the end of the message
body was reached (i.e., a blank line)."
  (let ((line (read-line* port)))
    (if (or (string-null? line)
            (string=? line "\r"))
        (values *eof* *eof*)
        (let* ((delim (or (string-index line #\:)
                          (bad-header '%read line)))
               (sym (string->header (substring line 0 delim))))
          (values
           sym
           (parse-header
            sym
            (read-continuation-line
             port
             (string-trim-both line char-set:whitespace (1+ delim)))))))))

(define (parse-header sym val)
  "Parse VAL, a string, with the parser registered for the header
named SYM.  Returns the parsed value."
  ((header-parser sym) val))

(define (valid-header? sym val)
  "Returns a true value iff VAL is a valid Scheme value for the
header with name SYM."
  (if (symbol? sym)
      ((header-validator sym) val)
      (error "header name not a symbol" sym)))

(define (write-header sym val port)
  "Write the given header name and value to PORT, using the writer
from ‘header-writer’."
  (display (header->string sym) port)
  (display ": " port)
  ((header-writer sym) val port)
  (display "\r\n" port))

(define (read-headers port)
  "Read the headers of an HTTP message from PORT, returning them
as an ordered alist."
  (let lp ((headers '()))
    (call-with-values (lambda () (read-header port))
      (lambda (k v)
        (if (eof-object? k)
            (reverse! headers)
            (lp (acons k v headers)))))))

(define (write-headers headers port)
  "Write the given header alist to PORT.  Doesn't write the final
‘\\r\\n’, as the user might want to add another header."
  (let lp ((headers headers))
    (if (pair? headers)
        (begin
          (write-header (caar headers) (cdar headers) port)
          (lp (cdr headers))))))




;;;
;;; Utilities
;;;

(define (bad-header sym val)
  (throw 'bad-header sym val))
(define (bad-header-component sym val)
  (throw 'bad-header-component sym val))

(define (bad-header-printer port key args default-printer)
  (apply (case-lambda
           ((sym val)
            (format port "Bad ~a header: ~a\n" (header->string sym) val))
           (_ (default-printer)))
         args))
(define (bad-header-component-printer port key args default-printer)
  (apply (case-lambda
           ((sym val)
            (format port "Bad ~a header component: ~a\n" sym val))
           (_ (default-printer)))
         args))
(set-exception-printer! 'bad-header bad-header-printer)
(set-exception-printer! 'bad-header-component bad-header-component-printer)

(define (parse-opaque-string str)
  str)
(define (validate-opaque-string val)
  (string? val))
(define (write-opaque-string val port)
  (display val port))

(define separators-without-slash
  (string->char-set "[^][()<>@,;:\\\"?= \t]"))
(define (validate-media-type str)
  (let ((idx (string-index str #\/)))
    (and idx (= idx (string-rindex str #\/))
         (not (string-index str separators-without-slash)))))
(define (parse-media-type str)
  (if (validate-media-type str)
      (string->symbol str)
      (bad-header-component 'media-type str)))

(define* (skip-whitespace str #:optional (start 0) (end (string-length str)))
  (let lp ((i start))
    (if (and (< i end) (char-whitespace? (string-ref str i)))
        (lp (1+ i))
        i)))

(define* (trim-whitespace str #:optional (start 0) (end (string-length str)))
  (let lp ((i end))
    (if (and (< start i) (char-whitespace? (string-ref str (1- i))))
        (lp (1- i))
        i)))

(define* (split-and-trim str #:optional (delim #\,)
                         (start 0) (end (string-length str)))
  (let lp ((i start))
    (if (< i end)
        (let* ((idx (string-index str delim i end))
               (tok (string-trim-both str char-set:whitespace i (or idx end))))
          (cons tok (split-and-trim str delim (if idx (1+ idx) end) end)))
        '())))

(define (list-of-strings? val)
  (list-of? val string?))

(define (write-list-of-strings val port)
  (write-list val port display ", "))

(define (split-header-names str)
  (map string->header (split-and-trim str)))

(define (list-of-header-names? val)
  (list-of? val symbol?))

(define (write-header-list val port)
  (write-list val port
              (lambda (x port)
                (display (header->string x) port))
              ", "))

(define (collect-escaped-string from start len escapes)
  (let ((to (make-string len)))
    (let lp ((start start) (i 0) (escapes escapes))
      (if (null? escapes)
          (begin
            (substring-move! from start (+ start (- len i)) to i)
            to)
          (let* ((e (car escapes))
                 (next-start (+ start (- e i) 2)))
            (substring-move! from start (- next-start 2) to i)
            (string-set! to e (string-ref from (- next-start 1)))
            (lp next-start (1+ e) (cdr escapes)))))))

;; in incremental mode, returns two values: the string, and the index at
;; which the string ended
(define* (parse-qstring str #:optional
                        (start 0) (end (trim-whitespace str start))
                        #:key incremental?)
  (if (and (< start end) (eqv? (string-ref str start) #\"))
      (let lp ((i (1+ start)) (qi 0) (escapes '()))
        (if (< i end)
            (case (string-ref str i)
              ((#\\)
               (lp (+ i 2) (1+ qi) (cons qi escapes)))
              ((#\")
               (let ((out (collect-escaped-string str (1+ start) qi escapes)))
                 (if incremental?
                     (values out (1+ i))
                     (if (= (1+ i) end)
                         out
                         (bad-header-component 'qstring str)))))
              (else
               (lp (1+ i) (1+ qi) escapes)))
            (bad-header-component 'qstring str)))
      (bad-header-component 'qstring str)))

(define (write-list l port write-item delim)
  (if (pair? l)
      (let lp ((l l))
        (write-item (car l) port)
        (if (pair? (cdr l))
            (begin
              (display delim port)
              (lp (cdr l)))))))

(define (write-qstring str port)
  (display #\" port)
  (if (string-index str #\")
      ;; optimize me
      (write-list (string-split str #\") port display "\\\"")
      (display str port))
  (display #\" port))

(define* (parse-quality str #:optional (start 0) (end (string-length str)))
  (define (char->decimal c)
    (let ((i (- (char->integer c) (char->integer #\0))))
      (if (and (<= 0 i) (< i 10))
          i
          (bad-header-component 'quality str))))
  (cond
   ((not (< start end))
    (bad-header-component 'quality str))
   ((eqv? (string-ref str start) #\1)
    (if (or (string= str "1" start end)
            (string= str "1." start end)
            (string= str "1.0" start end)
            (string= str "1.00" start end)
            (string= str "1.000" start end))
        1000
        (bad-header-component 'quality str)))
   ((eqv? (string-ref str start) #\0)
    (if (or (string= str "0" start end)
            (string= str "0." start end))
        0
        (if (< 2 (- end start) 6)
            (let lp ((place 1) (i (+ start 4)) (q 0))
              (if (= i (1+ start))
                  (if (eqv? (string-ref str (1+ start)) #\.)
                      q
                      (bad-header-component 'quality str))
                  (lp (* 10 place) (1- i)
                      (if (< i end)
                          (+ q (* place (char->decimal (string-ref str i))))
                          q))))
            (bad-header-component 'quality str))))
   ;; Allow the nonstandard .2 instead of 0.2.
   ((and (eqv? (string-ref str start) #\.)
         (< 1 (- end start) 5))
    (let lp ((place 1) (i (+ start 3)) (q 0))
      (if (= i start)
          q
          (lp (* 10 place) (1- i)
              (if (< i end)
                  (+ q (* place (char->decimal (string-ref str i))))
                  q)))))
   (else
    (bad-header-component 'quality str))))

(define (valid-quality? q)
  (and (non-negative-integer? q) (<= q 1000)))

(define (write-quality q port)
  (define (digit->char d)
    (integer->char (+ (char->integer #\0) d)))
  (display (digit->char (modulo (quotient q 1000) 10)) port)
  (display #\. port)
  (display (digit->char (modulo (quotient q 100) 10)) port)
  (display (digit->char (modulo (quotient q 10) 10)) port)
  (display (digit->char (modulo q 10)) port))

(define (list-of? val pred)
  (or (null? val)
      (and (pair? val)
           (pred (car val))
           (list-of? (cdr val) pred))))

(define* (parse-quality-list str)
  (map (lambda (part)
         (cond
          ((string-rindex part #\;)
           => (lambda (idx)
                (let ((qpart (string-trim-both part char-set:whitespace (1+ idx))))
                  (if (string-prefix? "q=" qpart)
                      (cons (parse-quality qpart 2)
                            (string-trim-both part char-set:whitespace 0 idx))
                      (bad-header-component 'quality qpart)))))
          (else
           (cons 1000 (string-trim-both part char-set:whitespace)))))
       (string-split str #\,)))

(define (validate-quality-list l)
  (list-of? l
            (lambda (elt)
              (and (pair? elt)
                   (valid-quality? (car elt))
                   (string? (cdr elt))))))

(define (write-quality-list l port)
  (write-list l port
              (lambda (x port)
                (let ((q (car x))
                      (str (cdr x)))
                  (display str port)
                  (if (< q 1000)
                      (begin
                        (display ";q=" port)
                        (write-quality q port)))))
              ","))

(define* (parse-non-negative-integer val #:optional (start 0)
                                     (end (string-length val)))
  (define (char->decimal c)
    (let ((i (- (char->integer c) (char->integer #\0))))
      (if (and (<= 0 i) (< i 10))
          i
          (bad-header-component 'non-negative-integer val))))
  (if (not (< start end))
      (bad-header-component 'non-negative-integer val)
      (let lp ((i start) (out 0))
        (if (< i end)
            (lp (1+ i)
                (+ (* out 10) (char->decimal (string-ref val i))))
            out))))

(define (non-negative-integer? code)
  (and (number? code) (>= code 0) (exact? code) (integer? code)))
                                    
(define (default-val-parser k val)
  val)

(define (default-val-validator k val)
  (or (not val) (string? val)))

(define (default-val-writer k val port)
  (if (or (string-index val #\;)
          (string-index val #\,)
          (string-index val #\"))
      (write-qstring val port)
      (display val port)))

(define* (parse-key-value-list str #:optional
                               (val-parser default-val-parser)
                               (start 0) (end (string-length str)))
  (let lp ((i start) (out '()))
    (if (not (< i end))
        (reverse! out)
        (let* ((i (skip-whitespace str i end))
               (eq (string-index str #\= i end))
               (comma (string-index str #\, i end))
               (delim (min (or eq end) (or comma end)))
               (k (string->symbol
                   (substring str i (trim-whitespace str i delim)))))
          (call-with-values
              (lambda ()
                (if (and eq (or (not comma) (< eq comma)))
                    (let ((i (skip-whitespace str (1+ eq) end)))
                      (if (and (< i end) (eqv? (string-ref str i) #\"))
                          (parse-qstring str i end #:incremental? #t)
                          (values (substring str i
                                             (trim-whitespace str i
                                                              (or comma end)))
                                  (or comma end))))
                    (values #f delim)))
            (lambda (v-str next-i)
              (let ((v (val-parser k v-str))
                    (i (skip-whitespace str next-i end)))
                (if (or (= i end) (eqv? (string-ref str i) #\,))
                    (lp (1+ i) (cons (if v (cons k v) k) out))
                    (bad-header-component 'key-value-list
                                          (substring str start end))))))))))

(define* (key-value-list? list #:optional
                          (valid? default-val-validator))
  (list-of? list
            (lambda (elt)
              (cond
               ((pair? elt)
                (let ((k (car elt))
                      (v (cdr elt)))
                  (and (symbol? k)
                       (valid? k v))))
               ((symbol? elt)
                (valid? elt #f))
               (else #f)))))

(define* (write-key-value-list list port #:optional
                               (val-writer default-val-writer) (delim ", "))
  (write-list
   list port
   (lambda (x port)
     (let ((k (if (pair? x) (car x) x))
           (v (if (pair? x) (cdr x) #f)))
       (display k port)
       (if v
           (begin
             (display #\= port)
             (val-writer k v port)))))
   delim))

;; param-component = token [ "=" (token | quoted-string) ] \
;;    *(";" token [ "=" (token | quoted-string) ])
;;
(define param-delimiters (char-set #\, #\; #\=))
(define param-value-delimiters (char-set-adjoin char-set:whitespace #\, #\;))
(define* (parse-param-component str #:optional
                                (val-parser default-val-parser)
                                (start 0) (end (string-length str)))
  (let lp ((i start) (out '()))
    (if (not (< i end))
        (values (reverse! out) end)
        (let ((delim (string-index str param-delimiters i)))
          (let ((k (string->symbol
                    (substring str i (trim-whitespace str i (or delim end)))))
                (delimc (and delim (string-ref str delim))))
            (case delimc
              ((#\=)
               (call-with-values
                   (lambda ()
                     (let ((i (skip-whitespace str (1+ delim) end)))
                       (if (and (< i end) (eqv? (string-ref str i) #\"))
                           (parse-qstring str i end #:incremental? #t)
                           (let ((delim
                                  (or (string-index str param-value-delimiters
                                                    i end)
                                      end)))
                             (values (substring str i delim)
                                     delim)))))
                 (lambda (v-str next-i)
                   (let* ((v (val-parser k v-str))
                          (x (if v (cons k v) k))
                          (i (skip-whitespace str next-i end)))
                     (case (and (< i end) (string-ref str i))
                       ((#f)
                        (values (reverse! (cons x out)) end))
                       ((#\;)
                        (lp (skip-whitespace str (1+ i) end)
                            (cons x out)))
                       (else            ; including #\,
                        (values (reverse! (cons x out)) i)))))))
              ((#\;)
               (let ((v (val-parser k #f)))
                 (lp (skip-whitespace str (1+ delim) end)
                     (cons (if v (cons k v) k) out))))
             
              (else ;; either the end of the string or a #\,
               (let ((v (val-parser k #f)))
                 (values (reverse! (cons (if v (cons k v) k) out))
                         (or delim end))))))))))

(define* (parse-param-list str #:optional
                           (val-parser default-val-parser)
                           (start 0) (end (string-length str)))
  (let lp ((i start) (out '()))
    (call-with-values
        (lambda () (parse-param-component str val-parser i end))
      (lambda (item i)
        (if (< i end)
            (if (eqv? (string-ref str i) #\,)
                (lp (skip-whitespace str (1+ i) end)
                    (cons item out))
                (bad-header-component 'param-list str))
            (reverse! (cons item out)))))))

(define* (validate-param-list list #:optional
                              (valid? default-val-validator))
  (list-of? list
            (lambda (elt)
              (key-value-list? elt valid?))))

(define* (write-param-list list port #:optional
                           (val-writer default-val-writer))
  (write-list
   list port
   (lambda (item port)
     (write-key-value-list item port val-writer ";"))
   ","))

(define-syntax string-match?
  (lambda (x)
    (syntax-case x ()
      ((_ str pat) (string? (syntax->datum #'pat))
       (let ((p (syntax->datum #'pat)))
         #`(let ((s str))
             (and
              (= (string-length s) #,(string-length p))
              #,@(let lp ((i 0) (tests '()))
                   (if (< i (string-length p))
                       (let ((c (string-ref p i)))
                         (lp (1+ i)
                             (case c
                               ((#\.)   ; Whatever.
                                tests)
                               ((#\d)   ; Digit.
                                (cons #`(char-numeric? (string-ref s #,i))
                                      tests))
                               ((#\a)   ; Alphabetic.
                                (cons #`(char-alphabetic? (string-ref s #,i))
                                      tests))
                               (else    ; Literal.
                                (cons #`(eqv? (string-ref s #,i) #,c)
                                      tests)))))
                       tests)))))))))

;; "Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun"
;; "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec"

(define (parse-month str start end)
  (define (bad)
    (bad-header-component 'month (substring str start end)))
  (if (not (= (- end start) 3))
      (bad)
      (let ((a (string-ref str (+ start 0)))
            (b (string-ref str (+ start 1)))
            (c (string-ref str (+ start 2))))
        (case a
          ((#\J)
           (case b
             ((#\a) (case c ((#\n) 1) (else (bad))))
             ((#\u) (case c ((#\n) 6) ((#\l) 7) (else (bad))))
             (else (bad))))
          ((#\F)
           (case b
             ((#\e) (case c ((#\b) 2) (else (bad))))
             (else (bad))))
          ((#\M)
           (case b
             ((#\a) (case c ((#\r) 3) ((#\y) 5) (else (bad))))
             (else (bad))))
          ((#\A)
           (case b
             ((#\p) (case c ((#\r) 4) (else (bad))))
             ((#\u) (case c ((#\g) 8) (else (bad))))
             (else (bad))))
          ((#\S)
           (case b
             ((#\e) (case c ((#\p) 9) (else (bad))))
             (else (bad))))
          ((#\O)
           (case b
             ((#\c) (case c ((#\t) 10) (else (bad))))
             (else (bad))))
          ((#\N)
           (case b
             ((#\o) (case c ((#\v) 11) (else (bad))))
             (else (bad))))
          ((#\D)
           (case b
             ((#\e) (case c ((#\c) 12) (else (bad))))
             (else (bad))))
          (else (bad))))))

;; "GMT" | "+" 4DIGIT | "-" 4DIGIT
;;
;; RFC 2616 requires date values to use "GMT", but recommends accepting
;; the others as they are commonly generated by e.g. RFC 822 sources.
(define (parse-zone-offset str start)
  (let ((s (substring str start)))
    (define (bad)
      (bad-header-component 'zone-offset s))
    (cond
     ((string=? s "GMT")
      0)
     ((string=? s "UTC")
      0)
     ((string-match? s ".dddd")
      (let ((sign (case (string-ref s 0)
                    ((#\+) +1)
                    ((#\-) -1)
                    (else (bad))))
            (hours (parse-non-negative-integer s 1 3))
            (minutes (parse-non-negative-integer s 3 5)))
        (* sign 60 (+ (* 60 hours) minutes)))) ; seconds east of Greenwich
     (else (bad)))))

;; RFC 822, updated by RFC 1123
;; 
;; Sun, 06 Nov 1994 08:49:37 GMT
;; 01234567890123456789012345678
;; 0         1         2
(define (parse-rfc-822-date str space zone-offset)
  ;; We could verify the day of the week but we don't.
  (cond ((string-match? (substring str 0 space) "aaa, dd aaa dddd dd:dd:dd")
         (let ((date (parse-non-negative-integer str 5 7))
               (month (parse-month str 8 11))
               (year (parse-non-negative-integer str 12 16))
               (hour (parse-non-negative-integer str 17 19))
               (minute (parse-non-negative-integer str 20 22))
               (second (parse-non-negative-integer str 23 25)))
           (make-date 0 second minute hour date month year zone-offset)))
        ((string-match? (substring str 0 space) "aaa, d aaa dddd dd:dd:dd")
         (let ((date (parse-non-negative-integer str 5 6))
               (month (parse-month str 7 10))
               (year (parse-non-negative-integer str 11 15))
               (hour (parse-non-negative-integer str 16 18))
               (minute (parse-non-negative-integer str 19 21))
               (second (parse-non-negative-integer str 22 24)))
           (make-date 0 second minute hour date month year zone-offset)))
        (else
         (bad-header 'date str)         ; prevent tail call
         #f)))

;; RFC 850, updated by RFC 1036
;; Sunday, 06-Nov-94 08:49:37 GMT
;;        0123456789012345678901
;;        0         1         2
(define (parse-rfc-850-date str comma space zone-offset)
  ;; We could verify the day of the week but we don't.
  (let ((tail (substring str (1+ comma) space)))
    (if (not (string-match? tail " dd-aaa-dd dd:dd:dd"))
        (bad-header 'date str))
    (let ((date (parse-non-negative-integer tail 1 3))
          (month (parse-month tail 4 7))
          (year (parse-non-negative-integer tail 8 10))
          (hour (parse-non-negative-integer tail 11 13))
          (minute (parse-non-negative-integer tail 14 16))
          (second (parse-non-negative-integer tail 17 19)))
      (make-date 0 second minute hour date month
                 (let* ((now (date-year (current-date)))
                        (then (+ now year (- (modulo now 100)))))
                   (cond ((< (+ then 50) now) (+ then 100))
                         ((< (+ now 50) then) (- then 100))
                         (else then)))
                 zone-offset))))

;; ANSI C's asctime() format
;; Sun Nov  6 08:49:37 1994
;; 012345678901234567890123
;; 0         1         2
(define (parse-asctime-date str)
  (if (not (string-match? str "aaa aaa .d dd:dd:dd dddd"))
      (bad-header 'date str))
  (let ((date (parse-non-negative-integer
               str
               (if (eqv? (string-ref str 8) #\space) 9 8)
               10))
        (month (parse-month str 4 7))
        (year (parse-non-negative-integer str 20 24))
        (hour (parse-non-negative-integer str 11 13))
        (minute (parse-non-negative-integer str 14 16))
        (second (parse-non-negative-integer str 17 19)))
    (make-date 0 second minute hour date month year 0)))

;; Convert all date values to GMT time zone, as per RFC 2616 appendix C.
(define (normalize-date date)
  (if (zero? (date-zone-offset date))
      date
      (time-utc->date (date->time-utc date) 0)))

(define (parse-date str)
  (let* ((space (string-rindex str #\space))
         (zone-offset (and space (false-if-exception
                                  (parse-zone-offset str (1+ space))))))
    (normalize-date
     (if zone-offset
         (let ((comma (string-index str #\,)))
           (cond ((not comma) (bad-header 'date str))
                 ((= comma 3) (parse-rfc-822-date str space zone-offset))
                 (else (parse-rfc-850-date str comma space zone-offset))))
         (parse-asctime-date str)))))

(define (write-date date port)
  (define (display-digits n digits port)
    (define zero (char->integer #\0))
    (let lp ((tens (expt 10 (1- digits))))
      (if (> tens 0)
          (begin
            (display (integer->char (+ zero (modulo (truncate/ n tens) 10)))
                    port)
            (lp (floor/ tens 10))))))
  (let ((date (if (zero? (date-zone-offset date))
                  date
                  (time-tai->date (date->time-tai date) 0))))
    (display (case (date-week-day date)
               ((0) "Sun, ") ((1) "Mon, ") ((2) "Tue, ")
               ((3) "Wed, ") ((4) "Thu, ") ((5) "Fri, ")
               ((6) "Sat, ") (else (error "bad date" date)))
             port)
    (display-digits (date-day date) 2 port)
    (display (case (date-month date)
               ((1)  " Jan ") ((2)  " Feb ") ((3)  " Mar ")
               ((4)  " Apr ") ((5)  " May ") ((6)  " Jun ")
               ((7)  " Jul ") ((8)  " Aug ") ((9)  " Sep ")
               ((10) " Oct ") ((11) " Nov ") ((12) " Dec ")
               (else (error "bad date" date)))
             port)
    (display-digits (date-year date) 4 port)
    (display #\space port)
    (display-digits (date-hour date) 2 port)
    (display #\: port)
    (display-digits (date-minute date) 2 port)
    (display #\: port)
    (display-digits (date-second date) 2 port)
    (display " GMT" port)))

(define (parse-entity-tag val)
  (if (string-prefix? "W/" val)
      (cons (parse-qstring val 2) #f)
      (cons (parse-qstring val) #t)))

(define (entity-tag? val)
  (and (pair? val)
       (string? (car val))))

(define (write-entity-tag val port)
  (if (not (cdr val))
      (display "W/" port))
  (write-qstring (car val) port))

(define* (parse-entity-tag-list val #:optional
                                (start 0) (end (string-length val)))
  (let ((strong? (not (string-prefix? "W/" val 0 2 start end))))
    (call-with-values (lambda ()
                        (parse-qstring val (if strong? start (+ start 2))
                                       end #:incremental? #t))
      (lambda (tag next)
        (acons tag strong?
               (let ((next (skip-whitespace val next end)))
                  (if (< next end)
                      (if (eqv? (string-ref val next) #\,)
                          (parse-entity-tag-list
                           val
                           (skip-whitespace val (1+ next) end)
                           end)
                          (bad-header-component 'entity-tag-list val))
                      '())))))))

(define (entity-tag-list? val)
  (list-of? val entity-tag?))

(define (write-entity-tag-list val port)
  (write-list val port write-entity-tag  ", "))

;; credentials = auth-scheme #auth-param
;; auth-scheme = token
;; auth-param = token "=" ( token | quoted-string )
;;
;; That's what the spec says. In reality the Basic scheme doesn't have
;; k-v pairs, just one auth token, so we give that token as a string.
;;
(define* (parse-credentials str #:optional (val-parser default-val-parser)
                            (start 0) (end (string-length str)))
  (let* ((start (skip-whitespace str start end))
         (delim (or (string-index str char-set:whitespace start end) end)))
    (if (= start end)
        (bad-header-component 'authorization str))
    (let ((scheme (string->symbol
                   (string-downcase (substring str start (or delim end))))))
      (case scheme
        ((basic)
         (let* ((start (skip-whitespace str delim end)))
           (if (< start end)
               (cons scheme (substring str start end))
               (bad-header-component 'credentials str))))
        (else
         (cons scheme (parse-key-value-list str default-val-parser delim end)))))))

(define (validate-credentials val)
  (and (pair? val) (symbol? (car val))
       (case (car val)
         ((basic) (string? (cdr val)))
         (else (key-value-list? (cdr val))))))

(define (write-credentials val port)
  (display (car val) port)
  (display #\space port)
  (case (car val)
    ((basic) (display (cdr val) port))
    (else (write-key-value-list (cdr val) port))))

;; challenges = 1#challenge
;; challenge = auth-scheme 1*SP 1#auth-param
;;
;; A pain to parse, as both challenges and auth params are delimited by
;; commas, and qstrings can contain anything. We rely on auth params
;; necessarily having "=" in them.
;;
(define* (parse-challenge str #:optional
                          (start 0) (end (string-length str)))
  (let* ((start (skip-whitespace str start end))
         (sp (string-index str #\space start end))
         (scheme (if sp
                     (string->symbol (string-downcase (substring str start sp)))
                     (bad-header-component 'challenge str))))
    (let lp ((i sp) (out (list scheme)))
      (if (not (< i end))
          (values (reverse! out) end)
          (let* ((i (skip-whitespace str i end))
                 (eq (string-index str #\= i end))
                 (comma (string-index str #\, i end))
                 (delim (min (or eq end) (or comma end)))
                 (token-end (trim-whitespace str i delim)))
            (if (string-index str #\space i token-end)
                (values (reverse! out) i)
                (let ((k (string->symbol (substring str i token-end))))
                  (call-with-values
                      (lambda ()
                        (if (and eq (or (not comma) (< eq comma)))
                            (let ((i (skip-whitespace str (1+ eq) end)))
                              (if (and (< i end) (eqv? (string-ref str i) #\"))
                                  (parse-qstring str i end #:incremental? #t)
                                  (values (substring
                                           str i
                                           (trim-whitespace str i
                                                            (or comma end)))
                                          (or comma end))))
                            (values #f delim)))
                    (lambda (v next-i)
                      (let ((i (skip-whitespace str next-i end)))
                        (if (or (= i end) (eqv? (string-ref str i) #\,))
                            (lp (1+ i) (cons (if v (cons k v) k) out))
                            (bad-header-component
                             'challenge
                             (substring str start end)))))))))))))

(define* (parse-challenges str #:optional (val-parser default-val-parser)
                           (start 0) (end (string-length str)))
  (let lp ((i start) (ret '()))
    (let ((i (skip-whitespace str i end)))
      (if (< i end)
          (call-with-values (lambda () (parse-challenge str i end))
            (lambda (challenge i)
              (lp i (cons challenge ret))))
          (reverse ret)))))

(define (validate-challenges val)
  (list-of? val (lambda (x)
                  (and (pair? x) (symbol? (car x))
                       (key-value-list? (cdr x))))))

(define (write-challenge val port)
  (display (car val) port)
  (display #\space port)
  (write-key-value-list (cdr val) port))

(define (write-challenges val port)
  (write-list val port write-challenge ", "))




;;;
;;; Request-Line and Response-Line
;;;

;; Hmm.
(define (bad-request message . args)
  (throw 'bad-request message args))
(define (bad-response message . args)
  (throw 'bad-response message args))

(define *known-versions* '())

(define* (parse-http-version str #:optional (start 0) (end (string-length str)))
  "Parse an HTTP version from STR, returning it as a major–minor
pair. For example, ‘HTTP/1.1’ parses as the pair of integers,
‘(1 . 1)’."
  (or (let lp ((known *known-versions*))
        (and (pair? known)
             (if (string= str (caar known) start end)
                 (cdar known)
                 (lp (cdr known)))))
      (let ((dot-idx (string-index str #\. start end)))
        (if (and (string-prefix? "HTTP/" str 0 5 start end)
                 dot-idx
                 (= dot-idx (string-rindex str #\. start end)))
            (cons (parse-non-negative-integer str (+ start 5) dot-idx)
                  (parse-non-negative-integer str (1+ dot-idx) end))
            (bad-header-component 'http-version (substring str start end))))))

(define (write-http-version val port)
  "Write the given major-minor version pair to PORT."
  (display "HTTP/" port)
  (display (car val) port)
  (display #\. port)
  (display (cdr val) port))

(for-each
 (lambda (v)
   (set! *known-versions*
         (acons v (parse-http-version v 0 (string-length v))
                *known-versions*)))
 '("HTTP/1.0" "HTTP/1.1"))


;; Request-URI = "*" | absoluteURI | abs_path | authority
;;
;; The `authority' form is only permissible for the CONNECT method, so
;; because we don't expect people to implement CONNECT, we save
;; ourselves the trouble of that case, and disallow the CONNECT method.
;;
(define* (parse-http-method str #:optional (start 0) (end (string-length str)))
  "Parse an HTTP method from STR.  The result is an upper-case
symbol, like ‘GET’."
  (cond
   ((string= str "GET" start end) 'GET)
   ((string= str "HEAD" start end) 'HEAD)
   ((string= str "POST" start end) 'POST)
   ((string= str "PUT" start end) 'PUT)
   ((string= str "DELETE" start end) 'DELETE)
   ((string= str "OPTIONS" start end) 'OPTIONS)
   ((string= str "TRACE" start end) 'TRACE)
   (else (bad-request "Invalid method: ~a" (substring str start end)))))

(define* (parse-request-uri str #:optional (start 0) (end (string-length str)))
  "Parse a URI from an HTTP request line.  Note that URIs in requests do
not have to have a scheme or host name.  The result is a URI object."
  (cond
   ((= start end)
    (bad-request "Missing Request-URI"))
   ((string= str "*" start end)
    #f)
   ((eq? (string-ref str start) #\/)
    (let* ((q (string-index str #\? start end))
           (f (string-index str #\# start end))
           (q (and q (or (not f) (< q f)) q)))
      (build-uri 'http
                 #:path (substring str start (or q f end))
                 #:query (and q (substring str (1+ q) (or f end)))
                 #:fragment (and f (substring str (1+ f) end)))))
   (else
    (or (string->uri (substring str start end))
        (bad-request "Invalid URI: ~a" (substring str start end))))))

(define (read-request-line port)
  "Read the first line of an HTTP request from PORT, returning
three values: the method, the URI, and the version."
  (let* ((line (read-line* port))
         (d0 (string-index line char-set:whitespace)) ; "delimiter zero"
         (d1 (string-rindex line char-set:whitespace)))
    (if (and d0 d1 (< d0 d1))
        (values (parse-http-method line 0 d0)
                (parse-request-uri line (skip-whitespace line (1+ d0) d1) d1)
                (parse-http-version line (1+ d1) (string-length line)))
        (bad-request "Bad Request-Line: ~s" line))))

(define (write-uri uri port)
  (if (uri-host uri)
      (begin
        (display (uri-scheme uri) port)
        (display "://" port)
        (if (uri-userinfo uri)
            (begin
              (display (uri-userinfo uri) port)
              (display #\@ port)))
        (display (uri-host uri) port)
        (let ((p (uri-port uri)))
          (if (and p (not (eqv? p 80)))
              (begin
                (display #\: port)
                (display p port))))))
  (let* ((path (uri-path uri))
         (len (string-length path)))
    (cond
     ((and (> len 0) (not (eqv? (string-ref path 0) #\/)))
      (bad-request "Non-absolute URI path: ~s" path))
     ((and (zero? len) (not (uri-host uri)))
      (bad-request "Empty path and no host for URI: ~s" uri))
     (else
      (display path port))))
  (if (uri-query uri)
      (begin
        (display #\? port)
        (display (uri-query uri) port))))

(define (write-request-line method uri version port)
  "Write the first line of an HTTP request to PORT."
  (display method port)
  (display #\space port)
  (when (http-proxy-port? port)
    (let ((scheme (uri-scheme uri))
          (host (uri-host uri))
          (host-port (uri-port uri)))
      (when (and scheme host)
        (display scheme port)
        (display "://" port)
        (if (string-index host #\:)
            (begin (display #\[ port)
                   (display host port)
                   (display #\] port))
            (display host port))
        (unless ((@@ (web uri) default-port?) scheme host-port)
          (display #\: port)
          (display host-port port)))))
  (let ((path (uri-path uri))
        (query (uri-query uri)))
    (if (string-null? path)
        (display "/" port)
        (display path port))
    (if query
        (begin
          (display "?" port)
          (display query port))))
  (display #\space port)
  (write-http-version version port)
  (display "\r\n" port))

(define (read-response-line port)
  "Read the first line of an HTTP response from PORT, returning
three values: the HTTP version, the response code, and the \"reason
phrase\"."
  (let* ((line (read-line* port))
         (d0 (string-index line char-set:whitespace)) ; "delimiter zero"
         (d1 (and d0 (string-index line char-set:whitespace
                                   (skip-whitespace line d0)))))
    (if (and d0 d1)
        (values (parse-http-version line 0 d0)
                (parse-non-negative-integer line (skip-whitespace line d0 d1)
                                            d1)
                (string-trim-both line char-set:whitespace d1))
        (bad-response "Bad Response-Line: ~s" line))))

(define (write-response-line version code reason-phrase port)
  "Write the first line of an HTTP response to PORT."
  (write-http-version version port)
  (display #\space port)
  (display code port)
  (display #\space port)
  (display reason-phrase port)
  (display "\r\n" port))




;;;
;;; Helpers for declaring headers
;;;

;; emacs: (put 'declare-header! 'scheme-indent-function 1)
;; emacs: (put 'declare-opaque!-header 'scheme-indent-function 1)
(define (declare-opaque-header! name)
  "Declares a given header as \"opaque\", meaning that its value is not
treated specially, and is just returned as a plain string."
  (declare-header! name
    parse-opaque-string validate-opaque-string write-opaque-string))

;; emacs: (put 'declare-date-header! 'scheme-indent-function 1)
(define (declare-date-header! name)
  (declare-header! name
    parse-date date? write-date))

;; emacs: (put 'declare-string-list-header! 'scheme-indent-function 1)
(define (declare-string-list-header! name)
  (declare-header! name
    split-and-trim list-of-strings? write-list-of-strings))

;; emacs: (put 'declare-symbol-list-header! 'scheme-indent-function 1)
(define (declare-symbol-list-header! name)
  (declare-header! name
    (lambda (str)
      (map string->symbol (split-and-trim str)))
    (lambda (v)
      (list-of? v symbol?))
    (lambda (v port)
      (write-list v port display ", "))))

;; emacs: (put 'declare-header-list-header! 'scheme-indent-function 1)
(define (declare-header-list-header! name)
  (declare-header! name
    split-header-names list-of-header-names? write-header-list))

;; emacs: (put 'declare-integer-header! 'scheme-indent-function 1)
(define (declare-integer-header! name)
  (declare-header! name
    parse-non-negative-integer non-negative-integer? display))

;; emacs: (put 'declare-uri-header! 'scheme-indent-function 1)
(define (declare-uri-header! name)
  (declare-header! name
    (lambda (str) (or (string->uri str) (bad-header-component 'uri str)))
    (@@ (web uri) absolute-uri?)
    write-uri))

;; emacs: (put 'declare-relative-uri-header! 'scheme-indent-function 1)
(define (declare-relative-uri-header! name)
  (declare-header! name
    (lambda (str)
      (or ((@@ (web uri) string->uri*) str)
          (bad-header-component 'uri str)))
    uri?
    write-uri))

;; emacs: (put 'declare-quality-list-header! 'scheme-indent-function 1)
(define (declare-quality-list-header! name)
  (declare-header! name
    parse-quality-list validate-quality-list write-quality-list))

;; emacs: (put 'declare-param-list-header! 'scheme-indent-function 1)
(define* (declare-param-list-header! name #:optional
                                     (val-parser default-val-parser)
                                     (val-validator default-val-validator)
                                     (val-writer default-val-writer))
  (declare-header! name
    (lambda (str) (parse-param-list str val-parser))
    (lambda (val) (validate-param-list val val-validator))
    (lambda (val port) (write-param-list val port val-writer))))

;; emacs: (put 'declare-key-value-list-header! 'scheme-indent-function 1)
(define* (declare-key-value-list-header! name #:optional
                                         (val-parser default-val-parser)
                                         (val-validator default-val-validator)
                                         (val-writer default-val-writer))
  (declare-header! name
    (lambda (str) (parse-key-value-list str val-parser))
    (lambda (val) (key-value-list? val val-validator))
    (lambda (val port) (write-key-value-list val port val-writer))))

;; emacs: (put 'declare-entity-tag-list-header! 'scheme-indent-function 1)
(define (declare-entity-tag-list-header! name)
  (declare-header! name
    (lambda (str) (if (string=? str "*") '* (parse-entity-tag-list str)))
    (lambda (val) (or (eq? val '*) (entity-tag-list? val)))
    (lambda (val port)
      (if (eq? val '*)
          (display "*" port)
          (write-entity-tag-list val port)))))

;; emacs: (put 'declare-credentials-header! 'scheme-indent-function 1)
(define (declare-credentials-header! name)
  (declare-header! name
    parse-credentials validate-credentials write-credentials))

;; emacs: (put 'declare-challenge-list-header! 'scheme-indent-function 1)
(define (declare-challenge-list-header! name)
  (declare-header! name
    parse-challenges validate-challenges write-challenges))




;;;
;;; General headers
;;;

;; Cache-Control   = 1#(cache-directive)
;; cache-directive = cache-request-directive | cache-response-directive
;; cache-request-directive =
;;        "no-cache"                          ; Section 14.9.1
;;      | "no-store"                          ; Section 14.9.2
;;      | "max-age" "=" delta-seconds         ; Section 14.9.3, 14.9.4
;;      | "max-stale" [ "=" delta-seconds ]   ; Section 14.9.3
;;      | "min-fresh" "=" delta-seconds       ; Section 14.9.3
;;      | "no-transform"                      ; Section 14.9.5
;;      | "only-if-cached"                    ; Section 14.9.4
;;      | cache-extension                     ; Section 14.9.6
;;  cache-response-directive =
;;        "public"                               ; Section 14.9.1
;;      | "private" [ "=" <"> 1#field-name <"> ] ; Section 14.9.1
;;      | "no-cache" [ "=" <"> 1#field-name <"> ]; Section 14.9.1
;;      | "no-store"                             ; Section 14.9.2
;;      | "no-transform"                         ; Section 14.9.5
;;      | "must-revalidate"                      ; Section 14.9.4
;;      | "proxy-revalidate"                     ; Section 14.9.4
;;      | "max-age" "=" delta-seconds            ; Section 14.9.3
;;      | "s-maxage" "=" delta-seconds           ; Section 14.9.3
;;      | cache-extension                        ; Section 14.9.6
;; cache-extension = token [ "=" ( token | quoted-string ) ]
;;
(declare-key-value-list-header! "Cache-Control"
  (lambda (k v-str)
    (case k
      ((max-age min-fresh s-maxage)
       (parse-non-negative-integer v-str))
      ((max-stale)
       (and v-str (parse-non-negative-integer v-str)))
      ((private no-cache)
       (and v-str (split-header-names v-str)))
      (else v-str)))
  (lambda (k v)
    (case k
      ((max-age min-fresh s-maxage)
       (non-negative-integer? v))
      ((max-stale)
       (or (not v) (non-negative-integer? v)))
      ((private no-cache)
       (or (not v) (list-of-header-names? v)))
      ((no-store no-transform only-if-cache must-revalidate proxy-revalidate)
       (not v))
      (else
       (or (not v) (string? v)))))
  (lambda (k v port)
    (cond
     ((string? v) (default-val-writer k v port))
     ((pair? v)
      (display #\" port)
      (write-header-list v port)
      (display #\" port))
     ((integer? v)
      (display v port))
     (else
      (bad-header-component 'cache-control v)))))

;; Connection = "Connection" ":" 1#(connection-token)
;; connection-token  = token
;; e.g.
;;     Connection: close, Foo-Header
;; 
(declare-header! "Connection"
  split-header-names
  list-of-header-names?
  (lambda (val port)
    (write-list val port
                (lambda (x port)
                  (display (if (eq? x 'close)
                               "close"
                               (header->string x))
                           port))
                ", ")))

;; Date  = "Date" ":" HTTP-date
;; e.g.
;;     Date: Tue, 15 Nov 1994 08:12:31 GMT
;;
(declare-date-header! "Date")

;; Pragma            = "Pragma" ":" 1#pragma-directive
;; pragma-directive  = "no-cache" | extension-pragma
;; extension-pragma  = token [ "=" ( token | quoted-string ) ]
;;
(declare-key-value-list-header! "Pragma")

;; Trailer  = "Trailer" ":" 1#field-name
;;
(declare-header-list-header! "Trailer")

;; Transfer-Encoding = "Transfer-Encoding" ":" 1#transfer-coding
;;
(declare-param-list-header! "Transfer-Encoding")

;; Upgrade = "Upgrade" ":" 1#product
;;
(declare-string-list-header! "Upgrade")

;; Via =  "Via" ":" 1#( received-protocol received-by [ comment ] )
;; received-protocol = [ protocol-name "/" ] protocol-version
;; protocol-name     = token
;; protocol-version  = token
;; received-by       = ( host [ ":" port ] ) | pseudonym
;; pseudonym         = token
;;
(declare-header! "Via"
  split-and-trim
  list-of-strings?
  write-list-of-strings
  #:multiple? #t)

;; Warning    = "Warning" ":" 1#warning-value
;;
;; warning-value = warn-code SP warn-agent SP warn-text
;;                                       [SP warn-date]
;;
;; warn-code  = 3DIGIT
;; warn-agent = ( host [ ":" port ] ) | pseudonym
;;                 ; the name or pseudonym of the server adding
;;                 ; the Warning header, for use in debugging
;; warn-text  = quoted-string
;; warn-date  = <"> HTTP-date <">
(declare-header! "Warning"
  (lambda (str)
    (let ((len (string-length str)))
      (let lp ((i (skip-whitespace str 0)))
        (let* ((idx1 (string-index str #\space i))
               (idx2 (string-index str #\space (1+ idx1))))
          (if (and idx1 idx2)
              (let ((code (parse-non-negative-integer str i idx1))
                    (agent (substring str (1+ idx1) idx2)))
                (call-with-values
                    (lambda () (parse-qstring str (1+ idx2) #:incremental? #t))
                  (lambda (text i)
                    (call-with-values
                        (lambda ()
                          (let ((c (and (< i len) (string-ref str i))))
                            (case c
                              ((#\space)
                               ;; we have a date.
                               (call-with-values
                                   (lambda () (parse-qstring str (1+ i)
                                                             #:incremental? #t))
                                 (lambda (date i)
                                   (values text (parse-date date) i))))
                              (else
                               (values text #f i)))))
                      (lambda (text date i)
                        (let ((w (list code agent text date))
                              (c (and (< i len) (string-ref str i))))
                          (case c
                            ((#f) (list w))
                            ((#\,) (cons w (lp (skip-whitespace str (1+ i)))))
                            (else (bad-header 'warning str))))))))))))))
  (lambda (val)
    (list-of? val
              (lambda (elt)
                (and (list? elt)
                     (= (length elt) 4)
                     (apply (lambda (code host text date)
                              (and (non-negative-integer? code) (< code 1000)
                                   (string? host)
                                   (string? text)
                                   (or (not date) (date? date))))
                            elt)))))
  (lambda (val port)
    (write-list
     val port
     (lambda (w port)
       (apply
        (lambda (code host text date)
          (display code port)
          (display #\space port)
          (display host port)
          (display #\space port)
          (write-qstring text port)
          (if date
              (begin
                (display #\space port)
                (write-date date port))))
        w))
     ", "))
  #:multiple? #t)




;;;
;;; Entity headers
;;;

;; Allow = #Method
;;
(declare-symbol-list-header! "Allow")

;; Content-Disposition = disposition-type *( ";" disposition-parm )
;; disposition-type = "attachment" | disp-extension-token
;; disposition-parm = filename-parm | disp-extension-parm
;; filename-parm = "filename" "=" quoted-string
;; disp-extension-token = token
;; disp-extension-parm = token "=" ( token | quoted-string )
;;
(declare-header! "Content-Disposition"
  (lambda (str)
    (let ((disposition (parse-param-list str default-val-parser)))
      ;; Lazily reuse the param list parser.
      (unless (and (pair? disposition)
                   (null? (cdr disposition)))
        (bad-header-component 'content-disposition str))
      (car disposition)))
  (lambda (val)
    (and (pair? val)
         (symbol? (car val))
         (list-of? (cdr val)
                   (lambda (x)
                     (and (pair? x) (symbol? (car x)) (string? (cdr x)))))))
  (lambda (val port)
    (write-param-list (list val) port)))

;; Content-Encoding = 1#content-coding
;;
(declare-symbol-list-header! "Content-Encoding")

;; Content-Language = 1#language-tag
;;
(declare-string-list-header! "Content-Language")

;; Content-Length = 1*DIGIT
;;
(declare-integer-header! "Content-Length")

;; Content-Location = ( absoluteURI | relativeURI )
;;
(declare-relative-uri-header! "Content-Location")

;; Content-MD5 = <base64 of 128 bit MD5 digest as per RFC 1864>
;;
(declare-opaque-header! "Content-MD5")

;; Content-Range = content-range-spec
;; content-range-spec      = byte-content-range-spec
;; byte-content-range-spec = bytes-unit SP
;;                           byte-range-resp-spec "/"
;;                           ( instance-length | "*" )
;; byte-range-resp-spec = (first-byte-pos "-" last-byte-pos)
;;                                | "*"
;; instance-length           = 1*DIGIT
;;
(declare-header! "Content-Range"
  (lambda (str)
    (let ((dash (string-index str #\-))
          (slash (string-index str #\/)))
      (if (and (string-prefix? "bytes " str) slash)
          (list 'bytes
                (cond
                 (dash
                  (cons
                   (parse-non-negative-integer str 6 dash)
                   (parse-non-negative-integer str (1+ dash) slash)))
                 ((string= str "*" 6 slash)
                  '*)
                 (else
                  (bad-header 'content-range str)))
                (if (string= str "*" (1+ slash))
                    '*
                    (parse-non-negative-integer str (1+ slash))))
          (bad-header 'content-range str))))
  (lambda (val)
    (and (list? val) (= (length val) 3)
         (symbol? (car val))
         (let ((x (cadr val)))
           (or (eq? x '*)
               (and (pair? x)
                    (non-negative-integer? (car x))
                    (non-negative-integer? (cdr x)))))
         (let ((x (caddr val)))
           (or (eq? x '*)
               (non-negative-integer? x)))))
  (lambda (val port)
    (display (car val) port)
    (display #\space port)
    (if (eq? (cadr val) '*)
        (display #\* port)
        (begin
          (display (caadr val) port)
          (display #\- port)
          (display (caadr val) port)))
    (if (eq? (caddr val) '*)
        (display #\* port)
        (display (caddr val) port))))

;; Content-Type = media-type
;;
(declare-header! "Content-Type"
  (lambda (str)
    (let ((parts (string-split str #\;)))
      (cons (parse-media-type (car parts))
            (map (lambda (x)
                   (let ((eq (string-index x #\=)))
                     (if (and eq (= eq (string-rindex x #\=)))
                         (cons
                          (string->symbol
                           (string-trim x char-set:whitespace 0 eq))
                          (string-trim-right x char-set:whitespace (1+ eq)))
                         (bad-header 'content-type str))))
                 (cdr parts)))))
  (lambda (val)
    (and (pair? val)
         (symbol? (car val))
         (list-of? (cdr val)
                   (lambda (x)
                     (and (pair? x) (symbol? (car x)) (string? (cdr x)))))))
  (lambda (val port)
    (display (car val) port)
    (if (pair? (cdr val)) 
       (begin
          (display ";" port)
          (write-list
           (cdr val) port
           (lambda (pair port)
             (display (car pair) port)
             (display #\= port)
             (display (cdr pair) port))
           ";")))))

;; Expires = HTTP-date
;;
(define *date-in-the-past* (parse-date "Thu, 01 Jan 1970 00:00:00 GMT"))

(declare-header! "Expires"
  (lambda (str)
    (if (member str '("0" "-1"))
        *date-in-the-past*
        (parse-date str)))
  date?
  write-date)

;; Last-Modified = HTTP-date
;;
(declare-date-header! "Last-Modified")




;;;
;;; Request headers
;;;

;; Accept = #( media-range [ accept-params ] )
;; media-range = ( "*/*" | ( type "/" "*" ) | ( type "/" subtype ) )
;;               *( ";" parameter )
;; accept-params = ";" "q" "=" qvalue *( accept-extension )
;; accept-extension = ";" token [ "=" ( token | quoted-string ) ]
;;
(declare-param-list-header! "Accept"
  ;; -> (type/subtype (sym-prop . str-val) ...) ...)
  ;;
  ;; with the exception of prop `q', in which case the val will be a
  ;; valid quality value
  ;;
  (lambda (k v)
    (if (eq? k 'q) 
        (parse-quality v)
        v))
  (lambda (k v)
    (if (eq? k 'q)
        (valid-quality? v)
        (or (not v) (string? v))))
  (lambda (k v port)
    (if (eq? k 'q)
        (write-quality v port)
        (default-val-writer k v port))))

;; Accept-Charset = 1#( ( charset | "*" )[ ";" "q" "=" qvalue ] )
;;
(declare-quality-list-header! "Accept-Charset")

;; Accept-Encoding = 1#( codings [ ";" "q" "=" qvalue ] )
;; codings = ( content-coding | "*" )
;;
(declare-quality-list-header! "Accept-Encoding")

;; Accept-Language = 1#( language-range [ ";" "q" "=" qvalue ] )
;; language-range  = ( ( 1*8ALPHA *( "-" 1*8ALPHA ) ) | "*" )
;;
(declare-quality-list-header! "Accept-Language")

;; Authorization = credentials
;; credentials = auth-scheme #auth-param
;; auth-scheme = token
;; auth-param = token "=" ( token | quoted-string )
;;
(declare-credentials-header! "Authorization")

;; Expect = 1#expectation
;; expectation = "100-continue" | expectation-extension
;; expectation-extension = token [ "=" ( token | quoted-string )
;;                         *expect-params ]
;; expect-params = ";" token [ "=" ( token | quoted-string ) ]
;;
(declare-param-list-header! "Expect")

;; From = mailbox
;;
;; Should be an email address; we just pass on the string as-is.
;;
(declare-opaque-header! "From")

;; Host = host [ ":" port ]
;; 
(declare-header! "Host"
  (lambda (str)
    (let* ((rbracket (string-index str #\]))
           (colon (string-index str #\: (or rbracket 0)))
           (host (cond
                  (rbracket
                   (unless (eqv? (string-ref str 0) #\[)
                     (bad-header 'host str))
                   (substring str 1 rbracket))
                  (colon
                   (substring str 0 colon))
                  (else
                   str)))
           (port (and colon
                      (parse-non-negative-integer str (1+ colon)))))
      (cons host port)))
  (lambda (val)
    (and (pair? val)
         (string? (car val))
         (or (not (cdr val))
             (non-negative-integer? (cdr val)))))
  (lambda (val port)
    (if (string-index (car val) #\:)
        (begin
          (display #\[ port)
          (display (car val) port)
          (display #\] port))
        (display (car val) port))
    (if (cdr val)
        (begin
          (display #\: port)
          (display (cdr val) port)))))

;; If-Match = ( "*" | 1#entity-tag )
;;
(declare-entity-tag-list-header! "If-Match")

;; If-Modified-Since = HTTP-date
;;
(declare-date-header! "If-Modified-Since")

;; If-None-Match = ( "*" | 1#entity-tag )
;;
(declare-entity-tag-list-header! "If-None-Match")

;; If-Range = ( entity-tag | HTTP-date )
;;
(declare-header! "If-Range"
  (lambda (str)
    (if (or (string-prefix? "\"" str)
            (string-prefix? "W/" str))
        (parse-entity-tag str)
        (parse-date str)))
  (lambda (val)
    (or (date? val) (entity-tag? val)))
  (lambda (val port)
    (if (date? val)
        (write-date val port)
        (write-entity-tag val port))))

;; If-Unmodified-Since = HTTP-date
;;
(declare-date-header! "If-Unmodified-Since")

;; Max-Forwards = 1*DIGIT
;;
(declare-integer-header! "Max-Forwards")

;; Proxy-Authorization = credentials
;;
(declare-credentials-header! "Proxy-Authorization")

;; Range = "Range" ":" ranges-specifier
;; ranges-specifier = byte-ranges-specifier
;; byte-ranges-specifier = bytes-unit "=" byte-range-set
;; byte-range-set = 1#( byte-range-spec | suffix-byte-range-spec )
;; byte-range-spec = first-byte-pos "-" [last-byte-pos]
;; first-byte-pos = 1*DIGIT
;; last-byte-pos = 1*DIGIT
;; suffix-byte-range-spec = "-" suffix-length
;; suffix-length = 1*DIGIT
;;
(declare-header! "Range"
  (lambda (str)
    (if (string-prefix? "bytes=" str)
        (cons
         'bytes
         (map (lambda (x)
                (let ((dash (string-index x #\-)))
                  (cond
                   ((not dash)
                    (bad-header 'range str))
                   ((zero? dash)
                    (cons #f (parse-non-negative-integer x 1)))
                   ((= dash (1- (string-length x)))
                    (cons (parse-non-negative-integer x 0 dash) #f))
                   (else
                    (cons (parse-non-negative-integer x 0 dash)
                          (parse-non-negative-integer x (1+ dash)))))))
              (string-split (substring str 6) #\,)))
        (bad-header 'range str)))
  (lambda (val)
    (and (pair? val)
         (symbol? (car val))
         (list-of? (cdr val)
                   (lambda (elt)
                     (and (pair? elt)
                          (let ((x (car elt)) (y (cdr elt)))
                            (and (or x y)
                                 (or (not x) (non-negative-integer? x))
                                 (or (not y) (non-negative-integer? y)))))))))
  (lambda (val port)
    (display (car val) port)
    (display #\= port)
    (write-list
     (cdr val) port
     (lambda (pair port)
       (if (car pair)
           (display (car pair) port))
       (display #\- port)
       (if (cdr pair)
           (display (cdr pair) port)))
     ",")))

;; Referer = ( absoluteURI | relativeURI )
;;
(declare-relative-uri-header! "Referer")

;; TE = #( t-codings )
;; t-codings = "trailers" | ( transfer-extension [ accept-params ] )
;;
(declare-param-list-header! "TE")

;; User-Agent = 1*( product | comment )
;;
(declare-opaque-header! "User-Agent")




;;;
;;; Reponse headers
;;;

;; Accept-Ranges = acceptable-ranges
;; acceptable-ranges = 1#range-unit | "none"
;;
(declare-symbol-list-header! "Accept-Ranges")

;; Age = age-value
;; age-value = delta-seconds
;;
(declare-integer-header! "Age")

;; ETag = entity-tag
;;
(declare-header! "ETag"
  parse-entity-tag
  entity-tag?
  write-entity-tag)

;; Location = absoluteURI
;; 
(declare-uri-header! "Location")

;; Proxy-Authenticate = 1#challenge
;;
(declare-challenge-list-header! "Proxy-Authenticate")

;; Retry-After  = ( HTTP-date | delta-seconds )
;;
(declare-header! "Retry-After"
  (lambda (str)
    (if (and (not (string-null? str))
             (char-numeric? (string-ref str 0)))
        (parse-non-negative-integer str)
        (parse-date str)))
  (lambda (val)
    (or (date? val) (non-negative-integer? val)))
  (lambda (val port)
    (if (date? val)
        (write-date val port)
        (display val port))))

;; Server = 1*( product | comment )
;;
(declare-opaque-header! "Server")

;; Vary = ( "*" | 1#field-name )
;;
(declare-header! "Vary"
  (lambda (str)
    (if (equal? str "*")
        '*
        (split-header-names str)))
  (lambda (val)
    (or (eq? val '*) (list-of-header-names? val)))
  (lambda (val port)
    (if (eq? val '*)
        (display "*" port)
        (write-header-list val port))))

;; WWW-Authenticate = 1#challenge
;;
(declare-challenge-list-header! "WWW-Authenticate")


;; Chunked Responses
(define (read-chunk-header port)
  (let* ((str (read-line port))
         (extension-start (string-index str (lambda (c) (or (char=? c #\;)
                                                       (char=? c #\return)))))
         (size (string->number (if extension-start ; unnecessary?
                                   (substring str 0 extension-start)
                                   str)
                               16)))
    size))

(define (read-chunk port)
  (let ((size (read-chunk-header port)))
    (read-chunk-body port size)))

(define (read-chunk-body port size)
  (let ((bv (get-bytevector-n port size)))
    (get-u8 port)                       ; CR
    (get-u8 port)                       ; LF
    bv))

(define* (make-chunked-input-port port #:key (keep-alive? #f))
  "Returns a new port which translates HTTP chunked transfer encoded
data from PORT into a non-encoded format. Returns eof when it has
read the final chunk from PORT. This does not necessarily mean
that there is no more data on PORT. When the returned port is
closed it will also close PORT, unless the KEEP-ALIVE? is true."
  (define (next-chunk)
    (read-chunk port))
  (define finished? #f)
  (define (close)
    (unless keep-alive?
      (close-port port)))
  (define buffer #vu8())
  (define buffer-size 0)
  (define buffer-pointer 0)
  (define (read! bv idx to-read)
    (define (loop to-read num-read)
      (cond ((or finished? (zero? to-read))
             num-read)
            ((<= to-read (- buffer-size buffer-pointer))
             (bytevector-copy! buffer buffer-pointer
                               bv (+ idx num-read)
                               to-read)
             (set! buffer-pointer (+ buffer-pointer to-read))
             (loop 0 (+ num-read to-read)))
            (else
             (let ((n (- buffer-size buffer-pointer)))
               (bytevector-copy! buffer buffer-pointer
                                 bv (+ idx num-read)
                                 n)
               (set! buffer (next-chunk))
               (set! buffer-pointer 0)
               (set! buffer-size (bytevector-length buffer))
               (set! finished? (= buffer-size 0))
               (loop (- to-read n)
                     (+ num-read n))))))
    (loop to-read 0))
  (make-custom-binary-input-port "chunked input port" read! #f #f close))

(define* (make-chunked-output-port port #:key (keep-alive? #f))
  "Returns a new port which translates non-encoded data into a HTTP
chunked transfer encoded data and writes this to PORT. Data
written to this port is buffered until the port is flushed, at which
point it is all sent as one chunk. Take care to close the port when
done, as it will output the remaining data, and encode the final zero
chunk. When the port is closed it will also close PORT, unless
KEEP-ALIVE? is true."
  (define (q-for-each f q)
    (while (not (q-empty? q))
      (f (deq! q))))
  (define queue (make-q))
  (define (put-char c)
    (enq! queue c))
  (define (put-string s)
    (string-for-each (lambda (c) (enq! queue c))
                     s))
  (define (flush)
    ;; It is important that we do _not_ write a chunk if the queue is
    ;; empty, since it will be treated as the final chunk.
    (unless (q-empty? queue)
      (let ((len (q-length queue)))
        (display (number->string len 16) port)
        (display "\r\n" port)
        (q-for-each (lambda (elem) (write-char elem port))
                    queue)
        (display "\r\n" port))))
  (define (close)
    (flush)
    (display "0\r\n" port)
    (force-output port)
    (unless keep-alive?
      (close-port port)))
  (make-soft-port (vector put-char put-string flush #f close) "w"))

(define %http-proxy-port? (make-object-property))
(define (http-proxy-port? port) (%http-proxy-port? port))
(define (set-http-proxy-port?! port flag)
  (set! (%http-proxy-port? port) flag))
