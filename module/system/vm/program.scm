;;; Guile VM program functions

;;; Copyright (C) 2001, 2009, 2010, 2013 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (system vm program)
  #:use-module (ice-9 match)
  #:use-module (system vm instruction)
  #:use-module (system vm debug)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (make-rtl-program

            make-binding binding:name binding:boxed? binding:index
            binding:start binding:end

            source:addr source:line source:column source:file
            source:line-for-user
            program-sources program-sources-pre-retire program-source

            program-bindings-for-ip

            program-arities program-arity arity:start arity:end

            arity:nreq arity:nopt arity:rest? arity:kw arity:allow-other-keys?

            program-arguments-alist program-arguments-alists
            program-lambda-list

            rtl-program? rtl-program-code
            program-free-variables
            program-num-free-variables
            program-free-variable-ref program-free-variable-set!))

(load-extension (string-append "libguile-" (effective-version))
                "scm_init_programs")

;; These procedures are called by programs.c.
(define (rtl-program-name program)
  (unless (rtl-program? program)
    (error "shouldn't get here"))
  (and=> (find-program-debug-info (rtl-program-code program))
         program-debug-info-name))
(define (rtl-program-documentation program)
  (unless (rtl-program? program)
    (error "shouldn't get here"))
  (find-program-docstring (rtl-program-code program)))
(define (rtl-program-minimum-arity program)
  (unless (rtl-program? program)
    (error "shouldn't get here"))
  (program-minimum-arity (rtl-program-code program)))
(define (rtl-program-properties program)
  (unless (rtl-program? program)
    (error "shouldn't get here"))
  (find-program-properties (rtl-program-code program)))

(define (make-binding name boxed? index start end)
  (list name boxed? index start end))
(define (binding:name b) (list-ref b 0))
(define (binding:boxed? b) (list-ref b 1))
(define (binding:index b) (list-ref b 2))
(define (binding:start b) (list-ref b 3))
(define (binding:end b) (list-ref b 4))

(define (source:addr source)
  (car source))
(define (source:file source)
  (cadr source))
(define (source:line source)
  (caddr source))
(define (source:column source)
  (cdddr source))

;; Lines are zero-indexed inside Guile, but users expect them to be
;; one-indexed. Columns, on the other hand, are zero-indexed to both. Go
;; figure.
(define (source:line-for-user source)
  (1+ (source:line source)))

(define (source-for-addr addr)
  (and=> (find-source-for-addr addr)
         (lambda (source)
           ;; FIXME: absolute or relative address?
           (cons* 0
                  (source-file source)
                  (source-line source)
                  (source-column source)))))

(define (program-sources proc)
  (map (lambda (source)
         (cons* (- (source-post-pc source) (rtl-program-code proc))
                (source-file source)
                (source-line source)
                (source-column source)))
       (find-program-sources (rtl-program-code proc))))

(define* (program-source proc ip #:optional (sources (program-sources proc)))
  (let lp ((source #f) (sources sources))
    (match sources
      (() source)
      (((and s (pc . _)) . sources)
       (if (<= pc ip)
           (lp s sources)
           source)))))

;; Source information could in theory be correlated with the ip of the
;; instruction, or the ip just after the instruction is retired. Guile
;; does the latter, to make backtraces easy -- an error produced while
;; running an opcode always happens after it has retired its arguments.
;;
;; But for breakpoints and such, we need the ip before the instruction
;; is retired -- before it has had a chance to do anything. So here we
;; change from the post-retire addresses given by program-sources to
;; pre-retire addresses.
;;
(define (program-sources-pre-retire proc)
  (map (lambda (source)
         (cons* (- (source-pre-pc source) (rtl-program-code proc))
                (source-file source)
                (source-line source)
                (source-column source)))
       (find-program-sources (rtl-program-code proc))))

(define (collapse-locals locs)
  (let lp ((ret '()) (locs locs))
    (if (null? locs)
        (map cdr (sort! ret 
                        (lambda (x y) (< (car x) (car y)))))
        (let ((b (car locs)))
          (cond
           ((assv-ref ret (binding:index b))
            => (lambda (bindings)
                 (append! bindings (list b))
                 (lp ret (cdr locs))))
           (else
            (lp (acons (binding:index b) (list b) ret)
                (cdr locs))))))))

;; returns list of list of bindings
;; (list-ref ret N) == bindings bound to the Nth local slot
(define (program-bindings-by-index prog)
  ;; FIXME!
  '())

(define (program-bindings-for-ip prog ip)
  (let lp ((in (program-bindings-by-index prog)) (out '()))
    (if (null? in)
        (reverse out)
        (lp (cdr in)
            (let inner ((binds (car in)))
              (cond ((null? binds) out)
                    ((<= (binding:start (car binds))
                         ip
                         (binding:end (car binds)))
                     (cons (car binds) out))
                    (else (inner (cdr binds)))))))))

(define (arity:start a)
  (match a ((start end . _) start) (_ (error "bad arity" a))))
(define (arity:end a)
  (match a ((start end . _) end) (_ (error "bad arity" a))))
(define (arity:nreq a)
  (match a ((_ _ nreq . _) nreq) (_ 0)))
(define (arity:nopt a)
  (match a ((_ _ nreq nopt . _) nopt) (_ 0)))
(define (arity:rest? a)
  (match a ((_ _ nreq nopt rest? . _) rest?) (_ #f)))
(define (arity:kw a)
  (match a ((_ _ nreq nopt rest? (_ . kw)) kw) (_ '())))
(define (arity:allow-other-keys? a)
  (match a ((_ _ nreq nopt rest? (aok . kw)) aok) (_ #f)))

(define (program-arity prog ip)
  (let ((arities (program-arities prog)))
    (and arities
         (let lp ((arities arities))
           (cond ((null? arities) #f)
                 ((not ip) (car arities)) ; take the first one
                 ((and (< (arity:start (car arities)) ip)
                       (<= ip (arity:end (car arities))))
                  (car arities))
                 (else (lp (cdr arities))))))))

(define (arglist->arguments-alist arglist)
  (match arglist
    ((req opt keyword allow-other-keys? rest . extents)
     `((required . ,req)
       (optional . ,opt)
       (keyword . ,keyword)
       (allow-other-keys? . ,allow-other-keys?)
       (rest . ,rest)
       (extents . ,extents)))
    (_ #f)))

(define* (arity->arguments-alist prog arity
                                 #:optional
                                 (make-placeholder
                                  (lambda (i) (string->symbol "_"))))
  (define var-by-index
    (let ((rbinds (map (lambda (x)
                         (cons (binding:index x) (binding:name x)))
                       (program-bindings-for-ip prog
                                                (arity:start arity)))))
      (lambda (i)
        (or (assv-ref rbinds i)
            ;; if we don't know the name, return a placeholder
            (make-placeholder i)))))

  (let lp ((nreq (arity:nreq arity)) (req '())
           (nopt (arity:nopt arity)) (opt '())
           (rest? (arity:rest? arity)) (rest #f)
           (n 0))
    (cond
     ((< 0 nreq)
      (lp (1- nreq) (cons (var-by-index n) req)
          nopt opt rest? rest (1+ n)))
     ((< 0 nopt)
      (lp nreq req
          (1- nopt) (cons (var-by-index n) opt)
          rest? rest (1+ n)))
     (rest?
      (lp nreq req nopt opt
          #f (var-by-index (+ n (length (arity:kw arity))))
          (1+ n)))
     (else
      `((required . ,(reverse req))
        (optional . ,(reverse opt))
        (keyword . ,(arity:kw arity))
        (allow-other-keys? . ,(arity:allow-other-keys? arity))
        (rest . ,rest))))))

;; the name "program-arguments" is taken by features.c...
(define* (program-arguments-alist prog #:optional ip)
  "Returns the signature of the given procedure in the form of an association list."
  (cond
   ((primitive? prog)
    (match (procedure-minimum-arity prog)
      (#f #f)
      ((nreq nopt rest?)
       (let ((start (primitive-call-ip prog)))
         ;; Assume that there is only one IP for the call.
         (and (or (not ip) (= start ip))
              (arity->arguments-alist
               prog
               (list 0 0 nreq nopt rest? '(#f . ()))))))))
   ((rtl-program? prog)
    (or-map (lambda (arity)
              (and (or (not ip)
                       (and (<= (arity-low-pc arity) ip)
                            (< ip (arity-high-pc arity))))
                   (arity-arguments-alist arity)))
            (or (find-program-arities (rtl-program-code prog)) '())))
   (else
    (let ((arity (program-arity prog ip)))
      (and arity
           (arity->arguments-alist prog arity))))))

(define* (program-lambda-list prog #:optional ip)
  "Returns the signature of the given procedure in the form of an argument list."
  (and=> (program-arguments-alist prog ip) arguments-alist->lambda-list))

(define (arguments-alist->lambda-list arguments-alist)
  (let ((req (or (assq-ref arguments-alist 'required) '()))
        (opt (or (assq-ref arguments-alist 'optional) '()))
        (key (map keyword->symbol
                  (map car (or (assq-ref arguments-alist 'keyword) '()))))
        (rest (or (assq-ref arguments-alist 'rest) '())))
    `(,@req
      ,@(if (pair? opt) (cons #:optional opt) '())
      ,@(if (pair? key) (cons #:key key) '())
      . ,rest)))

(define (program-free-variables prog)
  "Return the list of free variables of PROG."
  (let ((count (program-num-free-variables prog)))
    (unfold (lambda (i) (>= i count))
            (cut program-free-variable-ref prog <>)
            1+
            0)))

(define (program-arguments-alists prog)
  "Returns all arities of the given procedure, as a list of association
lists."
  (define (fallback)
    (match (procedure-minimum-arity prog)
      (#f '())
      ((nreq nopt rest?)
       (list
        (arity->arguments-alist
         prog
         (list 0 0 nreq nopt rest? '(#f . ())))))))
  (cond
   ((primitive? prog) (fallback))
   ((rtl-program? prog)
    (let ((arities (find-program-arities (rtl-program-code prog))))
      (if arities
          (map arity-arguments-alist arities)
          (fallback))))
   (else (error "expected a program" prog))))

(define (write-program prog port)
  (define (program-identity-string)
    (or (procedure-name prog)
        (and=> (program-source prog 0)
               (lambda (s)
                 (format #f "~a at ~a:~a:~a"
                         (number->string (object-address prog) 16)
                         (or (source:file s)
                             (if s "<current input>" "<unknown port>"))
                         (source:line-for-user s) (source:column s))))
        (number->string (object-address prog) 16)))

  (define (program-formals-string)
    (let ((arguments (program-arguments-alists prog)))
      (if (null? arguments)
          ""
          (string-append
           " " (string-join (map (lambda (a)
                                   (object->string
                                    (arguments-alist->lambda-list a)))
                                 arguments)
                            " | ")))))

  (format port "#<procedure ~a~a>"
          (program-identity-string) (program-formals-string)))
