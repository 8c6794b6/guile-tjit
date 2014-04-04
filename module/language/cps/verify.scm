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
;;;
;;; Code:

(define-module (language cps verify)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:export (verify-cps))

(define (verify-cps fun)
  (define seen-labels (make-hash-table))
  (define seen-vars (make-hash-table))

  (define (add sym seen env)
    (when (hashq-ref seen sym)
      (error "duplicate gensym" sym))
    (hashq-set! seen sym #t)
    (cons sym env))

  (define (add-env new seen env)
    (if (null? new)
        env
        (add-env (cdr new) seen (add (car new) seen env))))

  (define (add-vars new env)
    (unless (and-map exact-integer? new)
      (error "bad vars" new))
    (add-env new seen-vars env))

  (define (add-labels new env)
    (unless (and-map exact-integer? new)
      (error "bad labels" new))
    (add-env new seen-labels env))

  (define (check-ref sym seen env)
    (cond
     ((not (hashq-ref seen sym))
      (error "unbound lexical" sym))
     ((not (memq sym env))
      (error "displaced lexical" sym))))

  (define (check-label sym env)
    (check-ref sym seen-labels env))

  (define (check-var sym env)
    (check-ref sym seen-vars env))

  (define (check-src src)
    (if (and src (not (and (list? src) (and-map pair? src)
                           (and-map symbol? (map car src)))))
        (error "bad src")))

  (define (visit-cont-body cont k-env v-env)
    (match cont
      (($ $kif kt kf)
       (check-label kt k-env)
       (check-label kf k-env))
      (($ $kreceive ($ $arity ((? symbol?) ...) () (or #f (? symbol?)) () #f) k)
       (check-label k k-env))
      (($ $kargs (name ...) (sym ...) body)
       (unless (= (length name) (length sym))
         (error "name and sym lengths don't match" name sym))
       (visit-term body k-env (add-vars sym v-env)))
      (_ 
       ;; $kclause, $kentry, and $ktail are only ever seen in $fun.
       (error "unexpected cont body" cont))))

  (define (visit-clause clause k-env v-env)
    (match clause
      (($ $cont kclause
          ($ $kclause 
             ($ $arity
                ((? symbol? req) ...)
                ((? symbol? opt) ...)
                (and rest (or #f (? symbol?)))
                (((? keyword? kw) (? symbol? kwname) kwsym) ...)
                (or #f #t))
             ($ $cont kbody (and body ($ $kargs names syms _)))
             alternate))
       (for-each (lambda (sym)
                   (unless (memq sym syms)
                     (error "bad keyword sym" sym)))
                 kwsym)
       ;; FIXME: It is technically possible for kw syms to alias other
       ;; syms.
       (unless (equal? (append req opt (if rest (list rest) '()) kwname)
                       names)
         (error "clause body names do not match arity names" exp))
       (let ((k-env (add-labels (list kclause kbody) k-env)))
         (visit-cont-body body k-env v-env))
       (when alternate
         (visit-clause alternate k-env v-env)))
      (_
       (error "unexpected clause" clause))))

  (define (visit-fun fun k-env v-env)
    (match fun
      (($ $fun src meta (free ...)
          ($ $cont kbody
             ($ $kentry self ($ $cont ktail ($ $ktail)) clause)))
       (when (and meta (not (and (list? meta) (and-map pair? meta))))
         (error "meta should be alist" meta))
       (for-each (cut check-var <> v-env) free)
       (check-src src)
       ;; Reset the continuation environment, because Guile's
       ;; continuations are local.
       (let ((v-env (add-vars (list self) v-env))
             (k-env (add-labels (list ktail) '())))
         (when clause
           (visit-clause clause k-env v-env))))
      (_
       (error "unexpected $fun" fun))))

  (define (visit-expression exp k-env v-env)
    (match exp
      (($ $void)
       #t)
      (($ $const val)
       #t)
      (($ $prim (? symbol? name))
       #t)
      (($ $fun)
       (visit-fun exp k-env v-env))
      (($ $call proc (arg ...))
       (check-var proc v-env)
       (for-each (cut check-var <> v-env) arg))
      (($ $callk k* proc (arg ...))
       ;; We don't check that k* is in scope; it's actually inside some
       ;; other function, probably.  We rely on the transformation that
       ;; introduces the $callk to be correct, and the linker to resolve
       ;; the reference.
       (check-var proc v-env)
       (for-each (cut check-var <> v-env) arg))
      (($ $primcall (? symbol? name) (arg ...))
       (for-each (cut check-var <> v-env) arg))
      (($ $values (arg ...))
       (for-each (cut check-var <> v-env) arg))
      (($ $prompt escape? tag handler)
       (unless (boolean? escape?) (error "escape? should be boolean" escape?))
       (check-var tag v-env)
       (check-label handler k-env))
      (_
       (error "unexpected expression" exp))))

  (define (visit-term term k-env v-env)
    (match term
      (($ $letk (($ $cont k cont) ...) body)
       (let ((k-env (add-labels k k-env)))
         (for-each (cut visit-cont-body <> k-env v-env) cont)
         (visit-term body k-env v-env)))

      (($ $letrec (name ...) (sym ...) (fun ...) body)
       (unless (= (length name) (length sym) (length fun))
         (error "letrec syms, names, and funs not same length" term))
       (let ((v-env (add-vars sym v-env)))
         (for-each (cut visit-fun <> k-env v-env) fun)
         (visit-term body k-env v-env)))

      (($ $continue k src exp)
       (check-label k k-env)
       (check-src src)
       (visit-expression exp k-env v-env))

      (_
       (error "unexpected term" term))))

  (visit-fun fun '() '())
  fun)
