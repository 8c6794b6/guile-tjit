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
  (define seen-gensyms (make-hash-table))

  (define (add sym env)
    (if (hashq-ref seen-gensyms sym)
        (error "duplicate gensym" sym)
        (begin
          (hashq-set! seen-gensyms sym #t)
          (cons sym env))))

  (define (add-env new env)
    (if (null? new)
        env
        (add-env (cdr new) (add (car new) env))))

  (define (check-var sym env)
    (cond
     ((not (hashq-ref seen-gensyms sym))
      (error "unbound lexical" sym))
     ((not (memq sym env))
      (error "displaced lexical" sym))))

  (define (check-src src)
    (if (and src (not (and (list? src) (and-map pair? src)
                           (and-map symbol? (map car src)))))
        (error "bad src")))

  (define (visit-cont-body cont k-env v-env)
    (match cont
      (($ $kif kt kf)
       (check-var kt k-env)
       (check-var kf k-env))
      (($ $kreceive ($ $arity ((? symbol?) ...) () (or #f (? symbol?)) () #f) k)
       (check-var k k-env))
      (($ $kargs ((? symbol? name) ...) ((? symbol? sym) ...) body)
       (unless (= (length name) (length sym))
         (error "name and sym lengths don't match" name sym))
       (visit-term body k-env (add-env sym v-env)))
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
                (((? keyword? kw) (? symbol? kwname) (? symbol? kwsym)) ...)
                (or #f #t))
             ($ $cont kbody (and body ($ $kargs names syms _)))))
       (for-each (lambda (sym)
                   (unless (memq sym syms)
                     (error "bad keyword sym" sym)))
                 kwsym)
       ;; FIXME: It is technically possible for kw syms to alias other
       ;; syms.
       (unless (equal? (append req opt (if rest (list rest) '()) kwname)
                       names)
         (error "clause body names do not match arity names" exp))
       (let ((k-env (add-env (list kclause kbody) k-env)))
         (visit-cont-body body k-env v-env)))
      (_
       (error "unexpected clause" clause))))

  (define (visit-fun fun k-env v-env)
    (match fun
      (($ $fun src meta ((? symbol? free) ...)
          ($ $cont kbody
             ($ $kentry (? symbol? self) ($ $cont ktail ($ $ktail)) clauses)))
       (when (and meta (not (and (list? meta) (and-map pair? meta))))
         (error "meta should be alist" meta))
       (for-each (cut check-var <> v-env) free)
       (check-src src)
       ;; Reset the continuation environment, because Guile's
       ;; continuations are local.
       (let ((v-env (add-env (list self) v-env))
             (k-env (add-env (list ktail) '())))
         (for-each (cut visit-clause <> k-env v-env) clauses)))
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
      (($ $call (? symbol? proc) ((? symbol? arg) ...))
       (check-var proc v-env)
       (for-each (cut check-var <> v-env) arg))
      (($ $callk (? symbol? k*) (? symbol? proc) ((? symbol? arg) ...))
       ;; We don't check that k* is in scope; it's actually inside some
       ;; other function, probably.  We rely on the transformation that
       ;; introduces the $callk to be correct, and the linker to resolve
       ;; the reference.
       (check-var proc v-env)
       (for-each (cut check-var <> v-env) arg))
      (($ $primcall (? symbol? name) ((? symbol? arg) ...))
       (for-each (cut check-var <> v-env) arg))
      (($ $values ((? symbol? arg) ...))
       (for-each (cut check-var <> v-env) arg))
      (($ $prompt escape? tag handler)
       (unless (boolean? escape?) (error "escape? should be boolean" escape?))
       (check-var tag v-env)
       (check-var handler k-env))
      (_
       (error "unexpected expression" exp))))

  (define (visit-term term k-env v-env)
    (match term
      (($ $letk (($ $cont (? symbol? k) cont) ...) body)
       (let ((k-env (add-env k k-env)))
         (for-each (cut visit-cont-body <> k-env v-env) cont)
         (visit-term body k-env v-env)))

      (($ $letrec ((? symbol? name) ...) ((? symbol? sym) ...) (fun ...) body)
       (unless (= (length name) (length sym) (length fun))
         (error "letrec syms, names, and funs not same length" term))
       (let ((v-env (add-env sym v-env)))
         (for-each (cut visit-fun <> k-env v-env) fun)
         (visit-term body k-env v-env)))

      (($ $continue k src exp)
       (check-var k k-env)
       (check-src src)
       (visit-expression exp k-env v-env))

      (_
       (error "unexpected term" term))))

  (visit-fun fun '() '())
  fun)
