;;;; Compile ANF IR to list of primitive operations

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA
;;;;

;;; Commentary:
;;;
;;; Assign registers to ANF IR, compile to list of primitive operations.
;;;
;;; Code:

(define-module (system vm native tjit ra)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (system vm native debug)
  #:export ($primlist
            primlist?
            primlist-entry
            primlist-loop
            anf->primlist))

;;;
;;; Record type
;;;

;; Record type to hold lists of primitives.
(define-record-type $primlist
  (make-primlist entry loop)
  primlist?
  ;; List of primitives for entry clause.
  (entry primlist-entry)

  ;; List of primitives for loop body.
  (loop primlist-loop))


;;;
;;; ANF to Primitive List
;;;

(define (anf->primlist locals vars term)
  (debug 1 ";;; anf->primlist:~%")
  (debug 1 ";;; locals: ~a~%" locals)
  (debug 1 ";;; vars: ~a~%" vars)
  (match term
    (`(letrec ((entry ,entry)
               (loop ,loop))
        entry)
     (let ((entry-asm (compile-primitive vars entry))
           (loop-asm (compile-primitive vars loop)))
       (make-primlist entry-asm loop-asm)))
    (`(letrec ((patch ,patch))
        patch)
     (let ((patch-asm (compile-primitive vars patch)))
       (make-primlist patch-asm '())))
    (_
     (error "anf->primlist: malformed term" term))))

(define (compile-primitive vars term)
  "Compile ANF form TERM to list of primitive operations."
  (let ((env (let lp ((t (make-hash-table))
                      (vars (reverse vars))
                      (i 0))
               (match vars
                 (((_ . name) . vars)
                  (hashq-set! t name `(gpr . ,i))
                  (lp t vars (+ i 1)))
                 (() t)))))
    (define (constant? x)
      (cond
       ((boolean? x) #t)
       ((char? x) #t)
       ((number? x) #t)
       (else #f)))
    (define (ref k)
      (or (hashq-ref env k)
          `(const . ,k)))
    (define (compile-term term acc)
      (match term
        (('lambda _ rest)
         (compile-term rest acc))
        (('begin term1 rest)
         (compile-term rest (compile-term term1 acc)))
        (('let ((dst (? constant? val))) term1)
         (compile-term term1 (cons `(%movi ,(ref dst) ,val) acc)))
        (('let ((dst (? symbol? src))) term1)
         (compile-term term1 (cons `(%movr ,(ref dst) ,(ref src)) acc)))
        (('let ((dst (op . args))) term1)
         (compile-term term1 (cons `(,op ,(ref dst) ,@(map ref args)) acc)))
        (('loop . _)
         acc)
        (((? number? ip) . env)
         (cons `(%snap ,ip ,@(map ref env)) acc))
        (((? symbol? op) . args)
         (cons `(,op ,@(map ref args)) acc))
        (()
         acc)))

    (reverse! (compile-term term '()))))
