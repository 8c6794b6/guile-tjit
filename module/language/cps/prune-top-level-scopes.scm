;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2014 Free Software Foundation, Inc.

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
;;; A simple pass to prune unneeded top-level scopes.
;;;
;;; Code:

(define-module (language cps prune-top-level-scopes)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:export (prune-top-level-scopes))

(define (compute-referenced-scopes fun)
  (let ((refs (make-hash-table)))
    (define (visit-cont cont)
      (match cont
        (($ $cont k ($ $kargs (name) (sym) body))
         (visit-term body)
         (when (hashq-get-handle refs sym)
           (hashq-set! refs k sym)))
        (($ $cont k ($ $kargs names syms body))
         (visit-term body))
        (($ $cont k ($ $kentry self tail clause))
         (when clause (visit-cont clause)))
        (($ $cont k ($ $kclause arity body alternate))
         (visit-cont body)
         (when alternate (visit-cont alternate)))
        (($ $cont k (or ($ $kreceive) ($ $kif)))
         #t)))
    (define (visit-term term)
      (match term
        (($ $letk conts body)
         (for-each visit-cont conts)
         (visit-term body))
        (($ $letrec names syms funs body)
         (for-each visit-fun funs)
         (visit-term body))
        (($ $continue k src exp)
         (match exp
           (($ $fun) (visit-fun exp))
           (($ $primcall 'cached-toplevel-box (scope name bound?))
            (hashq-set! refs scope #t))
           (($ $primcall 'cache-current-module! (module scope))
            (hashq-set! refs scope #f))
           (($ $const val)
            ;; If there is an entry in the table for "k", it means "val"
            ;; is a scope symbol, bound for use by cached-toplevel-box
            ;; or cache-current-module!, or possibly both (though this
            ;; is not currently the case).
            (and=> (hashq-ref refs k)
                   (lambda (sym)
                     (when (hashq-ref refs sym)
                       ;; We have a use via cached-toplevel-box.  Mark
                       ;; this scope as used.
                       (hashq-set! refs val #t))
                     (when (and (hashq-ref refs val)
                                (not (hashq-ref refs sym)))
                       ;; There is a use, and this sym is used by
                       ;; cache-current-module!.
                       (hashq-set! refs sym #t)))))
           (_ #t)))))
    (define (visit-fun fun)
      (match fun
        (($ $fun src meta free body)
         (visit-cont body))))

    (visit-fun fun)
    refs))

(define (prune-top-level-scopes fun)
  (let ((referenced-scopes (compute-referenced-scopes fun)))
    (define (visit-cont cont)
      (rewrite-cps-cont cont
        (($ $cont sym ($ $kargs names syms body))
         (sym ($kargs names syms ,(visit-term body))))
        (($ $cont sym ($ $kentry self tail clause))
         (sym ($kentry self ,tail ,(and clause (visit-cont clause)))))
        (($ $cont sym ($ $kclause arity body alternate))
         (sym ($kclause ,arity ,(visit-cont body)
                        ,(and alternate (visit-cont alternate)))))
        (($ $cont sym (or ($ $kreceive) ($ $kif)))
         ,cont)))
    (define (visit-term term)
      (rewrite-cps-term term
        (($ $letk conts body)
         ($letk ,(map visit-cont conts) ,(visit-term body)))
        (($ $letrec names syms funs body)
         ($letrec names syms funs ,(visit-term body)))
        (($ $continue k src
            (and ($ $primcall 'cache-current-module! (module scope))
                 (? (lambda _
                      (not (hashq-ref referenced-scopes scope))))))
         ($continue k src ($primcall 'values ())))
        (($ $continue)
         ,term)))
    (rewrite-cps-exp fun
      (($ $fun src meta free body)
       ($fun src meta free ,(visit-cont body))))))
