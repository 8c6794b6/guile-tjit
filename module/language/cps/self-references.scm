;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014, 2015 Free Software Foundation, Inc.

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
;;; A pass that prunes successors of expressions that bail out.
;;;
;;; Code:

(define-module (language cps self-references)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:export (resolve-self-references))

(define* (resolve-self-references fun #:optional (env '()))
  (define (subst var)
    (or (assq-ref env var) var))

  (define (visit-cont cont)
    (rewrite-cps-cont cont
      (($ $cont label ($ $kargs names vars body))
       (label ($kargs names vars ,(visit-term body))))
      (($ $cont label ($ $kfun src meta self tail clause))
       (label ($kfun src meta self ,tail
                ,(and clause (visit-cont clause)))))
      (($ $cont label ($ $kclause arity body alternate))
       (label ($kclause ,arity ,(visit-cont body)
                        ,(and alternate (visit-cont alternate)))))
      (_ ,cont)))

  (define (visit-term term)
    (rewrite-cps-term term
      (($ $letk conts body)
       ($letk ,(map visit-cont conts)
         ,(visit-term body)))
      (($ $continue k src exp)
       ($continue k src ,(visit-exp exp)))))

  (define (visit-exp exp)
    (rewrite-cps-exp exp
      ((or ($ $const) ($ $prim)) ,exp)
      (($ $fun body)
       ($fun ,(resolve-self-references body env)))
      (($ $rec names vars funs)
       ($rec names vars (map visit-recursive-fun funs vars)))
      (($ $call proc args)
       ($call (subst proc) ,(map subst args)))
      (($ $callk k proc args)
       ($callk k (subst proc) ,(map subst args)))
      (($ $primcall name args)
       ($primcall name ,(map subst args)))
      (($ $branch k exp)
       ($branch k ,(visit-exp exp)))
      (($ $values args)
       ($values ,(map subst args)))
      (($ $prompt escape? tag handler)
       ($prompt escape? (subst tag) handler))))

  (define (visit-recursive-fun fun var)
    (rewrite-cps-exp fun
      (($ $fun (and cont ($ $cont _ ($ $kfun src meta self))))
       ($fun ,(resolve-self-references cont (acons var self env))))))

  (visit-cont fun))
