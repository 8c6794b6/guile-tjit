;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2015 Free Software Foundation, Inc.

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
;;; Some arithmetic operations have multiple implementations: one
;;; polymorphic implementation that works on all kinds of numbers, like
;;; `add', and one or more specialized variants for unboxed numbers of
;;; some kind, like `fadd'.  If we can replace a polymorphic
;;; implementation with a monomorphic implementation, we should do so --
;;; it will speed up the runtime and avoid boxing numbers.
;;;
;;; A polymorphic operation can be specialized if its result is
;;; specialized.  To specialize an operation, we manually unbox its
;;; arguments and box its return value, relying on CSE to remove boxes
;;; where possible.
;;;
;;; Code:

(define-module (language cps specialize-numbers)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps renumber)
  #:use-module (language cps types)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (specialize-numbers))

(define (specialize-f64-binop cps k src op a b)
  (let ((fop (match op
               ('add 'fadd)
               ('sub 'fsub)
               ('mul 'fmul)
               ('div 'fdiv))))
    (with-cps cps
      (letv f64-a f64-b result)
      (letk kbox ($kargs ('result) (result)
                   ($continue k src
                     ($primcall 'f64->scm (result)))))
      (letk kop ($kargs ('f64-b) (f64-b)
                  ($continue kbox src
                    ($primcall fop (f64-a f64-b)))))
      (letk kunbox-b ($kargs ('f64-a) (f64-a)
                       ($continue kop src
                         ($primcall 'scm->f64 (b)))))
      (build-term
        ($continue kunbox-b src
          ($primcall 'scm->f64 (a)))))))

(define (specialize-numbers cps)
  (define (visit-cont label cont cps types)
    (match cont
      (($ $kfun)
       (values cps (infer-types cps label)))
      (($ $kargs names vars
          ($ $continue k src
             ($ $primcall (and op (or 'add 'sub 'mul 'div)) (a b))))
       (match (intmap-ref cps k)
         (($ $kargs (_) (result))
          (call-with-values (lambda ()
                              (lookup-post-type types label result 0))
            (lambda (type min max)
              (values
               (if (eqv? type &flonum)
                   (with-cps cps
                     (let$ body (specialize-f64-binop k src op a b))
                     (setk label ($kargs names vars ,body)))
                   cps)
               types))))))
      (_ (values cps types))))

  ;; Type inference wants a renumbered graph; OK.
  (let ((cps (renumber cps)))
    (with-fresh-name-state cps
      (values (intmap-fold visit-cont cps cps #f)))))
