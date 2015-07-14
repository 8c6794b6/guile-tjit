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
;;; Compiling CPS2 to CPS.  When/if CPS2 replaces CPS, this module will be removed.
;;;
;;; Code:

(define-module (language cps2 compile-cps)
  #:use-module (ice-9 match)
  #:use-module (language cps2)
  #:use-module ((language cps) #:prefix cps:)
  #:use-module (language cps2 utils)
  #:use-module (language cps2 optimize)
  #:use-module (language cps2 renumber)
  #:use-module (language cps intmap)
  #:export (compile-cps))

;; Precondition: For each function in CONTS, the continuation names are
;; topologically sorted.
(define (conts->fun conts)
  (define (convert-fun kfun)
    (let ((doms (compute-dom-edges (compute-idoms conts kfun))))
      (define (visit-cont label)
        (cps:rewrite-cps-cont (intmap-ref conts label)
          (($ $kargs names syms body)
           (label (cps:$kargs names syms ,(redominate label (visit-term body)))))
          (($ $ktail)
           (label (cps:$ktail)))
          (($ $kreceive ($ $arity req () rest () #f) kargs)
           (label (cps:$kreceive req rest kargs)))))
      (define (visit-clause label)
        (and label
             (cps:rewrite-cps-cont (intmap-ref conts label)
               (($ $kclause ($ $arity req opt rest kw aok?) kbody kalt)
                (label (cps:$kclause (req opt rest kw aok?)
                                     ,(visit-cont kbody)
                                     ,(visit-clause kalt)))))))
      (define (redominate label term)
        (define (visit-dom-conts label)
          (match (intmap-ref conts label)
            (($ $ktail) '())
            (($ $kargs) (list (visit-cont label)))
            (else
             (cons (visit-cont label)
                   (visit-dom-conts* (intmap-ref doms label))))))
        (define (visit-dom-conts* labels)
          (match labels
            (() '())
            ((label . labels)
             (append (visit-dom-conts label)
                     (visit-dom-conts* labels)))))
        (cps:rewrite-cps-term (visit-dom-conts* (intmap-ref doms label))
          (() ,term)
          (conts (cps:$letk ,conts ,term))))
      (define (visit-term term)
        (cps:rewrite-cps-term term
          (($ $continue k src (and ($ $fun) fun))
           (cps:$continue k src ,(visit-fun fun)))
          (($ $continue k src ($ $rec names syms funs))
           (cps:$continue k src (cps:$rec names syms (map visit-fun funs))))
          (($ $continue k src exp)
           (cps:$continue k src ,(visit-exp exp)))))
      (define (visit-exp exp)
        (cps:rewrite-cps-exp exp
          (($ $const val) (cps:$const val))
          (($ $prim name) (cps:$prim name))
          (($ $closure k nfree) (cps:$closure k nfree))
          (($ $call proc args) (cps:$call proc args))
          (($ $callk k proc args) (cps:$callk k proc args))
          (($ $primcall name args) (cps:$primcall name args))
          (($ $branch k exp) (cps:$branch k ,(visit-exp exp)))
          (($ $values args) (cps:$values args))
          (($ $prompt escape? tag handler) (cps:$prompt escape? tag handler))))
      (define (visit-fun fun)
        (cps:rewrite-cps-exp fun
          (($ $fun body)
           (cps:$fun ,(convert-fun body)))))

      (cps:rewrite-cps-cont (intmap-ref conts kfun)
        (($ $kfun src meta self tail clause)
         (kfun (cps:$kfun src meta self (tail (cps:$ktail))
                 ,(visit-clause clause)))))))
  (convert-fun 0))

(define (compile-cps exp env opts)
  (let ((exp (renumber (optimize-higher-order-cps exp opts))))
    (values (conts->fun exp) env env)))
