;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013 Free Software Foundation, Inc.

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
;;; A pass to adapt expressions to the arities of their continuations,
;;; and to rewrite some tail expressions as primcalls to "return".
;;;
;;; Code:

(define-module (language cps arities)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (srfi srfi-26)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (language cps primitives)
  #:export (fix-arities))

(define (fix-clause-arities clause)
  (let ((conts (build-local-cont-table clause))
        (ktail (match clause
                 (($ $cont _ _ ($ $kentry _ ($ $cont ktail) _)) ktail))))
    (define (visit-term term)
      (rewrite-cps-term term
        (($ $letk conts body)
         ($letk ,(map visit-cont conts) ,(visit-term body)))
        (($ $letrec names syms funs body)
         ($letrec names syms (map fix-arities funs) ,(visit-term body)))
        (($ $continue k exp)
         ,(visit-exp k exp))))

    (define (adapt-exp nvals k exp)
      (match nvals
        (0
         (rewrite-cps-term (lookup-cont k conts)
           (($ $ktail)
            ,(let-gensyms (kvoid kunspec unspec)
               (build-cps-term
                 ($letk* ((kunspec #f ($kargs (unspec) (unspec)
                                        ($continue k
                                          ($primcall 'return (unspec)))))
                          (kvoid #f ($kargs () ()
                                      ($continue kunspec ($void)))))
                   ($continue kvoid ,exp)))))
           (($ $ktrunc arity kargs)
            ,(rewrite-cps-term arity
               (($ $arity () () #f () #f)
                ($continue kargs ,exp))
               (_
                ,(let-gensyms (kvoid kvalues void)
                   (build-cps-term
                     ($letk* ((kvalues #f ($kargs ('void) (void)
                                            ($continue k
                                              ($primcall 'values (void)))))
                              (kvoid #f ($kargs () ()
                                          ($continue kvalues
                                            ($void)))))
                       ($continue kvoid ,exp)))))))
           (($ $kargs () () _)
            ($continue k ,exp))
           (_
            ,(let-gensyms (k*)
               (build-cps-term
                 ($letk ((k* #f ($kargs () () ($continue k ($void)))))
                   ($continue k* ,exp)))))))
        (1
         (rewrite-cps-term (lookup-cont k conts)
           (($ $ktail)
            ,(rewrite-cps-term exp
               (($var sym)
                ($continue ktail ($primcall 'return (sym))))
               (_
                ,(let-gensyms (k* v)
                   (build-cps-term
                     ($letk ((k* #f ($kargs (v) (v)
                                      ($continue k
                                        ($primcall 'return (v))))))
                       ($continue k* ,exp)))))))
           (($ $ktrunc arity kargs)
            ,(rewrite-cps-term arity
               (($ $arity (_) () #f () #f)
                ($continue kargs ,exp))
               (_
                ,(let-gensyms (kvalues value)
                   (build-cps-term
                     ($letk ((kvalues #f ($kargs ('value) (value)
                                           ($continue k
                                             ($primcall 'values (value))))))
                       ($continue kvalues ,exp)))))))
           (($ $kargs () () _)
            ,(let-gensyms (k* drop)
               (build-cps-term
                 ($letk ((k* #f ($kargs ('drop) (drop)
                                  ($continue k ($values ())))))
                   ($continue k* ,exp)))))
           (_
            ($continue k ,exp))))))

    (define (visit-exp k exp)
      (rewrite-cps-term exp
        ((or ($ $void)
             ($ $const)
             ($ $prim)
             ($ $var))
         ,(adapt-exp 1 k exp))
        (($ $fun)
         ,(adapt-exp 1 k (fix-arities exp)))
        (($ $call)
         ;; In general, calls have unknown return arity.  For that
         ;; reason every non-tail call has an implicit adaptor
         ;; continuation to adapt the return to the target
         ;; continuation, and we don't need to do any adapting here.
         ($continue k ,exp))
        (($ $primcall 'return (arg))
         ;; Primcalls to return are in tail position.
         ($continue ktail ,exp))
        (($ $primcall (? (lambda (name)
                           (and (not (prim-rtl-instruction name))
                                (not (branching-primitive? name))))))
         ($continue k ,exp))
        (($ $primcall name args)
         ,(match (prim-arity name)
            ((out . in)
             (if (= in (length args))
                 (adapt-exp out k
                            (let ((inst (prim-rtl-instruction name)))
                              (if (and inst (not (eq? inst name)))
                                  (build-cps-exp ($primcall inst args))
                                  exp)))
                 (let-gensyms (k* p*)
                   (build-cps-term
                     ($letk ((k* #f ($kargs ('prim) (p*)
                                      ($continue k ($call p* args)))))
                       ($continue k* ($prim name)))))))))
        (($ $values)
         ;; Values nodes are inserted by CPS optimization passes, so
         ;; we assume they are correct.
         ($continue k ,exp))
        (($ $prompt)
         ($continue k ,exp))))

    (define (visit-cont cont)
      (rewrite-cps-cont cont
        (($ $cont sym src ($ $kargs names syms body))
         (sym src ($kargs names syms ,(visit-term body))))
        (($ $cont sym src ($ $kclause arity body))
         (sym src ($kclause ,arity ,(visit-cont body))))
        (($ $cont)
         ,cont)))

    (rewrite-cps-cont clause
      (($ $cont sym src ($ $kentry self tail clauses))
       (sym src ($kentry self ,tail ,(map visit-cont clauses)))))))

(define (fix-arities fun)
  (rewrite-cps-exp fun
    (($ $fun meta free body)
     ($fun meta free ,(fix-clause-arities body)))))
