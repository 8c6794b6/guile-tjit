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

(define (fix-clause-arities clause dfg)
  (let ((ktail (match clause
                 (($ $cont _ ($ $kentry _ ($ $cont ktail) _)) ktail))))
    (define (visit-term term)
      (rewrite-cps-term term
        (($ $letk conts body)
         ($letk ,(map visit-cont conts) ,(visit-term body)))
        (($ $letrec names syms funs body)
         ($letrec names syms (map (cut fix-arities* <> dfg) funs)
                  ,(visit-term body)))
        (($ $continue k src exp)
         ,(visit-exp k src exp))))

    (define (adapt-exp nvals k src exp)
      (match nvals
        (0
         (rewrite-cps-term (lookup-cont k dfg)
           (($ $ktail)
            ,(let-fresh (kvoid kunspec) (unspec)
               (build-cps-term
                 ($letk* ((kunspec ($kargs (unspec) (unspec)
                                     ($continue k src
                                       ($primcall 'return (unspec)))))
                          (kvoid ($kargs () ()
                                   ($continue kunspec src ($void)))))
                   ($continue kvoid src ,exp)))))
           (($ $kreceive arity kargs)
            ,(match arity
               (($ $arity () () rest () #f)
                (if rest
                    (let-fresh (knil) ()
                      (build-cps-term
                        ($letk ((knil ($kargs () ()
                                        ($continue kargs src ($const '())))))
                          ($continue knil src ,exp))))
                    (build-cps-term
                      ($continue kargs src ,exp))))
               (_
                (let-fresh (kvoid kvalues) (void)
                  (build-cps-term
                    ($letk* ((kvalues ($kargs ('void) (void)
                                        ($continue k src
                                          ($primcall 'values (void)))))
                             (kvoid ($kargs () ()
                                      ($continue kvalues src
                                        ($void)))))
                      ($continue kvoid src ,exp)))))))
           (($ $kargs () () _)
            ($continue k src ,exp))
           (_
            ,(let-fresh (k*) ()
               (build-cps-term
                 ($letk ((k* ($kargs () () ($continue k src ($void)))))
                   ($continue k* src ,exp)))))))
        (1
         (rewrite-cps-term (lookup-cont k dfg)
           (($ $ktail)
            ,(rewrite-cps-term exp
               (($values (sym))
                ($continue ktail src ($primcall 'return (sym))))
               (_
                ,(let-fresh (k*) (v)
                   (build-cps-term
                     ($letk ((k* ($kargs (v) (v)
                                   ($continue k src
                                     ($primcall 'return (v))))))
                       ($continue k* src ,exp)))))))
           (($ $kreceive arity kargs)
            ,(match arity
               (($ $arity (_) () rest () #f)
                (if rest
                    (let-fresh (kval) (val nil)
                      (build-cps-term
                        ($letk ((kval ($kargs ('val) (val)
                                        ($letconst (('nil nil '()))
                                          ($continue kargs src
                                            ($values (val nil)))))))
                          ($continue kval src ,exp))))
                    (build-cps-term ($continue kargs src ,exp))))
               (_
                (let-fresh (kvalues) (value)
                  (build-cps-term
                    ($letk ((kvalues ($kargs ('value) (value)
                                       ($continue k src
                                         ($primcall 'values (value))))))
                      ($continue kvalues src ,exp)))))))
           (($ $kargs () () _)
            ,(let-fresh (k*) (drop)
               (build-cps-term
                 ($letk ((k* ($kargs ('drop) (drop)
                               ($continue k src ($values ())))))
                   ($continue k* src ,exp)))))
           (_
            ($continue k src ,exp))))))

    (define (visit-exp k src exp)
      (rewrite-cps-term exp
        ((or ($ $void)
             ($ $const)
             ($ $prim)
             ($ $values (_)))
         ,(adapt-exp 1 k src exp))
        (($ $fun)
         ,(adapt-exp 1 k src (fix-arities* exp dfg)))
        ((or ($ $call) ($ $callk))
         ;; In general, calls have unknown return arity.  For that
         ;; reason every non-tail call has a $kreceive continuation to
         ;; adapt the return to the target continuation, and we don't
         ;; need to do any adapting here.
         ($continue k src ,exp))
        (($ $primcall 'return (arg))
         ;; Primcalls to return are in tail position.
         ($continue ktail src ,exp))
        (($ $primcall (? (lambda (name)
                           (and (not (prim-instruction name))
                                (not (branching-primitive? name))))))
         ($continue k src ,exp))
        (($ $primcall name args)
         ,(match (prim-arity name)
            ((out . in)
             (if (= in (length args))
                 (adapt-exp out k src
                            (let ((inst (prim-instruction name)))
                              (if (and inst (not (eq? inst name)))
                                  (build-cps-exp ($primcall inst args))
                                  exp)))
                 (let-fresh (k*) (p*)
                   (build-cps-term
                     ($letk ((k* ($kargs ('prim) (p*)
                                   ($continue k src ($call p* args)))))
                       ($continue k* src ($prim name)))))))))
        (($ $values)
         ;; Non-unary values nodes are inserted by CPS optimization
         ;; passes, so we assume they are correct.
         ($continue k src ,exp))
        (($ $prompt)
         ($continue k src ,exp))))

    (define (visit-cont cont)
      (rewrite-cps-cont cont
        (($ $cont sym ($ $kargs names syms body))
         (sym ($kargs names syms ,(visit-term body))))
        (($ $cont sym ($ $kclause arity body alternate))
         (sym ($kclause ,arity ,(visit-cont body)
                        ,(and alternate (visit-cont alternate)))))
        (($ $cont)
         ,cont)))

    (rewrite-cps-cont clause
      (($ $cont sym ($ $kentry self tail clause))
       (sym ($kentry self ,tail ,(and clause (visit-cont clause))))))))

(define (fix-arities* fun dfg)
  (rewrite-cps-exp fun
    (($ $fun src meta free body)
     ($fun src meta free ,(fix-clause-arities body dfg)))))

(define (fix-arities fun)
  (with-fresh-name-state fun
    (fix-arities* fun (compute-dfg fun))))
