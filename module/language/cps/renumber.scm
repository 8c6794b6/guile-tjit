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
;;; A pass to renumber variables and continuation labels so that they
;;; are contiguous within each function.
;;;
;;; Code:

(define-module (language cps renumber)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:export (renumber))

(define (visit-funs proc fun)
  (define (visit-cont cont)
    (match cont
      (($ $cont label cont)
       (match cont
         (($ $kargs names vars body)
          (visit-term body))
         (($ $kentry self tail clauses)
          (for-each visit-cont clauses))
         (($ $kclause arity body)
          (visit-cont body))
         ((or ($ $kreceive) ($ $kif))
          #f)))))
  (define (visit-term term)
    (match term
      (($ $letk conts body)
       (for-each visit-cont conts)
       (visit-term body))
      (($ $letrec names syms funs body)
       (for-each visit-fun funs)
       (visit-term body))
      (($ $continue k src (and fun ($ $fun)))
       (visit-fun fun))
      (($ $continue k src _)
       #f)))
  (define (visit-fun fun)
    (proc fun)
    (match fun
      (($ $fun src meta free body)
       (visit-cont body))))
  (visit-fun fun))

(define (compute-new-labels-and-vars fun)
  (call-with-values (lambda () (compute-max-label-and-var fun))
    (lambda (max-label max-var)
      (let ((labels (make-vector (1+ max-label)))
            (next-label 0)
            (vars (make-vector (1+ max-var)))
            (next-var 0))
        (define (relabel! label)
          (vector-set! labels label next-label)
          (set! next-label (1+ next-label)))
        (define (rename! var)
          (vector-set! vars var next-var)
          (set! next-var (1+ next-var)))
        (define (compute-names-in-fun fun)
          (define (visit-cont cont)
            (match cont
              (($ $cont label cont)
               (relabel! label)
               (match cont
                 (($ $kargs names vars body)
                  (for-each rename! vars)
                  (visit-term body))
                 (($ $kentry self tail clauses)
                  (rename! self)
                  (visit-cont tail)
                  (for-each visit-cont clauses))
                 (($ $kclause arity body)
                  (visit-cont body))
                 ((or ($ $ktail) ($ $kreceive) ($ $kif))
                  #f)))))
          (define (visit-term term)
            (match term
              (($ $letk conts body)
               (for-each visit-cont conts)
               (visit-term body))
              (($ $letrec names syms funs body)
               (for-each rename! syms)
               (visit-term body))
              (($ $continue k src _)
               #f)))
          (match fun
            (($ $fun src meta free body)
             (visit-cont body))))

        (visit-funs compute-names-in-fun fun)
        (values labels vars)))))

(define (renumber fun)
  (call-with-values (lambda () (compute-new-labels-and-vars fun))
    (lambda (labels vars)
      (define (relabel label) (vector-ref labels label))
      (define (rename var) (vector-ref vars var))
      (define (rename-kw-arity arity)
        (match arity
          (($ $arity req opt rest kw aok?)
           (make-$arity req opt rest
                        (map (match-lambda
                              ((kw kw-name kw-var)
                               (list kw kw-name (rename kw-var))))
                             kw)
                        aok?))))
      (define (visit-cont cont)
        (rewrite-cps-cont cont
          (($ $cont label ($ $kargs names vars body))
           ((relabel label)
            ($kargs names (map rename vars) ,(visit-term body))))
          (($ $cont label ($ $kentry self tail clauses))
           ((relabel label)
            ($kentry (rename self) ,(visit-cont tail)
              ,(map visit-cont clauses))))
          (($ $cont label ($ $ktail))
           ((relabel label) ($ktail)))
          (($ $cont label ($ $kclause arity body))
           ((relabel label)
            ($kclause ,(rename-kw-arity arity) ,(visit-cont body))))
          (($ $cont label ($ $kreceive ($ $arity req () rest () #f) kargs))
           ((relabel label) ($kreceive req rest (relabel kargs))))
          (($ $cont label ($ $kif kt kf))
           ((relabel label) ($kif (relabel kt) (relabel kf))))))
      (define (visit-term term)
        (rewrite-cps-term term
          (($ $letk conts body)
           ($letk ,(map visit-cont conts)
             ,(visit-term body)))
          (($ $letrec names vars funs body)
           ($letrec names (map rename vars) (map visit-fun funs)
                    ,(visit-term body)))
          (($ $continue k src exp)
           ($continue (relabel k) src ,(visit-exp exp)))))
      (define (visit-exp exp)
        (match exp
          ((or ($ $void) ($ $const) ($ $prim))
           exp)
          (($ $fun)
           (visit-fun exp))
          (($ $values args)
           (let ((args (map rename args)))
              (build-cps-exp ($values args))))
          (($ $call proc args)
           (let ((args (map rename args)))
              (build-cps-exp ($call (rename proc) args))))
          (($ $callk k proc args)
           (let ((args (map rename args)))
              (build-cps-exp ($callk (relabel k) (rename proc) args))))
          (($ $primcall name args)
           (let ((args (map rename args)))
              (build-cps-exp ($primcall name args))))
          (($ $prompt escape? tag handler)
           (build-cps-exp
             ($prompt escape? (rename tag) (relabel handler))))))
      (define (visit-fun fun)
        (rewrite-cps-exp fun
          (($ $fun src meta free body)
           ($fun src meta (map rename free) ,(visit-cont body)))))
      (visit-fun fun))))
