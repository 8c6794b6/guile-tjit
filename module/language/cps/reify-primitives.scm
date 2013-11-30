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
;;; A pass to reify lone $prim's that were never folded into a
;;; $primcall, and $primcall's to primitives that don't have a
;;; corresponding VM op.
;;;
;;; Code:

(define-module (language cps reify-primitives)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:use-module (language cps primitives)
  #:use-module (language rtl)
  #:export (reify-primitives))

(define (module-box src module name public? bound? val-proc)
  (let-gensyms (module-sym name-sym public?-sym bound?-sym kbox box)
    (build-cps-term
      ($letconst (('module module-sym module)
                  ('name name-sym name)
                  ('public? public?-sym public?)
                  ('bound? bound?-sym bound?))
        ($letk ((kbox ($kargs ('box) (box) ,(val-proc box))))
          ($continue kbox src
            ($primcall 'cached-module-box
                       (module-sym name-sym public?-sym bound?-sym))))))))

(define (primitive-module name)
  (case name
    ((bytevector-u8-ref bytevector-u8-set!
      bytevector-s8-ref bytevector-s8-set!

      bytevector-u16-ref bytevector-u16-set!
      bytevector-u16-native-ref bytevector-u16-native-set!
      bytevector-s16-ref bytevector-s16-set!
      bytevector-s16-native-ref bytevector-s16-native-set!

      bytevector-u32-ref bytevector-u32-set!
      bytevector-u32-native-ref bytevector-u32-native-set!
      bytevector-s32-ref bytevector-s32-set!
      bytevector-s32-native-ref bytevector-s32-native-set!

      bytevector-u64-ref bytevector-u64-set!
      bytevector-u64-native-ref bytevector-u64-native-set!
      bytevector-s64-ref bytevector-s64-set!
      bytevector-s64-native-ref bytevector-s64-native-set!

      bytevector-ieee-single-ref bytevector-ieee-single-set!
      bytevector-ieee-single-native-ref bytevector-ieee-single-native-set!
      bytevector-ieee-double-ref bytevector-ieee-double-set!
      bytevector-ieee-double-native-ref bytevector-ieee-double-native-set!)
     '(rnrs bytevectors))
    ((class-of) '(oop goops))
    (else '(guile))))

(define (primitive-ref name k src)
  (module-box #f (primitive-module name) name #f #t
              (lambda (box)
                (build-cps-term
                  ($continue k src ($primcall 'box-ref (box)))))))

(define (builtin-ref idx k src)
  (let-gensyms (idx-sym)
    (build-cps-term
      ($letconst (('idx idx-sym idx))
        ($continue k src
          ($primcall 'builtin-ref (idx-sym)))))))

(define (reify-clause ktail)
  (let-gensyms (kclause kbody wna false str eol kthrow throw)
    (build-cps-cont
      (kclause ($kclause ('() '() #f '() #f)
                 (kbody
                  ($kargs () ()
                    ($letconst (('wna wna 'wrong-number-of-args)
                                ('false false #f)
                                ('str str "Wrong number of arguments")
                                ('eol eol '()))
                      ($letk ((kthrow
                               ($kargs ('throw) (throw)
                                 ($continue ktail #f
                                   ($call throw
                                          (wna false str eol false))))))
                        ,(primitive-ref 'throw kthrow #f))))))))))

;; FIXME: Operate on one function at a time, for efficiency.
(define (reify-primitives fun)
  (let ((conts (build-cont-table fun)))
    (define (visit-fun term)
      (rewrite-cps-exp term
        (($ $fun src meta free body)
         ($fun src meta free ,(visit-cont body)))))
    (define (visit-cont cont)
      (rewrite-cps-cont cont
        (($ $cont sym ($ $kargs names syms body))
         (sym ($kargs names syms ,(visit-term body))))
        (($ $cont sym ($ $kentry self (and tail ($ $cont ktail)) ()))
         ;; A case-lambda with no clauses.  Reify a clause.
         (sym ($kentry self ,tail (,(reify-clause ktail)))))
        (($ $cont sym ($ $kentry self tail clauses))
         (sym ($kentry self ,tail ,(map visit-cont clauses))))
        (($ $cont sym ($ $kclause arity body))
         (sym ($kclause ,arity ,(visit-cont body))))
        (($ $cont)
         ,cont)))
    (define (visit-term term)
      (rewrite-cps-term term
        (($ $letk conts body)
         ($letk ,(map visit-cont conts) ,(visit-term body)))
        (($ $continue k src exp)
         ,(match exp
            (($ $prim name)
             (match (lookup-cont k conts)
               (($ $kargs (_))
                (cond
                 ((builtin-name->index name)
                  => (lambda (idx)
                       (builtin-ref idx k src)))
                 (else (primitive-ref name k src))))
               (_ (build-cps-term ($continue k src ($void))))))
            (($ $fun)
             (build-cps-term ($continue k src ,(visit-fun exp))))
            (($ $primcall 'call-thunk/no-inline (proc))
             (build-cps-term
               ($continue k src ($call proc ()))))
            (($ $primcall name args)
             (cond
              ((or (prim-rtl-instruction name) (branching-primitive? name))
               ;; Assume arities are correct.
               term)
              (else
               (let-gensyms (k* v)
                 (build-cps-term
                   ($letk ((k* ($kargs (v) (v)
                                 ($continue k src ($call v args)))))
                     ,(cond
                       ((builtin-name->index name)
                        => (lambda (idx)
                             (builtin-ref idx k* src)))
                       (else (primitive-ref name k* src)))))))))
            (_ term)))))

    (visit-fun fun)))
