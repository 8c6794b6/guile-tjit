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
  #:use-module (language bytecode)
  #:export (reify-primitives))

(define (module-box src module name public? bound? val-proc)
  (let-fresh (kbox) (module-var name-var public?-var bound?-var box)
    (build-cps-term
      ($letconst (('module module-var module)
                  ('name name-var name)
                  ('public? public?-var public?)
                  ('bound? bound?-var bound?))
        ($letk ((kbox ($kargs ('box) (box) ,(val-proc box))))
          ($continue kbox src
            ($primcall 'cached-module-box
                       (module-var name-var public?-var bound?-var))))))))

(define (primitive-module name)
  (case name
    ((bytevector-length

      bytevector-u8-ref bytevector-u8-set!
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
  (let-fresh () (idx-var)
    (build-cps-term
      ($letconst (('idx idx-var idx))
        ($continue k src
          ($primcall 'builtin-ref (idx-var)))))))

(define (reify-clause ktail)
  (let-fresh (kclause kbody kthrow) (wna false str eol throw)
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
                        ,(primitive-ref 'throw kthrow #f)))))
                 ,#f)))))

(define (reify-primitives/1 fun single-value-conts)
  (define (visit-clause cont)
    (rewrite-cps-cont cont
      (($ $cont label ($ $kclause arity body alternate))
       (label ($kclause ,arity ,(visit-cont body)
                      ,(and alternate (visit-clause alternate)))))))
  (define (visit-cont cont)
    (rewrite-cps-cont cont
      (($ $cont label ($ $kargs (name) (var) body))
       ,(begin
          (bitvector-set! single-value-conts label #t)
          (build-cps-cont
            (label ($kargs (name) (var) ,(visit-term body))))))
      (($ $cont label ($ $kargs names vars body))
       (label ($kargs names vars ,(visit-term body))))
      (($ $cont)
       ,cont)))
  (define (visit-term term)
    (match term
      (($ $letk conts body)
       ;; Visit continuations before their uses.
       (let ((conts (map visit-cont conts)))
         (build-cps-term
           ($letk ,conts ,(visit-term body)))))
      (($ $continue k src exp)
       (match exp
         (($ $prim name)
          (if (bitvector-ref single-value-conts k)
              (cond
               ((builtin-name->index name)
                => (lambda (idx)
                     (builtin-ref idx k src)))
               (else (primitive-ref name k src)))
              (build-cps-term ($continue k src ($void)))))
         (($ $primcall 'call-thunk/no-inline (proc))
          (build-cps-term
            ($continue k src ($call proc ()))))
         (($ $primcall name args)
          (cond
           ((or (prim-instruction name) (branching-primitive? name))
            ;; Assume arities are correct.
            term)
           (else
            (let-fresh (k*) (v)
              (build-cps-term
                ($letk ((k* ($kargs (v) (v)
                              ($continue k src ($call v args)))))
                  ,(cond
                    ((builtin-name->index name)
                     => (lambda (idx)
                          (builtin-ref idx k* src)))
                    (else (primitive-ref name k* src)))))))))
         (_ term)))))

  (rewrite-cps-cont fun
    (($ $cont label ($ $kfun src meta self (and tail ($ $cont ktail)) #f))
     ;; A case-lambda with no clauses.  Reify a clause.
     (label ($kfun src meta self ,tail ,(reify-clause ktail))))
    (($ $cont label ($ $kfun src meta self tail clause))
     (label ($kfun src meta self ,tail ,(visit-clause clause))))))

(define (reify-primitives term)
  (with-fresh-name-state term
    (let ((single-value-conts (make-bitvector (label-counter) #f)))
      (rewrite-cps-term term
        (($ $program procs)
         ($program ,(map (lambda (cont)
                           (reify-primitives/1 cont single-value-conts))
                         procs)))))))
