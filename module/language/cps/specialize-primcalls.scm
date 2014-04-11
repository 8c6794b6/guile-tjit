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
;;; Some bytecode operations can encode an immediate as an operand.
;;; This pass tranforms generic primcalls to these specialized
;;; primcalls, if possible.
;;;
;;; Code:

(define-module (language cps specialize-primcalls)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps dfg)
  #:export (specialize-primcalls))

(define (specialize-primcalls fun)
  (let ((dfg (match fun
               (($ $fun free body)
                (compute-dfg body #:global? #t)))))
    (with-fresh-name-state-from-dfg dfg
      (define (immediate-u8? sym)
        (call-with-values (lambda () (find-constant-value sym dfg))
          (lambda (has-const? val)
            (and has-const? (integer? val) (exact? val) (<= 0 val 255)))))
      (define (visit-cont cont)
        (rewrite-cps-cont cont
          (($ $cont sym ($ $kargs names syms body))
           (sym ($kargs names syms ,(visit-term body))))
          (($ $cont sym ($ $kfun src meta self tail clause))
           (sym ($kfun src meta self ,tail
                  ,(and clause (visit-cont clause)))))
          (($ $cont sym ($ $kclause arity body alternate))
           (sym ($kclause ,arity ,(visit-cont body)
                          ,(and alternate (visit-cont alternate)))))
          (($ $cont)
           ,cont)))
      (define (visit-term term)
        (rewrite-cps-term term
          (($ $letk conts body)
           ($letk ,(map visit-cont conts)
             ,(visit-term body)))
          (($ $letrec names syms funs body)
           ($letrec names syms (map visit-fun funs)
                    ,(visit-term body)))
          (($ $continue k src (and fun ($ $fun)))
           ($continue k src ,(visit-fun fun)))
          (($ $continue k src ($ $primcall name args))
           ,(visit-primcall k src name args))
          (($ $continue)
           ,term)))
      (define (visit-primcall k src name args)
        ;; If we introduce a VM op from a primcall without a VM op, we
        ;; will need to ensure that the return arity matches.  Rely on the
        ;; elide-values pass to clean up.
        (define-syntax-rule (adapt-void exp)
          (let-fresh (k* kvoid) (val)
            (build-cps-term
              ($letk ((k* ($kargs ('val) (val)
                            ($continue k src ($primcall 'values (val)))))
                      (kvoid ($kargs () ()
                               ($continue k* src ($void)))))
                ($continue kvoid src exp)))))
        (define-syntax-rule (adapt-val exp)
          (let-fresh (k*) (val)
            (build-cps-term
              ($letk ((k* ($kargs ('val) (val)
                            ($continue k src ($primcall 'values (val))))))
                ($continue k* src exp)))))
        (match (cons name args)
          (('make-vector (? immediate-u8? n) init)
           (adapt-val ($primcall 'make-vector/immediate (n init))))
          (('vector-ref v (? immediate-u8? n))
           (build-cps-term
             ($continue k src ($primcall 'vector-ref/immediate (v n)))))
          (('vector-set! v (? immediate-u8? n) x)
           (build-cps-term
             ($continue k src ($primcall 'vector-set!/immediate (v n x)))))
          (('allocate-struct v (? immediate-u8? n))
           (adapt-val ($primcall 'allocate-struct/immediate (v n))))
          (('struct-ref s (? immediate-u8? n))
           (adapt-val ($primcall 'struct-ref/immediate (s n))))
          (('struct-set! s (? immediate-u8? n) x)
           ;; Unhappily, and undocumentedly, struct-set! returns the value
           ;; that was set.  There is code that relies on this.  Hackety
           ;; hack...
           (let-fresh (k*) ()
             (build-cps-term
               ($letk ((k* ($kargs () ()
                             ($continue k src ($primcall 'values (x))))))
                 ($continue k* src ($primcall 'struct-set!/immediate (s n x)))))))
          (_ 
           (build-cps-term ($continue k src ($primcall name args))))))

      (define (visit-fun fun)
        (rewrite-cps-exp fun
          (($ $fun free body)
           ($fun free ,(visit-cont body)))))

      (visit-fun fun))))
