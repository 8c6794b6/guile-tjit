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
;;; Some bytecode operations can encode an immediate as an operand.
;;; This pass tranforms generic primcalls to these specialized
;;; primcalls, if possible.
;;;
;;; Code:

(define-module (language cps specialize-primcalls)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps utils)
  #:use-module (language cps intmap)
  #:export (specialize-primcalls))

(define (specialize-primcalls conts)
  (let ((constants (compute-constant-values conts)))
    (define (u8? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (exact-integer? val) (<= 0 val 255))))
    (define (specialize-primcall name args)
      (define (rename name)
        (build-exp ($primcall name args)))
      (match (cons name args)
        (('make-vector (? u8? n) init) (rename 'make-vector/immediate))
        (('vector-ref v (? u8? n)) (rename 'vector-ref/immediate))
        (('vector-set! v (? u8? n) x) (rename 'vector-set!/immediate))
        (('allocate-struct v (? u8? n)) (rename 'allocate-struct/immediate))
        (('struct-ref s (? u8? n)) (rename 'struct-ref/immediate))
        (('struct-set! s (? u8? n) x) (rename 'struct-set!/immediate))
        (('add x (? u8? y)) (build-exp ($primcall 'add/immediate (x y))))
        (('add (? u8? x) y) (build-exp ($primcall 'add/immediate (y x))))
        (('sub x (? u8? y)) (build-exp ($primcall 'sub/immediate (x y))))
        (('uadd x (? u8? y)) (build-exp ($primcall 'uadd/immediate (x y))))
        (('uadd (? u8? x) y) (build-exp ($primcall 'uadd/immediate (y x))))
        (('usub x (? u8? y)) (build-exp ($primcall 'usub/immediate (x y))))
        (('umul x (? u8? y)) (build-exp ($primcall 'umul/immediate (x y))))
        (('umul (? u8? x) y) (build-exp ($primcall 'umul/immediate (y x))))
        (_ #f)))
    (intmap-map
     (lambda (label cont)
       (match cont
         (($ $kargs names vars ($ $continue k src ($ $primcall name args)))
          (let ((exp* (specialize-primcall name args)))
            (if exp*
                (build-cont
                  ($kargs names vars ($continue k src ,exp*)))
                cont)))
         (_ cont)))
     conts)))
