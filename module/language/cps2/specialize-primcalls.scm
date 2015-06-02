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

(define-module (language cps2 specialize-primcalls)
  #:use-module (ice-9 match)
  #:use-module (language cps2)
  #:use-module (language cps2 utils)
  #:use-module (language cps intmap)
  #:export (specialize-primcalls))

(define (specialize-primcalls conts)
  (let ((constants (compute-constant-values conts)))
    (define (immediate-u8? var)
      (let ((val (intmap-ref constants var (lambda (_) #f))))
        (and (exact-integer? val) (<= 0 val 255))))
    (define (specialize-primcall name args)
      (match (cons name args)
        (('make-vector (? immediate-u8? n) init) 'make-vector/immediate)
        (('vector-ref v (? immediate-u8? n)) 'vector-ref/immediate)
        (('vector-set! v (? immediate-u8? n) x) 'vector-set!/immediate)
        (('allocate-struct v (? immediate-u8? n)) 'allocate-struct/immediate)
        (('struct-ref s (? immediate-u8? n)) 'struct-ref/immediate)
        (('struct-set! s (? immediate-u8? n) x) 'struct-set!/immediate)
        (_ #f)))
    (intmap-map
     (lambda (label cont)
       (match cont
         (($ $kargs names vars ($ $continue k src ($ $primcall name args)))
          (let ((name* (specialize-primcall name args)))
            (if name*
                (build-cont
                  ($kargs names vars
                    ($continue k src ($primcall name* args))))
                cont)))
         (_ cont)))
     conts)))
