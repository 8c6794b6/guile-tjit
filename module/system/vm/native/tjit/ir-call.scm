;;; ANF IR for call and return

;;;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.
;;;;
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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA
;;;;

;;; Commentary:
;;;
;;; Module containing ANF IR definitions for call and return operations.
;;;
;;; Code:

(define-module (system vm native tjit ir-call)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
  #:use-module (system vm program))

;;; XXX: halt is not defined, might not necessary.

(define-ir (call proc nlocals)
  ;; When procedure get inlined, taking snapshot of previous frame.
  ;; Contents of previous frame could change in native code. Note that
  ;; frame return address will get checked at the time of `%return'.
  ;;
  ;; Refilling dynamic link and return address.  These two locals would be
  ;; restored with values in snapshot when taiking side exit. An `%eq' guard
  ;; is added to test the procedure value, to bailout when procedure has been
  ;; redefined.
  ;;
  ;; Call to C subroutines (a.k.a: primitive-code) are inlined, will not emit
  ;; native operation.
  ;;
  (let* ((sp-offset (current-sp-offset))
         (stack-size (vector-length locals))
         (fp (- stack-size proc))
         (dst-ptr (make-pointer (+ ip (* 2 4))))
         (rra (cons (+ sp-offset fp) (make-return-address dst-ptr)))
         (rdl (cons (+ sp-offset fp 1) (make-dynamic-link proc)))
         (vra (var-ref fp))
         (vdl (var-ref (+ fp 1)))
         (vproc (var-ref (- fp 1)))
         (rproc (local-ref (- fp 1)))
         (primitive? (and (program? rproc)
                          (primitive-code? (program-code rproc))))
         (snapshot (take-snapshot! ip 0)))
    (push-outline! (ir-outline ir) rdl rra sp-offset locals)
    `(let ((_ ,snapshot))
       (let ((_ (%eq ,vproc ,(pointer-address (scm->pointer rproc)))))
         ,(if (and (< 0 (current-fp-offset))
                   (not primitive?))
              `(let ((_ (%scall ,proc)))
                 ,(next))
              (next))))))

;; XXX: call-label
(define-ir (call-label proc nlocals label)
  (nyi "call-label"))

(define-ir (tail-call nlocals)
  (let* ((stack-size (vector-length locals))
         (proc-index (- stack-size 1))
         (proc/v (var-ref proc-index))
         (proc/l (local-ref proc-index))
         (proc-addr (pointer-address (scm->pointer proc/l))))
    `(let ((_ (%eq ,proc/v ,proc-addr)))
       ,(next))))

(define-ir (tail-call-label nlocals label)
  (next))

(define-syntax gen-receive-thunk
  (syntax-rules ()
    ((_ proc skip-var?)
     (let* ((return-subr? (ir-return-subr? ir))
            (stack-size (vector-length locals))
            (sp-offset (current-sp-offset))
            (min-local-index (+ (- stack-size proc 1) sp-offset 2))
            (max-local-index (+ stack-size sp-offset))
            (se-type (lambda (i e n)
                       (cond
                        ((eq? 'f64 e) &f64)
                        ((eq? 'u64 e) &u64)
                        ((eq? 's64 e) &s64)
                        ((eq? 'scm e) (type-of (stack-element locals i e)))
                        (else
                         (tjitc-error 'receive "unknown type ~s at ~s" e n))))))
       (lambda ()
         (set-ir-return-subr! ir #f)

         ;; Ignoring `unspecified' values when loading from previous
         ;; frame. Those values might came from dead slots in stack
         ;; which were overwritten by gc. See `scm_i_vm_mark_stack' in
         ;; "libguile/vm.c".
         ;;
         ;; XXX: Add tests to check that this strategy works with
         ;; explicitly given `unspecified' values.
         ;;
         (if (or (<= (current-fp-offset) 0)
                 return-subr?)
             (next)
             `(let ((_ ,(take-snapshot! ip 0)))
                ,(let lp ((vars (reverse (ir-vars ir))))
                   (match vars
                     (((n . var) . vars)
                      (if (skip-var? var)
                          (lp vars)
                          (cond
                           ((< min-local-index n max-local-index)
                            (let* ((i (- n sp-offset))
                                   (e (outline-type-ref (ir-outline ir) n))
                                   (t (se-type i e n)))
                              (if (eq? t &unspecified)
                                  (lp vars)
                                  (with-frame-ref lp vars var t n))))
                           (else
                            (lp vars)))))
                     (()
                      (next)))))))))))

(define-ir (receive dst proc nlocals)
  (let* ((stack-size (vector-length locals))
         (dst/i (- stack-size dst 1))
         (dst/v (var-ref dst/i))
         (src/i (- (- stack-size proc) 2))
         (src/v (var-ref src/i))
         (thunk (gen-receive-thunk proc (lambda (v) (eq? v dst/v))))
         (last-local-index (- (vector-length locals) 1)))
    ;; Update values in local, so that snapshot can resolve value from stack
    ;; element type.
    (when (and (<= 0 src/i last-local-index)
               (<= 0 dst/i last-local-index))
      (vector-set! locals dst/i (scm->pointer (local-ref src/i))))
    `(let ((,dst/v ,src/v))
       ,(thunk))))

(define-ir (receive-values proc allow-extra? nlocals)
  (let ((thunk (gen-receive-thunk proc (lambda _ #f))))
    (thunk)))

;; XXX: tail-call/shuffle

(define-ir (return-values nlocals)
  ;; Two locals below callee procedure in VM frame contain dynamic link and
  ;; return address. VM interpreter refills these two with #f, doing the same
  ;; thing in `emit-next'.
  ;;
  (let ((ra/val (make-return-address (make-pointer ra)))
        (dl/val (make-dynamic-link dl))
        (stack-size (vector-length locals)))
    (set-outline-previous-dl-and-ra! (ir-outline ir) stack-size
                                     ra/val dl/val))
  (let ((snapshot (take-snapshot! ip 0)))
    (pop-outline! (ir-outline ir) (current-sp-offset) locals)
    (set-ir-return-subr! ir #f)
    `(let ((_ ,snapshot))
       ,(if (< (current-fp-offset) 0)
            (next)
            (let* ((stack-size (vector-length locals))
                   (vra (var-ref stack-size))
                   (vdl (var-ref (+ stack-size 1))))
              `(let ((_ (%return ,ra)))
                 (let ((,vra #f))
                   (let ((,vdl #f))
                     ,(next)))))))))
