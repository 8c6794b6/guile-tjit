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
         (proc/v (var-ref (- fp 1)))
         (proc/l (local-ref (- fp 1)))
         (primitive? (and (program? proc/l)
                          (primitive-code? (program-code proc/l))))
         (snapshot
          (begin
            (vector-set! locals (- stack-size proc) (scm->pointer #f))
            (vector-set! locals (+ (- stack-size proc) 1) (scm->pointer #f))
            (take-snapshot! ip 0))))
    (push-outline! (ir-outline ir) rdl rra sp-offset locals)
    `(let ((_ ,snapshot))
       (let ((_ (%eq ,proc/v ,(pointer-address (scm->pointer proc/l)))))
         ,(if (and (< 0 (current-fp-offset))
                   (not primitive?))
              `(let ((_ (%scall ,proc)))
                 ,(next))
              (next))))))

(define-ir (call-label proc nlocals label)
  (let* ((sp-offset (current-sp-offset))
         (stack-size (vector-length locals))
         (fp (- stack-size proc))
         (dst-ptr (make-pointer (+ ip (* 2 4))))
         (rra (cons (+ sp-offset fp) (make-return-address dst-ptr)))
         (rdl (cons (+ sp-offset fp 1) (make-dynamic-link proc))))
    (push-outline! (ir-outline ir) rdl rra sp-offset locals)
    (if (< 0 (current-fp-offset))
        `(let ((_ (%scall ,proc)))
           ,(next))
        (next))))

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

(define-ir (receive dst proc nlocals)
  (let* ((stack-size (vector-length locals))
         (dst/i (- stack-size dst 1))
         (dst/v (var-ref dst/i))
         (src/i (- (- stack-size proc) 2))
         (src/v (var-ref src/i))
         (thunk (gen-receive-thunk proc #t (lambda (v) (eq? v dst/v))))
         (last-local-index (- (vector-length locals) 1)))
    ;; Update values in local, so that snapshot can resolve value from stack
    ;; element type.
    (when (and (<= 0 src/i last-local-index)
               (<= 0 dst/i last-local-index))
      (vector-set! locals dst/i (scm->pointer (local-ref src/i))))
    `(let ((,dst/v ,src/v))
       ,(thunk))))

(define-ir (receive-values proc allow-extra? nlocals)
  (let ((thunk (gen-receive-thunk proc #t (lambda _ #f))))
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
                     (let ((_ ,(take-snapshot! ra 0 #t)))
                       ,(next))))))))))
