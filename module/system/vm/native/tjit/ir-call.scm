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
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit state)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables)
  #:use-module (system vm program))

(define-syntax-rule (scan-call ol proc nlocals label?)
  (let* ((stack-size (vector-length locals))
         (sp-offset (outline-sp-offset ol))
         (sp-proc (- stack-size proc 1)))
    (unless label?
      (set-scan-read! ol sp-proc)
      (set-entry-type! ol sp-proc &procedure))
    (set-scan-read! ol (+ sp-proc 1) (+ sp-proc 2))
    (set-scan-write! ol (+ sp-proc 1) (+ sp-proc 2))
    (do ((n 1 (+ n 1))) ((<= nlocals n))
      (set-entry-type! ol (- (+ sp-proc sp-offset) n) &scm))
    (set-scan-initial-fields! ol)
    (push-scan-fp-offset! ol proc)
    (push-scan-sp-offset! ol (- (+ proc nlocals) stack-size))))

(define-syntax-rule (ti-call ol proc nlocals label?)
  ;; Bytecode `call' changes SP after saving for current SP offset, see
  ;; `scan-call' above.
  (let* ((stack-size (vector-length locals))
         (sp-offset (if (outline-initialized? ol)
                        (outline-sp-offset ol)
                        (car (outline-sp-offsets ol))))
         (fp (- stack-size proc))
         (sp-proc (- stack-size proc 1))
         (ra-ty (make-return-address
                 (make-pointer (+ ip (* 4 (if label? 3 2))))))
         (dl-ty (make-dynamic-link proc)))
    (unless label?
      (set-expected-type! ol (+ sp-proc sp-offset) &procedure))
    (do ((n 0 (+ n 1))) ((<= nlocals n))
      (match (assq-ref (outline-inferred-types ol) n)
        (('copy . dst)
         (set-entry-type! ol dst &scm)
         (set-expected-type! ol dst &scm))
        (_
         (values)))
      (set-expected-type! ol (- (+ sp-proc sp-offset) n) &scm))
    (set-inferred-type! ol (+ sp-offset fp) ra-ty)
    (set-inferred-type! ol (+ sp-offset fp 1) dl-ty)
    (do ((n 0 (+ n 1))) ((<= nlocals n))
      (match (assq-ref (outline-inferred-types ol) n)
        (('copy . dst)
         (set-inferred-type! ol n &scm))
        (_
         (values)))
      (set-expected-type! ol (- (+ sp-proc sp-offset) n) &scm))))

;;; XXX: halt is not defined, might not necessary.

(define-anf (call proc nlocals)
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
         (proc/l (scm-ref (- fp 1)))
         (snapshot (take-snapshot! ip 0))
         (inlineable (or (and (program? proc/l)
                              (primitive-code? (program-code proc/l)))
                         (and (<= (current-fp-offset) 0)
                              (not (tj-linking-roots? (ir-tj ir)))))))
    `(let ((_ ,snapshot))
       (let ((_ (%eq ,proc/v ,(pointer-address (scm->pointer proc/l)))))
         ,(if inlineable
              (next)
              `(let ((_ (%scall ,proc)))
                 ,(next)))))))

(define-scan (call ol proc nlocals)
  (scan-call ol proc nlocals #f))

(define-ti (call ol proc nlocals)
  (ti-call ol proc nlocals #f))

(define-anf (call-label proc nlocals label)
  (let* ((sp-offset (current-sp-offset))
         (stack-size (vector-length locals))
         (fp (- stack-size proc))
         (dst-ptr (make-pointer (+ ip (* 2 4))))
         (rra (cons (+ sp-offset fp) (make-return-address dst-ptr)))
         (rdl (cons (+ sp-offset fp 1) (make-dynamic-link proc)))
         (inlineable (and (< 0 (current-fp-offset))
                          (not (tj-linking-roots? (ir-tj ir))))))
    (if inlineable
        `(let ((_ (%scall ,proc)))
           ,(next))
        (next))))

(define-scan (call-label ol proc nlocals label)
  (scan-call ol proc nlocals #t))

(define-ti (call-label ol proc nlocals label)
  (ti-call ol proc nlocals #t))

(define-syntax-rule (scan-tail-call ol nlocals label?)
  (let* ((stack-size (vector-length locals))
         (proc-sp (- stack-size 1)))
    (unless label?
      (set-expected-type! ol proc-sp &procedure))
    (set-scan-write! ol proc-sp)
    (set-scan-initial-fields! ol)
    (push-scan-sp-offset! ol (- nlocals stack-size))))

(define-anf (tail-call nlocals)
  (let* ((stack-size (vector-length locals))
         (proc-index (- stack-size 1))
         (proc/v (var-ref proc-index))
         (proc/l (scm-ref proc-index))
         (proc-addr (pointer-address (scm->pointer proc/l))))
    `(let ((_ (%eq ,proc/v ,proc-addr)))
       ,(next))))

(define-scan (tail-call ol nlocals)
  (scan-tail-call ol nlocals #f))

(define-anf (tail-call-label nlocals label)
  (next))

(define-scan (tail-call-label ol nlocals label)
  (scan-tail-call ol nlocals #t))

(define-anf (receive dst proc nlocals)
  (let* ((stack-size (vector-length locals))
         (dst/i (- stack-size dst 1))
         (dst/v (var-ref dst/i))
         (src/i (- (- stack-size proc) 2))
         (src/v (var-ref src/i))
         (thunk (gen-load-thunk proc nlocals (lambda (v) (eq? v dst/v)))))
    `(let ((,dst/v ,src/v))
       ,(thunk))))

(define-scan (receive ol dst proc nlocals)
  (let* ((stack-size (vector-length locals))
         (sp-offset (outline-sp-offset ol))
         (fp (- stack-size proc)))
    (set-scan-write! ol (- stack-size dst 1))
    (set-scan-read! ol (- stack-size proc 2))
    (let ((new-offsets (cons (outline-sp-offset ol)
                             (outline-sp-offsets ol))))
      (set-outline-sp-offsets! ol new-offsets))
    (pop-scan-sp-offset! ol (- stack-size nlocals))
    (set-scan-write! ol (- nlocals dst 1))
    (let ((new-fp-offsets (cons (outline-fp-offset ol)
                                (outline-fp-offsets ol)))
          (writes (outline-write-indices ol))
          (buf (outline-write-buf ol)))
      (set-outline-fp-offsets! ol new-fp-offsets)
      (set-outline-write-buf! ol (cons writes buf)))))

(define-ti (receive ol dst proc nlocals)
  ;; Bytecode `receive' changes SP after saving for current SP offset, see
  ;; `define-scan' for `receive' above.
  (let* ((stack-size (vector-length locals))
         (sp-offset (if (outline-initialized? ol)
                        (outline-sp-offset ol)
                        (car (outline-sp-offsets ol))))
         (proc/i (+ (- stack-size proc 2) sp-offset))
         (dst/i (+ (- stack-size dst 1) sp-offset))
         (ty (or (assq-ref (outline-inferred-types ol) proc/i)
                 (assq-ref (outline-expected-types ol) proc/i)
                 `(copy . ,proc/i))))
    (set-inferred-type! ol dst/i ty)))

(define-anf (receive-values proc allow-extra? nvalues)
  (let ((thunk (gen-load-thunk proc nvalues (const #f))))
    (thunk)))

(define-scan (receive-values ol proc allow-extra? nvalues)
  (if (= nvalues 1)
      (let* ((stack-size (vector-length locals))
             (fp (- stack-size proc 1)))
        (do ((n nvalues (- n 1))) (<= n 0)
          (set-scan-read! ol (- fp n)))
        (set-scan-initial-fields! ol))
      (begin
        (debug 1 "NYI: receive-values ~a ~a ~a~%" proc allow-extra? nvalues)
        (values #f 'receive-values))))

(define-ti (receive-values ol proc allow-extra? nvalues)
  ;; XXX: Should infer multiple values.
  (values))

;; XXX: tail-call/shuffle

(define-anf (return-values nlocals)
  ;; Two locals below callee procedure in VM frame contain dynamic link and
  ;; return address. VM interpreter refills these two with #f, doing the same
  ;; thing in `emit-next'.
  ;;
  (let* ((ra/val (make-return-address (make-pointer ra)))
         (dl/val (make-dynamic-link dl))
         (stack-size (vector-length locals))
         (inlineable (< (current-fp-offset) 0))
         (snapshot (take-snapshot! ip 0)))
    (set-ir-return-subr! ir #f)
    `(let ((_ ,snapshot))
       ,(if inlineable
            (next)
            (let* ((stack-size (vector-length locals))
                   (vra (var-ref stack-size))
                   (vdl (var-ref (+ stack-size 1))))
              `(let ((_ (%return ,ra)))
                 (let ((,vra #f))
                   (let ((,vdl #f))
                     (let ((_ ,(take-snapshot! ra 0 #t)))
                       ,(next))))))))))

(define-scan (return-values ol nlocals)
  (let* ((sp-offset (outline-sp-offset ol))
         (stack-size (vector-length locals))
         (ra-offset stack-size)
         (dl-offset (+ ra-offset 1)))
    (set-scan-read! ol ra-offset dl-offset)
    (set-scan-write! ol ra-offset dl-offset)
    (do ((n nlocals (- n 1))) ((< n 2))
      (set-scan-read! ol (- stack-size n)))
    (set-scan-initial-fields! ol)
    (pop-scan-sp-offset! ol (- stack-size nlocals))
    (pop-scan-fp-offset! ol dl)))

(define-ti (return-values ol nlocals)
  ;; As in `call' and `receive', `return-values' shifts SP offset, see
  ;; `define-scan' for `return-values' above.
  (let* ((stack-size (vector-length locals))
         (sp-offset (if (outline-initialized? ol)
                        (outline-sp-offset ol)
                        (car (outline-sp-offsets ol))))
         (ra-offset (+ sp-offset stack-size))
         (dl-offset (+ ra-offset 1)))
    (set-inferred-type! ol ra-offset &false)
    (set-inferred-type! ol dl-offset &false)))
