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

(define-syntax-rule (scan-call proc nlocals label?)
  (let* ((stack-size (vector-length locals))
         (sp-offset (outline-sp-offset outline))
         (sp-proc (- stack-size proc 1)))
    (unless label?
      (set-scan-read! outline sp-proc)
      (set-entry-type! outline sp-proc &procedure))
    (set-scan-read! outline (+ sp-proc 1) (+ sp-proc 2))
    (set-scan-write! outline (+ sp-proc 1) (+ sp-proc 2))
    (do ((n 1 (+ n 1))) ((<= nlocals n))
      (let ((i (- (+ sp-proc sp-offset) n)))
        (set-entry-type! outline i &scm)))
    (do ((n 0 (+ n 1))) ((<= nlocals n))
      (let ((i (- (+ sp-proc sp-offset) n)))
        (match (assq-ref (outline-inferred-types outline) i)
          (('copy . dst)
           (set-entry-type! outline dst &scm))
          (_
           (values)))))
    (set-scan-initial-fields! outline)
    (push-scan-fp-offset! outline proc)
    (push-scan-sp-offset! outline (- (+ proc nlocals) stack-size))))

(define-syntax-rule (ti-call proc nlocals label?)
  ;; Bytecode `call' changes SP after saving for current SP offset, see
  ;; `scan-call' above.
  (let* ((stack-size (vector-length locals))
         (sp-offset (if (outline-initialized? outline)
                        (outline-sp-offset outline)
                        (car (outline-sp-offsets outline))))
         (fp (- stack-size proc))
         (sp-proc (- stack-size proc 1))
         (ra-ty (make-return-address
                 (make-pointer (+ ip (* 4 (if label? 3 2))))))
         (dl-ty (make-dynamic-link proc)))
    (set-inferred-type! outline (+ sp-offset fp) ra-ty)
    (set-inferred-type! outline (+ sp-offset fp 1) dl-ty)
    (do ((n 0 (+ n 1))) ((<= nlocals n))
      (match (assq-ref (outline-inferred-types outline) n)
        (('copy . dst)
         (set-inferred-type! outline n &scm))
        (_
         (values))))))

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
                              (not (tj-linking-roots? tj))))))
    `(let ((_ ,snapshot))
       (let ((_ (%eq ,proc/v ,(pointer-address (scm->pointer proc/l)))))
         ,(if inlineable
              (next)
              `(let ((_ (%scall ,proc)))
                 ,(next)))))))

(define-scan (call proc nlocals)
  (scan-call proc nlocals #f))

(define-ti (call proc nlocals)
  (ti-call proc nlocals #f))

(define-anf (call-label proc nlocals label)
  (let* ((sp-offset (current-sp-offset))
         (stack-size (vector-length locals))
         (fp (- stack-size proc))
         (dst-ptr (make-pointer (+ ip (* 2 4))))
         (rra (cons (+ sp-offset fp) (make-return-address dst-ptr)))
         (rdl (cons (+ sp-offset fp 1) (make-dynamic-link proc)))
         (inlineable (and (< 0 (current-fp-offset))
                          (not (tj-linking-roots? tj)))))
    (if inlineable
        `(let ((_ (%scall ,proc)))
           ,(next))
        (next))))

(define-scan (call-label proc nlocals label)
  (scan-call proc nlocals #t))

(define-ti (call-label ol proc nlocals label)
  (ti-call proc nlocals #t))

(define-syntax-rule (scan-tail-call nlocals label?)
  (let* ((stack-size (vector-length locals))
         (proc-sp (- stack-size 1)))
    (unless label?
      (set-entry-type! outline proc-sp &procedure))
    (set-scan-write! outline proc-sp)
    (set-scan-initial-fields! outline)
    (push-scan-sp-offset! outline (- nlocals stack-size))))

(define-anf (tail-call nlocals)
  (let* ((stack-size (vector-length locals))
         (proc-index (- stack-size 1))
         (proc/v (var-ref proc-index))
         (proc/l (scm-ref proc-index))
         (proc-addr (pointer-address (scm->pointer proc/l))))
    `(let ((_ (%eq ,proc/v ,proc-addr)))
       ,(next))))

(define-scan (tail-call nlocals)
  (scan-tail-call nlocals #f))

(define-anf (tail-call-label nlocals label)
  (next))

(define-scan (tail-call-label nlocals label)
  (scan-tail-call nlocals #t))

(define-anf (receive dst proc nlocals)
  (let* ((stack-size (vector-length locals))
         (dst/i (- stack-size dst 1))
         (dst/v (var-ref dst/i))
         (src/i (- (- stack-size proc) 2))
         (src/v (var-ref src/i))
         (thunk (gen-load-thunk proc nlocals (lambda (v) (eq? v dst/v)))))
    `(let ((,dst/v ,src/v))
       ,(thunk))))

(define-scan (receive dst proc nlocals)
  (let* ((stack-size (vector-length locals))
         (sp-offset (outline-sp-offset outline))
         (fp (- stack-size proc)))
    (set-scan-write! outline (- stack-size dst 1))
    (set-scan-read! outline (- stack-size proc 2))
    (let ((new-offsets (cons (outline-sp-offset outline)
                             (outline-sp-offsets outline))))
      (set-outline-sp-offsets! outline new-offsets))
    (pop-scan-sp-offset! outline (- stack-size nlocals))
    (set-scan-write! outline (- nlocals dst 1))
    (let ((new-fp-offsets (cons (outline-fp-offset outline)
                                (outline-fp-offsets outline)))
          (writes (outline-write-indices outline))
          (buf (outline-write-buf outline)))
      (set-outline-fp-offsets! outline new-fp-offsets)
      (set-outline-write-buf! outline (cons writes buf)))))

(define-ti (receive dst proc nlocals)
  ;; Bytecode `receive' changes SP after saving for current SP offset, see
  ;; `define-scan' for `receive' above.
  (let* ((stack-size (vector-length locals))
         (sp-offset (if (outline-initialized? outline)
                        (outline-sp-offset outline)
                        (car (outline-sp-offsets outline))))
         (proc/i (+ (- stack-size proc 2) sp-offset))
         (dst/i (+ (- stack-size dst 1) sp-offset))
         (ty (or (assq-ref (outline-inferred-types outline) proc/i)
                 `(copy . ,proc/i))))
    (set-inferred-type! outline dst/i ty)))

(define-anf (receive-values proc allow-extra? nvalues)
  (let ((thunk (gen-load-thunk proc nvalues (const #f))))
    (thunk)))

(define-scan (receive-values proc allow-extra? nvalues)
  (if (= nvalues 1)
      (let* ((stack-size (vector-length locals))
             (fp (- stack-size proc 1)))
        (do ((n nvalues (- n 1))) (<= n 0)
          (set-scan-read! outline (- fp n)))
        (set-scan-initial-fields! outline))
      (begin
        (debug 1 "NYI: receive-values ~a ~a ~a~%" proc allow-extra? nvalues)
        (values #f 'receive-values))))

(define-ti (receive-values proc allow-extra? nvalues)
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

(define-scan (return-values nlocals)
  (let* ((sp-offset (outline-sp-offset outline))
         (stack-size (vector-length locals))
         (ra-offset stack-size)
         (dl-offset (+ ra-offset 1)))
    (set-scan-read! outline ra-offset dl-offset)
    (set-scan-write! outline ra-offset dl-offset)
    (do ((n nlocals (- n 1))) ((< n 2))
      (set-scan-read! outline (- stack-size n)))
    (set-scan-initial-fields! outline)
    (pop-scan-sp-offset! outline (- stack-size nlocals))
    (pop-scan-fp-offset! outline dl)))

(define-ti (return-values nlocals)
  ;; As in `call' and `receive', `return-values' shifts SP offset, see
  ;; `define-scan' for `return-values' above.
  (let* ((stack-size (vector-length locals))
         (sp-offset (if (outline-initialized? outline)
                        (outline-sp-offset outline)
                        (car (outline-sp-offsets outline))))
         (ra-offset (+ sp-offset stack-size))
         (dl-offset (+ ra-offset 1)))
    (set-inferred-type! outline ra-offset &false)
    (set-inferred-type! outline dl-offset &false)))
