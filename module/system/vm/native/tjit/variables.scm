;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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

;;; CPS variable resolution and register alloocatoion.  Applying naive strategy
;;; to assign registers to locals, does nothing sophisticated such as
;;; linear-scan, binpacking, or graph coloring.

;;; Code:

(define-module (system vm native tjit variables)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps2)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit registers)
  #:export (resolve-vars
            resolve-variables
            loop-start
            ref? ref-value ref-type
            constant? constant
            register? gpr gpr? fpr fpr?
            memory?))

;;;
;;; Variable
;;;

(define (ref? x)
  (and (pair? x) (symbol? (car x))))

(define (ref-value x)
  (and (ref? x) (cdr x)))

(define (ref-type x)
  (and (ref? x) (car x)))

(define (make-constant x)
  (cons 'const x))

(define (constant? x)
  (eq? 'const (ref-type x)))

(define (constant x)
  (let ((val (ref-value x)))
    (cond
     ((and (number? val) (exact? val))
      (make-pointer (ref-value x)))
     (else
      (scm->pointer val)))))

(define (make-gpr x)
  (cons 'gpr x))

(define (gpr x)
  (register-ref (ref-value x)))

(define (gpr? x)
  (eq? 'gpr (ref-type x)))

(define (make-fpr x)
  (cons 'fpr x))

(define (fpr x)
  (fpr-ref (ref-value x)))

(define (fpr? x)
  (eq? 'fpr (ref-type x)))

(define (register? x)
  (or (eq? 'gpr (ref-type x))
      (eq? 'fpr (ref-type x))))

(define (make-memory x)
  (cons 'mem x))

(define (memory? x)
  (eq? 'mem (ref-type x)))

(define (resolve-variable-types cps max-var)
  "Resolve type of register used for variables appearing in CPS.

Returns a vector with length MAX-VAR + 1, vector values are one of: value `int'
or `double' from (system foreign), or '(const . ${val}) where ${val} is a
constant value."
  (let ((types (make-vector (+ max-var 1) #f))
        (entry-start-syms '())
        (entry-end-syms '())
        (loop-start-syms '())
        (loop-end-syms '())
        (kend #f))                      ; internal states.

    (define (set-entry-syms!)
      ;; Entry cont is always in intmap ref 2.
      (match (intmap-ref cps 2)
        (($ $kargs _ syms ($ $continue knext _ exp))
         (for-each (lambda (sym)
                     (vector-set! types sym int))
                   syms)
         (set! entry-start-syms syms))
        (_
         (error "resolve-variable-types: cps without entry clause."))))

    (define kstart (loop-start cps))

    (define (set-type! ty i)
      (when (not (vector-ref types i))
        (vector-set! types i ty)))

    (define (set-types! ty . is)
      (map (lambda (i) (set-type! ty i)) is))

    ;; Module (system vm native tjit assembler) imports this module, using `@'.
    (define (lookup-prim-type op)
      (hashq-ref (@ (system vm native tjit assembler) *prim-types*) op))

    (define (expr-type op dst args)
      (cond
       ((lookup-prim-type op)
        =>
        (lambda (tys)
          (let ((vals (append dst args)))
            (cond
             ((= (length vals) (length tys))
              (for-each (lambda (ty val)
                          (when (not (eq? void ty))
                            (set-type! ty val)))
                        tys vals))
             (else
              (debug 2 "*** expr-type: arity mismatch in ~a~%" op))))))
       (else
        (debug 2 "*** expr-type: `~a' not found~%" op))))

    (define (visit-exp syms exp)
      (match exp
        (($ $branch kt exp)
         (visit-exp syms exp)
         (visit-cont #f kt))
        (($ $primcall op args)
         (expr-type op syms args))
        (($ $const val)
         (match syms
           ((sym)
            (vector-set! types sym (make-constant val)))
           (_
            #f)))
        (#f #f)
        (_
         (debug 2 ";;; match to exp: ~a~%" exp))))

    (define (visit-cont exp k)
      (match (intmap-ref cps k)
        (($ $kfun _ _ self _ knext)
         (visit-cont #f knext))
        (($ $kclause _ knext kalternate)
         (visit-cont #f knext))
        (($ $kargs names syms ($ $continue knext _ enext))
         (visit-exp syms exp)
         (cond
          ((< knext k)
           (match enext
             (($ $values vals)
              (set! loop-end-syms vals)
              (set! kend k))
             (_
              (debug 2 "loop with no values?: ~a~%" exp))))
          ((= knext kstart)
           (match enext
             (($ $values vals)
              (set! entry-end-syms vals)
              (visit-cont #f knext))
             (_
              ;; Entry-end-syms could be null. Possible to emit something else
              ;; than $values, when the loop takes single argument.
              (visit-cont enext knext))))
          ((= kstart k)
           (set! loop-start-syms syms)
           (visit-cont enext knext))
          (else
           (visit-cont enext knext))))
        (($ $ktail)
         (values))))

    (set-entry-syms!)
    (visit-cont #f 0)

    ;; (debug 2 ";;; var-types:~%")
    ;; (let ((end (vector-length types)))
    ;;   (let lp ((i 0))
    ;;     (when (< i end)
    ;;       (debug 2 "  ~3@a: ~a~%" i (vector-ref types i))
    ;;       (lp (+ i 1)))))

    (debug 2 ";;; entry-start-syms: ~{~2@a ~}~%" entry-start-syms)
    (debug 2 ";;; entry-end-syms:   ~{~2@a ~}~%" entry-end-syms)
    (debug 2 ";;; loop-start-syms:  ~{~2@a ~}~%" loop-start-syms)
    (debug 2 ";;; loop-end-syms:    ~{~2@a ~}~%" loop-end-syms)

    (values types
            kstart kend
            entry-start-syms entry-end-syms
            loop-start-syms loop-end-syms)))

(define (resolve-variables cps locals max-var)
  "Assign registers and memory addresses for variables in CPS.

Returns initial local and register alist with LOCAL, and vector of length
MAX-VAR + 1 which contains register and memory information."

  (let ((gpr-idx 0)
        (fpr-idx 0)
        (mem-idx 0)
        (vars (make-vector (+ max-var 1) #f)))

    (define (var-ref i)
      (vector-ref vars i))

    (define (var-set! i x)
      (vector-set! vars i x))

    (define (next-mem)
      (let ((m (make-memory mem-idx)))
        (set! mem-idx (+ mem-idx 1))
        m))

    (define (next-gpr)
      (cond
       ((< gpr-idx *num-registers*)
        (let ((r (make-gpr gpr-idx)))
          (set! gpr-idx (+ gpr-idx 1))
          r))
       (else (next-mem))))

    (define (next-fpr)
      (cond
       ((< fpr-idx *num-fpr*)
        (let ((r (make-fpr fpr-idx)))
          (set! fpr-idx (+ fpr-idx 1))
          r))
       (else (next-mem))))

    (define (local-var-alist locals vars)
      (map cons (reverse locals) vars))

    (let-values (((types
                   kstart kend
                   entry-start-syms entry-end-syms
                   loop-start-syms loop-end-syms)
                  (resolve-variable-types cps max-var)))

      (define (type i)
        (vector-ref types i))

      (define (gpr? i)
        (eq? int (type i)))

      (define (fpr? i)
        (eq? double (type i)))

      (define *loop-min-sym*
        (apply min loop-start-syms))

      (define *loop-max-sym*
        (apply max loop-end-syms))

      (define *loop-end-table*
        (let ((t (make-hash-table)))
          (for-each (lambda (sym)
                      (let ((i (hashq-ref t sym 0)))
                        (hashq-set! t sym (+ i 1))))
                    loop-end-syms)
          t))

      (define (loop-end-shareable? loop-start-sym loop-end-sym)
        (and (< (hashq-ref *loop-end-table* loop-end-sym) 2)
             ;; XXX: Compare with constant?
             (eq? (type loop-start-sym) (type loop-end-sym))))

      (define (entry-end-shareable? entry-end-sym loop-start-sym)
        (not (= entry-end-sym loop-start-sym)))

      ;; Assign loop start syms.
      (for-each
       (lambda (sym)
         (unless (var-ref sym)
           (cond
            ((gpr? sym) (var-set! sym (next-gpr)))
            ((fpr? sym) (var-set! sym (next-fpr)))
            (else
             (error "resolve variable failed in loop-start" sym)))))
       loop-start-syms)

      ;; Assign loop end syms.
      ;;
      ;; Commented for now, need to do more analysis to detect the
      ;; `shareable'-ness of variables at the end loop. A failing case was
      ;; variables 8 and 11 in:
      ;;
      ;; 0010    (kargs (v0 v1) (8 9) (continue 11 (const 65)))
      ;; 0011  > (kargs (val) (10) (continue 15 (branch 12 (primcall %lt 8 10))))
      ;; 0012    (kargs () () (continue 13 (primcall %fxadd1 8)))
      ;; 0013    (kargs (val) (11) (continue 14 (primcall %fxadd 9 8)))
      ;; 0014    (kargs (val) (12) (continue 10 (values 11 12)))
      ;;
      ;; In above case, 8 and 11 could not be shared, because of the `add' in
      ;; line 0013. If 8 and 11 were shared, `add' in 0013 will add the
      ;; incremented value which would be stored in variable 11, which is
      ;; different from the original value in variable 8.

      ;; (for-each
      ;;  (lambda (loop-end loop-start)
      ;;    (cond
      ;;     ((var-ref loop-end)
      ;;      (values))                    ; Assigned already.
      ;;     ((constant? (type loop-end))
      ;;      (var-set! loop-end (type loop-end)))
      ;;     ((loop-end-shareable? loop-start loop-end)
      ;;      (var-set! loop-end (var-ref loop-start)))
      ;;     ((gpr? loop-end)
      ;;      (var-set! loop-end (next-gpr)))
      ;;     ((fpr? loop-end)
      ;;      (var-set! loop-end (next-fpr)))
      ;;     (else
      ;;      (error "resolve-variables failed in loop-end" loop-end))))
      ;;  loop-end-syms loop-start-syms)

      ;; Assign to variables inside loop body.
      (let lp ((i *loop-min-sym*))
        (when (< i *loop-max-sym*)
          (let ((ty (type i)))
            (cond
             ((var-ref i) (values))     ; Assigned already.
             ((constant? ty) (var-set! i ty))
             ((gpr? i) (var-set! i (next-gpr)))
             ((fpr? i) (var-set! i (next-fpr)))
             (else
              (error "resolve-variable failed in loop body"))))
          (lp (+ i 1))))

      ;; Assign shareable registers between loop start and entry end.
      ;; Entry-end-syms could be null, but loop-start-syms is not.
      (when (= (length entry-end-syms) (length loop-start-syms))
        (for-each
         (lambda (entry-end loop-start)
           (cond
            ((entry-end-shareable? entry-end loop-start)
             (var-set! entry-end (var-ref loop-start)))
            ((gpr? entry-end)
             (var-set! entry-end (next-gpr)))
            ((fpr? entry-end)
             (var-set! entry-end (next-fpr)))
            (else
             (error "resolve-variable failed in entry-end" entry-end))))
         entry-end-syms loop-start-syms))

      ;; Rest of values.
      (let lp ((i 1))
        (when (< i (+ max-var 1))
          (let ((ty (type i)))
            (cond
             ((var-ref i) (values))
             ((constant? ty) (var-set! i ty))
             ((gpr? i) (var-set! i (next-gpr)))
             ((fpr? i) (var-set! i (next-fpr)))
             (else (error "resolve-variables at rest" i))))
          (lp (+ i 1))))

      (debug 2 ";;; vars:~%")
      (let lp ((i 0))
        (when (< i (+ max-var 1))
          (debug 2 ";;; ~3@a: ~a~%" i (vector-ref vars i))
          (lp (+ i 1))))

      (values vars
              (local-var-alist locals entry-start-syms)
              loop-start-syms))))

;;;
;;; Loop start
;;;

(define* (loop-start cps #:optional (kfun 0))
  (define (go k)
    (match (intmap-ref cps k)
      (($ $kfun src meta self tail next)
       (go next))
      (($ $kclause arity next)
       (go next))
      (($ $kargs names syms ($ $continue next src ($ $branch kt exp)))
       (or (and (< next k) next)
           (go kt)
           (go next)))
      (($ $kargs names syms ($ $continue next src exp))
       (or (and (< next k) next)
           (go next)))
      (($ $ktail)
       #f)
      (($ $kreceive _ _ next)
       (go next))))
  (go kfun))
