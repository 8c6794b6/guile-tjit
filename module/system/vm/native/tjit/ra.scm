;;;; Compile ANF IR to list of primitive operations

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
;;;
;;; Assign registers to ANF IR, compile to list of primitive operations.
;;;
;;; Code:

(define-module (system vm native tjit ra)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit variables)
  #:use-module (system vm native tjit registers)
  #:export ($primlist
            primlist?
            primlist-entry
            primlist-loop
            primlist-initial-locals
            anf->primlist))

;;;
;;; Record type
;;;

;; Record type to hold lists of primitives.
(define-record-type $primlist
  (make-primlist entry loop initial-locals)
  primlist?
  ;; List of primitives for entry clause.
  (entry primlist-entry)

  ;; List of primitives for loop body.
  (loop primlist-loop)

  ;; Initial locals, if any. Used in side trace.
  (initial-locals primlist-initial-locals))


;;;
;;; ANF to Primitive List
;;;

(define (anf->primlist vars term)
  (debug 1 ";;; anf->primlist:~%")
  (debug 1 ";;;   vars: ~a~%" vars)
  (let ((env (make-hash-table)))
    (match term
      (`(letrec ((entry ,entry)
                 (loop ,loop))
          entry)
       (let*-values (((entry-asm gpr-idx fpr-idx mem-idx arg-vars)
                      (compile-primitive entry env 0 0 0 #f))
                     ((loop-asm gpr-idx fpr-idx mem-idx arg-vars)
                      (compile-primitive loop env gpr-idx fpr-idx mem-idx #f)))
         (make-primlist entry-asm loop-asm '())))
      (`(letrec ((patch ,patch))
          patch)
       (let-values (((patch-asm gpr-idx fpr-idx mem-idx initial-locals)
                     (compile-primitive patch env 0 0 0 #t)))
         (make-primlist patch-asm '() initial-locals)))
      (_
       (error "anf->primlist: malformed term" term)))))

(define (compile-primitive term env gpr-idx fpr-idx mem-idx assign-args?)
  "Compile ANF form TERM to list of primitive operations."
  (let ((arg-vars '()))
    (define (lookup-prim-type op)
      (hashq-ref (@ (system vm native tjit assembler) *native-prim-types*) op))
    (define (gen-mem)
      (let ((ret (make-memory mem-idx)))
        (set! mem-idx (+ 1 mem-idx))
        ret))
    (define (gen-gpr)
      (if (< gpr-idx *num-gpr*)
          (let ((ret (make-gpr gpr-idx)))
            (set! gpr-idx (+ 1 gpr-idx))
            ret)
          (gen-mem)))
    (define (gen-fpr)
      (if (< fpr-idx *num-fpr*)
          (let ((ret (make-fpr fpr-idx)))
            (set! fpr-idx (+ 1 fpr-idx))
            ret)
          (gen-mem)))
    (define (set-types! op dst args)
      (let ((types (lookup-prim-type op)))
        (let lp ((types (if dst
                            (cdr types)
                            types))
                 (args args))
          (match (list types args)
            (((type . types) (arg . args))
             (if (symbol? arg)
                 (cond
                  ((hashq-ref env arg)
                   => (lambda (reg)
                        (debug 1 ";;; set-types!: ~a already assigned to ~a~%"
                               arg reg)))
                  ((= type int)
                   (debug 1 ";;; set-types!: ~a to gpr ~a~%" arg gpr-idx)
                   (hashq-set! env arg (gen-gpr)))
                  ((= type double)
                   (debug 1 ";;; set-types!: ~a to fpr ~a~%" arg fpr-idx)
                   (hashq-set! env arg (gen-fpr)))
                  (else
                   (debug 1 ";;; set-types!: unknown type ~a~%" type)))
                 (debug 1 ";;; set-types!: skipping constant ~a~%" arg)))
            (_
             (values))))))
    (define (set-dst-type! op dst)
      (let ((type (car (lookup-prim-type op)))
            (assigned (hashq-ref env dst)))
        ;; Assign new register. Overwrite register used for dst if type
        ;; differs.
        (if (and assigned
                 (or (and (= type int)
                          (not (fpr? assigned)))
                     (and (= type double)
                          (not (gpr? assigned)))))
            (debug 1 ";;; set-dst-type!: dst already assigned and same type~%")
            (cond
             ((= type int)
              (hashq-set! env dst (gen-gpr)))
             ((= type double)
              (hashq-set! env dst (gen-fpr)))
             (else
              (debug 1 ";;; set-dst-types!: ~a unknown type ~a~%" dst type))))))
    (define (ref k)
      (cond
       ((constant? k) (make-constant k))
       ((symbol? k) (hashq-ref env k))
       (else
        (error "compile-primitive: ref not found" k))))
    (define (constant? x)
      (cond
       ((boolean? x) #t)
       ((char? x) #t)
       ((number? x) #t)
       (else #f)))
    (define (constant v)
      `(const . ,v))
    (define (compile-term term acc)
      (match term
        (('lambda args rest)
         (debug 1 ";;; compile-term: args=~a~%" args)
         (when assign-args?
           (set! arg-vars (let lp ((args args) (acc '()))
                            (match args
                              ((arg . args)
                               (let ((var (gen-gpr)))
                                 (hashq-set! env arg var)
                                 (lp args (cons var acc))))
                              (()
                               (reverse! acc))))))
         (compile-term rest acc))
        (('begin term1 rest)
         (compile-term rest (compile-term term1 acc)))
        (('let ((dst (? constant? val))) term1)
         (compile-term term1 (cons `(%move ,(ref dst) ,(constant val)) acc)))
        (('let ((dst (? symbol? src))) term1)
         (compile-term term1 (cons `(%move ,(ref dst) ,(ref src)) acc)))
        (('let ((dst (op . args))) term1)
         (set-types! op dst args)
         (let ((arg-regs (map ref args)))
           (set-dst-type! op dst)
           (let ((dst-reg (ref dst)))
             (compile-term term1 (cons `(,op ,dst-reg ,@arg-regs) acc)))))
        (('loop . _)
         acc)
        (('%snap id . args)
         (cons `(%snap ,id ,@(map ref args)) acc))
        (((? symbol? op) . args)
         (set-types! op #f args)
         (cons `(,op ,@(map ref args)) acc))
        (()
         acc)))
    (let ((plist (reverse! (compile-term term '()))))
      (values plist gpr-idx fpr-idx mem-idx arg-vars))))
