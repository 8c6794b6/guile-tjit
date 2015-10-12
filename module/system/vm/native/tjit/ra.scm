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
;;; Applying naive strategy to assign registers to locals, does nothing
;;; sophisticated such as linear-scan, binpacking, or graph coloring.
;;;
;;; Code:

(define-module (system vm native tjit ra)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (language cps types)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit variables)
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

(define (anf->primlist vars snapshots term)
  (debug 1 ";;; anf->primlist:~%")
  (debug 1 ";;;   vars: ~a~%" vars)
  (let ((env (make-hash-table)))
    (match term
      (`(letrec ((entry ,entry)
                 (loop ,loop))
          entry)
       (let*-values (((entry-asm gpr-idx fpr-idx mem-idx snapshot-idx arg-vars)
                      (compile-primlist entry env 0 0 0 0 snapshots #t))
                     ((loop-asm gpr-idx fpr-idx mem-idx snapshot-idx arg-vars)
                      (compile-primlist loop env gpr-idx fpr-idx mem-idx
                                        snapshot-idx snapshots #f)))
         (make-primlist entry-asm loop-asm '())))
      (`(letrec ((patch ,patch))
          patch)
       (let-values (((patch-asm gpr-idx fpr-idx mem-idx snapshot-idx arg-vars)
                     (compile-primlist patch env 0 0 0 0 snapshots #t)))
         (make-primlist patch-asm '() arg-vars)))
      (_
       (error "anf->primlist: malformed term" term)))))

(define (compile-primlist term env gpr-idx fpr-idx mem-idx
                          snapshot-id snapshots assign-args?)
  "Compile ANF term to list of primitive operations."
  (let ((arg-vars '()))
    (define (lookup-prim-type op)
      (hashq-ref (@ (system vm native tjit assembler) *native-prim-types*) op))
    (define-syntax-rule (gen-var proc idx)
      (let ((ret (proc idx)))
        (set! idx (+ 1 idx))
        ret))
    (define-syntax-rule (gen-mem)
      (gen-var make-memory mem-idx))
    (define-syntax-rule (gen-gpr)
      (if (< gpr-idx *num-gpr*)
          (gen-var make-gpr gpr-idx)
          (gen-mem)))
    (define-syntax-rule (gen-fpr)
      (if (< fpr-idx *num-fpr*)
          (gen-var make-fpr fpr-idx)
          (gen-mem)))
    (define-syntax-rule (set-env! gen var)
      (let ((ret (gen)))
        (hashq-set! env var ret)
        ret))
    (define-syntax-rule (get-mem! var)
      (set-env! gen-mem var))
    (define-syntax-rule (get-gpr! var)
      (set-env! gen-gpr var))
    (define-syntax-rule (get-fpr! var)
      (set-env! gen-fpr var))
    (define (get-arg-types! op dst args)
      (let ((types (lookup-prim-type op)))
        (let lp ((types (if dst
                            (cdr types)
                            types))
                 (args args)
                 (acc '()))
          (match (list types args)
            (((type . types) (arg . args))
             (cond
              ((constant? arg)
               (debug 1 ";;; get-arg-types!: got constant ~a~%" arg)
               (lp types args (cons (make-constant arg) acc)))
              ((symbol? arg)
               (cond
                ((hashq-ref env arg)
                 => (lambda (reg)
                      (debug 1 ";;; get-arg-types!: ~a already assigned to ~a~%"
                             arg reg)
                      (lp types args (cons reg acc))))
                ((= type int)
                 (debug 1 ";;; get-arg-types!: ~a to gpr ~a~%" arg gpr-idx)
                 (lp types args (cons (get-gpr! arg) acc)))
                ((= type double)
                 (debug 1 ";;; get-arg-types!: ~a to fpr ~a~%" arg fpr-idx)
                 (lp types args (cons (get-fpr! arg) acc)))
                (else
                 (debug 1 ";;; get-arg-types!: unknown type ~a~%" type)
                 (lp types args acc))))
              (else
               (error "set-types!: unknown arg with type" arg type))))
            (_
             (reverse! acc))))))
    (define (get-dst-type! op dst)
      ;; Assign new register. Overwrite register used for dst if type
      ;; differs.
      (let ((type (car (lookup-prim-type op)))
            (assigned (hashq-ref env dst)))
        (cond
         ((and assigned
               (or (and (= type int)
                        (not (fpr? assigned)))
                   (and (= type double)
                        (not (gpr? assigned)))))
          (debug 1 ";;; get-dst-type!: dst already assigned and same type~%")
          assigned)
         ((= type int)
          (get-gpr! dst))
         ((= type double)
          (get-fpr! dst))
         (else
          (error "get-dst-types!: unknown type~%" dst type)))))
    (define (ref k)
      (cond
       ((constant? k) (make-constant k))
       ((symbol? k) (hashq-ref env k))
       (else
        (error "compile-primlist: ref not found" k))))
    (define (constant? x)
      (cond
       ((boolean? x) #t)
       ((char? x) #t)
       ((number? x) #t)
       (else #f)))
    (define (compile-term term acc)
      (match term
        (('lambda args rest)
         (when assign-args?
           (let lp ((args args)
                    (local-x-types (snapshot-locals
                                    (hashq-ref snapshots snapshot-id)))
                    (acc '()))
             (match (list args local-x-types)
               (((arg . args) ((local . type) . local-x-types))
                (let ((var (if (eq? type &flonum)
                               (get-fpr! arg)
                               (get-gpr! arg))))
                  (lp args local-x-types (cons var acc))))
               (_
                (set! arg-vars (reverse! acc))))))
         (compile-term rest acc))
        (('let (('_ ('%snap id . args))) term1)
         (let ((prim `(%snap ,id ,@(map ref args))))
           (set! snapshot-id id)
           (compile-term term1 (cons prim acc))))
        (('let (('_ (op . args))) term1)
         (let ((prim `(,op ,@(get-arg-types! op #f args))))
           (compile-term term1 (cons prim acc))))
        (('let ((dst (? constant? val))) term1)
         (let* ((reg (cond
                      ((ref dst) => identity)
                      ((flonum? val) (get-fpr! dst))
                      (else (get-gpr! dst))))
                (prim `(%move ,reg ,(make-constant val))))
           (compile-term term1 (cons prim acc))))
        (('let ((dst (? symbol? src))) term1)
         (let* ((src-reg (ref src))
                (dst-reg (cond
                          ((ref dst) => identity)
                          ((gpr? src-reg) (get-gpr! dst))
                          ((fpr? src-reg) (get-fpr! dst))
                          ((memory? src-reg) (get-mem! dst))))
                (prim `(%move ,dst-reg ,src-reg)))
           (compile-term term1 (cons prim acc))))
        (('let ((dst (op . args))) term1)
         ;; Set and get argument types before destination type.
         (let* ((arg-regs (get-arg-types! op dst args))
                (prim `(,op ,(get-dst-type! op dst) ,@arg-regs)))
           (compile-term term1 (cons prim acc))))
        (('loop . _)
         acc)
        ('_
         acc)
        (()
         acc)))

    (let ((plist (compile-term term '())))
      (values (reverse! plist) gpr-idx fpr-idx mem-idx snapshot-id arg-vars))))
