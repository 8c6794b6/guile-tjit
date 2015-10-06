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
        (error "compile-primlist: ref not found" k))))
    (define (constant? x)
      (cond
       ((boolean? x) #t)
       ((char? x) #t)
       ((number? x) #t)
       (else #f)))
    (define (constant v)
      `(const . ,v))
    (define (compile-term term acc)
      (debug 1 ";;; compile-term: term=~a~%" term)
      (match term
        (('lambda args rest)
         (debug 1 ";;;   snapshot-id: ~a~%" snapshot-id)
         (when assign-args?
           (let ((snapshot (hashq-ref snapshots snapshot-id)))
             (debug 1 ";;;   snapshot: ~a~%" snapshot)
             (let lp ((args args)
                      (local-x-types (snapshot-locals snapshot))
                      (acc '()))
               (match (list args local-x-types)
                 (((arg . args) ((local . type) . local-x-types))
                  (let ((var (if (eq? type &flonum)
                                 (gen-fpr)
                                 (gen-gpr))))
                    (hashq-set! env arg var)
                    (lp args local-x-types (cons var acc))))
                 (_
                  (set! arg-vars (reverse! acc)))))))
         (compile-term rest acc))
        (('begin term1 term2)
         (let ((acc (compile-term term1 acc)))
           (compile-term term2 acc)))
        (('let ((dst (? constant? val))) term1)
         (let ((prim `(%move ,(ref dst) ,(constant val))))
           (compile-term term1 (cons prim acc))))
        (('let ((dst (? symbol? src))) term1)
         (let ((prim `(%move ,(ref dst) ,(ref src))))
           (compile-term term1 (cons prim acc))))
        (('let (('_ ('%snap id . args))) term1)
         (let ((prim `(%snap ,id ,@(map ref args))))
           (set! snapshot-id id)
           (compile-term term1 (cons prim acc))))
        (('let (('_ (op . args))) term1)
         (let ((prim `(,op ,@(map ref args))))
           (compile-term term1 (cons prim acc))))
        (('let ((dst (op . args))) term1)
         (set-types! op dst args)
         (let ((arg-regs (map ref args)))
           (set-dst-type! op dst)
           (let* ((dst-reg (ref dst))
                  (prim `(,op ,dst-reg ,@arg-regs)))
             (compile-term term1 (cons prim acc)))))
        (('loop . _)
         acc)
        (('%snap id . args)
         (set! snapshot-id id)
         (cons `(%snap ,id ,@(map ref args)) acc))
        (((? symbol? op) . args)
         (set-types! op #f args)
         (cons `(,op ,@(map ref args)) acc))
        (()
         acc)))
    (let ((plist (compile-term term '())))
      (debug 1 ";;; compile-term: at end snapshot-id is ~a~%" snapshot-id)
      (values (reverse! plist) gpr-idx fpr-idx mem-idx snapshot-id arg-vars))))
