;;;; Assign resiters to IR

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
;;; Assign registers to ANF IR, compile to list of primitive operations.
;;; Applying naive strategy to assign registers to locals, does nothing
;;; sophisticated such as linear-scan, binpacking, graph coloring, etc.
;;;
;;; Code:

(define-module (language trace ra)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (language trace error)
  #:use-module (language trace fragment)
  #:use-module (language trace env)
  #:use-module (language trace registers)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables)
  #:export ($primops
            primops?
            primops-entry
            primops-loop
            primops-nspills
            anf->primops

            storage-ref
            storage-set!
            fold-storage))


;;;; Data types

;; Record type to hold lists of primitives.
(define-record-type $primops
  (make-primops entry loop nspills storage)
  primops?
  ;; List of primitives for entry clause.
  (entry primops-entry)
  ;; List of primitives for loop body.
  (loop primops-loop)
  ;; Number of spilled variables.
  (nspills primops-nspills)
  ;; Hash-table containing variable information.
  (storage primops-storage))

(define-inlinable (%make-storage alist)
  (cons 'storage alist))

(define-inlinable (storage-alist storage)
  (cdr storage))

(define-inlinable (set-storage-alist! storage alist)
  (set-cdr! storage alist))

(define-inlinable (storage-ref storage key)
  (assq-ref (storage-alist storage) key))

(define-inlinable (storage-set! storage key val)
  (cond
   ((assq key (storage-alist storage))
    => (lambda (handle)
         (set-cdr! handle val)))
   (else
    (set-storage-alist! storage (cons (cons key val)
                                      (storage-alist storage))))))

(define-inlinable (fold-storage proc init storage)
  (let lp ((alist (storage-alist storage)) (init init))
    (if (null? alist)
        init
        (let ((kv (car alist)))
          (lp (cdr alist) (proc (car kv) (cdr kv) init))))))

(define-inlinable (make-storage env free-gprs free-fprs mem-idx)
  (let ((alist '())
        (parent-storage (and=> (env-parent-fragment env)
                               fragment-storage)))
    (%make-storage
     (if parent-storage
         (let lp ((parent-alist (storage-alist parent-storage))
                  (acc '()))
           (match parent-alist
             (((k . v) . parent-alist)
              (cond
               ((not v) (lp parent-alist acc))
               ((gpr? v)
                (let ((i (ref-value v)))
                  (when (<= 0 i)
                    (vector-set! free-gprs i #f))
                  (lp parent-alist (acons k v acc))))
               ((fpr? v)
                (let ((i (ref-value v)))
                  (when (<= 0 i)
                    (vector-set! free-fprs i #f))
                  (lp parent-alist (acons k v acc))))
               ((memory? v)
                (let ((i (ref-value v)))
                  (when (<= (variable-ref mem-idx) i)
                    (variable-set! mem-idx (+ i 1)))
                  (lp parent-alist (acons k v acc))))
               (else
                (failure 'make-storage "unknown var ~s" v))))
             (() acc)))
         (let* ((alist (acons (make-tmpvar 0) (make-gpr -1) alist))
                (alist (acons (make-tmpvar 1) (make-gpr -2) alist))
                (alist (acons (make-tmpvar 2) (make-gpr -3) alist))
                (alist (acons (make-tmpvar/f 0) (make-fpr -1) alist))
                (aistt (acons (make-tmpvar/f 1) (make-fpr -2) alist))
                (alist (acons (make-tmpvar/f 2) (make-fpr -3) alist))
                (alist (acons (make-spill 0) (make-memory -1) alist)))
           alist)))))

;;; Alternative implementation of storage using hash-table.

;; (define-inlinable (storage-ref storage key)
;;   (hashq-ref storage key))
;;
;; (define-inlinable (storage-set! storage key val)
;;   (hashq-set! storage key val))
;;
;; (define-inlinable (fold-storage proc init storage)
;;   (hash-fold proc init storage))
;;
;; (define-inlinable (make-storage env free-gprs free-fprs mem-idx)
;;   (let ((storage (make-hash-table))
;;         (parent-storage (and=> (env-parent-fragment env)
;;                                fragment-storage)))
;;     (if parent-storage
;;         ;; Share registers and memory offset for side trace with parent
;;         ;; trace.
;;         (hash-for-each
;;          (lambda (k v)
;;            (cond
;;             ((not v) (values))
;;             ((gpr? v)
;;              (storage-set! storage k v)
;;              (let ((i (ref-value v)))
;;                (when (<= 0 i)
;;                  (vector-set! free-gprs i #f))))
;;             ((fpr? v)
;;              (storage-set! storage k v)
;;              (let ((i (ref-value v)))
;;                (when (<= 0 i)
;;                  (vector-set! free-fprs i #f))))
;;             ((memory? v)
;;              (storage-set! storage k v)
;;              (let ((i (ref-value v)))
;;                (when (<= (variable-ref mem-idx) i)
;;                  (variable-set! mem-idx (+ i 1)))))
;;             (else
;;              (failure 'anf->primops "unknown var ~s" v))))
;;          parent-storage)
;;         (begin
;;           (storage-set! storage (make-tmpvar 0) (make-gpr -1))
;;           (storage-set! storage (make-tmpvar 1) (make-gpr -2))
;;           (storage-set! storage (make-tmpvar 2) (make-gpr -3))
;;           (storage-set! storage (make-tmpvar/f 0) (make-fpr -1))
;;           (storage-set! storage (make-tmpvar/f 1) (make-fpr -2))
;;           (storage-set! storage (make-tmpvar/f 2) (make-fpr -3))
;;           (storage-set! storage (make-spill 0) (make-memory -1))))
;;     storage))


;;;; Auxiliary

(define-syntax-rule (make-initial-free-gprs)
  (make-vector *num-gpr* #t))

(define-syntax-rule (make-initial-free-fprs)
  (make-vector *num-fpr* #t))

(define-syntax define-register-acquire!
  (syntax-rules ()
    ((_ name num constructor)
     (define (name free-regs)
       (let lp ((i 0))
         (cond
          ((= i num) #f)
          ((vector-ref free-regs i)
           (let ((ret (constructor i)))
             (vector-set! free-regs i #f)
             ret))
          (else (lp (+ i 1)))))))))

(define-register-acquire! acquire-gpr! *num-gpr* make-gpr)
(define-register-acquire! acquire-fpr! *num-fpr* make-fpr)

(define-syntax-parameter mem-idx
  (lambda (x)
    (syntax-violation 'mem-idx "mem-idx undefined" x)))

(define-syntax-parameter free-gprs
  (lambda (x)
    (syntax-violation 'free-gprs "free-gprs undefined" x)))

(define-syntax-parameter free-fprs
  (lambda (x)
    (syntax-violation 'free-fprs "free-fprs undefined" x)))

(define-syntax-parameter storage
  (lambda (x)
    (syntax-violation 'storage "storage undefined" x)))

(define-syntax-rule (gen-mem)
  (let ((ret (make-memory (variable-ref mem-idx))))
    (variable-set! mem-idx (+ 1 (variable-ref mem-idx)))
    ret))

(define-syntax-rule (set-storage! gen var)
  (let ((ret gen))
    (storage-set! storage var ret)
    ret))

(define-syntax-rule (get-mem! var)
  (set-storage! (gen-mem) var))

(define-syntax-rule (get-gpr! var)
  (set-storage! (or (acquire-gpr! free-gprs) (gen-mem)) var))

(define-syntax-rule (get-fpr! var)
  (set-storage! (or (acquire-fpr! free-fprs) (gen-mem)) var))

(define (assign-registers term snapshots arg-storage arg-free-gprs
                          arg-free-fprs arg-mem-idx snapshot-id)
  "Compile ANF term to list of primitive operations."
  (syntax-parameterize
      ((storage (identifier-syntax arg-storage))
       (free-gprs (identifier-syntax arg-free-gprs))
       (free-fprs (identifier-syntax arg-free-fprs))
       (mem-idx (identifier-syntax arg-mem-idx)))
    (define (lookup-prim-type op)
      ((@ (language trace assembler) prim-types-ref) op))
    (define (get-arg-types! op dst args)
      (let ((types (lookup-prim-type op)))
        (let lp ((types (if dst
                            (if (pair? types)
                                (cdr types)
                                (failure 'get-arg-types!
                                         "unknown type ~s ~s"
                                         op types))
                            types))
                 (args args)
                 (acc '()))
          (match (list types args)
            (((type . types) (arg . args))
             (cond
              ((symbol? arg)
               (cond
                ((storage-ref storage arg)
                 => (lambda (reg)
                      (lp types args (cons reg acc))))
                ((= type int)
                 (let ((reg (get-gpr! arg)))
                   (lp types args (cons reg acc))))
                ((= type double)
                 (let ((reg (get-fpr! arg)))
                   (lp types args (cons reg acc))))
                (else
                 (lp types args acc))))
              ((constant-value? arg)
               (lp types args (cons (make-con arg) acc)))
              (else
               (failure 'get-arg-types! "arg ~s ~s" arg type))))
            (_
             (reverse! acc))))))
    (define (get-dst-type! op dst)
      ;; Get assigned register. Will assign new register if not assigned yet.
      (let ((type (car (lookup-prim-type op)))
            (assigned (storage-ref storage dst)))
        (cond
         (assigned        assigned)
         ((= type int)    (get-gpr! dst))
         ((= type double) (get-fpr! dst))
         (else (failure 'get-dst-types! "dst ~s ~s" dst type)))))
    (define (ref k)
      (cond
       ((symbol? k) (storage-ref storage k))
       ((constant-value? k) (make-con k))
       (else (failure 'assign-registers "ref ~s not found" k))))
    (define (ref-map ks)
      (let lp ((ks ks))
        (if (null? ks)
            '()
            (cons (ref (car ks)) (lp (cdr ks))))))
    (define (constant-value? x)
      (or (boolean? x)
          (char? x)
          (number? x)
          (null? x)
          (undefined? x)
          (unspecified? x)))
    (define (assign-term term acc)
      (match term
        (('let (('_ '_)) term1)
         (assign-term term1 acc))
        (('let (('_ ('%snap id . args))) term1)
         (let* ((regs (ref-map args))
                (prim `(%snap ,id ,@regs)))
           (set! snapshot-id id)
           (set-snapshot-variables! (snapshots-ref snapshots id) regs)
           (assign-term term1 (cons prim acc))))
        (('let (('_ (op . args))) term1)
         (let ((prim `(,op ,@(ref-map args))))
           (assign-term term1 (cons prim acc))))
        (('let ((dst (? constant-value? val))) term1)
         (let* ((reg (cond
                      ((ref dst) => identity)
                      ((flonum? val) (get-fpr! dst))
                      (else (get-gpr! dst))))
                (prim `(%move ,reg ,(make-con val))))
           (assign-term term1 (cons prim acc))))
        (('let ((dst (? symbol? src))) term1)
         (let ((src-reg (ref src)))
           (if src-reg
               (let* ((dst-reg (cond
                                ((ref dst) => identity)
                                ((gpr? src-reg) (get-gpr! dst))
                                ((fpr? src-reg) (get-fpr! dst))
                                ((memory? src-reg) (get-mem! dst))))
                      (prim `(%move ,dst-reg ,src-reg)))
                 (assign-term term1 (cons prim acc)))
               (assign-term term1 acc))))
        (('let ((dst (op . args))) term1)
         ;; Set and get argument types before destination type.
         (let* ((arg-regs (get-arg-types! op dst args))
                (prim `(,op ,(get-dst-type! op dst) ,@arg-regs)))
           (assign-term term1 (cons prim acc))))
        (_ acc)))

    (let ((plist (reverse! (assign-term term '()))))
      (values plist snapshot-id))))


;;;; IR to list of primitive operations

(define (anf->primops term env initial-snapshot vars snapshots)
  (let* ((parent-snapshot (env-parent-snapshot env))
         (initial-free-gprs (make-initial-free-gprs))
         (initial-free-fprs (make-initial-free-fprs))
         (initial-mem-idx (make-variable 0))
         (initial-storage (make-storage env initial-free-gprs
                                        initial-free-fprs initial-mem-idx))
         (initial-local-x-types (snapshot-locals initial-snapshot)))

    ;; Assign scratch registers to tmporary variables.
    (syntax-parameterize
        ((free-gprs (identifier-syntax initial-free-gprs))
         (free-fprs (identifier-syntax initial-free-fprs))
         (mem-idx (identifier-syntax initial-mem-idx))
         (storage (identifier-syntax initial-storage)))
      (define-syntax-rule (set-initial-args! initial-args initial-locals)
        (let lp ((args initial-args)
                 (local-x-types initial-locals)
                 (acc '()))
          (match (list args local-x-types)
            (((arg . args) ((local . type) . local-x-types))
             (let ((reg (cond
                         ((storage-ref storage arg) => identity)
                         ((eq? type &flonum) (get-fpr! arg))
                         (else (get-gpr! arg)))))
               (lp args local-x-types (cons reg acc))))
            (_ (reverse! acc)))))
      (define (sort-variables-in-storage t) ; For debug.
        (define (var-index sym)
          (string->number (substring (symbol->string sym) 1)))
        (sort (hash-map->list (lambda (k v)
                                (list k (and v (physical-name v)))) t)
              (lambda (a b)
                (< (var-index (car a))
                   (var-index (car b))))))

      (match term
        ;; ANF with entry clause and loop body.
        (`(letrec ((entry (lambda ,entry-args ,entry-body))
                   (loop (lambda ,loop-args ,loop-body)))
            entry)
         (set-initial-args! entry-args initial-local-x-types)
         (let*-values (((entry-ops snapshot-idx)
                        (assign-registers entry-body snapshots storage
                                          free-gprs free-fprs mem-idx
                                          0))
                       ((loop-ops snapshot-idx)
                        (assign-registers loop-body snapshots storage
                                          free-gprs free-fprs mem-idx
                                          snapshot-idx)))
           (make-primops entry-ops loop-ops (variable-ref mem-idx) storage)))

        ;; ANF without loop.
        (`(letrec ((patch (lambda ,patch-args ,patch-body)))
            patch)

         ;; Refill variables. Using the locals assigned in snapshot, which are
         ;; determined at the time of exit from parent trace.
         (match parent-snapshot
           (($ $snapshot _ _ _ _ locals variables _ _)
            ;; The number of assigned variables might fewer than the number of
            ;; locals. Reversed and assigning from highest frame to lowest
            ;; frame.
            (let lp ((variables (reverse variables))
                     (locals (reverse locals)))
              (match (list variables locals)
                (((var . vars) ((local . type) . locals))
                 (storage-set! storage (make-var local) var)
                 (match var
                   (('gpr . n) (vector-set! free-gprs n #f))
                   (('fpr . n) (vector-set! free-fprs n #f))
                   (('mem . n)
                    (when (<= (variable-ref mem-idx) n)
                      (variable-set! mem-idx (+ n 1))))
                   (_
                    (unless (or (return-address? type)
                                (dynamic-link? type)
                                (constant? type)
                                (eq? &undefined type)
                                (eq? &false type)
                                (eq? &any type))
                      (failure 'ir->primops "var ~a at local ~a, type ~a"
                               var local (pretty-type type)))))
                 (lp vars locals))
                (_ (values)))))
           (_
            (debug 2 ";;; ir->primops: perhaps loop-less root trace~%")))

         (let-values (((patch-ops snapshot-idx)
                       (assign-registers patch-body snapshots storage
                                         free-gprs free-fprs mem-idx 0)))
           (make-primops patch-ops '() (variable-ref mem-idx) storage)))
        (_ (failure 'ir->primops "malformed term" term))))))
