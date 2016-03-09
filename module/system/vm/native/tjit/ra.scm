;;;; Assign resiters to IR

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
;;; sophisticated such as linear-scan, binpacking, graph coloring, etc.
;;;
;;; Code:

(define-module (system vm native tjit ra)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit registers)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:use-module (system vm native tjit variables)
  #:export ($primops
            primops?
            primops-entry
            primops-loop
            primops-nspills
            anf->primops))

;;;
;;; Record type
;;;

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


;;;
;;; Auxiliary
;;;

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
          ((= i num)
           #f)
          ((vector-ref free-regs i)
           (let ((ret (constructor i)))
             (vector-set! free-regs i #f)
             ret))
          (else
           (lp (+ i 1)))))))))

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
    (hashq-set! storage var ret)
    ret))

(define-syntax-rule (get-mem! var)
  (set-storage! (gen-mem) var))

(define-syntax-rule (get-gpr! var)
  (set-storage! (or (acquire-gpr! free-gprs)
                (gen-mem))
            var))

(define-syntax-rule (get-fpr! var)
  (set-storage! (or (acquire-fpr! free-fprs)
                (gen-mem))
            var))

(define (assign-registers term snapshots arg-storage arg-free-gprs arg-free-fprs
                          arg-mem-idx snapshot-id)
  "Compile ANF term to list of primitive operations."
  (syntax-parameterize
      ((storage (identifier-syntax arg-storage))
       (free-gprs (identifier-syntax arg-free-gprs))
       (free-fprs (identifier-syntax arg-free-fprs))
       (mem-idx (identifier-syntax arg-mem-idx)))
    (define (lookup-prim-type op)
      (hashq-ref (@ (system vm native tjit assembler) *native-prim-types*)
                 op))
    (define (get-arg-types! op dst args)
      (let ((types (lookup-prim-type op)))
        (let lp ((types (if dst
                            (if (pair? types)
                                (cdr types)
                                (tjitc-error 'get-arg-types!
                                             "unknown type ~s ~s"
                                             op types))
                            types))
                 (args args)
                 (acc '()))
          (match (list types args)
            (((type . types) (arg . args))
             (cond
              ((constant? arg)
               (lp types args (cons (make-constant arg) acc)))
              ((symbol? arg)
               (cond
                ((hashq-ref storage arg)
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
              (else
               (tjitc-error 'get-arg-types! "arg ~s ~s" arg type))))
            (_
             (reverse! acc))))))
    (define (get-dst-type! op dst)
      ;; Get assigned register. Will assign new register if not assigned yet.
      (let ((type (car (lookup-prim-type op)))
            (assigned (hashq-ref storage dst)))
        (cond
         (assigned
          ;; Doing additional check for `%fref' and `%fref/f' operations, to
          ;; support suited register assignments in looping side trace started
          ;; from guard failure in parent trace's entry clause,
          (cond
           ((and (eq? op '%fref/f)
                 (not (fpr? assigned))
                 (not (memory? assigned)))
            (get-fpr! dst))
           ((and (eq? op '%fref)
                 (not (gpr? assigned))
                 (not (memory? assigned)))
            (get-gpr! dst))
           (else
            assigned)))
         ((= type int)    (get-gpr! dst))
         ((= type double) (get-fpr! dst))
         (else (tjitc-error 'get-dst-types! "dst ~s ~s" dst type)))))
    (define (ref k)
      (cond
       ((constant? k) (make-constant k))
       ((symbol? k) (hashq-ref storage k))
       (else (tjitc-error 'assign-registers "ref ~s not found" k))))
    (define (constant? x)
      (cond
       ((boolean? x) #t)
       ((char? x) #t)
       ((number? x) #t)
       ((null? x) #t)
       ((undefined? x) #t)
       ((unspecified? x) #t)
       (else #f)))
    (define (assign-term term acc)
      (match term
        (('let (('_ ('%snap id . args))) term1)
         (let* ((regs (map ref args))
                (prim `(%snap ,id ,@regs)))
           (set! snapshot-id id)
           (set-snapshot-variables! (hashq-ref snapshots id) regs)
           (assign-term term1 (cons prim acc))))
        (('let (('_ (op . args))) term1)
         (let ((prim `(,op ,@(map ref args))))
           (assign-term term1 (cons prim acc))))
        (('let ((dst (? constant? val))) term1)
         (let* ((reg (cond
                      ((ref dst) => identity)
                      ((flonum? val) (get-fpr! dst))
                      (else (get-gpr! dst))))
                (prim `(%move ,reg ,(make-constant val))))
           (assign-term term1 (cons prim acc))))
        (('let ((dst (? symbol? src))) term1)
         (let* ((src-reg (ref src))
                (dst-reg (cond
                          ((ref dst) => identity)
                          ((gpr? src-reg) (get-gpr! dst))
                          ((fpr? src-reg) (get-fpr! dst))
                          ((memory? src-reg) (get-mem! dst))))
                (prim `(%move ,dst-reg ,src-reg)))
           (assign-term term1 (cons prim acc))))
        (('let ((dst (op . args))) term1)
         ;; Set and get argument types before destination type.
         (let* ((arg-regs (get-arg-types! op dst args))
                (prim `(,op ,(get-dst-type! op dst) ,@arg-regs)))
           (assign-term term1 (cons prim acc))))
        (('loop . _)
         acc)
        ('_
         acc)
        (()
         acc)))

    (let ((plist (reverse! (assign-term term '()))))
      (values plist snapshot-id))))


;;;
;;; IR to list of primitive operations
;;;

(define (anf->primops term env initial-snapshot vars snapshots)
  (let ((parent-snapshot (env-parent-snapshot env))
        (initial-free-gprs (make-initial-free-gprs))
        (initial-free-fprs (make-initial-free-fprs))
        (initial-mem-idx (make-variable 0))
        (initial-storage (make-hash-table))
        (initial-local-x-types (snapshot-locals initial-snapshot)))
    (define (merge-storage storage)
      (hash-for-each
       (lambda (k v)
         (hashq-set! initial-storage k v)
         (cond
          ((gpr? v)
           (let ((i (ref-value v)))
             (when (<= 0 i)
               (vector-set! initial-free-gprs i #f))))
          ((fpr? v)
           (let ((i (ref-value v)))
             (when (<= 0 i)
               (vector-set! initial-free-fprs i #f))))
          ((memory? v)
           (let ((i (ref-value v)))
             (when (<= (variable-ref initial-mem-idx) i)
               (variable-set! initial-mem-idx (+ i 1)))))
          (else
           (tjitc-error 'anf->primops "unknown var ~s" v))))
       storage))

    ;; Sharing registers and memory offset for side trace to share variables
    ;; with parent trace. Also sharing registers and variables for loop-less
    ;; root tracec.
    (and=> (and=> (env-parent-fragment env) fragment-storage)
           merge-storage)
    (when (and (not (env-parent-fragment env))
               (not (env-loop? env)))
      (and=> (and=> (env-linked-fragment env) fragment-storage)
             merge-storage))

    ;; Assign scratch registers to tmporary variables.
    (hashq-set! initial-storage (make-tmpvar 0) (make-gpr -1))
    (hashq-set! initial-storage (make-tmpvar 1) (make-gpr -2))
    (hashq-set! initial-storage (make-tmpvar 2) (make-gpr -3))
    (hashq-set! initial-storage (make-tmpvar/f 0) (make-fpr -1))
    (hashq-set! initial-storage (make-tmpvar/f 1) (make-fpr -2))
    (hashq-set! initial-storage (make-tmpvar/f 2) (make-fpr -3))

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
             (cond
              ((hashq-ref storage arg)
               => (lambda (reg)
                    (lp args local-x-types (cons reg acc))))
              (else
               (let ((reg (if (eq? type &flonum)
                              (get-fpr! arg)
                              (get-gpr! arg))))
                 (lp args local-x-types (cons reg acc))))))
            (_
             (reverse! acc)))))
      (define-syntax-rule (make-var n)
        (string->symbol (string-append "v" (number->string n))))
      (define (sort-variables-in-storage t)
        (define (var-index sym)
          (string->number (substring (symbol->string sym) 1)))
        (sort (hash-map->list (lambda (k v)
                                (list k (physical-name v))) t)
              (lambda (a b)
                (< (var-index (car a))
                   (var-index (car b))))))

      (debug 2 ";;; storage (before)~%~{;;;   ~a~%~}"
             (sort-variables-in-storage storage))

      (match term
        ;; ANF with entry clause and loop body.
        (`(letrec ((entry (lambda ,entry-args
                            ,entry-body))
                   (loop (lambda ,loop-args
                           ,loop-body)))
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
           (debug 2 ";;; storage (after)~%~{;;;   ~a~%~}"
                  (sort-variables-in-storage storage))
           (make-primops entry-ops loop-ops (variable-ref mem-idx) storage)))

        ;; ANF without loop.
        (`(letrec ((patch (lambda ,patch-args
                            ,patch-body)))
            patch)

         ;; Refill variables. Using the locals assigned to snapshot, which are
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
                 (hashq-set! storage (make-var local) var)
                 (match var
                   (('gpr . n)
                    (vector-set! free-gprs n #f))
                   (('fpr . n)
                    (vector-set! free-fprs n #f))
                   (('mem . n)
                    (when (<= (variable-ref mem-idx) n)
                      (variable-set! mem-idx (+ n 1))))
                   (_
                    (tjitc-error 'ir->primops "var ~a at local ~a, type ~a"
                                 var local type)))
                 (lp vars locals))
                (_
                 (values)))))
           (_
            (debug 2 ";;; ir->primops: perhaps loop-less root trace~%")))

         (let-values (((patch-ops snapshot-idx)
                       (assign-registers patch-body snapshots storage
                                         free-gprs free-fprs mem-idx
                                         0)))
           (debug 2 ";;; storage (after)~%~{;;;   ~a~%~}"
                  (sort-variables-in-storage storage))
           (make-primops patch-ops '() (variable-ref mem-idx) storage)))
        (_
         (tjitc-error 'ir->primops "malformed term" term))))))
