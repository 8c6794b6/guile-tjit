;;;; Bytecode to IR compiler

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
;;; Compile list of bytecode operations to intermediate representation in almost
;;; A-Normal Form (ANF).
;;;
;;; One of the main reasons to convert bytecode to ANF is to do floating point
;;; arithmetic efficiently. VM bytecodes uses integer index to refer 'scm stack
;;; items, and those locals are not distinguished from floating point values
;;; from others. In ANF format, it is possible to perform floating point
;;; arithmetic directly with unboxed value in floating point register inside
;;; loop.
;;;
;;; Code:

(define-module (system vm native tjit compile-ir)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit ir-array)
  #:use-module (system vm native tjit ir-branch)
  #:use-module (system vm native tjit ir-call)
  #:use-module (system vm native tjit ir-dynamic)
  #:use-module (system vm native tjit ir-immediate)
  #:use-module (system vm native tjit ir-lexical)
  #:use-module (system vm native tjit ir-misc)
  #:use-module (system vm native tjit ir-mutable)
  #:use-module (system vm native tjit ir-numeric)
  #:use-module (system vm native tjit ir-pair)
  #:use-module (system vm native tjit ir-prologue)
  #:use-module (system vm native tjit ir-specialized)
  #:use-module (system vm native tjit ir-string)
  #:use-module (system vm native tjit ir-struct)
  #:use-module (system vm native tjit env)
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit types)
  #:export (compile-ir))


;;;
;;; Bytecode to ANF, and ANF to primop compiler
;;;

(define (compile-ir env trace)
  "Compiles TRACE to primops with ENV and TRACE."
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log (env-id env))))
      (set-tjit-time-log-anf! log (get-internal-run-time))))
  (let*-values (((vars snapshots anf) (compile-anf env trace))
                ((snapshot0) (hashq-ref snapshots 0)))
    (when (tjit-dump-time? (tjit-dump-option))
      (let ((log (get-tjit-time-log (env-id env))))
        (set-tjit-time-log-ops! log (get-internal-run-time))))
    (let* ((anf (optimize-anf anf))
           (primops (anf->primops anf env snapshot0 vars snapshots)))
      (values snapshots anf primops))))

(define (optimize-anf anf)
  "Potential path to optimize ANF.

Currently does nothing, returns the given argument."
  anf)

(define (add-initial-loads env snapshots initial-locals initial-sp-offset
                           parent-snapshot initial-vars live-vars-in-parent
                           loaded-vars thunk)
  ;; When local was passed from parent and snapshot 0 contained the local with
  ;; same type, no need to load from frame. If type does not match, the value
  ;; passed from parent has different was untagged with different type, reload
  ;; from frame.
  ;;
  ;; When locals index was found in parent snapshot locals and not from snapshot
  ;; 0 of this trace, the local will be passed from parent fragment, ignoreing.
  ;;
  ;; If initial offset is positive and local index is negative, locals from
  ;; lower frame won't be passed as argument. Loading later with '%fref' or
  ;; '%fref/f'.
  ;;
  (let* ((snapshot0 (hashq-ref snapshots 0))
         (snapshot0-locals (snapshot-locals snapshot0))
         (parent-snapshot-locals (or (and=> parent-snapshot snapshot-locals)
                                     '())))
    (define (type-from-snapshot n)
      (assq-ref snapshot0-locals n))
    (define (type-from-parent n)
      (assq-ref parent-snapshot-locals n))
    (debug 3 ";;; add-initial-loads:~%")
    (debug 3 ";;;   initial-locals=~a~%"
           (let lp ((copy (vector-copy initial-locals))
                    (i (- (vector-length initial-locals) 1)))
             (if (< i 0)
                 copy
                 (let ((addr (object-address (vector-ref copy i))))
                   (vector-set! copy i (format #f "#x~x" addr))
                   (lp copy (- i 1))))))
    (debug 3 ";;;   parent-snapshot-locals=~a~%" parent-snapshot-locals)
    (debug 3 ";;;   locals0=~a~%"
           (sort (snapshot-locals snapshot0)
                 (lambda (a b) (< (car a) (car b)))))
    (debug 3 ";;;   sp-offset0=~a~%" (snapshot-sp-offset snapshot0))
    (debug 3 ";;;   live-vars-in-parent=~s~%" live-vars-in-parent)
    (debug 3 ";;;   read-indices=~s~%" (env-read-indices env))
    (debug 3 ";;;   initial-vars=~s~%" initial-vars)
    (let lp ((vars (reverse initial-vars)))
      (match vars
        (((n . var) . vars)
         (cond
          ((or (< n 0)
               (let ((i (- n (snapshot-sp-offset snapshot0))))
                 (not (<= 0 i (- (vector-length initial-locals) 1))))
               (let ((parent-type (type-from-parent n))
                     (snapshot-type (type-from-snapshot n))
                     (entry-type (assq-ref (env-entry-types env) n)))
                 (debug 3 ";;;   n:~a parent:~a snap:~a entry:~a~%" n
                        (pretty-type parent-type)
                        (pretty-type snapshot-type)
                        (pretty-type entry-type))
                 (or (eq? &any entry-type)
                     (and (env-parent-snapshot env)
                          (or (not (memq n (env-read-indices env)))
                              (and (memq var live-vars-in-parent)
                                   (or (and parent-type
                                            snapshot-type
                                            (eq? parent-type snapshot-type))
                                       (or (dynamic-link? parent-type)
                                           (return-address? parent-type))
                                       (and (<= 0 initial-sp-offset)
                                            (< n 0)))))))))
           (lp vars))
          (else
           (let ((guard (assq-ref (env-entry-types env) n))
                 (type (assq-ref (env-inferred-types env) n)))
             (hashq-set! loaded-vars n guard)
             ;; XXX: Any other way to select preferred register between
             ;; GPR and FPR?
             (if (or (eq? type &flonum)
                     (eq? type &f64))
                 (with-frame-ref var type n lp vars)
                 (begin
                   ;; XXX: Updating inferred type of loaded variable,
                   ;; again.
                   (when (env-parent-snapshot env)
                     (set-inferred-type! env n guard))
                   (with-frame-ref var guard n lp vars)))))))
        (()
         (let ((live-indices (sort (hash-fold (lambda (k v acc)
                                                (if (memq k acc)
                                                    acc
                                                    (cons k acc)))
                                              (env-live-indices env)
                                              loaded-vars)
                                   <)))
           (set-env-live-indices! env live-indices))
         (thunk))))))

(define (compile-anf env trace)
  (let* ((parent-snapshot (env-parent-snapshot env))
         (local-indices (env-local-indices env))
         (vars (if (< (tjit-max-locals) (length local-indices))
                   (break 1 "too many locals")
                   (make-vars local-indices)))
         (initial-trace (car trace))
         (initial-ip (vector-ref initial-trace 1))
         (initial-locals (vector-ref initial-trace 4))
         (initial-nlocals (vector-length initial-locals))
         (initial-inline-depth (env-inline-depth env))
         (initial-sp-offset (env-sp-offset env))
         (initial-fp-offset (env-fp-offset env))
         (snapshots (make-hash-table))
         (snapshot-id 0)
         (loaded-vars (make-hash-table)))
    (define (initial-snapshot! write-indices vars)
      (let-values (((ret snapshot)
                    (take-snapshot initial-ip 0 initial-locals
                                   vars write-indices snapshot-id
                                   initial-sp-offset initial-fp-offset
                                   (min initial-sp-offset 0)
                                   initial-inline-depth env)))
        (hashq-set! snapshots snapshot-id snapshot)
        (set! snapshot-id (+ snapshot-id 1))
        ret))
    (define (get-live-vars storage snapshot)
      (let* ((parent-snapshot-vars (snapshot-variables snapshot))
             (parent-live-vars
              (map make-var (snapshot-live-indices snapshot)))
             (f (lambda (k v acc)
                  (if (and (memq v parent-snapshot-vars)
                           (not (memq k acc)))
                      (cons k acc)
                      acc))))
        (hash-fold f parent-live-vars storage)))
    (define (make-anf)
      (let ((emit (lambda ()
                    (let ((ir (make-ir snapshots snapshot-id vars
                                       (min initial-sp-offset 0)
                                       0 #f #f))
                          (sp (vector-ref (env-sp-offsets env) 0))
                          (fp (vector-ref (env-fp-offsets env) 0)))
                      (set-env-sp-offset! env sp)
                      (set-env-fp-offset! env fp)
                      (trace->anf env ir trace)))))
        (cond
         ((env-parent-snapshot env)     ; Side trace.
          => (lambda (parent-snapshot)
               (let* ((parent-fragment (env-parent-fragment env))
                      (parent-locals (snapshot-locals parent-snapshot))
                      (parent-inferred-types
                       (snapshot-inferred-types parent-snapshot))
                      (live-vars-in-parent
                       (get-live-vars (fragment-storage parent-fragment)
                                      parent-snapshot))
                      (indices-from-parent
                       (map car (snapshot-locals parent-snapshot)))
                      (vars-from-parent
                       (filter (match-lambda
                                 ((_ . var) (memq var live-vars-in-parent)))
                               vars))
                      (args-from-parent (reverse (map cdr vars-from-parent)))
                      (min-sp initial-sp-offset)
                      (max-sp (- (+ initial-sp-offset initial-nlocals) 1))
                      (make-snap0
                       (lambda ()
                         (initial-snapshot! indices-from-parent
                                            vars-from-parent)))
                      (patch-body
                       (lambda ()
                         (add-initial-loads env snapshots initial-locals
                                            initial-sp-offset parent-snapshot
                                            vars live-vars-in-parent
                                            loaded-vars emit))))

                 ;; Side trace updates inferred type with locals from parent
                 ;; snapshot. Update is done before creating initial snapshot,
                 ;; since snapshot creation will refer inferred types in env.
                 (let lp ((srcs (env-entry-types env))
                          (dsts (env-inferred-types env)))
                   (match srcs
                     (((n . t) . srcs)
                      (lp srcs (assq-set! dsts n t)))
                     (()
                      (let lp ((srcs parent-inferred-types)
                               (dsts dsts))
                        (match srcs
                          (((n . t) . srcs)
                           (let* ((t (if (and (<= min-sp n max-sp)
                                              (not (dynamic-link? t))
                                              (not (return-address? t))
                                              (not (eq? &undefined t))
                                              (not (eq? &false t)))
                                         t
                                         (assq-ref parent-locals n)))
                                  (dsts (if t
                                            (assq-set! dsts n t)
                                            dsts)))
                             (lp srcs dsts)))
                          (()
                           (set-env-inferred-types! env dsts)
                           `(letrec ((patch (lambda ,args-from-parent
                                              (let ((_ ,(make-snap0)))
                                                ,(patch-body)))))
                              patch))))))))))

         (else ; Root trace.
          ;; Root trace takes snapshot 0, used to emit bailout code without
          ;; stack element update. Snapshot 0 is saved to snapshots as usual,
          ;; and snapshot ID get incremented.
          (let* ((arg-indices (filter (lambda (n)
                                        (<= 0 n (- initial-nlocals 1)))
                                      (reverse local-indices)))
                 (args (map make-var (reverse local-indices)))
                 (indices (env-write-indices env))
                 (thunk (lambda ()
                          (let ((snap1 (initial-snapshot! indices vars)))
                            `(let ((_ ,snap1))
                               (loop ,@args)))))
                 (snap0 (make-snapshot 0 0 0 initial-nlocals arg-indices
                                       env initial-ip 0))
                 (live-indices (if (env-uprec? env)
                                   (filter (lambda (n)
                                             (<= 0 n (- initial-nlocals 1)))
                                           (env-read-indices env))
                                   (env-read-indices env)))
                 (entry-body
                  (lambda ()
                    (add-initial-loads env snapshots initial-locals
                                       initial-sp-offset #f vars '()
                                       loaded-vars thunk)))
                 (loop-body
                  (lambda ()
                    (let* ((locals (sort (hash-map->list cons loaded-vars)
                                         (lambda (a b)
                                           (< (car a) (car b)))))
                           (vars (map (lambda (l)
                                        (make-var (car l)))
                                      (reverse locals))))
                      (set-env-loop-vars! env vars)
                      (set-env-loop-locals! env locals)
                      (emit)))))

            ;; Update inferred types with entry types. All guards for entry
            ;; types have passed at this point. Then update snapshots, snapshot
            ;; id, live indices.
            (let lp ((srcs (env-entry-types env))
                     (dsts (env-inferred-types env)))
              (match srcs
                (((n . t) . srcs)
                 (lp srcs (assq-set! dsts n t)))
                (()
                 (set-env-inferred-types! env dsts)
                 (set-env-live-indices! env live-indices)
                 (hashq-set! snapshots 0 snap0)
                 (set! snapshot-id (+ snapshot-id 1))
                 `(letrec ((entry (lambda ()
                                    (let ((_ (%snap 0)))
                                      ,(entry-body))))
                           (loop (lambda ,args
                                   ,(loop-body))))
                    entry)))))))))

    (values vars snapshots (make-anf))))

(define (trace->anf env ir traces)
  (let* ((initial-nlocals (snapshot-nlocals (hashq-ref (ir-snapshots ir) 0)))
         (last-sp-offset (env-last-sp-offset env))
         (last-fp-offset (let* ((fp-offsets (env-fp-offsets env))
                                (i (- (vector-length fp-offsets) 1)))
                           (vector-ref fp-offsets i)))
         (initial-inline-depth (env-inline-depth env))
         (root-trace? (not (env-parent-snapshot env))))
    (define (entry-snapshot! ip locals sp-offset min-sp nlocals)
      (let-values (((ret snapshot)
                    (take-snapshot ip 0 locals (ir-vars ir)
                                   (env-write-indices env)
                                   (ir-snapshot-id ir) sp-offset last-fp-offset
                                   min-sp (env-inline-depth env) env #f
                                   nlocals)))
        (let ((old-id (ir-snapshot-id ir)))
          (hashq-set! (ir-snapshots ir) old-id snapshot)
          (set-ir-snapshot-id! ir (+ old-id 1))
          ret)))
    (define (nlocals-from-op op)
      ;; Get the new `nlocals' when side trace ended with one of the call
      ;; or operations containing `ALLOC_FRAME'. Otherwise false.
      (match op
        (('alloc-frame nlocals) nlocals)
        (('reset-frame nlocals) nlocals)
        (('assert-nargs-ee/locals expected nlocals) (+ nlocals expected))
        (('call _ nlocals) nlocals)
        (('call-label _ nlocals _) nlocals)
        (('tail-call nlocals) nlocals)
        (('tail-call-label nlocals _) nlocals)
        (_ #f)))
    (define (gen-last-op op ip ra locals)
      ;; Last operation is wrapped in a thunk, to assign snapshot ID in last
      ;; expression after taking snapshots from various works defined in `ir-*'
      ;; modules. Trace with loop will emit `loop'.  Side traces and loop-less
      ;; root traces will capture variables with `take-snapshot!' at the end, to
      ;; pass the register and local information to linked trace.
      ;;
      (cond
       (root-trace?
        (cond
         ((or (env-downrec? env)
              (< last-sp-offset 0))
          (lambda ()
            `(let ((_ ,(entry-snapshot! *ip-key-downrec* locals last-sp-offset
                                        (ir-min-sp-offset ir)
                                        (nlocals-from-op op))))
               (loop ,@(reverse (map cdr (ir-vars ir)))))))
         ((env-uprec? env)
          (lambda ()
            `(let ((_ (%return ,ra)))
               (let ((_ ,(entry-snapshot! *ip-key-uprec* locals last-sp-offset
                                          (ir-min-sp-offset ir)
                                          (vector-length locals))))
                 (loop ,@(reverse (map cdr (ir-vars ir))))))))
         ((zero? last-sp-offset)
          (lambda ()
            `(loop ,@(reverse (map cdr (ir-vars ir))))))
         (else
          (nyi "root trace with up SP shift"))))
       (else                            ; Side trace
        (lambda ()
          `(let ((_ ,(entry-snapshot! *ip-key-link* locals last-sp-offset
                                      (ir-min-sp-offset ir)
                                      (nlocals-from-op op))))
             _)))))
    (define (gen-next ir ip dl locals op rest)
      (lambda ()
        (let* ((old-index (ir-bytecode-index ir))
               (new-index (+ old-index 1))
               (sp-offsets (env-sp-offsets env))
               (old-sp-offset
                (or (and (<= 0 old-index (1- (vector-length sp-offsets)))
                         (vector-ref sp-offsets old-index))
                    (failure 'trace->anf "gen-next: SP index ~s out of range ~s"
                             old-index sp-offsets)))
               (fp-offsets (env-fp-offsets env))
               (old-fp-offset (vector-ref fp-offsets old-index))
               (nlocals (vector-length locals))
               (new-sp-offset (if (< 0 new-index (vector-length sp-offsets))
                                  (vector-ref sp-offsets new-index)
                                  0))
               (new-fp-offset (if (< 0 new-index (vector-length fp-offsets))
                                  (vector-ref fp-offsets new-index)
                                  0)))
          (infer-type env op ip dl locals)
          (set-ir-bytecode-index! ir new-index)
          (set-env-sp-offset! env new-sp-offset)
          (set-env-fp-offset! env new-fp-offset)
          (when (< new-sp-offset (ir-min-sp-offset ir))
            (set-ir-min-sp-offset! ir new-sp-offset))
          (increment-env-call-return-num! env op)
          (convert ir rest))))
    (define (convert-one ir op ip ra dl locals rest)
      (debug 2 ";;; [convert-one] op=~s~%" op)
      (match (hashq-ref *ir-procedures* (car op))
        ((? list? procs)
         (let lp ((procs procs))
           (match procs
             (((test . work) . procs)
              (if (test op locals)
                  (let ((next (gen-next ir ip dl locals op rest)))
                    (apply work env ir next ip ra dl locals (cdr op)))
                  (lp procs)))
             (_ (nyi "~a" (car op))))))
        (_ (nyi "~a" (car op)))))
    (define (convert ir trace)
      (match trace
        ((#(op ip ra dl locals) . ())
         (let ((last-op (gen-last-op op ip ra locals)))
           (set-ir-last-op! ir #t)
           (convert-one ir op ip ra dl locals last-op)))
        ((#(op ip ra dl locals) . rest)
         (convert-one ir op ip ra dl locals rest))
        (last-op
         ;; XXX: Remove min-sp-offset from ir, use min-sp-offset field of env.
         (set-env-min-sp-offset! env (ir-min-sp-offset ir))
         (last-op))))
    (convert ir traces)))
