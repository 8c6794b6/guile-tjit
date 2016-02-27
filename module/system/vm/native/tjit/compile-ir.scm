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
;;; A-normal form, (ANF).
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
  #:use-module (language scheme spec)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:use-module (system base compile)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit fragment)
  #:use-module (system vm native tjit ir)
  #:use-module (system vm native tjit ir-array)
  #:use-module (system vm native tjit ir-branch)
  #:use-module (system vm native tjit ir-call)
  #:use-module (system vm native tjit ir-immediate)
  #:use-module (system vm native tjit ir-lexical)
  #:use-module (system vm native tjit ir-misc)
  #:use-module (system vm native tjit ir-mutable)
  #:use-module (system vm native tjit ir-numeric)
  #:use-module (system vm native tjit ir-pair)
  #:use-module (system vm native tjit ir-prologue)
  #:use-module (system vm native tjit ir-specialized)
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

(define (get-live-vars-in-parent storage snapshot)
  (if (and storage snapshot)
      (let* ((parent-snapshot-vars
              (or (and=> snapshot snapshot-variables) '()))
             (parent-read-vars
              (map make-var (snapshot-live-indices snapshot))))
        (hash-fold (lambda (k v acc)
                     (if (and (memq v parent-snapshot-vars)
                              (not (memq k acc)))
                         (cons k acc)
                         acc))
                   parent-read-vars storage))
      '()))

(define (add-initial-loads env snapshots
                           initial-locals initial-sp-offset
                           parent-snapshot vars live-vars-in-parent
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
                 (let ((addr (pointer-address
                              (scm->pointer (vector-ref copy i)))))
                   (vector-set! copy i (format #f "#x~x" addr))
                   (lp copy (- i 1))))))
    (debug 3 ";;;   parent-snapshot-locals=~a~%" parent-snapshot-locals)
    (debug 3 ";;;   locals0=~a~%"
           (sort (snapshot-locals snapshot0)
                 (lambda (a b) (< (car a) (car b)))))
    (debug 3 ";;;   live-vars-in-parent=~s~%" live-vars-in-parent)
    (let lp ((vars (reverse vars)))
      (match vars
        (((n . var) . vars)
         (let ((j (+ n initial-sp-offset))
               (i (- n (snapshot-sp-offset snapshot0))))
           (cond
            ((or (< j 0)
                 (not (<= 0 i (- (vector-length initial-locals) 1)))
                 (let ((parent-type (type-from-parent j))
                       (snapshot-type (type-from-snapshot j)))
                   (debug 3 ";;;   n:~a sp-offset:~a parent:~a snap:~a~%"
                          n initial-sp-offset (pretty-type parent-type)
                          (pretty-type snapshot-type))
                   (and (not (env-loop? env))
                        (or (and parent-type
                                 snapshot-type
                                 (eq? parent-type snapshot-type))
                            (and (not snapshot-type)
                                 parent-type)
                            (or (dynamic-link? parent-type)
                                (return-address? parent-type))
                            (and (<= 0 initial-sp-offset)
                                 (< n 0))
                            (not (memq n (env-read-indices env)))
                            (memq var live-vars-in-parent)))))
             (lp vars))
            (else
             (let ((guard (assq-ref (env-entry-types env) n))
                   (type (assq-ref (env-inferred-types env) n)))
               (hashq-set! loaded-vars n guard)
               ;; XXX: Any other way to select preferred register between GPR
               ;; and FPR?
               (if (or (eq? type &flonum)
                       (eq? type &f64))
                   (with-frame-ref vars var type n lp)
                   (with-frame-ref vars var guard n lp)))))))
        (()
         (let ((live-indices
                (sort (hash-fold (lambda (k v acc)
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
         (root-trace? (not parent-snapshot))
         (initial-sp-offset (get-initial-sp-offset parent-snapshot))
         (initial-fp-offset (get-initial-fp-offset parent-snapshot))
         (initial-trace (car trace))
         (initial-ip (cadr initial-trace))
         (initial-locals (list-ref initial-trace 4))
         (initial-nlocals (vector-length initial-locals))
         (initial-inline-depth (env-inline-depth env))
         (local-indices (env-local-indices env))
         (vars (make-vars local-indices))
         (snapshots (make-hash-table))
         (snapshot-id (if root-trace? 1 0))
         (parent-snapshot-locals (or (and=> parent-snapshot snapshot-locals)
                                     '()))
         (live-vars-in-parent
          (get-live-vars-in-parent (and=> (env-parent-fragment env)
                                          fragment-storage)
                                   parent-snapshot))
         (vars-from-parent
          (filter (match-lambda
                    ((_ . var) (memq var live-vars-in-parent)))
                  vars))
         (args-from-parent (reverse (map cdr vars-from-parent))))
    (define (initial-snapshot!)
      (let-values (((ret snapshot)
                    (take-snapshot initial-ip 0 initial-locals
                                   vars-from-parent
                                   (if (and parent-snapshot
                                            (not (env-loop? env)))
                                       (map car parent-snapshot-locals)
                                       (env-write-indices env))
                                   snapshot-id
                                   initial-sp-offset initial-fp-offset
                                   (min initial-sp-offset 0)
                                   (get-max-sp-offset initial-sp-offset
                                                      initial-fp-offset
                                                      initial-nlocals)
                                   initial-inline-depth env)))
        (hashq-set! snapshots snapshot-id snapshot)
        (set! snapshot-id (+ snapshot-id 1))
        ret))

    (define (make-anf)
      (let ((emit
             (lambda ()
               (let* ((initial-nlocals
                       (snapshot-nlocals (hashq-ref snapshots 0)))
                      (env-sp-offset
                       (vector-ref (env-sp-offsets env) 0))
                      (env-fp-offset
                       (vector-ref (env-fp-offsets env) 0))
                      (min-sp-offset (min initial-sp-offset 0))
                      (max-sp-offset (get-max-sp-offset initial-sp-offset
                                                        initial-fp-offset
                                                        initial-nlocals))
                      (ir (make-ir snapshots snapshot-id vars
                                   min-sp-offset max-sp-offset
                                   0 #f)))
                 (set-env-sp-offset! env env-sp-offset)
                 (set-env-fp-offset! env env-fp-offset)
                 (trace->anf env ir trace)))))

        ;; Update inferred type with entry types. All guards for entry types
        ;; have passed at this point. Then if this trace was side trace, set
        ;; inferred types from parent snapshot locals.
        (let ((srcs (env-entry-types env))
              (dsts (env-inferred-types env)))
          (let lp ((srcs srcs) (dsts dsts))
            (match srcs
              (((n . ty) . srcs)
               (lp srcs (assq-set! dsts n ty)))
              (()
               (if (and (pair? parent-snapshot-locals)
                        (not (env-loop? env)))
                   (let lp ((srcs parent-snapshot-locals) (dsts dsts))
                     (match srcs
                       (((n . ty) . srcs)
                        (lp srcs (assq-set! dsts n ty)))
                       (()
                        (set-env-inferred-types! env dsts))))
                   (set-env-inferred-types! env dsts))))))

        (cond
         (root-trace?
          (let* ((arg-indices (filter (lambda (n)
                                        (<= 0 n (- initial-nlocals 1)))
                                      (reverse local-indices)))
                 (args (map make-var (reverse local-indices)))
                 (thunk (lambda ()
                          `(loop ,@args)))
                 (snap0 (make-snapshot 0 0 0 initial-nlocals arg-indices
                                       env initial-ip 0))
                 (_ (hashq-set! snapshots 0 snap0))
                 (loaded-vars (make-hash-table)))
            (set-env-live-indices! env (env-read-indices env))
            `(letrec ((entry (lambda ()
                               (let ((_ (%snap 0)))
                                 ,(add-initial-loads env snapshots
                                                     initial-locals
                                                     initial-sp-offset
                                                     #f vars '()
                                                     loaded-vars
                                                     thunk))))
                      (loop (lambda ,args
                              ,(let* ((locals
                                       (sort (hash-map->list cons loaded-vars)
                                             (lambda (a b)
                                               (< (car a) (car b)))))
                                      (vars (map (lambda (l)
                                                   (make-var (car l)))
                                                 (reverse locals))))
                                 (set-env-loop-vars! env vars)
                                 (set-env-loop-locals! env locals)
                                 (emit)))))
               entry)))
         ((env-loop? env)
          (let* ((args-from-vars (reverse! (map cdr vars)))
                 (thunk (lambda ()
                          `(loop ,@args-from-vars)))
                 (loaded-vars (make-hash-table))
                 (snap0 (initial-snapshot!)))
            (set-env-live-indices! env (env-read-indices env))
            `(letrec ((entry (lambda ,args-from-parent
                               (let ((_ ,snap0))
                                 ,(add-initial-loads env snapshots
                                                     initial-locals
                                                     initial-sp-offset
                                                     parent-snapshot
                                                     vars live-vars-in-parent
                                                     loaded-vars
                                                     thunk))))
                      (loop (lambda ,args-from-vars
                              ,(emit))))
               entry)))
         (else
          (let ((snap0 (initial-snapshot!))
                (loaded-vars (make-hash-table)))
            `(letrec ((patch (lambda ,args-from-parent
                               (let ((_ ,snap0))
                                 ,(add-initial-loads env snapshots
                                                     initial-locals
                                                     initial-sp-offset
                                                     parent-snapshot
                                                     vars live-vars-in-parent
                                                     loaded-vars
                                                     emit)))))
               patch))))))

    (values vars snapshots (make-anf))))

(define (trace->anf env ir traces)
  (let* ((initial-nlocals (snapshot-nlocals (hashq-ref (ir-snapshots ir) 0)))
         (last-sp-offset (env-last-sp-offset env))
         (last-fp-offset (let* ((fp-offsets (env-fp-offsets env))
                                (i (- (vector-length fp-offsets) 1)))
                           (vector-ref fp-offsets i)))
         (initial-inline-depth (env-inline-depth env)))
    (define (entry-snapshot! ip locals sp-offset min-sp)
      (let-values (((ret snapshot)
                    (take-snapshot ip 0 locals (ir-vars ir)
                                   (env-write-indices env)
                                   (ir-snapshot-id ir) sp-offset last-fp-offset
                                   min-sp (ir-max-sp-offset ir)
                                   (env-inline-depth env)
                                   env)))
        (let ((old-id (ir-snapshot-id ir)))
          (hashq-set! (ir-snapshots ir) old-id snapshot)
          (set-ir-snapshot-id! ir (+ old-id 1))
          ret)))
    (define (gen-last-op op ip locals)
      ;; Last operation is wrapped in a thunk, to assign snapshot ID in last
      ;; expression after taking snapshots from various works defined in `ir-*'
      ;; modules. Trace with loop will emit `loop'.  Side traces and loop-less
      ;; root traces will capture variables with `take-snapshot!' at the end, to
      ;; pass the register and local information to linked trace.
      ;;
      (cond
       ((env-downrec? env)
        (match op
          (('call proc nlocals)
           (lambda ()
             (let* ((next-sp (- last-fp-offset proc nlocals))
                    (sp-shift
                     (cond
                      ((and=> (env-parent-fragment env)
                              (lambda (fragment)
                                (fragment-loop-locals fragment)))
                       => (lambda (loop-locals)
                            (length loop-locals)))
                      (else
                       initial-nlocals)))
                    (next-sp-offset (+ next-sp sp-shift))
                    (dr-locals
                     (let lp ((n 0) (end (vector-length locals)) (acc '()))
                       (if (= n nlocals)
                           (list->vector acc)
                           (let* ((i (- end proc n 1))
                                  (e (vector-ref locals i)))
                             (lp (+ n 1) end (cons e acc)))))))
               `(let ((_ ,(entry-snapshot! *ip-key-downrec*
                                           (dr-locals proc nlocals)
                                           next-sp-offset next-sp-offset)))
                  (loop ,@(reverse (map cdr (ir-vars ir))))))))
          (('call-label . _)
           ;; XXX: TODO.
           (nyi "down-recursion with last op `call-label'"))
          (_
           (nyi "Unknown op ~a" op))))
       ((env-uprec? env)
        (match op
          (('return-values n)
           (lambda ()
             (let* ((next-sp-offset last-sp-offset))
               `(let ((_ ,(entry-snapshot! *ip-key-uprec* locals next-sp-offset
                                           (ir-min-sp-offset ir))))
                  (loop ,@(reverse (map cdr (ir-vars ir))))))))
          (_
           (nyi "uprec with last op ~a" op))))
       ((env-loop? env)
        (lambda ()
          `(loop ,@(reverse (map cdr (ir-vars ir))))))
       (else
        (lambda ()
          `(let ((_ ,(entry-snapshot! *ip-key-link* locals last-sp-offset
                                      (ir-min-sp-offset ir))))
             _)))))
    (define (gen-next ir ip dl locals op rest)
      (lambda ()
        (let* ((old-index (ir-bytecode-index ir))
               (new-index (+ old-index 1))
               (sp-offsets (env-sp-offsets env))
               (old-sp-offset (vector-ref sp-offsets old-index))
               (fp-offsets (env-fp-offsets env))
               (old-fp-offset (vector-ref fp-offsets old-index))
               (nlocals (vector-length locals))
               (max-offset (get-max-sp-offset old-sp-offset old-fp-offset
                                              nlocals))
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
          (when (< old-sp-offset (ir-min-sp-offset ir))
            (set-ir-min-sp-offset! ir old-sp-offset))
          (when (< (ir-max-sp-offset ir) max-offset)
            (set-ir-max-sp-offset! ir max-offset))
          (increment-env-call-return-num! env op)
          (convert ir rest))))
    (define (convert-one ir op ip ra dl locals rest)
      (debug 2 ";;; [convert-one] op=~s~%" op)
      (match (hashq-ref *ir-procedures* (car op))
        ((? list? procs)
         (let lp ((procs procs))
           (match procs
             (((test . work) . procs)
              (if (apply test (list op locals))
                  (let ((next (gen-next ir ip dl locals op rest)))
                    (apply work env ir next ip ra dl locals (cdr op)))
                  (lp procs)))
             (_ (nyi "~a" (car op))))))
        (_ (nyi "~a" (car op)))))
    (define (convert ir trace)
      (match trace
        (((op ip ra dl locals) . ())
         (let ((last-op (gen-last-op op ip locals)))
           (convert-one ir op ip ra dl locals last-op)))
        (((op ip ra dl locals) . rest)
         (convert-one ir op ip ra dl locals rest))
        (last-op
         (last-op))))
    (convert ir traces)))
