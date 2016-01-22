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
;;; Compile list of bytecode operations to intermediate representation (IR) in
;;; (almost) A-normal form (ANF).
;;;
;;; One of the main reasons to convert bytecode to ANF is to do floating point
;;; arithmetic efficiently. VM bytecodes uses integer index to refer 'scm stack
;;; items, and those locals are not distinguished from floating point values
;;; from other. In ANF format, it is possible to perform floating point
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
  #:use-module (system vm native tjit parameters)
  #:use-module (system vm native tjit ra)
  #:use-module (system vm native tjit scan)
  #:use-module (system vm native tjit snapshot)
  #:use-module (system vm native tjit state)
  #:export (compile-ir))


;;;
;;; Traced bytecode to ANF IR, and ANF to primop IR compiler
;;;

(define (compile-ir tj trace)
  "Compiles TRACE to primops with TJ and TRACE."
  (when (tjit-dump-time? (tjit-dump-option))
    (let ((log (get-tjit-time-log (tj-id tj))))
      (set-tjit-time-log-scm! log (get-internal-run-time))))
  (let*-values (((vars snapshots anf) (compile-anf tj trace))
                ((snapshot0) (hashq-ref snapshots 0)))
    (when (tjit-dump-time? (tjit-dump-option))
      (let ((log (get-tjit-time-log (tj-id tj))))
        (set-tjit-time-log-ops! log (get-internal-run-time))))
    (let ((primops (anf->primops anf tj snapshot0 vars snapshots)))
      (values snapshots anf primops))))

(define (compile-anf tj trace)
  (let* ((parent-snapshot (tj-parent-snapshot tj))
         (root-trace? (not parent-snapshot))
         (initial-sp-offset (get-initial-sp-offset parent-snapshot))
         (initial-fp-offset (get-initial-fp-offset parent-snapshot))
         (local-indices (outline-local-indices (tj-outline tj)))
         (vars (make-vars local-indices))
         (lowest-offset (min initial-sp-offset 0))
         (snapshots (make-hash-table))
         (snapshot-id (if root-trace? 1 0)))
    (define (make-vars-from-parent vars live-vars)
      (let lp ((vars vars) (acc '()))
        (match vars
          (((n . var) . vars)
           (if (memq var live-vars)
               (lp vars (cons (cons n var) acc))
               (lp vars acc)))
          (()
           (reverse! acc)))))
    (let* ((args (map make-var (reverse local-indices)))
           (initial-trace (car trace))
           (initial-ip (cadr initial-trace))
           (initial-locals (list-ref initial-trace 4))
           (initial-nlocals (vector-length initial-locals))
           (parent-snapshot-locals (match parent-snapshot
                                     (($ $snapshot _ _ _ _ locals) locals)
                                     (_ #f)))
           (parent-snapshot-vars
            (or (and=> parent-snapshot snapshot-variables) '()))
           (vars-in-parent-snapshot
            (or (and=> (tj-parent-fragment tj)
                       (lambda (fragment)
                         (hash-fold (lambda (k v acc)
                                      (if (memq v parent-snapshot-vars)
                                          (cons k acc)
                                          acc))
                                    '()
                                    (fragment-env fragment))))
                '()))
           (parent-snapshot-read-indices
            (or (and=> parent-snapshot snapshot-read-indices) '()))
           (live-vars-in-parent
            (append! vars-in-parent-snapshot
                     (map make-var parent-snapshot-read-indices)))
           (vars-from-parent (make-vars-from-parent vars live-vars-in-parent))
           (args-from-parent (reverse (map cdr vars-from-parent)))
           (local-indices-from-parent (map car vars-from-parent)))
      (define (take-initial-snapshot!)
        (let-values (((ret snapshot)
                      (take-snapshot initial-ip 0 initial-locals
                                     vars-from-parent
                                     (if parent-snapshot
                                         (map car (snapshot-locals parent-snapshot))
                                         (outline-write-indices (tj-outline tj)))
                                     snapshot-id
                                     initial-sp-offset initial-fp-offset
                                     lowest-offset
                                     (get-max-sp-offset initial-sp-offset
                                                        initial-fp-offset
                                                        initial-nlocals)
                                     parent-snapshot (tj-outline tj))))
          (hashq-set! snapshots snapshot-id snapshot)
          (set! snapshot-id (+ snapshot-id 1))
          ret))
      (define* (add-initial-loads exp-body #:optional (loaded-vars #f))
        (debug 3 ";;; add-initial-loads:~%")
        (debug 3 ";;;   initial-locals=~a~%"
               (let lp ((copy (vector-copy initial-locals))
                        (i (- (vector-length initial-locals) 1)))
                 (if (< i 0)
                     copy
                     (let ((addr (pointer-address (vector-ref copy i))))
                       (vector-set! copy i (format #f "#x~x" addr))
                       (lp copy (- i 1))))))
        (debug 3 ";;;   parent-snapshot-locals=~a~%" parent-snapshot-locals)
        (debug 3 ";;;   initial-types=~a~%" (tj-initial-types tj))
        (debug 3 ";;;   locals0=~a~%"
               (sort (snapshot-locals (hashq-ref snapshots 0))
                     (lambda (a b) (< (car a) (car b)))))
        (let ((snapshot0 (hashq-ref snapshots 0)))
          (define (type-from-snapshot n)
            (assq-ref (snapshot-locals snapshot0) n))
          (define (type-from-runtime i)
            (let ((type (outline-type-ref (tj-outline tj)
                                          (+ i initial-sp-offset))))
              (cond
               ((eq? type 'f64) &f64)
               ((eq? type 'u64) &u64)
               ((eq? type 's64) &s64)
               ((eq? type 'scm) (type-of (stack-element initial-locals i type)))
               (else (tjitc-error 'type-from-runtime "~s ~s" i type)))))
          (define (type-from-parent n)
            (assq-ref parent-snapshot-locals n))
          (debug 3 ";;;    live-vars-in-parent=~s~%" live-vars-in-parent)
          (let lp ((vars (reverse vars)))
            (match vars
              (((n . var) . vars)
               (let ((j (+ n initial-sp-offset))
                     (i (- n (snapshot-sp-offset snapshot0))))
                 (cond
                  ;; When local was passed from parent and snapshot 0 contained
                  ;; the local with same type, no need to load from frame. If
                  ;; type does not match, the value passed from parent has
                  ;; different was untagged with different type, reload from
                  ;; frame.
                  ;;
                  ;; When locals index was found in parent snapshot locals and
                  ;; not from snapshot 0 of this trace, the local will be passed
                  ;; from parent fragment, ignoreing.
                  ;;
                  ;; If initial offset is positive and local index is negative,
                  ;; locals from lower frame won't be passed as
                  ;; argument. Loading later with '%fref' or '%fref/f'.
                  ;;
                  ((let ((parent-type (type-from-parent j))
                         (snapshot-type (type-from-snapshot j)))
                     (debug 3 ";;;   n + initial-sp-offset: ~a~%" j)
                     (debug 3 ";;;   var: ~a~%" var)
                     (debug 3 ";;;   parent: ~a~%" (pretty-type parent-type))
                     (debug 3 ";;;   snap: ~a~%" (pretty-type snapshot-type))
                     (or (< j 0)
                         (not (<= 0 i (- (vector-length initial-locals) 1)))
                         (and (not (tj-loop? tj))
                              (or (and parent-type
                                       snapshot-type
                                       (eq? parent-type snapshot-type))
                                  (and (not snapshot-type)
                                       parent-type)
                                  (or (dynamic-link? parent-type)
                                      (return-address? parent-type))
                                  (and (<= 0 initial-sp-offset)
                                       (< n 0))
                                  (not (memq n (outline-read-indices
                                                (tj-outline tj))))
                                  (memq var live-vars-in-parent)))))
                   (lp vars))
                  (else
                   (let* ((type (or (assq-ref (snapshot-locals snapshot0) n)
                                    (type-from-runtime i))))
                     (debug 3 ";;;   type: ~a~%" (pretty-type type))
                     (when loaded-vars
                       (hashq-set! loaded-vars n type))
                     (with-frame-ref lp vars var type n))))))
              (()
               exp-body)))))
      (define (make-anf)
        (let ((emit
               (lambda ()
                 (let* ((initial-nlocals
                         (snapshot-nlocals (hashq-ref snapshots 0)))
                        (outline (tj-outline tj))
                        (outline-sp-offset
                         (vector-ref (outline-sp-offsets outline) 0))
                        (outline-fp-offset
                         (vector-ref (outline-fp-offsets outline) 0))
                        (_ (set-outline-sp-offset! outline outline-sp-offset))
                        (_ (set-outline-fp-offset! outline outline-fp-offset))
                        (ir (make-ir snapshots
                                     snapshot-id
                                     (tj-parent-snapshot tj)
                                     vars
                                     (min initial-sp-offset 0)
                                     (get-max-sp-offset initial-sp-offset
                                                        initial-fp-offset
                                                        initial-nlocals)
                                     0 (tj-outline tj) #f #f (tj-loop? tj)))
                        (anf (trace->anf tj ir trace))
                        (interrupts? (ir-handle-interrupts? ir)))

                   (set-tj-handle-interrupts! tj interrupts?)
                   anf))))
          (merge-outline-types! (tj-outline tj) (tj-initial-types tj))
          (cond
           (root-trace?
            (let* ((arg-indices (filter (lambda (n)
                                          (<= 0 n (- initial-nlocals 1)))
                                        (reverse local-indices)))
                   (snap0 (make-snapshot 0 0 0
                                         initial-nlocals initial-locals
                                         #f arg-indices (tj-outline tj)
                                         initial-ip))
                   (_ (hashq-set! snapshots 0 snap0))
                   (vt (make-hash-table)))
              `(letrec ((entry (lambda ()
                                 (let ((_ (%snap 0)))
                                   ,(add-initial-loads `(loop ,@args) vt))))
                        (loop (lambda ,args
                                ,(let* ((locals (sort (hash-map->list cons vt)
                                                      (lambda (a b)
                                                        (< (car a) (car b)))))
                                        (vars (map (lambda (l)
                                                     (make-var (car l)))
                                                   (reverse locals))))
                                   (set-tj-loop-vars! tj vars)
                                   (set-tj-loop-locals! tj locals)
                                   (emit)))))
                 entry)))
           ((tj-loop? tj)
            (let ((args-from-vars (reverse! (map cdr vars)))
                  (snap (take-initial-snapshot!)))
              `(letrec ((entry (lambda ,args-from-parent
                                 (let ((_ ,snap))
                                   ,(add-initial-loads
                                     `(loop ,@args-from-vars)))))
                        (loop (lambda ,args-from-vars
                                ,(emit))))
                 entry)))
           (else
            (let ((snap (take-initial-snapshot!)))
              `(letrec ((patch (lambda ,args-from-parent
                                 (let ((_ ,snap))
                                   ,(add-initial-loads (emit))))))
                 patch))))))

      (values vars snapshots (make-anf)))))

(define (trace->anf tj ir traces)
  (let* ((initial-nlocals (snapshot-nlocals (hashq-ref (ir-snapshots ir) 0)))
         (last-sp-offset (tj-last-sp-offset tj))
         (last-fp-offset (let* ((fp-offsets (outline-fp-offsets
                                             (tj-outline tj)))
                                (i (- (vector-length fp-offsets) 1)))
                           (vector-ref fp-offsets i))))
    (define (take-entry-snapshot! ip locals sp-offset min-sp)
      (let-values (((ret snapshot)
                    (take-snapshot ip 0 locals (ir-vars ir)
                                   (outline-write-indices (ir-outline ir))
                                   (ir-snapshot-id ir) sp-offset last-fp-offset
                                   min-sp (ir-max-sp-offset ir)
                                   (tj-parent-snapshot tj) (tj-outline tj))))
        (let ((old-id (ir-snapshot-id ir)))
          (hashq-set! (ir-snapshots ir) old-id snapshot)
          (set-ir-snapshot-id! ir (+ old-id 1))
          ret)))
    (define (gen-last-op op ip locals)
      ;; Last operation is wrapped in a thunk, to assign snapshot ID in last
      ;; expression after taking snapshots from guard operations defined in
      ;; `ir-*' modules.
      ;;
      ;; Trace with loop will emit `loop', Scheme IR emitted in `make-anf'.
      ;;
      ;; Side traces and loop-less root traces are capturing variables with
      ;; `take-snapshot!' at the end, to pass the register and local information
      ;; to linked trace.
      ;;
      (cond
       ((tj-downrec? tj)
        (match op
          (('call proc nlocals)
           (lambda ()
             (let* ((next-sp (- last-fp-offset proc nlocals))
                    (sp-shift
                     (cond
                      ((and=> (tj-parent-fragment tj)
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
               `(let ((_ ,(take-entry-snapshot! *ip-key-downrec*
                                                (dr-locals proc nlocals)
                                                next-sp-offset
                                                next-sp-offset)))
                  (loop ,@(reverse (map cdr (ir-vars ir))))))))
          (('call-label . _)
           ;; XXX: TODO.
           (nyi "down-recursion with last op `call-label'"))
          (_
           (nyi "Unknown op ~a" op))))
       ((tj-uprec? tj)
        (match op
          (('return-values n)
           (lambda ()
             (let* ((next-sp-offset last-sp-offset))
               `(let ((_ ,(take-entry-snapshot! *ip-key-uprec* locals
                                                next-sp-offset
                                                (ir-min-sp-offset ir))))
                  (loop ,@(reverse (map cdr (ir-vars ir))))))))
          (_
           (nyi "uprec with last op ~a" op))))
       ((tj-loop? tj)
        (lambda ()
          `(loop ,@(reverse (map cdr (ir-vars ir))))))
       (else
        (lambda ()
          `(let ((_ ,(take-entry-snapshot! *ip-key-link* locals
                                           last-sp-offset
                                           (ir-min-sp-offset ir))))
             _)))))
    (define (convert-one ir op ip ra dl locals rest)
      (scan-locals (ir-outline ir) op #f dl locals #t #f)
      (cond
       ((hashq-ref *ir-procedures* (car op))
        => (lambda (proc)
             (let ((next
                    (lambda ()
                      (let* ((old-index (ir-bytecode-index ir))
                             (new-index (+ old-index 1))
                             (sp-offsets (outline-sp-offsets (ir-outline ir)))
                             (sp-offset (vector-ref sp-offsets old-index))
                             (fp-offsets (outline-fp-offsets (ir-outline ir)))
                             (fp-offset (vector-ref fp-offsets old-index))
                             (nlocals (vector-length locals))
                             (max-offset (get-max-sp-offset sp-offset
                                                            fp-offset
                                                            nlocals))
                             (new-sp-offset
                              (if (< 0 new-index (vector-length sp-offsets))
                                  (vector-ref sp-offsets new-index)
                                  0))
                             (new-fp-offset
                              (if (< 0 new-index (vector-length fp-offsets))
                                  (vector-ref fp-offsets new-index)
                                  0)))
                        (set-ir-bytecode-index! ir new-index)
                        (set-outline-sp-offset! (ir-outline ir) new-sp-offset)
                        (set-outline-fp-offset! (ir-outline ir) new-fp-offset)
                        (when (< sp-offset (ir-min-sp-offset ir))
                          (set-ir-min-sp-offset! ir sp-offset))
                        (when (< (ir-max-sp-offset ir) max-offset)
                          (set-ir-max-sp-offset! ir max-offset)))
                      (convert ir rest))))
               (apply proc ir next ip ra dl locals (cdr op)))))
       (else
        (nyi "~a" (car op)))))
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
