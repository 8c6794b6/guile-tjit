;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.

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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:
;;;
;;; Compiling CPS to bytecode.  The result is in the bytecode language,
;;; which happens to be an ELF image as a bytecode.
;;;
;;; Code:

(define-module (language cps compile-bytecode)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps)
  #:use-module (language cps arities)
  #:use-module (language cps closure-conversion)
  #:use-module (language cps contification)
  #:use-module (language cps constructors)
  #:use-module (language cps cse)
  #:use-module (language cps dce)
  #:use-module (language cps dfg)
  #:use-module (language cps elide-values)
  #:use-module (language cps primitives)
  #:use-module (language cps prune-bailouts)
  #:use-module (language cps prune-top-level-scopes)
  #:use-module (language cps reify-primitives)
  #:use-module (language cps renumber)
  #:use-module (language cps self-references)
  #:use-module (language cps simplify)
  #:use-module (language cps slot-allocation)
  #:use-module (language cps specialize-primcalls)
  #:use-module (system vm assembler)
  #:export (compile-bytecode))

;; TODO: Local var names.

(define (kw-arg-ref args kw default)
  (match (memq kw args)
    ((_ val . _) val)
    (_ default)))

(define (optimize exp opts)
  (define (run-pass exp pass kw default)
    (if (kw-arg-ref opts kw default)
        (pass exp)
        exp))

  ;; The first DCE pass is mainly to eliminate functions that aren't
  ;; called.  The last is mainly to eliminate rest parameters that
  ;; aren't used, and thus shouldn't be consed.

  (let* ((exp (run-pass exp eliminate-dead-code #:eliminate-dead-code? #t))
         (exp (run-pass exp prune-top-level-scopes #:prune-top-level-scopes? #t))
         (exp (run-pass exp simplify #:simplify? #t))
         (exp (run-pass exp contify #:contify? #t))
         (exp (run-pass exp inline-constructors #:inline-constructors? #t))
         (exp (run-pass exp specialize-primcalls #:specialize-primcalls? #t))
         (exp (run-pass exp elide-values #:elide-values? #t))
         (exp (run-pass exp prune-bailouts #:prune-bailouts? #t))
         (exp (run-pass exp eliminate-common-subexpressions #:cse? #t))
         (exp (run-pass exp resolve-self-references #:resolve-self-references? #t))
         (exp (run-pass exp eliminate-dead-code #:eliminate-dead-code? #t))
         (exp (run-pass exp simplify #:simplify? #t)))
    ;; Passes that are needed:
    ;; 
    ;;  * Abort contification: turning abort primcalls into continuation
    ;;    calls, and eliding prompts if possible.
    ;;
    ;;  * Loop peeling.  Unrolls the first round through a loop if the
    ;;    loop has effects that CSE can work on.  Requires effects
    ;;    analysis.  When run before CSE, loop peeling is the equivalent
    ;;    of loop-invariant code motion (LICM).

    exp))

(define (compile-fun f asm)
  (let* ((dfg (compute-dfg f #:global? #f))
         (allocation (allocate-slots f dfg)))
    (define (maybe-slot sym)
      (lookup-maybe-slot sym allocation))

    (define (slot sym)
      (lookup-slot sym allocation))

    (define (constant sym)
      (lookup-constant-value sym allocation))

    (define (maybe-mov dst src)
      (unless (= dst src)
        (emit-mov asm dst src)))

    (define (maybe-load-constant slot src)
      (call-with-values (lambda ()
                          (lookup-maybe-constant-value src allocation))
        (lambda (has-const? val)
          (and has-const?
               (begin
                 (emit-load-constant asm slot val)
                 #t)))))

    (define (compile-entry meta)
      (let ((label (dfg-min-label dfg)))
        (match (lookup-cont label dfg)
          (($ $kentry self tail clause)
           (emit-begin-program asm label meta)
           (compile-clause (1+ label))
           (emit-end-program asm)))))

    (define (compile-clause label)
      (match (lookup-cont label dfg)
        (($ $kclause ($ $arity req opt rest kw allow-other-keys?)
            body alternate)
         (let* ((kw-indices (map (match-lambda
                                  ((key name sym)
                                   (cons key (lookup-slot sym allocation))))
                                 kw))
                (nlocals (lookup-nlocals label allocation)))
           (emit-label asm label)
           (emit-begin-kw-arity asm req opt rest kw-indices allow-other-keys?
                                nlocals
                                (match alternate (#f #f) (($ $cont alt) alt)))
           (let ((next (compile-body (1+ label) nlocals)))
             (emit-end-arity asm)
             (match alternate
               (($ $cont alt)
                (unless (eq? next alt)
                  (error "unexpected k" alt))
                (compile-clause next))
               (#f
                (unless (= next (+ (dfg-min-label dfg) (dfg-label-count dfg)))
                  (error "unexpected end of clauses")))))))))

    (define (compile-body label nlocals)
      (let compile-cont ((label label))
        (if (eq? label (+ (dfg-min-label dfg) (dfg-label-count dfg)))
            label
            (match (lookup-cont label dfg)
              (($ $kclause) label)
              (($ $kargs _ _ term)
               (emit-label asm label)
               (let find-exp ((term term))
                 (match term
                   (($ $letk conts term)
                    (find-exp term))
                   (($ $continue k src exp)
                    (when src
                      (emit-source asm src))
                    (compile-expression label k exp nlocals)
                    (compile-cont (1+ label))))))
              (_
               (emit-label asm label)
               (compile-cont (1+ label)))))))

    (define (compile-expression label k exp nlocals)
      (let* ((fallthrough? (= k (1+ label))))
        (define (maybe-emit-jump)
          (unless fallthrough?
            (emit-br asm k)))
        (match (lookup-cont k dfg)
          (($ $ktail)
           (compile-tail label exp))
          (($ $kargs (name) (sym))
           (let ((dst (maybe-slot sym)))
             (when dst
               (compile-value label exp dst nlocals)))
           (maybe-emit-jump))
          (($ $kargs () ())
           (compile-effect label exp k nlocals)
           (maybe-emit-jump))
          (($ $kargs names syms)
           (compile-values label exp syms)
           (maybe-emit-jump))
          (($ $kif kt kf)
           (compile-test label exp kt kf (and fallthrough? (1+ k))))
          (($ $kreceive ($ $arity req () rest () #f) kargs)
           (compile-trunc label k exp (length req)
                          (and rest
                               (match (lookup-cont kargs dfg)
                                 (($ $kargs names (_ ... rest)) rest)))
                          nlocals)
           (unless (and fallthrough? (= kargs (1+ k)))
             (emit-br asm kargs))))))

    (define (compile-tail label exp)
      ;; There are only three kinds of expressions in tail position:
      ;; tail calls, multiple-value returns, and single-value returns.
      (match exp
        (($ $call proc args)
         (for-each (match-lambda
                    ((src . dst) (emit-mov asm dst src)))
                   (lookup-parallel-moves label allocation))
         (let ((tail-slots (cdr (iota (1+ (length args))))))
           (for-each maybe-load-constant tail-slots args))
         (emit-tail-call asm (1+ (length args))))
        (($ $callk k proc args)
         (for-each (match-lambda
                    ((src . dst) (emit-mov asm dst src)))
                   (lookup-parallel-moves label allocation))
         (let ((tail-slots (cdr (iota (1+ (length args))))))
           (for-each maybe-load-constant tail-slots args))
         (emit-tail-call-label asm (1+ (length args)) k))
        (($ $values ())
         (emit-reset-frame asm 1)
         (emit-return-values asm))
        (($ $values (arg))
         (if (maybe-slot arg)
             (emit-return asm (slot arg))
             (begin
               (emit-load-constant asm 1 (constant arg))
               (emit-return asm 1))))
        (($ $values args)
         (for-each (match-lambda
                    ((src . dst) (emit-mov asm dst src)))
                   (lookup-parallel-moves label allocation))
         (let ((tail-slots (cdr (iota (1+ (length args))))))
           (for-each maybe-load-constant tail-slots args))
         (emit-reset-frame asm (1+ (length args)))
         (emit-return-values asm))
        (($ $primcall 'return (arg))
         (emit-return asm (slot arg)))))

    (define (compile-value label exp dst nlocals)
      (match exp
        (($ $values (arg))
         (or (maybe-load-constant dst arg)
             (maybe-mov dst (slot arg))))
        (($ $void)
         (emit-load-constant asm dst *unspecified*))
        (($ $const exp)
         (emit-load-constant asm dst exp))
        (($ $fun src meta () ($ $cont k))
         (emit-load-static-procedure asm dst k))
        (($ $fun src meta free ($ $cont k))
         (emit-make-closure asm dst k (length free)))
        (($ $primcall 'current-module)
         (emit-current-module asm dst))
        (($ $primcall 'cached-toplevel-box (scope name bound?))
         (emit-cached-toplevel-box asm dst (constant scope) (constant name)
                                   (constant bound?)))
        (($ $primcall 'cached-module-box (mod name public? bound?))
         (emit-cached-module-box asm dst (constant mod) (constant name)
                                 (constant public?) (constant bound?)))
        (($ $primcall 'resolve (name bound?))
         (emit-resolve asm dst (constant bound?) (slot name)))
        (($ $primcall 'free-ref (closure idx))
         (emit-free-ref asm dst (slot closure) (constant idx)))
        (($ $primcall 'vector-ref (vector index))
         (emit-vector-ref asm dst (slot vector) (slot index)))
        (($ $primcall 'make-vector/immediate (length init))
         (emit-make-vector/immediate asm dst (constant length) (slot init)))
        (($ $primcall 'vector-ref/immediate (vector index))
         (emit-vector-ref/immediate asm dst (slot vector) (constant index)))
        (($ $primcall 'allocate-struct/immediate (vtable nfields))
         (emit-allocate-struct/immediate asm dst (slot vtable) (constant nfields)))
        (($ $primcall 'struct-ref/immediate (struct n))
         (emit-struct-ref/immediate asm dst (slot struct) (constant n)))
        (($ $primcall 'builtin-ref (name))
         (emit-builtin-ref asm dst (constant name)))
        (($ $primcall 'bv-u8-ref (bv idx))
         (emit-bv-u8-ref asm dst (slot bv) (slot idx)))
        (($ $primcall 'bv-s8-ref (bv idx))
         (emit-bv-s8-ref asm dst (slot bv) (slot idx)))
        (($ $primcall 'bv-u16-ref (bv idx))
         (emit-bv-u16-ref asm dst (slot bv) (slot idx)))
        (($ $primcall 'bv-s16-ref (bv idx))
         (emit-bv-s16-ref asm dst (slot bv) (slot idx)))
        (($ $primcall 'bv-u32-ref (bv idx val))
         (emit-bv-u32-ref asm dst (slot bv) (slot idx)))
        (($ $primcall 'bv-s32-ref (bv idx val))
         (emit-bv-s32-ref asm dst (slot bv) (slot idx)))
        (($ $primcall 'bv-u64-ref (bv idx val))
         (emit-bv-u64-ref asm dst (slot bv) (slot idx)))
        (($ $primcall 'bv-s64-ref (bv idx val))
         (emit-bv-s64-ref asm dst (slot bv) (slot idx)))
        (($ $primcall 'bv-f32-ref (bv idx val))
         (emit-bv-f32-ref asm dst (slot bv) (slot idx)))
        (($ $primcall 'bv-f64-ref (bv idx val))
         (emit-bv-f64-ref asm dst (slot bv) (slot idx)))
        (($ $primcall name args)
         ;; FIXME: Inline all the cases.
         (let ((inst (prim-instruction name)))
           (emit-text asm `((,inst ,dst ,@(map slot args))))))))

    (define (compile-effect label exp k nlocals)
      (match exp
        (($ $values ()) #f)
        (($ $prompt escape? tag handler)
         (match (lookup-cont handler dfg)
           (($ $kreceive ($ $arity req () rest () #f) khandler-body)
            (let ((receive-args (gensym "handler"))
                  (nreq (length req))
                  (proc-slot (lookup-call-proc-slot handler allocation)))
              (emit-prompt asm (slot tag) escape? proc-slot receive-args)
              (emit-br asm k)
              (emit-label asm receive-args)
              (unless (and rest (zero? nreq))
                (emit-receive-values asm proc-slot (->bool rest) nreq))
              (when (and rest
                         (match (lookup-cont khandler-body dfg)
                           (($ $kargs names (_ ... rest))
                            (maybe-slot rest))))
                (emit-bind-rest asm (+ proc-slot 1 nreq)))
              (for-each (match-lambda
                         ((src . dst) (emit-mov asm dst src)))
                        (lookup-parallel-moves handler allocation))
              (emit-reset-frame asm nlocals)
              (emit-br asm khandler-body)))))
        (($ $primcall 'cache-current-module! (sym scope))
         (emit-cache-current-module! asm (slot sym) (constant scope)))
        (($ $primcall 'free-set! (closure idx value))
         (emit-free-set! asm (slot closure) (slot value) (constant idx)))
        (($ $primcall 'box-set! (box value))
         (emit-box-set! asm (slot box) (slot value)))
        (($ $primcall 'struct-set!/immediate (struct index value))
         (emit-struct-set!/immediate asm (slot struct) (constant index) (slot value)))
        (($ $primcall 'vector-set! (vector index value))
         (emit-vector-set! asm (slot vector) (slot index) (slot value)))
        (($ $primcall 'vector-set!/immediate (vector index value))
         (emit-vector-set!/immediate asm (slot vector) (constant index)
                                     (slot value)))
        (($ $primcall 'set-car! (pair value))
         (emit-set-car! asm (slot pair) (slot value)))
        (($ $primcall 'set-cdr! (pair value))
         (emit-set-cdr! asm (slot pair) (slot value)))
        (($ $primcall 'define! (sym value))
         (emit-define! asm (slot sym) (slot value)))
        (($ $primcall 'push-fluid (fluid val))
         (emit-push-fluid asm (slot fluid) (slot val)))
        (($ $primcall 'pop-fluid ())
         (emit-pop-fluid asm))
        (($ $primcall 'wind (winder unwinder))
         (emit-wind asm (slot winder) (slot unwinder)))
        (($ $primcall 'bv-u8-set! (bv idx val))
         (emit-bv-u8-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'bv-s8-set! (bv idx val))
         (emit-bv-s8-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'bv-u16-set! (bv idx val))
         (emit-bv-u16-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'bv-s16-set! (bv idx val))
         (emit-bv-s16-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'bv-u32-set! (bv idx val))
         (emit-bv-u32-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'bv-s32-set! (bv idx val))
         (emit-bv-s32-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'bv-u64-set! (bv idx val))
         (emit-bv-u64-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'bv-s64-set! (bv idx val))
         (emit-bv-s64-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'bv-f32-set! (bv idx val))
         (emit-bv-f32-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'bv-f64-set! (bv idx val))
         (emit-bv-f64-set! asm (slot bv) (slot idx) (slot val)))
        (($ $primcall 'unwind ())
         (emit-unwind asm))))

    (define (compile-values label exp syms)
      (match exp
        (($ $values args)
         (for-each (match-lambda
                    ((src . dst) (emit-mov asm dst src)))
                   (lookup-parallel-moves label allocation))
         (for-each maybe-load-constant (map slot syms) args))))

    (define (compile-test label exp kt kf next-label)
      (define (unary op sym)
        (cond
         ((eq? kt next-label)
          (op asm (slot sym) #t kf))
         (else
          (op asm (slot sym) #f kt)
          (unless (eq? kf next-label)
            (emit-br asm kf)))))
      (define (binary op a b)
        (cond
         ((eq? kt next-label)
          (op asm (slot a) (slot b) #t kf))
         (else
          (op asm (slot a) (slot b) #f kt)
          (unless (eq? kf next-label)
            (emit-br asm kf)))))
      (match exp
        (($ $values (sym))
         (call-with-values (lambda ()
                             (lookup-maybe-constant-value sym allocation))
           (lambda (has-const? val)
             (if has-const?
                 (if val
                     (unless (eq? kt next-label)
                       (emit-br asm kt))
                     (unless (eq? kf next-label)
                       (emit-br asm kf)))
                 (unary emit-br-if-true sym)))))
        (($ $primcall 'null? (a)) (unary emit-br-if-null a))
        (($ $primcall 'nil? (a)) (unary emit-br-if-nil a))
        (($ $primcall 'pair? (a)) (unary emit-br-if-pair a))
        (($ $primcall 'struct? (a)) (unary emit-br-if-struct a))
        (($ $primcall 'char? (a)) (unary emit-br-if-char a))
        (($ $primcall 'symbol? (a)) (unary emit-br-if-symbol a))
        (($ $primcall 'variable? (a)) (unary emit-br-if-variable a))
        (($ $primcall 'vector? (a)) (unary emit-br-if-vector a))
        (($ $primcall 'string? (a)) (unary emit-br-if-string a))
        (($ $primcall 'bytevector? (a)) (unary emit-br-if-bytevector a))
        ;; Add more TC7 tests here.  Keep in sync with
        ;; *branching-primcall-arities* in (language cps primitives) and
        ;; the set of macro-instructions in assembly.scm.
        (($ $primcall 'eq? (a b)) (binary emit-br-if-eq a b))
        (($ $primcall 'eqv? (a b)) (binary emit-br-if-eqv a b))
        (($ $primcall 'equal? (a b)) (binary emit-br-if-equal a b))
        (($ $primcall '< (a b)) (binary emit-br-if-< a b))
        (($ $primcall '<= (a b)) (binary emit-br-if-<= a b))
        (($ $primcall '= (a b)) (binary emit-br-if-= a b))
        (($ $primcall '>= (a b)) (binary emit-br-if-<= b a))
        (($ $primcall '> (a b)) (binary emit-br-if-< b a))))

    (define (compile-trunc label k exp nreq rest-var nlocals)
      (define (do-call proc args emit-call)
        (let* ((proc-slot (lookup-call-proc-slot label allocation))
               (nargs (1+ (length args)))
               (arg-slots (map (lambda (x) (+ x proc-slot)) (iota nargs))))
          (for-each (match-lambda
                     ((src . dst) (emit-mov asm dst src)))
                    (lookup-parallel-moves label allocation))
          (for-each maybe-load-constant arg-slots (cons proc args))
          (emit-call asm proc-slot nargs)
          (emit-dead-slot-map asm proc-slot
                              (lookup-dead-slot-map label allocation))
          (cond
           ((and (= 1 nreq) (and rest-var) (not (maybe-slot rest-var))
                 (match (lookup-parallel-moves k allocation)
                   ((((? (lambda (src) (= src (1+ proc-slot))) src)
                      . dst)) dst)
                   (_ #f)))
            ;; The usual case: one required live return value, ignoring
            ;; any additional values.
            => (lambda (dst)
                 (emit-receive asm dst proc-slot nlocals)))
           (else
            (unless (and (zero? nreq) rest-var)
              (emit-receive-values asm proc-slot (->bool rest-var) nreq))
            (when (and rest-var (maybe-slot rest-var))
              (emit-bind-rest asm (+ proc-slot 1 nreq)))
            (for-each (match-lambda
                       ((src . dst) (emit-mov asm dst src)))
                      (lookup-parallel-moves k allocation))
            (emit-reset-frame asm nlocals)))))
      (match exp
        (($ $call proc args)
         (do-call proc args
                  (lambda (asm proc-slot nargs)
                    (emit-call asm proc-slot nargs))))
        (($ $callk k proc args)
         (do-call proc args
                  (lambda (asm proc-slot nargs)
                    (emit-call-label asm proc-slot nargs k))))))

    (match f
      (($ $fun src meta free ($ $cont k ($ $kentry self tail clause)))
       ;; FIXME: src on kentry instead?
       (when src
         (emit-source asm src))
       (compile-entry (or meta '()))))))

(define (visit-funs proc exp)
  (match exp
    (($ $continue _ _ exp)
     (visit-funs proc exp))

    (($ $fun src meta free body)
     (proc exp)
     (visit-funs proc body))

    (($ $letk conts body)
     (visit-funs proc body)
     (for-each (lambda (cont) (visit-funs proc cont)) conts))

    (($ $cont sym ($ $kargs names syms body))
     (visit-funs proc body))

    (($ $cont sym ($ $kclause arity body alternate))
     (visit-funs proc body)
     (when alternate
       (visit-funs proc alternate)))

    (($ $cont sym ($ $kentry self tail clause))
     (when clause
       (visit-funs proc clause)))

    (_ (values))))

(define (compile-bytecode exp env opts)
  (let* ((exp (fix-arities exp))
         (exp (optimize exp opts))
         (exp (convert-closures exp))
         (exp (reify-primitives exp))
         (exp (renumber exp))
         (asm (make-assembler)))
    (visit-funs (lambda (fun)
                  (compile-fun fun asm))
                exp)
    (values (link-assembly asm #:page-aligned? (kw-arg-ref opts #:to-file? #f))
            env
            env)))
