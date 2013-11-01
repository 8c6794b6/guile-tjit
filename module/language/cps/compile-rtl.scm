;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2013 Free Software Foundation, Inc.

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
;;; Compiling CPS to RTL.  The result is in the RTL language, which
;;; happens to be an ELF image as a bytecode.
;;;
;;; Code:

(define-module (language cps compile-rtl)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps)
  #:use-module (language cps arities)
  #:use-module (language cps closure-conversion)
  #:use-module (language cps contification)
  #:use-module (language cps constructors)
  #:use-module (language cps dfg)
  #:use-module (language cps elide-values)
  #:use-module (language cps primitives)
  #:use-module (language cps reify-primitives)
  #:use-module (language cps slot-allocation)
  #:use-module (system vm assembler)
  #:export (compile-rtl))

;; TODO: Local var names.

(define (kw-arg-ref args kw default)
  (match (memq kw args)
    ((_ val . _) val)
    (_ default)))

(define (optimize exp opts)
  (define (run-pass exp pass kw default)
    (if (kw-arg-ref opts kw default)
        (begin
          (pk 'OPTIMIZING kw)
          (pass exp))
        exp))

  ;; Calls to source-to-source optimization passes go here.
  (let* ((exp (run-pass exp contify #:contify? #t))
         (exp (run-pass exp inline-constructors #:inline-constructors? #t))
         (exp (run-pass exp elide-values #:elide-values? #t)))
    ;; Passes that are needed:
    ;; 
    ;;  * Abort contification: turning abort primcalls into continuation
    ;;    calls, and eliding prompts if possible.
    ;;
    ;;  * Common subexpression elimination.  Desperately needed.  Requires
    ;;    effects analysis.
    ;;
    ;;  * Loop peeling.  Unrolls the first round through a loop if the
    ;;    loop has effects that CSE can work on.  Requires effects
    ;;    analysis.  When run before CSE, loop peeling is the equivalent
    ;;    of loop-invariant code motion (LICM).
    ;;
    ;;  * Generic simplification pass, to be run as needed.  Used to
    ;;    "clean up", both on the original raw input and after specific
    ;;    optimization passes.

    exp))

(define (collect-conts f cfa)
  (let ((srcv (make-vector (cfa-k-count cfa) #f))
        (contv (make-vector (cfa-k-count cfa) #f)))
    (fold-local-conts
     (lambda (k src cont tail)
       (let ((idx (cfa-k-idx cfa k #:default (lambda (k) #f))))
         (when idx
           (when src
             (vector-set! srcv idx src))
           (vector-set! contv idx cont))))
     '()
     (match f
       (($ $fun meta free entry)
        entry)))
    (values srcv contv)))

(define (compile-fun f asm)
  (let* ((dfg (compute-dfg f #:global? #f))
         (cfa (analyze-control-flow f dfg))
         (allocation (allocate-slots f dfg)))
    (call-with-values (lambda () (collect-conts f cfa))
      (lambda (srcv contv)
        (define (lookup-cont k)
          (vector-ref contv (cfa-k-idx cfa k)))

        (define (maybe-emit-source n)
          (let ((src (vector-ref srcv n)))
            (when src
              (emit-source asm src))))

        (define (emit-label-and-maybe-source n)
          (emit-label asm (cfa-k-sym cfa n))
          (maybe-emit-source n))

        (define (immediate-u8? val)
          (and (integer? val) (exact? val) (<= 0 val 255)))

        (define (maybe-immediate-u8 sym)
          (call-with-values (lambda ()
                              (lookup-maybe-constant-value sym allocation))
            (lambda (has-const? val)
              (and has-const? (immediate-u8? val) val))))

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
          (match (vector-ref contv 0)
            (($ $kentry self tail clauses)
             (emit-begin-program asm (cfa-k-sym cfa 0) meta)
             (maybe-emit-source 0)
             (let lp ((n 1)
                      (ks (map (match-lambda (($ $cont k) k)) clauses)))
               (match ks
                 (()
                  (unless (= n (vector-length contv))
                    (error "unexpected end of clauses"))
                  (emit-end-program asm))
                 ((k . ks)
                  (unless (eq? (cfa-k-sym cfa n) k)
                    (error "unexpected k" k))
                  (lp (compile-clause n (and (pair? ks) (car ks)))
                      ks)))))))

        (define (compile-clause n alternate)
          (match (vector-ref contv n)
            (($ $kclause ($ $arity req opt rest kw allow-other-keys?))
             (let ((kw-indices (map (match-lambda
                                     ((key name sym)
                                      (cons key (lookup-slot sym allocation))))
                                    kw))
                   (nlocals (lookup-nlocals (cfa-k-sym cfa n) allocation)))
               (emit-label-and-maybe-source n)
               (emit-begin-kw-arity asm req opt rest kw-indices
                                    allow-other-keys? nlocals alternate)
               (let ((next (compile-body (1+ n) nlocals)))
                 (emit-end-arity asm)
                 next)))))

        (define (compile-body n nlocals)
          (let compile-cont ((n n))
            (if (= n (vector-length contv))
                n
                (match (vector-ref contv n)
                  (($ $kclause) n)
                  (($ $kargs _ _ term)
                   (emit-label-and-maybe-source n)
                   (let find-exp ((term term))
                     (match term
                       (($ $letk conts term)
                        (find-exp term))
                       (($ $continue k exp)
                        (compile-expression n k exp nlocals)
                        (compile-cont (1+ n))))))
                  (_
                   (emit-label-and-maybe-source n)
                   (compile-cont (1+ n)))))))

        (define (compile-expression n k exp nlocals)
          (let* ((label (cfa-k-sym cfa n))
                 (k-idx (cfa-k-idx cfa k))
                 (fallthrough? (= k-idx (1+ n))))
            (define (maybe-emit-jump)
              (unless (= k-idx (1+ n))
                (emit-br asm k)))
            (match (vector-ref contv k-idx)
              (($ $ktail)
               (compile-tail label exp))
              (($ $kargs (name) (sym))
               (let ((dst (slot sym)))
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
               (compile-test label exp kt kf
                             (and (= k-idx (1+ n))
                                  (< (+ n 2) (cfa-k-count cfa))
                                  (cfa-k-sym cfa (+ n 2)))))
              (($ $ktrunc ($ $arity req () rest () #f) k)
               (compile-trunc label exp (length req) (and rest #t) nlocals)
               (unless (and (= k-idx (1+ n))
                            (< (+ n 2) (cfa-k-count cfa))
                            (eq? (cfa-k-sym cfa (+ n 2)) k))
                 (emit-br asm k))))))

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
            (($ $values args)
             (let ((tail-slots (cdr (iota (1+ (length args))))))
               (for-each (match-lambda
                          ((src . dst) (emit-mov asm dst src)))
                         (lookup-parallel-moves label allocation))
               (for-each maybe-load-constant tail-slots args))
             (emit-reset-frame asm (1+ (length args)))
             (emit-return-values asm))
            (($ $primcall 'return (arg))
             (emit-return asm (slot arg)))))

        (define (compile-value label exp dst nlocals)
          (match exp
            (($ $var sym)
             (maybe-mov dst (slot sym)))
            ;; FIXME: Remove ($var sym), replace with ($values (sym))
            (($ $values (arg))
             (or (maybe-load-constant dst arg)
                 (maybe-mov dst (slot arg))))
            (($ $void)
             (emit-load-constant asm dst *unspecified*))
            (($ $const exp)
             (emit-load-constant asm dst exp))
            (($ $fun meta () ($ $cont k))
             (emit-load-static-procedure asm dst k))
            (($ $fun meta free ($ $cont k))
             (emit-make-closure asm dst k (length free)))
            (($ $call proc args)
             (let ((proc-slot (lookup-call-proc-slot label allocation))
                   (nargs (length args)))
               (or (maybe-load-constant proc-slot proc)
                   (maybe-mov proc-slot (slot proc)))
               (let lp ((n (1+ proc-slot)) (args args))
                 (match args
                   (()
                    (emit-call asm proc-slot (+ nargs 1))
                    (emit-receive asm dst proc-slot nlocals))
                   ((arg . args)
                    (or (maybe-load-constant n arg)
                        (maybe-mov n (slot arg)))
                    (lp (1+ n) args))))))
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
            (($ $primcall 'make-vector (length init))
             (cond
              ((maybe-immediate-u8 length)
               => (lambda (length)
                    (emit-constant-make-vector asm dst length (slot init))))
              (else
               (emit-make-vector asm dst (slot length) (slot init)))))
            (($ $primcall 'vector-ref (vector index))
             (cond
              ((maybe-immediate-u8 index)
               => (lambda (index)
                    (emit-constant-vector-ref asm dst (slot vector) index)))
              (else
               (emit-vector-ref asm dst (slot vector) (slot index)))))
            (($ $primcall 'builtin-ref (name))
             (emit-builtin-ref asm dst (constant name)))
            (($ $primcall 'bv-u8-ref (bv idx))
             (emit-bv-u8-ref asm dst (slot bv) (slot idx)))
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
             (let ((inst (prim-rtl-instruction name)))
               (emit-text asm `((,inst ,dst ,@(map slot args))))))))

        (define (compile-effect label exp k nlocals)
          (match exp
            (($ $values ()) #f)
            (($ $prompt escape? tag handler pop)
             (match (lookup-cont handler)
               (($ $ktrunc ($ $arity req () rest () #f) khandler-body)
                (let ((receive-args (gensym "handler"))
                      (nreq (length req))
                      (proc-slot (lookup-call-proc-slot label allocation)))
                  (emit-prompt asm (slot tag) escape? proc-slot receive-args)
                  (emit-br asm k)
                  (emit-label asm receive-args)
                  (emit-receive-values asm proc-slot (->bool rest) nreq)
                  (when rest
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
            (($ $primcall 'struct-set! (struct index value))
             (emit-struct-set! asm (slot struct) (slot index) (slot value)))
            (($ $primcall 'vector-set! (vector index value))
             (call-with-values (lambda ()
                                 (lookup-maybe-constant-value index allocation))
               (lambda (has-const? index-val)
                 (if (and has-const? (integer? index-val) (exact? index-val)
                          (<= 0 index-val 255))
                     (emit-constant-vector-set! asm (slot vector) index-val
                                                (slot value))
                     (emit-vector-set! asm (slot vector) (slot index)
                                       (slot value))))))
            (($ $primcall 'variable-set! (var val))
             (emit-box-set! asm (slot var) (slot val)))
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
            (($ $var sym) (unary emit-br-if-true sym))
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

        (define (compile-trunc label exp nreq rest? nlocals)
          (match exp
            (($ $call proc args)
             (let ((proc-slot (lookup-call-proc-slot label allocation))
                   (nargs (length args)))
               (or (maybe-load-constant proc-slot proc)
                   (maybe-mov proc-slot (slot proc)))
               (let lp ((n (1+ proc-slot)) (args args))
                 (match args
                   (()
                    (emit-call asm proc-slot (+ nargs 1))
                    ;; FIXME: Only allow more values if there is a rest arg.
                    ;; Express values truncation by the presence of an
                    ;; unused rest arg instead of implicitly.
                    (emit-receive-values asm proc-slot #t nreq)
                    (when rest?
                      (emit-bind-rest asm (+ proc-slot 1 nreq)))
                    (for-each (match-lambda
                               ((src . dst) (emit-mov asm dst src)))
                              (lookup-parallel-moves label allocation))
                    (emit-reset-frame asm nlocals))
                   ((arg . args)
                    (or (maybe-load-constant n arg)
                        (maybe-mov n (slot arg)))
                    (lp (1+ n) args))))))))

        (match f
          (($ $fun meta free ($ $cont k src ($ $kentry self tail clauses)))
           (compile-entry (or meta '()))))))))

(define (visit-funs proc exp)
  (match exp
    (($ $continue _ exp)
     (visit-funs proc exp))

    (($ $fun meta free body)
     (proc exp)
     (visit-funs proc body))

    (($ $letk conts body)
     (visit-funs proc body)
     (for-each (lambda (cont) (visit-funs proc cont)) conts))

    (($ $cont sym src ($ $kargs names syms body))
     (visit-funs proc body))

    (($ $cont sym src ($ $kclause arity body))
     (visit-funs proc body))

    (($ $cont sym src ($ $kentry self tail clauses))
     (for-each (lambda (clause) (visit-funs proc clause)) clauses))

    (_ (values))))

(define (compile-rtl exp env opts)
  (pk 'COMPILING)
  (let* ((exp (fix-arities exp))
         (exp (optimize exp opts))
         (exp (convert-closures exp))
         (exp (reify-primitives exp))
         (asm (make-assembler)))
    (pk 'CODEGEN)
    (visit-funs (lambda (fun)
                  (compile-fun fun asm))
                exp)
    (values (link-assembly asm #:page-aligned? (kw-arg-ref opts #:to-file? #f))
            env
            env)))
