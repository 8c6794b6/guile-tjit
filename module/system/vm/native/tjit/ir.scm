;;; -*- mode: scheme; coding: utf-8; -*-

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

;;; Compile list of bytecode operations to CPS intermediate representation via
;;; Scheme in (almost) ANF.
;;;
;;; One of the main reasons to convert bytecode to CPS is to do floating point
;;; arithmetic efficiently. VM bytecodes uses integer index to refer locals.
;;; Those locals does not distinguish floating point values from other. In CPS
;;; format, since every variables are named uniquely, it is possible to perform
;;; floating point arithmetic directly with unboxed value in floating point
;;; register inside loop.
;;;
;;; Traced bytecode operations are compiled to CPS IR via Scheme IR. Use of
;;; Scheme as another intermediate representation is because once compiled to
;;; Scheme, compiling to CPS could be done with `(@@ (system base compile)
;;; compile)' procedure.


;;; Code:

(define-module (system vm native tjit ir)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps intset)
  #:use-module (language cps closure-conversion)
  #:use-module (language cps optimize)
  #:use-module (language cps renumber)
  #:use-module (language cps types)
  #:use-module (language scheme spec)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (system base compile)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit fragment)
  #:export (trace->cps
            trace->scm
            scm->cps
            accumulate-locals
            fixnum?
            flonum?
            *ip-key-jump-to-linked-code*
            *ip-key-set-loop-info!*

            $snapshot
            make-snapshot
            snapshot?
            snapshot-offset
            snapshot-nlocals
            snapshot-locals
            snapshot-variables
            set-snapshot-variables!
            set-snapshot-code!

            $return-address
            return-address?
            return-address-ip

            $dynamic-link
            dynamic-link?
            dynamic-link-offset))

;;;
;;; Record types
;;;

(define-record-type $return-address
  (make-return-address ip)
  return-address?
  (ip return-address-ip))

(define-record-type $dynamic-link
  (make-dynamic-link offset)
  dynamic-link?
  (offset dynamic-link-offset))

;; Data type to contain past frame data.
;;
;; Stores dynamic link, return addresses, and locals of caller procedure when
;; inlined procedure exist in trace.
(define-record-type $past-frame
  (%make-past-frame dls ras locals local-indices lowers)
  past-frame?

  ;; Association list for dynamic link: (local . pointer to fp).
  (dls past-frame-dls set-past-frame-dls!)

  ;; Association list for return address: (local . pointer to ra).
  (ras past-frame-ras set-past-frame-ras!)

  ;; Vector containing locals.
  (locals past-frame-locals set-past-frame-locals!)

  ;; All local indices found in trace.
  (local-indices past-frame-local-indices)

  ;; Lower frame data.
  (lowers past-frame-lowers))

(define (make-past-frame dls ras local-offset locals local-indices lowers)
  ;; Using hash-table to contain locals, since local index could take negative
  ;; value.
  (let ((table (make-hash-table))
        (nlocals (vector-length locals)))
    (let lp ((i 0) (end nlocals))
      (when (< i end)
        (let ((elem (and (vector-ref locals i)))
              (j (+ i local-offset)))
          (hashq-set! table j elem))
        (lp (+ i 1) end)))
    (%make-past-frame dls ras table local-indices lowers)))

(define (push-past-frame! past-frame dl ra local-offset locals)
  (set-past-frame-dls! past-frame (cons dl (past-frame-dls past-frame)))
  (set-past-frame-ras! past-frame (cons ra (past-frame-ras past-frame)))
  (let lp ((i 0)
           (end (vector-length locals))
           (to-update (past-frame-locals past-frame)))
    (when (< i end)
      (hashq-set! to-update (+ i local-offset) (vector-ref locals i))
      (lp (+ i 1) end to-update)))
  past-frame)

(define (pop-past-frame! past-frame)
  (let ((old-dls (past-frame-dls past-frame))
        (old-ras (past-frame-ras past-frame)))
    (when (not (null? old-dls))
      (set-past-frame-dls! past-frame (cdr old-dls)))
    (when (not (null? old-ras))
      (set-past-frame-ras! past-frame (cdr old-ras)))
    past-frame))

(define (past-frame-local-ref past-frame i)
  (hashq-get-handle (past-frame-locals past-frame) i))

(define (past-frame-lower-ref past-frame i)
  (let ((frames (past-frame-lowers past-frame)))
    (match frames
      (((offset . locals) . _)
       (let ((j (- i offset)))
         (or (and (<= 0 j)
                  (< j (vector-length locals))
                  (vector-ref locals j))
             #f)))
      (_ #f))))

;; Record type for snapshot.
(define-record-type $snapshot
  (%make-snapshot offset nlocals locals variables code)
  snapshot?

  ;; Integer number to shift vp->fp after returning with this snapshot.
  (offset snapshot-offset)

  ;; Number of locals at the time of snapshot.
  (nlocals snapshot-nlocals)

  ;; Association list of (local . type).
  (locals snapshot-locals)

  ;; Variables used at the time of taking exit.
  (variables snapshot-variables set-snapshot-variables!)

  ;; Native code of bailout with this snapshot.
  (code snapshot-code set-snapshot-code!))

(define (make-snapshot offset nlocals locals)
  ;; Initial variables and code are empty, using #f.
  (%make-snapshot offset nlocals locals #f #f))


;;;
;;; Type checker based on runtime values
;;;

(define (fixnum? val)
  (and (exact-integer? val)
       (<= most-negative-fixnum val most-positive-fixnum)))

(define (flonum? val)
  (and (real? val) (inexact? val)))

(define (unbound? x)
  (= (pointer-address (scm->pointer x)) #x904))

(define (false? x)
  (eq? x #f))

(define (true? x)
  (eq? x #t))


;;;
;;; Locals
;;;

(define (accumulate-locals local-offset ops)
  (let* ((ret (make-hash-table))
         (lowers '())
         (offset local-offset))
    (define (nyi st op)
      (debug 3 "ir:accumulate-locals: NYI ~a~%" op)
      st)
    (define (acc-one st op locals)
      (define-syntax-rule (push-offset! n)
        (set! offset (+ offset n)))
      (define-syntax-rule (pop-offset! n)
        (set! offset (- offset n)))
      (define-syntax add!
        (syntax-rules ()
          ((_ st i j k l)
           (add! (add! st i j) k l))
          ((_ st i j k)
           (add! (add! st i j) k))
          ((_ st i j)
           (add! (add! st i) j))
          ((_ st i)
           (begin
             (hashq-set! st (+ i offset) #t)
             st))))
      (match op
        ((op a1)
         (case op
           ((return)
            ;; Store proc, returned value, VM frame dynamic link, and VM frame
            ;; return address.
            (add! st a1 1 -1 -2))
           ((br tail-call)
            st)
           (else
            (nyi st op))))
        ((op a1 a2)
         (case op
           ((call)
            ;; Store proc, VM frame dynamic link, and VM frame return address.
            (let ((st (add! st a1 (- a1 1) (- a1 2))))
              (push-offset! a1)
              st))
           ((static-ref
             make-short-immediate make-long-immediate make-long-long-immediate)
            (add! st a1))
           ((mov sub1 add1 box-ref box-set!)
            (add! st a1 a2))
           ((assert-nargs-ee/locals)
            st)
           (else
            (nyi st op))))
        ((op a1 a2 a3)
         (case op
           ((add sub mul div quo)
            (add! st a1 a2 a3))
           ((call-label)
            (let ((st (add! st a1)))
              (push-offset! a1)
              st))
           ((receive)
            (pop-offset! a2)
            (set! lowers (acons offset locals lowers))
            (add! st a1 a2))
           ((receive-values)
            (pop-offset! a1)
            (add! st a1))
           (else
            (nyi st op))))
        ((op a1 a2 a3 a4)
         (case op
           ((br-if-< br-if-= br-if-<=)
            (add! st a1 a2))
           (else
            (nyi st op))))
        ((op a1 a2 a3 a4 a5)
         (case op
           ((toplevel-box)
            (add! st a1))
           (else
            (nyi st op))))
        ((op)
         (nyi st op))
        (_
         (error (format #f "ir:accumulate-locals: ~a" ops)))))
    (define (acc st ops)
      (match ops
        (((op _ _ _ locals) . rest)
         (acc (acc-one st op locals) rest))
        (()
         st)))
    (let ((local-indices (sort (hash-fold (lambda (k v acc)
                                            (cons k acc))
                                          '()
                                          (acc ret ops))
                               >)))
      (let ((verbosity (lightning-verbosity)))
        (when (and (number? verbosity)
                   (<= 3 verbosity))
          (format #t ";;; local-indices:~%")
          (format #t ";;;   ~a~%" local-indices)
          (format #t ";;; lowers:~%")
          (for-each (lambda (lower)
                      (format #t ";;;   ~a~%" lower))
                    lowers)))

      ;; Make past-frame with locals in lower frames.
      ;;
      ;; Lower frame data is saved at the time of accumulation. Otherwise, if
      ;; one of the guard operation appeared soon after bytecode sequence
      ;; `return' and `receive', snapshot does not know the value of locals in
      ;; lower frame. When recorded bytecode contains `return', snapshot will
      ;; recover a frame lower than the one used to enter the native call.
      ;;
      (make-past-frame '() '() local-offset #() local-indices lowers))))


;;;
;;; IP Keys
;;;

(define *ip-key-jump-to-linked-code* 0)
(define *ip-key-set-loop-info!* 1)


;;;
;;; Auxiliary procedures for ANF compiler
;;;

(define (from-fixnum num)
  `(let ((tmp (%lsh ,num 2)))
     (%add tmp 2)))

(define (to-fixnum scm)
  `(%rsh ,scm 2))

(define (to-double scm)
  `(%cell-object/f ,scm 2))


;;;
;;; Scheme ANF compiler
;;;

(define (trace->scm fragment exit-id loop? ops)
  (define-syntax br-op-size (identifier-syntax 2))
  (define root-trace? (not fragment))
  (define *initial-ip*
    (cadr (car ops)))
  (define *initial-locals*
    (list-ref (car ops) 4))
  (define *initial-nlocals*
    (vector-length *initial-locals*))
  (define *parent-snapshot*
    (and fragment
         (hashq-ref (fragment-snapshots fragment) exit-id)))
  (define *parent-snapshot-locals*
    (match *parent-snapshot*
      (($ $snapshot _ _ locals) locals)
      (_ #f)))
  (define (dereference-scm addr)
    (pointer->scm (dereference-pointer (make-pointer addr))))
  (define (make-var index)
    (string->symbol (string-append "v" (number->string index))))
  (define (make-vars locals)
    ;; Might better to use other data structure than alist for variables.
    ;; Number of variables won't change after getting the number of locals from
    ;; `accumulate-locals'.
    (map (lambda (n)
           (cons n (make-var n)))
         locals))
  (define (get-initial-offset)
    ;; Initial offset of side trace is where parent trace left, using offset
    ;; value from snapshot data.
    (if root-trace?
        0
        (match *parent-snapshot*
          (($ $snapshot offset)
           offset)
          (_
           (error "parent snapshot not found" exit-id)))))
  (define (get-initial-snapshot-id)
    ;; Snapshot ID `0' in root trace contains no locals, used for guard failure
    ;; in entry clause to quickly return to VM interpreter without restoring
    ;; frame locals.
    (if root-trace? 1 0))

  (let* ((local-offset (get-initial-offset))
         (past-frame (accumulate-locals local-offset ops))
         (local-indices (past-frame-local-indices past-frame))
         (args (map make-var (reverse local-indices)))
         (vars (make-vars local-indices))
         (expecting-types (make-hash-table))
         (known-types (make-hash-table))
         (lowest-offset 0)
         (snapshots (make-hash-table))
         (snapshot-id (get-initial-snapshot-id)))

    (define-syntax-rule (push-offset! n)
      (set! local-offset (+ local-offset n)))

    (define-syntax-rule (pop-offset! n)
      (set! local-offset (- local-offset n)))

    ;; Some bytecode operation fills in local with statically known value:
    ;; `make-short-immediate', `static-ref', `toplevel-box' ... etc. Filling in
    ;; known type when those bytecode operations were seen.
    (define-syntax-rule (set-known-type! idx ty)
      (let ((i (+ idx local-offset)))
        (when (not (hashq-ref known-types i))
          (hashq-set! known-types i ty))))

    ;; Some bytecode operations expect particular type: `add', `mul', ... etc.
    ;; Add type to expect with expecting bytecode operations.
    (define-syntax-rule (set-expecting-type! idx ty)
      (let ((i (+ idx local-offset)))
        (when (and (not (hashq-ref expecting-types i))
                   (not (hashq-ref known-types i)))
          (hashq-set! expecting-types i ty))))

    (define *vars-from-parent*
      (let lp ((vars vars) (acc '()))
        (match vars
          (((n . var) . vars)
           (if (assq-ref *parent-snapshot-locals* n)
               (lp vars (cons (cons n var) acc))
               (lp vars acc)))
          (()
           acc))))

    (define *args-from-parent*
      (map cdr *vars-from-parent*))

    (define *local-indices-of-args*
      (if root-trace?
          local-indices
          (let lp ((local-indices local-indices) (acc '()))
            (match local-indices
              ((n . local-indices)
               (if (assq-ref *parent-snapshot-locals* n)
                   (lp local-indices (cons n acc))
                   (lp local-indices acc)))
              (()
               (reverse! acc))))))

    (define (parent-snapshot-local-ref i)
      (and *parent-snapshot-locals*
           (assq-ref *parent-snapshot-locals* i)))

    (define (type-of obj)
      (cond
       ((fixnum? obj) &exact-integer)
       ((flonum? obj) &flonum)
       ((char? obj) &char)
       ((unspecified? obj) &unspecified)
       ((unbound? obj) &unbound)
       ((false? obj) &false)
       ((true? obj) &true)
       ((procedure? obj) &procedure)
       ((pair? obj) &pair)
       ((variable? obj) &box)
       (else
        (debug 3 "*** Type not determined: ~a~%" obj)
        #f)))

    (define (take-snapshot! ip offset locals indices args)
      (define-syntax-rule (local-ref i)
        (vector-ref locals i))
      (define (compute-args args)
        (debug 3 ";;;   compute-args: lowest-offset=~a local-offset=~a~%"
               lowest-offset local-offset)
        ;; XXX: Getting number from symbol. Could use `vars' instead of `args'.
        (filter (lambda (arg)
                  (let* ((str (substring (symbol->string arg) 1)))
                    (<= lowest-offset (string->number str))))
                args))
      (define (shift-lowest acc)
        (map (match-lambda
              ((n . local)
               `(,(- n lowest-offset) . ,local)))
             acc))
      (debug 3 ";;; take-snapshot!:~%")
      (debug 3 ";;;   snapshot-id=~a~%" snapshot-id)
      (debug 3 ";;;   ip=~x, offset=~a~%" ip offset)
      (debug 3 ";;;   vars=~a~%" vars)
      (debug 3 ";;;   (vector-length locals)=~a~%" (vector-length locals))
      (debug 3 ";;;   local-offset=~a~%" local-offset)
      (debug 3 ";;;   past-frame-dls=~a~%" (past-frame-dls past-frame))
      (debug 3 ";;;   past-frame-ras=~a~%" (past-frame-ras past-frame))
      (when (< local-offset lowest-offset)
        (set! lowest-offset local-offset))
      (let lp ((is (reverse indices)) (acc '()))
        (match is
          ((i . is)
           (define (dl-or-ra i)
             (or (assq-ref (past-frame-dls past-frame) i)
                 (assq-ref (past-frame-ras past-frame) i)
                 (let ((val (parent-snapshot-local-ref i)))
                   (and (or (dynamic-link? val)
                            (return-address? val))
                        val))))
           (define (add-local local)
             (debug 3 ";;;   add-local: i=~a, local=~a~%" i local)
             (let ((type (type-of local)))
               (if type
                   (lp is (cons `(,i . ,type) acc))
                   (lp is acc))))
           (define (add-val val)
             (debug 3 ";;;   add-val: i=~a, val=~a~%" i val)
             (lp is (cons `(,i . ,val) acc)))
           (cond
            ((= local-offset 0)
             (if (< i 0)
                 (let ((frame-val (dl-or-ra i)))
                   (if frame-val
                       (add-val frame-val)
                       (add-local (past-frame-lower-ref past-frame i))))
                 (add-local (and (< i (vector-length locals))
                                 (local-ref i)))))

            ;; Dynamic link and return address might need to be passed from
            ;; parent trace. When side trace of inlined procedure takes bailout
            ;; code, recorded trace might not contain bytecode operation to fill
            ;; in the dynamic link and return address of past frame.
            ((dl-or-ra i)
             => add-val)

            ;; Local in inlined procedure.
            ((<= 0 local-offset i)
             (let ((j (- i local-offset)))
               (if (< -1 j (vector-length locals))
                   (add-local (local-ref j))
                   (begin
                     (debug 1 ";;;   i=~a, local-offset=~a, skipping~%"
                            i local-offset)
                     (lp is acc)))))

            ;; Local in lower frame.
            ((<= local-offset i 0)
             (let ((j (- i local-offset)))
               (if (< -1 j (vector-length locals))
                   (add-local (local-ref j))
                   (begin
                     (debug 1 ";;;   i=~a, local-offset=~a, skipping~%"
                            i local-offset)
                     (lp is acc)))))

            ;; When side trace contains inlined procedure and the guard taking
            ;; this snapshot is from the caller of the inlined procedure,
            ;; saving local in upper frame. Looking up locals from newest
            ;; locals in past-frame.
            ((past-frame-local-ref past-frame i)
             => (match-lambda ((_ . local)
                               (add-local local))))

            ;; Side trace could start from the middle of inlined procedure,
            ;; locals in past frame may not have enough information to recover
            ;; locals in caller of the inlined procedure. In such case, look
            ;; up locals in the snapshot of parent trace.
            ((parent-snapshot-local-ref i)
             => add-val)

            ;; Giving up, skip this local.
            (else
             (debug 3 "*** local for snapshot-id=~a i=~a not found~%"
                    snapshot-id i)
             (add-local #f))))
          (()
           (let* ((acc (reverse! acc))
                  (args (compute-args args))
                  (snapshot (make-snapshot local-offset
                                           (vector-length locals)
                                           (shift-lowest acc))))
             (debug 3 ";;; computed-args=~a~%" args)

             ;; Using snapshot-id as key for hash-table. Identical IP could
             ;; be used for entry clause and first operation in the loop.
             (hashq-set! snapshots snapshot-id snapshot)
             (set! snapshot-id (+ snapshot-id 1))

             ;; Call dummy procedure to capture CPS variables by emitting
             ;; `$call' term.  It might not a good way to use `$call', though
             ;; no other idea to capture CPS variables with less amount of CPS
             ;; terms.
             `(,(+ ip (* offset 4)) ,@args))))))

    (define (convert-one escape op ip fp ra locals rest)
      (define-syntax-rule (local-ref i)
        (vector-ref locals i))
      (define-syntax-rule (var-ref i)
        (assq-ref vars (+ i local-offset)))

      ;; (debug 3 ";;; convert-one: ~a ~a ~a ~a~%"
      ;;        (or (and (number? ip) (number->string ip 16))
      ;;            #f)
      ;;        local-offset
      ;;        op
      ;;        locals)

      (match op
        ;; *** Call and return

        ;; XXX: halt

        (('call proc nlocals)
         ;; When procedure get inlined, taking snapshot of previous frame,
         ;; contents of previous frame could change in native code loop.
         ;;
         ;; XXX: Need to add a guard to test that whether the `proc' local match
         ;; with the value used during compilation? Frame local after returning
         ;; from callee procedure might differ.
         (let* ((dl (cons (- (+ proc local-offset) 2)
                          (make-dynamic-link local-offset)))
                (ra (cons (- (+ proc local-offset) 1)
                          (make-return-address (make-pointer (+ ip (* 2 4))))))
                (vproc (var-ref proc))
                (rproc (local-ref proc))
                (snapshot (take-snapshot! ip 0 locals local-indices args)))
           (debug 3 ";;; ir.scm:call proc=~a local-offset=~a fp=~a~%"
                  rproc local-offset fp)
           (set-expecting-type! proc &procedure)
           (push-past-frame! past-frame dl ra local-offset locals)
           (push-offset! proc)
           `(begin
              ,snapshot
              ;; Adding `%eq' guard to test the procedure value, so that
              ;; re-defined procedure with same name will not get updated.
              (%eq ,vproc ,(pointer-address (scm->pointer rproc)))
              ,(convert escape rest))))

        (('call-label proc nlocals label)
         (push-offset! proc)
         (convert escape rest))

        ;; XXX: tail-call
        (('tail-call nlocals)
         (convert escape rest))

        ;; XXX: tail-call-label
        ;; XXX: tail-call/shuffle

        (('receive dst proc nlocals)
         (pop-offset! proc)
         (debug 3 ";;; ir.scm:receive args=~a fp=~x ra=~x ip=~x~%"
                args (pointer-address fp) ra ip)

         ;; `local-offset' could be modified during `convert' with
         ;; `push-offset!' and `pop-offset!'. Wrapping next expression as
         ;; anonymous procedure to prevent overriding variables at this point.
         (let* ((vdst (var-ref dst))
                (vret (var-ref (+ proc 1)))
                (do-next-convert (lambda ()
                                   `(let ((,vdst ,vret))
                                      ,(convert escape rest)))))
           (if (< 0 local-offset)
               (do-next-convert)
               (let lp ((vars vars))
                 (match vars
                   (((n . var) . vars)
                    (debug 3 ";;; ir.scm::receive n=~a var=~a~%" n var)
                    (cond
                     ;; Two locals below callee procedure in VM frame contain
                     ;; dynamic link and return address. VM interpreter refills
                     ;; these two with #f, doing the same thing.
                     ((or (= n -1) (= n -2))
                      `(let ((,var #f))
                         ,(lp vars)))
                     ((< n 0)
                      (let* ((i (- n local-offset))
                             (val (if (< -1 i (vector-length locals))
                                      (vector-ref locals i)
                                      #f))
                             (type (type-of val)))
                        `(let ((,var (%frame-ref ,(- n local-offset) ,type)))
                           ,(lp vars))))
                     (else
                      (lp vars))))
                   (()
                    (do-next-convert)))))))

        (('receive-values proc allow-extra? nvalues)
         (pop-offset! proc)
         (escape #f))

        (('return src)
         (let* ((vsrc (var-ref src))
                (vdst (var-ref 1))
                (assign (if (eq? vdst vsrc)
                            '()
                            `((,vdst ,vsrc))))
                (return (if (< 0 local-offset)
                            '()
                            `((%return ,ra)))))
           (pop-past-frame! past-frame)
           (debug 3 ";;; ir.scm:return~%;;;    fp=~a ip=~x ra=~x local-offset=~a~%"
                  fp ip ra local-offset)
           (debug 3 ";;;    locals=~a~%;;;    local-indices=~a~%;;;    args=~a~%"
                  locals local-indices args)
           `(let ,assign
              ,@return
              ,(convert escape rest))))

        ;; XXX: return-values

        ;; *** Specialized call stubs

        ;; XXX: subr-call
        ;; XXX: foreign-call
        ;; XXX: continuation-call
        ;; XXX: compose-continuation
        ;; XXX: tail-apply
        ;; XXX: call/cc
        ;; XXX: abort
        ;; XXX: builtin-ref

        ;; *** Function prologues

        ;; XXX: br-if-nargs-ne
        ;; XXX: br-if-nargs-lt
        ;; XXX; br-if-nargs-gt
        ;; XXX: assert-nargs-ee
        ;; XXX: assert-nargs-ge
        ;; XXX: assert-nargs-le
        ;; XXX: alloc-frame
        ;; XXX: reset-frame

        (('assert-nargs-ee/locals expected nlocals)
         (convert escape rest))

        ;; XXX: br-if-npos-gt
        ;; XXX: bind-kw-args
        ;; XXX: bind-rest

        ;; *** Branching instructions

        (('br offset)
         (convert escape rest))

        ;; XXX: br-if-true
        ;; XXX: br-if-null
        ;; XXX: br-if-nil
        ;; XXX: br-if-pair
        ;; XXX: br-if-struct
        ;; XXX: br-if-char
        ;; XXX: br-if-tc7
        ;; XXX: br-if-eq
        ;; XXX: br-if-eqv
        ;; XXX: br-if-equal

        (('br-if-= a b invert? offset)
         (let ((ra (local-ref a))
               (rb (local-ref b))
               (va (var-ref a))
               (vb (var-ref b)))
           (let ((dest (if (= ra rb)
                           (if invert? offset br-op-size)
                           (if invert? br-op-size offset))))
             (cond
              ((and (fixnum? ra) (fixnum? rb))
               (set-expecting-type! a &exact-integer)
               (set-expecting-type! b &exact-integer)
               `(begin
                  ,(take-snapshot! ip dest locals local-indices args)
                  ,(if (= ra rb) `(%eq ,va ,vb) `(%ne ,va ,vb))
                  ,(convert escape rest)))
              (else
               (debug 3 "*** ir:convert = ~a ~a~%" ra rb)
               (escape #f))))))

        (('br-if-< a b invert? offset)
         (let ((ra (local-ref a))
               (rb (local-ref b))
               (va (var-ref a))
               (vb (var-ref b)))
           (let ((dest (if (< ra rb)
                           (if invert? offset br-op-size)
                           (if invert? br-op-size offset))))
             (cond
              ((and (fixnum? ra) (fixnum? rb))
               (set-expecting-type! a &exact-integer)
               (set-expecting-type! b &exact-integer)
               `(begin
                  ,(take-snapshot! ip dest locals local-indices args)
                  ,(if (< ra rb) `(%lt ,va ,vb) `(%ge ,va ,vb))
                  ,(convert escape rest)))

              ((and (flonum? ra) (flonum? rb))
               (set-expecting-type! a &flonum)
               (set-expecting-type! b &flonum)
               `(begin
                  ,(take-snapshot! ip dest locals local-indices args)
                  ,(if (< ra rb) `(%flt ,va ,vb) `(%fge ,va ,vb))
                  ,(convert escape rest)))
              (else
               (debug 3 "ir:convert < ~a ~a~%" ra rb)
               (escape #f))))))

        ;; XXX: br-if-<=

        ;; *** Lexical binding instructions

        (('mov dst src)
         (let ((vdst (var-ref dst))
               (vsrc (var-ref src)))
           (cond
            ((flonum? (local-ref src))
             (set-expecting-type! src &flonum)
             (set-known-type! dst &flonum))
            ((fixnum? (local-ref src))
             (set-expecting-type! src &exact-integer)
             (set-known-type! dst &exact-integer)))
           `(let ((,vdst ,vsrc))
              ,(convert escape rest))))

        ;; XXX: long-mov
        ;; XXX: box

        ;; XXX: Reconsider how to manage `box', `box-ref', and `box-set!'.
        ;; Boxing back tagged value every time will make the loop slow, need
        ;; more analysis when the storing could be removed from native code loop
        ;; and delayed to side exit code.
        ;;
        ;; XXX: Add test for nested boxes.

        (('box-ref dst src)
         ;; XXX: Add guard to check type of box contents.
         (let ((vdst (var-ref dst))
               (vsrc (var-ref src))
               (rsrc (and (< src (vector-length locals))
                          (variable-ref (vector-ref locals src)))))
           (set-expecting-type! src &box)
           `(let ((,vdst (%cell-object ,vsrc 1)))
              ,(cond
                ((flonum? rsrc)
                 `(let ((,vdst ,(to-double vdst)))
                    ,(convert escape rest)))
                ((fixnum? rsrc)
                 `(let ((,vdst ,(to-fixnum vdst)))
                    ,(convert escape rest)))
                (else
                 (convert escape rest))))))

        (('box-set! dst src)
         (let ((vdst (var-ref dst))
               (vsrc (var-ref src))
               (rdst (and (< dst (vector-length locals))
                          (variable-ref (vector-ref locals dst)))))
           (set-expecting-type! dst &box)
           (cond
            ((flonum? rdst)
             `(let ((,vsrc (%from-double ,vsrc)))
                (%set-cell-object! ,vdst 1 ,vsrc)
                ,(convert escape rest)))
            ((fixnum? rdst)
             `(let ((,vsrc ,(from-fixnum vsrc)))
                (%set-cell-object! ,vdst 1 ,vsrc)
                ,(convert escape rest)))
            (else
             `(begin
                (%set-cell-object! ,vdst 1 ,vsrc)
                ,(convert escape rest))))))

        ;; XXX: make-closure
        ;; XXX: free-ref
        ;; XXX: free-set!

        ;; *** Immediates and statically allocated non-immediates

        (('make-short-immediate dst low-bits)
         (set-known-type! dst &exact-integer)
         `(let ((,(var-ref dst) ,(ash low-bits -2)))
            ,(convert escape rest)))

        (('make-long-immediate dst low-bits)
         (set-known-type! dst &exact-integer)
         `(let ((,(var-ref dst) ,(ash low-bits -2)))
            ,(convert escape rest)))

        (('make-long-long-immediate dst high-bits low-bits)
         `(let ((,(var-ref dst)
                 ,(ash (logior (ash high-bits 32) low-bits) -2)))
            ,(convert escape rest)))

        ;; XXX: make-non-immediate

        (('static-ref dst offset)
         `(let ((,(var-ref dst) ,(dereference-scm (+ ip (* 4 offset)))))
            ,(convert escape rest)))

        ;; XXX: static-set!
        ;; XXX: static-patch!

        ;; *** Mutable top-level bindings

        ;; XXX: current-module
        ;; XXX: resolve
        ;; XXX: define!

        (('toplevel-box dst var-offset mod-offset sym-offset bound?)
         (set-known-type! dst &box)
         (let ((vdst (var-ref dst))
               (src (pointer-address
                     (scm->pointer
                      (dereference-scm (+ ip (* var-offset 4)))))))
           `(let ((,vdst ,src))
              ,(convert escape rest))))

        ;; XXX: module-box

        ;; *** The dynamic environment

        ;; XXX: prompt
        ;; XXX: wind
        ;; XXX: unwind
        ;; XXX: push-fluid
        ;; XXX: pop-fluid
        ;; XXX: fluid-ref
        ;; XXX: fluid-set

        ;; *** Strings, symbols, and keywords

        ;; XXX: string-length
        ;; XXX: string-ref
        ;; XXX: string->number
        ;; XXX: string->symbol
        ;; XXX: symbol->keyword

        ;; *** Pairs

        ;; XXX: cons
        ;; XXX: car
        ;; XXX: cdr
        ;; XXX: set-car!
        ;; XXX: set-cdr!

        ;; *** Numeric operations

        (('add dst a b)
         (let ((rdst (local-ref dst))
               (ra (local-ref a))
               (rb (local-ref b))
               (vdst (var-ref dst))
               (va (var-ref a))
               (vb (var-ref b)))
           (cond
            ((and (fixnum? ra) (fixnum? rb))
             (set-expecting-type! a &exact-integer)
             (set-expecting-type! b &exact-integer)
             `(let ((,vdst (%add ,va ,vb)))
                ,(convert escape rest)))
            ((and (flonum? ra) (flonum? rb))
             (set-expecting-type! a &flonum)
             (set-expecting-type! b &flonum)
             `(let ((,vdst (%fadd ,va ,vb)))
                ,(convert escape rest)))
            (else
             (debug 3 "ir:convert add ~a ~a ~a~%" rdst ra rb)
             (escape #f)))))

        (('add1 dst src)
         (let ((rdst (local-ref dst))
               (rsrc (local-ref src))
               (vdst (var-ref dst))
               (vsrc (var-ref src)))
           (cond
            ((fixnum? rsrc)
             (set-expecting-type! src &exact-integer)
             `(let ((,vdst (%add ,vsrc 1)))
                ,(convert escape rest)))
            (else
             (debug 3 "ir:convert add1 ~a ~a" rdst rsrc)
             (escape #f)))))

        (('sub dst a b)
         (let ((rdst (local-ref dst))
               (ra (local-ref a))
               (rb (local-ref b))
               (vdst (var-ref dst))
               (va (var-ref a))
               (vb (var-ref b)))
           (cond
            ((and (fixnum? ra) (fixnum? rb))
             (set-expecting-type! a &exact-integer)
             (set-expecting-type! b &exact-integer)
             `(let ((,vdst (%sub ,va ,vb)))
                ,(convert escape rest)))
            ((and (flonum? ra) (flonum? rb))
             (set-expecting-type! a &flonum)
             (set-expecting-type! b &flonum)
             `(let ((,vdst (%fsub ,va ,vb)))
                ,(convert escape rest)))
            (else
             (debug 3 "ir:convert sub ~a ~a ~a~%" rdst ra rb)
             (escape #f)))))

        (('sub1 dst src)
         (let ((rdst (local-ref dst))
               (rsrc (local-ref src))
               (vdst (var-ref dst))
               (vsrc (var-ref src)))
           (cond
            ((fixnum? rsrc)
             (set-expecting-type! src &exact-integer)
             `(let ((,vdst (%sub ,vsrc 1)))
                ,(convert escape rest)))
            (else
             (debug 3 "ir:convert sub1 ~a ~a~%" rdst rsrc)
             (escape #f)))))

        (('mul dst a b)
         (let ((rdst (local-ref dst))
               (ra (local-ref a))
               (rb (local-ref b))
               (vdst (var-ref dst))
               (va (var-ref a))
               (vb (var-ref b)))
           (cond
            ((and (flonum? ra) (flonum? rb))
             (set-expecting-type! a &flonum)
             (set-expecting-type! b &flonum)
             `(let ((,vdst (%fmul ,va ,vb)))
                ,(convert escape rest)))
            (else
             (debug 3 "*** ir:convert: NYI mul ~a ~a ~a~%" rdst ra rb)
             (escape #f)))))

        ;; XXX: div
        ;; XXX: quo
        ;; XXX: rem

        (('mod dst a b)
         (let ((rdst (local-ref dst))
               (ra (local-ref a))
               (rb (local-ref b))
               (vdst (var-ref dst))
               (va (var-ref a))
               (vb (var-ref b)))
           (cond
            ((and (fixnum? ra) (fixnum? rb))
             (set-expecting-type! a &exact-integer)
             (set-expecting-type! b &exact-integer)
             `(let ((,vdst (%mod ,va ,vb)))
                ,(convert escape rest)))
            (else
             (debug 3 "*** ir:convert: NYI mod ~a ~a ~a~%" rdst ra rb)
             (escape #f)))))

        ;; XXX: ash
        ;; XXX: logand
        ;; XXX: logior
        ;; XXX: logxor
        ;; XXX: make-vector
        ;; XXX: make-vector/immediate
        ;; XXX: vector-length
        ;; XXX: vector-ref
        ;; XXX: vector-ref/immediate

        ;; XXX: vector-set!
        (('vector-set! dst idx src)
         (let ((rdst (local-ref dst))
               (rsrc (local-ref src))
               (ridx (local-ref idx))
               (vdst (var-ref dst))
               (vsrc (var-ref src))
               (vidx (var-ref idx)))
           (cond
            ((and (vector? rsrc) (fixnum? ridx))
             (set-expecting-type! dst &vector)
             `(begin
                (%vector-set! ,vdst ,vsrc ,vidx)
                ,(convert escape rest)))
            (else
             (debug 3 "*** ir.scm:convert: NYI vector-set! ~a ~a ~a~%"
                    rdst rsrc ridx)
             (escape #f)))))

        ;; XXX: vector-set!/immediate

        ;; *** Structs and GOOPS

        ;; XXX: struct-vtable
        ;; XXX: allocate-struct/immediate
        ;; XXX: struct-ref/immediate
        ;; XXX: struct-set!/immediate
        ;; XXX: class-of

        ;; *** Arrays, packed uniform arrays, and bytevectors

        ;; XXX: load-typed-array
        ;; XXX: make-array
        ;; XXX: bv-u8-ref
        ;; XXX: bv-s8-ref
        ;; XXX: bv-u16-ref
        ;; XXX: bv-s16-ref
        ;; XXX: bv-u32-ref
        ;; XXX: bv-s32-ref
        ;; XXX: bv-u64-ref
        ;; XXX: bv-s64-ref
        ;; XXX: bv-f32-ref
        ;; XXX: bv-f64-ref
        ;; XXX: bv-u8-set!
        ;; XXX: bv-s8-set!
        ;; XXX: bv-u16-set!
        ;; XXX: bv-s16-set!
        ;; XXX: bv-u32-set!
        ;; XXX: bv-s32-set!
        ;; XXX: bv-u64-set!
        ;; XXX: bv-s64-set!
        ;; XXX: bv-f32-set!
        ;; XXX: bv-f64-set!

        ;; *** Move above

        ;; XXX: br-if-logtest
        ;; XXX: allocate-struct
        ;; XXX: struct-ref
        ;; XXX: struct-set!

        ;; *** Misc

        (('take-snapshot! ip offset)
         (take-snapshot! ip offset locals local-indices args))

        (op
         (debug 3 "*** ir:convert: NYI ~a~%" (car op))
         (escape #f))))

    (define (convert escape traces)
      (match traces
        (((op ip fp ra locals) . ())
         (if (and root-trace? loop?)
             ;; Root trace with loop. Emit `loop', which is the name of
             ;; procedure for looping the body of Scheme IR emitted in
             ;; `make-scm'.
             (convert-one escape op ip fp ra locals
                          `(loop ,@(filter identity (reverse (map cdr vars)))))
             ;; Side trace or loop-less root trace.  Capturing CPS variables with
             ;; `take-snapshot!' at the end, so that the native code can pass the
             ;; register information to linked trace.
             (let* ((snap! `(take-snapshot! ,*ip-key-jump-to-linked-code* 0))
                    (last-op `((,snap! ,ip #f #f ,locals))))
               (convert-one escape op ip fp ra locals last-op))))
        (((op ip fp ra locals) . rest)
         (convert-one escape op ip fp ra locals rest))
        (_ traces)))

    (define (choose-type-for-local n local)
      ;; Current policy for deciding the type of initial argument loaded from
      ;; frame:
      ;;
      ;; * If local has known type, use the known type.
      ;; * Else if local has expecting type, use the expecting type.
      ;; * Else if local is found, load from frame with type of the found
      ;;   value.
      ;; * Otherwise, initialize the variable as false, without loading from
      ;;   frame.
      (cond
       ((hashq-ref known-types n)
        => identity)
       ((hashq-ref expecting-types n)
        => identity)
       ((type-of local)
        => identity)
       (else
        #f)))

    (define (add-initial-loads exp-body)
      (debug 3 ";;; add-initial-loads:~%")
      (debug 3 ";;;   known-types=~{~a ~}~%" (hash-map->list cons known-types))
      (debug 3 ";;;   initial-locals=~a~%" *initial-locals*)
      (let lp ((vars vars))
        (match vars
          (((n . var) . vars)
           (cond
            ;; When locals index was found in parent snapshot locals, the local
            ;; will be passed from parent fragment, ignoreing.
            ;;
            ;; If local index is negative, locals from lower frame won't be
            ;; passed as argument. Loading later with CPS IR '%frame-ref'
            ;; or '%frame-ref/f'.
            ;;
            ;; Locals index exceeding initial number of locals are also ignored,
            ;; should not loaded at the time of entering the native code.
            ;;
            ((or (assq-ref *parent-snapshot-locals* n)
                 (< n 0)
                 (<= *initial-nlocals* n))
             (lp vars))
            (else
             (let* ((snapshot0 (hashq-ref snapshots 0))
                    (i (- n (snapshot-offset snapshot0)))
                    (local (if (and (< -1 i)
                                    (< i (vector-length *initial-locals*)))
                               (vector-ref *initial-locals* i)
                               #f))
                    (type (choose-type-for-local n local)))
               (debug 3 ";;; add-initial-loads: n=~a local=~a~%" n local)
               (debug 3 ";;;   known-type:     ~a~%" (hashq-ref known-types n))
               (debug 3 ";;;   expecting-type: ~a~%" (hashq-ref expecting-types n))
               (debug 3 ";;;   local:          ~a~%" local)
               (debug 3 ";;;   type:           ~a~%" type)
               (cond
                ((not type)
                 `(let ((,var #f))
                    ,(lp vars)))
                ((= type &flonum)
                 `(let ((,var (%frame-ref/f ,n)))
                    ,(lp vars)))
                (else
                 `(let ((,var (%frame-ref ,n ,type)))
                    ,(lp vars))))))))
          (()
           exp-body))))

    (define (make-entry-body)
      `(begin
         (,*initial-ip*)
         ,(let lp ((lp-vars vars))
            (match lp-vars
              (((i . var) . lp-vars)
               (let* ((type (or (hashq-ref expecting-types i)
                                &box))
                      (exp (if (= type &flonum)
                               `(let ((,var (%frame-ref/f ,i)))
                                  ,(lp lp-vars))
                               `(let ((,var (%frame-ref ,i ,type)))
                                  ,(lp lp-vars)))))
                 exp))
              (()
               `(begin
                  ,(take-snapshot! *ip-key-set-loop-info!*
                                   0
                                   *initial-locals*
                                   local-indices
                                   args)
                  (loop ,@args)))))))

    (define (make-scm escape traces)
      (cond
       (root-trace?
        ;; `make-entry-body' uses updated information in `expecting-types',
        ;; invoke `convert' before `make-entry-body'.
        (let ((loop (convert escape traces)))
          `(letrec ((entry (lambda ()
                             ,(make-entry-body)))
                    (loop (lambda ,args
                            ,loop)))
             entry)))
       (else
        `(letrec ((patch (lambda ,*args-from-parent*
                           (begin
                             ,(take-snapshot! *initial-ip*
                                              0
                                              *initial-locals*
                                              *local-indices-of-args*
                                              *args-from-parent*)
                             ,(add-initial-loads (convert escape traces))))))
           patch))))

    (define (get-lowest-offset)
      (let lp ((lowers (past-frame-lowers past-frame))
               (lowest 0))
        (match lowers
          (((n . _) . lowers)
           (if (< n lowest)
               (lp lowers n)
               (lp lowers lowest)))
          (()
           lowest))))

    (let ((scm (call-with-escape-continuation
                (lambda (escape)
                  (make-scm escape ops)))))
      ;; Adding initial snapshot for root trace with key=0, to hold bytevector
      ;; of native code later.
      (when root-trace?
        (hashq-set! snapshots 0 (make-snapshot 0 *initial-nlocals* '())))
      (debug 3 ";;; snapshot:~%~{;;;   ~a~%~}"
             (sort (hash-fold acons '() snapshots)
                   (lambda (a b) (< (car a) (car b)))))

      (values *local-indices-of-args* snapshots (get-lowest-offset) scm))))

(define (scm->cps scm)
  (define ignored-passes
    '(#:prune-top-level-scopes? #f
      #:specialize-primcalls? #f
      #:type-fold? #f
      #:inline-constructors? #f))
  (define (body-fun cps)
    (define (go cps k)
      (match (intmap-ref cps k)
        (($ $kfun _ _ _ _ next)
         (go (intmap-remove cps k) next))
        (($ $kclause _ next _)
         (go (intmap-remove cps k) next))
        (($ $kargs _ _ ($ $continue next _ expr))
         (go (intmap-remove cps k) next))
        (($ $ktail)
         (values (+ k 1) (intmap-remove cps k)))
        (_
         (error "body-fun: got ~a" (intmap-ref cps k)))))
    (call-with-values
        (lambda ()
          (set! cps (optimize-higher-order-cps cps ignored-passes))
          (set! cps (convert-closures cps))
          (set! cps (optimize-first-order-cps cps))
          (set! cps (renumber cps))
          (go cps 0))
      (lambda (k cps)
        (set! cps (renumber cps k))
        cps)))
  (let ((cps (and scm (compile scm #:from 'scheme #:to 'cps))))
    (set! cps (and cps (body-fun cps)))
    cps))

(define (trace->cps fragment exit-id loop? trace)
  (define (dump-cps conts)
    (and conts
         (intmap-fold (lambda (k v out)
                        (debug 3 "~4,,,'0@a:  ~a~%" k (unparse-cps v))
                        out)
                      conts
                      conts)))
  (call-with-values (lambda () (trace->scm fragment exit-id loop? trace))
    (lambda (locals snapshots lowest-offset scm)
      ;; (debug 3 ";;; scm:~%~y" scm)
      (let ((cps (scm->cps scm)))
        ;; (dump-cps cps)
        (values locals snapshots lowest-offset scm cps)))))
