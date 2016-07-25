;;;; ANF IR for call and return

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
;;; Module containing ANF IR definitions for call and return operations.
;;;
;;; Code:

(define-module (language trace ir-call)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (system vm native debug)
  #:use-module (language trace error)
  #:use-module (language trace env)
  #:use-module (language trace fragment)
  #:use-module (language trace ir)
  #:use-module (language trace primitives)
  #:use-module (language trace snapshot)
  #:use-module (language trace types)
  #:use-module (language trace variables)
  #:use-module (system vm program))



(define-syntax scm-f-program-is-boot
  (identifier-syntax #x100))

(define-syntax scm-f-program-is-primitive
  (identifier-syntax #x200))

(define-syntax scm-f-program-is-primitive-generic
  (identifier-syntax #x400))

(define-syntax scm-f-program-is-continuation
  (identifier-syntax #x800))

(define-syntax scm-f-program-is-partial-continuation
  (identifier-syntax #x1000))

(define-syntax scm-f-program-is-foreign
  (identifier-syntax #x2000))

(define-syntax scm-f-program-is-bytecode-mask
  (identifier-syntax #xff00))

(define (program-flag p)
  (pointer-address (dereference-pointer (scm->pointer p))))

(define (bytecode-program? pflag)
  (zero? (logand pflag scm-f-program-is-bytecode-mask)))

(define (primitive-program? pflag)
  (not (zero? (logand pflag scm-f-program-is-primitive))))

(define (continuation-program? pflag)
  (not (zero? (logand pflag scm-f-program-is-continuation))))

(define-syntax-rule (scan-call proc nlocals label?)
  (let* ((stack-size (vector-length locals))
         (sp-offset (env-sp-offset env))
         (sp-proc (- stack-size proc 1))
         (proc+offset (+ sp-proc sp-offset))
         (ra+offset (+ proc+offset 1))
         (dl+offset (+ ra+offset 1)))

    ;; Using `&any' or `&procedure' type to fill in locals for procedure, `&scm'
    ;; for return address and dynamic link, to suppress type guards.  Specifying
    ;; each types in sequence, not using batch update procedure
    ;; `set-entry-types!'.
    ;;
    ;; XXX: Batch update of proc, ra, and dle made some recursive codes to not
    ;; work with dump option.
    ;;
    (set-entry-type! env proc+offset (if label? &any &procedure))
    (set-entry-type! env ra+offset &scm)
    (set-entry-type! env dl+offset &scm)

    ;; Filling in arguments as `&scm' type. Also resolving copy entry type when
    ;; the copy source index in inferred type matched with one of the arguments.
    (do ((n 1 (+ n 1)))
        ((<= nlocals n))
      (let ((i (- proc+offset n)))
        (match (assq-ref (env-inferred-types env) i)
          (('copy . dst)
           (set-entry-type! env dst &scm))
          (_
           (values)))
        (set-entry-type! env i &scm)))

    (set-scan-initial-fields! env)
    (add-env-call! env)
    (push-scan-fp-offset! env proc)
    (push-scan-sp-offset! env (- (+ proc nlocals) stack-size))))

(define-syntax-rule (ti-call proc nlocals label?)
  (let* ((stack-size (vector-length locals))
         (sp-offset (current-sp-for-ti))
         (fp (- stack-size proc))
         (sp-proc (- stack-size proc 1))
         (ra/t (make-return-address (+ ip (* 4 (if label? 3 2)))))
         (dl/t (make-dynamic-link proc)))
    (set-inferred-type! env (+ sp-offset fp) ra/t)
    (set-inferred-type! env (+ sp-offset fp 1) dl/t)
    (do ((n 0 (+ n 1)))
        ((<= nlocals n))
      (match (assq-ref (env-inferred-types env) n)
        (('copy . dst)
         (set-inferred-type! env n &scm))
        (_
         (values))))))

(define-syntax-rule (check-entry-ip next-ip message)
  (when (ir-last-op? ir)
    (let ((entry-ip (or (and=> (env-linked-fragment env) fragment-entry-ip)
                        (env-entry-ip env))))
      (when (not (= entry-ip next-ip))
        (break 1 "last ~s to non-entry IP, entry=~x linked=~x next=~x" message
               (env-entry-ip env)
               (or (and=> (env-linked-fragment env) fragment-entry-ip) 0)
               next-ip)))))

(define-syntax-rule (with-callee-guard proc flag var thunk)
  ;; Add guard to test the procedure value, to bailout when callee has been
  ;; changed. Bytecode programs, primitive programs, foreign programs, ... etc
  ;; uses different constant reference value recorded at compile time. The guard
  ;; first check the type of callee program, and then fetch the runtime value
  ;; according to the type, and compare with the value used at compile time.
  ;;
  (let ((r2 (make-tmpvar 2)))
    (cond
     ((not (program? proc))
      (nyi "with-callee-guard: ~a" proc))
     ((bytecode-program? flag)
      `(let ((,r2 (,%cref ,var 0)))
         (let ((,r2 (,%band ,r2 ,scm-f-program-is-bytecode-mask)))
           (let ((_ (,%eq ,r2 0)))
             (let ((,r2 (,%cref ,var 1)))
               (let ((_ (,%eq ,r2 ,(program-code proc))))
                 ,(thunk)))))))
     ((primitive-program? flag)
      (let ((free-ref0 (program-free-variable-ref proc 0)))
        `(let ((,r2 (,%cref ,var 0)))
           (let ((,r2 (,%band ,r2 ,scm-f-program-is-primitive)))
             (let ((_ (,%ne ,r2 0)))
               (let ((,r2 (,%cref ,var ,(+ 2 0))))
                 (let ((,r2 (,%cref ,r2 1)))
                   (let ((_ (,%eq ,r2 ,(pointer-address free-ref0))))
                     ,(thunk)))))))))
     ((continuation-program? flag)
      (let ((code (program-code proc)))
        `(let ((,r2 (,%cref ,var 0)))
           (let ((,r2 (,%band ,r2 ,scm-f-program-is-continuation)))
             (let ((_ (,%ne ,r2 0)))
               (let ((,r2 (,%cref ,var 1)))
                 (let ((_ (,%eq ,r2 ,code)))
                   ,(thunk))))))))
     (else
      (nyi "with-callee-guard: ~a" proc)))))

;;; XXX: halt is not defined, might not necessary.

(define-scan (call proc nlocals)
  (scan-call proc nlocals #f))

(define-ti (call proc nlocals)
  (ti-call proc nlocals #f))

(define-anf (call proc nlocals)
  (let* ((sp-offset (current-sp-offset))
         (stack-size (vector-length locals))
         (fp (- stack-size proc))
         (r2 (make-tmpvar 2))
         (proc/i (- fp 1))
         (proc/v (src-ref proc/i))
         (proc/l (scm-ref proc/i))
         (proc/f (program-flag proc/l))
         (emit-next (lambda ()
                      (if (inline-current-call?)
                          (next)
                          `(let ((_ (,%scall ,proc)))
                             ,(next))))))
    (check-entry-ip (program-code proc/l) 'call)
    `(let ((_ ,(take-snapshot! ip 0)))
       ,(with-type-guard &procedure proc/i
          (with-callee-guard proc/l proc/f proc/v emit-next)))))

(define-scan (call-label proc nlocals label)
  (scan-call proc nlocals #t))

(define-ti (call-label proc nlocals label)
  (ti-call proc nlocals #t))

(define-anf (call-label proc nlocals label)
  (if (inline-current-call?)
      (next)
      `(let ((_ (,%scall ,proc)))
         ,(next))))

(define-syntax-rule (scan-tail-call nlocals label?)
  (let* ((stack-size (vector-length locals))
         (proc-sp (- stack-size 1))
         (proc-sp+offset (+ proc-sp (env-sp-offset env))))
    (unless label?
      (set-entry-type! env proc-sp+offset &procedure))
    (set-scan-initial-fields! env)
    (push-scan-sp-offset! env (- nlocals stack-size))))

(define-syntax-rule (ti-tail-call label?)
  (let* ((stack-size (vector-length locals))
         (proc/i (+ (- stack-size 1) (current-sp-for-ti))))
    (unless label?
      (set-inferred-type! env proc/i &procedure))))

(define-scan (tail-call nlocals)
  (scan-tail-call nlocals #f))

(define-ti (tail-call nlocals)
  (ti-tail-call #f))

(define-anf (tail-call nlocals)
  (let* ((stack-size (vector-length locals))
         (proc-index (- stack-size 1))
         (proc/v (src-ref proc-index))
         (proc/l (scm-ref proc-index))
         (proc-addr (object-address proc/l))
         (proc/f (program-flag proc/l))
         (r2 (make-tmpvar 2)))
    (check-entry-ip (program-code proc/l) 'tail-call)
    `(let ((_ ,(take-snapshot! ip 0)))
       ,(with-callee-guard proc/l proc/f proc/v next))))

(define-scan (tail-call-label nlocals label)
  (scan-tail-call nlocals #t))

(define-ti (tail-call-label nlocals label)
  (ti-tail-call #t))

(define-anf (tail-call-label nlocals label)
  (next))

(define-scan (receive dst proc nlocals)
  (let* ((stack-size (vector-length locals))
         (sp-offset (env-sp-offset env))
         (fp (- stack-size proc)))
    (set-entry-type! env (+ (- stack-size proc 2) sp-offset) &scm)
    (let ((new-offsets (cons (env-sp-offset env) (env-sp-offsets env))))
      (set-env-sp-offsets! env new-offsets))
    (pop-scan-sp-offset! env (- stack-size nlocals))
    (let ((new-fp-offsets (cons (env-fp-offset env) (env-fp-offsets env))))
      (set-env-fp-offsets! env new-fp-offsets))))

(define-ti (receive dst proc nlocals)
  (let* ((stack-size (vector-length locals))
         (sp-offset (current-sp-for-ti))
         (proc/i (+ (- stack-size proc 2) sp-offset))
         (dst/i (+ (- stack-size dst 1) sp-offset))
         (ty (or (assq-ref (env-inferred-types env) proc/i)
                 `(copy . ,proc/i))))
    (set-inferred-type! env dst/i ty)))

(define-anf (receive dst proc nlocals)
  (let* ((stack-size (vector-length locals))
         (sp-offset (current-sp-offset))
         (src/i (- (- stack-size proc) 2))
         (src/v (src-ref src/i))
         (dst/i (- stack-size dst 1))
         (dst/v (dst-ref dst/i))
         (live-indices (env-live-indices env))
         (dst/i+sp-offset (+ dst/i sp-offset))
         (test (lambda (v) (eq? v dst/v))))
    ;; Update live indices before generating load thunk.
    (unless (memq dst/i+sp-offset live-indices)
      (set-env-live-indices! env (cons dst/i+sp-offset live-indices)))
    `(let ((,dst/v ,src/v))
       ,(let ((thunk (gen-load-thunk proc nlocals test)))
          (thunk)))))

(define-scan (receive-values proc allow-extra? nvalues)
  (if (<= nvalues 1)
      (set-scan-initial-fields! env)
      (begin
        (debug 1 "NYI: receive-values ~a ~a ~a~%" proc allow-extra? nvalues)
        #f)))

(define-ti (receive-values proc allow-extra? nvalues)
  ;; XXX: Should infer multiple values.
  (values))

(define-anf (receive-values proc allow-extra? nvalues)
  (let* ((nlocals (vector-length locals))
         (thunk (gen-load-thunk proc nlocals (const #f))))
    (thunk)))

;; XXX: tail-call/shuffle

(define-scan (return-values nlocals)
  (let ((stack-size (vector-length locals)))
    (set-scan-initial-fields! env)
    (add-env-return! env)
    (pop-scan-sp-offset! env (- stack-size nlocals))
    (pop-scan-fp-offset! env dl)))

(define-ti (return-values nlocals)
  ;; VM interpreter refills return address and dynamic link with false, doing
  ;; the same.
  (let* ((stack-size (vector-length locals))
         (sp-offset (current-sp-for-ti))
         (ra-offset (+ sp-offset stack-size))
         (dl-offset (+ ra-offset 1)))
    (when (or (not (env-uprec? env))
              (env-parent-fragment env)
              (inline-current-return?))
      (set-inferred-type! env ra-offset &false)
      (set-inferred-type! env dl-offset &false))))

(define-anf (return-values nlocals)
  ;; Add guard to test return address. Two locals below callee procedure in VM
  ;; frame contain dynamic link and return address.
  (let ((stack-size (vector-length locals))
        (maybe-add-indices (lambda (i indices)
                             (if (memq i indices)
                                 indices
                                 (cons i indices)))))
    (if (inline-current-return?)
        (next)
        (let* ((ra/i (+ stack-size (current-sp-offset)))
               (dl/i (+ ra/i 1))
               (live-indices (env-live-indices env))
               (live-indices (maybe-add-indices ra/i live-indices))
               (live-indices (maybe-add-indices dl/i live-indices)))
          (set-env-live-indices! env live-indices)
          `(let ((_ ,(take-snapshot! ip 0)))
             (let ((_ (,%return ,dl ,ra)))
               (let ((_ ,(take-snapshot! ra 0 #t)))
                 ,(next))))))))
