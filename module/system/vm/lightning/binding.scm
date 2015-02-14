;;; -*- mode: scheme; coding: utf-8; -*-

;;;; Copyright (C) 2014, 2015  Free Software Foundation, Inc.
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

;;; FFI binding of lightning for guile.

(define-module (system vm lightning binding)
  #:use-module (ice-9 regex)
  #:use-module (system foreign)
  #:export
  (
   ;;
   ;; Prototypes
   ;;

   init-jit
   finish-jit

   jit-state jit-new-state jit-clear-state jit-destroy-state
   jit-address jit-name jit-note
   jit-label jit-forward jit-indirect jit-link
   jit-forward-p jit-indirect-p jit-target-p

   jit-prolog

   jit-allocai jit-ellipsis

   jit-arg
   jit-getarg jit-getarg-c jit-getarg-uc jit-getarg-s jit-getarg-us
   jit-getarg-i jit-getarg-ui jit-getarg-l

   jit-prepare jit-pushargr jit-pushargi jit-finishr jit-finishi
   jit-ret jit-retr jit-reti
   jit-retval jit-retval-c jit-retval-uc jit-retval-s jit-retval-us
   jit-retval-i jit-retval-ui jit-retval-l ;; x86-64
   jit-epilog

   jit-patch jit-patch-at jit-patch-abs jit-realize
   jit-get-code jit-set-code jit-get-data jit-set-data
   jit-frame jit-tramp jit-emit

   jit-print

   jit-arg-f jit-getarg-f jit-pushargr-f jit-pushargi-f
   jit-retr-f jit-reti-f jit-retval-f

   jit-arg-d jit-getarg-d jit-pushargr-d jit-pushargi-d
   jit-retr-d jit-reti-d jit-retval-d

   jit-new-node jit-new-node-w jit-new-node-p
   jit-new-node-ww jit-new-node-wp jit-new-node-pw
   jit-new-node-wf jit-new-node-wd
   jit-new-node-www jit-new-node-qww
   jit-new-node-wwf jit-new-node-wwd
   jit-new-node-pww jit-new-node-pwf jit-new-node-pwd

   jit-callee-save-p jit-pointer-p

   jit-get-note

   jit-disassemble

   %jit-set-memory-functions %jit-get-memory-functions


   ;;
   ;; jit_code_t
   ;;

   ;; Ops with general purporse registers

   jit-live jit-align

   jit-addr jit-addi jit-addcr jit-addci jit-addxr jit-addxi
   jit-subr jit-subi jit-subcr jit-subci jit-subxr jit-subxi
   jit-rsbr jit-rsbi
   jit-mulr jit-muli jit-qmulr jit-qmuli jit-qmulr-u jit-qmuli-u
   jit-divr jit-divi jit-divr-u jit-divi-u
   jit-qdivr jit-qdivi jit-qdivr-u jit-qdivi-u
   jit-remr jit-remi jit-remr-u jit-remi-u

   jit-andr jit-andi jit-orr jit-ori jit-xorr jit-xori

   jit-lshr jit-lshi jit-rshr jit-rshi jit-rshr-u jit-rshi-u

   jit-negr jit-comr

   jit-ltr jit-lti jit-ltr-u jit-lti-u
   jit-ler jit-lei jit-ler-u jit-lei-u
   jit-eqr jit-eqi
   jit-ger jit-gei jit-ger-u jit-gei-u
   jit-gtr jit-gti jit-gtr-u jit-gti-u
   jit-ner jit-nei

   jit-movr jit-movi
   jit-extr-c jit-extr-uc jit-extr-s jit-extr-us
   jit-extr-i jit-extr-ui ;; x86-64

   jit-htonr-us jit-ntohr-us jit-ntohr-ui jit-ntohr-ui
   ;; jit-htonr jit-ntohr ; x86-64

   jit-ldr jit-ldi
   jit-ldr-c jit-ldi-c jit-ldr-uc jit-ldi-uc
   jit-ldr-s jit-ldi-s jit-ldr-us jit-ldi-us
   jit-ldr-i jit-ldi-i
   jit-ldr jit-ldr-i
   jit-ldr-ui jit-ldi-ui jit-ldr-l jit-ldi-l ;; x86-64

   jit-ldxr-c jit-ldxi-c jit-ldxr-uc jit-ldxi-uc
   jit-ldxr-s jit-ldxi-s jit-ldxr-us jit-ldxi-us
   jit-ldxr-i jit-ldxi-i
   jit-ldxr jit-ldxi
   jit-ldxr-ui jit-ldxi-ui jit-ldxr-l jit-ldxi-l ;; x86-64

   jit-str jit-sti
   jit-str-c jit-sti-c jit-str-s jit-sti-s
   jit-str-l jit-sti-l ;; x86-64

   jit-stxr jit-stxi
   jit-stxr-c jit-stxi-c jit-stxr-s jit-stxi-s jit-stxr-i jit-stxi-i
   jit-stxr-l jit-stxi-l ;; x86-64

   jit-bltr jit-blti jit-bltr-u jit-blti-u
   jit-bler jit-blei jit-bler-u jit-blei-u
   jit-beqr jit-beqi
   jit-bger jit-bgei jit-bger-u jit-bgei-u
   jit-bgtr jit-bgti jit-bgtr-u jit-bgti-u
   jit-bner jit-bnei

   jit-bmsr jit-bmsi jit-bmcr jit-bmci

   jit-boaddr jit-boaddi jit-boaddr-u jit-boaddi-u
   jit-bxaddr jit-bxaddi jit-bxaddr-u jit-bxaddr-u
   jit-bosubr jit-bosubi jit-bosubr-u jit-bosubi-u
   jit-bxsubr jit-bxsubi jit-bxsubr-u jit-bxsubi-u

   jit-jmpr jit-jmpi jit-callr jit-calli

   ;; Ops with float

   jit-addr-f jit-addi-f jit-subr-f jit-subi-f
   jit-rsbr-f jit-rsbi-f jit-mulr-f jit-muli-f
   jit-divr-f jit-divi-f
   jit-negr-f jit-absr-f jit-sqrt-f

   jit-ltr-f jit-lti-f jit-ler-f jit-lei-f
   jit-eqr-f jit-eqi-f jit-ger-f jit-gei-f
   jit-gtr-f jit-gti-f jit-ner-f jit-nei-f
   jit-unltr-f jit-unlti-f jit-unler-f jit-unlei-f
   jit-uneqr-f jit-uneqi-f jit-unger-f jit-ungei-f
   jit-ungtr-f jit-ungti-f jit-ltgtr-f jit-ltgti-f
   jit-ordr-f jit-ordi-f jit-unordr-f jit-unordi-f

   jit-truncr-f-i jit-truncr-f-l jit-truncr-f ;; x86-64
   jit-extr-f jit-extr-d-f jit-movr-f jit-movi-f

   jit-ldr-f jit-ldi-f jit-ldxr-f jit-ldxi-f
   jit-str-f jit-sti-f jit-stxr-f jit-stxi-f

   jit-bltr-f jit-blti-f jit-bler-f jit-blei-f
   jit-beqr-f jit-beqi-f jit-bger-f jit-bgei-f
   jit-bgtr-f jit-bgti-f jit-bner-f jit-bnei-f
   jit-bunltr-f jit-bunlti-f jit-bunler-f jit-bunlei-f
   jit-buneqr-f jit-buneqi-f jit-bunger-f jit-bungei-f
   jit-bungtr-f jit-bungti-f jit-bltgtr-f jit-bltgti-f
   jit-bordr-f it-bordi-f jit-bunordr-f jit-bunordi-f

   ;; Ops with double

   jit-addr-d jit-addi-d jit-subr-d jit-subi-d
   jit-rsbr-d jit-rsbi-d jit-mulr-d jit-muli-d
   jit-divr-d jit-divi-d
   jit-negr-d jit-absr-d jit-sqrtr-d

   jit-ltr-d jit-lti-d jit-ler-d jit-lei-d
   jit-eqr-d jit-eqi-d jit-ger-d jit-gei-d
   jit-gtr-d jit-gti-d jit-ner-d jit-nei-d
   jit-unltr-d jit-unlti-d jit-unler-d jit-unlei-d
   jit-uneqr-d jit-uneqi-d jit-unger-d jit-ungei-d
   jit-ungtr-d jit-ungti-d jit-ltgtr-d jit-ltgti-d
   jit-ordr-d jit-ordi-d jit-unordr-d jit-unordi-d

   jit-truncr-d-i jit-truncr-d-l ;; x86
   jit-extr-d jit-extr-f-d jit-movr-d jit-movi-d

   jit-ldr-d jit-ldr-i jit-ldxr-d jit-ldxi-d
   jit-str-d jit-sti-d jit-stxr-d jit-stxi-d

   jit-bltr-d jit-blti-d jit-bler-d jit-blei-d
   jit-beqr-d jit-beqi-d jit-bger-d jit-bgei-d
   jit-bgtr-d jit-bgti-d jit-bner-d jit-bnei-d
   jit-bunltr-d jit-bunlti-d jit-bunler-d jit-bunlei-d
   jit-buneqr-d jit-buneqi-d jit-bunger-d jit-bungei-d
   jit-bungtr-d jit-bungti-d jit-bltgtr-d jit-bltgti-d
   jit-bordr-d jit-bordi-d jit-bunordr-d jit-bunordi-d

   jit-movr-w-f jit-movr-ww-d jit-movr-w-d
   jit-movr-f-w jit-movi-f-w jit-movr-d-ww jit-movi-d-ww
   jit-movr-d-w jit-movi-d-w

   ;;
   ;; Registers
   ;;

   r0 r1 r2 r3 v0 v1 v2 v3 f0 f1 f2 f3 f4 f5 f6 f7 jit-fp
   jit-r jit-v jit-f jit-r-num jit-v-num jit-f-num
   imm null

   jit-code-size
   make-bytevector-executable))


;;;
;;; Auxiliary
;;;

(eval-when (compile load expand)

  (define lightning
    (dynamic-link "liblightning"))

  (define jit-state
    (make-parameter %null-pointer))

  (define (header-name name private?)
    (let ((str (symbol->string (syntax->datum name))))
      (string-append
       (if private? "_" "")
       (regexp-substitute/global #f "-" str 'pre "_" 'post))))

  (define (scm-c-name name)
    (string->symbol
     (string-append "%" (symbol->string (syntax->datum name))))))


;;;
;;; Syntax
;;;

(define-syntax define-lightning
  (syntax-rules ()
    ((_ ret scheme-name c-name args)
     (define scheme-name
       (pointer->procedure ret (dynamic-func c-name lightning) args)))))

(define-syntax define-lightning/no-arg
  (lambda (x)
    (syntax-case x ()
      ((_ ret name)
       (let ((h-name (datum->syntax x (header-name #'name #f))))
         #`(define-lightning ret name #,h-name '()))))))

(define-syntax define-lightning/state
  (lambda (x)
    (define (arg-ptr args)
      (cons '* (map (lambda (_) '*) (syntax->datum args))))
    (syntax-case x ()
      ((_ ret name . args)
       (let ((h-name (datum->syntax x (header-name #'name #t)))
             (c-name (datum->syntax x (scm-c-name #'name)))
             (args* (datum->syntax x (arg-ptr #'args))))
         #`(begin
             (define-lightning ret #,c-name #,h-name '#,args*)
             (define (name . args)
               (#,c-name (jit-state) . args))))))))

(define-syntax begin-define-lightning/state
  (syntax-rules ()
    ((_ formal ...)
     (begin
       (define-lightning/state . formal) ...))))


;;;
;;; Prototypes
;;;

(define-lightning void %init-jit "init_jit" '(*))

(define-lightning/no-arg void finish-jit)

(define-lightning/no-arg '* jit-new-state)

(begin-define-lightning/state
 (void jit-clear-state)
 (void jit-destroy-state)

 ('*   jit-address node)

 ;; "jit-name" and "jit-note" are defined below.
 ;; ('*   jit-name char)
 ;; ('*   jit-note char int)

 ('*   jit-label)
 ('*   jit-forward)
 ('*   jit-indirect)
 (void jit-link u)
 ('*   jit-forward-p u)
 ('*   jit-indirect-p u)
 ('*   jit-target-p u)

 (void jit-prolog)

 ('*   jit-allocai u)
 (void jit-ellipsis)

 ('*   jit-arg)
 (void jit-getarg-c u v)
 (void jit-getarg-uc u v)
 (void jit-getarg-s u v)
 (void jit-getarg-us u v)
 (void jit-getarg-i u v)
 ;; XXX: #if __WORDSIZE == 64
 (void jit-getarg-ui u v)
 (void jit-getarg-l u v)
 ;; XXX: #endif

 (void jit-prepare)
 (void jit-pushargr u)
 (void jit-pushargi u)
 (void jit-pushargi u)
 (void jit-finishr u)
 ('*   jit-finishi u)
 (void jit-ret)
 (void jit-retr u)
 (void jit-reti u)
 (void jit-retval-c u)
 (void jit-retval-uc u)
 (void jit-retval-s u)
 (void jit-retval-us u)
 (void jit-retval-i u)
 ;; XXX: #if __WORDSIZE == 64
 (void jit-retval-ui u)
 (void jit-retval-l u)
 ;; XXX: #endif
 (void jit-epilog)

 (void jit-patch u)
 (void jit-patch-at u v)
 (void jit-patch-abs u v)
 (void jit-realize)
 ('*   jit-get-code u)
 (void jit-set-code u v)
 ('*   jit-get-data u v)
 (void jit-set-data u v w)
 (void jit-frame u)
 (void jit-tramp u)
 ('*   jit-emit)

 (void jit-print)

 ('*   jit-arg-f)
 (void jit-getarg-f u v)
 (void jit-pushargr-f u)
 (void jit-pushargi-f u)
 (void jit-retr-f u)
 (void jit-reti-f u)
 (void jit-retval-f u)

 ('*   jit-arg-d)
 (void jit-getarg-d u v)
 (void jit-pushargr-d u)
 (void jit-pushargi-d u)
 (void jit-retr-d u)
 (void jit-reti-d u)
 (void jit-retval-d u)

 ('*   jit-new-node c)
 ('*   jit-new-node-w c u)
 ('*   jit-new-node-p c u)
 ('*   jit-new-node-ww c u v)
 ('*   jit-new-node-wp c u v)
 ('*   jit-new-node-pw c u v)
 ('*   jit-new-node-wf c u f)
 ('*   jit-new-node-wd c u f)
 ('*   jit-new-node-www c u v w)
 ('*   jit-new-node-qww c l h v w)
 ('*   jit-new-node-wwf c u v w)
 ('*   jit-new-node-wwd c u v w)
 ('*   jit-new-node-pww c u v w)
 ('*   jit-new-node-pwf c u v w)
 ('*   jit-new-node-pwd c u v w)

 ('*   jit-callee-save-p u)
 ('*   jit-pointer-p u)
 ('*   jit-get-note n u v w)
 (void jit-disassemble))

(define-lightning
  void %jit-set-memory-functions "jit_set_memory_functions" '(* * *))

(define-lightning
  void %jit-get-memory-functions "jit_get_memory_functions" '(* * *))

;;; Redefining procedures taking C strings

(define (init-jit arg)
  (%init-jit (string->pointer arg)))

(define-lightning/state '* jit-name char)
(define (jit-name name)
  (%jit-name (jit-state) (string->pointer name)))

(define-lightning/state '* jit-note char int)
(define (jit-note name int)
  (%jit-note (jit-state) (string->pointer name) (make-pointer int)))

(define (jit-code-size)
  (%jit-code-size (jit-state)))


;;;
;;; Registers
;;;

;; Interfaces for C wrapper code containing functions for C macros: JIT_R(),
;; JIT_V(), and JIT_F().

(define (jit-r i) (make-pointer (%jit-r i)))
(define (jit-v i) (make-pointer (%jit-v i)))
(define (jit-f i) (make-pointer (%jit-f i)))

(define r0 (jit-r 0))
(define r1 (jit-r 1))
(define r2 (jit-r 2))
(define r3 (jit-r 3))

(define v0 (jit-v 0))
(define v1 (jit-v 1))
(define v2 (jit-v 2))
(define v3 (jit-v 3))

(define f0 (jit-f 0))
(define f1 (jit-f 1))
(define f2 (jit-f 2))
(define f3 (jit-f 3))
(define f4 (jit-f 4))
(define f5 (jit-f 5))
(define f6 (jit-f 6))
(define f7 (jit-f 7))

(define (jit-fp)
  (make-pointer (%jit-fp)))

;; Aliases for immediate values

(define imm make-pointer)
(define null %null-pointer)


;;;
;;; jit_code_t and C macros
;;;

(include "jit-code-t.scm")
