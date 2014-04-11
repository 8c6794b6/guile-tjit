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
;;; A pass that prunes successors of expressions that bail out.
;;;
;;; Code:

(define-module (language cps prune-bailouts)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:export (prune-bailouts))

(define (module-box src module name public? bound? val-proc)
  (let-fresh (kbox) (module-sym name-sym public?-sym bound?-sym box)
    (build-cps-term
      ($letconst (('module module-sym module)
                  ('name name-sym name)
                  ('public? public?-sym public?)
                  ('bound? bound?-sym bound?))
        ($letk ((kbox ($kargs ('box) (box) ,(val-proc box))))
          ($continue kbox src
            ($primcall 'cached-module-box
                       (module-sym name-sym public?-sym bound?-sym))))))))

(define (primitive-ref name k src)
  (module-box #f '(guile) name #f #t
              (lambda (box)
                (build-cps-term
                  ($continue k src ($primcall 'box-ref (box)))))))

(define (prune-bailouts* fun)
  (define (visit-cont cont ktail)
    (rewrite-cps-cont cont
      (($ $cont label ($ $kargs names vars body))
       (label ($kargs names vars ,(visit-term body ktail))))
      (($ $cont label ($ $kfun src meta self tail clause))
       (label ($kfun src meta self ,tail
                ,(and clause (visit-cont clause ktail)))))
      (($ $cont label ($ $kclause arity body alternate))
       (label ($kclause ,arity ,(visit-cont body ktail)
                        ,(and alternate (visit-cont alternate ktail)))))
      (_ ,cont)))

  (define (visit-term term ktail)
    (rewrite-cps-term term
      (($ $letrec names vars funs body)
       ($letrec names vars (map visit-fun funs)
                ,(visit-term body ktail)))
      (($ $letk conts body)
       ($letk ,(map (lambda (cont) (visit-cont cont ktail)) conts)
         ,(visit-term body ktail)))
      (($ $continue k src exp)
       ,(visit-exp k src exp ktail))))

  (define (visit-exp k src exp ktail)
    (rewrite-cps-term exp
      (($ $fun) ($continue k src ,(visit-fun exp)))
      (($ $primcall (and name (or 'error 'scm-error 'throw)) args)
       ,(if (eq? k ktail)
            (build-cps-term ($continue k src ,exp))
            (let-fresh (kprim kresult kreceive) (prim rest)
              (build-cps-term
                ($letk ((kresult ($kargs ('rest) (rest)
                                   ($continue ktail src ($values ()))))
                        (kreceive ($kreceive '() 'rest kresult))
                        (kprim ($kargs ('prim) (prim)
                                 ($continue kreceive src
                                   ($call prim args)))))
                  ,(primitive-ref name kprim src))))))
      (_ ($continue k src ,exp))))

  (define (visit-fun fun)
    (rewrite-cps-exp fun
      (($ $fun free body)
       ($fun free ,(prune-bailouts* body)))))

  (rewrite-cps-cont fun
    (($ $cont kfun
        ($ $kfun src meta self ($ $cont ktail ($ $ktail)) clause))
     (kfun ($kfun src meta self (ktail ($ktail))
             ,(and clause (visit-cont clause ktail)))))))

(define (prune-bailouts fun)
  (with-fresh-name-state fun
    (prune-bailouts* fun)))
