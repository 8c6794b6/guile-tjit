;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2014 Free Software Foundation, Inc.

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
;;; A pass to renumber variables and continuation labels so that they
;;; are contiguous within each function and, in the case of labels,
;;; topologically sorted.
;;;
;;; Code:

(define-module (language cps renumber)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (language cps)
  #:export (renumber))

;; Topologically sort the continuation tree starting at k0, using
;; reverse post-order numbering.
(define (sort-conts k0 conts new-k0)
  (define (for-each-successor f cont)
    (visit-cont-successors
     (case-lambda
       (() #t)
       ((succ0) (f succ0))
       ((succ0 succ1)
        ;; Visit higher-numbered successors first, so that if they are
        ;; unordered, their original order is preserved.
        (cond
         ((< succ0 succ1) (f succ1) (f succ0))
         (else (f succ0) (f succ1)))))
     cont))

  (let ((next -1))
    (let visit ((k k0))
      (let ((cont (vector-ref conts k)))
        ;; Clear the cont table entry to mark this continuation as
        ;; visited.
        (vector-set! conts k #f)
        (for-each-successor (lambda (k)
                              (let ((entry (vector-ref conts k)))
                                ;; Visit the successor if it has not been
                                ;; visited yet.
                                (when (and entry (not (exact-integer? entry)))
                                  (visit k))))
                            cont)
        ;; Chain this label to the label that will follow it in the sort
        ;; order, and record this label as the new head of the order.
        (vector-set! conts k next)
        (set! next k)))

    ;; Finally traverse the label chain, giving each label its final
    ;; name.
    (let lp ((n new-k0) (head next))
      (if (< head 0)
          n
          (let ((next (vector-ref conts head)))
            (vector-set! conts head n)
            (lp (1+ n) next))))))

(define (compute-new-labels-and-vars fun)
  (call-with-values (lambda () (compute-max-label-and-var fun))
    (lambda (max-label max-var)
      (let ((labels (make-vector (1+ max-label) #f))
            (next-label 0)
            (vars (make-vector (1+ max-var) #f))
            (next-var 0))
        (define (rename! var)
          (vector-set! vars var next-var)
          (set! next-var (1+ next-var)))

        (define (collect-conts fun)
          (define (visit-cont cont)
            (match cont
              (($ $cont label cont)
               (vector-set! labels label cont)
               (match cont
                 (($ $kargs names vars body)
                  (visit-term body))
                 (($ $kfun src meta self tail clause)
                  (visit-cont tail)
                  (when clause
                    (visit-cont clause)))
                 (($ $kclause arity body alternate)
                  (visit-cont body)
                  (when alternate
                    (visit-cont alternate)))
                 ((or ($ $ktail) ($ $kreceive) ($ $kif))
                  #f)))))
          (define (visit-term term)
            (match term
              (($ $letk conts body)
               (for-each visit-cont conts)
               (visit-term body))
              (($ $letrec names syms funs body)
               (visit-term body))
              (($ $continue k src _) #f)))
          (match fun
            (($ $fun free body)
             (visit-cont body))))

        (define (compute-names-in-fun fun)
          (define queue '())
          (define (visit-cont cont)
            (match cont
              (($ $cont label cont)
               (let ((reachable? (exact-integer? (vector-ref labels label))))
                 ;; This cont is reachable if it was given a number.
                 ;; Otherwise the cont table entry still contains the
                 ;; cont itself; clear it out to indicate that the cont
                 ;; should not be residualized.
                 (unless reachable?
                   (vector-set! labels label #f))
                 (match cont
                   (($ $kargs names vars body)
                    (when reachable?
                      (for-each rename! vars))
                    (visit-term body reachable?))
                   (($ $kfun src meta self tail clause)
                    (unless reachable? (error "entry should be reachable"))
                    (rename! self)
                    (visit-cont tail)
                    (when clause
                      (visit-cont clause)))
                   (($ $kclause arity body alternate)
                    (unless reachable? (error "clause should be reachable"))
                    (visit-cont body)
                    (when alternate
                      (visit-cont alternate)))
                   (($ $ktail)
                    (unless reachable?
                      ;; It's possible for the tail to be unreachable,
                      ;; if all paths contify to infinite loops.  Make
                      ;; sure we mark as reachable.
                      (vector-set! labels label next-label)
                      (set! next-label (1+ next-label))))
                   ((or ($ $kreceive) ($ $kif))
                    #f))))))
          (define (visit-term term reachable?)
            (match term
              (($ $letk conts body)
               (for-each visit-cont conts)
               (visit-term body reachable?))
              (($ $letrec names syms funs body)
               (when reachable?
                 (for-each rename! syms)
                 (set! queue (fold cons queue funs)))
               (visit-term body reachable?))
              (($ $continue k src (and fun ($ $fun)))
               (when reachable?
                 (set! queue (cons fun queue))))
              (($ $continue) #f)))

          (collect-conts fun)
          (match fun
            (($ $fun free (and entry ($ $cont kfun)))
             (set! next-label (sort-conts kfun labels next-label))
             (visit-cont entry)
             (for-each compute-names-in-fun (reverse queue)))))

        (compute-names-in-fun fun)
        (values labels vars next-label next-var)))))

(define (renumber fun)
  (call-with-values (lambda () (compute-new-labels-and-vars fun))
    (lambda (labels vars nlabels nvars)
      (define (relabel label) (vector-ref labels label))
      (define (rename var) (vector-ref vars var))
      (define (rename-kw-arity arity)
        (match arity
          (($ $arity req opt rest kw aok?)
           (make-$arity req opt rest
                        (map (match-lambda
                              ((kw kw-name kw-var)
                               (list kw kw-name (rename kw-var))))
                             kw)
                        aok?))))
      (define (must-visit-cont cont)
        (or (visit-cont cont)
            (error "internal error -- failed to visit cont")))
      (define (visit-conts conts)
        (match conts
          (() '())
          ((cont . conts)
           (cond
            ((visit-cont cont)
             => (lambda (cont)
                  (cons cont (visit-conts conts))))
            (else (visit-conts conts))))))
      (define (visit-cont cont)
        (match cont
          (($ $cont label cont)
           (let ((label (relabel label)))
             (and
              label
              (rewrite-cps-cont cont
                (($ $kargs names vars body)
                 (label ($kargs names (map rename vars) ,(visit-term body))))
                (($ $kfun src meta self tail clause)
                 (label
                  ($kfun src meta (rename self) ,(must-visit-cont tail)
                    ,(and clause (must-visit-cont clause)))))
                (($ $ktail)
                 (label ($ktail)))
                (($ $kclause arity body alternate)
                 (label
                  ($kclause ,(rename-kw-arity arity) ,(must-visit-cont body)
                            ,(and alternate (must-visit-cont alternate)))))
                (($ $kreceive ($ $arity req () rest () #f) kargs)
                 (label ($kreceive req rest (relabel kargs))))
                (($ $kif kt kf)
                 (label ($kif (relabel kt) (relabel kf))))))))))
      (define (visit-term term)
        (rewrite-cps-term term
          (($ $letk conts body)
           ,(match (visit-conts conts)
              (() (visit-term body))
              (conts (build-cps-term ($letk ,conts ,(visit-term body))))))
          (($ $letrec names vars funs body)
           ($letrec names (map rename vars) (map visit-fun funs)
                    ,(visit-term body)))
          (($ $continue k src exp)
           ($continue (relabel k) src ,(visit-exp exp)))))
      (define (visit-exp exp)
        (match exp
          ((or ($ $void) ($ $const) ($ $prim))
           exp)
          (($ $fun)
           (visit-fun exp))
          (($ $values args)
           (let ((args (map rename args)))
             (build-cps-exp ($values args))))
          (($ $call proc args)
           (let ((args (map rename args)))
             (build-cps-exp ($call (rename proc) args))))
          (($ $callk k proc args)
           (let ((args (map rename args)))
             (build-cps-exp ($callk (relabel k) (rename proc) args))))
          (($ $primcall name args)
           (let ((args (map rename args)))
             (build-cps-exp ($primcall name args))))
          (($ $prompt escape? tag handler)
           (build-cps-exp
             ($prompt escape? (rename tag) (relabel handler))))))
      (define (visit-fun fun)
        (rewrite-cps-exp fun
          (($ $fun free body)
           ($fun (map rename free) ,(must-visit-cont body)))))
      (values (visit-fun fun) nlabels nvars))))
