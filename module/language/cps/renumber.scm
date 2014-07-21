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
(define (sort-conts k0 conts new-k0 path-lengths)
  (let ((next -1))
    (let visit ((k k0))
      (define (maybe-visit k)
        (let ((entry (vector-ref conts k)))
          ;; Visit the successor if it has not been
          ;; visited yet.
          (when (and entry (not (exact-integer? entry)))
            (visit k))))

      (let ((cont (vector-ref conts k)))
        ;; Clear the cont table entry to mark this continuation as
        ;; visited.
        (vector-set! conts k #f)

        (match cont
          (($ $kargs names syms body)
           (let lp ((body body))
             (match body
               (($ $letk conts body) (lp body))
               (($ $letrec names syms funs body) (lp body))
               (($ $continue k src exp)
                (match exp
                  (($ $prompt escape? tag handler)
                   (maybe-visit handler)
                   (maybe-visit k))
                  (($ $branch kt)
                   ;; Visit the successor with the shortest path length
                   ;; to the tail first, so that if the branches are
                   ;; unsorted, the longer path length will appear
                   ;; first.  This will move a loop exit out of a loop.
                   (let ((k-len (vector-ref path-lengths k))
                         (kt-len (vector-ref path-lengths kt)))
                     (cond
                      ((if kt-len
                           (or (not k-len)
                               (< k-len kt-len)
                               ;; If the path lengths are the
                               ;; same, preserve original order
                               ;; to avoid squirreliness.
                               (and (= k-len kt-len) (< kt k)))
                           (if k-len #f (< kt k)))
                       (maybe-visit k)
                       (maybe-visit kt))
                      (else
                       (maybe-visit kt)
                       (maybe-visit k)))))
                  (_
                   (maybe-visit k)))))))
          (($ $kreceive arity k) (maybe-visit k))
          (($ $kclause arity ($ $cont kbody) alt)
           (match alt
             (($ $cont kalt) (maybe-visit kalt))
             (_ #f))
           (maybe-visit kbody))
          (($ $kfun src meta self tail clause)
           (match clause
             (($ $cont kclause) (maybe-visit kclause))
             (_ #f)))
          (_ #f))

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

(define (compute-tail-path-lengths preds ktail path-lengths)
  (let visit ((k ktail) (length-in 0))
    (let ((length (vector-ref path-lengths k)))
      (unless (and length (<= length length-in))
        (vector-set! path-lengths k length-in)
        (let lp ((preds (vector-ref preds k)))
          (match preds
            (() #t)
            ((pred . preds)
             (visit pred (1+ length-in))
             (lp preds))))))))

(define (compute-new-labels-and-vars fun)
  (call-with-values (lambda () (compute-max-label-and-var fun))
    (lambda (max-label max-var)
      (let ((labels (make-vector (1+ max-label) #f))
            (next-label 0)
            (vars (make-vector (1+ max-var) #f))
            (next-var 0)
            (preds (make-vector (1+ max-label) '()))
            (path-lengths (make-vector (1+ max-label) #f)))
        (define (add-predecessor! pred succ)
          (vector-set! preds succ (cons pred (vector-ref preds succ))))
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
                  (visit-term body label))
                 (($ $kfun src meta self tail clause)
                  (visit-cont tail)
                  (match clause
                    (($ $cont kclause)
                     (add-predecessor! label kclause)
                     (visit-cont clause))
                    (#f #f)))
                 (($ $kclause arity (and body ($ $cont kbody)) alternate)
                  (add-predecessor! label kbody)
                  (visit-cont body)
                  (match alternate
                    (($ $cont kalt)
                     (add-predecessor! label kalt)
                     (visit-cont alternate))
                    (#f #f)))
                 (($ $kreceive arity kargs)
                  (add-predecessor! label kargs))
                 (($ $ktail) #f)))))
          (define (visit-term term label)
            (match term
              (($ $letk conts body)
               (let lp ((conts conts))
                 (unless (null? conts)
                   (visit-cont (car conts))
                   (lp (cdr conts))))
               (visit-term body label))
              (($ $letrec names syms funs body)
               (visit-term body label))
              (($ $continue k src exp)
               (add-predecessor! label k)
               (match exp
                 (($ $branch kt)
                  (add-predecessor! label kt))
                 (($ $prompt escape? tag handler)
                  (add-predecessor! label handler))
                 (_ #f)))))
          (visit-cont fun))

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
                   (($ $kreceive)
                    #f))))))
          (define (visit-term term reachable?)
            (match term
              (($ $letk conts body)
               (for-each visit-cont conts)
               (visit-term body reachable?))
              (($ $letrec names syms funs body)
               (when reachable?
                 (for-each rename! syms)
                 (set! queue (fold (lambda (fun queue)
                                     (match fun
                                       (($ $fun free body)
                                        (cons body queue))))
                                   queue
                                   funs)))
               (visit-term body reachable?))
              (($ $continue k src ($ $fun free body))
               (when reachable?
                 (set! queue (cons body queue))))
              (($ $continue) #f)))

          (match fun
            (($ $cont kfun ($ $kfun src meta self ($ $cont ktail)))
             (collect-conts fun)
             (compute-tail-path-lengths preds ktail path-lengths)
             (set! next-label (sort-conts kfun labels next-label path-lengths))
             (visit-cont fun)
             (for-each compute-names-in-fun (reverse queue)))
            (($ $program conts)
             (for-each compute-names-in-fun conts))))

        (compute-names-in-fun fun)
        (values labels vars next-label next-var)))))

(define (apply-renumbering term labels vars)
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
             (label ($kreceive req rest (relabel kargs))))))))))
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
      (($ $closure k nfree)
       (build-cps-exp ($closure (relabel k) nfree)))
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
      (($ $branch kt exp)
       (build-cps-exp ($branch (relabel kt) ,(visit-exp exp))))
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

  (match term
    (($ $cont)
     (must-visit-cont term))
    (($ $program conts)
     (build-cps-term
       ($program ,(map must-visit-cont conts))))))

(define (renumber term)
  (call-with-values (lambda () (compute-new-labels-and-vars term))
    (lambda (labels vars nlabels nvars)
      (values (apply-renumbering term labels vars) nlabels nvars))))
