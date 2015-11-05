;;; Simple cons, using non-volatile registers only.
;;;
;;; XXX: Test passes, but getting segfault with:
;;;
;;;   $ ../../meta/guile --tjit --fresh-auto-compile t-pairs-03.scm
;;;

(define (loop n)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        (let lp ((acc acc) (sum 0))
          (if (null? acc)
              sum
              (lp (cdr acc) (+ sum (car acc)))))
        (lp (- n 1) (cons n acc)))))

(loop #e1e3)
