;;; Loop containing subr-call, uses spilled variable and lots of FPRs.

(define (loop count a b c
              d e f g
              h i j k
              l m n o)
  (let lp ((count count) (acc 0))
    (if (= count 0)
        acc
        (lp (- count 1)
            (+ acc (exact->inexact count) a b c
               d e f g
               h i j k
               l m n o)))))

(loop #e1e5 1.0 1.0 1.0
      1.0 1.0 1.0 1.0
      1.0 1.0 1.0 1.0
      1.0 1.0 1.0 1.0)
