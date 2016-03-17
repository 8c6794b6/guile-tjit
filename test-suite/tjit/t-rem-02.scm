;;; Loop with remainder, various signs.

(define (loop n x)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (list (remainder n x)
                                (remainder (- n) x)
                                (remainder n (- x))
                                (remainder (- n) (- x)))
                          acc)))))

(loop 100 -17)
