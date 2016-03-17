;;; Loop with modulo, with various sign combinations.

(define (loop n x)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        (reverse! acc)
        (lp (- n 1) (cons (list (modulo n x)
                                (modulo (- n) x)
                                (modulo n (- x))
                                (modulo (- n) (- x)))
                          acc)))))

(loop 100 17)
