(define (micro a)
  (let lp ((a a) (b 0) (c 0) (d 0) (e 0))
    (if (< 0 a)
        (lp (- a 1) (+ b 1) (+ c 1) (+ d 1) (+ e 1))
        (+ a b c d e))))

(micro #e1e3)
