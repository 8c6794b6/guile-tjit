;;; Simple inter-procedure nested loop.

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (< 0 i)
        (lp (- i 1) (+ acc 1))
        acc)))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (< 0 i)
        (lp (- i 1) (loop1 n acc))
        acc)))

(list (loop2 #e1e2)
      (loop2 #e1e3)
      (loop2 #e1e4))
