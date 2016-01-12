;;; Nested inter-procedure loop with condition in inner loop.

(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (cond
     ((< 500 n)
      (lp (- n 1) (+ acc 4)))
     ((< 250 n)
      (lp (- n 1) (+ acc 3)))
     ((< 0 n)
      (lp (- n 1) (+ acc 2)))
     (else
      acc))))

(define (loop2 n)
  (let lp ((n n) (acc 0))
    (if (< 0 n)
        (lp (- n 1) (+ acc (loop1 n 0)))
        acc)))

(loop2 1000)
