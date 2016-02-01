;; Nested loop with flonums.

(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0.0 n)
        (lp (- n 1.0) (+ acc 1.2345))
        acc)))

(define (loop2 n)
  (let lp ((n n) (acc 0.0))
    (if (< 0.0 n)
        (lp (- n 1.0) (loop1 n acc))
        acc)))

(loop2 1000.0)
