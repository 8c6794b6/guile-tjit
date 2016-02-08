;;; Another nested loop, calling inner loop twice.

(define (inner n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (+ acc n))
        acc)))

(define (outer n)
  (let lp ((n n) (acc 0))
    (if (< 0 n)
        (lp (- n 1) (inner n (inner n acc)))
        acc)))

(let lp ((n 0) (acc '()))
  (if (< n 20)
      (lp (+ n 1) (cons (outer 100) acc))
      acc))
