;;; Nested inter-procedure loop with condition in outer loop.

(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (+ acc 2))
        acc)))

(define (loop2 n)
  (let lp ((n n) (acc 0))
    (cond
     ((< 400 n)
      (lp (- n 1) (loop1 n acc)))
     ((< 200 n)
      (lp (- n 1) (+ acc 1)))
     (else
      acc))))

(let lp ((n 200) (acc '()))
  (if (< 0 n)
      (lp (- n 1) (cons (loop2 #e1e3) acc))
      acc))
