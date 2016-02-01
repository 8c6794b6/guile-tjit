;;; Nested inter-procedure loop with condition in outer loop. Outer loop
;;; gets hot first, then inner loop gets hot.

(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (+ acc 2))
        acc)))

(define (loop2 n)
  (let lp ((n n) (acc 0))
    (cond
     ((< 400 n)
      (lp (- n 1) (+ acc 1)))
     ((< 200 n)
      (lp (- n 1) (loop1 n acc)))
     (else
      acc))))

(loop2 1000)
