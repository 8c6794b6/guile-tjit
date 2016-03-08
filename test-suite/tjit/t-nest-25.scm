;;; Nested loop, with inner loop having small number of iterations.

(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (cond
     ((< n 0) acc)
     ((< 50 n) (lp (- n 1) (+ acc n)))
     (else (lp (- n 1) (+ acc 1))))))

(define (loop2 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop1 n acc))
        acc)))

(do ((n 0 (+ n 1))
     (acc '() (cons (loop2 n 0) acc)))
    ((<= 50 n) acc))
