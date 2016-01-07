;; Nested loop. Passing returned value from scheme procedure `add2' in
;; outer loop to inner loop.

(define (loop1 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc n)))))

(define (add2 n)
  (+ n 2))

(define (loop2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (loop1 (add2 n)))))))

(loop2 #e1e4)
