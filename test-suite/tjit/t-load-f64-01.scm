;; Nested loop with `br-if-=', outer loop contains `load-f64'.

(define (loop1 n)
  (let lp ((i 0.0) (acc 0))
    (if (= i n)
        acc
        (lp (+ i 1.0) (+ acc 1)))))

(define (loop2 n)
  (let lp ((i 0) (acc 0))
    (if (= i n)
        acc
        (lp (+ i 1) (+ acc (loop1 12345.0))))))

(loop2 100)
