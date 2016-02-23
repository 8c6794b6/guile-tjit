;; Nested loop containing `make-long-long-immediate'.

(define (loop1 n)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons 2305843009213693951 acc)))))

(define (loop2 n)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (loop1 n) acc)))))

(loop2 50)
