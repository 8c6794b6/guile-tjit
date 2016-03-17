;; Loop with tail-call, side trace ends with `tail-call'.

(define (g n acc)
    (if (= n 0)
        acc
        (if (< 300 n)
            (g (- n 1) (+ acc n))
            (g (- n 1) (+ acc n 1)))))

(define (loop n acc)
  (define (f n acc)
    (if (< n 300)
        (+ acc 2)
        (+ acc 1)))
  (if (= n 0)
      acc
      (if (< 300 n)
          (loop (- n 1) (f n (g n (+ acc 1))))
          (loop (- n 1) (f n (g n acc))))))

(loop 1000 0)
