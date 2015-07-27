;; Another nested branches, was showing incorrect result for while.

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1)
            (if (< i 800)
                (+ (if (< i 400)
                       (+ (if (< i 200)
                              (+ i 3)
                              (+ i 4))
                          1)
                       (+ i 2))
                   1)
                (+ i 1))))))

(loop 1000)
