(define (func-loops n)
  (let loop1 ((i1 1) (result 0))
    (if (> i1 n)
        result
        (let loop2 ((i2 1) (result result))
          (if (= i2 n)
              (loop1 (+ i1 1) result)
              (loop2 (+ i2 1) (+ result 1)))))))

(func-loops 18)
