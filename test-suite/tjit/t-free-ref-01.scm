;;; Simple loop for `free-ref'.

(define (gen-loop n)
  (lambda (acc)
    (let lp ((n n) (acc acc))
      (if (= n 0)
          acc
          (lp (- n 1) (+ acc n))))))

(define (outer n)
  (let lp ((n n) (acc 0) (closure (gen-loop n)))
    (if (= n 0)
        acc
        (lp (- n 1) (closure acc) closure))))

(outer 1000)
