;; Non-tail recursive sum with flonum.

(define (sum n)
  (if (<= n 0.0)
      n
      (+ n (sum (- n 1.0)))))

(sum 1000.0)
