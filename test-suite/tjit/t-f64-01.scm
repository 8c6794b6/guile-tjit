;;; Simple loop containing `fadd'.

(define (loop n)
  (let lp ((n n) (acc 0.0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc 123.45)))))

(loop 1000)
