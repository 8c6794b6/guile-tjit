;;; Simple loop containing `add'.

(define (loop n ini incr)
  (let lp ((n n) (acc ini))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc incr)))))

(list (loop 1000 0 123)
      (loop 1000 0 1.23)
      (loop 1000 0.0 123456)
      (loop 1000 0.0 123.456))
