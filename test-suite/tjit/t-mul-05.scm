;;; Simple loop containing `mul' with combinations of fixnum and flonum.

(define (loop n ini incr)
  (let lp ((n n) (acc ini))
    (if (= n 0)
        acc
        (lp (- n 1) (* acc incr)))))

(list (loop 100 1 123)
      (loop 100 1 1.23)
      (loop 100 1.0 123456)
      (loop 100 1.0 123.456))
