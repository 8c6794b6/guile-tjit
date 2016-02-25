;;; Simple loop containing `mul' with floating point number and small
;;; integer.

(define (loop n)
  (let lp ((n n) (acc 1.0e-100))
    (if (= n 1)
        acc
        (lp (- n 1) (* acc n)))))

(define (outer n)
  (let lp ((i 1) (acc 1))
    (if (= i n)
        acc
        (lp (+ i 1) (loop 200)))))

(outer #e1e5)
