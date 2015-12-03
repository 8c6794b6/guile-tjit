(define (loop n)
  (let lp ((n n) (acc 0))
    (cond
     ((= n 0)
      acc)
     ((< n 200)
      (lp (- n 1) (+ acc 1)))
     (else
      (lp (- n 1) (+ acc 2))))))

(loop 400)
