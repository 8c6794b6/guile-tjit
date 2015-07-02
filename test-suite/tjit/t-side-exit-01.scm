(define (loop n)
  (let lp ((i 0) (acc 0))
    (cond
     ((= i n)
      acc)
     ((< i 200)
      (lp (+ i 1) (+ acc 1)))
     (else
      (lp (+ i 1) (+ acc 2))))))

(loop 400)
