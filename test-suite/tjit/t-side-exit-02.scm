(define (loop low high)
  (let lp ((i 0) (acc 0))
    (cond
     ((= i 1000)
      acc)
     ((< high i)
      (lp (+ i 1) (+ acc 1)))
     ((< low i)
      (lp (+ i 1) (+ acc 2)))
     (else
      (lp (+ i 1) acc)))))

(loop 200 800)
