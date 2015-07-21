(define (loop low mid high)
  (let lp ((i 0) (acc 0))
    (cond
     ((= i 10000)
      acc)
     ((< low i mid)
      (lp (+ i 1) (+ acc 1)))
     ((< mid i high)
      (lp (+ i 1) (+ acc 2)))
     (else
      (lp (+ i 1) acc)))))

(loop 2000 5000 9000)
