(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (cond
     ((< n 0) acc)
     ((< 50 n) (lp (- n 1) (+ acc n)))
     (else (lp (- n 1) (+ acc 1))))))

(do ((n 0 (+ n 1))
     (acc '() (cons (loop1 2 0) acc)))
    ((<= 100 n) acc))
