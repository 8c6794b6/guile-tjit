(define (inner n acc)
  (let lp ((n n) (acc acc))
    (cond
     ((<= n 0) acc)
     ((<= n 100) (lp (- n 1) (+ acc 2)))
     (else (lp (- n 1) (+ acc 1))))))

(define (outer acc)
  (do ((i 0 (+ i 1)))
      ((< 40 i))
    (do ((j 0 (+ j 1)))
        ((< 40 j))
      (set! acc (inner j acc))))
  acc)

(do ((i 0 (+ i 1))
     (acc '() (cons (outer i) acc)))
    ((<= 20 i) acc))
