;;; Simple loop containing `call-label'.

(define (inner n)
  (define (proc i)
    (+ i 1))
  (let lp ((n n) (f proc) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) f (f acc)))))

(define (outer n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ (inner n) acc)))))

(outer #e1e3)
