;;; Simple loop containing `call-label', with side exit.

(define (inner n)
  (define (proc n)
    (if (< n 100)
        (+ n 1)
        (+ n 2)))
  (let lp ((n n) (f proc) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) f (f acc)))))

(define (outer n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (inner n))))))

(outer #e1e3)
