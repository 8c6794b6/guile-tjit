(define (f a)
  (if (< a 200)
      (+ a 1)
      (+ a 2)))

(define (g a)
  (+ (f a) 1))

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1) (g acc)))))

(loop 1000)
