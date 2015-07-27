;;; Almost same as t-side-exit-12, but with lower threshold value.

(define (f a)
  (if (< a 248) ; (< a 249) works.
      (+ a 3)
      (+ a 4)))

(define (g a)
  (if (< a 1000)
      (+ (f a) 1)
      (+ a 2)))

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1) (g acc)))))

(list (loop #e2e3)
      (loop #e2e3))
