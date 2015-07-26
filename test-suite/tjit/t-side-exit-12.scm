;;; Almost same as t-side-exit-11, but calling twice.

(define (f a)
  (if (< a 1000)
      (+ a 3)
      (+ a 4)))

(define (g a)
  (if (< a 2000)
      (+ (f a) 2)
      (+ a 2)))

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1) (g acc)))))

(list (loop #e1e5)
      (loop #e1e5))
