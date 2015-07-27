;; More nested branches with inlined procedures.

(define (f a)
  (if (< a 400)
      (+ a 3)
      (+ a 4)))

(define (g a)
  (if (< a 200)
      (+ a 2)
      (f a)))

(define (h a)
  (if (< a 100)
      (+ a 1)
      (g a)))

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1) (h acc)))))

(loop #e1e5)
