;; Nested branches with inlined procedures.

(define (f a)
  (if (< a 400)
      (+ a 2)
      (+ a 3)))

(define (g a)
  (if (< a 200)
      (+ a 1)
      (f a)))

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1) (g acc)))))

(loop #e1e5)
