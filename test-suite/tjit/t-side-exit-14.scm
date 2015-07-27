;;; Side exit from inlined procedure, take 2.

(define (f a)
  (if (< a 2000)
      (+ a 3)
      (+ a 4)))

(define (g a)
  (if (< a 4000)
      (+ (f a) 1)
      (+ a 2)))

(define (h a)
  (if (< a 8000)
      (+ (g a) 1)
      (+ a 1)))

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1) (h acc)))))

(list (loop #e1e5)
      (loop #e1e5))
