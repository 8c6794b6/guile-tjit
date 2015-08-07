;;; Side exit from inlined procedure, take 2.

(define (f a)
  (if (< a 100)
      (+ a 1)
      (+ a 2)))

(define (g a)
  (if (< a 200)
      (+ (f a) 1)
      (+ a 1)))

(define (h a)
  (if (< a 400)
      (+ (g a) 1)
      (+ a 1)))

(define (loop n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (h acc)))))

(list (loop 1000)
      (loop 1000))
