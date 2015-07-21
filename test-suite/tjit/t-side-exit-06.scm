;;; Side exit in inlined procedure.

(define (f a)
  (if (< a 100)
      (+ a 1)
      (+ a 2)))

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (= 0 i)
        acc
        (lp (- i 1) (f acc)))))

(loop 200)
