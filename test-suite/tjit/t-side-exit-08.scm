;; Test for local management with side exit from level 1 inlined
;; procedure.

(define (f a b c)
  (if (< a 400)
      (+ a b)
      (+ a c)))

(define (loop n)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1) (f acc 1 2)))))

(loop 1000)
