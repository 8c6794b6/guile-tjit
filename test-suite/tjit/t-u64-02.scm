;;; Test for `usub/immediate'.

(define (strange-sum a b)
  (if (= (modulo a 46) 10)
      (+ a b)
      (- a b)))

(define (go)
  (let lp ((i #e1e3) (acc 0))
    (if (< 0 i)
        (lp (- i 1) (+ acc (strange-sum i 10)))
        acc)))

(go)
