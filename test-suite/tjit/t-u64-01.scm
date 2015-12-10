;;; Test for `uadd/immediate', `br-if-u64-<', `br-if-u64-='.

(define (strange-sum a b)
  (if (= (modulo a 46) 10)
      (+ a b)
      (- a b)))

(define (go)
  (let lp ((i 0) (acc 0))
    (if (< i #e1e3)
        (lp (+ i 1) (+ acc (strange-sum i 10)))
        acc)))

(go)
