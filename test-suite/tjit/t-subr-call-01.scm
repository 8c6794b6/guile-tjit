;; Simple loop with subr-call `length'.

(define (loop n lst)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (length lst))))))

(loop #e1e3 '(1 2 3 4 5))
