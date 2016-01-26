;;; Simple loop calling closure.

(define (gen n)
 (lambda ()
    (let ((old-n n))
      (set! n (+ old-n 1))
      n)))

(define (loop n)
  (let lp ((n n) (c (gen 0)) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) c (+ acc (c))))))

(loop #e1e3)
