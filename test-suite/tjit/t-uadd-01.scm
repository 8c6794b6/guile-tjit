;;; Simple loop containing `uadd'

(define (loop n)
  (define (f a)
    (let ((c (modulo a 11))
          (d (modulo a 12)))
      (+ c d)))
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (f n))))))

(loop 1000)
