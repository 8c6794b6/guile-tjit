;; Simple loop with `br-if-eq'.

(define (loop n acc)
  (if (eq? n 0)
      acc
      (loop (- n 1) (+ acc 1))))

(loop 1000 0)
