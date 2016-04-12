;;; Simple loop with `br-if-eqv?', comparing flonums.

(define (loop n dec acc)
  (if (eqv? n 1.0)
      acc
      (loop (- n dec) dec (+ acc n))))

(loop 100000.0 1.0 0)
