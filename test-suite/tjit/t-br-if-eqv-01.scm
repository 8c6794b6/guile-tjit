;;; Simple loop with `br-if-eqv?', comparing fixnums.

(define (loop n dec acc)
  (if (eqv? n 1)
      acc
      (loop (- n dec) dec (+ acc n))))

(loop 100000 1 0)
