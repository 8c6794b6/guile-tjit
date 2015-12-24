;;; Simple loop with box, updating float value.

(define (loop n)
  (let ((b1 0)
        (b2 0)
        (b3 0))
    (let lp ((n n))
      (if (< 0 n)
          (begin
            (set! b1 (+ b1 n))
            (set! b2 (+ b1 0.1234567))
            (set! b3 (+ b1 b2))
            (lp (- n 1)))
          (list b1 b2 b3)))))

(loop #e1e3)
