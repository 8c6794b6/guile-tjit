(define (loop n)
  (let ((result 0))
    (let lp ((n n))
      (if (< 0 n)
          (begin
            (set! result (+ result 1))
            (lp (- n 1)))
          result))))

(list (loop #e1e3)
      (loop #e1e3))
