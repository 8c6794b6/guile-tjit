;;; Simple loop with subr-call, pushes multiple arguments.

(define (loop n tbl)
  (let lp ((n n) (acc 0))
    (if (< n 0)
        acc
        (lp (- n 1) (+ acc (hashq-ref tbl 1 0))))))

(let ((t (make-hash-table)))
  (hashq-set! t 0 123)
  (hashq-set! t 1 456)
  (hashq-set! t 2 789)
  (loop 1000 t))
