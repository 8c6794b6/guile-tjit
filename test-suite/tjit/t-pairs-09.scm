;;; Simple loop to accumulate list of floating numbers.

(define (loop lst)
  (let lp ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (lp (cdr lst) (+ acc (car lst))))))

(loop (map (lambda (x) (+ x 1000.0))
           (iota #e1e3)))
