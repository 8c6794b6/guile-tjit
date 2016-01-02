;;; Simple loop to generate and accumulate list of floating numbers.

(define (gen n)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (+ n 1000.0) acc)))))

(define (rec lst)
  (let lp ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (lp (cdr lst) (+ (car lst) acc)))))

(rec (gen #e1e4))
