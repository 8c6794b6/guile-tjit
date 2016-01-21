;;; Simple loop containing `call-label', with boxing and unboxing.

(define (inner n)
  (define (proc n)
    (+ n 1))
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons proc acc)))))

(define (outer n)
  (let lp ((procs (inner n)) (acc 0))
    (if (null? procs)
        acc
        (lp (cdr procs) (+ acc ((car procs) n))))))

(outer #e1e3)
