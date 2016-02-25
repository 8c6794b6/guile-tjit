;;; Nested loop containing procedure defined as inner procedure with
;;; side exit, with boxing and unboxing.

(define (inner n)
  (define (proc n)
    (if (< n 100)
        (+ n 1)
        (+ n 2)))
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons proc acc)))))

(define (outer n)
  (let lp ((procs (inner n)) (acc 0))
    (if (null? procs)
        acc
        (lp (cdr procs)
            (let lp ((n n) (acc acc))
              (if (= n 0)
                  acc
                  (lp (- n 1) (+ acc ((car procs) n)))))))))

(outer #e1e3)
