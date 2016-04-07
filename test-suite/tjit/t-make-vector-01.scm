;;; Simple loop containing `make-vector'.

(define (loop1 n k)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        (let lp ((acc acc) (sum 0))
          (if (null? acc)
              sum
              (lp (cdr acc)
                  (let ((v (car acc)))
                    (let lp ((i (- (vector-length v) 1)) (k 0))
                      (if (< i 0)
                          (+ sum k)
                          (lp (- i 1) (+ k (vector-ref v i)))))))))
        (lp (- n 1) (cons (make-vector k n) acc)))))

(loop1 1000 5)
