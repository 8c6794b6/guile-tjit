;;; More multiple calls to cons with different arguments.

(define (loop n a b c d e f g h)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        (let lp ((acc acc) (sum 0))
          (if (null? acc)
              sum
              (lp (cdr acc) (+ sum (car acc)))))
        (lp (- n 1)
            (let* ((r1 (cons (+ n g) (cons (+ n h) acc)))
                   (r2 (cons (+ n d) (cons (+ n e) (cons (+ n f) r1)))))
              (cons n (cons (+ n a) (cons (+ n b) (cons (+ n c) r2)))))))))

(loop #e1e3 1 2 3 4 5 6 7 8)
