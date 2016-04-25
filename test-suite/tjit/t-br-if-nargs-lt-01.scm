;;; Simple loop containing `br-ifnargs-le'.

(define loop1
  (case-lambda*
   ((n #:optional (acc 0))
    (let lp ((n n) (acc acc))
      (if (zero? n)
          acc
          (lp (- n 1) (+ acc n)))))
   ((n acc inc)
    (let lp ((n n) (acc acc))
      (let lp ((n n) (acc acc))
        (if (zero? n)
            acc
            (lp (- n 1) (+ acc inc))))))))

(define (loop2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (loop1 n))))))

(define (loop3 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (loop1 n acc)))))

(define (loop4 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (loop1 n acc n)))))

(list (loop2 100)
      (loop3 100)
      (loop4 100))
