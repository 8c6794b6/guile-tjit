;;; Simple loop containing `br-ifnargs-ne'.

(define loop1
  (case-lambda
    ((n)
     (let lp ((n n) (acc 0))
       (if (= n 0)
           acc
           (lp (- n 1) (+ acc n)))))
    ((n acc)
     (let lp ((n n) (acc acc))
       (if (= n 0)
           acc
           (lp (- n 1) (+ acc n)))))))

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

(list (loop2 100)
      (loop3 100))
