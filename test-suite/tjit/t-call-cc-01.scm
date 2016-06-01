;;; Loop containing `call/cc'. Ignores the continuation.

(define (loop1 lst acc)
  (call/cc
   (lambda (return)
     (let lp ((lst lst) (acc acc))
       (if (null? lst)
           acc
           (lp (cdr lst) (+ acc (car lst))))))))

(define (loop2 n)
  (let lp ((n n) (acc 0))
    (if (zero? n)
        acc
        (let ((lst (if (even? n)
                       '(1 2 3 4 5)
                       '(4 3 2 0 1))))
          (lp (- n 1) (+ acc (loop1 lst 0)))))))

(loop2 100)
