;;; Loop with boxed flonums. Iteration cound is made large to trigger
;;; garbage collection.

(define (loop1 n)
  (let lp ((n n) (j 0.0) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (+ j 1.0) (cons (cons (+ j 1.0) (+ j 2.0)) acc)))))

(define (loop2 lst)
  (define (f lst acc)
    (if (null? lst)
        acc
        (f (cdr lst) (let ((elem (car lst)))
                       (+ acc (car elem) (cdr elem))))))
  (f lst 0))

(loop2 (loop1 #e1e6))
