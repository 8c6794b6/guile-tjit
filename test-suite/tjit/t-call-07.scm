;;; Loop containing `call' with varying procedure.

(define (loop lst f)
  (let lp ((lst lst) (acc '()))
    (if (null? lst)
        acc
        (lp (cdr lst) (cons (f (car lst)) acc)))))

(define (add2 x)
  (+ x 2))

(let ((lst (iota 100)))
  (list (loop lst symbol?)
        (loop lst (lambda (x) (= x 100)))
        (loop lst number?)
        (loop lst add2)
        (loop lst even?)))
