;;; Simple loop containing `make-vector', with flonum vector contents.

(define (loop lst k)
  (let lp ((lst lst) (acc '()))
    (if (null? lst)
        acc
        (lp (cdr lst) (cons (make-vector k (+ (car lst) 7.654321)) acc)))))

(let ((lst (make-list 100 1.234567)))
  (loop lst 3))
