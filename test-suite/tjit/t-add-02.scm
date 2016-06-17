;;; Simple loop containing `add' with heap object.

(define (loop lst ini)
  (let lp ((lst lst) (acc ini))
    (if (null? lst)
        acc
        (lp (cdr lst) (+ acc (car lst))))))

(list (let ((lst (make-list 1000 1)))
        (loop lst 0))
      (let ((lst (make-list 1000 1.23)))
        (loop lst 0)))
