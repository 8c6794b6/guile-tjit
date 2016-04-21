;;; Loop containing `mul' with flonum and fraction.

(define (loop lst1 lst2)
  (let lp ((lst1 lst1) (lst2 lst2) (acc 1.0))
    (if (null? lst1)
        acc
        (lp (cdr lst1) (cdr lst2) (* acc (car lst1) (car lst2))))))

(let* ((n #e1e3)
       (lst1 (make-list n 99/100))
       (lst2 (make-list n 101/100)))
  (loop lst1 lst2))
