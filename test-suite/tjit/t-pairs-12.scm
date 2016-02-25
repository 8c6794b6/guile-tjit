;; Reversing list with variable element types.

(define (f lst)
  (let lp ((lst lst) (acc '()))
    (if (null? lst)
        acc
        (lp (cdr lst) (cons (car lst) acc)))))

(let* ((e '(123 1.2345 #f #\a (1 2 3) foo #(10 20 30) "bar"))
       (lst (apply append (make-list #e1e3 e))))
  (f lst))
