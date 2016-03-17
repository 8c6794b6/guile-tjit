;;; Simple loop containing `set-car!'.

(define (loop lst)
  (let lp ((lst lst) (n 0) (acc '()))
    (if (null? lst)
        (reverse! acc)
        (let ((h (copy-tree (car lst)))
              (t (cdr lst)))
          (set-car! h n)
          (lp t (+ n 1) (cons h acc))))))

(let ((lst (make-list 100 '(1 2 3 4 5))))
  (loop lst))
