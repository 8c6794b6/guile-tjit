;; Nested loop, inner loop taking flonum getting hot before outer loop
;; taking fixnum gets hot.

(define (loop1 lst acc)
  (let lp ((lst lst) (acc acc))
    (if (null? lst)
        acc
        (lp (cdr lst) (+ acc (car lst))))))

(define (loop2 n lst)
  (let lp ((n n) (r #f))
    (if (= n 0)
        r
        (lp (- n 1) (loop1 lst 0)))))

(let* ((n 65)
       (l1 (make-list n 1))
       (l2 (make-list n 1.0))
       (m 1000)
       (l3 (make-list m 1))
       (l4 (make-list m 1.0)))
  (list (loop1 l1 0)
        (loop1 l2 0)
        (loop2 100 l3)
        (loop2 100 l4)))
