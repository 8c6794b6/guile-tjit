;; Simple nested loop, contains `subr-call' in outer loop, outer loop
;; passes returned value from `subr-call' to inner.

(define (loop1 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc 1)))))

(define (loop2 lst)
  (let lp ((lst lst) (acc 0))
    (if (null? lst)
        acc
        (lp (cdr lst) (+ acc (loop1 (char->integer (car lst))))))))

(let ((elem (string->list "abcdefghijklmnopqrstuvwxyz")))
  (loop2 (apply append (make-list #e1e3 elem))))
