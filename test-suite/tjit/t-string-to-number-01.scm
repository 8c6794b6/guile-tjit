;;; Simple loop containing `string->number'.

(define (loop1 n str)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ (string->number str) acc)))))

(define (loop2 n)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        acc
        (lp (- n 1) (cons (loop1 n "12345") acc)))))

(loop2 100)
