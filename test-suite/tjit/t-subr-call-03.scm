;;; Simple loops, one returning car from subr-call, another returning
;;; small integer.

(define (f n i)
  (let lp ((n n) (i i) (acc '()))
    (if (= n 0)
        (g acc)
        (lp (- n 1)
            (modulo (+ i 1) 60)
            (cons (integer->char (+ i 65)) acc)))))

(define (g lst)
  (let lp ((lst lst) (s 0))
    (if (null? lst)
        s
        (lp (cdr lst) (+ s (char->integer (car lst)))))))

(f #e1e3 0)
