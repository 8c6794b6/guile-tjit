;;; Simple loop with bind-rest, rest has 5 elements.

(define (loop n . args)
  (define (f n acc lst)
    (if (= n 0)
        acc
        (f (- n 1) (+ acc (length lst)) lst)))
  (f n 0 args))

(define (loop2 n acc)
  (if (= n 0)
      (apply + acc)
      (loop2 (- n 1) (cons (loop n 1 2 3 4 5) acc))))

(loop2 #e1e3 '())
