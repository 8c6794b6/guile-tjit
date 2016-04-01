;;; Simple loop with bind-rest, null rest.

(define (loop n . args)
  (define (f n acc lst)
    (if (= n 0)
        acc
        (f (- n 1) (+ acc (length lst)) lst)))
  (f n 0 args))

(define (loop2 n acc)
  (if (= n 0)
      (apply + acc)
      (loop2 (- n 1) (cons (loop n) acc))))

(loop2 #e1e3 '())
