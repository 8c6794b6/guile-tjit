(define res1 #f)
(define res2 #f)

(define *add-me* 200)

(define (micro n)
  (let lp ((n n) (acc 0))
    (if (< 0 n)
        (lp (- n 1) (+ acc *add-me*))
        acc)))

(set! res1 (micro #e1e7))
(set! *add-me* 300)
(set! res2 (micro #e1e7))

(list res1 res2)
