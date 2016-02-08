;;; Simple loop with `free-ref', uses non-zero free ref index.

(define (gen a b c)
  (lambda (i)
    (cond
     ((= i 0) (set! a (+ a 1)) a)
     ((= i 1) (set! b (+ b 2)) b)
     (else    (set! c (+ c 3)) c))))

(define (loop n)
  (let lp ((n n) (c (gen 0 0 0)) (acc 0))
    (cond
     ((< n 0) acc)
     ((< n 200) (lp (- n 1) c (+ acc (c 0))))
     ((< n 400) (lp (- n 1) c (+ acc (c 1))))
     (else (lp (- n 1) c (+ acc (c 2)))))))

(loop 1000)
