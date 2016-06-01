;; Loop with call/cc, take 3. Update top level variable inside loop.

(define count 0)

(define (loop lst)
  (call/cc
   (lambda (return)
     (cond
      ((null? lst)
       0)
      ((= (car lst) 42)
       (set! count (+ count 1))
       (return 1))
      (else
       (loop (cdr lst)))))))

(define (outer n)
  (let ((lst (iota 300)))
    (let lp ((n n) (v #f))
      (if (zero? n)
          count
          (lp (- n 1) (loop lst))))))

(outer 1000)
