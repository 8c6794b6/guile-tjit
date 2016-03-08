;; Getting Scheme error `out of range' from `vector-ref'.

(define (loop1 n v)
  (let lp ((i 0) (acc 0))
    (if (= i n)
        acc
        (lp (+ i 1) (+ acc (vector-ref v i))))))

(define (run-loop v)
  (catch #t
    (lambda () (loop1 100 v))
    (lambda args (format #f "~a" args))))

(list (run-loop (make-vector 200 1))
      (run-loop (make-vector 50 1)))
