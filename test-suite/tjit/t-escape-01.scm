(define z #f)

(define (loop)
  (let lp ((i 1) (x 0))
    (if (= i 100)
        x
        (begin
          (when (= i 90) (set! z i))
          (lp (+ i 1) (+ x i))))))

(let ((res (loop)))
  (list res z))
