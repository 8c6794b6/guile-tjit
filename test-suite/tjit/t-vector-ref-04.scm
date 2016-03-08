;;; Loop to catch out-of-range error with `vector-ref/immediate'.

(define (loop1 v n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (vector-ref v 2))))))

(define (loop2 n)
  (let ((v #(1 2 3)))
    (let lp ((n n) (acc 0))
      (if (= n 0)
          acc
          (lp (- n 1) (+ acc (loop1 v n)))))))

(define (loop3 n)
  (let ((v #(1)))
    (let lp ((n n) (acc 0))
      (if (= n 0)
          acc
          (lp (- n 1) (+ acc (loop1 v n)))))))

(define (run-loop2)
  (catch #t
    (lambda ()
      (loop2 100))
    (lambda args
      (format #f "~a~%" args))))

(define (run-loop3)
  (catch #t
    (lambda ()
      (loop3 100))
    (lambda args
      (format #f "~a~%" args))))

;; Call `loop2' enough times to make peeled loop in `loop1' hot.
(do ((i 0 (+ i 1)))
    ((= i 100))
  (loop2 100))

(list (run-loop2)
      (run-loop3))
