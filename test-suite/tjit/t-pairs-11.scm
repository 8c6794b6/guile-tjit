;;; Simple loop to show error message. Test that unboxed heap object
;;; has guard for float numbers.

(define (loop vec)
  (let lp ((i (- (vector-length vec) 1)) (acc 0))
    (if (< i 0)
        acc
        (lp (- i 1) (+ (vector-ref vec i) acc)))))

(let* ((v (make-vector 1000 1.2345))
       (r1 (loop v))
       (_ (vector-set! v 127 'blahblah))
       (r2 (catch #t
             (lambda () (loop v))
             (lambda args args))))
  (list r1 r2))
