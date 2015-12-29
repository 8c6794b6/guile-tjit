;;; Simple loop containing `vector-ref/immediate'.
;;;
;;; Loops are nested since `vector-ref/immediate' bytecode operation is
;;; moved outside of loop in `f1'.

(define (f1 n v)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (vector-ref v 6))))))

(define (f2 n v)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1) (+ acc (f1 n v))))))

(f2 100 #(1 2 3 4 5 6 7))
