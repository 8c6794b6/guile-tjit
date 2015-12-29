;;; Simple test for `vector-length'. Calling `f1' from `f2' to compile
;;; bytecode for `vector-length', since byte compilation moves out
;;; `vector-length' from the loop in `f1'.

(define (f1 n v)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (vector-length v))))))

(define (f2 n)
  (let lp ((i n) (acc 0))
    (if (= i 0)
        acc
        (lp (- i 1) (+ acc (f1 n #(1 2 3 4 5 6 7)))))))

(f2 1000)
