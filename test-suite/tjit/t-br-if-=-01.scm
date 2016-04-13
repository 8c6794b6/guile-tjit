;; Loop with `br-if-=', loop ends with non-backward-jump, takes small
;; fixnum.

(define (loop n)
  (let lp ((i 0) (acc 0))
    (if (= i n)
        acc
        (lp (+ i 1) (+ acc 1)))))

(do ((i 0 (+ i 1))
     (acc 0 (+ (loop 3) acc)))
    ((= i 65) acc))
