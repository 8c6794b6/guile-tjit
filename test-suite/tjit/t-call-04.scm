;; Test to see vm-tjit is not inlining call to `my-add' from
;; `my-add-add'.

(define (my-add a b)
  (+ a b))

(define (my-add-add a b)
  (+ (my-add a b)
     (my-add b 1)
     (my-add b 2)
     (my-add b 3)))

(define (f n)
  (let lp ((n n) (acc 0))
    (if (< 0 n)
        (lp (- n 1) (my-add-add acc n))
        acc)))

(f #e1e3)
