;; Loop containing long inlined procedure call.

(define (f a)
  (+ (f1 a) (f1 1)))

(define (f1 x)
  (+ (f2 x) (f2 2)))

(define (f2 x)
  (+ (f3 x) (f3 3)))

(define (f3 x)
  (+ (f4 x) (f4 4)))

(define (f4 x)
  (+ (f5 x) (f5 5)))

(define (f5 x)
  (+ (f6 x) (f6 6)))

(define (f6 x)
  (+ (f7 x) (f7 7)))

(define (f7 x)
  (+ 8 x))

(define (loop n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (f acc)))))

(loop #e1e3)
