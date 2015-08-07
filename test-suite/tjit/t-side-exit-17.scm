;;; More side exit from inlined procedure.
;;;
;;; Contains side trace which takes some of its initial arguments from
;;; parent trace, rest of the arguments from current frame.

(define (f a)
  (if (< a 100)
      (+ a 1)
      (+ a 2)))

(define (g a)
  (if (< a 200)
      (+ (f a) 1)
      (+ a 1)))

(define (h a)
  (if (< a 400)
      (+ (g a) 1)
      (+ a 1)))

(define (i a)
  (if (< a 800)
      (+ (h a) 1)
      (+ a 1)))

(define (loop n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (h n))))))

(define (loop2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (+ acc (i n))))))

(list (loop 1000)
      (loop2 1000))
