;;; More multiple calls to cons with different arguments.

(use-modules (srfi srfi-1))

(define (loop n a b c d e f g h)
  (let lp ((n n) (acc '()))
    (if (= n 0)
        (fold + 0 acc)
        (lp (- n 1)
            (cons n (cons a (cons b (cons c (cons d (cons e (cons f (cons g (cons h acc)))))))))))))

(loop #e1e3 1 2 3 4 5 6 7 8)
