;; Redefining top-level procedure, more than twice.

(define *result* '())
(define *num-loops* 1000)

(define (add-result! r)
  (set! *result* (cons r *result*)))

(define (f a)
  (+ a 1))

(define (loop n)
  (let lp ((n n) (acc 0))
    (if (= 0 n)
        acc
        (lp (- n 1) (f acc)))))

(add-result! (loop *num-loops*))

(define (f a)
  (+ a 2))
(add-result! (loop *num-loops*))

(define (f a)
  (+ a 3))
(add-result! (loop *num-loops*))

(define (f a)
  (+ a 4))
(add-result! (loop *num-loops*))

(define (f a)
  (+ a 5))
(add-result! (loop *num-loops*))

(define (f a)
  (+ a 6))
(add-result! (loop *num-loops*))

(reverse *result*)
