;; Redefining top-level procedure, more than twice. Test for replacing
;; the first bailout code in side trace. Failing guard tests whether the
;; IP of callee procedure passed at runtime matches with the one used
;; at compilation time.

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
