;;; Redefining nested inlined procedure once.  Frame local data in trace
;;; contains non-SCM values, will show segfault in REPL when debug level
;;; is greater than 1.

(define *r1* #f)
(define *r2* #f)

(define (f a)
  (+ a 1))

(define (g a)
  (+ (f a) 1))

(define (h a)
  (+ (g a) 1))

(define (loop n)
  (let lp ((n n) (acc 0))
    (if (= 0 n)
        acc
        (lp (- n 1) (h acc)))))

(set! *r1* (loop 1000))

(define (f a)
  (+ a 2))

(set! *r2* (loop 1000))

(list *r1* *r2*)
