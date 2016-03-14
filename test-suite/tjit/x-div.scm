;;; `diviter' and `divrec' from Gabriel benchmark.

(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))

(define *ll* (create-n 200))

(define (iterative-div2 l)
  (do ((l l (cddr l))
       (a '() (cons (car l) a)))
      ((null? l) a)))

(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))

(define (test-1 l)
  (do ((i 3000 (- i 1))
       (r #f (iterative-div2 l)))
      ((= i 0) r))
  (iterative-div2 l)
  (iterative-div2 l)
  (iterative-div2 l))

(define (test-2 l)
  (do ((i 3000 (- i 1))
       (r #f (recursive-div2 l)))
      ((= i 0) r)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))

(define (run)
  (let loop ((n 15) (v 0))
    (if (zero? n)
        v
        (loop (- n 1)
              (cons
               (test-1 *ll*)
               (test-2 *ll*))))))

(run)
