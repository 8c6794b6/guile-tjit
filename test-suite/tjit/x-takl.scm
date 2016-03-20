;;; takl from racket benchmarks.

(define (listn n)
  (if (not (= 0 n))
      (cons n (listn (- n 1)))
      '()))

(define l18l (listn 18))
(define l12l (listn 12))
(define  l6l (listn 2))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x)
                y z)
           (mas (cdr y)
                z x)
           (mas (cdr z)
                x y))))

(define (shorterp x y)
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))

;;; call: (mas 18l 12l 6l)

(define (run-takl x)
  (let loop ((n 2) (v 0))
    (if (zero? n)
        v
        (loop (- n 1)
              (mas l18l l12l x)))))

(run-takl l6l)
