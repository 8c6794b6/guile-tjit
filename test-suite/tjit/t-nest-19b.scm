;;; More nested loops, nesting 10 simple procedures.

(define (loop1 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (+ acc 1))
        acc)))

(define (loop2 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop1 n acc))
        acc)))

(define (loop3 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop2 n acc))
        acc)))

(define (loop4 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop3 n acc))
        acc)))

(define (loop5 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop4 n acc))
        acc)))

(define (loop6 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop5 n acc))
        acc)))

(define (loop7 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop6 n acc))
        acc)))

(define (loop8 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop7 n acc))
        acc)))

(define (loop9 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop8 n acc))
        acc)))

(define (loop10 n acc)
  (let lp ((n n) (acc acc))
    (if (< 0 n)
        (lp (- n 1) (loop9 n acc))
        acc)))

(loop10 10 0)
