;; More inlined procedures in nested loop. Inner procedure has three
;; loops, calling the inner loop and outer loop 50 times each, returning
;; list of results.

(define (loop1 n acc)
  (let lp ((i n) (acc acc))
    (if (= 0 i)
        acc
        (lp (- i 1)
            (let lp ((i n) (acc acc))
              (if (= 0 i)
                  acc
                  (lp (- i 1)
                      (let lp ((i n) (acc acc))
                        (if (= 0 i)
                            acc
                            (lp (- i 1) (+ acc 1)))))))))))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (< 0 i)
        (lp (- i 1) (loop1 n acc))
        acc)))

(define (run1 n)
  (map (lambda (k)
         (cons k (loop1 k 0)))
       (iota n)))

(define (run2 n)
  (map (lambda (k)
         (cons k (loop2 k)))
       (iota n)))

;; See comments in "t-nest-07b.scm" for the call of `(loop2 20)'.
(list (loop2 20)
      (run1 50)
      (run2 50))
