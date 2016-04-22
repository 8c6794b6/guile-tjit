;;; Loop containing string related procedures, from `string' benchmark.
;;; Iteration count is made enough big to trigger a garbage collection.

(define s "abcdef")

(define (grow)
  (set! s (string-append "123" s "456" s "789"))
  (set! s (string-append
           (substring s (quotient (string-length s) 2) (string-length s))
           (substring s 0 (+ 1 (quotient (string-length s) 2)))))
  s)

(define (trial n)
  (do ((i 0 (+ i 1)))
      ((> (string-length s) n) (string-length s))
    (grow)))

(define (my-try n)
  (do ((i 0 (+ i 1)))
      ((>= i 10) (string-length s))
    (set! s "abcdef")
    (trial n)))

(define (run n)
  (let lp ((n n) (v #f))
    (if (= n 0)
        v
        (lp (- n 1) (my-try 100000)))))

(run 100)
