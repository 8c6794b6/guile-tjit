;;;; Calling non-inter-procedure loop multiple times with `map'.  Once
;;;; this code was causing segfault, since beginning IP of side trace
;;;; was same as entry IP of existing root trace.

(define (func-loops n)
  (let loop1 ((i1 1) (result 0))
    (if (> i1 n)
        result
        (let loop2 ((i2 1) (result result))
          (if (= i2 n)
              (loop1 (+ i1 1) result)
              (loop2 (+ i2 1) (+ result 1)))))))

(map (lambda (n)
       (func-loops 18))
     (iota 600))
