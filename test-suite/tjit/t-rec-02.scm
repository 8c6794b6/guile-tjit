;; Another simple recursion, has different initial nlocals than
;; t-rec-01.scm.

(define (sum2 n inc)
  (if (= n 0)
      0
      (+ inc (sum2 (- n 1) inc))))

(list (sum2 1000 2)
      (sum2 1000 3)
      (sum2 1000 4))
