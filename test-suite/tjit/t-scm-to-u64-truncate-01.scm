;;; Simple loop containing `scm->u64/truncate'.

(define (f x)
  (logand x #xff))

(define (loop n acc)
  (if (= n 0)
      acc
      (loop (- n 1) (+ acc (f n)))))

(loop 1000 1)
