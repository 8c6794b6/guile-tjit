;;; More inlined procedures in nested loop.
;;;
;;; This test is similar to t-nest-07, but calls `run2' without calling
;;; `loop2'.  Test to see that trace code works with values greater than
;;; hot exit count after calling with value less than hot exit count.

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

(define (run2 n)
  (map (lambda (k)
         (cons k (loop2 k)))
       (iota n)))

(define (run-loop2 n)
  (display "(loop2 ")
  (display n)
  (display ") ===> ")
  (display (loop2 n))
  (newline))

;;; Following sequence of `run-loop2's won't work when
;;; `tjit-max-retries' were set to 1. `(run-loop2 4)' is showing 236
;;; instead of 256. By default, `tjit-max-retries' is not 1.

;; (run-loop2 2)
;; (run-loop2 3)
;; (run-loop2 4)

;;; Following works with `tjit-max-retries' set to 1.

;; (run-loop2 3)
;; (run-loop2 4)
;; (run-loop2 5)

(run2 50)
