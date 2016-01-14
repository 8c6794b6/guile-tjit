;; Loop with subr-call. Returned values from subroutine have variable
;; types.

(define (loop alst n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (let ((e (assoc-ref alst n)))
                      (if e (+ acc 1) acc))))))

(let ((alst '((0 . foo) (1 . bar) (2 . 100) (4 . blah)
              (100 . aaa) (101 . bbb) (102 . ccc) (103 . ddd) (104 . eee)
              (200 . ()) (201 . ()) (202 . ()) (203 . ()) (204 . ())
              (300 . a) (301 . b) (302 . c) (303 . d) (304 . e)
              (400 . 1) (401 . 2) (402 . 3) (403 . 4) (404 . 5))))
  (loop alst #e1e4))
