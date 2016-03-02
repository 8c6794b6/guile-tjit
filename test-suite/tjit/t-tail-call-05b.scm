;;; Same procedures as defined in `t-tail-call-05', but passing
;;; different argument to record different trace.

;; XXX: Trace will be marked as down recursion when `f2' called with
;; argument `13'. The hot loop then will marked as failed, since
;; non-tail recursive down recursion is not yet implemented. To make the
;; loop hot, increasing max number of retries to 5 in this test.
(use-modules (system vm native tjit parameters))
(set-tjit-max-retries! 5)

(define (f1 n acc)
  (if (= n 0)
      acc
      (f1 (- n 1) (+ acc n))))

(define (f2 n)
  (let lp ((n n) (acc 0))
    (if (= n 0)
        acc
        (lp (- n 1) (f1 n (f1 n acc))))))

(f2 13)
