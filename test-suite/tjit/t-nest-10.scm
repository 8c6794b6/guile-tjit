;;; More inlined procedure in nested loop.
;;;
;;; Calling procedure containing loop twice, adding results. Return
;;; address of the first call to `loop1' is different from the second
;;; call to `loop1'.

(define (loop1 n acc)
    (let lp ((i n) (acc acc))
      (if (< 0 i)
          (lp (- i 1) (+ acc 1))
          acc)))

(define (loop2 n)
  (let lp ((i n) (acc 0))
    (if (< 0 i)
        (lp (- i 1) (+ (loop1 n 0)
                       (loop1 n 0)))
        acc)))

(loop2 100)

;; (begin (display (loop2 12))
;;        (newline))

;; (begin (display (loop2 13))
;;        (newline))

;; (let ((engine (vm-engine)))
;;   (when (eq? engine 'tjit)
;;     (hash-for-each (lambda (k t)
;;                      (dump-fragment t))
;;                    (fragment-table))))

;; (define (loop3 n)
;;   (let lp ((i n) (acc 0))
;;     (if (< 0 i)
;;         (lp (- i 1) (+ acc
;;                        (loop1 n 0)
;;                        (loop1 n 0)))
;;         acc)))

;; (define (loop4 n)
;;   (let lp ((i n) (acc 0))
;;     (if (< 0 i)
;;         (lp (- i 1) (list (loop1 n 0)
;;                           (loop1 n 0)))
;;         acc)))

;; (define (loop5 n)
;;   (let lp ((i n) (v #f) (w #f))
;;     (if (< 0 i)
;;         (lp (- i 1) (loop1 n 0) (loop1 n 0))
;;         (list v w))))

;; (begin (display (loop3 100))
;;        (newline))

;; (begin (display (loop4 100))
;;        (newline))

;; (begin (display (loop5 100))
;;        (newline))
