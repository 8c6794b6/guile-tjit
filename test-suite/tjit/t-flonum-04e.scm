;; Similar to `t-flonum-04', `mandelbrot-main' procedure contains
;; integer to character conversion work with min procedure defined at
;; top-level, max at inner definition.

(define *bailout* 16.0)
(define *max-iterations* 1000)

(define (mandelbrot x y)
  (let ((cr (- y 0.5))
        (ci x)
        (zi 0.0)
        (zr 0.0))
    (let lp ((i 0) (zr zr) (zi zi))
      (if (< *max-iterations* i)
          0
          (let ((zi2 (* zi zi))
                (zr2 (* zr zr)))
            (if (< *bailout* (+ zi2 zr2))
                i
                (lp (+ i 1)
                    (+ (- zr2 zi2) cr)
                    (+ (* 2.0 zr zi) ci))))))))

(define (mymin a b)
    (if (< a b) a b))

(define (mandelbrot-main size)
  (define (mymax a b)
    (if (< a b) b a))
  (let loop ((y (- (- size 1))) (acc '()))
    (if (not (= y (- size 1)))
        (let ((line
               (let loop ((x (- (- size 1))) (output '()))
                 (if (= x (- size 1))
                     (list->string output)
                     (let* ((size (exact->inexact size))
                            (r (mandelbrot (/ x size) (/ y size)))
                            (c (if (zero? r)
                                   #\space
                                   (let ((i (inexact->exact
                                             (truncate (/ r 0.0853)))))
                                     (integer->char (mymax (mymin 126 i) 32))))))
                       (loop (+ x 1) (cons c output)))))))
          (loop (+ y 1) (cons line acc)))
        (reverse! acc))))

(mandelbrot-main 40)
