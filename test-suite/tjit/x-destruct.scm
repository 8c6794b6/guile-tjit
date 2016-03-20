;;; destruct from racket benchmark

(use-modules (ice-9 time))

(define (my-append! x y)
  (if (null? x)
      y
      (do ((a x b)
           (b (cdr x) (cdr b)))
          ((null? b)
           (set-cdr! a y)
           x))))

;;; DESTRU -- Destructive operation benchmark

(define (destructive n m)
  (let ((l (do ((i 10 (- i 1))
                (a '() (cons '() a)))
               ((= i 0) a))))
    (do ((i n (- i 1)))
        ((= i 0))
      (cond ((null? (car l))
             (do ((l l (cdr l)))
                 ((null? l))
               (or (car l)
                   (set-car! l (cons '() '())))
               (my-append! (car l)
                      (do ((j m (- j 1))
                           (a '() (cons '() a)))
                          ((= j 0) a)))))
            (else
             (do ((l1 l (cdr l1))
                  (l2 (cdr l) (cdr l2)))
                 ((null? l2))
               (set-cdr! (do ((j (quotient (length (car l2)) 2) (- j 1))
                            (a (car l2) (cdr a)))
                           ((zero? j) a)
                         (set-car! a i))
                       (let ((n (quotient (length (car l1)) 2)))
                         (cond ((= n 0) (set-car! l1 '())
                                (car l1))
                               (else
                                (do ((j n (- j 1))
                                     (a (car l1) (cdr a)))
                                    ((= j 1)
                                     (let ((x (cdr a)))
                                            (set-cdr! a '())
                                          x))
                                  (set-car! a i))))))))))))

;;; call:  (destructive 600 50)

(define (run k)
  (let loop ((n 10) (v 0))
    (if (zero? n)
        'v
        (loop (- n 1)
              (destructive k 500)))))

(run 600)
