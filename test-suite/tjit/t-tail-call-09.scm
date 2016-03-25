;; Loop with tail-call, taking primitive procedures from arguments.
;; Different procedures are passed to the arguments.

(define (loop lst f g h acc)
  (if (null? lst)
      acc
      (let ((elem (car lst)))
        (loop (cdr lst) f g h
              (cons (list (f elem) (g elem) (h elem)) acc)))))

(let ((lst (apply append (make-list 100 '(1 foo (1 2 3))))))
  (list
   (let lp ((n #e1e3) (acc #f))
     (if (= n 0)
         acc
         (lp (- n 1) (loop lst symbol? pair? number? '()))))
   (let lp ((n #e1e3) (acc #f))
     (if (= n 0)
         acc
         (lp (- n 1) (loop lst number? symbol? pair? '()))))
   (let lp ((n #e1e3) (acc #f))
     (if (= n 0)
         acc
         (lp (- n 1) (loop lst pair? number? symbol? '()))))))
