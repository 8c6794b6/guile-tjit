;;;; "qp.scm" Print finite length representation for any Scheme object.
;;; Copyright (C) 1991, 1992, 1993, 1995 Aubrey Jaffer.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define *qp-width* (output-port-width (current-output-port)))

(define qp:qp
  (let
      ((+ +) (- -) (< <) (= =) (>= >=) (apply apply) (boolean? boolean?)
       (car car) (cdr cdr) (char? char?) (display display) (eq? eq?)
       (for-each for-each) (input-port? input-port?)
       (not not) (null? null?) (number->string number->string)
       (number? number?) (output-port? output-port?) (eof-object? eof-object?)
       (procedure? procedure?) (string-length string-length)
       (string? string?) (substring substring)
       (symbol->string symbol->string) (symbol? symbol?)
       (vector-length vector-length) (vector-ref vector-ref)
       (vector? vector?) (write write) (quotient quotient))
    (letrec
	((num-cdrs
	  (lambda (pairs max-cdrs)
	    (cond
	     ((null? pairs) 0)
	     ((< max-cdrs 1) 1)
	     ((pair? pairs) (+ 1 (num-cdrs (cdr pairs) (- max-cdrs 1))))
	     (else 1))))

	 (l-elt-room
	  (lambda (room pairs)
	    (quotient room (num-cdrs pairs (quotient room 8)))))

	 (qp-pairs
	  (lambda (cdrs room)
	    (cond
	     ((null? cdrs) 0)
	     ((not (pair? cdrs))
	      (display " . ")
	      (+ 3 (qp-obj cdrs (l-elt-room (- room 3) cdrs))))
	     ((< 11 room)
	      (display #\ )
	      ((lambda (used)
		 (+ (qp-pairs (cdr cdrs) (- room used)) used))
	       (+ 1 (qp-obj (car cdrs) (l-elt-room (- room 1) cdrs)))))
	     (else
	      (display " ...") 4))))

	 (v-elt-room
	  (lambda (room vleft)
	    (quotient room (min vleft (quotient room 8)))))

	 (qp-vect
	  (lambda (vect i room)
	    (cond
	     ((= (vector-length vect) i) 0)
	     ((< 11 room)
	      (display #\ )
	      ((lambda (used)
		 (+ (qp-vect vect (+ i 1) (- room used)) used))
	       (+ 1 (qp-obj (vector-ref vect i)
			    (v-elt-room (- room 1)
					(- (vector-length vect) i))))))
	     (else
	      (display " ...") 4))))

	 (qp-string
	  (lambda (str room)
	    (cond
	     ((>= (string-length str) room 3)
	      (display (substring str 0 (- room 3)))
	      (display "...")
	      room)
	     (else
	      (display str)
	      (string-length str)))))

	 (qp-obj
	  (lambda (obj room)
	    (cond
	     ((null? obj) (write obj) 2)
	     ((boolean? obj) (write obj) 2)
	     ((char? obj) (write obj) 8)
	     ((number? obj) (qp-string (number->string obj) room))
	     ((string? obj)
	      (display #\")
	      ((lambda (ans) (display #\") ans)
	       (+ 2 (qp-string obj (- room 2)))))
	     ((symbol? obj) (qp-string (symbol->string obj) room))
	     ((input-port? obj) (display "#[input]") 8)
	     ((output-port? obj) (display "#[output]") 9)
	     ((procedure? obj) (display "#[proc]") 7)
	     ((eof-object? obj) (display "#[eof]") 6)
	     ((vector? obj)
	      (set! room (- room 3))
	      (display "#(")
	      ((lambda (used) (display #\)) (+ used 3))
	       (cond
		((= 0 (vector-length obj)) 0)
		((< room 8) (display "...") 3)
		(else
		 ((lambda (used) (+ (qp-vect obj 1 (- room used)) used))
		  (qp-obj (vector-ref obj 0)
			  (v-elt-room room (vector-length obj))))))))
	     ((pair? obj)
	      (set! room (- room 2))
	      (display #\()
	      ((lambda (used) (display #\)) (+ 2 used))
	       (if (< room 8) (begin (display "...") 3)
		   ((lambda (used)
		      (+ (qp-pairs (cdr obj) (- room used)) used))
		    (qp-obj (car obj) (l-elt-room room obj))))))
	     (else (display "#[unknown]") 10)))))

      (lambda objs
	(cond
	 ((or (not *qp-width*) (= 0 *qp-width*))
	  (for-each (lambda (x) (write x) (display #\ )) objs)
	  (newline))
	 (else
	  (qp-pairs (cdr objs)
		    (- *qp-width*
		       (qp-obj (car objs) (l-elt-room *qp-width* objs))))))))))

(define qp:qpn
  (let ((newline newline) (apply apply) (qp:qp qp:qp))
    (lambda objs (apply qp:qp objs) (newline))))

(define qp:qpr
  (let ((- -) (apply apply) (length length) (list-ref list-ref) (qp:qpn qp:qpn))
    (lambda objs (apply qp:qpn objs)
	    (list-ref objs (- (length objs) 1)))))

(define qp qp:qp)
(define qpn qp:qpn)
(define qpr qp:qpr)
