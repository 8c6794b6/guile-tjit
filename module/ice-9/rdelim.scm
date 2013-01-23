;;; installed-scm-file

;;;; Copyright (C) 1997, 1999, 2000, 2001, 2006, 2010, 2013 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;; 


;;; This is the Scheme part of the module for delimited I/O.  It's
;;; similar to (scsh rdelim) but somewhat incompatible.

(define-module (ice-9 rdelim)
  #:export (read-line
            read-line!
            read-delimited
            read-delimited!
            read-string
            read-string!
            %read-delimited!
            %read-line
            write-line))


(%init-rdelim-builtins)

(define* (read-line! string #:optional (port current-input-port))
  ;; corresponds to SCM_LINE_INCREMENTORS in libguile.
  (define scm-line-incrementors "\n")
  (let* ((rv (%read-delimited! scm-line-incrementors
                               string
                               #t
                               port))
         (terminator (car rv))
         (nchars (cdr rv)))
    (cond ((and (= nchars 0)
                (eof-object? terminator))
           terminator)
          ((not terminator) #f)
          (else nchars))))

(define* (read-delimited! delims buf #:optional
                          (port (current-input-port)) (handle-delim 'trim)
                          (start 0) (end (string-length buf)))
  (let* ((rv (%read-delimited! delims
                               buf
                               (not (eq? handle-delim 'peek))
                               port
                               start
                               end))
         (terminator (car rv))
         (nchars (cdr rv)))
    (cond ((or (not terminator)         ; buffer filled
               (eof-object? terminator))
           (if (zero? nchars)
               (if (eq? handle-delim 'split)
                   (cons terminator terminator)
                   terminator)
               (if (eq? handle-delim 'split)
                   (cons nchars terminator)
                   nchars)))
          (else
           (case handle-delim
             ((trim peek) nchars)
             ((concat) (string-set! buf (+ nchars start) terminator)
              (+ nchars 1))
             ((split) (cons nchars terminator))
             (else (error "unexpected handle-delim value: " 
                          handle-delim)))))))
  
(define* (read-delimited delims #:optional (port (current-input-port))
                         (handle-delim 'trim))
  (let loop ((substrings '())
             (total-chars 0)
             (buf-size 100))		; doubled each time through.
    (let* ((buf (make-string buf-size))
           (rv (%read-delimited! delims
                                 buf
                                 (not (eq? handle-delim 'peek))
                                 port))
           (terminator (car rv))
           (nchars (cdr rv))
           (new-total (+ total-chars nchars)))
      (cond
       ((not terminator)
        ;; buffer filled.
        (loop (cons (substring buf 0 nchars) substrings)
              new-total
              (* buf-size 2)))
       ((and (eof-object? terminator) (zero? new-total))
        (if (eq? handle-delim 'split)
            (cons terminator terminator)
            terminator))
       (else
        (let ((joined
               (string-concatenate-reverse
                (cons (substring buf 0 nchars) substrings))))
          (case handle-delim
            ((concat)
             (if (eof-object? terminator)
                 joined
                 (string-append joined (string terminator))))
            ((trim peek) joined)
            ((split) (cons joined terminator))
            (else (error "unexpected handle-delim value: "
                         handle-delim)))))))))

(define-syntax-rule (check-arg exp message arg ...)
  (unless exp
    (error message arg ...)))

(define (index? n)
  (and (integer? n) (exact? n) (>= n 0)))

(define* (read-string! buf #:optional
                       (port (current-input-port))
                       (start 0) (end (string-length buf)))
  "Read all of the characters out of PORT and write them to BUF.
Returns the number of characters read.

This function only reads out characters from PORT if it will be able to
write them to BUF.  That is to say, if BUF is smaller than the number of
available characters, then BUF will be filled, and characters will be
left in the port."
  (check-arg (string? buf) "not a string" buf)
  (check-arg (index? start) "bad index" start)
  (check-arg (index? end) "bad index" end)
  (check-arg (<= start end) "start beyond end" start end)
  (check-arg (<= end (string-length buf)) "end beyond string length" end)
  (let lp ((n start))
    (if (< n end)
        (let ((c (read-char port)))
          (if (eof-object? c)
              (- n start)
              (begin
                (string-set! buf n c)
                (lp (1+ n)))))
        (- n start))))

(define* (read-string #:optional (port (current-input-port)) (count #f))
  "Read all of the characters out of PORT and return them as a string.
If the COUNT argument is present, treat it as a limit to the number of
characters to read.  By default, there is no limit."
  (check-arg (or (not count) (index? count)) "bad count" count)
  (let loop ((substrings '())
             (total-chars 0)
             (buf-size 100))		; doubled each time through.
    (let* ((buf (make-string (if count
                                 (min buf-size (- count total-chars))
                                 buf-size)))
           (nchars (read-string! buf port))
           (new-total (+ total-chars nchars)))
      (cond
       ((= nchars buf-size)
        ;; buffer filled.
        (loop (cons buf substrings) new-total (* buf-size 2)))
       (else
        (string-concatenate-reverse
         (cons (substring buf 0 nchars) substrings)))))))

;;; read-line [PORT [HANDLE-DELIM]] reads a newline-terminated string
;;; from PORT.  The return value depends on the value of HANDLE-DELIM,
;;; which may be one of the symbols `trim', `concat', `peek' and
;;; `split'.  If it is `trim' (the default), the trailing newline is
;;; removed and the string is returned.  If `concat', the string is
;;; returned with the trailing newline intact.  If `peek', the newline
;;; is left in the input port buffer and the string is returned.  If
;;; `split', the newline is split from the string and read-line
;;; returns a pair consisting of the truncated string and the newline.

(define* (read-line #:optional (port (current-input-port))
                    (handle-delim 'trim))
  (let* ((line/delim	(%read-line port))
	 (line		(car line/delim))
	 (delim		(cdr line/delim)))
    (case handle-delim
      ((trim) line)
      ((split) line/delim)
      ((concat) (if (and (string? line) (char? delim))
		    (string-append line (string delim))
		    line))
      ((peek) (if (char? delim)
		  (unread-char delim port))
	      line)
      (else
       (error "unexpected handle-delim value: " handle-delim)))))
