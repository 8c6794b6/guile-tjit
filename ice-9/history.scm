;;;; 	Copyright (C) 2000, 2001 Free Software Foundation, Inc.
;;;; 
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;;; 

;;;; A simple value history support

(define-module (ice-9 history))

(process-define-module '((value-history)))

(define (use-value-history x)
  (module-use! (current-module)
	       (resolve-module '(value-history))))

(define save-value-history
  (let ((count 0)
	(history (resolve-module '(value-history))))
    (lambda (v)
      (if (not (unspecified? v))
	  (let* ((c (1+ count))
		 (s (string->symbol (simple-format #f "$~A" c))))
	    (simple-format #t "~A = " s)
	    (module-define! history s v)
	    (set! count c))))))

(add-hook! before-eval-hook use-value-history)
(add-hook! before-print-hook save-value-history)
