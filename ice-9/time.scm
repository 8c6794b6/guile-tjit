;;;; 	Copyright (C) 2001 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;;
;;;; As a special exception, the Free Software Foundation gives permission
;;;; for additional uses of the text contained in its release of GUILE.
;;;;
;;;; The exception is that, if you link the GUILE library with other files
;;;; to produce an executable, this does not by itself cause the
;;;; resulting executable to be covered by the GNU General Public License.
;;;; Your use of that executable is in no way restricted on account of
;;;; linking the GUILE library code into it.
;;;;
;;;; This exception does not however invalidate any other reasons why
;;;; the executable file might be covered by the GNU General Public License.
;;;;
;;;; This exception applies only to the code released by the
;;;; Free Software Foundation under the name GUILE.  If you copy
;;;; code from other Free Software Foundation releases into a copy of
;;;; GUILE, as the General Public License permits, the exception does
;;;; not apply to the code that you add in this way.  To avoid misleading
;;;; anyone as to the status of such modified files, you must delete
;;;; this exception notice from them.
;;;;
;;;; If you write modifications of your own for GUILE, it is your choice
;;;; whether to permit this exception to apply to your modifications.
;;;; If you do not wish that, delete this exception notice.
;;;;

;;; Commentary:

;; This module exports a single macro: `time'.
;; Usage: (time exp)
;;
;; Example:
;; guile> (time (sleep 3))
;; clock utime stime cutime cstime gctime
;; 3.01  0.00  0.00   0.00   0.00   0.00
;; 0

;;; Code:

(define-module (ice-9 time)
  :use-module (ice-9 format)
  :export (time))

(define (time-proc proc)
  (let* ((gc-start (gc-run-time))
         (tms-start (times))
         (result (proc))
         (tms-end (times))
         (gc-end (gc-run-time)))
    (define (get proc start end)
      (/ (- (proc end) (proc start)) internal-time-units-per-second))
    (display "clock utime stime cutime cstime gctime\n")
    (format #t "~5,2F ~5,2F ~5,2F ~6,2F ~6,2F ~6,2F\n"
            (get tms:clock tms-start tms-end)
            (get tms:utime tms-start tms-end)
            (get tms:stime tms-start tms-end)
            (get tms:cutime tms-start tms-end)
            (get tms:cstime tms-start tms-end)
            (get identity gc-start gc-end))
    result))

(define-macro (time exp)
  `(,time-proc (lambda () ,exp)))

;;; time.scm ends here
