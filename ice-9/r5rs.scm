;;;; 	Copyright (C) 2000 Free Software Foundation, Inc.
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

;;;; R5RS bindings

(define-module (ice-9 r5rs))

(module-use! %module-public-interface (resolve-interface '(ice-9 safe-r5rs)))

(export scheme-report-environment
	;;transcript-on
	;;transcript-off
	)

(re-export interaction-environment

	   call-with-input-file call-with-output-file
	   with-input-from-file with-output-to-file
	   open-input-file open-output-file
	   close-input-port close-output-port

	   load
	   )

(define scheme-report-interface %module-public-interface)

(define (scheme-report-environment n)
  (if (not (= n 5))
      (scm-error 'misc-error 'scheme-report-environment
		 "~A is not a valid version"
		 (list n)
		 '()))
  scheme-report-interface)
