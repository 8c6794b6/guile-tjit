;;; Guile VM core

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (system vm vm)
  #:use-module (system vm frame)
  #:use-module (system vm program)
  #:export (vm? the-vm make-vm vm-version
            vm:ip vm:sp vm:fp vm:last-ip

            vm-load vm-option set-vm-option! vm-version vm-stats
            vms:time vms:clock

            vm-trace-frame
            vm-next-hook vm-apply-hook vm-boot-hook vm-return-hook
            vm-break-hook vm-exit-hook vm-halt-hook vm-enter-hook))

(dynamic-call "scm_init_vm" (dynamic-link "libguile"))

(define (vms:time stat) (vector-ref stat 0))
(define (vms:clock stat) (vector-ref stat 1))

(define (vm-load vm objcode)
  (vm (make-program objcode)))
