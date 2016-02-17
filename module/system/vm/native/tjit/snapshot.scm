;;;; Snapshot and other data to restore frame locals

;;;; Copyright (C) 2014, 2015 Free Software Foundation, Inc.
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
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301 USA
;;;;

;;; Commentary:
;;;
;;; This module contains snapshot related codes and record type used when
;;; recovering frame locals. Snapshot data are used when restoring locals in
;;; frame, so that the VM interpreter can continue from where the native code
;;; has returned.
;;;
;;; Code:

(define-module (system vm native tjit snapshot)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (language cps types)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (system vm debug)
  #:use-module (system vm native debug)
  #:use-module (system vm native tjit error)
  #:use-module (system vm native tjit outline)
  #:use-module (system vm native tjit types)
  #:export ($snapshot
            make-snapshot
            %make-snapshot
            snapshot?
            snapshot-id
            snapshot-sp-offset
            snapshot-fp-offset
            snapshot-nlocals
            snapshot-locals
            snapshot-variables set-snapshot-variables!
            snapshot-code set-snapshot-code!
            snapshot-ip
            snapshot-outline-types
            snapshot-read-indices
            snapshot-live-indices

            snapshot-link?
            snapshot-downrec?
            snapshot-uprec?
            *ip-key-link*
            *ip-key-downrec*
            *ip-key-uprec*

            stack-element
            resolve-stack-element))


;; Record type for snapshot.
(define-record-type $snapshot
  (%make-snapshot id sp-offset fp-offset nlocals locals variables code ip
                  outline-types read-indices live-indices)
  snapshot?

  ;; ID number of this snapshot.
  (id snapshot-id)

  ;; Integer number to shift SP after returning with this snapshot.
  (sp-offset snapshot-sp-offset)

  ;; Integer number to shift vp->fp after returning with this snapshot.
  (fp-offset snapshot-fp-offset)

  ;; Number of locals at the time of snapshot.
  (nlocals snapshot-nlocals)

  ;; Association list of (local . type).
  (locals snapshot-locals)

  ;; Variables used at the time of taking exit.
  (variables snapshot-variables set-snapshot-variables!)

  ;; Native code of bailout with this snapshot.
  (code snapshot-code set-snapshot-code!)

  ;; Bytecode IP of this snapshot to return.
  (ip snapshot-ip)

  ;; Types from outline.
  (outline-types snapshot-outline-types)

  ;; Read indices.
  (read-indices snapshot-read-indices)

  ;; Live indices.
  (live-indices snapshot-live-indices))


;;;
;;; Stack element
;;;

(define (stack-element locals n type)
  (let ((elem (vector-ref locals n)))
    ;; (debug 1 ";;; stack-element ~s ~s => ~s~%" n type elem)
    (cond
     ((eq? 'u64 type)
      (pointer-address elem))
     ((eq? 's64 type)
      (tjitc-error 'stack-element "got s64"))
     ((eq? 'f64 type)
      (tjitc-error 'stack-element "got f64"))
     ((eq? 'scm type)
      (resolve-stack-element elem))
     (else
      (tjitc-error 'stack-element "~s ~s ~s" type n elem)))))

(define (resolve-stack-element ptr)
  (let ((scm? (and (pointer? ptr)
                   (let ((addr (pointer-address ptr)))
                     (and (zero? (logand 1 addr))
                          ;; XXX: Workaround to avoid segmentation fault.
                          (not (and (zero? (modulo addr 8))
                                    (<= addr 800))))))))
    (if scm?
        (pointer->scm ptr)
        (tjitc-error 'resolve-stack-element "non-SCM ~s" ptr))))


;;;
;;; Snapshot
;;;

(define* (make-snapshot id sp-offset fp-offset nlocals locals
                        parent-snapshot write-indices outline ip
                        #:optional (refill-ra-and-dl? #f))
  (let ((car-< (lambda (a b) (< (car a) (car b)))))
    (debug 1 ";;; [make-snapshot] id:~s sp:~s fp:~s nlocals:~s~%"
           id sp-offset fp-offset nlocals)
    (debug 1 ";;; write-indices:~s~%" write-indices)
    (debug 1 ";;; read-indices: ~s~%" (outline-read-indices outline))
    (debug 1 ";;; locals:~a~%"
           (let lp ((i (- (vector-length locals) 1)) (acc '()))
             (if (< i 0)
                 acc
                 (lp (- i 1) (cons (format #f "0x~x"
                                           (pointer-address (vector-ref locals i)))
                                   acc)))))
    (debug 1 ";;; live-indices:~a~%" (outline-live-indices outline))
    (debug 1 ";;; types:~a~%" (sort (outline-types outline) car-<))
    (debug 1 ";;; refill-ra-and-dl?:~a~%" refill-ra-and-dl?)
    (debug 1 "~a"
           (and ((@ (system vm native tjit dump) dump-outline) outline) "")))
  (let lp ((is write-indices) (acc '()))
    (match is
      ((i . is)
       (let ((type (assq-ref (outline-inferred-types outline) i)))
         (lp is (cons `(,i . ,type) acc))))
      (()
       (let ((copied-types (copy-tree (outline-types outline))))
         (call-with-values
             (lambda ()
               (if refill-ra-and-dl?
                   (let* ((acc (cons `(,(+ sp-offset nlocals) . ,&false) acc))
                          (acc (cons `(,(+ sp-offset nlocals 1) . ,&false) acc)))
                     (values (+ fp-offset 2) (+ nlocals 2) acc))
                   (values fp-offset nlocals acc)))
           (lambda (fp-offset nlocals acc)
             (%make-snapshot id sp-offset fp-offset nlocals
                             (reverse! acc) #f #f ip copied-types
                             (outline-read-indices outline)
                             (outline-live-indices outline)))))))))


;;;
;;; IP Keys
;;;

(define *ip-key-link* 0)
(define *ip-key-downrec* 1)
(define *ip-key-uprec* 2)

(define (snapshot-link? snapshot)
  (= (snapshot-ip snapshot) *ip-key-link*))

(define (snapshot-downrec? snapshot)
  (= (snapshot-ip snapshot) *ip-key-downrec*))

(define (snapshot-uprec? snapshot)
  (= (snapshot-ip snapshot) *ip-key-uprec*))
