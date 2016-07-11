;;;; Snapshot and other data to restore frame locals

;;;; Copyright (C) 2015, 2016 Free Software Foundation, Inc.
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

(define-module (language trace snapshot)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (system vm native debug)
  #:use-module (language trace error)
  #:use-module (language trace env)
  #:use-module (language trace parameters)
  #:use-module (language trace types)
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
            snapshot-env-types
            snapshot-live-indices
            snapshot-inline-depth
            snapshot-inferred-types

            make-empty-snapshots
            snapshots-ref
            snapshots-set!

            snapshot-link?
            snapshot-downrec?
            snapshot-uprec?
            snapshot-longjmp?
            *ip-key-link*
            *ip-key-downrec*
            *ip-key-uprec*
            *ip-key-longjmp*))


;;;; Snapshot

(define-record-type $snapshot
  (%make-snapshot id sp-offset fp-offset nlocals locals variables code ip
                  live-indices inline-depth inferred-types)
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

  ;; Live indices.
  (live-indices snapshot-live-indices)

  ;; Call depth.
  (inline-depth snapshot-inline-depth)

  ;; Inferred types at the time of snapshot creation.
  (inferred-types snapshot-inferred-types))

(define (make-snapshot id sp-offset fp-offset nlocals write-indices
                       env ip inline-depth refill-ra-dl?)
  (let lp ((is (sort! write-indices <)) (acc '()))
    (match is
      ((i . is)
       (let ((type (assq-ref (env-inferred-types env) i)))
         (lp is (cons (cons i type) acc))))
      (()
       (call-with-values
           (lambda ()
             (if refill-ra-dl?
                 (let* ((acc (acons (+ sp-offset nlocals) &false acc))
                        (acc (acons (+ sp-offset nlocals 1) &false acc)))
                   (values (+ fp-offset 2) (+ nlocals 2) acc))
                 (values fp-offset nlocals acc)))
         (lambda (fp-offset nlocals acc)
           (%make-snapshot id sp-offset fp-offset nlocals (reverse! acc)
                           #f #f ip (env-live-indices env) inline-depth
                           (copy-tree (env-inferred-types env)))))))))


;;;; Snapshots

(define-inlinable (make-empty-snapshots)
  (make-hash-table))

(define-inlinable (snapshots-ref snapshots index)
  (hashq-ref snapshots index))

(define-inlinable (snapshots-set! snapshots index snapshot)
  (hashq-set! snapshots index snapshot))


;;;; IP Keys

(define-syntax-rule (define-ip-key name val)
  (define-syntax name (identifier-syntax val)))

(define-ip-key *ip-key-link* 0)
(define-ip-key *ip-key-downrec* 1)
(define-ip-key *ip-key-uprec* 2)
(define-ip-key *ip-key-longjmp* 3)

(define-inlinable (snapshot-link? snapshot)
  (= (snapshot-ip snapshot) *ip-key-link*))

(define-inlinable (snapshot-downrec? snapshot)
  (= (snapshot-ip snapshot) *ip-key-downrec*))

(define-inlinable (snapshot-uprec? snapshot)
  (= (snapshot-ip snapshot) *ip-key-uprec*))

(define-inlinable (snapshot-longjmp? snapshot)
  (= (snapshot-ip snapshot) *ip-key-longjmp*))
