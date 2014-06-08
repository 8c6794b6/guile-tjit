;;; Functional name maps
;;; Copyright (C) 2014 Free Software Foundation, Inc.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Some CPS passes need to perform a flow analysis in which every
;;; program point has an associated map over some set of labels or
;;; variables.  The naive way to implement this is with an array of
;;; arrays, but this has N^2 complexity, and it really can hurt us.
;;;
;;; Instead, this module provides a functional map that can share space
;;; between program points, reducing the amortized space complexity of
;;; the representations down to O(n).  Adding entries to the mapping is
;;; O(1), though lookup is O(log n).  When augmented with a dominator
;;; analysis, "meet" operations (intersection or union) can be made in
;;; O(log n) time as well, which results in overall O(n log n)
;;; complexity for flow analysis.  (It would be nice to prove this
;;; properly; I could have some of the details wrong.)
;;;
;;; Namesets are functional hashed lists that map names (unsigned
;;; integers) to values.  Instead of using vhashes from ice-9/vlist.scm,
;;; we copy that code below and specialize it to hold pointerless tuple
;;; values.  The code was originally written by Ludovic Courtès
;;; <ludo@gnu.org>.  See ice-9/vlist.scm for more detailed commentary.
;;;
;;; A nameset backing store starts with the entries of the hash table,
;;; including the chain links, the keys, and the payload.  A bucket list
;;; array follows.
;;;
;;; Although the hash table logic is the same for all namesets, each
;;; nameset kind can have its own payload size and format.  Adding an
;;; entry to a nameset fills in the next unused hash entry slot and
;;; updates the corresponding bucket to point to the newly allocated
;;; hash entry.
;;;
;;; As an example, consider a nameset with two entries, each with its
;;; key K, and with 12 bytes of payload consisting of type T and range
;;; R- and R+.  Assume that H = hashv(K1) = hashv(K2).  The resulting
;;; layout is as follows:
;;;
;;;      byte offset       contents
;;;               0 ,---------------------------.
;;;              +4 | size                      | Header
;;;              +8 | next-free                 |
;;;               8 +---------------------------+
;;;              +0 |     -1, K1, T1, R-1, R+1  |
;;;             +20 | ,->  0, K2, T2, R-2, R+2  | Chain links
;;;             +40 | |                         |
;;;   8 + size * 20 +-|-------------------------+
;;;              +0 | |    -1                   | Hash buckets
;;;              +4 | |    -1                   |
;;;              +8 | '-- 1 <-------------------- H
;;;   8 + size * 24 `---------------------------'
;;;
;;; For the purposes of illustration, the backing store has size 3,
;;; indicating space for three entries.  In practice backing stores will
;;; only have power-of-two sizes.
;;;
;;; Code:

(define-module (language cps nameset)
  #:use-module (rnrs bytevectors)
  #:export (define-nameset-type))

(define-syntax define-nameset-type
  (lambda (x)
    (define (id base suffix)
      (datum->syntax base (symbol-append (syntax->datum base) suffix)))
    (define-syntax-rule (with-ids (stem suffix ...) body)
      (with-syntax ((suffix (id stem 'suffix))
                    ...)
        body))
    (syntax-case x ()
      ((_ (stem val ... #:size size) read write meet)
       (with-ids
        (#'stem -null -lookup -ref -has-entry? -length -add -meet)
        #'(define-values (-null -lookup -ref -has-entry? -length -add -meet)
            (let ((read* read) (write* write) (meet* meet))
              (nameset-type (val ... #:size size)
                            read* write* meet*))))))))

(define-syntax-rule (nameset-type (val ... #:size *value-size*)
                                  value-ref value-set!
                                  value-meet)
  (let* ((*size-offset* 0)
         (*next-free-offset* 4)
         (*header-size* 8)

         ;; int32 link
         (*link-size* 4)
         (*link-offset* 0)
         ;; uint32 key
         (*key-size* 4)
         (*key-offset* 4)

         ;; *value-size* is a parameter.
         (*value-offset* 8)

         (*entry-size* (+ *key-size* *link-size* *value-size*))

         ;; int32 bucket
         (*bucket-size* 4))

    (define (block-size block)
      (bytevector-u32-native-ref block *size-offset*))
    (define (block-next-free block)
      (bytevector-u32-native-ref block *next-free-offset*))

    (define (set-block-next-free! block next-free)
      (bytevector-u32-native-set! block *next-free-offset* next-free))

    (define (block-key-ref block offset)
      (let ((entry (+ *header-size* (* offset *entry-size*))))
        (bytevector-u32-native-ref block (+ entry *key-offset*))))

    (define (block-link-ref block offset)
      (let ((entry (+ *header-size* (* offset *entry-size*))))
        (bytevector-s32-native-ref block (+ entry *link-offset*))))

    (define (block-value-ref block offset)
      (let ((entry (+ *header-size* (* offset *entry-size*))))
        (value-ref block (+ entry *value-offset*))))

    (define (hash-bucket-offset size khash)
      (+ *header-size* (* size *entry-size*) (* khash *bucket-size*)))

    ;; Returns the index of the last entry stored in BLOCK with
    ;; SIZE-modulo hash value KHASH.
    (define (block-hash-bucket-ref block size khash)
      (bytevector-s32-native-ref block (hash-bucket-offset size khash)))

    (define (block-entry-init! block offset key val ...)
      (let* ((size (block-size block))
             (entry (+ *header-size* (* offset *entry-size*)))
             (hash (hashv key size))
             (link (block-hash-bucket-ref block size hash)))
        (bytevector-s32-native-set! block (+ entry *link-offset*) link)
        (bytevector-u32-native-set! block (+ entry *key-offset*) key)
        (value-set! block (+ entry *value-offset*) val ...)
        (bytevector-s32-native-set! block (hash-bucket-offset size hash) offset)))

    (define (make-block size)
      ;; Having the fill value be -1 makes the initial buckets empty.  The
      ;; fill value doesn't affect the other fields.
      (let ((bv (make-bytevector (+ *header-size*
                                    (* size (+ *entry-size* *bucket-size*)))
                                 -1)))
        (bytevector-u32-native-set! bv *size-offset* size)
        (bytevector-u32-native-set! bv *next-free-offset* 0)
        bv))

    ;;;
    ;;; nameset := (OFFSET . HEAD)
    ;;; head := (BLOCK . TAIL)
    ;;; tail := '() | NAMESET
    ;;;
    (define (make-nameset offset head) (cons offset head))
    (define (nameset-offset nameset) (car nameset))
    (define (nameset-head nameset) (cdr nameset))

    (define (make-nameset-head block tail) (cons block tail))
    (define (nameset-head-block head) (car head))
    (define (nameset-head-tail head) (cdr head))

    (define (nameset-block nameset)
      (nameset-head-block (nameset-head nameset)))
    (define (nameset-tail nameset)
      (nameset-head-tail (nameset-head nameset)))

    (define block-null (make-block 0))
    (define nameset-null (make-nameset 0 (make-nameset-head block-null '())))

    (define (nameset-ref nameset index)
      "Return the element at index INDEX in NAMESET."
      (let loop ((index index)
                 (nameset nameset))
        (let ((block (nameset-block nameset))
              (offset (nameset-offset nameset)))
          (if (<= index offset)
              (call-with-values (lambda ()
                                  (block-value-ref block (- offset index)))
                (lambda (val ...)
                  (values (block-key-ref block (- offset index)) val ...)))
              (loop (- index offset 1) (nameset-tail nameset))))))

    (define* (nameset-lookup nameset name #:optional max-depth)
      "Return the index at which NAME is found, or #f if NAME is not present
in NAMESET."
      (let lookup ((nameset nameset) (pos 0))
        (let* ((max-offset (nameset-offset nameset))
               (block (nameset-block nameset))
               (size (block-size block)))
          (and (> size 0)
               (let visit-link ((offset (block-hash-bucket-ref block size
                                                               (hashv name size))))
                 (cond
                  ((and max-depth (>= (+ pos (- max-offset offset)) max-depth))
                   #f)
                  ((< offset 0)
                   (lookup (nameset-tail nameset) (+ pos (1+ max-offset))))
                  ((and (<= offset max-offset)
                        (eqv? name (block-key-ref block offset)))
                   (+ pos (- max-offset offset)))
                  (else
                   (visit-link (block-link-ref block offset)))))))))

    (define-syntax-rule (tmp-id prefix id)
      (datum->syntax prefix
                     (symbol-append (syntax->datum prefix)
                                    '-
                                    (syntax->datum id))))

    (define-syntax &t
      (lambda (x)
        (syntax-case x ()
          ((_ stem id) (tmp-id #'stem #'id)))))

    (define-syntax lambda&t
      (lambda (x)
        (syntax-case x ()
          ((_ stem (id (... ...)) body (... ...))
           (with-syntax (((t (... ...))
                          (map (lambda (x) (tmp-id #'stem x))
                               #'(id (... ...)))))
             #'(lambda (t (... ...)) body (... ...)))))))

    (define (nameset-has-entry? nameset name val ...)
      (cond
       ((nameset-lookup nameset name)
        => (lambda (idx)
             (call-with-values (lambda () (nameset-ref nameset idx))
               (lambda&t
                existing (name val ...)
                (and (eqv? val (&t existing val))
                     ...)))))
       (else #f)))

    (define (nameset-length nameset)
      "Return the length of NAMESET."
      (let loop ((nameset nameset)
                 (len  0))
        (if (eq? nameset nameset-null)
            len
            (loop (nameset-tail nameset)
              (+ len 1 (nameset-offset nameset))))))

    (define (nameset-add nameset name val ...)
      "Return a new nameset, with the additional association of NAME
with VAL..."
      (define (next-nameset nameset)
        (let* ((block (nameset-block nameset))
               (offset (1+ (nameset-offset nameset)))
               (old-size (block-size block)))
          (cond
           ((and (< offset old-size)
                 (= offset (block-next-free block)))
            ;; Fast path: Add the item directly to the block.
            (set-block-next-free! block (1+ offset))
            (values (make-nameset offset (nameset-head nameset))
                    block
                    offset))
           (else
            ;; Slow path: Allocate a new block.
            (let* ((new-size (cond ((zero? old-size) 1)
                                   ((< offset old-size) 1) ;; new head
                                   (else (* 2 old-size))))
                   (block (make-block new-size)))
              (set-block-next-free! block 1)
              (values (make-nameset 0 (make-nameset-head block nameset))
                      block
                      0))))))

      (call-with-values (lambda () (next-nameset nameset))
        (lambda (nameset block offset)
          (block-entry-init! block offset name val ...)
          nameset)))

    (define (nameset-adjoin nameset name val ...)
      "Like nameset-add, but doesn't add a new association if one exists
already."
      (if (nameset-has-entry? nameset name val ...)
          nameset
          (nameset-add nameset name val ...)))

    (define (nameset-shared-tail a b)
      (let lp ((a-offset (nameset-offset a))
               (a-head (nameset-head a))
               (a-len (nameset-length a))
               (b-offset (nameset-offset b))
               (b-head (nameset-head b))
               (b-len (nameset-length b)))
        (cond
         ((< b-len a-len)
          ;; Ensure A is the shorter list.
          (lp b-offset b-head b-len
              a-offset a-head a-len))
         ((< a-len b-len)
          ;; Traverse B until it is not the longer list.
          (if (< (- b-len a-len) (1+ b-offset))
              (lp a-offset a-head a-len
                  (- b-offset (- b-len a-len)) b-head a-len)
              (let ((b (nameset-head-tail b-head)))
                (lp a-offset a-head a-len
                    (nameset-offset b)
                    (nameset-head b)
                    (- b-len (1+ b-offset))))))
         ((< b-offset a-offset)
          ;; Ensure A is the list with the least block offset.
          (lp b-offset b-head b-len
              a-offset a-head a-len))
         ((not (eq? (nameset-head-block a-head) (nameset-head-block b-head)))
          ;; Lists are of equal length but don't have the same block --
          ;; their offsets must differ, and A must have the smaller offset.
          (let ((a (nameset-head-tail a-head)))
            (lp (nameset-offset a)
                (nameset-head a)
                (- a-len (1+ a-offset))
                (- b-offset (1+ a-offset))
                b-head
                (- b-len (1+ a-offset)))))
         (else
          ;; Lists are of equal length and have the same block, and thus
          ;; must have the same offset -- they are the same.  We found
          ;; the shared tail.  Try to preserve eq? identity if possible.
          (cond
           ((and (eqv? (nameset-offset a) a-offset)
                 (eq? (nameset-head a) a-head))
            a)
           ((and (eqv? (nameset-offset b) a-offset)
                 (eq? (nameset-head b) a-head))
            b)
           (else
            (make-nameset a-offset a-head)))))))

    (define* (nameset-meet base new old adjoin)
      (let* ((len (nameset-length base))
             (new-len (- (nameset-length new)
                         (nameset-length (nameset-shared-tail new old)))))
        (let lp ((offset (nameset-offset new))
                 (block (nameset-block new))
                 (tail (nameset-tail new))
                 (visited 0)
                 (base base)
                 (added 0))
          (cond
           ((= visited new-len)
            ;; Done with adjoining new entries.
            base)
           ((< offset 0)
            ;; Reached the end of the current block; keep going with
            ;; the next one.
            (lp (nameset-offset tail)
                (nameset-block tail)
                (nameset-tail tail)
                visited
                base
                added))
           (else
            (let ((name (block-key-ref block offset)))
              (define (recur base*)
                (lp (1- offset) block tail (1+ visited)
                    base* (if (eq? base base*) added (1+ added))))
                  
              (cond
               ((nameset-lookup new name visited)
                ;; This name is shadowed by a more shallow entry.
                (recur base))
               ;; Otherwise meet the entry in A with the entry in B.
               (else
                (call-with-values
                    (lambda ()
                      (block-value-ref block offset))
                  (lambda (val ...)
                    (recur (adjoin base name val ...))))))))))))

    (values nameset-null
            nameset-lookup
            nameset-ref
            nameset-has-entry?
            nameset-length
            nameset-add
            nameset-meet)))
