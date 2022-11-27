#|
SRFI 231: Intervals and Generalized Arrays

Copyright 2022, Joseph Donaldson

Bigloo adaption
various utilities and procedures used for testing
SRFI 231

Copyright 2016, 2018, 2020, 2021, 2022 Bradley J Lucier.
All Rights Reserved.

Permission is hereby granted, free of charge,
to any person obtaining a copy of this software
and associated documentation files (the "Software"),
to deal in the Software without restriction,
including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice
(including the next paragraph) shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
|#
(module test_infra
   (library srfi231)
   (import test-random-infrastructure
           number_utils
           bigloo_compat)
   (include "test_macros.sch")
   (export random-tests
           total-tests
           failed-tests
           (myarray= array1 array2 #!optional (compare equal?))
           (sampled-array-packed? base scales)
           (permuted-array-packed? array permutation)
           (string-trim-right str)
           (in-order < l)
           (foldl op id l)
           (foldr op id l)
           (indices->string  . l)
           (srfi231-time thunk::procedure)
           random-storage-class-and-initializer
           extreme-values-alist
           (multi-index< ind1 ind2)
           (indices-in-proper-order l)
           (palindrome? s)
           (round-and-clip pixel max-grey)
           edge-filter
           sharpen-filter
           (array-convolve source filter)
           test-pgm
           (write-pgm pgm-data file #!optional force-ascii)
           (read-pgm file)
           pgm-pixels
           pgm-greys
           make-pgm
           (array-sum a)
           (array-max a)
           (max-norm a)
           (operator-max-norm a)
           (one-norm a)
           (operator-one-norm a)
           (flatten ls)
           (tensor nested-ls . o)
           (identity-array k . o)))


(define random-tests 100)

(define total-tests 0)

(define failed-tests 0)


(define (srfi231-time thunk::procedure)
   (multiple-value-bind (res rtime stime utime)
      (time (lambda () (thunk)))
      (print "real: " rtime " sys: " stime " user: " utime)))



(define (string-trim-right str)
   (let ((res (string-copy str)))
      (let loop ((i (- (string-length str) 1)))
         (if (>= i 0)
             (if (char-whitespace? (string-ref str i))
                 (loop (- i 1))
                 (string-shrink! res (+ i 1)))))))



(define (in-order < l)
  (or (null? l)
      (null? (cdr l))
      (and (< (car l) (cadr l))
           (in-order < (cdr l)))))

(define (foldl op id l)
  (if (null? l)
      id
      (foldl op (op id (car l)) (cdr l))))

(define (foldr op id l)
  (if (null? l)
      id
      (op (car l) (foldr op id (cdr l)))))

(define (indices->string  . l)
  (apply string-append (number->string (car l))
         (map (lambda (n) (string-append "_" (number->string n))) (cdr l))))


(define (myarray= array1 array2 #!optional (compare equal?))
   (and (interval= (array-domain array1)
           (array-domain array2))
        (array-every compare array1 array2)))

(define random-storage-class-and-initializer
   (let* ((storage-classes
             (vector
                ;; generic
                (list generic-storage-class
                   (lambda args (random-permutation (length args))) 'generic)
                ;; signed integer
                (list s8-storage-class
                   (lambda args  (number->int8 (random (- (expt 2 7)) (- (expt 2 7) 1)))) 's8)
                (list s16-storage-class
                   (lambda args (number->int16 (random (- (expt 2 15)) (- (expt 2 15) 1)))) 's16)
                (list s32-storage-class
                   (lambda args
                      (number->int32 (random (- (expt #z2 31)) (- (expt #z2 31) 1))))
                   's32)
                (list s64-storage-class
                   (lambda args
                      (number->int64 (random (- (expt #z2 63)) (- (expt #z2 63) 1)))) 's64)
                ;; unsigned integer
                (list u1-storage-class
                   (lambda args (random (expt 2 1))) 'u1)
                (list u8-storage-class
                   (lambda args (number->uint8 (random (expt 2 8)))) 'u8)
                (list u16-storage-class
                   (lambda args (number->uint16 (random (expt 2 16)))) 'u16)
                (list u32-storage-class
                   (lambda args
                      (number->uint32 (random (expt #z2 32)))) 'u32)
                (list u64-storage-class
                   (lambda args
                      (number->uint64 (random (expt #z2 64)))) 'u64)
                ;; float
                (list f32-storage-class
                   (lambda args (test-random-real)) 'f32)
                (list f64-storage-class
                   (lambda args (test-random-real)) 'f64)
                ;; char
                (list char-storage-class
                   (lambda args (random-char)) 'char)
                ;; complex-float
                ;(list c64-storage-class
                ;      (lambda args (make-rectangular (test-random-real) (test-random-real))))
                ;(list c128-storage-class
                ;    (lambda args (make-rectangular (test-random-real) (test-random-real))))
                ))
          (n
             (vector-length storage-classes)))
      (lambda ()
         (vector-ref storage-classes (random n)))))

(define %%vector-every
   (case-lambda
      ((pred vec)
       (let loop ((i (-fx (vector-length vec) 1)))
          (or (<fx i 0)
              (and (pred (vector-ref vec i))
                   (loop (-fx i 1))))))
      ((pred vec vec2)
       (let loop ((i (-fx (vector-length vec) 1)))
          (or (<fx i 0)
              (and (pred (vector-ref vec i)
                      (vector-ref vec2 i))
                   (loop (-fx i 1))))))
      ((pred vec vec2 . rest)
       (let ((vecs (cons vec (cons vec2 rest))))
          (let loop ((i (-fx (vector-length vec) 1)))
             (or (<fx i 0)
                 (and (apply pred (map (lambda (vec) (vector-ref vec i)) vecs))
                      (loop (-fx i 1)))))))))


;; permutations

;; A permuted array has elements in order iff all the dimensions with
;; sidelength > 1 are in the same order, or if it's empty.

(define (permuted-array-packed? array permutation)
   (let* ((domain
             (array-domain array))
          (axes-and-limits
             (vector-map list
                (list->vector (iota (vector-length permutation)))
                (interval-lower-bounds->vector domain)
                (interval-upper-bounds->vector domain)))
          (permuted-axes-and-limits
             (vector->list (vector-permute axes-and-limits permutation))))
      (or (interval-empty? domain)
          (in-order (lambda (x y)
                       (< (car x) (car y)))
             (filter (lambda (l)
                        (let ((i (car l))
                              (l (cadr l))
                              (u (caddr l)))
                           (< 1 (- u l))))
                permuted-axes-and-limits)))))


;; a sampled array has elements in order iff after a string of
;; dimensions with side-length 1 at the beginning, all the rest
;; of the dimensions have sidelengths the same as the original

(define (sampled-array-packed? base scales)
   (let* ((domain
             (array-domain base))
          (sampled-base
             (array-sample base scales))
          (scaled-domain
             (array-domain sampled-base))
          (base-sidelengths
             (vector->list
                (vector-map -
                   (interval-upper-bounds->vector domain)
                   (interval-lower-bounds->vector domain))))
          (scaled-sidelengths
             (vector->list
                (vector-map -
                   (interval-upper-bounds->vector scaled-domain)
                   (interval-lower-bounds->vector scaled-domain)))))
      (let loop-1 ((base-lengths   base-sidelengths)
                   (scaled-lengths scaled-sidelengths))
         (or (null? base-lengths)
             (if (= (car scaled-lengths) 1)
                 (loop-1 (cdr base-lengths)
                    (cdr scaled-lengths))
                 (let loop-2 ((base-lengths   base-lengths)
                              (scaled-lengths scaled-lengths))
                    (or (null? base-lengths)
                        (and (= (car base-lengths) (car scaled-lengths))
                             (loop-2 (cdr base-lengths)
                                (cdr scaled-lengths))))))))))



(define extreme-values-alist
   (list
      (list u1-storage-class  0 (- (expt 2 1) 1))
      (list u8-storage-class  #u8:0 (number->uint8 (- (expt 2 8) 1)))
      (list u16-storage-class #u16:0 (number->uint16 (- (expt 2 16) 1)))
      (list u32-storage-class #u32:0 (number->uint32 (- (expt #z2 32) 1)))
      (list u64-storage-class #u64:0 (number->uint64 (- (expt #z2 64) 1)))
      (list s8-storage-class  (number->int8 (- (expt 2 (- 8 1))))
         (number->int8 (- (expt 2 (- 8 1)) 1)))
      (list s16-storage-class (number->int16 (- (expt 2 (- 16 1))))
         (number->int16 (- (expt 2 (- 16 1)) 1)))
      (list s32-storage-class (number->int32 (- (expt #z2 (- 32 1))))
         (number->int32 (- (expt #z2 (- 32 1)) 1)))
      (list s64-storage-class   (number->int64 (- (expt #z2 (- 64 1))))
         (number->int64 (- (expt #z2 (- 64 1)) 1)))
      (list generic-storage-class    'a)
      (list f32-storage-class       1.0)
      (list f64-storage-class       1.0)
      (list char-storage-class (integer->char 255))
      ;(list c64-storage-class  1.0+1.0i)
      ;(list c128-storage-class 1.0+1.0i)
      ))


(define (multi-index< ind1 ind2)
   (and (not (null? ind1))
        (not (null? ind2))
        (or (< (car ind1)
               (car ind2))
            (and (= (car ind1)
                    (car ind2))
                 (multi-index< (cdr ind1)
                    (cdr ind2))))))

(define (indices-in-proper-order l)
   (or (null? l)
       (null? (cdr l))
       (and (multi-index< (car l)
               (cadr l))
            (indices-in-proper-order (cdr l)))))

(define (palindrome? s)
  (let* ((n
          (string-length s))
         (a
          ;; an array accessing the characters of s
          (make-array (make-interval (vector n))
                      (lambda (i)
                        (string-ref s i))))
         (ra
          ;; the characters accessed in reverse order
          (array-reverse a))
         (half-domain
          (make-interval (vector (quotient n 2)))))
    ;; If n is 0 or 1 the following extracted arrays
    ;; are empty.
    (array-every
     char=?
     ;; the first half of s
     (array-extract a half-domain)
     ;; the reversed second half of s
     (array-extract ra half-domain))))


(define make-pgm   cons)
(define pgm-greys  car)
(define pgm-pixels cdr)

(define (read-pgm file)

  (define (read-pgm-object port)
    (skip-white-space port)
    (let ((o (read port)))
      (read-char port) ; to skip the newline or next whitespace
      (if (eof-object? o)
          (test-error "reached end of pgm file")
          o)))

  (define (skip-to-end-of-line port)
    (let loop ((ch (read-char port)))
      (if (not (eq? ch #\newline))
          (loop (read-char port)))))

  (define (white-space? ch)
    (case ch
      ((#\newline #\space #\tab) #t)
      (else #f)))

  (define (skip-white-space port)
    (let ((ch (peek-char port)))
      (cond ((white-space? ch) (read-char port) (skip-white-space port))
            ((eq? ch #\#) (skip-to-end-of-line port)(skip-white-space port))
            (else #f))))

  (call-with-input-file file
     (lambda (port)

      ;; We're going to read text for a while,
      ;; then switch to binary.
      ;; So we need to turn off buffering until
      ;; we switch to binary.

      ;(port-settings-set! port '(buffering: #f))

      (let* ((header (read-pgm-object port))
             (columns (read-pgm-object port))
             (rows (read-pgm-object port))
             (greys (read-pgm-object port)))

        ;; now we switch back to buffering
        ;; to speed things up

        ;(port-settings-set! port '(buffering: #t))

        (make-pgm greys
                  (array-copy
                   (make-array
                    (make-interval (vector rows columns))
                    (cond ((or (eq? header 'p5)                                     ;; pgm binary
                               (eq? header 'P5))
                           (if (< greys 256)
                               (lambda (i j)                                        ;; one byte/pixel
                                 (char->integer (read-char port)))
                               (lambda (i j)                                        ;; two bytes/pixel, little-endian
                                 (let* ((first-byte (char->integer (read-char port)))
                                        (second-byte (char->integer (read-char port))))
                                   (+ (* second-byte 256) first-byte)))))
                          ((or (eq? header 'p2)                                     ;; pgm ascii
                               (eq? header 'P2))
                           (lambda (i j)
                             (read port)))
                          (else
                           (test-error "read-pgm: not a pgm file"))))))))))

(define (write-pgm pgm-data file #!optional force-ascii)
  (call-with-output-file file
    (lambda (port)
      (let* ((greys
              (pgm-greys pgm-data))
             (pgm-array
              (pgm-pixels pgm-data))
             (domain
              (array-domain pgm-array))
             (rows
              (-fx (interval-upper-bound domain 0)
                   (interval-lower-bound domain 0)))
             (columns
              (-fx (interval-upper-bound domain 1)
                   (interval-lower-bound domain 1))))
        (if force-ascii
            (display "P2" port)
            (display "P5" port))
        (newline port)
        (display columns port) (display " " port)
        (display rows port) (newline port)
        (display greys port) (newline port)
        (array-for-each (if force-ascii
                            (let ((next-pixel-in-line 1))
                              (lambda (p)
                                (write p port)
                                (if (zerofx? (bit-and next-pixel-in-line 15))
                                    (begin
                                      (newline port)
                                      (set! next-pixel-in-line 1))
                                    (begin
                                      (display " " port)
                                      (set! next-pixel-in-line (+fx 1 next-pixel-in-line))))))
                            (if (<fx greys 256)
                                (lambda (p)
                                  (write-u8 p port))
                                (lambda (p)
                                  (write-u8 (bit-and p 255) port)
                                  (write-u8 (bit-rsh p 8) port))))
                        pgm-array)))))

(define test-pgm (read-pgm "testfiles/girl.pgm"))

(define (array-convolve source filter)
  (let* ((source-domain
          (array-domain source))
         (S_
          (array-getter source))
         (filter-domain
          (array-domain filter))
         (F_
          (array-getter filter))
         (result-domain
          (interval-dilate
           source-domain
           ;; left bound of an interval is an equality,
           ;; right bound is an inequality, hence the
           ;; the difference in the following two expressions
           (vector-map -
                       (interval-lower-bounds->vector filter-domain))
           (vector-map (lambda (x)
                         (- 1 x))
                       (interval-upper-bounds->vector filter-domain)))))
    (make-array result-domain
                #|
                This was my first attempt at convolve, but the problem is that
                it creates two specialized arrays per pixel, which is a lot of
                overhead (computing an indexer and a setter, for example) for
                not very much computation.
                (lambda (i j)
                  (array-dot-product
                   (array-extract
                    (array-translate source (vector (- i) (- j)))
                    filter-domain)
                   filter))
where

(define (array-dot-product a b)
  (array-fold-left (lambda (x y)
                 (+ x y))
               0
               (array-map
                (lambda (x y)
                  (* x y))
                a b)))

                The times are
(time (let ((greys (pgm-greys test-pgm))) (write-pgm (make-pgm greys (array-map (lambda (p) (round-and-clip p greys)) (array-convolve (pgm-pixels test-pgm) sharpen-filter))) "sharper-test.pgm")))
    0.514201 secs real time
    0.514190 secs cpu time (0.514190 user, 0.000000 system)
    64 collections accounting for 0.144107 secs real time (0.144103 user, 0.000000 system)
    663257736 bytes allocated
    676 minor faults
    no major faults
(time (let* ((greys (pgm-greys test-pgm)) (edge-array (array-copy (array-map abs (array-convolve (pgm-pixels test-pgm) edge-filter)))) (max-pixel (array-fold max 0 edge-array)) (normalizer (/ greys max-pixel))) (write-pgm (make-pgm greys (array-map (lambda (p) (- greys (round-and-clip (* p normalizer) greys))) edge-array)) "edge-test.pgm")))
    0.571130 secs real time
    0.571136 secs cpu time (0.571136 user, 0.000000 system)
    57 collections accounting for 0.154109 secs real time (0.154093 user, 0.000000 system)
    695631496 bytes allocated
    959 minor faults
    no major faults


In the following, where we just package up a little array for each result pixel
that computes the componentwise products when we need them, the times are

(time (let ((greys (pgm-greys test-pgm))) (write-pgm (make-pgm greys (array-map (lambda (p) (round-and-clip p greys)) (array-convolve (pgm-pixels test-pgm) sharpen-filter))) "sharper-test.pgm")))
    0.095921 secs real time
    0.095922 secs cpu time (0.091824 user, 0.004098 system)
    6 collections accounting for 0.014276 secs real time (0.014275 user, 0.000000 system)
    62189720 bytes allocated
    678 minor faults
    no major faults
(time (let* ((greys (pgm-greys test-pgm)) (edge-array (array-copy (array-map abs (array-convolve (pgm-pixels test-pgm) edge-filter)))) (max-pixel (array-fold max 0 edge-array)) (normalizer (inexact (/ greys max-pixel)))) (write-pgm (make-pgm greys (array-map (lambda (p) (- greys (round-and-clip (* p normalizer) greys))) edge-array)) "edge-test.pgm")))
    0.165065 secs real time
    0.165066 secs cpu time (0.165061 user, 0.000005 system)
    13 collections accounting for 0.033885 secs real time (0.033878 user, 0.000000 system)
    154477720 bytes allocated
    966 minor faults
    no major faults
            |#
                (lambda (i j)
                  (array-fold-left
                   (lambda (p q)
                     (+ p q))
                   0
                   (make-array
                    filter-domain
                    (lambda (k l)
                      (* (S_ (+ i k)
                             (+ j l))
                         (F_ k l)))))))))

(define sharpen-filter
  (list->array
   (make-interval '#(-1 -1) '#(2 2))
   '(0 -1  0
    -1  5 -1
     0 -1  0)))

(define edge-filter
  (list->array
   (make-interval '#(-1 -1) '#(2 2))
   '(0 -1  0
    -1  4 -1
     0 -1  0)))

(define (round-and-clip pixel max-grey)
  (max 0 (min (exact (round pixel)) max-grey)))

(define (array-sum a)
  (array-fold-left + 0 a))
(define (array-max a)
  (array-fold-left max -inf.0 a))

(define (max-norm a)
  (array-max (array-map abs a)))
(define (one-norm a)
  (array-sum (array-map abs a)))

(define (operator-max-norm a)
  (max-norm (array-map one-norm (array-curry (array-permute a '#(1 0)) 1))))

(define (operator-one-norm a)
  ;; The "permutation" to apply here is the identity, so we omit it.
  (max-norm (array-map one-norm (array-curry a 1))))

;;; We steal some tests from Alex Shinn's test suite.

(define (flatten ls)
   (if (pair? (car ls))
       (append-map flatten ls)
       ls))

(define (tensor nested-ls . o)
   (let lp ((ls nested-ls) (lens '()))
      (cond
         ((pair? ls) (lp (car ls) (cons (length ls) lens)))
         (else
          (apply list->array
             (make-interval (list->vector (reverse lens)))
             (flatten nested-ls)
              o)))))

(define (identity-array k . o)
   (array-copy (make-array (make-interval (vector k k))
                  (lambda args
                     (if (apply = args)
                         1
                         0)))
      (if (null? o) generic-storage-class (car o))))

