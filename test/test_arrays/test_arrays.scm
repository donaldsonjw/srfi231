#|
SRFI 231: Intervals and Generalized Arrays

Copyright 2022, Joseph Donaldson

Bigloo adaption
The original test-arrays.scm was broken into multiple
files to permit jvm compilation. Otherwise, the
resulting class files were larger than then permitted
by the jvm specification.


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

(module testarrays
   (include "test_macros.sch")
   (import bigloo_compat
           number_utils
           test_infra
           test-random-infrastructure
           interval-tests
           storage-class-tests
           array-test1
           array-test2)
   (library srfi231 srfi27 srfi39))


;;; A test program for SRFI 231:
;;; Intervals and Generalized Arrays

(interval-error-tests)
   
(interval-result-tests)

(interval?-result-tests)

(interval-dimension-tests)

(interval-lower-bound-error-tests)

(interval-upper-bound-error-tests)

(interval-lower-bounds->list-error-tests)


(interval-upper-bounds->list-error-tests)


(interval-lower-bounds-vector-error-tests)


(interval-upper-bounds->vector-error-tests)



(interval-width-interval-widths-error-tests)



(interval-lower-bound-interval-upper-bound-etc-tests)

(next-test-random-source-state!)


(interval-projections-error-tests)

(interval-projections-result-tests)

(next-test-random-source-state!)
   
(pp "interval-contains-multi-index? error tests")

(pp "interval-volume error tests")

(test (interval-volume #f)
      "interval-volume: The argument is not an interval: ")

(interval-volume-result-tests)

(next-test-random-source-state!)

(interval=-error-tests)

(interval=-result-tests)

(interval-subset?-result-tests)

(next-test-random-source-state!)

(interval-contains-multi-index?-error-tests)

(interval-contains-multi-index?-result-tests)

(interval-for-each-error-tests)

(interval-for-each-result-tests)

(next-test-random-source-state!)

(interval-fold-lef-and-interval-fold-right-error-tests)

;;; We'll rely on tests for array-fold[lr] to test interval-fold[lr]

(interval-dilate-error-tests)

(storage-class-tests)


(array-error-tests)


(array-result-tests)


(array-domain-and-array-getter-error-tests)


(array?-array-domain-and-array-getter-result-tests)


(mutable-array-result-tests)


(array-setter-error-tests)


(mutable-array?-and-array-setter-result-tests)


(array-freeze!-tests)

(new-indexer-result-tests)


(array-body-indexer-storage-class-safe?-error-tests)

(make-specialized-array-error-tests)

(make-specialized-array-from-data-error-tests)

(list*->array-and-vector*->array-tests)

(array->list*-and-array-vector*)

(array-packed?-tests)

(%%move-array-elements-tests)

(extreme-values-test-start)

(array-copy-and-array-copy!-error-tests)

(array-copy-result-tests)

(array-map-error-tests)

(array-every-and-array-any-error-tests)

(array-every-and-array-any-tests)

(array-foldlr-error-tests)

(array-for-each-error-tests)

(array-map-array-fold-right-and-array-for-each-result-tests)

(array-reduce-tests)

(some-array-curry-tests)

(array-decurry-and-array-decurry!-tests)

(specialized-array-share-error-tests)

(specialized-array-share-results-tests)

(interval-and-array-translation-tests)

(interval-and-array-permutation-tests)

(interval-permute-and-array-permute-tests)

(interval-intersect-tests)

(test-interval-scale-and-array-sample)

(test-array-extract-and-array-tile)

(array-reverse-tests)

(array-assign!-tests)

(miscellaneous-error-tests)

(array->list-array-vector-and-list-array-vector-array)


(interval-cartesian-product-and-array-outer-product)

(specialized-array-default-safe? #t)

(array-ref-and-array-set!-tests)


(test-code-from-srfi-document)

(let ()
   (define a
      (array-copy
         (make-array (make-interval '#(5 10))
            list)))
   (define b
      (specialized-array-share
         a
         (make-interval '#(5 5))
         (lambda (i j)
            (values i (+ i j)))))
   ;; Print the \"rows\" of b
   (array-for-each (lambda (row)
                      (pretty-print (array->list row)))
      (array-curry b 1))

   ;; which prints
   ;; ((0 0) (0 1) (0 2) (0 3) (0 4))
   ;; ((1 1) (1 2) (1 3) (1 4) (1 5))
   ;; ((2 2) (2 3) (2 4) (2 5) (2 6))
   ;; ((3 3) (3 4) (3 5) (3 6) (3 7))
   ;; ((4 4) (4 5) (4 6) (4 7) (4 8))
   )



(srfi231-time
   (lambda ()
      (let ((greys (pgm-greys test-pgm)))
       (write-pgm
          (make-pgm
             greys
             (array-map (lambda (p)
                           (round-and-clip p greys))
                (array-convolve
                   (pgm-pixels test-pgm)
                   sharpen-filter)))
          "testfiles/sharper-test.pgm"))))

(srfi231-time
   (lambda ()
      (let* ((greys (pgm-greys test-pgm))
             (edge-array
              (array-copy
                 (array-map
                    abs
                    (array-convolve
                       (pgm-pixels test-pgm)
                       edge-filter))))
             (max-pixel
              (array-fold-left max 0 edge-array))
             (normalizer
                (inexact (/ greys max-pixel))))
         (write-pgm
            (make-pgm
             greys
             (array-map (lambda (p)
                           (- greys
                              (round-and-clip (* p normalizer) greys)))
                edge-array))
            "testfiles/edge-test.pgm"))))


(define m (array-copy (make-array (make-interval '#(0 0) '#(40 30)) (lambda (i j) (exact->inexact (+ i j))))))

(test (operator-max-norm m) 1940.)

(test (operator-one-norm m) 1605.)

(define (all-second-differences image direction)
  (let ((image-domain (array-domain image)))
    (let loop ((i 1)
               (result '()))
      (let ((negative-scaled-direction
             (vector-map (lambda (j) (* -1 j i)) direction))
            (twice-negative-scaled-direction
             (vector-map (lambda (j) (* -2 j i)) direction)))
        (cond ((interval-intersect image-domain
                                    (interval-translate image-domain negative-scaled-direction)
                                    (interval-translate image-domain twice-negative-scaled-direction))
               => (lambda (subdomain)
                    (loop (+ i 1)
                          (cons (array-copy
                                 (array-map (lambda (f_i f_i+d f_i+2d)
                                              (+ f_i+2d
                                                 (* -2. f_i+d)
                                                 f_i))
                                            (array-extract image
                                                           subdomain)
                                            (array-extract (array-translate image
                                                                            negative-scaled-direction)
                                                           subdomain)
                                            (array-extract (array-translate image
                                                                            twice-negative-scaled-direction)
                                                           subdomain)))
                                result))))
              (else
               (reverse result)))))))


(define image (array-copy (make-array (make-interval '#(8 8))
                                      (lambda (i j)
                                        (exact->inexact (+ (* i i) (* j j)))))))

(define (expose difference-images)
  (pretty-print (map (lambda (difference-image)
                       (list (array-domain difference-image)
                             (array->list* difference-image)))
                     difference-images)))


(begin
  (display "\nOriginal image:\n")
  (pretty-print (list (array-domain image)
                      (array->list* image)))
  (display "\nSecond-difference images in the direction $k\\times (1,0)$, $k=1,2,...$, wherever they're defined:\n")
  (expose (all-second-differences image '#(1 0)))
  (display "\nSecond-difference images in the direction $k\\times (1,1)$, $k=1,2,...$, wherever they're defined:\n")
  (expose (all-second-differences image '#(1 1)))
  (display "\nSecond-difference images in the direction $k\\times (1,-1)$, $k=1,2,...$, wherever they're defined:\n")
  (expose (all-second-differences image '#(1 -1)))
  )

(define (make-separable-transform 1D-transform)
  (lambda (a)
    (let ((n (array-dimension a)))
      (do ((d 0 (+fx d 1)))
          ((=fx d n))
        (array-for-each
         1D-transform
         (array-curry (array-permute a (index-last n d)) 1))))))

(define (recursively-apply-transform-and-downsample transform)
  (lambda (a)
    (let ((sample-vector (make-vector (array-dimension a) 2)))
      (define (helper a)
        (if (<fx 1 (interval-upper-bound (array-domain a) 0))
            (begin
              (transform a)
              (helper (array-sample a sample-vector)))))
      (helper a))))

(define (recursively-downsample-and-apply-transform transform)
  (lambda (a)
    (let ((sample-vector (make-vector (array-dimension a) 2)))
      (define (helper a)
        (if (<fx 1 (interval-upper-bound (array-domain a) 0))
            (begin
              (helper (array-sample a sample-vector))
              (transform a))))
      (helper a))))

(define (1D-Haar-loop a)
  (let ((a_ (array-getter a))
        (a! (array-setter a))
        (n (interval-upper-bound (array-domain a) 0)))
    (do ((i 0 (+fx i 2)))
        ((=fx i n))
      (let* ((a_i               (a_ i))
             (a_i+1             (a_ (+fx i 1)))
             (scaled-sum        (/fl (+fl a_i a_i+1) (sqrtfl 2.0)))
             (scaled-difference (/fl (-fl a_i a_i+1) (sqrtfl 2.0))))
        (a! scaled-sum i)
        (a! scaled-difference (+fx i 1))))))

(define 1D-Haar-transform
  (recursively-apply-transform-and-downsample 1D-Haar-loop))

(define 1D-Haar-inverse-transform
  (recursively-downsample-and-apply-transform 1D-Haar-loop))

(define hyperbolic-Haar-transform
  (make-separable-transform 1D-Haar-transform))

(define hyperbolic-Haar-inverse-transform
  (make-separable-transform 1D-Haar-inverse-transform))

(define Haar-transform
  (recursively-apply-transform-and-downsample
   (make-separable-transform 1D-Haar-loop)))

(define Haar-inverse-transform
  (recursively-downsample-and-apply-transform
   (make-separable-transform 1D-Haar-loop)))

(let ((image
       (array-copy
        (make-array (make-interval '#(4 4))
                    (lambda (i j)
                      (case i
                        ((0) 1.)
                        ((1) -1.)
                        (else 0.)))))))
  (display "\nInitial image: \n")
  (pretty-print (list (array-domain image)
                      (array->list* image)))
  (hyperbolic-Haar-transform image)
  (display "\nArray of hyperbolic Haar wavelet coefficients: \n")
  (pretty-print (list (array-domain image)
                      (array->list* image)))
  (hyperbolic-Haar-inverse-transform image)
  (display "\nReconstructed image: \n")
  (pretty-print (list (array-domain image)
                      (array->list* image))))


(let ((image
       (array-copy
        (make-array (make-interval '#(4 4))
                    (lambda (i j)
                      (case i
                        ((0) 1.)
                        ((1) -1.)
                        (else 0.)))))))
  (display "\nInitial image: \n")
  (pretty-print (list (array-domain image)
                      (array->list* image)))
  (Haar-transform image)
  (display "\nArray of Haar wavelet coefficients: \n")
  (pretty-print (list (array-domain image)
                      (array->list* image)))
  (Haar-inverse-transform image)
  (display "\nReconstructed image: \n")
  (pretty-print (list (array-domain image)
                      (array->list* image))))

(define (LU-decomposition A)
  ;; Assumes the domain of A is [0,n)\\times [0,n)
  ;; and that Gaussian elimination can be applied
  ;; without pivoting.
  (let ((n
         (interval-upper-bound (array-domain A) 0))
        (A_
         (array-getter A)))
    (do ((i 0 (+fx i 1)))
        ((= i (-fx n 1)) A)
      (let* ((pivot
              (A_ i i))
             (column/row-domain
              ;; both will be one-dimensional
              (make-interval (vector (+ i 1))
                             (vector n)))
             (column
              ;; the column below the (i,i) entry
              (specialized-array-share A
                                       column/row-domain
                                       (lambda (k)
                                         (values k i))))
             (row
              ;; the row to the right of the (i,i) entry
              (specialized-array-share A
                                       column/row-domain
                                       (lambda (k)
                                         (values i k))))

             ;; the subarray to the right and
             ;;below the (i,i) entry
             (subarray
              (array-extract
               A (make-interval
                  (vector (+fx i 1) (+fx i 1))
                  (vector n         n)))))
        ;; compute multipliers
        (array-assign!
         column
         (array-map (lambda (x)
                      (/ x pivot))
                    column))
        ;; subtract the outer product of i'th
        ;; row and column from the subarray
        (array-assign!
         subarray
         (array-map -
                    subarray
                    (array-outer-product * column row)))))))


(define A_
  ;; A Hilbert matrix
  (array-copy
   (make-array (make-interval '#(4 4))
               (lambda (i j)
                 (/ (+ 1 i j))))))

(display "\nHilbert matrix:\n\n")
(array-display A_)

(LU-decomposition A_)

(display "\nLU decomposition of Hilbert matrix:\n\n")

(array-display A_)

;;; Functions to extract the lower- and upper-triangular
;;; matrices of the LU decomposition of A.

(define (L a)
  (let ((a_ (array-getter a))
        (d  (array-domain a)))
    (make-array
     d
     (lambda (i j)
       (cond ((= i j) 1)        ;; diagonal
             ((> i j) (a_ i j)) ;; below diagonal
             (else 0))))))      ;; above diagonal

(define (U a)
  (let ((a_ (array-getter a))
        (d  (array-domain a)))
    (make-array
     d
     (lambda (i j)
       (cond ((<= i j) (a_ i j)) ;; diagonal and above
             (else 0))))))       ;; below diagonal

(display "\nLower triangular matrix of decomposition of Hilbert matrix:\n\n")
(array-display (L A_))

(display "\nUpper triangular matrix of decomposition of Hilbert matrix:\n\n")
(array-display (U A_))

;;; We'll define a brief, not-very-efficient matrix multiply routine.

(define (matrix-multiply a b)
  (array-inner-product a + * b))

;;; We'll check that the product of the result of LU
;;; decomposition of A is again A.

(define product (matrix-multiply (L A_) (U A_)))

(display "\nProduct of lower and upper triangular matrices ")
(display "of LU decomposition of Hilbert matrix:\n\n")
(array-display product)

(array-display
 (matrix-multiply (list->array (make-interval '#(2 2))
                               '(1 0
                                 0 1))
                  (make-array (make-interval '#(2 4))
                              (lambda (i j)
                                (+ i j)))))

(test (myarray= (matrix-multiply (list->array (make-interval '#(2 2))
                                              '(1 0
                                                   0 1))
                                  (make-array (make-interval '#(2 4))
                                              (lambda (i j)
                                                (+ i j))))
                 (make-array (make-interval '#(2 4))
                             (lambda (i j)
                               (+ i j))))
      #t)

;; Examples from
;; http://microapl.com/apl_help/ch_020_020_880.htm

(define TABLE1
  (list->array
   (make-interval '#(3 2))
   '(1 2
     5 4
     3 0)))

(define TABLE2
  (list->array
   (make-interval '#(2 4))
   '(6 2 3 4
     7 0 1 8)))

(pp (array->list* (array-inner-product TABLE1 + * TABLE2)))

(array-display (array-inner-product TABLE1 + * TABLE2))

;;; Displays
;;; 20 2 5 20
;;; 58 10 19 52
;;; 18 6 9 12

(define X (list*->array 1 '(1 3 5 7)))

(define Y (list*->array 1 '(2 3 6 7)))

(pp (array->list* (array-inner-product X + (lambda (x y) (if (= x y) 1 0)) Y)))

;;; Displays
;;; 2

(define A- (array-copy (make-array (make-interval '#(3 4)) list)))

(array-display A-)

(array-display (array-permute A- '#(1 0)))

(array-display (specialized-array-reshape A- (make-interval '#(4 3))))

(define B (array-sample A- '#(2 1)))

(array-display B)

(test (array-display (specialized-array-reshape B (make-interval '#(8))))
      "specialized-array-reshape: Requested reshaping is impossible: ")

(array-display (specialized-array-reshape B (make-interval '#(8)) #t))

(define interval-flat (make-interval '#(100 100 4)))

(define interval-2x2  (make-interval '#(100 100 2 2)))

(define A\ (array-copy (make-array interval-flat (lambda args (test-random-integer 5)))))

(define B\ (array-copy (make-array interval-flat (lambda args (test-random-integer 5)))))

(define C (array-copy (make-array interval-flat (lambda args 0))))

(define (2x2-matrix-multiply-into! A B C)
  (let ((C! (array-setter C))
        (A_ (array-getter A))
        (B_ (array-getter B)))
    (C! (+ (* (A_ 0 0) (B_ 0 0))
           (* (A_ 0 1) (B_ 1 0)))
        0 0)
    (C! (+ (* (A_ 0 0) (B_ 0 1))
           (* (A_ 0 1) (B_ 1 1)))
        0 1)
    (C! (+ (* (A_ 1 0) (B_ 0 0))
           (* (A_ 1 1) (B_ 1 0)))
        1 0)
    (C! (+ (* (A_ 1 0) (B_ 0 1))
           (* (A_ 1 1) (B_ 1 1)))
        1 1)))

(srfi231-time
   (lambda ()
      (array-for-each 2x2-matrix-multiply-into!
         (array-curry (specialized-array-reshape A\ interval-2x2) 2)
         (array-curry (specialized-array-reshape B\ interval-2x2) 2)
         (array-curry (specialized-array-reshape C interval-2x2) 2))))

(srfi231-time
   (lambda ()
      (array-for-each (lambda (A B C)
                         (array-assign! C (matrix-multiply A B)))
         (array-curry (specialized-array-reshape A\ interval-2x2) 2)
         (array-curry (specialized-array-reshape B\ interval-2x2) 2)
         (array-curry (specialized-array-reshape C interval-2x2) 2))))

(array-display ((array-getter
                 (array-curry
                  (specialized-array-reshape A\ interval-2x2)
                  2))
                0 0))
(array-display ((array-getter
                 (array-curry
                  (specialized-array-reshape B\ interval-2x2)
                  2))
                0 0))
(array-display ((array-getter
                 (array-curry
                  (specialized-array-reshape C interval-2x2)
                  2))
                0 0))

(define 2x2 (make-interval '#(2 2)))

(srfi231-time
   (lambda ()
      (array-for-each (lambda (A B C)
                         (2x2-matrix-multiply-into!
                          (specialized-array-reshape A 2x2)
                          (specialized-array-reshape B 2x2)
                          (specialized-array-reshape C 2x2)))
         (array-curry A\ 1)
         (array-curry B\ 1)
         (array-curry C 1))))

(srfi231-time
   (lambda ()
         (array-for-each (lambda (A B C)
                            (array-assign!
                               (specialized-array-reshape C 2x2)
                               (matrix-multiply
                                  (specialized-array-reshape A 2x2)
                                  (specialized-array-reshape B 2x2))))
            (array-curry A\ 1)
            (array-curry B\ 1)
            (array-curry C 1))))

(cursor-array-inner-product-tests)

(array-append-and-array-append!-tests)

(array-stack-and-array-stack!-tests)

(array-block-and-array-block!-tests)

(define (array-pad-periodically a N)
  ;; Pad a periodically with N rows and columns top and bottom, left and right.
  ;; Returns a generalized array.
  (let* ((domain     (array-domain a))
         (m          (interval-upper-bound domain 0))
         (n          (interval-upper-bound domain 1))
         (a_         (array-getter a)))
    (make-array (interval-dilate domain (vector (- N) (- N)) (vector N N))
                (lambda (i j)
                  (a_ (modulo i m) (modulo j n))))))

(define (neighbor-count a)
  (let* ((big-a      (array-copy (array-pad-periodically a 1)
                                 (array-storage-class a)))
         (domain     (array-domain a))
         (translates (map (lambda (translation)
                            (array-extract (array-translate big-a translation) domain))
                          '(#(1 0) #(0 1) #(-1 0) #(0 -1)
                            #(1 1) #(1 -1) #(-1 1) #(-1 -1)))))
    ;; Returns a generalized array that contains the number
    ;; of 1s in the 8 cells surrounding each cell in the original array.
    (apply array-map + translates)))

(define (game-rules a neighbor-count)
  ;; a is a single cell, neighbor-count is the count of 1s in
  ;; its 8 neighboring cells.
  (if (= a 1)
      (if (or (= neighbor-count 2)
              (= neighbor-count 3))
          1 0)
      ;; (= a 0)
      (if (= neighbor-count 3)
          1 0)))

(define (advance a)
  (array-copy
   (array-map game-rules a (neighbor-count a))
   (array-storage-class a)))

(define glider
  (list*->array
   2
   '((0 0 0 0 0 0 0 0 0 0)
     (0 0 1 0 0 0 0 0 0 0)
     (0 0 0 1 0 0 0 0 0 0)
     (0 1 1 1 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0))
   u1-storage-class))

(define (generations a N)
  (do ((i 0 (+fx i 1))
       (a a  (advance a)))
      ((=fx i N))
    (newline)
    (pretty-print (array->list* a))))

(generations glider 5)

;(pp (reverse %%test-moves)
(for-each display (list "Failed " failed-tests " out of " total-tests " total tests.\n"))

