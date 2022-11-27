#|
SRFI 231: Intervals and Generalized Arrays

Copyright 2022, Joseph Donaldson

Bigloo adaption
the random testing infrastructure required by SRFI 231

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

(module test-random-infrastructure
   (library srfi27 srfi231)
   (import bigloo_compat)
   (export test-random-source
           initial-test-random-source-state
           next-test-random-source-state!
           test-random-integer
           test-random-real
           (random a #!optional b)
           (random-inclusive a #!optional b)
           (random-char)
           (random-sample n #!optional (l 4))
           (random-permutation n)
           (vector-permute v permutation)
           (random-f64vector n)
           (array-display A)
           (random-boolean)
           (random-positive-vector n #!optional (max 5))
           (random-nonnegative-interval #!optional (min 1) (max 6))
           (random-subinterval interval)
           (random-nonempty-interval #!optional (min 0) (max 6))
           (random-interval #!optional (min 0) (max 6))
           use-bignum-intervals
           (random-multi-index interval)))

;;; requires make-list function

;;; Pseudo-random infrastructure

;;; The idea is to have more reproducibility in the random tests.
;;; Our goal is that if this file is run with random-tests=N and then
;;; run with random-tests=M>N, then the parameters of the first N of M tests in each block
;;; will be the same as the parameters of the tests in the run with random-tests=N.

;;; Call next-test-random-source-state! immediately *after* each loop that is
;;; executed random-tests number of times.

(define test-random-source
  (make-random-source))

(define initial-test-random-source-state
  (random-source-state-ref test-random-source))

(define next-test-random-source-state!
  (let ((j 0))
    (lambda ()
      (set! j (+fx j 1))
      (random-source-state-set!
       test-random-source
       initial-test-random-source-state)
      (random-source-pseudo-randomize!
       test-random-source
       0 j))))

(define test-random-integer
  (random-source-make-integers
   test-random-source))

(define test-random-real
  (random-source-make-reals
   test-random-source))


(define (random a #!optional b)
  (if b
      (+ a (test-random-integer (- b a)))
      (test-random-integer a)))

(define (random-inclusive a #!optional b)
  (if b
      (+ a (test-random-integer (- b a -1)))
      (test-random-integer (+ a 1))))

(define (random-char)
  (let ((n (random-inclusive 255)))
    (if (or (<fx n #xd800)
            (<fx #xdfff n))
        (integer->char n)
        (random-char))))

(define (random-sample n #!optional (l 4))
  (list->vector (map (lambda (i)
                       (random 1 l))
                     (iota n))))

(define (random-permutation n)
  (let ((result (make-vector n)))
    ;; fill it
    (do ((i 0 (+fx i 1)))
        ((=fx i n))
      (vector-set! result i i))
    ;; permute it
    (do ((i 0 (+fx i 1)))
        ((=fx i n) result)
      (let* ((index (random i n))
             (temp (vector-ref result index)))
        (vector-set! result index (vector-ref result i))
        (vector-set! result i temp)))))

(define (vector-permute v permutation)
  (let* ((n (vector-length v))
         (result (make-vector n)))
    (do ((i 0 (+ i 1)))
        ((= i n) result)
      (vector-set! result i (vector-ref v (vector-ref permutation i))))))

(define (random-f64vector n)
   (let ((res (make-f64vector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) res)
          (f64vector-set! res i (random-real)))))

;;; define random-interval, random-multi-index

(define (random-multi-index interval)
  (apply values
         (apply map
                random
                (map (lambda (bounds)
                       (bounds interval))
                     (list interval-lower-bounds->list
                           interval-upper-bounds->list)))))

(define use-bignum-intervals #f)


(define (random-interval #!optional (min 0) (max 6))
  ;; a random interval with min <= dimension < max
  ;; positive and negative lower bounds
  (let* ((lower
          (map (lambda (x)
                 (if use-bignum-intervals
                     (random (- (expt #z2 90)) (expt #z2 90))
                     (random -10 10)))
               (iota (random min max))))
         (upper
          (map (lambda (x)
                 (+ (random 0 8) x))
               lower)))
    (make-interval (list->vector lower)
                   (list->vector upper))))

(define (random-nonempty-interval #!optional (min 0) (max 6))
  ;; a random interval with min <= dimension < max
  ;; positive and negative lower bounds
  (let* ((lower
          (map (lambda (x)
                 (if use-bignum-intervals
                     (random (- (expt #z2 90)) (expt #z2 90))
                     (random -10 10)))
               (vector->list (make-vector (random min max)))))
         (upper
          (map (lambda (x)
                 (+ (random 1 8) x))
               lower)))
    (make-interval (list->vector lower)
                   (list->vector upper))))

(define (random-subinterval interval)
  (let* ((lowers (interval-lower-bounds->vector interval))
         (uppers (interval-upper-bounds->vector interval))
         (new-lowers (vector-map random-inclusive lowers uppers))
         (new-uppers (vector-map random-inclusive new-lowers uppers))
         (subinterval (make-interval new-lowers new-uppers)))
    subinterval))


(define (random-nonnegative-interval #!optional (min 1) (max 6))
  ;; a random interval with min <= dimension < max
  ;; positive and negative lower bounds
  (let* ((lower
          (make-vector (random min max) 0))
         (upper
          (vector-map (lambda (x) (random 1 7)) lower)))
    (make-interval lower upper)))

(define (random-positive-vector n #!optional (max 5))
  (vector-map (lambda (x)
                (random 1 max))
              (make-vector n)))

(define (random-boolean)
  (zero? (random 2)))

(define (array-display A)

  (define (display-item x)
    (display x) (display "\t"))

  (newline)
  (case (array-dimension A)
    ((1) (array-for-each display-item A) (newline))
    ((2) (array-for-each (lambda (row)
                           (array-for-each display-item row)
                           (newline))
                         (array-curry A 1)))
    (else
     (test-error "array-display can't handle > 2 dimensions: " A))))