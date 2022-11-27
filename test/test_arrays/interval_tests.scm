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
(module interval-tests
   (include "test_macros.sch")
   (library srfi231)
   (import bigloo_compat
           test_infra
           test-random-infrastructure)
   
   (export (interval-error-tests)
           (interval-result-tests)
           (interval?-result-tests)
           (interval-lower-bound-error-tests)
           (interval-upper-bound-error-tests)
           (interval-dimension-tests)
           (interval-lower-bounds->list-error-tests)
           (interval-upper-bounds->list-error-tests)
           (interval-lower-bounds-vector-error-tests)
           (interval-upper-bounds->vector-error-tests)
           (interval-width-interval-widths-error-tests)
           (interval-lower-bound-interval-upper-bound-etc-tests)
           (interval-projections-error-tests)
           (interval-projections-result-tests)
           (interval-volume-result-tests)
           (interval=-error-tests)
           (interval=-result-tests)
           (interval-subset?-result-tests)
           (interval-contains-multi-index?-error-tests)
           (interval-contains-multi-index?-result-tests)
           (interval-for-each-error-tests)
           (interval-for-each-result-tests)
           (interval-fold-lef-and-interval-fold-right-error-tests)
           (interval-dilate-error-tests)))

 (define (interval-error-tests)
   (pp "Interval error tests")
   
   (test (make-interval 1 '#(3 4))
      "make-interval: The first argument is not a vector of exact integers: ")
   
   (test (make-interval '#(1 1)  3)
    "make-interval: The second argument is not a vector of exact integers: ")
   
   (test (make-interval '#(1 1)  '#(3))
      "make-interval: The first and second arguments are not the same length: ")
   
   (test (interval-volume (make-interval '#()  '#()))
      1)
   
   (test (make-interval '#(1.)  '#(1))
      "make-interval: The first argument is not a vector of exact integers: ")
   
   (test (make-interval '#(1 #f)  '#(1 2))
      "make-interval: The first argument is not a vector of exact integers: ")
   
   (test (make-interval '#(1)  '#(1.))
      "make-interval: The second argument is not a vector of exact integers: ")
   
   (test (make-interval '#(1 1)  '#(1 #f))
      "make-interval: The second argument is not a vector of exact integers: ")
   
   (test (interval-volume (make-interval '#(1)  '#(1)))
      0)
   
   (test (interval-volume (make-interval '#(1 2 3)  '#(4 2 6)))
      0)
   
   (test (make-interval 1)
      "make-interval: The argument is not a vector of nonnegative exact integers: ")
   
   (test (interval-volume (make-interval '#()))
      1)
   
   (test (make-interval '#(1.))
      "make-interval: The argument is not a vector of nonnegative exact integers: ")

   (test (make-interval '#(-1))
      "make-interval: The argument is not a vector of nonnegative exact integers: "))


(define (interval-result-tests)
   (pp "interval result tests")
   
   (test (make-interval '#(11111)  '#(11112))
      (make-interval '#(11111) '#(11112)))
   
   (test (make-interval '#(1 2 3)  '#(4 5 6))
      (make-interval '#(1 2 3) '#(4 5 6))))


(define (interval?-result-tests)
   (pp "interval? result tests")

  (test (interval? #t)
     #f)
  
  (test (interval? (make-interval '#(1 2 3) '#(4 5 6)))
     #t))

(define (interval-dimension-tests)
   (pp "interval-dimension error tests")
    
    (test (interval-dimension 1)
       "interval-dimension: The argument is not an interval: ")
    
    (pp "interval-dimension result tests")
    
    (test (interval-dimension (make-interval '#(1 2 3) '#(4 5 6)))
       3))

(define (interval-lower-bound-error-tests)
   (pp "interval-lower-bound error tests")
   
   (test (interval-lower-bound 1 0)
      "interval-lower-bound: The first argument is not an interval: ")
   
   (test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) #f)
      "interval-lower-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")
   
    (test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) 1.)
       "interval-lower-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")
    
    (test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) -1)
       "interval-lower-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")
    
    (test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) 3)
       "interval-lower-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

    (test (interval-lower-bound (make-interval '#(1 2 3) '#(4 5 6)) 4)
       "interval-lower-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")
    
    (test (interval-lower-bound (make-interval '#()) 0)
       "interval-lower-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

    )


(define (interval-upper-bound-error-tests)
   (pp "interval-upper-bound error tests")
   
   (test (interval-upper-bound 1 0)
      "interval-upper-bound: The first argument is not an interval: ")
   
   (test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) #f)
      "interval-upper-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")
   
   (test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) 1.)
      "interval-upper-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")
   
   (test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) -1)
      "interval-upper-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")
   
   (test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) 3)
      "interval-upper-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")
   
   (test (interval-upper-bound (make-interval '#(1 2 3) '#(4 5 6)) 4)
      "interval-upper-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): ")

   (test (interval-upper-bound (make-interval '#()) 0)
      "interval-upper-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): "))


(define (interval-lower-bounds->list-error-tests)
   (pp "interval-lower-bounds->list error tests")

    (test (interval-lower-bounds->list 1)
       "interval-lower-bounds->list: The argument is not an interval: ")

    (test (interval-lower-bounds->list (make-interval '#()))
       '()))

(define (interval-upper-bounds->list-error-tests)
   (pp "interval-upper-bounds->list error tests")
   
    (test (interval-upper-bounds->list #f)
       "interval-upper-bounds->list: The argument is not an interval: ")
    
    (test (interval-upper-bounds->list (make-interval '#()))
       '()))

(define (interval-lower-bounds-vector-error-tests)
   (pp "interval-lower-bounds->vector error tests")
   
   (test (interval-lower-bounds->vector 1)
      "interval-lower-bounds->vector: The argument is not an interval: ")
   
   (test (interval-lower-bounds->vector (make-interval '#()))
      '#()))

(define (interval-upper-bounds->vector-error-tests)
   (pp "interval-upper-bounds->vector error tests")

    (test (interval-upper-bounds->vector #f)
       "interval-upper-bounds->vector: The argument is not an interval: ")

    (test (interval-upper-bounds->vector (make-interval '#()))
       '#()))

(define (interval-width-interval-widths-error-tests)
   (pp "interval-width, interval-widths error tests")
    (test (interval-width 1 0)
       "interval-width: The first argument is not an interval: ")

    (test (interval-width (make-interval '#(1 2 3) '#(4 5 6)) #f)
       "interval-width: The second argument is not an exact integer between 0 (inclusive) and the dimension of the first argument (exclusive): ")

    (test (interval-width (make-interval '#(1 2 3) '#(4 5 6)) 1.)
       "interval-width: The second argument is not an exact integer between 0 (inclusive) and the dimension of the first argument (exclusive): ")

    (test (interval-width (make-interval '#(1 2 3) '#(4 5 6)) -1)
       "interval-width: The second argument is not an exact integer between 0 (inclusive) and the dimension of the first argument (exclusive): ")

    (test (interval-width (make-interval '#(1 2 3) '#(4 5 6)) 3)
       "interval-width: The second argument is not an exact integer between 0 (inclusive) and the dimension of the first argument (exclusive): ")

    (test (interval-width (make-interval '#(1 2 3) '#(4 5 6)) 4)
       "interval-width: The second argument is not an exact integer between 0 (inclusive) and the dimension of the first argument (exclusive): ")

    (test (interval-widths 1)
       "interval-widths: The argument is not an interval: ")

    (test (interval-widths (make-interval '#()))
       '#())

    (test (interval-widths (make-interval '#(1 0)))
       '#(1 0)))


(define (interval-lower-bound-interval-upper-bound-etc-tests)
   (pp "interval-lower-bound, interval-upper-bound, interval-lower-bounds->list, interval-upper-bounds->list,")
   (pp "interval-lower-bounds->vector, interval-upper-bounds->vector, interval-width, interval-widths result tests")

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
              (upper (map (lambda (x) (+ (random 11) x)) lower)))
          (let ((interval (make-interval (list->vector lower)
                             (list->vector upper)))
                (offset (random (length lower))))
             (test (interval-lower-bound interval offset)
                (list-ref lower offset))
             (test (interval-upper-bound interval offset)
                (list-ref upper offset))
             (test (interval-lower-bounds->list interval)
                lower)
             (test (interval-upper-bounds->list interval)
                upper)
             (test (interval-lower-bounds->vector interval)
                (list->vector lower))
             (test (interval-upper-bounds->vector interval)
                (list->vector upper))
             (test (interval-width interval offset)
                (- (list-ref upper offset)
                   (list-ref lower offset)))
             )))

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 1 11)))))
              (upper (map (lambda (x) (+ (random 11) x)) lower)))
          (let ((interval (make-interval (list->vector lower)
                             (list->vector upper))))
             (test (interval-widths interval)
                (vector-map -
                   (interval-upper-bounds->vector interval)
                   (interval-lower-bounds->vector interval)))))))

(define (interval-projections-error-tests)
   (pp "interval-projections error tests")

    (test (interval-projections 1 1)
       "interval-projections: The first argument is not an interval: ")

    (test (interval-projections (make-interval '#(0) '#(1)) #t)
       "interval-projections: The second argument is not an exact integer between 0 and the dimension of the first argument (inclusive): ")


    (test (interval-projections (make-interval '#(0 0) '#(1 1)) 0.5)
       "interval-projections: The second argument is not an exact integer between 0 and the dimension of the first argument (inclusive): ")

    (test (interval-projections (make-interval '#(0 0) '#(1 1)) 1.)
       "interval-projections: The second argument is not an exact integer between 0 and the dimension of the first argument (inclusive): ")

    (test-multiple-values
       (interval-projections (make-interval '#(0 0) '#(1 1)) 0)
       (list (make-interval '#(1 1))
          (make-interval '#()) ))

    (test-multiple-values
       (interval-projections (make-interval '#(0 0) '#(1 1)) 2)
       (list (make-interval '#())
          (make-interval '#(1 1)))))

(define (interval-projections-result-tests)
   (pp "interval-projections result tests")

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((lower (map (lambda (x) (random 10)) (vector->list (make-vector (random 3 11)))))
              (upper (map (lambda (x) (+ (random 1 11) x)) lower))
              (left-dimension (random 1 (- (length lower) 1)))
              (right-dimension (- (length lower) left-dimension)))
          (test-multiple-values
             (interval-projections (make-interval (list->vector lower)
                                      (list->vector upper))
                right-dimension)
             (list (make-interval (list->vector (take lower left-dimension))
                      (list->vector (take upper left-dimension)))
                (make-interval (list->vector (drop lower left-dimension))
                   (list->vector (drop upper left-dimension)))))))

)

(define (interval-volume-result-tests)
   (pp "interval-volume result tests")

    (do ((i 0 (+ i 1)))
        ((= i random-tests))
        (let* ((lower (map (lambda (x) (random 6)) (vector->list (make-vector (random 6)))))
               (upper (map (lambda (x) (+ (random 6) x)) lower)))
           (test (interval-volume (make-interval (list->vector lower)
                                     (list->vector upper)))
              (apply * (map - upper lower))))))

(define (interval=-error-tests)
   (pp "interval= error tests")

 (test (interval= #f (make-interval '#(1 2 3) '#(4 5 6)))
    "interval=: Not all arguments are intervals: ")

 (test (interval= (make-interval '#(1 2 3) '#(4 5 6)) #f)
    "interval=: Not all arguments are intervals: "))

(define (interval=-result-tests)
   (pp "interval= result tests")

 (do ((i 0 (+ i 1)))
     ((= i random-tests))
     (let* ((lower1 (map (lambda (x) (random 2)) (vector->list (make-vector (random 4)))))
            (upper1 (map (lambda (x) (+ (random 3) x)) lower1))
            (lower2 (map (lambda (x) (random 2)) lower1))
            (upper2 (map (lambda (x) (+ (random 3) x)) lower2)))
        (test (interval= (make-interval (list->vector lower1)
                            (list->vector upper1))
                 (make-interval (list->vector lower2)
                    (list->vector upper2)))
           (and (equal? lower1 lower2)                              ;; the probability of this happening is about 1/16
                (equal? upper1 upper2)))))

 (next-test-random-source-state!)

 ; (pp "interval-subset? error tests")

 (test (interval-subset? #f (make-interval '#(1 2 3) '#(4 5 6)))
    "interval-subset?: Not all arguments are intervals: ")

 (test (interval-subset? (make-interval '#(1 2 3) '#(4 5 6)) #f)
    "interval-subset?: Not all arguments are intervals: ")

 (test (interval-subset? (make-interval '#(1) '#(2))
          (make-interval '#(0 0) '#(1 2)))
    "interval-subset?: The arguments do not have the same dimension: "))

(define (interval-subset?-result-tests)
   (pp "interval-subset? result tests")

    (do ((i 0 (+ i 1)))
        ((= i random-tests))
        (let* ((lower1 (map (lambda (x) (random 2)) (vector->list (make-vector (random 6)))))
               (upper1 (map (lambda (x) (+ (random 3) x)) lower1))
               (lower2 (map (lambda (x) (random 2)) lower1))
               (upper2 (map (lambda (x) (+ (random 3) x)) lower2)))
           (test (interval-subset? (make-interval (list->vector lower1)
                                      (list->vector upper1))
                    (make-interval (list->vector lower2)
                       (list->vector upper2)))
              (and (every (lambda (x) (>= (car x) (cdr x))) (map cons lower1 lower2))
                   (every (lambda (x) (<= (car x) (cdr x))) (map cons upper1 upper2))))))

    )

(define (interval-contains-multi-index?-error-tests)
   (pp "interval-contains-multi-index?  error tests")

    (test (interval-contains-multi-index? 1 1)
       "interval-contains-multi-index?: The first argument is not an interval: ")

    (test (interval-contains-multi-index? (make-interval '#(1 2 3) '#(4 5 6)) 1)
       "interval-contains-multi-index?: The dimension of the first argument (an interval) does not match number of indices: ")

    (test (interval-contains-multi-index? (make-interval '#(1 2 3) '#(4 5 6)) 1 0.5 0.1)
       "interval-contains-multi-index?: At least one multi-index component is not an exact integer: "))

(define (interval-contains-multi-index?-result-tests)
   (pp "interval-contains-multi-index?  result tests")

    (let ((interval   (make-interval '#(1 2 3) '#(4 5 6)))
          (interval-2 (make-interval '#(10 11 12) '#(13 14 15))))
       (if (not (array-fold-left (lambda (result x)
                                    (and result (apply interval-contains-multi-index? interval x)))
                   #t
                   (make-array interval list)))
           (test-error "these should all be true"))
       (if (not (array-fold-left (lambda (result x)
                                    (and result (not (apply interval-contains-multi-index? interval x))))
                   #t
                   (make-array interval-2 list)))
           (test-error "these should all be false"))))

(define (interval-for-each-error-tests)
   (pp "interval-for-each error tests")

    (test (interval-for-each (lambda (x) x) 1)
       "interval-for-each: The second argument is not a interval: ")

    (test (interval-for-each 1 (make-interval '#(3) '#(4)))
       "interval-for-each: The first argument is not a procedure: "))

(define (interval-for-each-result-tests)
   (pp "interval-for-each result tests")

 (do ((i 0 (+ i 1)))
     ((= i random-tests))
     (let* ((lower (map (lambda (x) (random 10))
                      (vector->list (make-vector (random 1 7)))))
            (upper (map (lambda (x) (+ (random 1 4) x))
                      lower)))
        (let ((result '()))

           (define (f . args)
              (set! result (cons args result)))

           (test (let ()
                    (interval-for-each f
                       (make-interval (list->vector lower)
                          (list->vector upper)))
                    result)
              (reverse (all-elements lower upper)))))))

(define (interval-fold-lef-and-interval-fold-right-error-tests)
   (pp "interval-fold-left and interval-fold-right error tests")

    (test (interval-fold-left 1 2 3 4)
       "interval-fold-left: The fourth argument is not an interval: ")

    (test (interval-fold-left 1 2 3 (make-interval '#(2 2)))
       "interval-fold-left: The second argument is not a procedure: ")

    (test (interval-fold-left 1 values 3 (make-interval '#(2 2)))
       "interval-fold-left: The first argument is not a procedure: ")

    (test (interval-fold-right 1 2 3 4)
       "interval-fold-right: The fourth argument is not an interval: ")

    (test (interval-fold-right 1 2 3 (make-interval '#(2 2)))
       "interval-fold-right: The second argument is not a procedure: ")

    (test (interval-fold-right 1 values 3 (make-interval '#(2 2)))
       "interval-fold-right: The first argument is not a procedure: ")
)

(define (interval-dilate-error-tests)
   (pp "interval-dilate error tests")

    (let ((interval (make-interval '#(0 0) '#(100 100))))
       (test (interval-dilate interval 'a '#(-10 10))
          "interval-dilate: The second argument is not a vector of exact integers: ")
       (test (interval-dilate 'a '#(10 10) '#(-10 -10))
          "interval-dilate: The first argument is not an interval: ")
       (test (interval-dilate interval '#(10 10) 'a)
          "interval-dilate: The third argument is not a vector of exact integers: ")
       (test (interval-dilate interval '#(10) '#(-10 -10))
          "interval-dilate: The second and third arguments must have the same length as the dimension of the first argument: ")
       (test (interval-dilate interval '#(10 10) '#( -10))
          "interval-dilate: The second and third arguments must have the same length as the dimension of the first argument: ")
       (test (interval-dilate interval '#(100 100) '#(-100 -100))
          "interval-dilate: Some resulting lower bounds are greater than corresponding upper bounds: ")))

