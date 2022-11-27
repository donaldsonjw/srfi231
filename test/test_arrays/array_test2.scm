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
(module array-test2
   (include "test_macros.sch")
   (library srfi231 srfi39 srfi27)
   (import bigloo_compat
           test_infra
           test-random-infrastructure
           number_utils)
   (export
      (array-reverse-tests)
      (array->list*-and-array-vector*)
      (test-array-extract-and-array-tile)
      (test-interval-scale-and-array-sample)
      (interval-intersect-tests)
      (interval-permute-and-array-permute-tests)
      (array-assign!-tests)
      (miscellaneous-error-tests)
      (array->list-array-vector-and-list-array-vector-array)
      (interval-cartesian-product-and-array-outer-product)
      (array-ref-and-array-set!-tests)
      (test-code-from-srfi-document)
      (cursor-array-inner-product-tests)
      (array-append-and-array-append!-tests)
      (array-stack-and-array-stack!-tests)
      (array-block-and-array-block!-tests)))


(define (array->list*-and-array-vector*)
   (pp "array->list* and array->vector*")
   
;;; Minimal tests, sorry.

   (test (array->list* 'a)
      "array->list*: The argument is not an array: ")

   (test (array->vector* 'a)
      "array->vector*: The argument is not an array: ")

   (test (array->list* (make-array (make-interval '#(1 1)) indices->string))
      '(("0_0")))

   (test (array->list* (make-array (make-interval '#(1)) indices->string))
      '("0"))

   (test (array->list* (make-array (make-interval '#(2 3)) indices->string))
      '(("0_0" "0_1" "0_2") ("1_0" "1_1" "1_2")))

   (test (array->list* (make-array (make-interval '#(1 1) '#(2 3)) indices->string))
      '(("1_1" "1_2")))

   (test (array->vector* (make-array (make-interval '#(1 1)) indices->string))
      '#(#("0_0")))

   (test (array->vector* (make-array (make-interval '#(1)) indices->string))
      '#("0"))

   (test (array->vector* (make-array (make-interval '#(2 3)) indices->string))
      '#(#("0_0" "0_1" "0_2") #("1_0" "1_1" "1_2")))

   (test (array->list* (make-array (make-interval '#(2 3)) indices->string))
      '(("0_0" "0_1" "0_2") ("1_0" "1_1" "1_2")))

   (test (array->vector* (make-array (make-interval '#(1 1) '#(2 3)) indices->string))
      '#(#("1_1" "1_2")))

   (test (array->vector* (list->array (make-interval '#(2 3))
                            '(0 1 0
                              0 1 1)
                            u1-storage-class))
      '#(#(0 1 0) #(0 1 1)))

   (test (array->list* (list->array (make-interval '#(2 3))
                          '(0 1 0
                            0 1 1)
                          u1-storage-class))
      '((0 1 0) (0 1 1)))

   (test (array->vector* (make-array (make-interval '#()) (lambda () 2)))
      2)

   (test (array->vector* (make-array (make-interval '#(0)) test-error))
      '#())

   (test (array->vector* (make-array (make-interval '#(2 0)) test-error))
      '#(#() #()))

   (test (array->vector* (make-array (make-interval '#(0 0)) test-error))
      '#())

   (test (array->vector* (make-array (make-interval '#(0 2)) test-error))
      '#())

   (test (array->list* (make-array (make-interval '#()) (lambda () 2)))
      2)

   (test (array->list* (make-array (make-interval '#(0)) test-error))
      '())

   (test (array->list* (make-array (make-interval '#(2 0)) test-error))
      '(() ()))

   (test (array->list* (make-array (make-interval '#(0 0)) test-error))
      '())

   (test (array->list* (make-array (make-interval '#(0 2)) test-error))
      '()))

(define (array-reverse-tests)
   (pp "array-reverse tests")

 (test (array-reverse 'a)
    "array-reverse: The argument is not an array: ")

 (test (array-reverse 'a 'a)
    "array-reverse: The first argument is not an array: ")

 (test (array-reverse (make-array (make-interval '#(0 0) '#(2 2)) list)
          'a)
    "array-reverse: The second argument is not a vector of booleans: ")

 (test (array-reverse (make-array (make-interval '#(0 0) '#(2 2)) list)
          '#(1 0))
    "array-reverse: The second argument is not a vector of booleans: ")

 (test (array-reverse (make-array (make-interval '#(0 0) '#(2 2)) list)
          '#(#t))
    "array-reverse: The dimension of the first argument (an array) does not equal the dimension of the second argument (a vector of booleans): ")


 (define (myarray-reverse array flip?)
    (let* ((flips (vector->list flip?))
           (domain (array-domain array))
           (lowers (interval-lower-bounds->list domain))
           (uppers (interval-upper-bounds->list domain))
           (transform
              (lambda (multi-index)
                 (map (lambda (i_k l_k u_k f_k?)
                         (if f_k?
                             (- (+ u_k l_k -1) i_k)
                             i_k))
                    multi-index lowers uppers flips))))
       (cond ((specialized-array? array)
              (specialized-array-share array
                 domain
                 (lambda multi-index
                    (apply values (transform multi-index)))))
             ((mutable-array? array)
              (let ((getter (array-getter array))
                    (setter (array-setter array)))
                 (make-array domain
                    (lambda multi-index
                       (apply getter (transform multi-index)))
                    (lambda (v . multi-index)
                       (apply setter v (transform multi-index))))))
             (else
              (let ((getter (array-getter array)))
                 (make-array domain
                    (lambda multi-index
                       (apply getter (transform multi-index)))))))))


 (do ((i 0 (+ i 1)))
     ((= i random-tests))
     (let* ((domain (random-interval))
            (Array (let ((temp (make-array domain list)))
                      (case (test-random-integer 3)
                         ((0) temp)
                         ((1) (array-copy temp))
                         ((2) (let ((temp (array-copy temp)))
                                 (make-array (array-domain temp)
                                    (array-getter temp)
                                    (array-setter temp)))))))
            (flips (vector-map (lambda (x) (random-boolean)) (make-vector (interval-dimension domain))))
            (reversed-array (array-reverse Array flips))
            (my-reversed-array (myarray-reverse Array flips)))

        (if (and (mutable-array? Array)
                 (not (array-empty? Array)))
            (do ((j 0 (+ j 1)))
                ((= j 50))
                (call-with-values
                   (lambda ()
                      (random-multi-index domain))
                   (lambda multi-index
                      (let ((value (test-random-integer 10000)))
                         (apply (array-setter reversed-array) value multi-index)
                         (apply (array-setter my-reversed-array) value multi-index))))))
        (test (myarray= reversed-array
                 my-reversed-array)
           #t)))

 (next-test-random-source-state!)

 ;; next test that the optional flip? argument is computed correctly.

 (for-each (lambda (n)
              (let* ((upper-bounds (make-vector n 2))
                     (lower-bounds (make-vector n 0))
                     (domain (make-interval lower-bounds upper-bounds))
                     (A (array-copy (make-array domain list)))
                     (immutable-A
                        (let ((A (array-copy A))) ;; copy A
                           (make-array domain
                              (array-getter A))))
                     (mutable-A
                        (let ((A (array-copy A))) ;; copy A
                           (make-array domain
                              (array-getter A)
                              (array-setter A))))
                     (flip? (make-vector n #t)))
                 (let ((r1 (array-reverse A))
                       (r2 (array-reverse A flip?)))
                    (if (not (and (specialized-array? r1)
                                  (specialized-array? r2)
                                  (myarray= r1 r2)))
                        (begin
                           (test-error "blah reverse specialized")
                           (pp 'crap))))
                 (let ((r1 (array-reverse mutable-A))
                       (r2 (array-reverse mutable-A flip?)))
                    (if (not (and (mutable-array? r1)
                                  (mutable-array? r2)
                                  (myarray= r1 r2)))
                        (begin
                           (test-error "blah reverse mutable")
                           (pp 'crap))))
                 (let ((r1 (array-reverse immutable-A))
                       (r2 (array-reverse immutable-A flip?)))
                    (if (not (and (array? r1)
                                  (array? r2)
                                  (myarray= r1 r2)))
                        (begin
                           (test-error "blah reverse immutable")
                           (pp 'crap))))))
    (iota 5 1)))

(define (test-array-extract-and-array-tile)
   (pp "test array-extract and array-tile")

 (test (array-extract (make-array (make-interval '#(0 0) '#(1 1)) list)
          'a)
    "array-extract: The second argument is not an interval: ")

 (test (array-extract 'a (make-interval '#(0 0) '#(1 1)))
    "array-extract: The first argument is not an array: ")

 (test (array-extract (make-array (make-interval '#(0 0) '#(1 1)) list)
          (make-interval '#(0) '#(1)))
    "array-extract: The dimension of the second argument (an interval) does not equal the dimension of the domain of the first argument (an array): ")

 (test (array-extract (make-array (make-interval '#(0 0) '#(1 1)) list)
          (make-interval '#(0 0) '#(1 3)))
    "array-extract: The second argument (an interval) is not a subset of the domain of the first argument (an array): ")

 (let* ((A (list->array (make-interval '#(10)) (iota 10)  generic-storage-class #f)) ;; not mutable
        (B (array-extract A (make-interval '#(0)))))   ;; used to tickle a bug
    (test (mutable-array? A)
       #f)
    (test (mutable-array? B)
       #f))

 (do ((i 0 (+fx i 1)))
     ((=fx i random-tests))
     (let* ((domain (random-interval))
            (subdomain (random-subinterval domain))
            (spec-A (array-copy (make-array domain list)))
            (spec-A-extract (array-extract spec-A subdomain))
            (mut-A (let ((A-prime (array-copy spec-A)))
                      (make-array domain
                         (array-getter A-prime)
                         (array-setter A-prime))))
            (mut-A-extract (array-extract mut-A subdomain))
            (immutable-A (let ((A-prime (array-copy spec-A)))
                            (make-array domain
                               (array-getter A-prime))))
            (immutable-A-extract (array-extract immutable-A subdomain))
            (spec-B (array-copy (make-array domain list)))
            (spec-B-extract (array-extract spec-B subdomain))
            (mut-B (let ((B-prime (array-copy spec-B)))
                      (make-array domain
                         (array-getter B-prime)
                         (array-setter B-prime))))
            (mut-B-extract (array-extract mut-B subdomain)))
        ;; test that the extracts are the same kind of arrays as the original
        (test (specialized-array? spec-A)
           #t)
        (test (specialized-array? spec-A-extract)
           #t)
        (test (and (mutable-array? mut-A)
                   (not (specialized-array? mut-A)))
           #t)
        (test (and (mutable-array? mut-A-extract)
                   (not (specialized-array? mut-A-extract)))
           #t)
        (test (and (array? immutable-A)
                   (not (mutable-array? immutable-A)))
           #t)
        (test (and (array? immutable-A-extract)
                   (not (mutable-array? immutable-A-extract)))
           #t)
        (test (array-domain spec-A-extract)
           subdomain)
        (test (array-domain mut-A-extract)
           subdomain)
        (test (array-domain immutable-A-extract)
           subdomain)
        ;; test that applying the original setter to arguments in
        ;; the subdomain gives the same answer as applying the
        ;; setter of the extracted array to the same arguments.
        (for-each (lambda (A B A-extract B-extract)
                     (let ((A-setter (array-setter A))
                           (B-extract-setter (array-setter B-extract)))
                        (do ((i 0 (+fx i 1)))
                            ((=fx i 100)
                             (test (myarray= spec-A spec-B)
                                #t)
                             (test (myarray= spec-A-extract spec-B-extract)
                                #t))
                            (if (not (interval-empty? subdomain))
                                (call-with-values
                                   (lambda ()
                                      (random-multi-index subdomain))
                                   (lambda multi-index
                                      (let ((val (test-random-real)))
                                         (apply A-setter val multi-index)
                                         (apply B-extract-setter val multi-index))))))))
           (list spec-A mut-A)
           (list spec-B mut-B)
           (list spec-A-extract mut-A-extract)
           (list spec-B-extract mut-B-extract))))

 (next-test-random-source-state!)


 (test (array-tile 'a '#(10))
    "array-tile: The first argument is not an array: ")
 (test (array-tile (make-array (make-interval '#(0 0) '#(10 10)) list) 'a)
    "array-tile: The second argument is not a vector of the same length as the dimension of the array first argument: ")
 (test (array-tile (make-array (make-interval '#(0 0) '#(10 10)) list) '#(a a))
    "array-tile: Axis 0 of the domain of the first argument has nonzero width, but element 0 of the second argument is neither an exact positive integer nor a vector of nonnegative exact integers summing to that width: ")
 (test (array-tile (make-array (make-interval '#(0 0) '#(10 10)) list) '#(-1 1))
    "array-tile: Axis 0 of the domain of the first argument has nonzero width, but element 0 of the second argument is neither an exact positive integer nor a vector of nonnegative exact integers summing to that width: ")
 (test (array-tile (make-array (make-interval '#(0 0) '#(10 10)) list) '#(10))
    "array-tile: The second argument is not a vector of the same length as the dimension of the array first argument: ")
 (test (array-tile (make-array (make-interval '#(4)) list) '#(#(0 3 0 -1 2)))
    "array-tile: Axis 0 of the domain of the first argument has nonzero width, but element 0 of the second argument is neither an exact positive integer nor a vector of nonnegative exact integers summing to that width: ")
 (test (array-tile (make-array (make-interval '#(4)) list) '#(#(0 3 0 0 2)))
    "array-tile: Axis 0 of the domain of the first argument has nonzero width, but element 0 of the second argument is neither an exact positive integer nor a vector of nonnegative exact integers summing to that width: ")

 (test (array-tile (make-array (make-interval '#(0)) list) '#(2))
    "array-tile: Axis 0 of the domain of the first argument has width 0, but element 0 of the second argument is not a nonempty vector of exact zeros: ")

 (do ((d 1 (+fx d 1)))
     ((=fx d 6))
     (let* ((A (make-array (make-interval (make-vector d 100)) list))
            (B (array-tile A (make-vector d 10)))
            (index (make-list d 12)))
        (test (apply array-ref B (make-list d 12))
           "array-tile: domain does not contain multi-index: ")
        (test (apply array-ref B (make-list d 'a))
           "array-tile: multi-index component is not an exact integer: ")
        (if (< 4 d)
            (test (array-ref B 0 0 0 0)
               "array-tile: multi-index is not the correct dimension: "))))

 (define (ceiling-quotient x d)
    ;; assumes x and d are positive
    (quotient (+ x d -1) d))

 (define (my-array-tile array sidelengths)
    ;; an alternate definition more-or-less from the srfi document
    (let* ((domain
              (array-domain array))
           (lowers
              (%%interval-lower-bounds domain))
           (uppers
              (%%interval-upper-bounds domain))
           (result-lowers
              (vector-map (lambda (x)
                             0)
                 lowers))
           (result-uppers
              (vector-map (lambda (l u s)
                             (ceiling-quotient (- u l) s))
                 lowers uppers sidelengths)))
       (make-array (make-interval result-lowers result-uppers)
          (lambda i
             (let* ((vec-i
                       (list->vector i))
                    (result-lowers
                       (vector-map (lambda (l i s)
                                      (+ l (* i s)))
                          lowers vec-i sidelengths))
                    (result-uppers
                       (vector-map (lambda (l u i s)
                                      (min u (+ l (* (+ i 1) s))))
                          lowers uppers vec-i sidelengths)))
                (array-extract array
                   (make-interval result-lowers result-uppers)))))))

;;; The array-block random tests also test array-tile.

 (do ((i 0 (+fx i 1)))
     ((=fx i random-tests))
     (let* ((domain
               (random-nonempty-interval))   ;; We use positive integers for the array-tile arguments here, so we need the domain to be nonempty.
            (array
               (let ((res (make-array domain list)))
                  (case (test-random-integer 3)
                     ;; immutable
                     ((0) res)
                     ;; specialized
                     ((1) (array-copy res))
                     (else
                      ;; mutable, but not specialized
                      (let ((res (array-copy res)))
                         (make-array domain (array-getter res) (array-setter res)))))))
            (lowers
               (%%interval-lower-bounds domain))
            (uppers
               (%%interval-upper-bounds domain))
            (sidelengths
               (vector-map (lambda (l u)
                              (let ((dim (- u l)))
                                 (random 1 (ceiling-quotient (* dim 7) 5))))
                  lowers uppers))
            (result
               (array-tile array sidelengths))
            (test-result
               (my-array-tile array sidelengths)))

        ;; extract-array is tested independently, so we just make a few tests.

        ;; test all the subdomain tiles are the same
        (test (array-every (lambda (r t)
                              (equal? (array-domain r) (array-domain t)))
                 result test-result)
           #t)
        ;; test that the subarrays are the same type
        (test (array-every (lambda (r t)
                              (and
                               (eq? (mutable-array? r) (mutable-array? t))
                               (eq? (mutable-array? r) (mutable-array? array))
                               (eq? (specialized-array? r) (specialized-array? t))
                               (eq? (specialized-array? r) (specialized-array? array))))
                 result test-result)
           #t)
        ;; test that the first tile has the right values
        (test (myarray= (apply (array-getter result) (make-list (vector-length lowers) 0))
                 (apply (array-getter test-result) (make-list (vector-length lowers) 0)))
           #t)))

 (next-test-random-source-state!))

(define (test-interval-scale-and-array-sample)
   (pp "test interval-scale and array-sample")

 (test (interval-scale 1 'a)
    "interval-scale: The first argument is not an interval with all lower bounds zero: ")

 (test (interval-scale (make-interval '#(1) '#(2)) 'a)
    "interval-scale: The first argument is not an interval with all lower bounds zero: ")

 (test (interval-scale (make-interval '#(0) '#(1))
          'a)
    "interval-scale: The second argument is not a vector of positive, exact, integers: ")

 (test (interval-scale (make-interval '#(0) '#(1))
          '#(a))
    "interval-scale: The second argument is not a vector of positive, exact, integers: ")

 (test (interval-scale (make-interval '#(0) '#(1))
          '#(0))
    "interval-scale: The second argument is not a vector of positive, exact, integers: ")

 (test (interval-scale (make-interval '#(0) '#(1))
          '#(1.))
    "interval-scale: The second argument is not a vector of positive, exact, integers: ")

 (test (interval-scale (make-interval '#(0) '#(1))
          '#(1 2))
    "interval-scale: The dimension of the first argument (an interval) is not equal to the length of the second (a vector): ")

 (define (myinterval-scale interval scales)
    (make-interval (interval-lower-bounds->vector interval)
       (vector-map (lambda (u s)
                      (quotient (+ u s -1) s))
          (interval-upper-bounds->vector interval)
          scales)))

 (do ((i 0 (+fx i 1)))
     ((=fx i random-tests))
     (let* ((interval (random-nonnegative-interval))
            (scales   (random-positive-vector (interval-dimension interval))))
        (test (  interval-scale interval scales)
           (myinterval-scale interval scales))))

 (next-test-random-source-state!)

 (test (array-sample 'a 'a)
    "array-sample: The first argument is not an array whose domain has zero lower bounds: ")

 (test (array-sample (make-array (make-interval '#(1) '#(2)) list) 'a)
    "array-sample: The first argument is not an array whose domain has zero lower bounds: ")

 (test (array-sample (make-array (make-interval '#(0) '#(2)) list) 'a)
    "array-sample: The second argument is not a vector of positive, exact, integers: ")

 (test (array-sample (make-array (make-interval '#(0) '#(2)) list) '#(1.))
    "array-sample: The second argument is not a vector of positive, exact, integers: ")

 (test (array-sample (make-array (make-interval '#(0) '#(2)) list) '#(0))
    "array-sample: The second argument is not a vector of positive, exact, integers: ")

 (test (array-sample (make-array (make-interval '#(0) '#(2)) list) '#(2 1))
    "array-sample: The dimension of the first argument (an array) is not equal to the length of the second (a vector): ")

 (define (myarray-sample array scales)
    (let ((scales-list (vector->list scales)))
       (cond ((specialized-array? array)
              (specialized-array-share array
                 (interval-scale (array-domain array) scales)
                 (lambda multi-index
                    (apply values (map * multi-index scales-list)))))
             ((mutable-array? array)
              (let ((getter (array-getter array))
                    (setter (array-setter array)))
                 (make-array (interval-scale (array-domain array) scales)
                    (lambda multi-index
                       (apply getter (map * multi-index scales-list)))
                    (lambda (v . multi-index)
                       (apply setter v (map * multi-index scales-list))))))
             (else
              (let ((getter (array-getter array)))
                 (make-array (interval-scale (array-domain array) scales)
                    (lambda multi-index
                       (apply getter (map * multi-index scales-list)))))))))



 (do ((i 0 (+ i 1)))
     ((= i random-tests))
     (let* ((domain (random-nonnegative-interval 1 6))
            (Array (let ((temp (make-array domain list)))
                      (case (test-random-integer 3)
                         ((0) temp)
                         ((1) (array-copy temp))
                         ((2) (let ((temp (array-copy temp)))
                                 (make-array (array-domain temp)
                                    (array-getter temp)
                                    (array-setter temp)))))))
            (scales (random-positive-vector (interval-dimension domain)))
            (sampled-array (array-sample Array scales))
            (my-sampled-array (myarray-sample Array scales)))

        (if (mutable-array? Array)
            (let ((scaled-domain (interval-scale domain scales)))
               (do ((j 0 (+ j 1)))
                   ((= j 50))
                   (call-with-values
                      (lambda ()
                         (random-multi-index scaled-domain))
                      (lambda multi-index
                         (let ((value (test-random-integer 10000)))
                            (apply (array-setter sampled-array) value multi-index)
                            (apply (array-setter my-sampled-array) value multi-index)))))))
        (test (myarray= sampled-array
                 my-sampled-array)
           #t)))

 (next-test-random-source-state!))

(define (interval-intersect-tests)
   (pp "interval-intersect tests")

 (let ((a (make-interval '#(0 0) '#(10 10)))
       (b (make-interval '#(0) '#(10)))
       (c (make-interval '#(10 10) '#(20 20))))
    (test (interval-intersect 'a)
       "interval-intersect: The argument is not an interval: ")
    (test (interval-intersect  a 'a)
       "interval-intersect: Not all arguments are intervals: ")
    (test (interval-intersect a b)
       "interval-intersect: Not all arguments have the same dimension: "))


 (define (my-interval-intersect . args)

    (define (fold-left operator           ;; called with (operator result-so-far (car list))
               initial-value
               list)
       (if (null? list)
           initial-value
           (fold-left operator
              (operator initial-value (car list))
              (cdr list))))


    (let ((new-uppers (let ((uppers (map interval-upper-bounds->vector args)))
                         (fold-left (lambda (arg result)
                                       (vector-map min arg result))
                            (car uppers)
                            uppers)))
          (new-lowers (let ((lowers (map interval-lower-bounds->vector args)))
                         (fold-left (lambda (arg result)
                                       (vector-map max arg result))
                            (car lowers)
                            lowers))))
       ;; (pp (list args new-lowers new-uppers (vector-every < new-lowers new-uppers)))
       (and (%%vector-every <= new-lowers new-uppers)
            (make-interval new-lowers new-uppers))))


 (do ((i 0 (+ i 1)))
     ((= i random-tests))
     (let* ((dimension (random 1 6))
            (number-of-intervals (random 1 4))
            (intervals (map (lambda (x)
                               (random-interval dimension (+ dimension 1)))
                          (local-iota 0 number-of-intervals))))
        ;; (pp (list intervals (apply my-interval-intersect intervals)))
        (test (apply my-interval-intersect intervals)
           (apply interval-intersect intervals))))

 (next-test-random-source-state!))

(define (interval-permute-and-array-permute-tests)
   (pp "interval-permute and array-permute tests")

 (let ((int (make-interval '#(0 0) '#(10 10)))
       (permutation '#(1 0)))
    (test (interval-permute 'a 10)
       "interval-permute: The first argument is not an interval: ")
    (test (interval-permute int 10)
       "interval-permute: The second argument is not a permutation: ")
    (test (interval-permute int '#(a b))
       "interval-permute: The second argument is not a permutation: ")
    (test (interval-permute int '#(1. 2.))
       "interval-permute: The second argument is not a permutation: ")
    (test (interval-permute int '#(10 -2))
       "interval-permute: The second argument is not a permutation: ")
    (test (interval-permute int '#(0))
       "interval-permute: The dimension of the first argument (an interval) does not equal the length of the second (a permutation): ")
    (do ((i 0 (+ i 1)))
        ((= i random-tests))
        (let* ((int (random-interval))
               (lower-bounds (interval-lower-bounds->vector int))
               (upper-bounds (interval-upper-bounds->vector int))
               (permutation (random-permutation (vector-length lower-bounds))))
           (interval= (interval-permute int permutation)
              (make-interval (vector-permute lower-bounds permutation)
                 (vector-permute upper-bounds permutation))))))

 (next-test-random-source-state!)

 (test (permutation? 'a) #f)
 (test (permutation? '#()) #t)
 (test (permutation? '#(1.0)) #f)
 (test (permutation? '#(1 1)) #f)
 (test (permutation? '#(1 2)) #f)
 (test (permutation? '#(1 2 0)) #t)

 (test (array-every equal?
          (array-permute (make-array (make-interval '#()) (lambda () 42))
             '#())
          (make-array (make-interval '#()) (lambda () 42)))
    #t)

 (test (array-every equal?
          (array-permute (make-array (make-interval '#(0 1)) test-error)
             '#(1 0))
          (make-array (make-interval '#(1 0)) test-error))
    #t)

 (let* ((specialized-array (array-copy (make-array (make-interval '#(0 0) '#(10 12))
                                          list)))
        (mutable-array (let ((temp (array-copy specialized-array)))
                          (make-array (array-domain temp)
                             (array-getter temp)
                             (array-setter temp))))
        (immutable-array (make-array (array-domain mutable-array)
                            (array-getter mutable-array)))
        (permutation '#(1 0)))

    (test (array-permute 'a 1)
       "array-permute: The first argument is not an array: ")
    (test (array-permute immutable-array '#(1.))
       "array-permute: The second argument is not a permutation: ")
    (test (array-permute immutable-array '#(2))
       "array-permute: The second argument is not a permutation: ")
    (test (array-permute immutable-array '#(0 1 2))
       "array-permute: The dimension of the first argument (an array) does not equal the dimension of the second argument (a permutation): ")
    (let ((specialized-result (array-permute specialized-array permutation)))
       (test (specialized-array? specialized-result)
          #t))
    (let ((mutable-result (array-permute mutable-array permutation)))
       (test (and (mutable-array? mutable-array)
                  (not (specialized-array? mutable-array))
                  (mutable-array? mutable-result)
                  (not (specialized-array? mutable-result)))
          #t))
    (let ((immutable-result (array-permute immutable-array permutation)))
       (test (and (array? immutable-array)
                  (not (mutable-array? immutable-array))
                  (array? immutable-result)
                  (not (mutable-array? immutable-result)))
          #t))

    (specialized-array-default-safe? #t)

    (do ((i 0 (+ i 1)))
        ((= i random-tests))
        (let* ((domain (random-interval))
               (Array (let ((temp (make-array domain list)))
                         (case (test-random-integer 3)
                            ((0) temp)
                            ((1) (array-copy temp))
                            ((2) (let ((temp (array-copy temp)))
                                    (make-array (array-domain temp)
                                       (array-getter temp)
                                       (array-setter temp)))))))
               (permutation (random-permutation (interval-dimension domain))))

           (define (my-array-permute Array permutation)
              (let* ((array-copy (array-copy Array))
                     (getter (array-getter array-copy))
                     (setter (array-setter array-copy))
                     (permutation-inverse (%%permutation-invert permutation)))
                 (make-array (interval-permute (array-domain Array)
                                permutation)
                    (lambda args
                       (apply getter
                          (vector->list (vector-permute (list->vector args) permutation-inverse))))
                    (lambda (v . args)
                       (apply setter
                          v
                          (vector->list (vector-permute (list->vector args) permutation-inverse)))))))

           ;; (pp (list domain permutation (interval-volume domain)))
           (let ((permuted-array       (array-permute Array permutation))
                 (my-permuted-array (my-array-permute Array permutation)))
              (if (and (mutable-array? Array)
                       (not (interval-empty? (array-domain Array))))
                  (let ((permuted-domain (interval-permute domain permutation)))
                     (do ((j 0 (+ j 1)))
                         ((= j 50))
                         (call-with-values
                            (lambda ()
                               (random-multi-index permuted-domain))
                            (lambda multi-index
                               (let ((value (test-random-integer 10000)))
                                  (apply (array-setter permuted-array) value multi-index)
                                  (apply (array-setter my-permuted-array) value multi-index)))))))
              (test (myarray= permuted-array
                       my-permuted-array)
                 #t))))

    (next-test-random-source-state!)

    (specialized-array-default-safe? #f)

    (do ((i 0 (+ i 1)))
        ((= i random-tests))
        (let* ((domain (random-interval))
               (Array (let ((temp (make-array domain list)))
                         (case (test-random-integer 3)
                            ((0) temp)
                            ((1) (array-copy temp))
                            ((2) (let ((temp (array-copy temp)))
                                    (make-array (array-domain temp)
                                       (array-getter temp)
                                       (array-setter temp)))))))
               (permutation (random-permutation (interval-dimension domain))))
           
           (define (my-array-permute Array permutation)
              (let* ((array-copy (array-copy Array))
                     (getter (array-getter array-copy))
                     (setter (array-setter array-copy))
                     (permutation-inverse (%%permutation-invert permutation)))
                 (make-array (interval-permute (array-domain Array)
                                permutation)
                    (lambda args
                       (apply getter
                          (vector->list (vector-permute (list->vector args) permutation-inverse))))
                    (lambda (v . args)
                       (apply setter
                          v
                          (vector->list (vector-permute (list->vector args) permutation-inverse)))))))

           ;; (pp (list domain permutation (interval-volume domain)))
           (let ((permuted-array       (array-permute Array permutation))
                 (my-permuted-array (my-array-permute Array permutation)))
              (if (and (not (array-empty? Array))
                       (mutable-array? Array))
                  (let ((permuted-domain (interval-permute domain permutation)))
                     (do ((j 0 (+ j 1)))
                         ((= j 50))
                         (call-with-values
                            (lambda ()
                               (random-multi-index permuted-domain))
                            (lambda multi-index
                               (let ((value (test-random-integer 10000)))
                                  (apply (array-setter permuted-array) value multi-index)
                                  (apply (array-setter my-permuted-array) value multi-index)))))))
              ; (test (myarray= permuted-array
              ;          my-permuted-array)
              ;    #t)
              #unspecified
              ))))

 (next-test-random-source-state!))


(define (array-assign!-tests)
   (pp "array-assign! tests")

   (test (array-assign! 'a 'a)
      "array-assign!: The destination is not a mutable array: ")

   (test (array-assign! (make-array (make-interval '#(0 0) '#(1 1)) values) 'a)
      "array-assign!: The destination is not a mutable array: ")

   (test (array-assign! (array-copy (make-array (make-interval '#(0 0) '#(1 1)) values)) 'a)
      "array-assign!: The source is not an array: ")

   (test (array-assign! (array-copy (make-array (make-interval '#(0 0) '#(1 1)) values))
            (make-array (make-interval '#(0 0) '#(2 1)) values))
      "array-assign: The destination and source do not have the same domains: ")

   (test (array-assign! (make-array (make-interval '#(1 2)) list list) ;; not valid
            (make-array (make-interval '#(0 0) '#(2 1)) values))
      "array-assign: The destination and source do not have the same domains: ")

   (test (array-assign! (array-permute (array-copy (make-array (make-interval '#(2 3))
                                                      list))
                           '#(1 0))
            (make-array (make-interval '#(2 3)) list))
      "array-assign: The destination and source do not have the same domains: ")

   (let ((destination (make-specialized-array (make-interval '#(3 2))))  ;; elements in order
         (source (array-permute (make-array (make-interval '#(3 2)) list) ;; not the same interval, but same volume
                    '#(1 0))))
      (test (array-assign! destination source)
         "array-assign: The destination and source do not have the same domains: "))



   (do ((d 1 (+fx d 1)))
       ((= d 6))
       (let* ((unsafe-specialized-destination
                 (make-specialized-array (make-interval (make-vector d 10))
                    u1-storage-class))
              (safe-specialized-destination
                 (make-specialized-array (make-interval (make-vector d 10))
                    u1-storage-class
                    0
                    #t))
              (mutable-destination
                 (make-array (array-domain safe-specialized-destination)
                    (array-getter safe-specialized-destination)
                    (array-setter safe-specialized-destination)))
              (source
                 (make-array (array-domain safe-specialized-destination)
                    (lambda args 100)))) ;; not 0 or 1
          (test (array-assign! unsafe-specialized-destination source) ;; should check anyway
             "array-assign!: Not all elements of the source can be stored in destination: ")
          (test (array-assign! safe-specialized-destination source)
             "array-assign!: Not all elements of the source can be stored in destination: ")
          (test (array-assign! mutable-destination source)
             "array-setter: value cannot be stored in body: ")))

   (do ((i 0 (+fx i 1)))
       ((=fx i random-tests))
       (let* ((interval
                 (random-interval))
              (subinterval
                 (random-subinterval interval))
              (storage-class-and-initializer
                 (random-storage-class-and-initializer))
              (storage-class
                 (car storage-class-and-initializer))
              (initializer
                 (cadr storage-class-and-initializer))
              (specialized-array
                 (array-copy
                    (make-array interval initializer)
                    storage-class))
              (mutable-array
                 (let ((specialized-array
                          (array-copy
                             (make-array interval initializer)
                             storage-class)))
                    (make-array interval
                       (array-getter specialized-array)
                       (array-setter specialized-array))))
              (specialized-subarray
                 (array-extract specialized-array subinterval))
              (mutable-subarray
                 (array-extract mutable-array subinterval))
              (new-subarray
                 (array-copy
                    (make-array subinterval initializer)
                    storage-class)))
          ;; (pp specialized-array)
          (array-assign! specialized-subarray new-subarray)
          (array-assign! mutable-subarray new-subarray)
          (test (myarray= specialized-array
                   (make-array interval
                      (lambda multi-index
                         (if (apply interval-contains-multi-index? subinterval multi-index)
                             (apply (array-getter new-subarray) multi-index)
                             (apply (array-getter specialized-array) multi-index)))))
             #t)
          (test (myarray= mutable-array
                   (make-array interval
                      (lambda multi-index
                         (if (apply interval-contains-multi-index? subinterval multi-index)
                             (apply (array-getter new-subarray) multi-index)
                             (apply (array-getter mutable-array) multi-index)))))
             #t)))

   (next-test-random-source-state!))

(define (miscellaneous-error-tests)
   (pp "Miscellaneous error tests")

   (test (make-array (make-interval '#(0 0) '#(10 10))
            list
            'a)
      "make-array: The third argument is not a procedure: ")

   (test (array-dimension 'a)
      "array-dimension: The argument is not an array: ")

   (test (array-safe?
            (array-copy (make-array (make-interval '#(0 0) '#(10 10)) list)
               generic-storage-class
               #t
               #t))
      #t)


   (test (array-safe?
            (array-copy (make-array (make-interval '#(0 0) '#(10 10)) list)
               generic-storage-class
               #t
               #f))
      #f)

   (let ((array-builders (vector (list u1-storage-class      (lambda indices (random (expt 2 1))) '(a -1))
                            (list u8-storage-class      (lambda indices (number->uint8 (random (expt 2 8)))) '(a -1))
                            (list u16-storage-class     (lambda indices (number->uint16 (random (expt 2 16)))) '(a -1))
                            (list u32-storage-class     (lambda indices (number->uint32 (random (expt #z2 32)))) '(a -1))
                            (list u64-storage-class     (lambda indices (number->uint64 (random (expt #z2 64)))) '(a -1))
                            (list s8-storage-class      (lambda indices (number->int8 (random (- (expt 2 7))  (expt 2 7)))) `(a ,(expt 2 8)))
                            (list s16-storage-class     (lambda indices (number->int16 (random (- (expt 2 15)) (expt 2 15)))) `(a ,(expt 2 16)))
                            (list s32-storage-class     (lambda indices (number->int32 (random (- (expt #z2 31)) (expt #z2 31)))) `(a ,(expt #z2 32)))
                            (list s64-storage-class     (lambda indices (number->int64 (random (- (expt #z2 63)) (expt #z2 63)))) `(a ,(expt #z2 64)))
                            (list f32-storage-class     (lambda indices (test-random-real)) `(a 1))
                            (list f64-storage-class     (lambda indices (test-random-real)) `(a 1))
                            (list char-storage-class    (lambda indices (random-char)) `(a 1))
                            ;(list c64-storage-class     (lambda indices (make-rectangular (test-random-real) (test-random-real))) `(a 1))
                            ;(list c128-storage-class    (lambda indices (make-rectangular (test-random-real) (test-random-real))) `(a 1))
                            )))
      (do ((i 0 (+ i 1)))
          ((= i random-tests))
          (let* ((domain (random-nonempty-interval))  ;; we're testing invalid arguments, so no zero-dimensional arrays
                 (builders (vector-ref array-builders (test-random-integer (vector-length array-builders))))
                 (storage-class (car builders))
                 (random-entry (cadr builders))
                 (invalid-entry (list-ref (caddr builders) (random 2)))
                 (Array (array-copy (make-array domain random-entry)
                           storage-class
                           #t   ; mutable
                           #t)) ; safe
                 (getter (array-getter Array))
                 (setter (array-setter Array))
                 (dimension (interval-dimension domain))
                 (valid-args (call-with-values
                                (lambda ()
                                   (random-multi-index domain))
                                list)))
             (test (apply setter invalid-entry valid-args)
                "array-setter: value cannot be stored in body: ")
             (if (positive? dimension)
                 (begin
                    (set-car! valid-args 'a)
                    (test (apply getter valid-args)
                       "array-getter: multi-index component is not an exact integer: ")
                    (test (apply setter 10 valid-args)
                       "array-setter: multi-index component is not an exact integer: ")
                    (set-car! valid-args 10000) ;; outside the range of any random-interval
                    (test (apply getter valid-args)
                       "array-getter: domain does not contain multi-index: ")
                    (test (apply setter 10 valid-args)
                       "array-setter: domain does not contain multi-index: ")))
             (if (< 4 dimension)
                 (begin
                    (set! valid-args (cons 1 valid-args))
                    (test (apply getter valid-args)
                       "array-getter: multi-index is not the correct dimension: ")
                    (test (apply setter 10 valid-args)
                       "array-setter: multi-index is not the correct dimension: "))))))

   (next-test-random-source-state!))

(define (array->list-array-vector-and-list-array-vector-array)
   (pp "array->list, array->vector and list->array, vector->array")

   (test (array->list 'a)
      "array->list: The argument is not an array: ")

   (test (array->vector 'a)
      "array->vector: The argument is not an array: ")

   (let* ((multi-indices
             '())
          (a
             (make-array (make-interval '#(3 5))
                (lambda (i j)
                   (set! multi-indices (cons (list i j) multi-indices))
                   (+ j (* i 5)))))
          (result
             (array->list a))
          (correct-multi-indices
             (let ((result '()))
                (interval-for-each (lambda (i j)
                                      (set! result (cons (list i j) result)))
                   (array-domain a))
                result)))
      (test result (iota 15))
      (test multi-indices correct-multi-indices))

   (for-each (lambda (function arg name name2)
                (test (function 'b arg)
                   (string-append name "The first argument is not an interval: "))
                (test (function (make-interval '#(0) '#(1)) 'b)
                   (string-append name "The second argument is not a " name2 ": "))
                (test (function (make-interval '#(0) '#(1)) arg 'a)
                   (string-append name "The third argument is not a storage-class: "))
                (test (function (make-interval '#(0) '#(1)) arg generic-storage-class 'a)
                   (string-append name "The fourth argument is not a boolean: "))
                (test (function (make-interval '#(0) '#(1)) arg generic-storage-class #t 'a)
                   (string-append name "The fifth argument is not a boolean: "))
                (test (function (make-interval '#(0) '#(10)) arg)
                   (string-append name "The volume of the first argument does not equal the length of the second: "))
                (test (function (make-interval '#(0) '#(1)) arg u1-storage-class)
                   (string-append name "Not all elements of the source can be stored in destination: "))
                (test (function (make-interval '#(10)) arg)
                   (string-append name "The volume of the first argument does not equal the length of the second: ")))
      (list list->array vector->array)
      '((10) #(10))
      '("list->array: " "vector->array: ")
      '("list" "vector"))


   (let ((array-builders (vector (list u1-storage-class      (lambda indices (random 0 (expt 2 1))))
                            (list u8-storage-class      (lambda indices (number->uint8 (random 0 (expt 2 8)))))
                            (list u16-storage-class     (lambda indices (number->uint16 (random 0 (expt 2 16)))))
                            (list u32-storage-class     (lambda indices (number->uint32 (random 0 (expt #z2 32)))))
                            (list u64-storage-class     (lambda indices (number->uint64 (random 0 (expt #z2 64)))))
                            (list s8-storage-class      (lambda indices (number->int8 (random (- (expt 2 7))  (expt 2 7)))))
                            (list s16-storage-class     (lambda indices (number->int16 (random (- (expt 2 15)) (expt 2 15)))))
                            (list s32-storage-class     (lambda indices (number->int32 (random (- (expt #z2 31)) (expt #z2 31)))))
                            (list s64-storage-class     (lambda indices (number->int64 (random (- (expt #z2 63)) (expt #z2 63)))))
                            (list f32-storage-class     (lambda indices (test-random-real)))
                            (list f64-storage-class     (lambda indices (test-random-real)))
                            (list char-storage-class    (lambda indices (random-char)))
                            ;(list c64-storage-class     (lambda indices (make-rectangular (test-random-real) (test-random-real))))
                            ;(list c128-storage-class    (lambda indices (make-rectangular (test-random-real) (test-random-real))))
                            (list generic-storage-class (lambda indices indices)))))
      (do ((i 0 (+ i 1)))
          ((= i random-tests))
          (let* ((domain (random-interval))
                 (builders (vector-ref array-builders (test-random-integer (vector-length array-builders))))
                 (storage-class (car builders))
                 (random-entry (cadr builders))
                 (Array (array-copy (make-array domain random-entry)
                           storage-class
                           #f
                           #t)) ; safe
                 (l (array->list Array))
                 (mutable? (zero? (test-random-integer 2)))
                 (new-list-array (list->array domain l storage-class mutable?))
                 (new-vector-array (vector->array domain (list->vector l) storage-class mutable?)))
             (test (myarray= Array new-list-array)
                #t)
             (test (myarray= Array new-vector-array)
                #t))))

   (next-test-random-source-state!)
   )

(define (interval-cartesian-product-and-array-outer-product)
   (pp "interval-cartesian-product and array-outer-product")

   (define (my-interval-cartesian-product . args)
      (make-interval (list->vector (apply append (map interval-lower-bounds->list args)))
         (list->vector (apply append (map interval-upper-bounds->list args)))))

   (test (interval-cartesian-product 'a)
      "interval-cartesian-product: Not all arguments are intervals: ")

   (test (interval-cartesian-product (make-interval '#(0) '#(1)) 'a)
      "interval-cartesian-product: Not all arguments are intervals: ")

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((intervals
                 (map (lambda (ignore)
                         (random-interval 1 4))
                    (make-list (random 1 4)))))
          (test (apply interval-cartesian-product intervals)
             (apply my-interval-cartesian-product intervals))))

   (next-test-random-source-state!)

   (let ((test-array (make-array  (make-interval '#(0) '#(1)) list)))

      (test (array-outer-product 'a test-array test-array)
         "array-outer-product: The first argument is not a procedure: ")

      (test (array-outer-product append 'a test-array)
         "array-outer-product: The second argument is not an array: ")

      (test (array-outer-product append test-array 'a)
         "array-outer-product: The third argument is not an array: "))

   (let* ((A (make-array (make-interval '#(0 10)) list))
          (B (make-array (make-interval '#(10 0)) list))
          (A*B (array-outer-product cons A B)))
      (test ((array-getter A*B) 0 0 0 0) ;; outside of domain
         "array-getter: Array domain is empty: "))


   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((arrays
                 (map (lambda (ignore)
                         (make-array (random-interval 0 5) list))
                    (make-list 2))))
          (myarray= (apply array-outer-product append arrays)
             (make-array (apply my-interval-cartesian-product (map array-domain arrays))
                list))
          (test (myarray= (apply array-outer-product append arrays)
                   (make-array (apply my-interval-cartesian-product (map array-domain arrays))
                      list))
             #t)))

   (next-test-random-source-state!))

(define (array-ref-and-array-set!-tests)
   
   (pp "array-ref and array-set! tests")
   
   
   (define A-ref
      (array-copy
         (make-array (make-interval '#(10 10))
            (lambda (i j) (if (= i j) 1 0)))))

   
   (do ((i 1 (+ i 1)))
       ((= i 6))
       (test (apply array-ref 1 (make-list i 0))
          "array-ref: The first argument is not an array: "))

   (test (array-ref A-ref 1)
      "Wrong number of arguments")

   (test (array-ref A-ref 1 1001)
      "array-getter: domain does not contain multi-index: ")

   (test (array-ref A-ref 4 4)
      1)

   (test (array-ref A-ref 4 5)
      0)

   (do ((d 0 (+ d 1)))
       ((= d 6))
       (let ((A (make-specialized-array (make-interval (make-vector d 1)) generic-storage-class 42)))
          (test (apply array-ref A (make-list d 0))
             42)
          (test (apply array-ref 2 (make-list d 0))
             (if (zero? d)
                 "array-ref: The argument is not an array: "
                 "array-ref: The first argument is not an array: "))))

   (test (array-ref (make-specialized-array (make-interval '#(0 0)) generic-storage-class 42) 0 0)
      "array-getter: Array domain is empty: ")

   (test (array-set! (make-specialized-array (make-interval '#(0 0)) generic-storage-class 42) 42 0 0)
      "array-setter: Array domain is empty: ")

   (define B-set!
      (array-copy
         (make-array (make-interval '#(10 10))
            (lambda (i j) (if (= i j) 1 0)))
         u1-storage-class))

   (test (array-set! 1 1 1)
      "array-set!: The first argument is not a mutable array: ")

   (test (array-set! B-set!)
      "no matching clause for args:")

   (test (array-set! B-set! 2)
      "Wrong number of arguments")

   (test (array-set! B-set! 2 1)
      "Wrong number of arguments")

   (test (array-set! B-set! 2 1 1)
      "array-setter: value cannot be stored in body: ")

   (array-set! B-set! 1 1 2)
   (array-set! B-set! 0 2 2)
   (array-display B-set!)

   (do ((d 0 (+ d 1)))
       ((= d 6))
       (let ((A (make-specialized-array (make-interval (make-vector d 1)) generic-storage-class 10)))
          (apply array-set! A 42 (make-list d 0))
          (test (apply array-ref A (make-list d 0))
             42)
          (test (apply array-set! 2 42 (make-list d 0))
             "array-set!: The first argument is not a mutable array: ")))

   (pp "specialized-array-reshape tests")

   (test (specialized-array-reshape 'a 1)
      "specialized-array-reshape: The first argument is not a specialized array: ")

   (test (specialized-array-reshape A-ref 'a)
      "specialized-array-reshape: The second argument is not an interval ")

   (test (specialized-array-reshape A-ref (make-interval '#(5)))
      "specialized-array-reshape: The volume of the domain of the first argument is not equal to the volume of the second argument: ")

   (test (specialized-array-reshape A-ref (make-interval '#(100)) 'a)
      "specialized-array-reshape: The third argument is not a boolean: ")

   (let ((array (array-copy (make-array (make-interval '#(2 1 3 1)) list))))
      (test (array->list array)
         (array->list (specialized-array-reshape array (make-interval '#(6))))))

   (let ((array (array-copy (make-array (make-interval '#(2 1 3 1)) list))))
      (test (array->list array)
         (array->list (specialized-array-reshape array (make-interval '#(3 2))))))

   (let ((array (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)))))
      (test (array->list array)
         (array->list (specialized-array-reshape array (make-interval '#(6))))))

   (let ((array (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)))))
      (test (array->list (specialized-array-reshape array (make-interval '#(3 2))))
         (array->list array)))

   (let ((array (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#f #f #f #t))))
      (test (array->list (specialized-array-reshape array (make-interval '#(3 2))))
         (array->list array)))

   (let ((array (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#f #f #f #t))))
      (test (array->list (specialized-array-reshape array (make-interval '#(3 1 2))))
         (array->list array)))

   (let ((array (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#f #f #f #t))))
      (test (array->list (specialized-array-reshape array (make-interval '#(1 1 1 3 2))))
         (array->list array)))

   (let ((array (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#f #f #f #t))))
      (test (array->list (specialized-array-reshape array (make-interval '#(3 2 1 1 1))))
         (array->list array)))

   (let ((array (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#f #f #f #t))))
      (test (array->list (specialized-array-reshape array (make-interval '#(3 1 1 2))))
         (array->list array)))

   (let ((array (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#f #f #f #t))))
      (test (array->list (specialized-array-reshape array (make-interval '#(3 1 2 1))))
         (array->list array)))

   (let ((array (array-sample (array-reverse (array-copy (make-array (make-interval '#(2 1 4 1)) list)) '#(#f #f #f #t)) '#(1 1 2 1))))
      (test (array->list (specialized-array-reshape array (make-interval '#(4))))
         (array->list array)))

   (let ((array (array-sample (array-reverse (array-copy (make-array (make-interval '#(2 1 4 1)) list)) '#(#t #f #t #t)) '#(1 1 2 1))))
      (test (array->list (specialized-array-reshape array (make-interval '#(4))))
         (array->list array)))

   (test (specialized-array-reshape (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#t #f #f #f)) (make-interval '#(6)))
      "specialized-array-reshape: Requested reshaping is impossible: ")

   (test (specialized-array-reshape (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#t #f #f #f)) (make-interval '#(3 2)))
      "specialized-array-reshape: Requested reshaping is impossible: ")

   (test (specialized-array-reshape (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#f #f #t #f)) (make-interval '#(6)))
      "specialized-array-reshape: Requested reshaping is impossible: ")

   (test (specialized-array-reshape (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#f #f #t #t)) (make-interval '#(3 2)))
      "specialized-array-reshape: Requested reshaping is impossible: ")

   (test (specialized-array-reshape (array-sample (array-reverse (array-copy (make-array (make-interval '#(2 1 3 1)) list)) '#(#f #f #f #t)) '#(1 1 2 1)) (make-interval '#(4)))
      "specialized-array-reshape: Requested reshaping is impossible: ")

   (test (specialized-array-reshape (array-sample (array-reverse (array-copy (make-array (make-interval '#(2 1 4 1)) list)) '#(#f #f #t #t)) '#(1 1 2 1)) (make-interval '#(4)))
      "specialized-array-reshape: Requested reshaping is impossible: ")

   (test (array? (specialized-array-reshape (make-specialized-array (make-interval '#(1 2 0 4)))
                    (make-interval '#(2 0 4))))
      #t))


(define (test-code-from-srfi-document)
   (pp "Test code from the SRFI document")
   
   (test (interval= (interval-dilate (make-interval '#(100 100)) '#(1 1) '#(1 1))
            (make-interval '#(1 1) '#(101 101)))
      #t)
   
   (test (interval= (interval-dilate (make-interval '#(100 100)) '#(-1 -1) '#(1 1))
            (make-interval '#(-1 -1) '#(101 101)))
      #t)
   
   (test (interval= (interval-dilate (make-interval '#(100 100))  '#(0 0) '#(-50 -50))
            (make-interval '#(50 50)))
      #t)
   
   (test (interval-dilate (make-interval '#(100 100)) '#(0 0) '#(-500 -50))
      "interval-dilate: Some resulting lower bounds are greater than corresponding upper bounds: ")
   
   (define a (make-array (make-interval '#(1 1) '#(11 11))
                (lambda (i j)
                   (if (= i j)
                       1
                       0))))
   
   (test ((array-getter a) 3 3)
      1)
   
   (test ((array-getter a) 2 3)
      0)

   ;; ((array-getter a) 11 0) is an error, but it isn't signalled
   
   (define a_1 (make-array (make-interval '#(0 0) '#(10 10))
                  list))
   
   (test ((array-getter a_1) 3 4)
      '(3 4))
   
   (define curried-a (array-curry a_1 1))
   
   (test ((array-getter ((array-getter curried-a) 3)) 4)
      '(3 4))
   
   (define sparse-array
      (let ((domain (make-interval '#(1000000 1000000)))
            (sparse-rows (make-vector 1000000 '())))
         (make-array domain
            (lambda (i j)
               (cond ((assv j (vector-ref sparse-rows i))
                      => cdr)
                     (else
                      0.0)))
            (lambda (v i j)
               (cond ((assv j (vector-ref sparse-rows i))
                      => (lambda (pair)
                            (set-cdr! pair v)))
                     (else
                      (vector-set! sparse-rows i (cons (cons j v) (vector-ref sparse-rows i)))))))))
   
   (test ((array-getter sparse-array) 12345 6789)
      0.)
   
   (test ((array-getter sparse-array) 0 0)
      0.)
   
   ((array-setter sparse-array) 1.0 0 0)
   
   (test ((array-getter sparse-array) 12345 6789)
      0.)
   
   (test ((array-getter sparse-array) 0 0)
      1.)

   (for-each (lambda (s)
                (for-each display
                   (list "(palindrome? \""
                      s
                      "\") => "
                      (palindrome? s)
                      #\newline)))
      '("" "a" "aa" "ab" "aba" "abc" "abba" "abca" "abbc"))

   (let ((a (make-array (make-interval '#(10)) (lambda (i) i))))
      (test (array-fold-left cons '() a)
         '((((((((((() . 0) . 1) . 2) . 3) . 4) . 5) . 6) . 7) . 8) . 9))
      (test (array-fold-right cons '() a)
         '(0 1 2 3 4 5 6 7 8 9))
      (test (array-fold-left - 0 a)
         -45)
      (test (array-fold-right - 0 a)
         -5)))


(define (cursor-array-inner-product-tests)
   (pp "cursory array-inner-product tests")

 (test (array-inner-product 'a 'a 'a 'a)
    "array-inner-product: The first argument is not an array: ")

 (test (array-inner-product (make-array (make-interval '#(10)) list) 'a 'a 'a)
    "array-inner-product: The second argument is not a procedure: ")

 (test (array-inner-product (make-array (make-interval '#(10)) list) list 'a 'a)
    "array-inner-product: The third argument is not a procedure: ")

 (test (array-inner-product (make-array (make-interval '#(10)) list) list list 'a)
    "array-inner-product: The fourth argument is not an array: ")

 (test (array-inner-product (make-array (make-interval '#(10 1)) list) list list (make-array (make-interval '#(10)) list))
    "array-inner-product: The bounds of the last dimension of the first argument are not the same as the bounds of the first dimension of the fourth argument: ")

 (test (array-inner-product (make-array (make-interval '#(10 1)) list) list list (make-array (make-interval '#(10 1)) list))
    "array-inner-product: The bounds of the last dimension of the first argument are not the same as the bounds of the first dimension of the fourth argument: ")


 (test (array-inner-product (make-array (make-interval '#(1 10)) list)
          list list
          (make-array (make-interval '#(0 10)) list))
    "array-inner-product: The bounds of the last dimension of the first argument are not the same as the bounds of the first dimension of the fourth argument: ")


 (test (array-inner-product (make-array (make-interval '#()) list)
          list list
          (make-array (make-interval '#(10 0)) list))
    "array-inner-product: The first argument has dimension zero: ")

 (test (array-inner-product (make-array (make-interval '#(10 0)) list)
          list list
          (make-array (make-interval '#()) list))
    "array-inner-product: The fourth argument has dimension zero: ")

 (let* ((A (make-array (make-interval '#(0 4)) list))
        (B (make-array (make-interval '#(4 0)) list))
        (C (array-inner-product A list list B))) ;; should be no error, you can take outer product of empty arrays
    (test (array-ref C 0 0)
       "array-getter: Array domain is empty: "))


 (let* ((A (make-array (make-interval '#(4 0)) list))
        (B (make-array (make-interval '#(0 4)) list))
        (C (array-inner-product A list list B))) ;; should be no error
    (test (array-ref C 0 0)
       "array-inner-product: Attempting to reduce over an empty array: ")))

(define (array-append-and-array-append!-tests)
   (pp "array-append and array-append! tests")

 (for-each
    (lambda (call/cc-safe?)
       (let ((array-append
                (if call/cc-safe?
                    array-append
                    array-append!))
             (message
                (if call/cc-safe?
                    "array-append:"    ;; no trailing space
                    "array-append!:")))

          (define (wrap error-reason)
             (string-append message error-reason))

          (test (array-append 1 'a)
             (wrap " Expecting as the second argument a nonnull list of arrays with the same dimension: "))

          (test (array-append 1 '())
             (wrap " Expecting as the second argument a nonnull list of arrays with the same dimension: "))

          (test (array-append 1 '(a))
             (wrap " Expecting as the second argument a nonnull list of arrays with the same dimension: "))

          (test (array-append 1 (list (make-array (make-interval '#(1)) list) (make-array (make-interval '#(2 2)) list)))
             (wrap " Expecting as the second argument a nonnull list of arrays with the same dimension: "))

          (test (array-append 1 (list (make-array (make-interval '#(2 2)) list) 'a))
             (wrap " Expecting as the second argument a nonnull list of arrays with the same dimension: "))

          (test (array-append 3 (list (make-array (make-interval '#(1 1)) list) (make-array (make-interval '#(2 2)) list)))
             (wrap " Expecting an exact integer between 0 (inclusive) and the dimension of the arrays (exclusive) as the first argument:"))

          (test (array-append -1 (list (make-array (make-interval '#(1 1)) list) (make-array (make-interval '#(2 2)) list)))
             (wrap " Expecting an exact integer between 0 (inclusive) and the dimension of the arrays (exclusive) as the first argument:"))

          (test (array-append 2 (list (make-array (make-interval '#(1 1)) list) (make-array (make-interval '#(2 2)) list)))
             (wrap " Expecting an exact integer between 0 (inclusive) and the dimension of the arrays (exclusive) as the first argument:"))

          (test (array-append 0
                   (list (make-array (make-interval '#(1 1)) list) (make-array (make-interval '#(2 2)) list))
                   'a)
             (wrap " Expecting a storage class as the third argument: "))

          (test (array-append 0
                   (list (make-array (make-interval '#(1 1)) list) (make-array (make-interval '#(2 2)) list))
                   u1-storage-class
                   'a)
             (wrap " Expecting a boolean as the fourth argument: "))

          (test (array-append 0
                   (list (make-array (make-interval '#(1 1)) list) (make-array (make-interval '#(2 2)) list))
                   u1-storage-class
                   #t
                   'a)
             (wrap " Expecting a boolean as the fifth argument: "))

          (test (array-append 0
                   (list (make-array (make-interval '#(2 4)) list)
                      (make-array (make-interval '#(3 5)) list)))
             (wrap " Expecting as the second argument a nonnull list of arrays with the same upper and lower bounds (except for index 0): "))

          (test (array-append 0
                   (list (make-array (make-interval '#(1 1)) list) (make-array (make-interval '#(2 1)) list))
                   u1-storage-class)
             (wrap " Not all elements of the source can be stored in destination: "))
          ))
    '(#t #f))



 (define (my-array-append k . arrays)              ;; call with at least one array
    (call-with-values
       (lambda ()
          ;; compute lower and upper bounds of where
          ;; we'll copy each array argument, plus
          ;; the size of the kth axis of the result array
          (let loop ((result '(0))
                     (arrays arrays))
             (if (null? arrays)
                 (values (reverse result) (car result))
                 (let ((interval (array-domain (car arrays))))
                    (loop (cons (+ (car result)
                                   (- (interval-upper-bound interval k)
                                      (interval-lower-bound interval k)))
                             result)
                       (cdr arrays))))))
       (lambda (axis-subdividers kth-size)
          (let* ((array
                    (car arrays))
                 (lowers                         ;; the domains of the arrays differ only in the kth axis
                    (interval-lower-bounds->vector (array-domain array)))
                 (uppers
                    (interval-upper-bounds->vector (array-domain array)))
                 (result                         ;; the result array
                    (make-specialized-array
                       (let ()
                          (vector-set! lowers k 0)
                          (vector-set! uppers k kth-size)
                          (make-interval lowers uppers))))
                 (translation
                    ;; a vector we'll use to align each argument
                    ;; array into the proper subarray of the result
                    (make-vector (array-dimension array) 0)))
             (let loop ((arrays arrays)
                        (subdividers axis-subdividers))
                (if (null? arrays)
                    ;; we've assigned every array to the appropriate subarray of result
                    result
                    (let ((array (car arrays)))
                       (vector-set! lowers k (car subdividers))
                       (vector-set! uppers k (cadr subdividers))
                       (vector-set! translation k (- (car subdividers)
                                                     (interval-lower-bound (array-domain array) k)))
                       (array-assign!
                          (array-extract result (make-interval lowers uppers))
                          (array-translate array translation))
                       (loop (cdr arrays)
                          (cdr subdividers)))))))))


 (for-each
    (lambda (array-append)

       (define (->generalized-array array)
          (make-array (array-domain array)
             (array-getter array)))

       (test (array-storage-class
                (array-append 0
                   (list (array-copy (make-array (make-interval '#(10)) (lambda (i) (number->uint8 (random-integer 10)))) u8-storage-class)
                      (array-copy (make-array (make-interval '#(10)) (lambda (i) (number->uint16 (random-integer 10)))) u16-storage-class))))
          generic-storage-class)

       (test (myarray= (array-append
                          0
                          (list (->generalized-array (list->array (make-interval '#(2 2))
                                                        '(1 2
                                                          3 4)))
                             (->generalized-array (list->array (make-interval '#(2 2))
                                                     '(5 6
                                                       7 8)))))
                (list->array (make-interval '#(4 2))
                   '(1 2
                     3 4
                     5 6
                     7 8)))
          #t)

       (test (myarray= (array-append
                          1
                          (list (->generalized-array (list->array (make-interval '#(2 2))
                                                        '(1 2
                                                          3 4)))
                             (list->array (make-interval '#(2 2))
                                '(5 6
                                  7 8))))
                (list->array (make-interval '#(2 4))
                   '(1 2 5 6
                     3 4 7 8)))
          #t)

       (test (myarray= (array-append
                          0
                          (list (->generalized-array (list->array (make-interval '#(2 2))
                                                        '(1 2
                                                          3 4)))
                             (list->array (make-interval '#(2 2))
                                '(5 6
                                  7 8))))
                (my-array-append
                   0
                   (list->array (make-interval '#(2 2))
                      '(1 2
                        3 4))
                   (list->array (make-interval '#(2 2))
                      '(5 6
                        7 8))))
          #t)

       (test (myarray= (array-append
                          1
                          (list (->generalized-array (list->array (make-interval '#(2 2))
                                                        '(1 2
                                                          3 4)))
                             (list->array (make-interval '#(2 2))
                                '(5 6
                                  7 8))))
                (my-array-append
                   1
                   (list->array (make-interval '#(2 2))
                      '(1 2
                        3 4))
                   (list->array (make-interval '#(2 2))
                      '(5 6
                        7 8))))
          #t)

       (test (myarray= (tensor '((4 7)
                                 (2 6)
                                 (1 0)
                                 (0 1)))
                (array-append 0 (list (tensor '((4 7)
                                                (2 6)))
                                   (identity-array 2))))
          #t)

       (test (myarray= (tensor '((4 7)
                                 (2 6)
                                 (1 0)
                                 (0 1)))
                (array-append 0
                   (list (list->array (make-interval '#(2 0) '#(4 2))
                            '(4 7 2 6))
                      (identity-array 2))))
          #t)

       (test (myarray= (tensor '((4 7 1 0)
                                 (2 6 0 1)))
                (array-append 1 (list (tensor '((4 7)
                                                (2 6)))
                                   (identity-array 2))))
          #t)

       (test (myarray= (tensor '((4 7 2 1 0)
                                 (6 3 5 0 1)))
                (array-append 1 (list (tensor '((4 7 2)
                                                (6 3 5)))
                                   (identity-array 2))))
          #t)

       (test (myarray= (tensor '((4 7 1 0 0 1 3)
                                 (2 6 0 1 5 8 9)))
                (array-append
                   1
                   (list (list->array (make-interval '#(2 2))
                            '(4 7 2 6))
                      (identity-array 2)
                      (list->array (make-interval '#(2 3))
                         '(0 1 3 5 8 9)))))
          #t)

       )
    (list array-append array-append!))


 (do ((i 0 (+ i 1)))
     ((= i random-tests))
     (let* ((domain
               (random-interval 1 6))  ;; you can't append zero-dimensional arrays
            (dimension
               (interval-dimension domain))
            (A
               (array-copy (make-array domain (lambda args (random 10)))))
            (domain-widths
               (interval-widths domain))
            (cutting-axis
               (random dimension))
            (tiling-argument
               (vector-map (lambda (k)
                              (let ((kth-width (interval-width domain k)))
                                 (if (=fx k cutting-axis)
                                     (if (zero? kth-width)
                                         (make-vector (random 1 4) 0)
                                         (let loop ((result '())
                                                    (sum 0))
                                            (if (<fx sum kth-width)
                                                (let ((slice-width (random (+ 1 kth-width))))
                                                   (loop (cons slice-width result)
                                                      (+ slice-width sum)))
                                                (vector-permute (list->vector (cons (- (car result) (- sum kth-width))
                                                                                 (cdr result)))
                                                   (random-permutation (length result))))))
                                     (if (zero? kth-width)
                                         '#(0)
                                         kth-width))))
                  (list->vector (iota dimension))))
            (arrays
               (array->list (array-tile A tiling-argument)))
            (A-reconstructed
               (array-append cutting-axis arrays))
            (A-reconstructed!
               (array-append! cutting-axis arrays)))
        (test (myarray= (array-translate A (vector-map -
                                              (interval-lower-bounds->vector (array-domain A-reconstructed))
                                              (interval-lower-bounds->vector (array-domain A))))
                 A-reconstructed)
           #t)
        (test (myarray= (array-translate A (vector-map -
                                              (interval-lower-bounds->vector (array-domain A-reconstructed))
                                              (interval-lower-bounds->vector (array-domain A))))
                 A-reconstructed!)
           #t)))

 (let* ((a (make-array (make-interval '#(4 6)) list))
        (k 2)
        (m (interval-upper-bound (array-domain a) 0))
        (n (interval-upper-bound (array-domain a) 1)))
    (pretty-print
       (array->list* a))
    (newline)
    (pretty-print
       (array->list*
          (array-append
             0
             (list (array-extract a (make-interval (vector k 0) (vector (+ k 1) n)))
                (array-extract a (make-interval (vector k n)))
                (array-extract a (make-interval (vector (+ k 1) 0) (vector m n))))))))


 (next-test-random-source-state!)
)

(define (array-stack-and-array-stack!-tests)
   (pp "array-stack and array-stack! tests")

 (for-each
    (lambda (call/cc-safe?)
       (let ((array-stack
                (if call/cc-safe?
                    array-stack
                    array-stack!))
             (message
                (if call/cc-safe?
                    "array-stack:"     ;; no trailing space
                    "array-stack!:")))

          (define (wrap error-reason)
             (string-append message error-reason))

          (test (array-stack 1 'a)
             (wrap " Expecting a nonnull list of arrays with the same domains as the second argument: "))

          (test (array-stack 1 '())
             (wrap " Expecting a nonnull list of arrays with the same domains as the second argument: "))

          (test (array-stack 1 '(a))
             (wrap " Expecting a nonnull list of arrays with the same domains as the second argument: "))

          (test (array-stack 1 (list (make-array (make-interval '#(1)) list) (make-array (make-interval '#(2 2)) list)))
             (wrap " Expecting a nonnull list of arrays with the same domains as the second argument: "))

          (test (array-stack 1 (list (make-array (make-interval '#(2 2)) list) 'a))
             (wrap " Expecting a nonnull list of arrays with the same domains as the second argument: "))

          (test (array-stack 'a (list (make-array (make-interval '#(2 2)) list) (make-array (make-interval '#(2 2)) list)))
             (wrap " Expecting an exact integer between 0 (inclusive) and the dimension of the arrays (inclusive) as the first argument:"))

          (test (array-stack -1 (list (make-array (make-interval '#(2 2)) list) (make-array (make-interval '#(2 2)) list)))
             (wrap " Expecting an exact integer between 0 (inclusive) and the dimension of the arrays (inclusive) as the first argument:"))

          (test (array-stack 3 (list (make-array (make-interval '#(2 2)) list) (make-array (make-interval '#(2 2)) list)))
             (wrap " Expecting an exact integer between 0 (inclusive) and the dimension of the arrays (inclusive) as the first argument:"))

          (test (array-stack 0
                   (list (make-array (make-interval '#(2 2)) list) (make-array (make-interval '#(2 2)) list))
                   'a)
             (wrap " Expecting a storage class as the third argument: "))

          (test (array-stack 0
                   (list (make-array (make-interval '#(2 2)) list) (make-array (make-interval '#(2 2)) list))
                   u1-storage-class
                   'a)
             (wrap " Expecting a boolean as the fourth argument: "))

          (test (array-stack 0
                   (list (make-array (make-interval '#(2 2)) list) (make-array (make-interval '#(2 2)) list))
                   u1-storage-class
                   #t
                   'a)
             (wrap " Expecting a boolean as the fifth argument: "))


          (test (array-stack 0
                   (list (make-array (make-interval '#(2 2)) list) (make-array (make-interval '#(2 2)) list))
                   u1-storage-class)
             (wrap " Not all elements of the source can be stored in destination: "))

          (test (array-storage-class
                   (array-stack 1 (list (make-array (make-interval '#(10)) list))))
             generic-storage-class)

          (test (array-storage-class
                   (array-stack 1
                      (list (array-copy (make-array (make-interval '#(10)) (lambda (i) (number->uint8 (random-integer 10)))) u8-storage-class)
                         (array-copy (make-array (make-interval '#(10)) (lambda (i) (number->uint16 (random-integer 10)))) u16-storage-class))))
             generic-storage-class)

          (test (myarray= (tensor '(((4 7) (2 6))
                                    ((1 0) (0 1))))
                   (array-stack 0 (list (tensor '((4 7)
                                                  (2 6)))
                                     (identity-array 2))))
             #t)

          (test (myarray= (tensor '(((4 7) (1 0))
                                    ((2 6) (0 1))))
                   (array-stack 1 (list (tensor '((4 7)
                                                  (2 6)))
                                     (identity-array 2))))
             #t)

          (test (myarray= (tensor '(((4 1) (7 0))
                                    ((2 0) (6 1))))
                   (array-stack 2 (list (tensor '((4 7)
                                                  (2 6)))
                                     (identity-array 2))))
             #t)

          (let* ((A
                    (make-array
                       (make-interval '#(4 10))
                       list))
                 (column_
                    (array-getter                  ;; the getter of ...
                       (array-curry                  ;; a 1-D array of the columns of A
                          (array-permute A '#(1 0))
                          1)))
                 (B
                    (array-stack                  ;; stack into a new 2-D array ...
                       1                            ;; along axis 1 (i.e., columns) ...
                       (map column_ '(1 2 5 8)))))  ;; the columns of A you want
             (array-display B))

          (let* ((A
                    (make-array
                       (make-interval '#(4 10))
                       list))
                 (B
                    (array-stack 1 (map (array-getter (array-curry (array-permute A '#(1 0)) 1)) '(1 2 5 8)))))
             (array-display B))
          ))
    '(#t #f))


;;; zero-dimensional and empty arrays

 (let ()

    (define arrays (map (lambda (ignore) (array-copy (make-array (make-interval '#()) (lambda () (random-integer 10))))) (iota 4)))

    (define b  (array-stack 0 arrays))
    (define c  (array-stack! 0 arrays))

    (test (map array-ref arrays)
       (array->list b))
    (test (map array-ref arrays)
       (array->list c)))

 (let* ((arrays (map (lambda (ignore) (array-copy (make-array (make-interval '#(0)) test-error))) (iota 4)))
        (b (array-stack 0 arrays))
        (c (array-stack 1 arrays))
        (b! (array-stack! 0 arrays))
        (c! (array-stack! 1 arrays)))

    (test (interval-upper-bounds->vector (array-domain b))
       '#(4 0))
    (test (interval-upper-bounds->vector (array-domain c))
       '#(0 4))

    (test (interval-upper-bounds->vector (array-domain b!))
       '#(4 0))
    (test (interval-upper-bounds->vector (array-domain c!))
       '#(0 4)))


;;; FIXME: Need to test the values of other optional arguments to array-append

 (define (myarray-stack k . arrays)
    (let* ((array
              (car arrays))
           (domain
              (array-domain array))
           (lowers
              (interval-lower-bounds->list domain))
           (uppers
              (interval-upper-bounds->list domain))
           (new-domain
              (make-interval
                 (list->vector (append (take lowers k) (cons 0 (drop lowers k))))
                 (list->vector (append (take uppers k) (cons (length arrays) (drop uppers k))))))
           (getters
              (list->vector (map %%array-getter arrays))))
       (make-array new-domain
          (lambda args
             (apply
                (vector-ref getters (list-ref args k))
                (append (take args k)
                   (drop args (+ k 1))))))))

 (do ((d 0 (+fx d 1)))
     ((= d 6))
     (let* ((uppers-list
               (iota d))
            (domain
               (make-interval (list->vector uppers-list))))
        (do ((i 0 (+fx i 1)))
            ;; distribute "tests" results over five dimensions
            ((= i (quotient random-tests 5)))
            (let* ((arrays
                      (map (lambda (ignore)
                              (array-copy
                                 (make-array domain
                                    (lambda args
                                       (number->uint8 (random 256))))
                                 u8-storage-class))
                         (iota (random 1 5))))
                   (k
                      (random (+ d 1))))
               (test (myarray= (array-stack k arrays)
                        (apply myarray-stack k arrays))
                  #t)
               (test (myarray= (array-stack! k arrays)
                        (apply myarray-stack k arrays))
                  #t)))))

 (next-test-random-source-state!))

(define (array-block-and-array-block!-tests)
   (pp "array-block and array-block! tests")

 (for-each
    (lambda (call/cc-safe?)
       (let ((array-block
                (if call/cc-safe?
                    array-block
                    array-block!))
             (message
                (if call/cc-safe?
                    "array-block: "
                    "array-block!: ")))

          (define (wrap error-reason)
             (string-append message error-reason))

          (test (array-block 'a)
             (wrap "The first argument is not an array: "))

          (test (array-block (make-array (make-interval '#(2 2)) list) 'a)
             (wrap "The second argument is not a storage class: "))

          (test (array-block (make-array (make-interval '#(2 2)) list)
                   u8-storage-class
                   'a)
             (wrap "The third argument is not a boolean: "))

          (test (array-block (make-array (make-interval '#(2 2)) list)
                   u8-storage-class
                   #f
                   'a)
             (wrap "The fourth argument is not a boolean: "))

          (test (array-block (make-array (make-interval '#(2 2)) list))
             (wrap "Not all elements of the first argument (an array) are arrays: "))

          (test (array-block (vector*->array 1 (vector (vector*->array 1 '#(1 1))
                                                  (vector*->array 2 '#(#(1 2) #(3 4))))))
             (wrap "Not all elements of the first argument (an array) have the same dimension as the first argument itself: "))

          (test (array-block (list*->array
                                2
                                (list (list (list*->array 2 '((0 1)
                                                              (2 3)))
                                         (list*->array 2 '((4)
                                                           (5)))
                                         (list*->array 2 '((6 7)     ;; these should each have ...
                                                           (9 10)))) ;; three elements
                                   (list (list*->array 2 '((12 13)))
                                      (list*->array 2 '((14)))
                                      (list*->array 2 '((15 16 17)))))))
             (wrap "Cannot stack array elements of the first argument into result array: "))


          (test (array? (array-block (list*->array
                                        1
                                        (list (make-array (make-interval '#(0)) list)
                                           (make-array (make-interval '#(0)) list)))))
             #t)


          (let* ((A (list*->array
                       2
                       (list (list (list*->array 2 '((0 1)
                                                     (2 3)))
                                (list*->array 2 '((4)
                                                  (5)))
                                (list*->array 2 '((6 7 8)
                                                  (9 10 11))))
                          (list (list*->array 2 '((12 13)))
                             (list*->array 2 '((14)))
                             (list*->array 2 '((15 16 17)))))))
                 (A-appended
                    (array-block A))
                 (A-tiled
                    (array-tile A-appended '#(#(2 1) #(2 1 3)))))

             (for-each (lambda (mutable?)
                          (for-each (lambda (safe?)
                                       (let ((new-A (array-block A generic-storage-class mutable? safe?)))
                                          (test (array-safe? new-A)
                                             safe?)
                                          (test (mutable-array? new-A)
                                             mutable?)))
                             '(#t #f)))
                '(#t #f))
             (for-each (lambda (mutable?)
                          (for-each (lambda (safe?)
                                       (parameterize ((specialized-array-default-mutable? mutable?)
                                                      (specialized-array-default-safe?    safe?))
                                          (let ((new-A (array-block A generic-storage-class)))
                                             (test (array-safe? new-A)
                                                safe?)
                                             (test (mutable-array? new-A)
                                                mutable?))))
                             '(#t #f)))
                '(#t #f))

             (test (array-every equal?            ;; we convert them to list*'s to ignore domains.
                      (array-map array->list* A)
                      (array-map array->list* A-tiled))
                #t))

          (let* ((A (list*->array
                       2
                       (list (list (list*->array 2 '((0 1)
                                                     (2 3)))
                                (list*->array 2 '((4)
                                                  (5)))
                                (list*->array 2 '((6 7 8)
                                                  (9 10 11))))
                          (list (list*->array 2 '((12 13)))
                             (list*->array 2 '((14)))
                             (list*->array 2 '((15 16 17))))))))
             (test (array-block A u1-storage-class)
                (wrap "Not all elements of the source can be stored in destination: ")))
          ))
    '(#t #f))



 (do ((i 0 (+ i 1)))
     ((= i random-tests))
     (let* ((dims
               (random 1 6))
            (A-uppers
               (list->vector (map (lambda (ignore) (random 3 6)) (iota dims))))
            (A
               (array-copy
                  (make-array (make-interval A-uppers)
                     (lambda args
                        (random 2)))
                  u1-storage-class))
            (A_
               (array-getter A))
            (number-of-cuts
               (array->vector
                  (make-array (make-interval (vector dims))
                     (lambda args (random 3)))))
            (cuts
               (vector-map (lambda (cuts upper)
                              (let ((bitmap (make-vector (+ upper 1) #f)))
                                 (vector-set! bitmap 0 #t)
                                 (vector-set! bitmap upper #t)
                                 (let loop ((i 0))
                                    (if (=fx i cuts)
                                        (let ((result (make-vector (+fx cuts 2))))
                                           (let inner ((l 0)
                                                       (j 0))
                                              (cond ((>fx j cuts)
                                                     (vector-set! result j upper)
                                                     result)
                                                    ((vector-ref bitmap l)
                                                     (vector-set! result j l)
                                                     (inner (+fx l 1)
                                                        (+fx j 1)))
                                                    (else
                                                     (inner (+fx l 1)
                                                        j)))))
                                        (let ((proposed-cut (random upper)))
                                           (if (vector-ref bitmap proposed-cut)
                                               (loop i)
                                               (begin
                                                  (vector-set! bitmap proposed-cut #t)
                                                  (loop (+fx i 1)))))))))
                  number-of-cuts
                  A-uppers))
            (side-lengths
               (vector-map
                  (lambda (cuts)
                     (let ((result
                              (make-vector (- (vector-length cuts) 1))))
                        (do ((i 0 (+fx i 1)))
                            ((=fx i (vector-length result)) result)
                            (vector-set! result i (- (vector-ref cuts (+ i 1))
                                                     (vector-ref cuts i))))))
                  cuts))
            (A-blocks
               (make-array (make-interval (vector-map (lambda (v)
                                                         (- (vector-length v) 1))
                                             cuts))
                  (lambda args
                     (let ((vector-args (list->vector args)))
                        (make-array (make-interval (vector-map (lambda (cuts i)
                                                                  (vector-ref cuts i))
                                                      cuts
                                                      vector-args)
                                       (vector-map (lambda (cuts i)
                                                      (vector-ref cuts (+ i 1)))
                                          cuts
                                          vector-args))
                           A_)))))
            (A-tiled
               (array-tile A side-lengths))
            (reconstructed-A
               (array-block A-blocks u1-storage-class))
            (reconstructed-A!
               (array-block! A-blocks u1-storage-class)))
        (test (array-every myarray= A-tiled A-blocks)
           #t)
        (test (array-every = A reconstructed-A)
           #t)
        (test (array-every = A reconstructed-A!)
           #t)
        (test (array-every = A
                 (array-block
                    (array-tile A
                       (list->vector
                          (map (lambda (ignore) (random 1 5))
                             (iota dims))))))
           #t)
        (test (array-every = A
                 (array-block!
                    (array-tile A
                       (list->vector
                          (map (lambda (ignore) (random 1 5))
                             (iota dims))))))
           #t)))

 (next-test-random-source-state!)

;;; Let's do something similar now with possibly empty arrays and subarrays.

 (do ((i 0 (+ i 1)))
     ((= i random-tests))
     (let* ((domain
               (random-interval))
            (domain-widths
               (interval-widths domain))
            (tiling-argument
               (vector-map (lambda (width)
                              (if (zero? width)                  ;; width of kth axis is 0
                                  (make-vector (random 1 3) 0)
                                  (if (even? (random 2))
                                      (let loop ((result '())    ;; accumulate a list of nonnegative integers that (eventually) sum to no less than width
                                                 (sum 0))
                                         (if (<= width sum)
                                             (vector-permute (list->vector (cons (- (car result)    ;; adjust last entry so the sum is width
                                                                                    (- sum width))
                                                                              (cdr result)))
                                                (random-permutation (length result)))  ;; randomly permute vector of cuts
                                             (let ((new-width (random (+ width 1))))
                                                (loop (cons new-width result)
                                                   (+ new-width sum)))))
                                      (random 1 (+ width 3)))))               ;; a positive scalar
                  domain-widths))
            (A
               (array-copy (make-array domain (lambda args (random 10)))))
            (A-tiled
               (array-tile A tiling-argument))
            (A-tiled
               (array-map (lambda (A) (make-array (array-domain A) (array-getter A))) A-tiled))
            (A-blocked!
               (array-block! A-tiled))
            (A-blocked
               (array-block A-tiled)))
        (test (myarray= (array-translate A (vector-map - (interval-lower-bounds->vector (array-domain A)))) ;; array-block returns an array based at the origin
                 A-blocked!)
           #t)
        (test (myarray= (array-translate A (vector-map - (interval-lower-bounds->vector (array-domain A)))) ;; array-block returns an array based at the origin
                 A-blocked)
           #t)))

 (next-test-random-source-state!))