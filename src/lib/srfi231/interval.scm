#|
SRFI 231: Intervals and Generalized Arrays

Copyright 2022, Joseph Donaldson
Bigloo Adaption

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

(module srfi231-interval
   (import srfi231-utils)
   (include "case-lambda.sci"
            "declare.sci")
   (export
      ;;; An interval is a cross product of multi-indices
      ;;; [l_0,u_0) x [l_1,u_1) x ... x [l_n-1,u_n-1)
      ;;; where l_i < u_i for 0 <= i < n, and n >= 0 is the dimension of the interval
      (class <interval>
         (dimension::long read-only)
         volume
         (lower-bounds::vector read-only)
         (upper-bounds::vector read-only))
   
      make-interval
      (interval? x)
      (interval-dimension interval)
      (interval-lower-bound interval i)
      (interval-upper-bound interval i)
      (interval-width interval k)
      (interval-widths interval)
      (interval-lower-bounds->vector interval)
      (interval-upper-bounds->vector interval)
      (interval-lower-bounds->list interval)
      (interval-upper-bounds->list interval)
      (interval-projections interval right-dimension)
      (interval-volume interval)
      (interval-empty? interval)
      (interval-permute interval permutation)
      (interval-translate interval translation)
      (interval-scale interval scales)
      (interval-cartesian-product #!rest intervals)
      (interval-dilate interval lower-diffs upper-diffs)
      (interval= interval1 interval2)
      (interval-subset? interval1 interval2)
      (interval-intersect interval #!rest intervals)
      (interval-contains-multi-index? interval #!rest multi-index)
      (interval-for-each f interval)
      (interval-fold-left f operator identity interval)
      (interval-fold-right f operator identity interval)

      (index-rotate n k)
      (index-first n k)
      (index-last n k)
      (index-swap n i j)
      
      (translation? translation)
      (permutation? permutation)


      
      (%%interval-empty? interval)
      (%%interval-fold-left f operator identity interval)
      (inline %%interval-dimension intv::<interval>)
      (inline %%interval-%%volume intv::<interval>)
      (inline %%interval-%%volume-set! intv::<interval> v)
      (inline %%interval-lower-bounds intv::<interval>)
      (inline %%interval-upper-bounds intv::<interval>)
      (%%interval-lower-bound interval i)
      (%%interval-upper-bound interval i)
      (%%interval-width interval k)
      (%%interval-lower-bounds->list interval)
      (%%interval-upper-bounds->vector interval)
      (%%interval-upper-bounds->list interval)
      (%%interval-contains-multi-index?-1 interval i)
      (%%interval-contains-multi-index?-2 interval i j)
      (%%interval-contains-multi-index?-3 interval i j k)
      (%%interval-contains-multi-index?-4 interval i j k l)
      (%%interval-contains-multi-index?-general interval multi-index)
      (%%interval->basic-indexer interval)
      (%%interval-volume interval)
      (%%interval= interval1 interval2)
      (%%interval-for-each f interval)
      (%%interval-subset? interval1 interval2)
      (%%finish-interval lower-bounds upper-bounds)
      (%%interval-widths interval)
      (%%interval-translate Interval translation)
      (%%vector-permute->list vector permutation)
      (%%permutation-invert permutation)
      (%%interval-permute interval permutation)
      (%%interval-scale interval scales)
      (%%interval-cartesian-product intervals)
      (%%interval-projections interval right-dimension)
      (%%interval-fold-right f operator identity interval)
      (%%index-rotate n k)
      (%%index-first n k)
      (%%interval-lower-bounds->vector interval)
      (%%index-last n k)
      (%%indexer-0 base)
      (%%indexer-1 base
                     low-0
                     increment-0)
      (%%indexer-2 base
                     low-0       low-1
                     increment-0 increment-1)
      (%%indexer-3 base
                     low-0       low-1       low-2
                     increment-0 increment-1 increment-2)
      (%%indexer-4 base
                     low-0       low-1       low-2       low-3
                     increment-0 increment-1 increment-2 increment-3)
      (%%indexer-generic base lower-bounds increments)
      (%%vector-permute vector permutation)))


(define (make-%%interval dimension volume lower-bounds upper-bounds)
   (instantiate::<interval> (dimension dimension) (volume volume)
                            (lower-bounds lower-bounds)
                            (upper-bounds upper-bounds)))

(define-inline (%%interval-dimension intv::<interval>)
   (-> intv dimension))

(define-inline (%%interval-%%volume intv::<interval>)
   (-> intv volume))

(define-inline (%%interval-%%volume-set! intv::<interval> v)
   (set! (-> intv volume) v))

(define-inline (%%interval-lower-bounds intv::<interval>)
   (-> intv lower-bounds))

(define-inline (%%interval-upper-bounds intv::<interval>)
   (-> intv upper-bounds))

;;; requires vector-map, vector-copy function

;;; requires vector-concatenate function

;;; requires exact-integer? function

;;; requires iota, drop, take from SRFI-1

;;; requires fixnum? and flonum?
(define (interval? x)
  (isa? x <interval>))

(define %%vector-of-zeros
  '#(#()
     #(0)
     #(0 0)
     #(0 0 0)
     #(0 0 0 0)))

(define (%%finish-interval lower-bounds upper-bounds)
  ;; Requires that lower-bounds and upper-bounds are not user visible and therefore
  ;; not user modifiable.
   (instantiate::<interval> (dimension (vector-length upper-bounds))
                            (volume #f)
                            (lower-bounds lower-bounds)
                            (upper-bounds upper-bounds)))

(define make-interval
  (case-lambda
   ((upper-bounds)
    (cond ((not (and (vector? upper-bounds)
                     (%%vector-every (lambda (x) (exact-integer? x)) upper-bounds)
                     (%%vector-every (lambda (x) (not (negative? x))) upper-bounds)))
           (srfi231-error "make-interval: The argument is not a vector of nonnegative exact integers: " upper-bounds))
          (else
           (let ((dimension (vector-length upper-bounds)))
             (%%finish-interval (if (<fx dimension 5)
                                    (vector-ref %%vector-of-zeros dimension)
                                    (make-vector dimension 0))
                                (vector-copy upper-bounds))))))
   ((lower-bounds upper-bounds)
    (cond ((not (and (vector? lower-bounds)
                     (%%vector-every (lambda (x) (exact-integer? x)) lower-bounds)))
           (srfi231-error "make-interval: The first argument is not a vector of exact integers: " lower-bounds upper-bounds))
          ((not (and (vector? upper-bounds)
                     (%%vector-every (lambda (x) (exact-integer? x)) upper-bounds)))
           (srfi231-error "make-interval: The second argument is not a vector of exact integers: " lower-bounds upper-bounds))
          ((not (=fx (vector-length lower-bounds)
                     (vector-length upper-bounds)))
           (srfi231-error "make-interval: The first and second arguments are not the same length: " lower-bounds upper-bounds))
          ((not (%%vector-every (lambda (x y) (<= x y)) lower-bounds upper-bounds))
           (srfi231-error "make-interval: Each lower-bound must be no greater than the associated upper-bound: " lower-bounds upper-bounds))
          (else
           (%%finish-interval (vector-copy lower-bounds)
                              (vector-copy upper-bounds)))))))

(define (%%interval-lower-bound interval i)
  (vector-ref (%%interval-lower-bounds interval) i))

(define (%%interval-upper-bound interval i)
  (vector-ref (%%interval-upper-bounds interval) i))

(define (%%interval-width interval k)
  (- (%%interval-upper-bound interval k)
     (%%interval-lower-bound interval k)))

(define (%%interval-widths interval)
  (vector-map (lambda (x y) (- x y))
              (%%interval-upper-bounds interval)
              (%%interval-lower-bounds interval)))

(define (%%interval-lower-bounds->vector interval)
  (vector-copy (%%interval-lower-bounds interval)))

(define (%%interval-upper-bounds->vector interval)
  (vector-copy (%%interval-upper-bounds interval)))

(define (%%interval-lower-bounds->list interval)
  (vector->list (%%interval-lower-bounds interval)))

(define (%%interval-upper-bounds->list interval)
  (vector->list (%%interval-upper-bounds interval)))

(define-macro (macro-make-index-tables)
  `(begin
     (define %%index-rotates
       ',(list->vector
          (map (lambda (n)
                 (list->vector
                  (map (lambda (k)
                         (list->vector
                          (let ((identity-permutation (iota n)))
                            (append (drop identity-permutation k)
                                    (take identity-permutation k)))))
                       (iota (+ n 1)))))
               (iota 5))))

     (define %%index-firsts
       ',(list->vector
          (map (lambda (n)
                 (list->vector
                  (map (lambda (k)
                         (list->vector
                          (let ((identity-permutation (iota n)))
                            (cons k
                                  (append (take identity-permutation k)
                                          (drop identity-permutation (+fx k 1)))))))
                       (iota n))))
               (iota 5))))

     (define %%index-lasts
       ',(list->vector
          (map (lambda (n)
                 (list->vector
                  (map (lambda (k)
                         (list->vector
                          (let ((identity-permutation (iota n)))
                            (append (take identity-permutation k)
                                    (drop identity-permutation (+fx k 1))
                                    (list k)))))
                       (iota n))))
               (iota 5))))
     (define %%index-swaps
       `,(list->vector
          (map (lambda (n)
                 (list->vector
                  (map (lambda (i)
                         (list->vector
                          (map (lambda (j)
                                 (let ((result (list->vector (iota n))))
                                   (vector-set! result i j)
                                   (vector-set! result j i)
                                   result))
                               (iota n))))
                       (iota n))))
               (iota 5))))))

(macro-make-index-tables)

(define (%%index-rotate n k)
  (if (<fx n 5)
      (vector-ref (vector-ref %%index-rotates n) k)
      (let ((identity-permutation (iota n)))
        (list->vector (append (drop identity-permutation k)
                              (take identity-permutation k))))))

(define (%%index-first n k)
  (if (<fx n 5)
      (vector-ref (vector-ref %%index-firsts n) k)
      (let ((identity-permutation (iota n)))
        (list->vector (cons k
                            (append (take identity-permutation k)
                                    (drop identity-permutation (+fx k 1))))))))

(define (%%index-last n k)
  (if (<fx n 5)
      (vector-ref (vector-ref %%index-lasts n) k)
      (let ((identity-permutation (iota n)))
        (list->vector (append (take identity-permutation k)
                              (drop identity-permutation (+fx k 1))
                              (list k))))))

(define (%%index-swap n i j)
  (if (<fx n 5)
      (vector-ref (vector-ref (vector-ref %%index-swaps n) i) j)
      (let ((result (list->vector (iota n))))
        (vector-set! result i j)
        (vector-set! result j i)
        result)))

(define (%%interval-empty? interval)
  (eqv? (%%interval-volume interval) 0))

(define (%%interval-volume interval)
  (or (%%interval-%%volume interval)
      (%%compute-interval-volume interval)))

(declare (not inline))

(define (%%compute-interval-volume interval)
  (let* ((upper-bounds
              (%%interval-upper-bounds interval))
             (lower-bounds
              (%%interval-lower-bounds interval))
             (dimension
              (%%interval-dimension interval))
             (volume
              (do ((i (-fx dimension 1) (-fx i 1))
                   (result 1 (* result (- (vector-ref upper-bounds i)
                                          (vector-ref lower-bounds i)))))
                  ((<fx i 0) result))))
        (%%interval-%%volume-set! interval volume)
        volume))

(define (index-rotate n k)
  (cond ((not (and (fixnum? n)
                   (<=fx 0 n)))
         (srfi231-error "index-rotate: The first argument is not a nonnegative fixnum: " n k))
        ((not (and (fixnum? k)
                   (<=fx3 0 k n)))
         (srfi231-error "index-rotate: The second argument is not a fixnum between 0 and the first argument (inclusive): " n k))
        (else
         (%%index-rotate n k))))

(define (index-first n k)
  (cond ((not (and (fixnum? n)
                   (positivefx? n)))
         (srfi231-error "index-first: The first argument is not a positive fixnum: " n k))
        ((not (and (fixnum? k)
                   (<=fx 0 k)
                   (<fx k n)))
         (srfi231-error "index-first: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): " n k))
        (else
         (%%index-first n k))))

(define (index-last n k)
  (cond ((not (and (fixnum? n)
                   (positivefx? n)))
         (srfi231-error "index-last: The first argument is not a positive fixnum: " n k))
        ((not (and (fixnum? k)
                   (<=fx 0 k)
                   (<fx k n)))
         (srfi231-error "index-last: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): " n k))
        (else
         (%%index-last n k))))

(define (index-swap n i j)
  (cond ((not (and (fixnum? n)
                   (positivefx? n)))
         (srfi231-error "index-swap: The first argument is not a positive fixnum: " n i j))
        ((not (and (fixnum? i) (<=fx 0 i) (<fx i n)))
         (srfi231-error "index-swap: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): " n i j))
        ((not (and (fixnum? j) (<=fx 0 j) (<fx j n)))
         (srfi231-error "index-swap: The third argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): " n i j))
        (else
         (%%index-swap n i j))))

(define (interval-dimension interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-dimension: The argument is not an interval: " interval))
        (else
         (%%interval-dimension interval))))

(define (interval-lower-bound interval i)
  (cond ((not (interval? interval))
         (srfi231-error "interval-lower-bound: The first argument is not an interval: " interval i))
        ((not (and (fixnum? i)
                   (<fx3 -1 i (%%interval-dimension interval))))
         (srfi231-error "interval-lower-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): " interval i))
        (else
         (%%interval-lower-bound interval i))))

(define (interval-upper-bound interval i)
  (cond ((not (interval? interval))
         (srfi231-error "interval-upper-bound: The first argument is not an interval: " interval i))
        ((not (and (fixnum? i)
                   (<fx3 -1 i (%%interval-dimension interval))))
         (srfi231-error "interval-upper-bound: The second argument is not an exact integer between 0 (inclusive) and (interval-dimension interval) (exclusive): " interval i))
        (else
         (%%interval-upper-bound interval i))))

(define (interval-width interval k)
  (cond ((not (interval? interval))
         (srfi231-error "interval-width: The first argument is not an interval: " interval k))
        ((not (and (fixnum? k)
                   (<fx3 -1 k (%%interval-dimension interval))))
         (srfi231-error "interval-width: The second argument is not an exact integer between 0 (inclusive) and the dimension of the first argument (exclusive): " interval k))
        (else
         (%%interval-width interval k))))

(define (interval-widths interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-widths: The argument is not an interval: " interval))
        (else
         (%%interval-widths interval))))

(define (interval-lower-bounds->vector interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-lower-bounds->vector: The argument is not an interval: " interval))
        (else
         (%%interval-lower-bounds->vector interval))))

(define (interval-upper-bounds->vector interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-upper-bounds->vector: The argument is not an interval: " interval))
        (else
         (%%interval-upper-bounds->vector interval))))

(define (interval-lower-bounds->list interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-lower-bounds->list: The argument is not an interval: " interval))
        (else
         (%%interval-lower-bounds->list interval))))

(define (interval-upper-bounds->list interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-upper-bounds->list: The argument is not an interval: " interval))
        (else
         (%%interval-upper-bounds->list interval))))

(define (interval-projections interval right-dimension)
  (cond ((not (interval? interval))
         (srfi231-error "interval-projections: The first argument is not an interval: " interval right-dimension))
        ((not (and (fixnum? right-dimension)
                   (<=fx3 0 right-dimension (%%interval-dimension interval))))
         (srfi231-error "interval-projections: The second argument is not an exact integer between 0 and the dimension of the first argument (inclusive): " interval right-dimension))
        (else
         (%%interval-projections interval right-dimension))))

(define (%%interval-projections interval right-dimension)
  (let ((left-dimension
         (-fx (%%interval-dimension interval) right-dimension))
        (lowers
         (%%interval-lower-bounds->list interval))
        (uppers
         (%%interval-upper-bounds->list interval)))
    (values (%%finish-interval (list->vector (take lowers left-dimension))
                               (list->vector (take uppers left-dimension)))
            (%%finish-interval (list->vector (drop lowers left-dimension))
                               (list->vector (drop uppers left-dimension))))))

(define (interval-volume interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-volume: The argument is not an interval: " interval))
        (else
         (%%interval-volume interval))))

(define (interval-empty? interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-empty?: The argument is not an interval: " interval))
        (else
         (%%interval-empty? interval))))

(define (permutation? permutation)
  (and (vector? permutation)
       (let* ((n (vector-length permutation))
              (permutation-range (make-vector n #f)))
         ;; we'll write things into permutation-range
         ;; each box should be written only once
         (let loop ((i 0))
           (or (=fx i n)
               (let ((p_i (vector-ref permutation i)))
                 (and (fixnum? p_i) ;; a permutation index can't be a bignum
                      (<fx3 -1 p_i n)
                      (not (vector-ref permutation-range p_i))
                      (let ()
                        (vector-set! permutation-range p_i #t)
                        (loop (+fx i 1))))))))))

(define (%%vector-permute vector permutation)
  (let* ((n (vector-length vector))
         (result (make-vector n)))
    (do ((i 0 (+fx i 1)))
        ((=fx i n) result)
      (vector-set! result i (vector-ref vector (vector-ref permutation i))))))

(define (%%vector-permute->list vector permutation)
  (do ((i (-fx (vector-length vector) 1) (-fx i 1))
       (result '() (cons (vector-ref vector (vector-ref permutation i))
                         result)))
      ((<fx i 0) result)))

(define (%%permutation-invert permutation)
  (let* ((n (vector-length permutation))
         (result (make-vector n)))
    (do ((i 0 (+fx i 1)))
        ((=fx i n) result)
      (vector-set! result (vector-ref permutation i) i))))

(define (%%interval-permute interval permutation)
  (%%finish-interval (%%vector-permute (%%interval-lower-bounds interval) permutation)
                     (%%vector-permute (%%interval-upper-bounds interval) permutation)))

(define (interval-permute interval permutation)
  (cond ((not (interval? interval))
         (srfi231-error "interval-permute: The first argument is not an interval: " interval permutation))
        ((not (permutation? permutation))
         (srfi231-error "interval-permute: The second argument is not a permutation: " interval permutation))
        ((not (=fx (%%interval-dimension interval) (vector-length permutation)))
         (srfi231-error "interval-permute: The dimension of the first argument (an interval) does not equal the length of the second (a permutation): " interval permutation))
        (else
         (%%interval-permute interval permutation))))

(define (translation? translation)
  (and (vector? translation)
       (%%vector-every (lambda (x) (exact-integer? x)) translation)))

(define (interval-translate interval translation)
  (cond ((not (interval? interval))
         (srfi231-error "interval-translate: The first argument is not an interval: " interval translation))
        ((not (translation? translation))
         (srfi231-error "interval-translate: The second argument is not a vector of exact integers: " interval translation))
        ((not (=fx (%%interval-dimension interval)
                   (vector-length translation)))
         (srfi231-error "interval-translate: The dimension of the first argument (an interval) does not equal the length of the second (a vector): " interval translation))
        (else
         (%%interval-translate interval translation))))

(define (%%interval-translate Interval translation)
  (%%finish-interval (vector-map (lambda (x y) (+ x y)) (%%interval-lower-bounds Interval) translation)
                     (vector-map (lambda (x y) (+ x y)) (%%interval-upper-bounds Interval) translation)))

(define (%%interval-scale interval scales)
  (let* ((uppers (%%interval-upper-bounds interval))
         (lowers (%%interval-lower-bounds interval))
         (new-uppers (vector-map (lambda (u s)
                                   (quotient (+ u s -1) s))
                                 uppers scales)))
    ;; lowers is not newly allocated, but it's already been copied because it's the
    ;; lower bounds of an existing interval
    (%%finish-interval lowers new-uppers)))

(define (interval-scale interval scales)
  (cond ((not (and (interval? interval)
                   (%%vector-every (lambda (x) (eqv? 0 x)) (%%interval-lower-bounds interval))))
         (srfi231-error "interval-scale: The first argument is not an interval with all lower bounds zero: " interval scales))
        ((not (and (vector? scales)
                   (%%vector-every (lambda (x) (exact-integer? x)) scales)
                   (%%vector-every (lambda (x) (positive? x)) scales)))
         (srfi231-error "interval-scale: The second argument is not a vector of positive, exact, integers: " interval scales))
        ((not (=fx (vector-length scales) (%%interval-dimension interval)))
         (srfi231-error "interval-scale: The dimension of the first argument (an interval) is not equal to the length of the second (a vector): "
                interval scales))
        (else
         (%%interval-scale interval scales))))

(define (%%interval-cartesian-product intervals)
  ;; Even if there is only one interval, its lower and upper bounds have already been copied.
  (%%finish-interval (vector-concatenate (map %%interval-lower-bounds intervals))
                     (vector-concatenate (map %%interval-upper-bounds intervals))))

(define (interval-cartesian-product #!rest intervals)
  (cond ((not (%%every interval? intervals))
         (apply srfi231-error "interval-cartesian-product: Not all arguments are intervals: " intervals))
        (else
         (%%interval-cartesian-product intervals))))

(define (interval-dilate interval lower-diffs upper-diffs)
  (cond ((not (interval? interval))
         (srfi231-error "interval-dilate: The first argument is not an interval: " interval lower-diffs upper-diffs))
        ((not (and (vector? lower-diffs)
                   (%%vector-every (lambda (x) (exact-integer? x)) lower-diffs)))
         (srfi231-error "interval-dilate: The second argument is not a vector of exact integers: " interval lower-diffs upper-diffs))
        ((not (and (vector? upper-diffs)
                   (%%vector-every (lambda (x) (exact-integer? x)) upper-diffs)))
         (srfi231-error "interval-dilate: The third argument is not a vector of exact integers: " interval lower-diffs upper-diffs))
        ((not (=fx3 (vector-length lower-diffs)
                   (vector-length upper-diffs)
                   (%%interval-dimension interval)))
         (srfi231-error "interval-dilate: The second and third arguments must have the same length as the dimension of the first argument: " interval lower-diffs upper-diffs))
        (else
         (let ((new-lower-bounds (vector-map (lambda (x y) (+ x y)) (%%interval-lower-bounds interval) lower-diffs))
               (new-upper-bounds (vector-map (lambda (x y) (+ x y)) (%%interval-upper-bounds interval) upper-diffs)))
           (if (%%vector-every (lambda (x y) (<= x y)) new-lower-bounds new-upper-bounds)
               (%%finish-interval new-lower-bounds new-upper-bounds)
               (srfi231-error "interval-dilate: Some resulting lower bounds are greater than corresponding upper bounds: " interval lower-diffs upper-diffs))))))

(define (%%interval= interval1 interval2)
  ;; This can be used a fair amount, so we open-code it
  (or (eq? interval1 interval2)
      (and (let ((upper1 (%%interval-upper-bounds interval1))
                 (upper2 (%%interval-upper-bounds interval2)))
             (or (eq? upper1 upper2)
                 (and (=fx (vector-length upper1) (vector-length upper2))
                      (%%vector-every (lambda (x y) (= x y)) upper1 upper2))))
           (let ((lower1 (%%interval-lower-bounds interval1))
                 (lower2 (%%interval-lower-bounds interval2)))
             (or (eq? lower1 lower2)
                 ;; We don't need to check that the two lower bounds
                 ;; are the same length after checking the upper bounds
                 (%%vector-every (lambda (x y) (= x y)) lower1 lower2))))))

(define (interval= interval1 interval2)
  (cond ((not (and (interval? interval1)
                   (interval? interval2)))
         (srfi231-error "interval=: Not all arguments are intervals: " interval1 interval2))
        (else
         (%%interval= interval1 interval2))))

(define (%%interval-subset? interval1 interval2)
  (and (%%vector-every (lambda (x y) (>= x y)) (%%interval-lower-bounds interval1) (%%interval-lower-bounds interval2))
       (%%vector-every (lambda (x y) (<= x y)) (%%interval-upper-bounds interval1) (%%interval-upper-bounds interval2))))

(define (interval-subset? interval1 interval2)
  (cond ((not (and (interval? interval1)
                   (interval? interval2)))
         (srfi231-error "interval-subset?: Not all arguments are intervals: " interval1 interval2))
        ((not (=fx (%%interval-dimension interval1)
                   (%%interval-dimension interval2)))
         (srfi231-error "interval-subset?: The arguments do not have the same dimension: " interval1 interval2))
        (else
         (%%interval-subset? interval1 interval2))))

(define (%%interval-intersect intervals)
  (let ((lower-bounds (apply vector-map max (map %%interval-lower-bounds intervals)))
        (upper-bounds (apply vector-map min (map %%interval-upper-bounds intervals))))
    (and (%%vector-every (lambda (x y) (<= x y)) lower-bounds upper-bounds)
         (%%finish-interval lower-bounds upper-bounds))))

(define (interval-intersect interval #!rest intervals)
  (if (null? intervals)
      (if (interval? interval)
          interval
          (srfi231-error "interval-intersect: The argument is not an interval: " interval))
      (let ((intervals (cons interval intervals)))
        (cond ((not (%%every interval? intervals))
               (apply srfi231-error "interval-intersect: Not all arguments are intervals: " intervals))
              ((let* ((dims (map %%interval-dimension intervals))
                      (dim1 (car dims)))
                 (not (%%every (lambda (dim) (=fx dim dim1)) (cdr dims))))
               (apply srfi231-error "interval-intersect: Not all arguments have the same dimension: " intervals))
              (else
               (%%interval-intersect intervals))))))

(declare (inline))

(define (%%interval-contains-multi-index?-1 interval i)
  (and (<= (%%interval-lower-bound interval 0) i) (< i (%%interval-upper-bound interval 0))))

(define (%%interval-contains-multi-index?-2 interval i j)
  (and (<= (%%interval-lower-bound interval 0) i) (< i (%%interval-upper-bound interval 0))
       (<= (%%interval-lower-bound interval 1) j) (< j (%%interval-upper-bound interval 1))))

(define (%%interval-contains-multi-index?-3 interval i j k)
  (and (<= (%%interval-lower-bound interval 0) i) (< i (%%interval-upper-bound interval 0))
       (<= (%%interval-lower-bound interval 1) j) (< j (%%interval-upper-bound interval 1))
       (<= (%%interval-lower-bound interval 2) k) (< k (%%interval-upper-bound interval 2))))

(define (%%interval-contains-multi-index?-4 interval i j k l)
  (and (<= (%%interval-lower-bound interval 0) i) (< i (%%interval-upper-bound interval 0))
       (<= (%%interval-lower-bound interval 1) j) (< j (%%interval-upper-bound interval 1))
       (<= (%%interval-lower-bound interval 2) k) (< k (%%interval-upper-bound interval 2))
       (<= (%%interval-lower-bound interval 3) l) (< l (%%interval-upper-bound interval 3))))

(declare (not inline))

(define (%%interval-contains-multi-index?-general interval multi-index)
  (let loop ((i 0)
             (multi-index multi-index))
    (or (null? multi-index)
        (let ((component (car multi-index)))
          (and (<= (%%interval-lower-bound interval i) component)
               (< component (%%interval-upper-bound interval i))
               (loop (+fx i 1)
                     (cdr multi-index)))))))

(define (interval-contains-multi-index? interval #!rest multi-index)

  ;; this is relatively slow, but (a) I haven't seen a need to use it yet, and (b) this formulation
  ;; significantly simplifies testing the error checking

  (cond ((not (interval? interval))
         (srfi231-error "interval-contains-multi-index?: The first argument is not an interval: " interval))
        ((not (=fx (%%interval-dimension interval)
                   (length multi-index)))
         (apply srfi231-error "interval-contains-multi-index?: The dimension of the first argument (an interval) does not match number of indices: " interval multi-index))
        ((not (%%every (lambda (x) (exact-integer? x)) multi-index))
         (apply srfi231-error "interval-contains-multi-index?: At least one multi-index component is not an exact integer: " interval multi-index))
        (else
         (%%interval-contains-multi-index?-general interval multi-index))))

;;; Applies f to every element of the domain; assumes that f is thread-safe,
;;; the order of application is not specified

(define (interval-for-each f interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-for-each: The second argument is not a interval: " interval))
        ((not (procedure? f))
         (srfi231-error "interval-for-each: The first argument is not a procedure: " f))
        (else
         (%%interval-for-each f interval))))

(define (%%interval-for-each f interval)
  (%%interval-fold-left f
                        (lambda (ignore f_i)
                          #t)   ;; just compute (apply f multi-index)
                        'ignore
                        interval)
  (void))

;;; Calculates
;;;
;;; (...(operator (operator (operator identity (f multi-index_1)) (f multi-index_2)) (f multi-index_3)) ...)
;;;
;;; where multi-index_1, multi-index_2, ... are the elements of interval in lexicographical order

(define (interval-fold-left f operator identity interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-fold-left: The fourth argument is not an interval: " f operator identity interval))
        ((not (procedure? operator))
         (srfi231-error "interval-fold-left: The second argument is not a procedure: " f operator identity interval))
        ((not (procedure? f))
         (srfi231-error "interval-fold-left: The first argument is not a procedure: " f operator identity interval))
        (else
         (%%interval-fold-left f operator identity interval))))

(define (%%interval-fold-left f operator identity interval)

  (define-macro (generate-code-ifl)

    (define (symbol-append . args)
      (string->symbol
       (apply string-append (map (lambda (x)
                                   (cond ((symbol? x) (symbol->string x))
                                         ((number? x) (number->string x))
                                         ((string? x) x)
                                         (else (srfi231-error "Arghh!"))))
                                 args))))


      (define (make-lower k)
        (symbol-append 'lower- k))

      (define (make-upper k)
        (symbol-append 'upper- k))

      (define (make-arg k)
        (symbol-append 'i_ k))

      (define (make-loop-name k)
        (symbol-append 'loop- k))

      (define (make-loop index depth k)
        `(let ,(make-loop-name index) ((,(make-arg index) ,(make-lower index))
                                       (result result))
              (if (= ,(make-arg index) ,(make-upper index))
                  ,(if (= index 0)
                       `result
                       `(,(make-loop-name (- index 1)) (+ ,(make-arg (- index 1)) 1) result))
                  ,(if (= depth 0)
                       `(,(make-loop-name index) (+ ,(make-arg index) 1) (operator result (f ,@(map (lambda (i) (make-arg i)) (iota k)))))
                       (make-loop (+ index 1) (- depth 1) k)))))

      (define (do-one-case k)
        (let ((result
               `((,k)
                 (let (,@(map (lambda (j)
                                `(,(make-lower j) (%%interval-lower-bound interval ,j)))
                              (iota k))
                       ,@(map (lambda (j)
                                `(,(make-upper j) (%%interval-upper-bound interval ,j)))
                              (iota k))
                       (result identity))
                   ,(make-loop 0 (- k 1) k)))))
          result))

      `(case (%%interval-dimension interval)
         ((0) (operator identity (f)))
         ,@(map do-one-case (iota 8 1))
         (else
          (let ()

            (define (get-next-args reversed-args
                                   reversed-lowers
                                   reversed-uppers)
              (let ((next-index (+ (car reversed-args) 1)))
                (if (< next-index (car reversed-uppers))
                    (cons next-index (cdr reversed-args))
                    (and (not (null? (cdr reversed-args)))
                         (let ((tail-result (get-next-args (cdr reversed-args)
                                                           (cdr reversed-lowers)
                                                           (cdr reversed-uppers))))
                           (and tail-result
                                (cons (car reversed-lowers) tail-result)))))))

            (let ((reversed-lowers (reverse (%%interval-lower-bounds->list interval)))
                  (reversed-uppers (reverse (%%interval-upper-bounds->list interval))))
              (let loop ((reversed-args reversed-lowers)
                         (result identity))
             ;;; There's at least one element of the interval, so we can
             ;;; use a do-until loop
                (let ((result (operator result (apply f (reverse reversed-args))))
                      (next-reversed-args (get-next-args reversed-args
                                                         reversed-lowers
                                                         reversed-uppers)))
                  (if next-reversed-args
                      (loop next-reversed-args result)
                      result))))))))

  (if (%%interval-empty? interval) ;; handle (make-interval '#(10000000 10000000 0)) efficiently
      identity
      (generate-code-ifl)))

(define (interval-fold-right f operator identity interval)
  (cond ((not (interval? interval))
         (srfi231-error "interval-fold-right: The fourth argument is not an interval: " f operator identity interval))
        ((not (procedure? operator))
         (srfi231-error "interval-fold-right: The second argument is not a procedure: " f operator identity interval))
        ((not (procedure? f))
         (srfi231-error "interval-fold-right: The first argument is not a procedure: " f operator identity interval))
        (else
         (%%interval-fold-right f operator identity interval))))

(define (%%interval-fold-right f operator identity interval)

  (declare (not lambda-lift))

  (define-macro (generate-code)

    (define (symbol-append . args)
      (string->symbol
       (apply string-append (map (lambda (x)
                                   (cond ((symbol? x) (symbol->string x))
                                         ((number? x) (number->string x))
                                         ((string? x) x)
                                         (else (srfi231-error "Arghh!"))))
                                 args))))


      (define (make-lower k)
        (symbol-append 'lower- k))

      (define (make-upper k)
        (symbol-append 'upper- k))

      (define (make-arg k)
        (symbol-append 'i_ k))

      (define (make-loop-name k)
        (symbol-append 'loop- k))

      (define (make-loop index depth k)
        `(let ,(make-loop-name index) ((,(make-arg index) ,(make-lower index)))
              (if (= ,(make-arg index) ,(make-upper index))
                  ,(if (= index 0)
                       `identity
                       `(,(make-loop-name (- index 1)) (+ ,(make-arg (- index 1)) 1)))
                  ,(if (= depth 0)
                       `(let* ((item (f ,@(map (lambda (i) (make-arg i)) (iota k))))
                               (result (,(make-loop-name index) (+ ,(make-arg index) 1))))
                          (operator item result))
                       (make-loop (+ index 1) (- depth 1) k)))))

      (define (do-one-case k)
        (let ((result
               `((,k)
                 (let (,@(map (lambda (j)
                                `(,(make-lower j) (%%interval-lower-bound interval ,j)))
                              (iota k))
                       ,@(map (lambda (j)
                                `(,(make-upper j) (%%interval-upper-bound interval ,j)))
                              (iota k))
                       (i 0))
                   ,(make-loop 0 (- k 1) k)))))
          result))

      (let ((result
             `(case (%%interval-dimension interval)
                ((0) (operator (f) identity))
                ,@(map do-one-case (iota 8 1))
                (else
                 (let ()

                   (define (get-next-args reversed-args
                                          reversed-lowers
                                          reversed-uppers)
                     (let ((next-index (+ (car reversed-args) 1)))
                       (if (< next-index (car reversed-uppers))
                           (cons next-index (cdr reversed-args))
                           (and (not (null? (cdr reversed-args)))
                                (let ((tail-result (get-next-args (cdr reversed-args)
                                                                  (cdr reversed-lowers)
                                                                  (cdr reversed-uppers))))
                                  (and tail-result
                                       (cons (car reversed-lowers) tail-result)))))))

                     (let ((reversed-lowers (reverse (%%interval-lower-bounds->list interval)))
                           (reversed-uppers (reverse (%%interval-upper-bounds->list interval))))
                       (let loop ((reversed-args reversed-lowers))
                         (if reversed-args
                             (let* ((item (apply f (reverse reversed-args)))
                                    (result (loop (get-next-args reversed-args
                                                               reversed-lowers
                                                               reversed-uppers))))
                               (operator item result))
                             identity))))))))
        result))

  (if (%%interval-empty? interval)
      identity
      (generate-code)))


(define (%%interval->basic-indexer interval)
  (case (%%interval-dimension interval)
    ((0) (%%indexer-0 0))
    ((1) (let ((low-0 (%%interval-lower-bound interval 0))
               (increment-0 1))
           (%%indexer-1 0 low-0 increment-0)))
    ((2) (let* ((low-0 (%%interval-lower-bound interval 0))
                (low-1 (%%interval-lower-bound interval 1))
                (increment-1 1)
                (increment-0 (* increment-1 (%%interval-width interval 1))))
           (%%indexer-2 0
                        low-0 low-1
                        increment-0 increment-1)))
    ((3) (let* ((low-0 (%%interval-lower-bound interval 0))
                (low-1 (%%interval-lower-bound interval 1))
                (low-2 (%%interval-lower-bound interval 2))
                (increment-2 1)
                (increment-1 (* increment-2 (%%interval-width interval 2)))
                (increment-0 (* increment-1 (%%interval-width interval 1))))
           (%%indexer-3 0
                        low-0 low-1 low-2
                        increment-0 increment-1 increment-2)))
    ((4) (let* ((low-0 (%%interval-lower-bound interval 0))
                (low-1 (%%interval-lower-bound interval 1))
                (low-2 (%%interval-lower-bound interval 2))
                (low-3 (%%interval-lower-bound interval 3))
                (increment-3 1)
                (increment-2 (* increment-3 (%%interval-width interval 3)))
                (increment-1 (* increment-2 (%%interval-width interval 2)))
                (increment-0 (* increment-1 (%%interval-width interval 1))))
           (%%indexer-4 0
                        low-0 low-1 low-2 low-3
                        increment-0 increment-1 increment-2 increment-3)))
    (else
     (do ((widths
           (reverse (vector->list (%%interval-widths interval)))
           (cdr widths))
          (increments
           (list 1)
           (cons (* (car increments) (car widths))
                 increments)))
         ((null? (cdr widths))
          (%%indexer-generic 0 (%%interval-lower-bounds->list interval) increments))))))


;; unfortunately, the next three functions were written by hand, so beware of bugs.

(define (%%indexer-0 base)
  (if (eqv? base 0)
      (lambda () 0)       ;; Don't generate closure for common case.
      (lambda () base)))

(define (%%indexer-1 base
                     low-0
                     increment-0)
  (if (eqv? base 0)
      (if (eqv? 0 low-0)
          (cond ((eqv? 1 increment-0)    (lambda (i) i))
                ;;((eqv? -1 increment-0)   (lambda (i) (- i)))               ;; an impossible case
                (else                    (lambda (i) (* i increment-0))))
          (cond ((eqv? 1 increment-0)    (lambda (i) (- i low-0)))
                ;;((eqv? -1 increment-0)   (lambda (i) (- low-0 i)))         ;; an impossible case
                (else                    (lambda (i) (* increment-0 (- i low-0))))))
      (if (eqv? 0 low-0)
          (cond ((eqv? 1 increment-0)    (lambda (i) (+ base i)))
                ((eqv? -1 increment-0)   (lambda (i) (- base i)))
                (else                    (lambda (i) (+ base (* increment-0 i)))))
          (cond ((eqv? 1 increment-0)    (lambda (i) (+ base (- i low-0))))
                ((eqv? -1 increment-0)   (lambda (i) (+ base (- low-0 i))))
                (else                    (lambda (i) (+ base (* increment-0 (- i low-0)))))))))

(define (%%indexer-2 base
                     low-0       low-1
                     increment-0 increment-1)
  (if (eqv? 0 base)
      (if (eqv? 0 low-0)
          (cond ((eqv? 1 increment-0)
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ i j)))
                           ((eqv? -1 increment-1) (lambda (i j) (+ i (- j))))
                           (else                  (lambda (i j) (+ i (* increment-1 j)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ i (- j low-1))))
                           ((eqv? -1 increment-1) (lambda (i j) (+ i (- low-1 j))))
                           (else                  (lambda (i j) (+ i (* increment-1 (- j low-1))))))))
               #; ((eqv? -1 increment-0)         ;; an impossible case
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (- j                           i)))
                           ((eqv? -1 increment-1) (lambda (i j) (- (- j)                       i)))
                           (else                  (lambda (i j) (- (* increment-1 j)           i))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (- (- j low-1)                 i)))
                           ((eqv? -1 increment-1) (lambda (i j) (- (- low-1 j)                 i)))
                           (else                  (lambda (i j) (- (* increment-1 (- j low-1)) i))))))
                (else
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ (* increment-0 i) j)))
                           ((eqv? -1 increment-1) (lambda (i j) (+ (* increment-0 i) (- j))))
                           (else                  (lambda (i j) (+ (* increment-0 i) (* increment-1 j)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ (* increment-0 i) (- j low-1))))
                           ((eqv? -1 increment-1) (lambda (i j) (+ (* increment-0 i) (- low-1 j))))
                           (else                  (lambda (i j) (+ (* increment-0 i) (* increment-1 (- j low-1)))))))))
          (cond ((eqv? 1 increment-0)
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ (- i low-0) j)))
                           ((eqv? -1 increment-1) (lambda (i j) (+ (- i low-0) (- j))))
                           (else                  (lambda (i j) (+ (- i low-0) (* increment-1 j)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ (- i low-0) (- j low-1))))
                           ((eqv? -1 increment-1) (lambda (i j) (+ (- i low-0) (- low-1 j))))
                           (else                  (lambda (i j) (+ (- i low-0) (* increment-1 (- j low-1))))))))
                #;((eqv? -1 increment-0)         ;; an impossible case
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (- j                           (- i low-0))))
                           ((eqv? -1 increment-1) (lambda (i j) (- (- j)                       (- i low-0))))
                           (else                  (lambda (i j) (- (* increment-1 j)           (- i low-0)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (- (- j low-1)                 (- i low-0))))
                           ((eqv? -1 increment-1) (lambda (i j) (- (- low-1 j)                 (- i low-0))))
                           (else                  (lambda (i j) (- (* increment-1 (- j low-1)) (- i low-0)))))))
                (else
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ (* increment-0 (- i low-0)) j)))
                           ((eqv? -1 increment-1) (lambda (i j) (+ (* increment-0 (- i low-0)) (- j))))
                           (else                  (lambda (i j) (+ (* increment-0 (- i low-0)) (* increment-1 j)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ (* increment-0 (- i low-0)) (- j low-1))))
                           ((eqv? -1 increment-1) (lambda (i j) (+ (* increment-0 (- i low-0)) (- low-1 j))))
                           (else                  (lambda (i j) (+ (* increment-0 (- i low-0)) (* increment-1 (- j low-1))))))))))
      (if (eqv? 0 low-0)
          (cond ((eqv? 1 increment-0)
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ base i j)))
                           ((eqv? -1 increment-1) (lambda (i j) (+ base i (- j))))
                           (else                  (lambda (i j) (+ base i (* increment-1 j)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ base i (- j low-1))))
                           ((eqv? -1 increment-1) (lambda (i j) (+ base i (- low-1 j))))
                           (else                  (lambda (i j) (+ base i (* increment-1 (- j low-1))))))))
                ((eqv? -1 increment-0)
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (- (+ base j)                           i)))
                           ((eqv? -1 increment-1) (lambda (i j) (- (- base j)                           i)))
                           (else                  (lambda (i j) (- (+ base (* increment-1 j))           i))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (- (+ base (- j low-1))                 i)))
                           ((eqv? -1 increment-1) (lambda (i j) (- (+ base (- low-1 j))                 i)))
                           (else                  (lambda (i j) (- (+ base (* increment-1 (- j low-1))) i))))))
                (else
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ base (* increment-0 i) j)))
                           ((eqv? -1 increment-1) (lambda (i j) (+ base (* increment-0 i) (- j))))
                           (else                  (lambda (i j) (+ base (* increment-0 i) (* increment-1 j)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ base (* increment-0 i) (- j low-1))))
                           ((eqv? -1 increment-1) (lambda (i j) (+ base (* increment-0 i) (- low-1 j))))
                           (else                  (lambda (i j) (+ base (* increment-0 i) (* increment-1 (- j low-1)))))))))
          (cond ((eqv? 1 increment-0)
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ base (- i low-0) j)))
                           ((eqv? -1 increment-1) (lambda (i j) (+ base (- i low-0) (- j))))
                           (else                  (lambda (i j) (+ base (- i low-0) (* increment-1 j)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ base (- i low-0) (- j low-1))))
                           ((eqv? -1 increment-1) (lambda (i j) (+ base (- i low-0) (- low-1 j))))
                           (else                  (lambda (i j) (+ base (- i low-0) (* increment-1 (- j low-1))))))))
                ((eqv? -1 increment-0)
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (- (+ base j)                           (- i low-0))))
                           ((eqv? -1 increment-1) (lambda (i j) (- (- base j)                           (- i low-0))))
                           (else                  (lambda (i j) (- (+ base (* increment-1 j))           (- i low-0)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (- (+ base (- j low-1))                 (- i low-0))))
                           ((eqv? -1 increment-1) (lambda (i j) (- (+ base (- low-1 j))                 (- i low-0))))
                           (else                  (lambda (i j) (- (+ base (* increment-1 (- j low-1))) (- i low-0)))))))
                (else
                 (if (eqv? 0 low-1)
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ base (* increment-0 (- i low-0)) j)))
                           ((eqv? -1 increment-1) (lambda (i j) (+ base (* increment-0 (- i low-0)) (- j))))
                           (else                  (lambda (i j) (+ base (* increment-0 (- i low-0)) (* increment-1 j)))))
                     (cond ((eqv? 1 increment-1)  (lambda (i j) (+ base (* increment-0 (- i low-0)) (- j low-1))))
                           ((eqv? -1 increment-1) (lambda (i j) (+ base (* increment-0 (- i low-0)) (- low-1 j))))
                           (else                  (lambda (i j) (+ base (* increment-0 (- i low-0)) (* increment-1 (- j low-1))))))))))))

;;; after this we basically punt

(define (%%indexer-3 base
                     low-0       low-1       low-2
                     increment-0 increment-1 increment-2)
  (if (and (eqv? 0 low-0)
           (eqv? 0 low-1)
           (eqv? 0 low-2))
      (if (eqv? base 0)
          (if (eqv? increment-2 1)
              (lambda (i j k)
                (+ (* increment-0 i)
                   (* increment-1 j)
                   k))
              (lambda (i j k)
                (+ (* increment-0 i)
                   (* increment-1 j)
                   (* increment-2 k))))
          (if (eqv? increment-2 1)
              (lambda (i j k)
                (+ base
                   (* increment-0 i)
                   (* increment-1 j)
                   k))
              (lambda (i j k)
                (+ base
                   (* increment-0 i)
                   (* increment-1 j)
                   (* increment-2 k)))))
      (if (eqv? base 0)
          (if (eqv? increment-2 1)
              (lambda (i j k)
                (+ (* increment-0 (- i low-0))
                   (* increment-1 (- j low-1))
                   (- k low-2)))
              (lambda (i j k)
                (+ (* increment-0 (- i low-0))
                   (* increment-1 (- j low-1))
                   (* increment-2 (- k low-2)))))
          (if (eqv? increment-2 1)
              (lambda (i j k)
                (+ base
                   (* increment-0 (- i low-0))
                   (* increment-1 (- j low-1))
                   (- k low-2)))
              (lambda (i j k)
                (+ base
                   (* increment-0 (- i low-0))
                   (* increment-1 (- j low-1))
                   (* increment-2 (- k low-2))))))))

(define (%%indexer-4 base
                     low-0       low-1       low-2       low-3
                     increment-0 increment-1 increment-2 increment-3)
  (if (and (eqv? 0 low-0)
           (eqv? 0 low-1)
           (eqv? 0 low-2)
           (eqv? 0 low-3))
      (if (eqv? base 0)
          (if (eqv? increment-3 1)
              (lambda (i j k l)
                (+ (* increment-0 i)
                   (* increment-1 j)
                   (* increment-2 k)
                   l))
              (lambda (i j k l)
                (+ (* increment-0 i)
                   (* increment-1 j)
                   (* increment-2 k)
                   (* increment-3 l))))
          (if (eqv? increment-3 1)
              (lambda (i j k l)
                (+ base
                   (* increment-0 i)
                   (* increment-1 j)
                   (* increment-2 k)
                   l))
              (lambda (i j k l)
                (+ base
                   (* increment-0 i)
                   (* increment-1 j)
                   (* increment-2 k)
                   (* increment-3 l)))))
      (if (eqv? base 0)
          (if (eqv? increment-3 1)
              (lambda (i j k l)
                (+ (* increment-0 (- i low-0))
                   (* increment-1 (- j low-1))
                   (* increment-2 (- k low-2))
                   (- l low-3)))
              (lambda (i j k l)
                (+ (* increment-0 (- i low-0))
                   (* increment-1 (- j low-1))
                   (* increment-2 (- k low-2))
                   (* increment-3 (- l low-3)))))
          (if (eqv? increment-3 1)
              (lambda (i j k l)
                (+ base
                   (* increment-0 (- i low-0))
                   (* increment-1 (- j low-1))
                   (* increment-2 (- k low-2))
                   (- l low-3)))
              (lambda (i j k l)
                (+ base
                   (* increment-0 (- i low-0))
                   (* increment-1 (- j low-1))
                   (* increment-2 (- k low-2))
                   (* increment-3 (- l low-3))))))))

(define (%%indexer-generic base lower-bounds increments)
  (let ((result
         (lambda multi-index
           (do ((multi-index  multi-index  (cdr multi-index))
                (lower-bounds lower-bounds (cdr lower-bounds))
                (increments   increments   (cdr increments))
                (result       base         (+ result (* (car increments)
                                                        (- (car multi-index)
                                                           (car lower-bounds))))))
               ((null? multi-index) result)))))
    result))


