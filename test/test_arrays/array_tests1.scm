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
(module array-test1
   (include "test_macros.sch")
   (library srfi231 srfi39)
   (import bigloo_compat
           test_infra
           test-random-infrastructure
           number_utils)
   (export (array-error-tests)
           (array-result-tests)
           (array-domain-and-array-getter-error-tests)
           (array?-array-domain-and-array-getter-result-tests)
           (mutable-array?-and-array-setter-result-tests)
           (array-freeze!-tests)
           (new-indexer-result-tests)
           (array-body-indexer-storage-class-safe?-error-tests)
           (make-specialized-array-error-tests)
           (mutable-array-result-tests)
           (array-setter-error-tests)
           (make-specialized-array-from-data-error-tests)
           (list*->array-and-vector*->array-tests)
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
           (interval-and-array-permutation-tests)))


(define (array-error-tests)
   (pp "array error tests")

   (test (make-array 1 values)
      "make-array: The first argument is not an interval: ")

   (test (make-array (make-interval '#(3) '#(4)) 1)
      "make-array: The second argument is not a procedure: ")

   (test (make-array 1 values values)
      "make-array: The first argument is not an interval: ")

   (test (make-array (make-interval '#(3) '#(4)) 1 values)
      "make-array: The second argument is not a procedure: ")

   (test (make-array (make-interval '#(3) '#(4)) list 1)
      "make-array: The third argument is not a procedure: "))


(define (array-result-tests)
   (pp "array result tests")
 
   (let ((getter (lambda args 1.)))
      (test (myarray= (make-array (make-interval '#(3) '#(4)) getter)
               (make-%%array (make-interval '#(3) '#(4))
                  getter
                  #f
                  #f
                  #f
                  #f
                  #f
                  %%order-unknown))
         #t)))

(define (array-domain-and-array-getter-error-tests)
   (pp "array-domain and array-getter error tests")
   
   (test (array-domain #f)
      "array-domain: The argument is not an array: ")
   
   (test (array-getter #f)
      "array-getter: The argument is not an array: "))

(define (array?-array-domain-and-array-getter-result-tests)
   (pp "array?, array-domain, and array-getter result tests")
   
   (let* ((getter (lambda args 1.))
          (domain (make-interval '#(3) '#(4)))
          (array  (make-array domain getter)))
      (test (array? #f)
         #f)
      (test (array? array)
         #t)
      (test (array-domain array)
         domain)
      (test (array-getter array)
         getter)))

(define (mutable-array-result-tests)
   (pp "mutable-array result tests")
   
   (let ((result #f))
      (let ((getter (lambda (i) result))
            (setter   (lambda (v i) (set! result v)))
            (domain   (make-interval '#(3) '#(4))))
         (test (make-array domain
                  getter
                  setter)
            (make-%%array domain
               getter
               setter
               #f
               #f
               #f
               #f
               %%order-unknown)))))

(define (array-setter-error-tests)
   (pp "array-setter error tests")

   (test (array-setter #f)
      "array-setter: The argument is not an mutable array: "))

(define (mutable-array?-and-array-setter-result-tests)
   (pp "mutable-array? and array-setter result tests")

   (let ((result (cons #f #f)))
      (let ((getter (lambda (i) (car result)))
            (setter   (lambda (v i) (set-car! result v)))
            (domain   (make-interval '#(3) '#(4))))
         (let ((array (make-array domain
                         getter
                         setter)))
            (test (array? array)
               #t)
            (test (mutable-array? array)
               #t)
            (test (mutable-array? 1)
               #f)
            (test (array-setter array)
               setter)
            (test (array-getter array)
               getter)
            (test (array-domain array)
               domain)))))

(define (array-freeze!-tests)
   (pp "array-freeze! tests")

   (test (array-freeze! 'a)
      "array-freeze!: The argument is not an array: ")
   
 (let ((A (make-specialized-array (make-interval '#()))))
    (test (mutable-array? A)
       #t)
    (let ((B (array-freeze! A)))
       (test (mutable-array? B)
          #f)
       (test (eq? A B)
          #t))
    (test (mutable-array? A)
       #f)))

(define (myindexer= indexer1 indexer2 interval)
  (array-fold-left (lambda (x y) (and x y))
                   #t
                   (make-array interval
                               (lambda args
                                 (= (apply indexer1 args)
                                    (apply indexer2 args))))))


(define (my-indexer base lower-bounds increments)
  (lambda indices
    (apply + base (map * increments (map - indices lower-bounds)))))



(define (random-sign)
  (- 1 (* 2 (random 2))))

(define (new-indexer-result-tests)
   (pp "new-indexer result tests")

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((lower-bounds
                 (map (lambda (x) (random 2))
                    (vector->list (make-vector (random 1 7)))))
              (upper-bounds
                 (map (lambda (x) (+ x (random 1 3)))
                    lower-bounds))
              (new-domain
                 (make-interval (list->vector lower-bounds)
                    (list->vector upper-bounds)))
              (new-domain-dimension
                 (interval-dimension new-domain))
              (old-domain-dimension
                 (random 1 7))
              (base
                 (random 100))
              (coefficients
                 (map (lambda (x) (* (random-sign)
                                     (random 20)))
                    (local-iota 0 old-domain-dimension)))
              (old-indexer
                 (lambda args
                    (apply + base (map * args coefficients))))
              (new-domain->old-domain-coefficients
                 (map (lambda (x)
                         (map (lambda (x) (* (random-sign) (random 10)))
                            (local-iota 0 new-domain-dimension)))
                    (local-iota 0 old-domain-dimension)))
              (new-domain->old-domain
                 (lambda args
                    (apply values (map (lambda (row)
                                          (apply + (map * row args)))
                                     new-domain->old-domain-coefficients)))))
          (if (not (and (myindexer= (lambda args
                                       (call-with-values
                                          (lambda () (apply new-domain->old-domain args))
                                          old-indexer))
                           (%%compose-indexers old-indexer new-domain  new-domain->old-domain)
                           new-domain)))
              (pp (list new-domain
                     old-domain-dimension
                     base
                     coefficients
                     new-domain->old-domain-coefficients)))))
   
   (next-test-random-source-state!))

(define (array-body-indexer-storage-class-safe?-error-tests)
   (pp "array body, indexer, storage-class, and safe? error tests")
   
   (let ((a (make-array (make-interval '#(0 0) '#(1 1)) ;; not valid
               values
               values)))
      (test (array-body a)
         "array-body: The argument is not a specialized array: ")
      (test (array-indexer a)
         "array-indexer: The argument is not a specialized array: ")
      (test (array-storage-class a)
         "array-storage-class: The argument is not a specialized array: ")
      (test (array-safe? a)
         "array-safe?: The argument is not a specialized array: "))
   )

(define (make-specialized-array-error-tests)
   (pp "make-specialized-array error tests")
   
   (test (make-specialized-array  'a)
      "make-specialized-array: The first argument is not an interval: ")
   
   (test (make-specialized-array (make-interval '#(0) '#(10)) 'a 1)
      "make-specialized-array: The second argument is not a storage-class: ")
   
   (test (make-specialized-array (make-interval '#(0) '#(10)) u16-storage-class 'a)
      "make-specialized-array: The third argument cannot be manipulated by the second (a storage class): ")
   
   
   (test (make-specialized-array (make-interval '#(0) '#(10)) generic-storage-class 'a 'a)
      "make-specialized-array: The fourth argument is not a boolean: ")
   
;;; let's test a few more

   (test (array-every (lambda (x) (eqv? x #u8:42)) (make-specialized-array (make-interval '#(10)) u8-storage-class #u8:42))
      #t)
   
   (test (array-safe? (make-specialized-array (make-interval '#(10)) u8-storage-class #u8:42))
      (specialized-array-default-safe?))
   
   (test (parameterize ((specialized-array-default-safe? #t)) (array-safe? (make-specialized-array (make-interval '#(10)) u8-storage-class #u8:42)))
      #t)
   
   (test (parameterize ((specialized-array-default-safe? #f)) (array-safe? (make-specialized-array (make-interval '#(10)) u8-storage-class #u8:4242)))
      #f)
   
   (test (array-safe? (make-specialized-array (make-interval '#(10)) u8-storage-class #u8:42  #t))
      #t)
   
   (test (array-safe? (make-specialized-array (make-interval '#(10)) u8-storage-class #u8:42 #f))
      #f))

(define (make-specialized-array-from-data-error-tests)
   (pp "make-specialized-array-from-data error tests")

   (test (make-specialized-array-from-data 'a 'a 'a 'a)
      "make-specialized-array-from-data: The fourth argument is not a boolean: ")
   
   (test (make-specialized-array-from-data 'a 'a 'a #t)
      "make-specialized-array-from-data: The third argument is not a boolean: ")
   
   (test (make-specialized-array-from-data 'a 'a 'a)
      "make-specialized-array-from-data: The third argument is not a boolean: ")
   
   (test (make-specialized-array-from-data 'a 'a #f #t)
      "make-specialized-array-from-data: The second argument is not a storage class: ")
   
   (test (make-specialized-array-from-data 'a 'a #f)
      "make-specialized-array-from-data: The second argument is not a storage class: ")
   
   (test (make-specialized-array-from-data 'a 'a)
      "make-specialized-array-from-data: The second argument is not a storage class: ")
   
   (test (make-specialized-array-from-data 'a generic-storage-class #f #t)
      "make-specialized-array-from-data: The first argument is not compatible with the storage class: ")

   (test (make-specialized-array-from-data 'a generic-storage-class #f)
      "make-specialized-array-from-data: The first argument is not compatible with the storage class: ")
   
   (test (make-specialized-array-from-data 'a generic-storage-class)
      "make-specialized-array-from-data: The first argument is not compatible with the storage class: ")
   
   (test (make-specialized-array-from-data 'a)
      "make-specialized-array-from-data: The first argument is not compatible with the storage class: ")
   
   (let ((test-values
            (list ;;       storae-class   default other data
               (list generic-storage-class  #f 'a 1 #\c)
               (list    char-storage-class  '#\0 '#\a '#\b)
               (list      u1-storage-class  0 1)
               (list      u8-storage-class  #u8:0 #u8:23)
               (list     u16-storage-class  #u16:00 #u16:500)
               (list     u32-storage-class  #u32:0 #u32:70000)
               (list     u64-storage-class  #u64:0 #u64:100000000000)
               (list      s8-storage-class  #s8:0 #s8:-1 #s8:1)
               (list     s16-storage-class  #s16:00 #s16:-300 #s16:300)
               (list     s32-storage-class  #s32:0 #s32:-70000 #s32:70000)
               (list     s64-storage-class  #s64:0 #s64:-1000000000 #s64:1000000000)
               (list     f32-storage-class  0. 1.)
               (list     f64-storage-class  0. 1.)
               ;(list     c64-storage-class  0.+0.i 1.+1.i)
               ;(list    c128-storage-class  0.+0.i 1.+1.i)
               )))
      (for-each (lambda (data)
                   (let ((storage-class (car data))
                         (default       (cadr data))
                         (other-values  (cddr data)))
                      (test (array-every (lambda (v)
                                            (equal? v default))
                               (make-specialized-array (make-interval '#(4 4)) storage-class))
                         #t)
                      (for-each (lambda (val)
                                   (test (array-every (lambda (v)
                                                         (equal? v val))
                                            (make-specialized-array (make-interval '#(4 4))
                                               storage-class
                                               val))
                                      #t))
                         other-values)))
         test-values))
   
   (let ((test-values
            (list ;;       storae-class  good data           bad data
               (list generic-storage-class (make-vector 10)    (make-u16vector 10))
               (list    char-storage-class (make-string 10)    (make-u16vector 10))
               (list      u1-storage-class (make-u16vector 10) (make-u32vector 10))
               (list      u8-storage-class (make-u8vector 10)  (make-u16vector 10))
               (list     u16-storage-class (make-u16vector 10) (make-u32vector 10))
               (list     u32-storage-class (make-u32vector 10) (make-u16vector 10))
               (list     u64-storage-class (make-u64vector 10) (make-s16vector 10))
               (list      s8-storage-class (make-s8vector 10)  (make-u16vector 10))
               (list     s16-storage-class (make-s16vector 10) (make-u16vector 10))
               (list     s32-storage-class (make-s32vector 10) (make-u16vector 10))
               (list     s64-storage-class (make-s64vector 10) (make-u16vector 10))
               (list     f32-storage-class (make-f32vector 10) (make-u16vector 10))
               (list     f64-storage-class (make-f64vector 10) (make-u16vector 10))
               ;(list     c64-storage-class (make-f32vector 10) (make-u16vector 10) (make-f32vector 5))
               ;(list    c128-storage-class (make-f64vector 10) (make-u16vector 10) (make-f64vector 5))
               )))
      (for-each (lambda (data)
                   (let ((storage-class (car data))
                         (good-data     (cadr data))
                         (rest          (cddr data)))
                      (test (and (array? (make-specialized-array-from-data good-data storage-class))
                                 (eq? ((storage-class-data? storage-class) good-data)
                                    #t))
                         #t)
                      (for-each (lambda (bad)
                                   (test (make-specialized-array-from-data bad storage-class)
                                      "make-specialized-array-from-data: The first argument is not compatible with the storage class: "))
                         rest)))
         test-values))
   
   (pretty-print
      (array->list*
         (specialized-array-reshape           ;; Reshape to a zero-dimensional array
            (array-extract                      ;; Restrict to the first element
               (make-specialized-array-from-data  ;; The basic one-dimensional array
                  (vector 'foo 'bar 'baz))
               (make-interval '#(1)))
            (make-interval '#()))))
   
   (let* ((board (u16vector #b111100110111))
          (A (specialized-array-reshape
                (array-extract
                   (make-specialized-array-from-data board u1-storage-class)
                   (make-interval '#(9)))
                (make-interval '#(3 3))))
          (B (list->array (make-interval '#(3 3))
                '(1 1 1
                  0 1 1
                  0 0 1)
                u1-storage-class)))
      (define (pad n s)
         (string-append (make-string (- n (string-length s)) #\0) s))
      
      (test (array-every = A B)
         #t)
      (for-each display (list "(array-every = A B) => " (array-every = A B) #\newline))
      (for-each display (list "(array-body A) => " (array-body A) #\newline))
      (for-each display (list "(array-body B) => " (array-body B) #\newline))
      (for-each display (list "(pad 16 (number->string (u16vector-ref (vector-ref (array-body A) 1) 0) 2)) => " #\newline
                           (pad 16 (number->string (u16vector-ref (vector-ref (array-body A) 1) 0) 2)) #\newline))
      (for-each display (list "(pad 16 (number->string (u16vector-ref (vector-ref (array-body B) 1) 0) 2)) => " #\newline
                           (pad 16 (number->string (u16vector-ref (vector-ref (array-body B) 1) 0) 2)) #\newline)))
   )

(define (list*->array-and-vector*->array-tests)
   (pp "list*->array and vector*->array tests")

;;; Error tests

   (for-each
      (lambda (operation message)

         (test (operation 1 2 3 4 5)
            (string-append message "The fifth argument is not a boolean: "))

         (test (operation 1 2 3 4 #t)
            (string-append message "The fourth argument is not a boolean: "))

         (test (operation 1 2 3 4)
            (string-append message "The fourth argument is not a boolean: "))

         (test (operation 1 2 3 #t #t)
            (string-append message "The third argument is not a storage class: "))

         (test (operation 1 2 3 #t)
            (string-append message "The third argument is not a storage class: "))

         (test (operation 1 2 3)
            (string-append message "The third argument is not a storage class: "))

         (test (operation 'a 1 generic-storage-class #t #f)
            (string-append message "The first argument is not a nonnegative fixnum: "))

         (test (operation -1 1 generic-storage-class #t #f)
            (string-append message "The first argument is not a nonnegative fixnum: "))

         (test (operation 'a 1 generic-storage-class #t)
            (string-append message "The first argument is not a nonnegative fixnum: "))

         (test (operation -1 1 generic-storage-class #t)
            (string-append message "The first argument is not a nonnegative fixnum: "))

         (test (operation 'a 1 generic-storage-class)
            (string-append message "The first argument is not a nonnegative fixnum: "))

         (test (operation -1 1 generic-storage-class)
            (string-append message "The first argument is not a nonnegative fixnum: "))

         (test (operation 'a 1)
            (string-append message "The first argument is not a nonnegative fixnum: "))

         (test (operation -1 1)
            (string-append message "The first argument is not a nonnegative fixnum: ")))
      (list list*->array
         vector*->array)
      (list "list*->array: "
         "vector*->array: "))

;;; Output tests

   (test (array-every equal?
            (list*->array 1 '((a b c) (1 2 3)))
            (list->array (make-interval '#(2))
               '((a b c) (1 2 3))))
      #t)

   (test (array-every equal?
            (list*->array 2 '((a b c) (1 2 3)))
            (list->array (make-interval '#(2 3))
               '(a b c 1 2 3)))
      #t)

   (test (array-every equal?
            (list*->array 3 '(((a b c) (1 2 3))))
            (list->array (make-interval '#(1 2 3))
               '(a b c 1 2 3)))
      #t)

   (test (array-every equal?
            (list*->array 2 '(((a b c) (1 2 3))))
            (list->array (make-interval '#(1 2))
               '((a b c) (1 2 3))))
      #t)

   (test (list*->array 3 '(((a b c) (1 2))))
      (string-append "list*->array: " "The second argument is not the right shape to be converted to an array of the given dimension: "))

   (test (array-every equal?
            (list*->array 2 '(((a b c) (1 2))))
            (list->array (make-interval '#(1 2))
               '((a b c) (1 2))))
      #t)

   (test (array-every equal?
            (list*->array 0 '())
            (make-array (make-interval '#()) (lambda () '())))
      #t)

   (test (array-every equal?
            (list*->array 1 '())
            (make-array (make-interval '#(0)) (lambda () (test-error))))
      #t)

   (test (array-every equal?
            (list*->array 2 '())
            (make-array (make-interval '#(0 0)) (lambda () (test-error))))
      #t)

   (test (array-every equal?
            (list*->array 2 '(()()))
            (make-array (make-interval '#(2 0)) (lambda () (test-error))))
      #t)

   (test (array-every equal?
            (vector*->array 2 '#(#(a b c) #(1 2 3)))
            (list->array (make-interval '#(2 3))
               '(a b c 1 2 3)))
      #t)

   (test (array-every equal?
            (vector*->array 3 '#(#(#(a b c) #(1 2 3))))
            (list->array (make-interval '#(1 2 3))
               '(a b c 1 2 3)))
      #t)

   (test (array-every equal?
            (vector*->array 2 '#(#((a b c) (1 2 3))))
            (list->array (make-interval '#(1 2))
               '((a b c) (1 2 3))))
      #t)

   (test (vector*->array 3 '#(#(#(a b c) #(1 2))))
      (string-append "vector*->array: " "The second argument is not the right shape to be converted to an array of the given dimension: "))

   (test (array-every equal?
            (vector*->array 2 '#(#((a b c) (1 2))))
            (list->array(make-interval '#(1 2))
               '((a b c) (1 2))))
      #t)

   (test (array-every equal?
            (vector*->array 0 '#())
            (make-array (make-interval '#()) (lambda () '#())))
      #t)

   (test (array-every equal?
            (vector*->array 1 '#())
            (make-array (make-interval '#(0)) (lambda () (test-error))))
      #t)

   (test (array-every equal?
            (vector*->array 2 '#())
            (make-array (make-interval '#(0 0)) (lambda () (test-error))))
      #t)

   (test (array-every equal?
            (vector*->array 2 '#(#()#()))
            (make-array (make-interval '#(2 0)) (lambda () (test-error))))
      #t)

   (test (vector*->array 2 '#(#((a b c) (1 2))) u8-storage-class)
      "vector*->array: Not all elements of the source can be stored in destination: ")

   (test (list*->array 2 '(((a b c) (1 2))) u8-storage-class)
      "list*->array: Not all elements of the source can be stored in destination: ")

   (test (list*->array 0 'a u8-storage-class)
      "list*->array: Not all elements of the source can be stored in destination: ")

   (test (vector*->array 0 'a u8-storage-class)
      "vector*->array: Not all elements of the source can be stored in destination: ")

   (for-each (lambda (operation data)
                (for-each (lambda (mutable?)
                             (for-each (lambda (safe?)
                                          (parameterize
                                             ((specialized-array-default-mutable? mutable?)
                                              (specialized-array-default-safe? safe?))
                                             (let ((A (operation 2 data)))
                                                (test (mutable-array? A) mutable?)
                                                (test (array-safe? A) safe?))))
                                '(#t #f)))
                   '(#t #f)))
      (list list*->array
         vector*->array)
      (list '(((a b c) (1 2)))
         '#(#((a b c) (1 2)))))
   )


(define (array-packed?-tests)
   (pp "array-packed? tests")

   ;; We'll use specialized arrays with u1-storage-class---we never
   ;; use the array contents, just the indexers, and it saves storage.

   (test (array-packed? 1)
      "array-packed?: The argument is not a specialized array: ")

   (test (array-packed? (make-array (make-interval '#(1 2)) list))
      "array-packed?: The argument is not a specialized array: ")

   (test (array-packed? (make-array (make-interval '#(1 2)) list list)) ;; not valid setter
      "array-packed?: The argument is not a specialized array: ")

   ;; all these are true, we'll have to see how to screw it up later.

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let ((array
                (make-specialized-array (random-interval)
                   u1-storage-class)))
          (test (array-packed? array)
             #t)))

   (next-test-random-source-state!)

   ;; the elements of curried arrays are in order

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((base
                 (make-specialized-array (random-interval 2 5)
                    u1-storage-class))
              (curried
                 (array-curry base (random 1 (array-dimension base)))))
          (test (array-every array-packed? curried)
             #t)))

   (next-test-random-source-state!)

   ;; Extracted arrays are in order if they are empty or have
   ;; dimension 0.
   ;; Elements of extracted arrays of newly created specialized
   ;; arrays are not in order unless
   ;; (1) the differences in the upper and lower bounds of the
   ;;     first dimensions all equal 1 *and*
   ;; (2) the next dimension doesn't matter *and*
   ;; (3) the upper and lower bounds of the latter dimensions
   ;;     of the original and extracted arrays are the same
   ;; Whew!

   (define (extracted-array-packed? base extracted)
      (let ((base-domain (array-domain base))
            (extracted-domain (array-domain extracted))
            (dim (array-dimension base)))
         (or (interval-empty? extracted-domain)
             (eqv? dim 0)
             (let loop-1 ((i 0))
                (or (= i (- dim 1))
                    (or (and (= 1 (- (interval-upper-bound extracted-domain i)
                                     (interval-lower-bound extracted-domain i)))
                             (loop-1 (+ i 1)))
                        (let loop-2 ((i (+ i 1)))
                           (or (= i dim)
                               (and (= (interval-upper-bound extracted-domain i)
                                       (interval-upper-bound base-domain i))
                                    (= (interval-lower-bound extracted-domain i)
                                       (interval-lower-bound base-domain i))
                                    (loop-2 (+ i 1)))))))))))

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((base
                 (make-specialized-array (random-interval 0 6)
                    u1-storage-class))
              (extracted
                 (array-extract base (random-subinterval (array-domain base)))))
          (test (array-packed? extracted)
             (extracted-array-packed? base extracted))))

   (next-test-random-source-state!)

   ;; Should we do reversed now?
   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((base
                 (make-specialized-array (random-interval)
                    u1-storage-class))
              (domain
                 (array-domain base))
              (reversed-dimensions
                 (vector-map (lambda args (random-boolean))
                    (make-vector (array-dimension base))))
              (reversed
                 (array-reverse base reversed-dimensions)))
          (test (array-packed? reversed)
             (or (array-empty? reversed)
                 (%%vector-every
                    (lambda (lower upper reversed)
                       (or (= (+ 1 lower) upper)  ;; side-length 1
                           (not reversed)))       ;; dimension not reversed
                    (interval-lower-bounds->vector domain)
                    (interval-upper-bounds->vector domain)
                    reversed-dimensions)))))
   
   (next-test-random-source-state!)

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((base
                 (make-specialized-array (random-interval)
                    u1-storage-class))
              (domain
                 (array-domain base))
              (permutation
                 (random-permutation (array-dimension base)))
              (permuted
                 (array-permute base permutation)))
          (test (array-packed? permuted)
             (permuted-array-packed? base permutation))))
   
   (next-test-random-source-state!)

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((base
                 (make-specialized-array (random-nonnegative-interval 1 6)
                    u1-storage-class))
              (scales
                 (random-positive-vector (array-dimension base) 4))
              (sampled
                 (array-sample base scales)))
          (test (array-packed? sampled)
             (sampled-array-packed? base scales))))

   (next-test-random-source-state!)

;;; Now we need to test the precomputation and caching of array-packed?
;;; The only places we precompute are
;;; 1.  after creating a new specialized array
;;; 2.  in %%specialized-array-translate
;;; 3.  in %%specialized-array-curry
;;; 4.  reshaping a specialized array in place.
;;; So we need to check these situations.

   (let ((array (array-copy (make-array (make-interval '#(3 5)) list))))
      (test (and (%%array-packed? array)
                 (%%compute-array-packed? (%%array-domain array) (%%array-indexer array)))
         #t))

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((array
                 (make-specialized-array (random-nonnegative-interval) u8-storage-class))
              (ignore  ;; compute and cache the results
                 (array-packed? array))
              (sampled-array
                 (array-sample array (random-sample (array-dimension array))))
              (ignore  ;; compute and cache the results
                 ;; possibly not in order
                 (array-packed? sampled-array))
              (translated-array
                 (array-translate array (vector-map (lambda (x) (random 10)) (make-vector (array-dimension array)))))
              (translated-sampled-array
                 (array-translate sampled-array (vector-map (lambda (x) (random 10)) (make-vector (array-dimension array))))))
          (test (%%array-packed? translated-array)
             (%%compute-array-packed? (%%array-domain translated-array) (%%array-indexer translated-array)))
          (test (%%array-packed? translated-sampled-array)
             (%%compute-array-packed? (%%array-domain translated-sampled-array) (%%array-indexer translated-sampled-array)))))

   (next-test-random-source-state!)

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((array
                 (make-specialized-array (random-nonnegative-interval 2 4) u8-storage-class))
              (d-1
                 (- (array-dimension array) 1))
              (ignore
                 ;; compute and cache the result, in order
                 (array-packed? array))
              (rotated-array
                 (array-permute array (index-rotate (array-dimension array) 1)))
              (ignore  ;; compute and cache the results
                 ;; possibly not in order
                 (array-packed? rotated-array))
              (sampled-array
                 (array-sample array (list->vector (cons 2 (make-list d-1 1)))))
              (ignore
                 ;; almost definitely not in order,
                 ;; but if we curry it with dimension 1 the subarrays are in order.
                 (array-packed? sampled-array))
              (curried-array
                 (array-ref (array-curry array d-1) (interval-lower-bound (array-domain array) 0)))
              (curried-rotated-array
                 (array-ref (array-curry rotated-array d-1) (interval-lower-bound (array-domain rotated-array) 0)))
              (curried-sampled-array
                 (array-ref (array-curry sampled-array d-1) (interval-lower-bound (array-domain sampled-array) 0))))
          (test (array-packed? curried-array)
             (%%compute-array-packed? (%%array-domain curried-array) (%%array-indexer curried-array)))
          (test (array-packed? curried-rotated-array)
             (%%compute-array-packed? (%%array-domain curried-rotated-array) (%%array-indexer curried-rotated-array)))
          (test (array-packed? curried-sampled-array)
             (%%compute-array-packed? (%%array-domain curried-sampled-array) (%%array-indexer curried-sampled-array)))))

   (next-test-random-source-state!))




;;; FIXME: array-reshape tests.


(define (%%move-array-elements-tests)
   (pp "%%move-array-elements tests")
   
   ;; error tests
   
   (test (%%move-array-elements (array-reverse (make-specialized-array (make-interval '#(2 2))))
            (make-array (make-interval '#(1 4)) list)
            "")
      "Arrays must have the same domains: ")
   
   
   (test (%%move-array-elements (make-specialized-array (make-interval '#(2 2)))
            (make-array (make-interval '#(1 5)) list)
            "")
      "Arrays must have the same domains: ")
   
   
   (test (%%move-array-elements (make-array (make-interval '#(2 2)) list list) ;; not a valid setter
            (make-array (make-interval '#(1 4)) list)
            "")
      "Arrays must have the same domains: ")
   
   (do ((d 1 (+fx d 1)))
       ((= d 6))
       (let* ((uppers-list
                 (iota d 2))
              (domain
                 (make-interval (list->vector uppers-list))))
          (do ((i 0 (+fx i 1)))
              ;; distribute "tests" results over five dimensions
              ((= i (quotient random-tests 5)))
              (let* ((storage-class-and-initializer
                        (random-storage-class-and-initializer))
                     (storage-class
                        (car storage-class-and-initializer))
                     (initializer
                        (cadr storage-class-and-initializer))
                     (specialized-source
                        (array-copy
                           (make-array domain
                              (lambda args
                                 (initializer)))
                           storage-class))
                     (specialized-destination
                        (make-specialized-array domain
                           storage-class))
                     (destination
                        (make-array (array-domain specialized-destination)
                           (array-getter specialized-destination)
                           (array-setter specialized-destination))))
                 ;; specialized-to-specialized, use fast copy
                 (test (%%move-array-elements specialized-destination specialized-source "test: ")
                    (if (not (storage-class-copier storage-class))
                        "In order, no checks needed"
                        "Block copy"))
                 (test (myarray= specialized-source specialized-destination)
                    #t)
                 ;; copy to non-adjacent elements of destination, no checking needed
                 (test (%%move-array-elements (array-reverse specialized-destination) specialized-source "test: ")
                    (if (array-packed? (array-reverse specialized-destination))
                        "In order, no checks needed"
                        "Out of order, no checks needed"))
                 (test (myarray= specialized-source (array-reverse specialized-destination))
                    #t)
                 ))))
   (next-test-random-source-state!))



(define (extreme-values-test-start)
   (print "extreme values test start")
   (do ((d 1 (+fx d 1)))
       ((= d 6))
       (let* ((uppers-list
                 (iota d 2))
              (domain
                 (make-interval (list->vector uppers-list)))
              (reversed-domain
                 (make-interval (list->vector (reverse uppers-list)))))
          (do ((i 0 (+fx i 1)))
              ;; distribute "tests" results over five dimensions
              ((= i (quotient random-tests 5)))
              (let* ((destination-storage-class-and-initializer
                        (random-storage-class-and-initializer))
                     (destination-storage-class
                        (car destination-storage-class-and-initializer))
                     (destination-initializer
                        (cadr destination-storage-class-and-initializer))
                     (source-storage-class-and-initializer
                        (random-storage-class-and-initializer))
                     (source-storage-class
                        (car source-storage-class-and-initializer))
                     (source-initializer
                        (cadr source-storage-class-and-initializer))
                     (source
                        (array-copy (make-array domain
                                       source-initializer)
                           source-storage-class))
                     (generalized-source
                        (make-array (array-domain source)
                           (array-getter source)))
                     (destination
                        (make-specialized-array domain
                           destination-storage-class))
                     (generalized-destination
                        (make-array (array-domain destination)
                           (array-getter destination)
                           (array-setter destination)))
                     (destination
                        (if (zero? (random 2))
                            (array-reverse destination)
                            destination))
                     (destination-checker
                        (storage-class-checker destination-storage-class)))
                 (if (array-every destination-checker source)
                     (begin
                        (test (let ((%%move-result
                                       (%%move-array-elements destination source "test: ")))
                                 (and (equal? (if (array-packed? destination)
                                                  (let ((expected (cond ((and (eq? destination-storage-class source-storage-class)
                                                                              (storage-class-copier destination-storage-class))
                                                                         "Block copy")
                                                                        ((eq? destination-storage-class generic-storage-class)
                                                                         "In order, no checks needed, generic-storage-class")
                                                                        ;; for Bigloo all of the homogeneous vector types are (or in case of the jvm, should be) distinct so
                                                                        ;; this case never occurs
                                                                        ; ((%%every destination-checker (cdr (assq source-storage-class extreme-values-alist)))
                                                                        ;  "In order, no checks needed")
                                                                        (else
                                                                         "In order, checks needed"))))
                                                     expected)
                                                  (cond ((%%every destination-checker (cdr (assq source-storage-class extreme-values-alist)))
                                                         "Out of order, no checks needed")
                                                        (else
                                                         "Out of order, checks needed")))
                                         %%move-result)
                                      (myarray= destination
                                         source
                                         (if (or
                                              ; (and (eq? source-storage-class c128-storage-class)
                                              ;              (eq? destination-storage-class c64-storage-class))
                                              (and (eq? source-storage-class f64-storage-class)
                                                   (eq? destination-storage-class f32-storage-class)))
                                             (lambda (x y)
                                                (< (magnitude (- x y)) 1e-5))
                                             equal?))))
                           #t)
                        (test (let ((%%move-result
                                       (%%move-array-elements generalized-destination source "test: ")))
                                 (and (equal? "Destination not specialized array"
                                         %%move-result)
                                      (myarray= generalized-destination
                                         source
                                         (if (or
                                              ; (and (eq? source-storage-class c128-storage-class)
                                              ;              (eq? destination-storage-class c64-storage-class))
                                              (and (eq? source-storage-class f64-storage-class)
                                                   (eq? destination-storage-class f32-storage-class)))
                                             (lambda (x y)
                                                (< (magnitude (- x y)) 1e-5))
                                             equal?))))
                           #t)
                        (test (let ((ignore (array-assign! destination generalized-source)))
                                 (myarray= destination
                                    generalized-source
                                    (if (or
                                         ; (and (eq? source-storage-class c128-storage-class)
                                         ;              (eq? destination-storage-class c64-storage-class))
                                         (and (eq? source-storage-class f64-storage-class)
                                              (eq? destination-storage-class f32-storage-class)))
                                        (lambda (x y)
                                           (< (magnitude (- x y)) 1e-5))
                                        equal?)))
                           #t)
                        (test (let ((ignore (array-assign! generalized-destination generalized-source)))
                                 (myarray= generalized-destination
                                    generalized-source
                                    (if (or
                                         ; (and (eq? source-storage-class c128-storage-class)
                                         ;              (eq? destination-storage-class c64-storage-class))
                                         (and (eq? source-storage-class f64-storage-class)
                                              (eq? destination-storage-class f32-storage-class)))
                                        (lambda (x y)
                                           (< (magnitude (- x y)) 1e-5))
                                        equal?)))
                           #t))
                     (test (array-assign! destination source)
                        "array-assign!: Not all elements of the source can be stored in destination: "))))))

   (next-test-random-source-state!))

(define (array-copy-and-array-copy!-error-tests)
   (pp "array-copy and array-copy! error tests")

   (for-each (lambda (call/cc-safe?)
                (let* ((array-copy (if call/cc-safe?
                                       array-copy
                                       array-copy!))
                       (message    (if call/cc-safe?
                                       "array-copy: "
                                       "array-copy!: ")))

                   (define (wrap error-reason)
                      (string-append message error-reason))

                   (test (array-copy (make-array (make-interval '#(4)) list) u8-storage-class #t 'a)
                      (wrap "The fourth argument is not a boolean: "))

                   (test (array-copy (make-array (make-interval '#(4)) list) u8-storage-class 'a #t)
                      (wrap "The third argument is not a boolean: "))

                   (test (array-copy (make-array (make-interval '#(4)) list) 'u8-storage-class #t #t)
                      (wrap "The second argument is not a storage-class: "))

                   (test (array-copy 'a)
                      (wrap "The first argument is not an array: "))

                   (test (array-copy #f generic-storage-class)
                      (wrap "The first argument is not an array: "))

                   (test (array-copy (make-array (make-interval '#(1) '#(2))
                                        list)
                            #f)
                      (wrap "The second argument is not a storage-class: "))

                   (test (array-copy (make-array (make-interval '#(1) '#(2))
                                        list)
                            generic-storage-class
                            'a)
                      (wrap "The third argument is not a boolean: "))


                   (test (array-copy (make-array (make-interval '#(1) '#(2))
                                        list)
                            generic-storage-class
                            #f
                            'a)
                      (wrap "The fourth argument is not a boolean: "))

                   ;; Check that explicit setting of mutable? and safe? work


                   (test (mutable-array? (array-copy (list->array (make-interval '#(2 2))
                                                        '(1 2 3 4)
                                                        generic-storage-class
                                                        #f)))
                      #f)

                   (test (mutable-array? (array-copy (list->array (make-interval '#(2 2))
                                                        '(1 2 3 4)
                                                        generic-storage-class
                                                        #f)
                                            generic-storage-class
                                            #t))
                      #t)


                   (test (array-safe? (array-copy (list->array (make-interval '#(2 2))
                                                     '(1 2 3 4)
                                                     generic-storage-class
                                                     #f
                                                     #f)))
                      #f)

                   (test (array-safe? (array-copy (list->array (make-interval '#(2 2))
                                                     '(1 2 3 4)
                                                     generic-storage-class
                                                     #f
                                                     #f)
                                         generic-storage-class
                                         #t
                                         #t))
                      #t)

                   ;; Check that defaults of mutable? and safe? work

                   (parameterize
                      ((specialized-array-default-mutable? #t))
                      (test (mutable-array? (array-copy (list->array (make-interval '#(2 2))
                                                           '(1 2 3 4)
                                                           generic-storage-class
                                                           #t)))
                         #t)

                      (test (mutable-array? (array-copy (list->array (make-interval '#(2 2))
                                                           '(1 2 3 4)
                                                           generic-storage-class
                                                           #t)
                                               generic-storage-class
                                               #f))
                         #f)
                      (test (mutable-array? (array-copy (make-array (make-interval '#(2 2)) list)))
                         #t)

                      (test (mutable-array? (array-copy (make-array (make-interval '#(2 2)) list)
                                               generic-storage-class
                                               #f))
                         #f))

                   (parameterize
                      ((specialized-array-default-mutable? #f))
                      (test (array-safe? (array-copy (list->array (make-interval '#(2 2))
                                                        '(1 2 3 4)
                                                        generic-storage-class
                                                        #t
                                                        #t)))
                         #t)

                      (test (array-safe? (array-copy (list->array (make-interval '#(2 2))
                                                        '(1 2 3 4)
                                                        generic-storage-class
                                                        #t
                                                        #t)
                                            generic-storage-class
                                            #f
                                            #f))
                         #f)

                      (test (mutable-array? (array-copy (make-array (make-interval '#(2 2)) list)))
                         #f)

                      (test (mutable-array? (array-copy (make-array (make-interval '#(2 2)) list)
                                               generic-storage-class
                                               #t))
                         #t))

                   (parameterize
                      ((specialized-array-default-safe? #t))
                      (test (mutable-array? (array-copy (list->array (make-interval '#(2 2))
                                                           '(1 2 3 4)
                                                           generic-storage-class
                                                           #f)))
                         #f)

                      (test (mutable-array? (array-copy (list->array (make-interval '#(2 2))
                                                           '(1 2 3 4)
                                                           generic-storage-class
                                                           #f)
                                               generic-storage-class
                                               #t))
                         #t)
                      (test (array-safe? (array-copy (make-array (make-interval '#(2 2)) list)))
                         #t)

                      (test (array-safe? (array-copy (make-array (make-interval '#(2 2)) list)
                                            generic-storage-class
                                            #f
                                            #f))
                         #f))

                   (parameterize
                      ((specialized-array-default-safe? #f))
                      (test (array-safe? (array-copy (list->array (make-interval '#(2 2))
                                                        '(1 2 3 4)
                                                        generic-storage-class
                                                        #f
                                                        #f)))
                         #f)

                      (test (array-safe? (array-copy (list->array (make-interval '#(2 2))
                                                        '(1 2 3 4)
                                                        generic-storage-class
                                                        #f
                                                        #f)
                                            generic-storage-class
                                            #t
                                            #t))
                         #t)

                      (test (array-safe? (array-copy (make-array (make-interval '#(2 2)) list)))
                         #f)

                      (test (array-safe? (array-copy (make-array (make-interval '#(2 2)) list)
                                            generic-storage-class
                                            #t
                                            #t))
                         #t))



                   ;; We gotta make sure than the error checks work in all dimensions ...

                   (test (array-copy (make-array (make-interval '#(1) '#(2))
                                        list)
                            u16-storage-class)
                      (wrap "Not all elements of the source can be stored in destination: "))

                   (test (array-copy (make-array (make-interval '#(1 1) '#(2 2))
                                        list)
                            u16-storage-class)
                      (wrap "Not all elements of the source can be stored in destination: "))

                   (test (array-copy (make-array (make-interval '#(1 1 1) '#(2 2 2))
                                        list)
                            u16-storage-class)
                      (wrap "Not all elements of the source can be stored in destination: "))

                   (test (array-copy (make-array (make-interval '#(1 1 1 1) '#(2 2 2 2))
                                        list)
                            u16-storage-class)
                      (wrap "Not all elements of the source can be stored in destination: "))

                   (test (array-copy (make-array (make-interval '#(1 1 1 1 1) '#(2 2 2 2 2))
                                        list)
                            u16-storage-class)
                      (wrap "Not all elements of the source can be stored in destination: "))
                   ))
      '(#t #f))

   (test (specialized-array-default-safe? 'a)
      "specialized-array-default-safe?: The argument is not a boolean: ")

   (test (specialized-array-default-mutable? 'a)
      "specialized-array-default-mutable?: The argument is not a boolean: ")

   (let ((mutable-default (specialized-array-default-mutable?)))
      (specialized-array-default-mutable? #f)
      (do ((i 1 (+ i 1)))
          ((= i 6))
          (let ((A (array-copy (make-array (make-interval (make-vector i 2)) (lambda args 10)))))
             (test (apply array-set! A 0 (make-list i 0))
                "array-set!: The first argument is not a mutable array: ")
             (test (array-assign! A A)
                "array-assign!: The destination is not a mutable array: ")))
      (specialized-array-default-mutable? mutable-default)))

(define (array-copy-result-tests)
   (pp "array-copy result tests")

   (specialized-array-default-safe? #t)

   (pp "Safe tests")

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((domain
                 (random-interval))
              (lower-bounds
                 (interval-lower-bounds->list domain))
              (upper-bounds
                 (interval-upper-bounds->list domain))
              (array1
                 (let ((alist '()))
                    (make-array
                       domain
                       (lambda indices
                          (cond ((assoc indices alist)
                                 => cdr)
                                (else
                                 indices)))
                       (lambda (value . indices)
                          (cond ((assoc indices alist)
                                 =>(lambda (entry)
                                      (set-cdr! entry value)))
                                (else
                                 (set! alist (cons (cons indices value)
                                                alist))))))))
              (array2
                 (array-copy array1 generic-storage-class))
              (array2!
                 (array-copy! array1 generic-storage-class))
              (setter1
                 (array-setter array1))
              (setter2
                 (array-setter array2))
              (setter2!
                 (array-setter array2!)))
          (if (not (array-empty? array1))
              (do ((j 0 (+ j 1)))
                  ((= j 25))
                  (let ((v (random 1000))
                        (indices (map random lower-bounds upper-bounds)))
                     (apply setter1 v indices)
                     (apply setter2 v indices)
                     (apply setter2! v indices))))
          (test (myarray= array1 array2) #t)
          (test (myarray= array1 array2!) #t)
          (test (myarray= (array-copy array1 generic-storage-class) array2) #t)
          (test (myarray= (array-copy array1 generic-storage-class) array2!) #t)))

   (next-test-random-source-state!)

   (specialized-array-default-safe? #f)

   (pp "Unsafe tests")

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((domain
                 (random-interval))
              (lower-bounds
                 (interval-lower-bounds->list domain))
              (upper-bounds
                 (interval-upper-bounds->list domain))
              (array1
                 (let ((alist '()))
                    (make-array
                       domain
                       (lambda indices
                          (cond ((assoc indices alist)
                                 => cdr)
                                (else
                                 indices)))
                       (lambda (value . indices)
                          (cond ((assoc indices alist)
                                 =>(lambda (entry)
                                      (set-cdr! entry value)))
                                (else
                                 (set! alist (cons (cons indices value)
                                                alist))))))))
              (array2
                 (array-copy array1 generic-storage-class))
              (array2!
                 (array-copy! array1 generic-storage-class))
              (setter1
                 (array-setter array1))
              (setter2
                 (array-setter array2))
              (setter2!
                 (array-setter array2!)))
          (if (not (array-empty? array1))
              (do ((j 0 (+ j 1)))
                  ((= j 25))
                  (let ((v (random 1000))
                        (indices (map random lower-bounds upper-bounds)))
                     (apply setter1 v indices)
                     (apply setter2 v indices)
                     (apply setter2! v indices))))
          (test (myarray= array1 array2) #t)
          (test (myarray= array1 array2!) #t)
          (test (myarray= (array-copy array1 generic-storage-class) array2) #t)
          (test (myarray= (array-copy array1 generic-storage-class) array2!) #t)))

   (next-test-random-source-state!))

(define (array-map-error-tests)
   (pp "array-map error tests")

   (test (array-map 1 #f)
      "array-map: The first argument is not a procedure: ")

   (test (array-map list 1 (make-array (make-interval '#(3) '#(4))
                              list))
      "array-map: Not all arguments after the first are arrays: ")

   (test (array-map list (make-array (make-interval '#(3) '#(4))
                            list) 1)
      "array-map: Not all arguments after the first are arrays: ")

   (test (array-map list
            (make-array (make-interval '#(3) '#(4))
               list)
            (make-array (make-interval '#(3 4) '#(4 5))
               list))
      "array-map: Not all arrays have the same domain: "))

(define (array-every-and-array-any-error-tests)
   (pp "array-every and array-any error tests")

   (test (array-every 1 2)
      "array-every: The first argument is not a procedure: ")

   (test (array-every list 1)
      "array-every: Not all arguments after the first are arrays: ")

   (test (array-every list
            (make-array (make-interval '#(3) '#(4))
               list)
            1)
      "array-every: Not all arguments after the first are arrays: ")

   (test (array-every list
            (make-array (make-interval '#(3) '#(4))
               list)
            (make-array (make-interval '#(3 4) '#(4 5))
               list))
      "array-every: Not all arrays have the same domain: ")

   (test (array-any 1 2)
      "array-any: The first argument is not a procedure: ")

   (test (array-any list 1)
      "array-any: Not all arguments after the first are arrays: ")

   (test (array-any list
            (make-array (make-interval '#(3) '#(4))
               list)
            1)
      "array-any: Not all arguments after the first are arrays: ")

   (test (array-any list
            (make-array (make-interval '#(3) '#(4))
               list)
            (make-array (make-interval '#(3 4) '#(4 5))
               list))
      "array-any: Not all arrays have the same domain: "))

(define (array-every-and-array-any-tests)
   (pp "array-every and array-any")

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((interval
                 (random-nonnegative-interval 1 6))
              (n
                 (interval-volume interval))
              (separator
                 ;; I want to make sure that the last item is chosen at least
                 ;; once for each random
                 (random (max 0 (- n 10)) n))
              (indexer
                 (%%interval->basic-indexer interval))
              (arguments-1
                 '())
              (array-1
                 (make-array interval
                    (lambda args
                       (set! arguments-1 (cons args
                                            arguments-1))
                       (let ((index (apply indexer args)))
                          (cond ((< index separator)
                                 #f)
                                ((= index separator)
                                 1)
                                (else
                                 (test-error "The array should never be called with these args"
                                    interval
                                    separator
                                    args
                                    index)))))))
              (arguments-2
                 '())
              (array-2
                 (make-array interval
                    (lambda args
                       (set! arguments-2 (cons args
                                            arguments-2))
                       (let ((index (apply indexer args)))
                          (cond ((< index separator)
                                 #t)
                                ((= index separator)
                                 #f)
                                (else
                                 (test-error "The array should never be called with these args"
                                    interval
                                    separator
                                    args
                                    index))))))))
          (test (array-any values array-1)
             1)
          (test (array-every values array-2)
             #f)
          (if (not (indices-in-proper-order (reverse arguments-1)))
              (test-error "arrghh arguments-1" arguments-1))
          (if (not (indices-in-proper-order (reverse arguments-2)))
              (test-error "arrghh arguments-2" arguments-2))))

   (next-test-random-source-state!))


(define (array-foldlr-error-tests)
   (pp "array-fold[lr] error tests")

   (test (array-fold-left 1 1 1)
      "array-fold-left: The first argument is not a procedure: ")

   (test (array-fold-left list 1 1)
      "array-fold-left: Not all arguments after the first two are arrays: ")

   (test (array-fold-left list 1 (make-array (make-interval '#()) list) 1)
      "array-fold-left: Not all arguments after the first two are arrays: ")

   (test (array-fold-left list 1 (make-array (make-interval '#()) list) (make-array (make-interval '#(1)) list))
      "array-fold-left: Not all arrays have the same domain: ")

   (test (array-fold-left cons '() (make-array (make-interval '#()) (lambda () 42)))
      '(() . 42))

   (test (array-fold-right cons 42 (make-array (make-interval '#(0)) test-error))
      42)

   (test (array-fold-right 1 1 1)
      "array-fold-right: The first argument is not a procedure: ")

   (test (array-fold-right list 1 1)
      "array-fold-right: Not all arguments after the first two are arrays: ")

   (test (array-fold-right list 1 (make-array (make-interval '#()) list) 1)
      "array-fold-right: Not all arguments after the first two are arrays: ")

   (test (array-fold-right list 1 (make-array (make-interval '#()) list) (make-array (make-interval '#(1)) list))
      "array-fold-right: Not all arrays have the same domain: ")

   (test (array-fold-right cons '() (make-array (make-interval '#()) (lambda () 42)))
      '(42))

   (test (array-fold-right cons 42 (make-array (make-interval '#(0)) test-error))
      42)
   )


(define (array-for-each-error-tests)
   (pp "array-for-each error tests")

   (test (array-for-each 1 #f)
      "array-for-each: The first argument is not a procedure: ")

   (test (array-for-each list 1 (make-array (make-interval '#(3) '#(4))
                                   list))
      "array-for-each: Not all arguments after the first are arrays: ")

   (test (array-for-each list (make-array (make-interval '#(3) '#(4))
                                 list) 1)
      "array-for-each: Not all arguments after the first are arrays: ")

   (test (array-for-each list
            (make-array (make-interval '#(3) '#(4))
               list)
            (make-array (make-interval '#(3 4) '#(4 5))
               list))
      "array-for-each: Not all arrays have the same domain: "))


(define (array-map-array-fold-right-and-array-for-each-result-tests)
   (pp "array-map, array-fold-right, and array-for-each result tests")

   (let ((list-of-60 (iota 60)))
      (for-each (lambda (divisor)   ;; 1 through 5
                   ;; Break up list-of-60 into equivalence classes modulo divisor
                   ;; Convert these to arrays.
                   ;; Do a simple test on array-fold-left and array-fold-right with cons and '()
                   (let* ((specialized-parts
                             (map (lambda (remainder)
                                     (list*->array
                                        1
                                        (filter (lambda (j)
                                                   (eqv? (modulo j divisor) remainder))
                                           list-of-60)))
                                (iota divisor)))
                          (generalized-parts
                             (map (lambda (a)
                                     (make-array (array-domain a)
                                        (array-getter a)))
                                specialized-parts)))
                      (test (apply array-fold-left
                               (lambda (id . lst)
                                  (foldl cons id lst))
                               '()
                               specialized-parts)
                         (foldl cons '() list-of-60))
                      (test (apply array-fold-right
                               (lambda args
                                  (foldr cons (list-ref args divisor) (take args divisor)))
                               '()
                               specialized-parts)
                         (foldr cons '() list-of-60))
                      (test (apply array-fold-left
                               (lambda (id . lst)
                                  (foldl cons id lst))
                               '()
                               generalized-parts)
                         (foldl cons '() list-of-60))
                      (test (apply array-fold-right
                               (lambda args
                                  (foldr cons (list-ref args divisor) (take args divisor)))
                               '()
                               generalized-parts)
                         (foldr cons '() list-of-60))
                      ))
         (iota 6 1)))

   (specialized-array-default-safe? #t)

   (let ((array-builders (vector (list u1-storage-class      (lambda indices (random 0 (expt 2 1))))
                            (list u8-storage-class           (lambda indices (number->uint8 (random 0 (expt 2 8)))))
                            (list u16-storage-class          (lambda indices (number->uint16 (random 0 (expt 2 16)))))
                            (list u32-storage-class          (lambda indices (number->uint32 (random 0 (expt #z2 32)))))
                            (list u64-storage-class          (lambda indices (number->uint64 (random 0 (expt #z2 64)))))
                            (list s8-storage-class           (lambda indices (number->int8 (random (- (expt 2 7))  (expt 2 7)))))
                            (list s16-storage-class          (lambda indices (number->int16 (random (- (expt 2 15)) (expt 2 15)))))
                            (list s32-storage-class          (lambda indices (number->int32 (random (- (expt #z2 31)) (expt #z2 31)))))
                            (list s64-storage-class          (lambda indices (number->int64 (random (- (expt #z2 63)) (expt #z2 63)))))
                            (list f32-storage-class     (lambda indices (test-random-real)))
                            (list f64-storage-class     (lambda indices (test-random-real)))
                            (list char-storage-class    (lambda indices (random-char)))
                            ;(list c64-storage-class     (lambda indices (make-rectangular (test-random-real) (test-random-real))))
                            ;(list c128-storage-class    (lambda indices (make-rectangular (test-random-real) (test-random-real))))
                            (list generic-storage-class (lambda indices indices)))))
      (do ((i 0 (+ i 1)))
          ((= i random-tests))
          (let* ((domain
                    (random-interval))
                 (lower-bounds
                    (interval-lower-bounds->list domain))
                 (upper-bounds
                    (interval-upper-bounds->list domain))
                 (array-length
                    (lambda (a)
                       (let ((upper-bounds (interval-upper-bounds->list (array-domain a)))
                             (lower-bounds (interval-lower-bounds->list (array-domain a))))
                          (apply * (map - upper-bounds lower-bounds)))))
                 (arrays
                    (map (lambda (ignore)
                            (let ((array-builder (vector-ref array-builders (random (vector-length array-builders)))))
                               (array-copy (make-array domain
                                              (cadr array-builder))
                                  (car array-builder))))
                       (local-iota 0 (random 1 7))))
                 (result-array-1
                    (apply array-map
                       list
                       arrays))
                 (result-array-2
                    (array-copy
                       (apply array-map
                          list
                          arrays)))
                 (getters
                    (map array-getter arrays))
                 (result-array-3
                    (make-array domain
                       (lambda indices
                          (map (lambda (g) (apply g indices)) getters)))))
             (test (myarray= result-array-1 result-array-2)
                #t)
             (test (myarray= result-array-2 result-array-3)
                #t)
             (test (vector->list (array-body result-array-2))
                (array-fold-right (lambda (x y) (cons x y))
                   '()
                   result-array-2))
             (test (vector->list (array-body result-array-2))
                (reverse (let ((result '()))
                            (array-for-each (lambda (f)
                                               (set! result (cons f result)))
                               result-array-2)
                            result)))
             (test  (map array-length arrays)
                (map (lambda (array)
                        ((storage-class-length (array-storage-class array)) (array-body array)))
                   arrays)))))
   
   (next-test-random-source-state!)

   (specialized-array-default-safe? #f)

   (let ((array-builders (vector (list u1-storage-class      (lambda indices (random (expt 2 1))))
                            (list u8-storage-class      (lambda indices (number->uint8 (random (expt 2 8)))))
                            (list u16-storage-class     (lambda indices (number->uint16 (random (expt 2 16)))))
                            (list u32-storage-class     (lambda indices (number->uint32 (random (expt #z2 32)))))
                            (list u64-storage-class     (lambda indices (number->uint64 (random (expt #z2 64)))))
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
          (let* ((domain
                    (random-interval))
                 (lower-bounds
                    (interval-lower-bounds->list domain))
                 (upper-bounds
                    (interval-upper-bounds->list domain))
                 (arrays
                    (map (lambda (ignore)
                            (let ((array-builder (vector-ref array-builders (random (vector-length array-builders)))))
                               (array-copy (make-array domain
                                              (cadr array-builder))
                                  (car array-builder))))
                       (local-iota 0 (random 1 7))))
                 (result-array-1
                    (apply array-map
                       list
                       arrays))
                 (result-array-2
                    (array-copy
                       (apply array-map
                          list
                          arrays)))
                 (getters
                    (map array-getter arrays))
                 (result-array-3
                    (make-array domain
                       (lambda indices
                          (map (lambda (g) (apply g indices)) getters)))))
             (test (myarray= result-array-1 result-array-2)
                #t)
             (test (myarray= result-array-2 result-array-3)
                #t)
             (test (vector->list (array-body result-array-2))
                (array-fold-right cons
                   '()
                   result-array-2))
             (test (vector->list (array-body result-array-2))
                (reverse (let ((result '()))
                            (array-for-each (lambda (f)
                                               (set! result (cons f result)))
                               result-array-2)
                            result))))))

   (next-test-random-source-state!))

(define (array-reduce-tests)
   (pp "array-reduce tests")

   (test (array-reduce 'a 'a)
      "array-reduce: The second argument is not an array: ")

   (test (array-reduce 'a (make-array (make-interval '#(1) '#(3)) list))
      "array-reduce: The first argument is not a procedure: ")

   (test (array-reduce + (make-array (make-interval '#(0)) test-error))
      "array-reduce: Attempting to reduce over an empty array: ")

   (test (array-reduce (lambda (a b) test-error) (make-array (make-interval '#()) (lambda () 42)))
      42)

;;; OK, how to test array-reduce?

;;; Well, we take an associative, non-commutative operation,
;;; multiplying 2x2 matrices, with data such that doing operations
;;; in the opposite order gives the wrong answer, doing it for the
;;; wrong interval (e.g., swapping axes) gives the wrong answer.

;;; This is not in the same style as the other tests, which use random
;;; data to a great extent, but I couldn't see how to choose random
;;; data that would satisfy the constraints.


   (define matrix vector)

   (define (2x2-multiply A B)
      (let ((a_11 (vector-ref A 0)) (a_12 (vector-ref A 1))
                                    (a_21 (vector-ref A 2)) (a_22 (vector-ref A 3))
                                    (b_11 (vector-ref B 0)) (b_12 (vector-ref B 1))
                                    (b_21 (vector-ref B 2)) (b_22 (vector-ref B 3)))
         (vector (+ (* a_11 b_11) (* a_12 b_21)) (+ (* a_11 b_12) (* a_12 b_22))
            (+ (* a_21 b_11) (* a_22 b_21)) (+ (* a_21 b_12) (* a_22 b_22)))))

   (define A (make-array (make-interval '#(1) '#(11))
                (lambda (i)
                   (if (even? i)
                       (matrix 1 i
                          0 1)
                       (matrix 1 0
                          i 1)))))

   (test (array-reduce 2x2-multiply A)
      (array-fold-right 2x2-multiply (matrix 1 0 0 1) A))

   (test (array-reduce 2x2-multiply A)
      (array-fold-left 2x2-multiply (matrix 1 0 0 1) A))


   (define A_2 (make-array (make-interval '#(1 1) '#(3 7))
                  (lambda (i j)
                     (if (and (even? i) (even? j))
                         (matrix 1 i
                            j 1)
                         (matrix 1 j
                            i -1)))))

   (test (array-reduce 2x2-multiply A_2)
      (array-fold-right 2x2-multiply (matrix 1 0 0 1) A_2))

   (test (array-reduce 2x2-multiply A_2)
      (array-fold-left 2x2-multiply (matrix 1 0 0 1) A_2))

   (test (equal? (array-reduce 2x2-multiply A_2)
            (array-reduce 2x2-multiply (array-permute A_2 (index-rotate (array-dimension A_2) 1))))
      #f)

   (define A_3 (make-array (make-interval '#(1 1 1) '#(3 5 4))
                  (lambda (i j k)
                     (if (and (even? i) (even? j))
                         (matrix 1 i
                            j k)
                         (matrix k j
                            i -1)))))

   (test (array-reduce 2x2-multiply A_3)
      (array-fold-right 2x2-multiply (matrix 1 0 0 1) A_3))

   (test (array-reduce 2x2-multiply A_3)
      (array-fold-left 2x2-multiply (matrix 1 0 0 1) A_3))

   (test (equal? (array-reduce 2x2-multiply A_3)
            (array-reduce 2x2-multiply (array-permute A_3 (index-rotate (array-dimension A_3) 1))))
      #f)

   (define A_4 (make-array (make-interval '#(1 1 1 1) '#(3 2 4 3))
                  (lambda (i j k l)
                     (if (and (even? i) (even? j))
                         (matrix l i
                            j k)
                         (matrix l k
                            i j)))))

   (test (array-reduce 2x2-multiply A_4)
      (array-fold-right 2x2-multiply (matrix 1 0 0 1) A_4))

   (test (array-reduce 2x2-multiply A_4)
      (array-fold-left 2x2-multiply (matrix 1 0 0 1) A_4))

   (test (equal? (array-reduce 2x2-multiply A_4)
            (array-reduce 2x2-multiply (array-permute A_4 (index-rotate (array-dimension A_4) 1))))
      #f)

   (define A_5 (make-array (make-interval '#(1 1 1 1 1) '#(3 2 4 3 3))
                  (lambda (i j k l m)
                     (if (even? m)
                         (matrix (+ m l) i
                            j k)
                         (matrix (- l m) k
                            i j)))))

   (test (array-reduce 2x2-multiply A_5)
      (array-fold-right 2x2-multiply (matrix 1 0 0 1) A_5))


   (test (equal? (array-reduce 2x2-multiply A_5)
            (array-reduce 2x2-multiply (array-permute A_5 (index-rotate (array-dimension A_5) 1))))      #f))

(define (some-array-curry-tests)
   (pp "Some array-curry tests.")

   (test (array-curry 'a 1)
      "array-curry: The first argument is not an array: ")

   (test (array-curry (make-array (make-interval '#(0) '#(1)) list)  'a)
      "array-curry: The second argument is not an exact integer between 0 and (interval-dimension (array-domain array)) (inclusive): ")

   (test (array-curry (make-array (make-interval '#(0 0) '#(1 1)) list)  -1)
      "array-curry: The second argument is not an exact integer between 0 and (interval-dimension (array-domain array)) (inclusive): ")

   (test (array-curry (make-array (make-interval '#(0 0) '#(1 1)) list)  3)
      "array-curry: The second argument is not an exact integer between 0 and (interval-dimension (array-domain array)) (inclusive): ")

   (let* ((dim 6)
          (domain (make-interval (make-vector dim 3)))
          (immutable (make-array domain list))
          (mutable   (make-array domain list list)) ;; nonsensical
          (special   (make-specialized-array domain)))
      (do ((left-dim 1 (+ left-dim 1)))
          ((> left-dim dim))
          (let* ((right-dim (- dim left-dim))
                 (immutable-curry (array-curry immutable right-dim))
                 (mutable-curry(array-curry  mutable right-dim))
                 (special-curry (array-curry special right-dim)))
             (for-each (lambda (array)
                          (test (apply array-ref array (make-list left-dim 100))
                             "array-curry: domain does not contain multi-index: ")
                          (test (apply array-ref array (make-list left-dim 'a))
                             "array-curry: multi-index component is not an exact integer: ")
                          (if (< 4 left-dim)
                              (test (apply array-ref array '(0 0))
                                 "array-curry: multi-index is not the correct dimension: ")))
                (list immutable-curry mutable-curry special-curry)))))

   (let ((array-builders (vector (list u1-storage-class      (lambda indices (random (expt 2 1))))
                            (list u8-storage-class      (lambda indices (number->uint8 (random (expt 2 8)))))
                            (list u16-storage-class     (lambda indices (number->uint16 (random (expt 2 16)))))
                            (list u32-storage-class     (lambda indices (number->uint32 (random (expt #z2 32)))))
                            (list u64-storage-class     (lambda indices (number->uint64 (random (expt #z2 64)))))
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
          (let* ((domain
                    (random-interval 0 7))
                 (lower-bounds
                    (interval-lower-bounds->list domain))
                 (upper-bounds
                    (interval-upper-bounds->list domain))
                 (array-builder
                    (vector-ref array-builders (random (vector-length array-builders))))
                 (random-array-element
                    (cadr array-builder))
                 (storage-class
                    (car array-builder))
                 (Array
                    (array-copy (make-array domain
                                   random-array-element)
                       storage-class))
                 (copied-array
                    (array-copy Array
                       storage-class))
                 (inner-dimension
                    (random-inclusive (interval-dimension domain)))
                 (domains
                    (call-with-values (lambda ()(interval-projections domain inner-dimension)) list))
                 (outer-domain
                    (car domains))
                 (inner-domain
                    (cadr domains))
                 (immutable-curry
                    (array-curry (make-array (array-domain Array)
                                    (array-getter Array))
                       inner-dimension))
                 (mutable-curry
                    (array-curry (make-array (array-domain Array)
                                    (array-getter Array)
                                    (array-setter Array))
                       inner-dimension))
                 (specialized-curry
                    (array-curry Array inner-dimension))
                 (immutable-curry-from-definition
                    (call-with-values
                       (lambda () (interval-projections (array-domain Array) inner-dimension))
                       (lambda (outer-interval inner-interval)
                          (make-array outer-interval
                             (lambda outer-multi-index
                                (make-array inner-interval
                                   (lambda inner-multi-index
                                      (apply (array-getter Array) (append outer-multi-index inner-multi-index)))))))))
                 (mutable-curry-from-definition
                    (call-with-values
                       (lambda () (interval-projections (array-domain Array) inner-dimension))
                       (lambda (outer-interval inner-interval)
                          (make-array outer-interval
                             (lambda outer-multi-index
                                (make-array inner-interval
                                   (lambda inner-multi-index
                                      (apply (array-getter Array) (append outer-multi-index inner-multi-index)))
                                   (lambda (v . inner-multi-index)
                                      (apply (array-setter Array) v (append outer-multi-index inner-multi-index)))))))))
                 (specialized-curry-from-definition
                    (call-with-values
                       (lambda () (interval-projections (array-domain Array) inner-dimension))
                       (lambda (outer-interval inner-interval)
                          (make-array outer-interval
                             (lambda outer-multi-index
                                (specialized-array-share Array
                                   inner-interval
                                   (lambda inner-multi-index
                                      (apply values (append outer-multi-index inner-multi-index))))))))))
             ;; mutate the curried array
             (if (and (not (interval-empty? outer-domain))
                      (not (interval-empty? inner-domain)))
                 (for-each (lambda (curried-array)
                              (let ((outer-getter
                                       (array-getter curried-array)))
                                 (do ((i 0 (+ i 1)))
                                     ((= i 50))  ;; used to be tests, not 50, but 50 will do fine
                                     (call-with-values
                                        (lambda ()
                                           (random-multi-index outer-domain))
                                        (lambda outer-multi-index
                                           (let ((inner-setter
                                                    (array-setter (apply outer-getter outer-multi-index))))
                                              (call-with-values
                                                 (lambda ()
                                                    (random-multi-index inner-domain))
                                                 (lambda inner-multi-index
                                                    (let ((new-element
                                                             (random-array-element)))
                                                       (apply inner-setter new-element inner-multi-index)
                                                       ;; mutate the copied array without currying
                                                       (apply (array-setter copied-array) new-element (append outer-multi-index inner-multi-index)))))))))))
                    (list mutable-curry
                       specialized-curry
                       mutable-curry-from-definition
                       specialized-curry-from-definition)))

             (test (myarray= Array copied-array) #t)
             (test (array-every array? immutable-curry) #t)
             (test (array-every (lambda (a) (not (mutable-array? a))) immutable-curry) #t)
             (test (array-every (lambda (a) (not (specialized-array? a))) mutable-curry) #t)
             (test (array-every specialized-array? specialized-curry) #t)
             (test (array-every (lambda (xy) (apply myarray= xy))
                      (array-map list immutable-curry immutable-curry-from-definition))
                #t)
             (test (array-every (lambda (xy) (apply myarray= xy))
                      (array-map list mutable-curry mutable-curry-from-definition))
                #t)
             (test (array-every (lambda (xy) (apply myarray= xy))
                      (array-map list specialized-curry specialized-curry-from-definition))
                #t))))


   (next-test-random-source-state!))

(define (array-decurry-and-array-decurry!-tests)
   (pp "array-decurry and array-decurry! tests")
   
   (for-each (lambda (call/cc-safe?)
                (let ((array-decurry
                         (if call/cc-safe?
                             array-decurry
                             array-decurry!))
                      (message
                         (if call/cc-safe?
                             "array-decurry: "
                             "array-decurry!: ")))

                   (define (wrap error-reason)
                      (string-append message error-reason))

                   (test (array-decurry 'a)
                      (wrap "The first argument is not an array: "))

                   (test (array-decurry (make-array (make-interval '#(0)) list))
                      (wrap "The first argument is an empty array: "))

                   (test (array-decurry (make-array (make-interval '#()) list) 'a)
                      (wrap "The second argument is not a storage class: "))

                   (test (array-decurry (make-array (make-interval '#()) list) generic-storage-class 'a)
                      (wrap "The third argument is not a boolean: "))

                   (test (array-decurry (make-array (make-interval '#()) list) generic-storage-class #f 'a)
                      (wrap "The fourth argument is not a boolean: "))

                   (test (array-decurry (make-array (make-interval '#()) list))
                      (wrap "Not all elements of the first argument (an array) are arrays: "))

                   (test (array-decurry (list*->array 1 (list (make-array (make-interval '#()) list)
                                                           (make-array (make-interval '#(1)) list))))
                      (wrap "Not all elements of the first argument (an array) have the domain: "))

                   (test (array-decurry (list*->array 1 (list (make-array (make-interval '#(1)) list)
                                                           (make-array (make-interval '#(1)) list)))
                            u1-storage-class)
                      (wrap "Not all elements of the source can be stored in destination: "))
                   ))
      '(#t #f))

   (define (my-array-decurry  A)
      (let* ((A
                (array-copy A))      ;; evaluate all elements of A once
             (A_dim
                (array-dimension A))
             (A_
                (array-getter A))
             (A_D
                (array-domain A))
             (element-domain
                (array-domain (apply A_ (interval-lower-bounds->list A_D))))
             (result-domain
                (interval-cartesian-product A_D (array-domain (apply A_ (interval-lower-bounds->list A_D)))))
             (result
                (make-specialized-array result-domain u1-storage-class))
             (curried-result
                (array-curry result (interval-dimension element-domain))))
         (array-for-each array-assign! result A)
         result))

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((outer-domain
                 (random-nonempty-interval 0 5))
              (inner-domain
                 (random-interval 0 5))
              (A
                 (array-copy (make-array (interval-cartesian-product outer-domain inner-domain)
                                (lambda args
                                   (random 2)))
                    u1-storage-class))
              (A-curried
                 (array-curry A (interval-dimension inner-domain)))
              (A-curried
                 (array-map (lambda (A) (make-array (array-domain A) (array-getter A))) A-curried))
              (A-decurried!
                 (array-decurry! A-curried u1-storage-class))
              (A-decurried
                 (array-decurry A-curried u1-storage-class)))
          (test (myarray= A A-decurried)
             #t)
          (test (myarray= A A-decurried!)
             #t)))

   (next-test-random-source-state!))

(define (specialized-array-share-error-tests)
   (pp "specialized-array-share error tests")

   (test (specialized-array-share 1 1 1)
      "specialized-array-share: The first argument is not a specialized-array: ")

   (test (specialized-array-share (make-specialized-array (make-interval '#(1) '#(2)))
            1 1)
      "specialized-array-share: The second argument is not an interval: ")

   (test (specialized-array-share (make-specialized-array (make-interval '#(1) '#(2)))
            (make-interval '#(0) '#(1))
            1)
      "specialized-array-share: The third argument is not a procedure: ")

   (test (specialized-array-share (make-specialized-array (make-interval '#(0 0)))
            (make-interval '#(1))
            (lambda (i) (values i i)))
      "specialized-array-share: The second argument (a domain) has more elements than the domain of the first argument (an array): ")


   (test (myarray= (list->array (make-interval '#(0) '#(10))
                      (reverse (local-iota 0 10)))
            (specialized-array-share (list->array (make-interval '#(0) '#(10))
                                        (local-iota 0 10))
               (make-interval '#(0) '#(10))
               (lambda (i)
                  (- 9 i))))
      #t))

(define (specialized-array-share-results-tests)
   (pp "specialized-array-share result tests")

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((n (random 1 11))
              (permutation (random-permutation n))
              (input-vec (list->vector (f64vector->list (random-f64vector n)))))
          (test (vector-permute input-vec permutation)
             (%%vector-permute input-vec permutation))
          (test (list->vector (%%vector-permute->list input-vec permutation))
             (vector-permute input-vec permutation))))


   (next-test-random-source-state!)


   (specialized-array-default-safe? #t)

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((interval (random-interval))
              (axes (local-iota 0 (interval-dimension interval)))
              (lower-bounds (interval-lower-bounds->vector interval))
              (upper-bounds (interval-upper-bounds->vector interval))
              (a (array-copy (make-array interval list)))
              (new-axis-order (vector-permute (list->vector axes) (random-permutation (length axes))))
              (reverse-order? (list->vector (map (lambda (x) (zero? (random 2))) axes))))
          (let ((b (make-array (make-interval (vector-permute lower-bounds new-axis-order)
                                  (vector-permute upper-bounds new-axis-order))
                      (lambda multi-index
                         (apply (array-getter a)
                            (let* ((n (vector-length new-axis-order))
                                   (multi-index-vector (list->vector multi-index))
                                   (result (make-vector n)))
                               (do ((i 0 (+ i 1)))
                                   ((= i n) (vector->list result))
                                   (vector-set! result (vector-ref new-axis-order i)
                                      (if (vector-ref reverse-order? (vector-ref new-axis-order i))
                                          (+ (vector-ref lower-bounds (vector-ref new-axis-order i))
                                             (- (vector-ref upper-bounds (vector-ref new-axis-order i))
                                                (vector-ref multi-index-vector i)
                                                1))
                                          (vector-ref multi-index-vector i)))))))))
                (c (specialized-array-share a
                      (make-interval (vector-permute lower-bounds new-axis-order)
                         (vector-permute upper-bounds new-axis-order))
                      (lambda multi-index
                         (apply values
                            (let* ((n (vector-length new-axis-order))
                                   (multi-index-vector (list->vector multi-index))
                                   (result (make-vector n)))
                               (do ((i 0 (+ i 1)))
                                   ((= i n) (vector->list result))
                                   (vector-set! result (vector-ref new-axis-order i)
                                      (if (vector-ref reverse-order? (vector-ref new-axis-order i))
                                          (+ (vector-ref lower-bounds (vector-ref new-axis-order i))
                                             (- (vector-ref upper-bounds (vector-ref new-axis-order i))
                                                (vector-ref multi-index-vector i)
                                                1))
                                          (vector-ref multi-index-vector i))))))))))
             (test (myarray= b c)
                #t))))

   (next-test-random-source-state!)

   (specialized-array-default-safe? #f)

   (do ((i 0 (+ i 1)))
       ((= i random-tests))
       (let* ((interval (random-interval))
              (axes (local-iota 0 (interval-dimension interval)))
              (lower-bounds (interval-lower-bounds->vector interval))
              (upper-bounds (interval-upper-bounds->vector interval))
              (a (array-copy (make-array interval list)))
              (new-axis-order (vector-permute (list->vector axes) (random-permutation (length axes))))
              (reverse-order? (list->vector (map (lambda (x) (zero? (random 2))) axes))))
          (let ((b (make-array (make-interval (vector-permute lower-bounds new-axis-order)
                                  (vector-permute upper-bounds new-axis-order))
                      (lambda multi-index
                         (apply (array-getter a)
                            (let* ((n (vector-length new-axis-order))
                                   (multi-index-vector (list->vector multi-index))
                                   (result (make-vector n)))
                               (do ((i 0 (+ i 1)))
                                   ((= i n) (vector->list result))
                                   (vector-set! result (vector-ref new-axis-order i)
                                      (if (vector-ref reverse-order? (vector-ref new-axis-order i))
                                          (+ (vector-ref lower-bounds (vector-ref new-axis-order i))
                                             (- (vector-ref upper-bounds (vector-ref new-axis-order i))
                                                (vector-ref multi-index-vector i)
                                                1))
                                          (vector-ref multi-index-vector i)))))))))
                (c (specialized-array-share a
                      (make-interval (vector-permute lower-bounds new-axis-order)
                         (vector-permute upper-bounds new-axis-order))
                      (lambda multi-index
                         (apply values
                            (let* ((n (vector-length new-axis-order))
                                   (multi-index-vector (list->vector multi-index))
                                   (result (make-vector n)))
                               (do ((i 0 (+ i 1)))
                                   ((= i n) (vector->list result))
                                   (vector-set! result (vector-ref new-axis-order i)
                                      (if (vector-ref reverse-order? (vector-ref new-axis-order i))
                                          (+ (vector-ref lower-bounds (vector-ref new-axis-order i))
                                             (- (vector-ref upper-bounds (vector-ref new-axis-order i))
                                                (vector-ref multi-index-vector i)
                                                1))
                                          (vector-ref multi-index-vector i))))))))))
             (if (not (myarray= b c))
                 (pp (list "piffle"
                        a b c))))))

   (next-test-random-source-state!))

(define (interval-and-array-translation-tests)
   (pp "interval and array translation tests")

   (test (translation? '()) #f)

   (test (translation? '#()) #t)

   (test (translation? '#(a)) #f)

   (test (translation? '#(1.0)) #f)

   (test (translation? '#(1 2)) #t)

   (let ((int (make-interval '#(0 0) '#(10 10)))
         (translation '#(10 -2)))
      (test (interval-translate 'a 10)
         "interval-translate: The first argument is not an interval: ")
      (test (interval-translate int 10)
         "interval-translate: The second argument is not a vector of exact integers: ")
      (test (interval-translate int '#(a b))
         "interval-translate: The second argument is not a vector of exact integers: ")
      (test (interval-translate int '#(1. 2.))
         "interval-translate: The second argument is not a vector of exact integers: ")
      (test (interval-translate int '#(1))
         "interval-translate: The dimension of the first argument (an interval) does not equal the length of the second (a vector): ")
      (do ((i 0 (+ i 1)))
          ((= i random-tests))
          (let* ((int (random-interval))
                 (lower-bounds (interval-lower-bounds->vector int))
                 (upper-bounds (interval-upper-bounds->vector int))
                 (translation (list->vector (map (lambda (x)
                                                    (random -10 10))
                                               (local-iota 0 (vector-length lower-bounds))))))
             (interval= (interval-translate int translation)
                (make-interval (vector-map + lower-bounds translation)
                   (vector-map + upper-bounds translation))))))

   (next-test-random-source-state!)

   (let* ((specialized-array (array-copy (make-array (make-interval '#(0 0) '#(10 12))
                                            list)))
          (mutable-array (let ((temp (array-copy specialized-array)))
                            (make-array (array-domain temp)
                               (array-getter temp)
                               (array-setter temp))))
          (immutable-array (make-array (array-domain mutable-array)
                              (array-getter mutable-array)))
          (translation '#(10 -2)))

      (define (my-array-translate Array translation)
         (let* ((array-copy (array-copy Array))
                (getter (array-getter array-copy))
                (setter (array-setter array-copy)))
            (make-array (interval-translate (array-domain Array)
                           translation)
               (lambda args
                  (apply getter
                     (map - args (vector->list translation))))
               (lambda (v . args)
                  (apply setter
                     v
                     (map - args (vector->list translation)))))))

      (test (array-translate 'a 1)
         "array-translate: The first argument is not an array: ")
      (test (array-translate immutable-array '#(1.))
         "array-translate: The second argument is not a vector of exact integers: ")
      (test (array-translate immutable-array '#(0 2 3))
         "array-translate: The dimension of the first argument (an array) does not equal the dimension of the second argument (a vector): ")
      (let ((specialized-result (array-translate specialized-array translation)))
         (test (specialized-array? specialized-result)
            #t))
      (let ((mutable-result (array-translate mutable-array translation)))
         (test (and (mutable-array? mutable-array)
                    (not (specialized-array? mutable-array))
                    (mutable-array? mutable-result)
                    (not (specialized-array? mutable-result)))
            #t))
      (let ((immutable-result (array-translate immutable-array translation)))
         (test (and (array? immutable-array)
                    (not (mutable-array? immutable-array))
                    (array? immutable-result)
                    (not (mutable-array? immutable-result)))
            #t))

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
                 (translation (list->vector (map (lambda (x) (random -10 10)) (vector->list (%%interval-lower-bounds domain))))))
             ;;(pp (list domain translation (interval-volume domain)))
             (let ((translated-array       (array-translate Array translation))
                   (my-translated-array (my-array-translate Array translation)))
                (if (and (mutable-array? Array)
                         (not (interval-empty? (array-domain Array))))
                    (let ((translated-domain (interval-translate domain translation)))
                       (do ((j 0 (+ j 1)))
                           ((= j 50))
                           (call-with-values
                              (lambda ()
                                 (random-multi-index translated-domain))
                              (lambda multi-index
                                 (let ((value (test-random-integer 10000)))
                                    (apply (array-setter translated-array) value multi-index)
                                    (apply (array-setter my-translated-array) value multi-index)))))))
                (test (myarray= (array-translate Array translation)
                         (my-array-translate Array translation))
                   #t)))))

   (next-test-random-source-state!)

   (let* ((specialized (make-specialized-array (make-interval '#(0 0 0 0 0) '#(1 1 1 1 1))))
          (mutable (make-array (array-domain specialized)
                      (array-getter specialized)
                      (array-setter specialized)))
          (A (array-translate  mutable '#(0 0 0 0 0))))

      (test ((array-getter A) 0 0)
         "The number of indices does not equal the array dimension: ")

      (test ((array-setter A) 'a 0 0)
         "The number of indices does not equal the array dimension: "))

   (next-test-random-source-state!))


(define (interval-and-array-permutation-tests)
   (pp "interval and array permutation tests")

   (test (index-first 'a 'b)
      "index-first: The first argument is not a positive fixnum: ")

   (test (index-first 1. 'b)
      "index-first: The first argument is not a positive fixnum: ")

   (test (index-first -1 2)
      "index-first: The first argument is not a positive fixnum: ")

   (test (index-first 1 'a)
      "index-first: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-first 2 1.0)
      "index-first: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-first 2 2)
      "index-first: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-first 2 -1)
      "index-first: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-last 'a 'b)
      "index-last: The first argument is not a positive fixnum: ")

   (test (index-last 1. 'b)
      "index-last: The first argument is not a positive fixnum: ")

   (test (index-last -1 2)
      "index-last: The first argument is not a positive fixnum: ")

   (test (index-last 1 'a)
      "index-last: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-last 2 1.0)
      "index-last: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-last 2 2)
      "index-last: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-last 2 -1)
      "index-last: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-rotate 'a 'b)
      "index-rotate: The first argument is not a nonnegative fixnum: ")

   (test (index-rotate 1. 'b)
      "index-rotate: The first argument is not a nonnegative fixnum: ")

   (test (index-rotate -1 2)
      "index-rotate: The first argument is not a nonnegative fixnum: ")

   (test (index-rotate 1 'a)
      "index-rotate: The second argument is not a fixnum between 0 and the first argument (inclusive): ")

   (test (index-rotate 2 1.0)
      "index-rotate: The second argument is not a fixnum between 0 and the first argument (inclusive): ")

   (test (index-rotate 2 3)
      "index-rotate: The second argument is not a fixnum between 0 and the first argument (inclusive): ")

   (test (index-rotate 2 -1)
      "index-rotate: The second argument is not a fixnum between 0 and the first argument (inclusive): ")

   (test (index-swap 'a 'b 'c)
      "index-swap: The first argument is not a positive fixnum: ")

   (test (index-swap -1 'b 'c)
      "index-swap: The first argument is not a positive fixnum: ")

   (test (index-swap 1 'b 'c)
      "index-swap: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-swap 1 -1 'c)
      "index-swap: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-swap 1 1 'c)
      "index-swap: The second argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-swap 2 0 'c)
      "index-swap: The third argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-swap 2 0 -1)
      "index-swap: The third argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

   (test (index-swap 2 0 2)
      "index-swap: The third argument is not a fixnum between 0 (inclusive) and the first argument (exclusive): ")

;;; Testing index-*

   (define (my-index-first n k)
      (let ((identity (iota n)))
         (list->vector
            (cons k (append (take identity k)
                       (drop identity (+ k 1)))))))

   (define (my-index-last n k)
      (let ((identity (iota n)))
         (list->vector
            (append (take identity k)
               (drop identity (+ k 1))
               (list k)))))

   (define (my-index-rotate n k)
      (let ((identity (iota n)))
         (list->vector
            (append (drop identity k)
               (take identity k)))))

   (define (my-index-swap n i j)
      (let ((result (list->vector (iota n))))
         (vector-set! result i j)
         (vector-set! result j i)
         result))

   (do ((n 0 (+ n 1)))
       ((= n 6))
       (do ((i 0 (+ i 1)))
           ((= i n)
            (test (index-rotate n i)
               (my-index-rotate n i)))
           (test (index-first n i)
              (my-index-first n i))
           (test (index-last n i)
              (my-index-last n i))
           (test (index-rotate n i)
              (my-index-rotate n i))
           (do ((j 0 (+ j 1)))
               ((= j n))
               (test (index-swap n i j)
                  (my-index-swap n i j))))))








