#|
SRFI 231: Intervals and Generalized Arrays

Copyright 2022, Joseph Donaldson

Bigloo adaption
Bigloo compatability procedures needed for SRFI231 testing

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

(module bigloo_compat
   (export pretty-print
           exact
           inexact
           magnitude
           (write-u8 v #!optional (port (current-output-port)))
           (test-error . objs)
           (all-elements lower upper)
           (local-iota a b)))

(define (test-error . objs)
   (error "test-array"  (if (pair? objs) (car objs) "")
      (if (pair? objs) (cdr objs) "")))

(define pretty-print pp)

(define (write-u8 v #!optional (port (current-output-port)))
   (if port
       (write-byte v port)
       (write-byte v)))

(define exact inexact->exact)
(define inexact exact->inexact)
(define magnitude abs)


(define (local-iota a b)
      (if (= a b)
          '()
          (cons a (local-iota (+ a 1) b))))

(define (all-elements lower upper)
   (if (null? (cdr lower))
       (map list (local-iota (car lower) (car upper)))
       (apply append (map (lambda (x)
                             (map (lambda (y)
                                     (cons x y))
                                (all-elements (cdr lower) (cdr upper))))
                        (local-iota (car lower) (car upper))))))
