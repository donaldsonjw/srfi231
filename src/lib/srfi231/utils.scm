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

(module srfi231-utils
   (include "case-lambda.sci")
   (export (%%every pred list)
           %%vector-every
           (inline $mutable? v)
           (inline void)
           (inline string-copy! to::bstring at::long from::bstring
           #!optional (start::long 0) (end::long (string-length from)))
           (inline vector-concatenate::vector vecs::pair-nil)
           (inline exact-integer? v)
           (srfi231-error msg . objs)
           (inline <=fx3 a b c)
           (inline <fx3 a b c)
           (inline =fx3 a b c)))


;;; Our naming convention prefixes %% to the names of internal procedures,
(define (c64vector-copy! to at from start end)
   (f32vector-copy! to (*fx 2 at) from (*fx 2 start) (*fx 2 end)))
(define (c128vector-copy! to at from start end)
   (f64vector-copy! to (*fx 2 at) from (*fx 2 start) (*fx 2 end)))


;;; We do not need a multi-argument every.

(define (%%every pred list)
  (let loop ((list list))
    (or (null? list)
        (and (pred (car list))
             (loop (cdr list))))))

;;; the following is used in srfi231-error checks.

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
;;; bigloo utilities

;; at this time all vector and homogenous vectors in Bigloo are mutable
(define-inline ($mutable? v)
   #t)

(define-inline (void)
   #unspecified)

(define-inline (string-copy! to::bstring at::long from::bstring
           #!optional (start::long 0) (end::long (string-length from)))
   (blit-string! to at from start end))

(define-inline (vector-concatenate::vector vecs::pair-nil)
   (apply vector-append vecs))

(define-inline (exact-integer? v)
   (and (exact? v)
        (integer? v)))

(define (srfi231-error msg . objs)
   (error "srfi231" msg (if (and (pair? objs) (null? (cdr objs)))
                            (car objs)
                            objs)))

(define-inline (<=fx3 a b c)
   (and (<=fx a b)
        (<=fx b c)))

(define-inline (<fx3 a b c)
   (and (<fx a b)
        (<fx b c)))

(define-inline (=fx3 a b c)
   (and (=fx a b)
        (=fx b c)))

