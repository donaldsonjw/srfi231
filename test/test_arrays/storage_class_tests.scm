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
(module storage-class-tests
   (include "test_macros.sch")
   (import bigloo_compat
           test_infra)
   (library srfi231)
   (export (storage-class-tests)))


(define (storage-class-tests)
   (pp "storage-class tests")

   (define storage-class-names
      (list (list   u1-storage-class   'u1-storage-class 'u16vector make-u16vector)
         (list   u8-storage-class   'u8-storage-class  'u8vector  make-u8vector)
         (list  u16-storage-class  'u16-storage-class 'u16vector make-u16vector)
         (list  u32-storage-class  'u32-storage-class 'u32vector make-u32vector)
         (list  u64-storage-class  'u64-storage-class 'u64vector make-u64vector)
         (list   s8-storage-class   's8-storage-class  's8vector  make-s8vector)
         (list  s16-storage-class  's16-storage-class 's16vector make-s16vector)
         (list  s32-storage-class  's32-storage-class 's32vector make-s32vector)
         (list  s64-storage-class  's64-storage-class 's64vector make-s64vector)
         (list  f32-storage-class  'f32-storage-class 'f32vector make-f32vector)
         (list  f64-storage-class  'f64-storage-class 'f64vector make-f64vector)
         (list char-storage-class 'char-storage-class 'string    make-string)
         ;(list  c64-storage-class  'c64-storage-class 'f32vector make-f32vector)
         ;(list c128-storage-class 'c128-storage-class 'f64vector make-f64vector)
         (list generic-storage-class 'generic-storage-class 'vector make-vector)
         ))

   (define uniform-storage-classes
      (list u8-storage-class u16-storage-class u32-storage-class u64-storage-class
         s8-storage-class s16-storage-class s32-storage-class s64-storage-class
         f32-storage-class f64-storage-class
         char-storage-class
         ;c64-storage-class c128-storage-class
         ))


   (for-each (lambda (storage-class)
                (test ((storage-class-data? storage-class)
                       ((storage-class-maker storage-class)
                        8 (storage-class-default storage-class)))
                   #t))
      uniform-storage-classes)

   (test ((storage-class-data? u1-storage-class) (u16vector 0))
      #t)

   (for-each (lambda (class-name-data-maker)
                (let* ((class
                             (car class-name-data-maker))
                       (name
                          (cadr class-name-data-maker))
                       (data
                          (caddr class-name-data-maker))
                       (maker
                          (cadddr class-name-data-maker))
                       (message
                          (string-append "Expecting a "
                             (symbol->string data)
                             " passed to "
                             "(storage-class-data->body "
                             (symbol->string name)
                             "): ")))
                   (test ((storage-class-data->body class) 'a)
                      message)
                   #;(test ((storage-class-data->body class) (maker 0))
                   message)
                   ))
      storage-class-names))
