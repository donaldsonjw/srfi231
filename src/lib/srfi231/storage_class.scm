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

(module srfi231-storage-class
   (import srfi231-utils)
   (export
      ;;;
      ;;; A storage-class contains functions and objects to manipulate the
      ;;; backing store of a specialized-array.
      ;;;
      ;;; getter:     (lambda (body i) ...)   returns the value of body at index i
      ;;; setter:     (lambda (body i v) ...) sets the value of body at index i to v
      ;;; checker:    (lambda (val) ...)      checks that val is an appropriate value for storing in (maker n)
      ;;; maker:      (lambda (n val) ...)    makes a body of length n with value val
      ;;; length:     (lambda (body) ...)     returns the number of objects in body
      ;;; default:    object                  is the default value with which to fill body
      ;;; data?:      (lambda (data) ...)     returns #t iff data can be converted to a body
      ;;; data->body: (lambda (data) ...)     converts data to a body, raising an exception if needed
      ;;;
      (class <storage-class>
         (name::bstring read-only)
         (getter::procedure read-only)
         (setter::procedure read-only)
         (checker::procedure read-only)
         (maker::procedure read-only)
         (copier read-only)
         (length::procedure read-only)
         (default read-only)
         (data?::procedure read-only)
         (data->body::procedure read-only))

          (make-storage-class name getter setter checker maker copier length default
           data? data->body)
      (inline storage-class? obj)
      (inline storage-class-getter sc::<storage-class>)
      (inline storage-class-setter sc::<storage-class>)
      (inline storage-class-checker sc::<storage-class>)
      (inline storage-class-maker sc::<storage-class>)
      (inline storage-class-copier sc::<storage-class>)
      (inline storage-class-length sc::<storage-class>)
      (inline storage-class-default sc::<storage-class>)
      (inline storage-class-data? sc::<storage-class>)
      (inline storage-class-data->body sc::<storage-class>)
      generic-storage-class
      char-storage-class
      s8-storage-class
      s16-storage-class
      s32-storage-class
      s64-storage-class
      u1-storage-class
      u8-storage-class
      u16-storage-class
      u32-storage-class
      u64-storage-class
      f8-storage-class
      f16-storage-class
      f32-storage-class
      f64-storage-class
  
  ))


(define (make-storage-class name getter setter checker maker copier length default
           data? data->body)
   (instantiate::<storage-class> (name name) (getter getter) (setter setter)
                                 (checker checker) (maker maker)
                                 (copier copier) (length length)
                                 (default default) (data? data?)
                                 (data->body data->body)))

(define-inline (storage-class-getter sc::<storage-class>)
   (-> sc getter))

(define-inline (storage-class-setter sc::<storage-class>)
   (-> sc setter))

(define-inline (storage-class-checker sc::<storage-class>)
   (-> sc checker))

(define-inline (storage-class-maker sc::<storage-class>)
   (-> sc maker))

(define-inline (storage-class-copier sc::<storage-class>)
   (-> sc copier))

(define-inline (storage-class-length sc::<storage-class>)
   (-> sc length))

(define-inline (storage-class-default sc::<storage-class>)
   (-> sc default))

(define-inline (storage-class-data? sc::<storage-class>)
   (-> sc data?))

(define-inline (storage-class-data->body sc::<storage-class>)
   (-> sc data->body))

(define-inline (storage-class? obj)
   (isa? obj <storage-class>))


;;; We define specialized storage-classes for:
;;;
;;; 32- and 64-bit floating-point numbers,
;;; complex numbers with real and imaginary parts of 32- and 64-bit floating-point numbers respectively
;;; 8-, 16-, 32-, and 64-bit signed integers,
;;; 8-, 16-, 32-, and 64-bit unsigned integers, and
;;; 1-bit unsigned integers
;;;
;;; as well as generic objects.

(define-macro (make-standard-storage-classes)

  (define (symbol-concatenate . symbols)
    (string->symbol (apply string-append (map (lambda (s)
                                                (if (string? s)
                                                    s
                                                    (symbol->string s)))
                                              symbols))))

  `(begin
     ,@(map (lambda (name prefix default checker)
              (let ((name   (symbol-concatenate name '-storage-class))
                    (ref    (symbol-concatenate prefix '-ref))
                    (set!   (symbol-concatenate prefix '-set!))
                    (make   (symbol-concatenate 'make- prefix))
                    (copy!  (symbol-concatenate prefix '-copy!))
                    (length (symbol-concatenate prefix '-length))
                    (?      (symbol-concatenate prefix '?)))
                `(define ,name
                   (instantiate::<storage-class>
                      (name ,(symbol->string name))
                      (getter
                         (lambda (v i)
                            (,ref v i)))
                      (setter
                         (lambda (v i val)
                            (,set! v i val)))
                      (checker
                       ,checker           ;; already expanded
                       )
                      (maker
                       (lambda (n val)
                          (,make n val)))
                      (copier
                       ,copy!             ;; complex call to memcopy, so don't expand
                       )
                      (length
                       (lambda (v)
                          (,length v)))
                      (default
                         ,default)
                      (data?
                         (lambda (data)
                            (,? data)))
                    (data->body
                       (lambda (data)
                          (if (,? data)
                              data
                              (srfi231-error ,(symbol->string
                                         (symbol-concatenate
                                            "Expecting a "
                                            prefix
                                            " passed to "
                                            "(storage-class-data->body "
                                            name
                                            "): "))
                                 data))))))))
            '(generic s8       u8       s16       u16       s32       u32       s64       u64       f32       f64       char)
            '(vector  s8vector u8vector s16vector u16vector s32vector u32vector s64vector u64vector f32vector f64vector string)
            '(#f      #s8:0    #u8:0    #s16:0    #u16:0    #s32:0    #u32:0    #s64:0    #u64:0    0.0       0.0       #\0)
            `((lambda (x) #t)                        ; generic
              int8?                                  ;s8
              uint8?                                 ;u8
              int16?                                 ;s16
              uint16?                                ;u16
              int32?                                 ;s32
              uint32?                                ;u32
              int64?                                 ;s64
              uint64?                                ;u64
              flonum?                                ; f32
              flonum?                                ; f64
              char?                                  ; char
              ))))

(make-standard-storage-classes)

;;; This sample implementation does not implement the following.

(define f16-storage-class #f)
(define f8-storage-class #f)

;;; for bit-arrays, body is a vector, the first element of which is the actual number of elements,
;;; the second element of which is a u16vector that contains the bit string

(define u1-storage-class
  (make-storage-class
    "u1-storage-class" 
   ;; getter
   (lambda (v i)
     (let ((index (bit-rsh i 4))
           (shift (bit-and i 15))
           (bodyv (vector-ref v  1)))
       (bit-and
        (bit-rsh
           (uint16->fixnum (u16vector-ref bodyv index))
         shift)
        1)))
   ;; setter
   (lambda (v i val)
     (let ((index (bit-rsh i 4))
           (shift (bit-and i 15))
           (bodyv (vector-ref v  1)))
       (u16vector-set! bodyv index (bit-or (bit-lsh val shift)
                                      (bit-and (uint16->fixnum (u16vector-ref bodyv index))
                                                 (bit-not  (bit-lsh 1 shift)))))))
   ;; checker
   (lambda (val)
     (and (fixnum? val)
          (eq? 0 (bit-and -2 val))))
   ;; maker
   (lambda (size initializer)
     (let ((u16-size (bit-rsh (+ size 15) 4)))
       (vector size (make-u16vector u16-size (if (eqv? 0 initializer) 0 65535)))))
   ;; no copier (for now)
   #f
   ;; length
   (lambda (v)
     (vector-ref v 0))
   ;; default
   0
   ;; data?
   (lambda (data)
     (u16vector? data))
   ;; data->body
   (lambda (data)
     (if (not (u16vector? data))
         (srfi231-error "Expecting a u16vector passed to (storage-class-data->body u1-storage-class): " data)
         (vector (*fx 16 (u16vector-length data))
                 data)))))


;;; Bigloo does not currently support complex numbers
; (define-macro (make-complex-storage-classes)
;   (define (symbol-concatenate . symbols)
;     (string->symbol (apply string-append (map (lambda (s)
;                                                 (if (string? s)
;                                                     s
;                                                     (symbol->string s)))
;                                               symbols))))
;   (define construct
;     (lambda (size)
;       (let ((prefix (string-append "c" (number->string (*fx 2 size))))
;             (floating-point-prefix (string-append "f" (number->string size))))
;         `(define ,(symbol-concatenate prefix '-storage-class)
;            (make-storage-class
;             ;; getter
;             (lambda (body i)
;               (make-rectangular (,(symbol-concatenate floating-point-prefix 'vector-ref) body (*fx 2 i))
;                                 (,(symbol-concatenate floating-point-prefix 'vector-ref) body (+fx (*fx 2 i) 1))))
;             ;; setter
;             (lambda (body i obj)
;               (,(symbol-concatenate floating-point-prefix 'vector-set!) body (*fx 2 i)         (real-part obj))
;               (,(symbol-concatenate floating-point-prefix 'vector-set!) body (+fx (*fx 2 i) 1) (imag-part obj)))
;             ;; checker
;             (lambda (obj)
;               (and (complex? obj)
;                    (inexact? (real-part obj))
;                    (inexact? (imag-part obj))))
;             ;; maker
;             (lambda (n val)
;               (let ((l (*fx 2 n))
;                     (re (real-part val))
;                     (im (imag-part val)))
;                 (let ((result (,(symbol-concatenate 'make-
;                                                     floating-point-prefix
;                                                     'vector)
;                                l)))
;                   (do ((i 0 (+ i 2)))
;                       ((= i l) result)
;                     (,(symbol-concatenate floating-point-prefix 'vector-set!) result i re)
;                     (,(symbol-concatenate floating-point-prefix 'vector-set!) result (+fx i 1) im)))))
;             ;; copier
;             ,(symbol-concatenate prefix 'vector-copy!)
;             ;; length
;             (lambda (body)
;               (fxquotient (,(symbol-concatenate floating-point-prefix 'vector-length) body) 2))
;             ;; default
;             0.+0.i
;             ;; data?
;             (lambda (data)
;               (and (,(symbol-concatenate floating-point-prefix 'vector?) data)
;                    (fxeven? (,(symbol-concatenate floating-point-prefix 'vector-length) data))))
;             ;; data->body
;             (lambda (data)
;               (if (and (,(symbol-concatenate floating-point-prefix 'vector?) data)
;                        (fxeven? (,(symbol-concatenate floating-point-prefix 'vector-length) data)))
;                   data
;                   (srfi231-error ,(symbol->string
;                            (symbol-concatenate
;                             "Expecting a "
;                             floating-point-prefix 'vector
;                             " with an even number of elements passed to "
;                             "(storage-class-data->body "
;                             prefix '-storage-class
;                             "): "))
;                          data))))))))
;   (let ((result
;          `(begin
;             ,@(map construct
;                    '(32 64)))))
;     result))

; (make-complex-storage-classes)

;;;
;;; Conceptually, an indexer is itself a 1-1 array that maps one interval to another; thus, it is
;;; an example of an array that can return multiple values.
;;;
;;; Rather than trying to formalize this idea, and trying to get it to work with array-map,
;;; array-fold, ..., we'll just manipulate the getter functions of these conceptual arrays.
;;;
;;; Indexers are 1-1 affine maps from one interval to another.
;;;
;;; The indexer field of a specialized-array obj is a 1-1 mapping from
;;;
;;; (array-domain obj)
;;;
;;; to [0, top), where top is
;;;
;;; ((storage-class-length (array-storage-class obj)) (array-body obj))
;;;


;;;
;;; The default getter and the setter of a specialized-array a are given by
;;;
;;; (lambda (i_0 ... i_n-1)
;;;   ((storage-class-getter (array-storage-class a))
;;;    (array-body a)
;;;    ((array-indexer a) i_0 ... i_n-1)))
;;;
;;; (lambda (v i_0 ... i_n-1)
;;;   ((storage-class-setter (array-storage-class a))
;;;    (array-body a)
;;;    ((array-indexer a) i_0 ... i_n-1)
;;;    v))
;;;
;;; The default initializer-value is
;;;
;;; (storage-class-default (array-storage-class a))
;;;
;;; The default body is
;;;
;;; ((storage-class-maker (array-storage-class a))
;;;  (interval-volume domain)
;;;  initializer-value)
;;;
;;; The default indexer is the mapping of
;;; the domain to the natural numbers in lexicographical order.
;;;


