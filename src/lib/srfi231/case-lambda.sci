#|
SRFI 231: Intervals and Generalized Arrays

Copyright 2022, Joseph Donaldson
Bigloo Adaption

Bigloo implementation of case-lambda

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


;; define case-lambda in terms of Bigloo's match-case 
(define-expander case-lambda
   (lambda (x e)
      (define (improper-map proc lst)
         (let loop ((lst lst)
                    (res '()))
            (cond ((pair? lst)
                   (loop (cdr lst)
                      (cons (proc (car lst)) res)))
                  ((symbol? lst)
                   (let ((t (reverse! res)))
                      (set-cdr! (last-pair t) (proc lst))
                      t))
                  ((null? lst)
                   (reverse! res))
                  (else (error "case-lambda" "illegal case-lambda expression" x))
                  )))
      (define (process-clause clause)
         (let ((formals (car clause))
               (body (cdr clause)))
            (cons (improper-map (lambda (f) (symbol-append '? f)) formals)
               body)))
       (match-case x                     ;formals body
          ((case-lambda (((? symbol?) ... ???-) ?- ...) ...)
           (let ((args (gensym 'args)))
              (e `(lambda ,args
                     (match-case ,args
                        ,@(append (map process-clause (cdr x))
                           `((else (error "case-lambda"
                                      "no matching clause for args:" ,args))))
                        )) e)))
          (else
           (error "case-lambda" "illegal case-lambda expression" x)))))