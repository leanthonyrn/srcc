#| 25.03.2011 14:16
Summary:
This file is part of dynamic-racket.

License:
Copyright (c) 2011  Mikhail Mosienko  <netluxe@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#

#lang dynamic-racket/base

(provide defmacro
         expand/macro
         expand/macro/trace
         macrolet
         symbol-macrolet
         macro-apply
         defun
         incf
         decf)

(define-for-provide defmacro
  (first-class-macros
   (lambda (name formals expr . body)
     (eval `(define ,name #f))
     `(set! ,name
            (first-class-macros
             (lambda ,formals
               (call/cc
                (lambda (return)
                  ,expr . ,body))))))))

(defmacro macrolet (mcrs . body)
  (let ([form (map (lambda(mcr)
                     (cons (car mcr)
                           `(first-class-macros
                             (lambda . ,(cdr mcr)))))
                   mcrs)])
    (eval-1 `(begin . ,body)
            #:local-macroses form)))

(defmacro symbol-macrolet (mcrsymbs . body)
  (let ([form (map (lambda(mcr)
                     (cons (car mcr) (cadr mcr)))
                   mcrsymbs)])
    (racket-eval (dynamic-transformer `(begin . ,body)
                                      #:local-macrosymbols form))))

(defmacro defun (name formals expr . body)
  (eval `(define ,name #f))
  `(set! ,name (lambda ,formals
                 (call/cc
                  (lambda (return)
                    ,expr . ,body)))))

(defmacro macro-apply (f . args)
  (eval-0 (cons f args)))

(defun expand/macro (s-exp)
  (eval-0 s-exp))

(defun expand/macro/trace (s-exp)
  (let loop ([pre-exp (racket-eval (dynamic-transformer s-exp))]
             [step 1])
    (printf "Macro transformation ~a\n~a\n" step pre-exp)
    (let ([s-exp (racket-eval (dynamic-transformer pre-exp))])
      (if (equal? pre-exp s-exp)
          pre-exp
          (loop s-exp (add1 step))))))

(defmacro incf (id)
  `(begin (set! ,id (add1 ,id)) ,id))

(defmacro decf (id)
  `(begin (set! ,id (sub1 ,id)) ,id))

(define-for-provide expand/macro)
(define-for-provide expand/macro/trace)
(define-for-provide macrolet)
(define-for-provide symbol-macrolet)
(define-for-provide defun)
(define-for-provide macro-apply)
(define-for-provide incf)
(define-for-provide decf)