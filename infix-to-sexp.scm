#|
Infix->S-expression v0.5

Copyright (c) 2010-2011  Mikhail Mosienko  <netluxe@gmail.com>

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

#lang racket/base

(require (for-syntax racket/base racket/match))
(provide infix:)

(define-syntax-rule (<> a b) (not (= a b)))

(define-for-syntax infix-operators
  (make-hash
   '((not        8    unary     right-left)
     (add1       9    unary     right-left)
     (sub1       9    unary     right-left)
     (*          5    binary    left-right)
     (/          5    binary    left-right)
     (modulo     5    binary    left-right)
     (+          4    binary    left-right)
     (-          4    binary    left-right)
     (>          3    binary    left-right)
     (<          3    binary    left-right)
     (>=         3    binary    left-right)
     (<=         3    binary    left-right)
     (=          2    binary    left-right)
     (<>         2    binary    left-right)
     (or         1    binary    left-right)
     (and        1    binary    left-right))))

(define-for-syntax (infix-operator? op)
  (hash-ref infix-operators op #f))

(define-for-syntax (not-infix-operator? op)
  (not (hash-ref infix-operators op #f)))

(define-for-syntax (infix-get-prioritet op)
  (and (hash-ref infix-operators op #f)
       (car (hash-ref infix-operators op))))

(define-for-syntax (infix-get-class op)
  (and (hash-ref infix-operators op #f)
       (cadr (hash-ref infix-operators op))))

(define-for-syntax (infix-get-associativity  op)
  (and (hash-ref infix-operators op #f)
       (caddr (hash-ref infix-operators op))))

(define-for-syntax (infix-oper&bin&lr? op)
  (and (hash-ref infix-operators op #f)
       (eq? (cadr (hash-ref infix-operators op)) 'binary)
       (eq? (caddr (hash-ref infix-operators op)) 'left-right)))

(define-for-syntax (infix-oper&unr&rl? op)
  (and (hash-ref infix-operators op #f)
       (eq? (cadr (hash-ref infix-operators op)) 'unary)
       (eq? (caddr (hash-ref infix-operators op)) 'right-left)))

(define-syntax (infix: so)
  (define (infix-const? s-exp)
    (memq s-exp '(quote unquote unquote-splicing 
                        syntax unsyntax unsyntax-splicing)))
  
  (define (unary-trans s-exp [ops null])
    (let utrans ([expr s-exp][ops null])
      (if (not (pair? expr))
          (error s-exp)
          (match expr
            [(list (? infix-oper&unr&rl? op) e1 rest ...)
             (utrans (cons e1 rest) (cons op ops))]
            [(list e1 rest ...)
             (cons (foldl list (infix-trans e1)
                          (sort ops (lambda(a b) 
                                      (> (infix-get-prioritet a)
                                         (infix-get-prioritet b)))))
                   rest)]))))
  
  (define (infix-trans s-exp)
    (match s-exp
      [(list (? infix-oper&unr&rl? op) rest ...)
       (let ([exp (unary-trans s-exp)])
         (if (null? (cdr exp))
             (car exp)
             (infix-trans exp)))]
      [(list e (? infix-oper&bin&lr? op1) (? infix-oper&unr&rl? op2) rest ...)
       (infix-trans `(,e ,op1 . ,(unary-trans (cons op2 rest))))]
      [(list e1 (? infix-oper&bin&lr? op1) e2 (? infix-oper&bin&lr? op2) e3 ...)
       (if ((infix-get-prioritet op1). >= .(infix-get-prioritet op2))
           (infix-trans `((,e1 ,op1 ,e2) ,op2 ,@e3))
           (infix-trans `(,e1 ,op1 (,e2 ,op2 ,@e3))))]
      [(list e1 (? infix-oper&bin&lr? op) e2)
       (list op (infix-trans e1) (infix-trans e2))]
      [(list (? infix-const? e1) e2)
       (list e1 e2)]
      [(list e rest ...)
       (cons e (map infix-trans rest))]
      [_
       s-exp]))
  
  (syntax-case so (#%test)
    [(_ #%test . rest)
     #`'#,(infix-trans (syntax->datum #'rest))]
    [(_ . rest)
     (datum->syntax so (infix-trans (syntax->datum #'rest)))]))

;>(infix: 6 + (4 * 5 + 8) * 7 + 23)
;225
;> (infix: #%test (- 1) * (- 3) + 6 or 4 - (3 * 6))
;(or (+ (* (- 1) (- 3)) 6) (- 4 (* 3 6)))
;> (infix: #%test 1 / 12 + 2 * (5 + 6 / 7))
;'(+ (/ 1 12) (* 2 (+ 5 (/ 6 7))))
;> (infix: #%test add1 sub1 not x and 5 + 7)
;'(and (not (add1 (sub1 x))) (+ 5 7))
;> (infix: #%test 2 * sub1 (x + 2 * 6) / 34)
;'(/ (* 2 (sub1 (+ x (* 2 6)))) 34)
;> (infix: #%test 1 or 4 - 3 * 6)
;'(or 1 (- 4 (* 3 6)))
;> (infix: #%test 2 and 1 * (9 + (5 + 6 - 9) + 5 + 6) or 4 - 3 * 6)
;'(and 2 (or (* 1 (+ (+ (+ 9 (- (+ 5 6) 9)) 5) 6)) (- 4 (* 3 6))))
;> (infix: #%test 2 and 1 * (+ 9 (5 + 6 - 9) 5 6) or 4 - 3 * 6)
;'(and 2 (or (* 1 (+ 9 (- (+ 5 6) 9) 5 6)) (- 4 (* 3 6))))
;> (infix: #%test
;        define (fact n)
;          (if (n = 0) 1
;              (n * (fact (n - 1)))))
;'(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
