#|
Infix->S-expression v0.6

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
     (-          4    bin-spec  left-right)
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
       (memq (cadr (hash-ref infix-operators op)) '(binary bin-spec))
       (eq? (caddr (hash-ref infix-operators op)) 'left-right)))

(define-for-syntax (infix-oper&unr&rl? op)
  (and (hash-ref infix-operators op #f)
       (or (and (eq? (cadr (hash-ref infix-operators op)) 'unary)
                (eq? (caddr (hash-ref infix-operators op)) 'right-left))
           (eq? (cadr (hash-ref infix-operators op)) 'bin-spec))))

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
            ;; new 
            [(list (? token? fn) (? list? args) rest ...)
             (cons (foldl list `(#%infix:fn ,fn ,@args)
                          (sort ops (lambda(a b) 
                                      (> (infix-get-prioritet a)
                                         (infix-get-prioritet b)))))
                   rest)]
            
            [(list e1 rest ...)
             (cons (foldl list (infix-trans e1)
                          (sort ops (lambda(a b) 
                                      (> (infix-get-prioritet a)
                                         (infix-get-prioritet b)))))
                   rest)]))))
  
  (define (token? op)
    (not (or (pair? op)
             (infix-operator? op))))
  
  (define (check-ops trans-sexp)
    (match trans-sexp
      [(list op1 e1 (list op2 e2 e3))
       (if (>= (infix-get-prioritet op2)
               (infix-get-prioritet op1))
           trans-sexp
           (list op2 (list op1 e1 e2) e3))]))
  
  (define (infix-trans s-exp)
    (match s-exp
      [(? token? id)
       id]
      [(list (? infix-const? e1) e2)
       s-exp]
      [(list '#%infix:fn fn args ...)
       (cons fn (map infix-trans args))]
      ;; new
      [(list (? token? fn) (? list? args))
       (cons fn (map infix-trans args))]
      [(list (? not-infix-operator? e1) 
             (? infix-oper&bin&lr? op) 
             (? not-infix-operator? e2))
       (list op (infix-trans e1) (infix-trans e2))]
      ;; new
      [(list (? token? fn) (? list? args) 
             (? infix-oper&bin&lr? op) 
             (? not-infix-operator? e2))
       (list op 
             (cons fn (map infix-trans args)) 
             (infix-trans e2))]
      ;; new
      [(list (? not-infix-operator? e1) 
             (? infix-oper&bin&lr? op) 
             (? token? fn) (? list? args))
       (list op 
             (infix-trans e1) 
             (cons fn (map infix-trans args)))]
      [(list (? infix-oper&unr&rl? op) 
             rest ...)
       (let ([exp (unary-trans s-exp)])
         (if (null? (cdr exp))
             (car exp)
             (infix-trans exp)))]
      [(list (? not-infix-operator? e) 
             (? infix-oper&bin&lr? op1) 
             (? infix-oper&unr&rl? op2) rest ...)
       (infix-trans `(,e ,op1 . ,(unary-trans (cons op2 rest))))]
      ;; new
      [(list (? token? fn) (? list? args)
             (? infix-oper&bin&lr? op1) 
             (? infix-oper&unr&rl? op2) rest ...)
       (infix-trans `((#%infix:fn ,fn ,@args) ,op1 . ,(unary-trans (cons op2 rest))))]
      [(list (? not-infix-operator? e1) 
             (? infix-oper&bin&lr? op1) 
             (? not-infix-operator? e2)
             (? infix-oper&bin&lr? op2) e3 ...)
       (if ((infix-get-prioritet op1). >= .(infix-get-prioritet op2))
           (infix-trans `((,e1 ,op1 ,e2) ,op2 ,@e3))
           (check-ops `(,op1 ,(infix-trans e1)
                             ,(infix-trans `(,e2 ,op2 ,@e3)))))]
      ;; new 
      [(list (? token? fn) (? list? args) 
             (? infix-oper&bin&lr? op1) 
             (? not-infix-operator? e2)
             (? infix-oper&bin&lr? op2) e3 ...)
       (if ((infix-get-prioritet op1). >= .(infix-get-prioritet op2))
           (infix-trans `(((#%infix:fn ,fn ,@args) ,op1 ,e2) ,op2 ,@e3))
           (check-ops `(,op1 ,(cons fn (map infix-trans args))
                             ,(infix-trans `(,e2 ,op2 ,@e3)))))]
      ;; new 
      [(list (? not-infix-operator? e1) 
             (? infix-oper&bin&lr? op1) 
             (? token? fn) (? list? args)
             (? infix-oper&bin&lr? op2) e3 ...)
       (if ((infix-get-prioritet op1). >= .(infix-get-prioritet op2))
           (infix-trans `((,e1 ,op1 (#%infix:fn ,fn ,@args)) ,op2 ,@e3))
           (check-ops `(,op1 ,(infix-trans e1)
                             ,(infix-trans `((#%infix:fn ,fn ,@args) ,op2 ,@e3)))))]
      [_ 
       (raise-syntax-error '|infix: | "Please check syntax" s-exp)]))
  
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
;;; Don't use it!
;;;> (infix: #%test
;;;        define (fact n)
;;;          (if (n = 0) 1
;;;              (n * (fact (n - 1)))))
;;;'(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
;> (infix: #%test 1 + 2 * - (- 3) > - 4)
;'(> (+ 1 (* 2 (- (- 3)))) (- 4))
;> (infix: #%test - x() + 78 * - - f([5 + 6]) < 8 * 90)
;'(< (+ (- (x)) (* 78 (- (- (f (+ 5 6)))))) (* 8 90))

