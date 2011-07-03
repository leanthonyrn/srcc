#|
Infix->S-expression v0.7

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

(require (for-syntax racket/base racket/match) racket/list)
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
            [(list (? token? fn) (? list? args) rest ...)
             (cons `(#%lisp ,(foldl list (trans-fn fn args)
                                    (sort ops (lambda(a b)
                                                (> (infix-get-prioritet a)
                                                   (infix-get-prioritet b))))))
                   rest)]
            [(list e1 rest ...)
             (cons `(#%lisp ,(foldl list (trans/i e1)
                                    (sort ops (lambda(a b)
                                                (> (infix-get-prioritet a)
                                                   (infix-get-prioritet b))))))
                   rest)]))))
  
  (define (token? op)
    (not (or (pair? op)
             (infix-operator? op))))
  
  (define (trans-fn name args)
    (cons name (map trans/i args)))
  
  (define (trans/i iexp)
    (match iexp
      [(? token? id)
       id]
      [(list (? infix-const? e1) e2)
       iexp]
      [(list '#%lisp e)
       e]
      [_
       (let* ([res null]
              [ops null]
              [check
               (lambda (op exp)
                 (set! res (cons exp res))
                 (do ()[(or (null? ops)
                            (< (infix-get-prioritet (car ops))
                               (infix-get-prioritet op)))]
                   (set! res
                         (cons (list (car ops) (cadr res) (car res))
                               (cddr res)))
                   (set! ops (cdr ops)))
                 (set! ops (cons op ops)))]
              [genres
               (lambda (exp)
                 (set! res (cons exp res))
                 (let loop ([opers ops])
                   (if (null? opers)
                       (car res)
                       (begin
                         (set! res
                               (cons (list (car opers) (cadr res) (car res))
                                     (cddr res)))
                         (loop (cdr opers))))))])
         (let next ([exp iexp])
           (match exp
             [(list '#%lisp e)
              (genres e)]
             [(list (? not-infix-operator? e))
              (genres (trans/i e))]
             [(list (? token? fn) (? list? args))
              (genres (trans-fn fn args))]
             [(list test r1 ... '? then r2 ... ': else r3 ...)
              `(if ,(trans/i (cons test r1))
                   ,(trans/i (cons then r2))
                   ,(trans/i (cons else r3)))]
             [(list (? not-infix-operator? e)
                    (? infix-oper&bin&lr? op)
                    rest ...)
              (check op (trans/i e))
              (next (cddr exp))]
             [(list (? token? fn) (? list? args)
                    (? infix-oper&bin&lr? op)
                    rest ...)
              (check op (trans-fn fn args))
              (next (cdddr exp))]
             [(list (? infix-oper&unr&rl? op)
                    rest ...)
              (let ([exp (unary-trans exp)])
                (next (if (null? (cdr exp))
                          (car exp)
                          exp)))]
             [_
              (raise-syntax-error '|infix: | "Please check syntax" iexp)])))]))
  
  (syntax-case so (#%test)
    [(_ #%test . rest)
     #`'#,(trans/i (syntax->datum #'rest))]
    [(_ . rest)
     (datum->syntax so (trans/i (syntax->datum #'rest)))]))

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
;'(or (and 2 (* 1 (+ (+ (+ 9 (- (+ 5 6) 9)) 5) 6))) (- 4 (* 3 6)))
;> (infix: #%test 1 + 2 * - (- 3) > - 4)
;'(> (+ 1 (* 2 (- (- 3)))) (- 4))
;> (infix: #%test - x() + 78 * - - f([5 + 6]) < 8 * 90)
;'(< (+ (- (x)) (* 78 (- (- (f (+ 5 6)))))) (* 8 90))
;> (infix: #%test [ 4 > 5 ? 5 * - - f(): 6 + 8 * 9])
;'(if (> 4 5) (* 5 (- (- (f)))) (+ 6 (* 8 9)))
;> (infix: #%test 23 + [ 4 > 5 ? 5 * - - f() : 6 + 8 * 9] > 7)
;'(> (+ 23 (if (> 4 5) (* 5 (- (- (f)))) (+ 6 (* 8 9)))) 7)