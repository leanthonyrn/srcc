#|
Infix->S-expression v0.9

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

(require (for-syntax racket/base
                     racket/match))
(provide infix: <> (for-syntax infix-operators))

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

(define-syntax (infix: so)
  
  (struct %id [symbol syntax])
  (struct %lisp [source])
  
  (define special-symbols '(? :))
  
  (define (token-list slst)
    (let ([val (syntax-e slst)])
      (cond
        [(pair? val)
         (if (list? val)
             (if (infix-const? (car val))
                 (%lisp slst)
                 (map token-list val))
             (raise-syntax-error 'infix "Syntax error" slst))]
        [(and (symbol? val)
              (not (memq val special-symbols)))
         (%id val slst)]
        [else
         val])))
  
  (define (infix-operator? op)
    (and (%id? op)
         (hash-ref infix-operators (%id-symbol op) #f)))
  
  (define (not-infix-operator? op)
    (not (infix-operator? op)))
  
  (define (infix-get-prioritet op)
    (and (%id? op)
         (let ([op (%id-symbol op)])
           (and (hash-ref infix-operators op #f)
                (car (hash-ref infix-operators op))))))
  
  (define (infix-get-class op)
    (and (%id? op)
         (let ([op (%id-symbol op)])
           (and (hash-ref infix-operators op #f)
                (cadr (hash-ref infix-operators op))))))
  
  (define (infix-get-associativity  op)
    (and (%id? op)
         (let ([op (%id-symbol op)])
           (and (hash-ref infix-operators op #f)
                (caddr (hash-ref infix-operators op))))))
  
  (define (infix-oper&bin&lr? op)
    (and (%id? op)
         (let ([op (%id-symbol op)])
           (and (hash-ref infix-operators op #f)
                (memq (cadr (hash-ref infix-operators op)) '(binary bin-spec))
                (eq? (caddr (hash-ref infix-operators op)) 'left-right)))))
  
  (define (infix-oper&unr&rl? op)
    (and (%id? op)
         (let ([op (%id-symbol op)])
           (and (hash-ref infix-operators op #f)
                (or (and (eq? (cadr (hash-ref infix-operators op)) 'unary)
                         (eq? (caddr (hash-ref infix-operators op)) 'right-left))
                    (eq? (cadr (hash-ref infix-operators op)) 'bin-spec))))))
  
  (define (infix-const? s-exp)
    (and (identifier? s-exp)
         (memq (syntax-e s-exp)
               '(quote unquote unquote-splicing
                       syntax unsyntax unsyntax-splicing))))
  
  (define (unary-trans s-exp [ops null])
    (let utrans ([expr s-exp][ops null])
      (if (not (pair? expr))
          (error s-exp)
          (match expr
            [(list (? infix-oper&unr&rl? op) e rest ...)
             (utrans (cons e rest) (cons op ops))]
            [(list (? token-id? fn) (? list? args) rest ...)
             (cons (%lisp (foldl list (trans-fn fn args)
                                 (map %id-syntax
                                      (sort ops (lambda(a b)
                                                  (> (infix-get-prioritet a)
                                                     (infix-get-prioritet b)))))))
                   rest)]
            [(list (? not-infix-operator? e) rest ...)
             (cons (%lisp (foldl list (trans/i e)
                                 (map %id-syntax
                                      (sort ops (lambda(a b)
                                                  (> (infix-get-prioritet a)
                                                     (infix-get-prioritet b)))))))
                   rest)]
            [_ 
             (raise-syntax-error 'infix "Syntax error" s-exp)]))))
  
  (define (token-id? tkn)
    (and (%id? tkn)
         (not (infix-operator? tkn))))
  
  (define (token-data? tkn)
    (or (token-id? tkn)
        (not (pair? tkn))))
  
  (define (trans-fn name args)
    (cons (%id-syntax name) (map trans/i args)))
  
  (define (trans/i iexp)
    (match iexp
      [(? %lisp? e)
       (%lisp-source e)]
      [(? token-data? v)
       (if (%id? v) (%id-syntax v) v)]
      [(list test r1 ... '? e1 r2 ... ': e2 r3 ...)
       `(if ,(trans/i (cons test r1))
            ,(trans/i (cons e1 r2))
            ,(trans/i (cons e2 r3)))]
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
                         (cons (list (%id-syntax (car ops)) (cadr res) (car res))
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
                               (cons (list (%id-syntax (car opers)) (cadr res) (car res))
                                     (cddr res)))
                         (loop (cdr opers))))))])
         (let next ([exp iexp])
           (match exp
             [(? %lisp? e)
              (genres (%lisp-source e))]
             [(list (? not-infix-operator? e))
              (genres (trans/i e))]
             [(list (? token-id? fn) (? list? args))
              (genres (trans-fn fn args))]
             [(list (? not-infix-operator? e)
                    (? infix-oper&bin&lr? op)
                    rest ...)
              (check op (trans/i e))
              (next (cddr exp))]
             [(list (? token-id? fn) (? list? args)
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
              (raise-syntax-error 'infix "Syntax error" iexp)])))]))
  
  (with-handlers ([exn:fail:syntax? (lambda(e)
                                      (raise-syntax-error '|infix: | 
                                                          "Please check syntax" so))])
    (syntax-case so (#%test)
      [(_ #%test s-exp . rest)
       #`'#,(trans/i (cons (token-list #'s-exp)
                           (token-list #'rest)))]
      [(_ s-exp . rest)
       (datum->syntax so (trans/i (cons (token-list #'s-exp)
                                        (token-list #'rest))))])))

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