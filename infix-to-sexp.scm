#|
Infix->S-expression v0.0.4

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

(require (for-syntax racket/base))
(provide infix:)

(define-syntax-rule (<> a b) (not (= a b)))

(define-for-syntax infix-operators
  (make-hash
   '((not        9    unary     right-left)
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

(define-for-syntax (infix-get-prioritet op)
  (and (hash-ref infix-operators op #f)
       (car (hash-ref infix-operators op))))

(define-for-syntax (infix-get-class op)
  (and (hash-ref infix-operators op #f)
       (cadr (hash-ref infix-operators op))))

(define-for-syntax (infix-get-associativity  op)
  (and (hash-ref infix-operators op #f)
       (caddr (hash-ref infix-operators op))))

(define-syntax (infix: so)
  (define (infix-unary-rl expr)
    (call/cc
     (lambda (return)
       (let ((res '()) (tail '()))
         (set! res
               (let loop ((head expr))
                 (if (and (eqv? (infix-get-class (car head)) 'unary)
                          (eqv? (infix-get-associativity (car head)) 'right-left)
                          (not (null? (cdr head))))
                     (cond
                       ((and (eqv? (infix-get-class (cadr head)) 'unary)
                             (eqv? (infix-get-associativity (cadr head)) 'right-left))
                        (when (cdr head)
                          (cons (car head) (list (loop (cdr head))))))
                       ((and (not (infix-operator? (cadr head)))
                             (not (null? (cadr head))))
                        (cond
                          ((null? (cddr head))
                           (set! tail (cddr head))
                           (list (car head) (cadr head)))
                          ((eqv? (infix-get-class (caddr head)) 'binary)
                           (if (>= (infix-get-prioritet (car head))
                                   (infix-get-prioritet (caddr head)))
                               (begin (set! tail (cddr head))
                                      (list (car head) (cadr head)))
                               (list (car head) (infix-format (cdr head))))) ; error?)))
                          (else
                           (return expr))))
                       (else
                        (return expr)))
                     (return expr))))
         (if (null? tail)
             (list res)
             (cons res tail))))))
  
  (define (infix-unary-rl-ops  expr)
    (let loop ((head expr))
      (cond
        ((null? head) '())
        ((and (eqv? (infix-get-class (car head)) 'unary)
              (eqv? (infix-get-associativity (car head)) 'right-left)
              (not (null? (cdr head))))
         (let ((v (infix-unary-rl head)))
           (if (list? (car v))
               (cons (car v) (loop (cdr v)))
               v)))
        (else
         (cons (car head) (loop (cdr head)))))))
  
  (define (infix-parentheses expr)
    (let loop ((head expr))
      (cond
        ((null? head) '())
        ((and (list? (car head))
              (not (null? (car head))))
         (cons (infix-format (car head)) (loop (cdr head))))
        (else
         (cons (car head) (loop (cdr head)))))))
  
  (define (infix-format expr)
    (if (or (null? expr)
            (not (list? expr)))
        expr
        (call/cc
         (lambda (return)
           (let ((res1 (infix-parentheses expr)))
             (let loop ((head (infix-unary-rl-ops res1)))
               (cond
                 ((null? head)
                  '())
                 ((null? (cdr head))
                  head)
                 ((>= (length head) 4)
                  (cond
                    ((and (not (infix-operator? (car head))) ; 1
                          (not (infix-operator? (caddr head)))) ; 3
                     (if (and (eqv? (infix-get-class (cadr head)) 'binary) ; 2
                              (eqv? (infix-get-class (cadddr head)) 'binary)) ; 4
                         (if (>= (infix-get-prioritet (cadr head)) (infix-get-prioritet (cadddr head)))
                             (loop
                              (cons (list (cadr head) (car head) (caddr head)) (cdddr head)))
                             (list (cadr head) (car head) (loop (cddr head))))
                         (return res1)))
                    (else
                     (return res1))))
                 ((= (length head) 3)
                  (if (and (not (infix-operator? (car head))) ; 1
                           (eqv? (infix-get-class (cadr head)) 'binary) ; 2
                           (not (infix-operator? (caddr head))) ; 3
                           (not (null? (caddr head)))) ; 3
                      (list (cadr head) (car head) (caddr head))
                      (return res1)))
                 (else
                  (return res1)))))))))
  (syntax-case so (#%test)
    [(_ #%test . rest)
     #`'#,(infix-format (syntax->datum #'rest))]
    [(_ . rest)
     (datum->syntax so (infix-format (syntax->datum #'rest)))]))

;>(infix: 6 + (4 * 5 + 8) * 7 + 23)
;225
;> (infix: #%test (- 1) * (- 3) + 6 or 4 - (3 * 6))
;(or (+ (* (- 1) (- 3)) 6) (- 4 (* 3 6)))
;> (infix: #%test 1 or 4 - 3 * 6)
;(or 1 (- 4 (* 3 6)))
;> (infix: #%test 2 and 1 * (9 + (5 + 6 - 9) + 5 + 6) or 4 - 3 * 6)
;(and 2 (or (* 1 (+ (+ (+ 9 (- (+ 5 6) 9)) 5) 6)) (- 4 (* 3 6))))
;> (infix: #%test 2 and 1 * (+ 9 (5 + 6 - 9) 5 6) or 4 - 3 * 6)
;(and 2 (or (* 1 (+ 9 (- (+ 5 6) 9) 5 6)) (- 4 (* 3 6))))
;> (infix: #%test
;        define (fact n)
;          (if (n = 0) 1
;              (n * (fact (n - 1)))))
;(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
