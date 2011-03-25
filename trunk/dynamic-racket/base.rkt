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

#lang racket/base

(require (for-syntax racket/base))

(provide (except-out (all-from-out racket/base)
                     #%module-begin
                     eval)
         (for-syntax (all-from-out racket/base))
         (struct-out first-class-macros)
         for-provide
         define-for-provide
         dynamic-eval-handler
         (rename-out [dynamic-context #%module-begin]
                     [dynamic-trans dynamic-transformer]
                     [eval racket-eval]
                     [dynamic-eval eval]
                     [dynamic-eval-0 eval-0]
                     [dynamic-eval-1 eval-1]))

(struct first-class-macros (closure))

(define-syntax-rule (atom? s-exp)
  (not (pair? s-exp)))

(define-syntax-rule (and/exc s-exp ...)
  (with-handlers ([void (lambda (e) #f)])
    (and s-exp ...)))

(define-syntax-rule (for-provide s-exp ...)
  (begin s-exp ...))

(define-syntax (define-for-provide stx)
  (syntax-case stx ()
    [(_ (id . formals) body ...)
     #'(define (id . formals) body ...)]
    [(_ id s-exp)
     #'(define id s-exp)]
    [(_ id)
     #'(define id id)]))

(define-for-syntax ignore-list
  '(provide for-provide define-for-provide))

(define-for-syntax (ignore? s-exp)
  (and (pair? s-exp)
       (list? s-exp)
       (memq (car s-exp) ignore-list)))

(define-syntax (dynamic-context stx)
  (syntax-case stx ()
    [(_ s-exp ...)
     (with-syntax ([exprs
                    (datum->syntax
                     stx
                     (map (lambda (e)
                            (if (ignore? e)
                                e
                                `(eval ',e)))
                          (syntax->datum #'(s-exp ...))))]
                   [anchor (datum->syntax stx (gensym))])
       #'(#%module-begin
          (define-namespace-anchor anchor)
          (current-namespace (namespace-anchor->namespace anchor))
          (current-eval dynamic-eval-handler)
          . exprs))]))

(define dynamic-eval-0
  (let ([curr-eval (current-eval)])
    (lambda (s-exp)
      (let loop ([pre-exp (curr-eval (dynamic-trans s-exp))])
        (let ([s-exp (curr-eval (dynamic-trans pre-exp))])
          (if (equal? pre-exp s-exp)
              pre-exp
              (loop s-exp)))))))

(define (dynamic-eval-1 s-exp
                        [namespace (current-namespace)]
                        #:local-macrosymbols [local-macrosymbols null]
                        #:local-macroses [local-macroses null])
  (let loop ([pre-exp (eval (dynamic-trans s-exp
                                           #:local-macrosymbols local-macrosymbols
                                           #:local-macroses local-macroses)
                            namespace)])
    (let ([s-exp (eval (dynamic-trans pre-exp
                                      #:local-macrosymbols local-macrosymbols
                                      #:local-macroses local-macroses)
                       namespace)])
      (if (equal? pre-exp s-exp)
          pre-exp
          (loop s-exp)))))

(define (dynamic-eval s-exp
                      [namespace (current-namespace)]
                      #:local-macrosymbols [local-macrosymbols null]
                      #:local-macroses [local-macroses null])
  (eval (dynamic-eval-1 s-exp
                        namespace
                        #:local-macrosymbols local-macrosymbols
                        #:local-macroses local-macroses)
        namespace))

(define dynamic-eval-handler
  (let ([curr-eval (current-eval)])
    (lambda (s-exp)
      (let ([form s-exp])
        (when (syntax? s-exp)
          (let ([s-exp (syntax->datum s-exp)])
            (when (and (pair? s-exp)
                       (list? s-exp)
                       (memq (car s-exp)
                             '(#%top-interaction)))
              (set! form (datum->syntax
                          form
                          (cons (car s-exp)
                                (dynamic-eval-0 (cdr s-exp)))
                          form form form)))))
        (curr-eval form)))))

(define-syntax-rule (quasi-exp? s-exp)
  (and (pair? s-exp)
       (list? s-exp)
       (memq (car s-exp)
             '(quasiquote quasisyntax))))

(define-syntax-rule (const-exp? s-exp)
  (or (atom? s-exp)
      (not (list? s-exp))
      (memq (car s-exp)
            '(quote unquote unquote-splicing
                    syntax unsyntax unsyntax-splicing))))

(define-syntax-rule (un-quasi-exp? s-exp)
  (and (pair? s-exp)
       (list? s-exp)
       (memq (car s-exp)
             '(unquote unquote-splicing
                       unsyntax unsyntax-splicing))))



(define (dynamic-trans s-exp
                       #:local-macrosymbols [local-macrosymbols null]
                       #:local-macroses [local-macroses null])
  (letrec ([quasi-trans
            (lambda(s-exp)
              (letrec ([qlist-trans
                        (lambda (s-exp)
                          (if (or (atom? s-exp)
                                  (un-quasi-exp? s-exp))
                              (qtrans s-exp)
                              `(cons ,(qtrans (car s-exp))
                                     ,(qlist-trans (cdr s-exp)))))]
                       [qtrans
                        (lambda (s-exp)
                          (cond
                            [(or (atom? s-exp)
                                 (not (list? s-exp))
                                 (memq (car s-exp)
                                       '(quote syntax quasiquote quasisyntax)))
                             `',s-exp]
                            [(un-quasi-exp? s-exp)
                             `(list ',(car s-exp) ,(dynamic-trans (cadr s-exp)
                                                                  #:local-macrosymbols local-macrosymbols
                                                                  #:local-macroses local-macroses))]
                            [else
                             (qlist-trans s-exp)]))])
                (if (quasi-exp? s-exp)
                    `(list ',(car s-exp) ,(qlist-trans (cadr s-exp)))
                    s-exp)))])
    (cond
      [(and (symbol? s-exp)
            (assq s-exp local-macrosymbols))
       `',(cdr (assq s-exp local-macrosymbols))]
      [(const-exp? s-exp)
       `',s-exp]
      [(quasi-exp? s-exp)
       (quasi-trans s-exp)]
      [(and (symbol? (car s-exp))
            (assq (car s-exp) local-macroses))
       `((first-class-macros-closure ,(cdr (assq (car s-exp) local-macroses)))
         ,@(map (lambda(s-exp)
                  `',s-exp)
                (cdr s-exp)))]
      [(and/exc (first-class-macros? (eval (car s-exp))))
       `((first-class-macros-closure ,(car s-exp))
         ,@(map (lambda(s-exp)
                  `',s-exp)
                (cdr s-exp)))]
      [else
       (let ([local-macrosymbols
              (with-handlers ([(lambda(e)
                                 (and (exn:fail:syntax? e)
                                      (not (memq (car s-exp)
                                                 '(begin)))))
                               (lambda (e) null)]
                              [void (lambda (e) local-macrosymbols)])
                (eval (car s-exp))
                local-macrosymbols)])
         (cons 'list (map (lambda (s-exp)
                            (dynamic-trans s-exp
                                           #:local-macrosymbols local-macrosymbols
                                           #:local-macroses local-macroses))
                          s-exp)))])))
