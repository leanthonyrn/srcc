#| 12.08.2010 17:07

SafeScheme to C# converter.

Copyright (c) 2010-2011 Mikhail Mosienko <netluxe@gmail.com>

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

(module SafeScheme racket

  (provide safe->csharp
           safe-file->csharp-file
           safe-files->csharp-files
           add-expr-translater
           remove-expr-translater)

  (struct tSymbol (symb type mod))

  (define (typed-symbol-view tsymb)
    (write (list (tSymbol-symb tsymb) (tSymbol-type tsymb) (tSymbol-mod tsymb))))

  (define (typed-symbol->list tsymb)
    (list (symbol->csharp-string (tSymbol-symb tsymb))
          (cond
            ((null? (tSymbol-type tsymb)) "")
            ((null? (cdr (tSymbol-type tsymb)))
             (symbol->csharp-string (car (tSymbol-type tsymb))))
            (else
             (string-append (symbol->csharp-string (car (tSymbol-type tsymb)))
                            "<"
                            (string-join (map (λ(s) (symbol->csharp-string s))
                                              (cdr (tSymbol-type tsymb)))
                                         ", ")
                            ">")))
          (string-join (map (λ(s) (symbol->csharp-string s))
                            (tSymbol-mod tsymb))
                       " ")))

  (define (typed-symbol symb)
    (if (symbol? symb)
        (let* ([slst (regexp-split #rx":+" (symbol->string symb))]
               [l (map (λ(s) (string->symbol s))
                       (cdr slst))]
               [ml '(static public private protected abstract partial)]
               [m (filter (λ(p) (memq p ml)) l)]
               [t (filter (λ(p) (not (memq p ml))) l)])
          (if (null? (cdr slst))
              #f
              (tSymbol (string->symbol (car slst)) t m)))
        #f))

  (define (string-flatten lst)
    (apply string (flatten lst)))

  (define (number->clist num)
    (string->list (number->string num)))

  (define (symbol->clist symb)
    (string->list (symbol->string symb)))

  (define (symbol->csharp-string symb)
    (case symb
      ((+ - * / < > <= >=) (symbol->string symb))
      ((=) "==")
      ((or) "||")
      ((and) "&&")
      (else
       (string-flatten (map (λ(c)
                              (if (or (char-alphabetic? c)
                                      (char-numeric? c)
                                      (char=? c #\.)
                                      (char=? c #\_))
                                  c
                                  (list #\_ (number->clist (char->integer c)) #\_)))
                            (symbol->clist symb))))))

  (define (throw-exception . exc)
    (case (car exc)
      ((NotImplementedExc) (error "Not implemented!"))
      ((GlobalErrorExc) (apply error "Error: " (cdr exc)))
      ((SyntaxExc) (error "Syntax error"))
      ((UsingSyntaxExc)
       (error "Syntax error - using - " (cadr exc)))
      ((NameSpaceSyntaxExc)
       (error "Syntax error - namespace - " (cadr exc)))
      ((ClassSyntaxExc)
       (error "Syntax error - class - " (cadr exc)))
      ((DefineSyntaxExc)
       (error "Syntax error - define - " (cadr exc)))
      ((ExprSyntaxExc)
       (error "Syntax error - expression - " (cadr exc)))
      ((ClosureSyntaxExc)
       (error "Syntax error - closure - " (cadr exc)))
      ((MethodSyntaxExc)
       (error "Syntax error - method - " (cadr exc)))
      ((ConstructorSyntaxExc)
       (error "Syntax error - constructor - " (cadr exc)))
      ))

  (define (Error! val . args)
    (unless val (apply throw-exception args)))

  (define (Error val . args)
    (when val (apply throw-exception args)))

  (define (make-ret clist)
    (Error! (and (pair? clist) (list? clist)) 'SyntaxExc)
    (let* ([rlist (reverse clist)] [ret (car rlist)])
      (if (and (list? ret) (eq? (car ret) 'return))
          clist
          (reverse (cons (cons 'return (list ret)) (cdr rlist))))))

  (define CALL #t)
  (define RET #t)

  (define (simple-expr-translater expr call? ret? return-result)
    (let ([r (cond
               ((number? expr) (number->string expr))
               ((string? expr) (string-append "\"" expr "\""))
               ((char? expr) (string-append (string #\' expr #\')))
               ((boolean? expr) (string-append (if expr "true" "false")))
               ((symbol? expr)
                (case expr
                  ((tvoid) "typeof(void)")
                  (else (symbol->csharp-string expr))))
               ((null? expr) (throw-exception  'ExprSyntaxExc expr))
               ((list? expr)
                (case (car expr)
                  ((quote) (throw-exception  'NotImplementedExc))
                  ((+ - * / or and = < > <= >=)
                   (let ((len (length expr)))
                     (case len
                       ((1)
                        (case (car expr)
                          ((+) "0")
                          ((*) "1")
                          ((or) "false")
                          ((and) "true")
                          (else (throw-exception  'ExprSyntaxExc expr))))
                       ((2)
                        (case (car expr)
                          ((+ *) (expr-translater (cadr expr) CALL #f))
                          ((-) (string-append "-" (expr-translater (cadr expr) CALL #f)))
                          ((/) (string-append "1/" (expr-translater (cadr expr) CALL #f)))
                          (else (throw-exception  'ExprSyntaxExc expr))))
                       (else
                        (string-append "("
                                       (string-join (map (λ (t)
                                                           (expr-translater t CALL #f))
                                                         (cdr expr))
                                                    (string-append " " (symbol->csharp-string (car expr)) " "))
                                       ")")))))
                  ((not)
                   (Error! (= (length expr) 2) 'ExprSyntaxExc expr)
                   (string-append "!" (expr-translater (cadr expr) CALL #f)))
                  ((set!)
                   (Error! (and (= (length expr) 3) (symbol? (cadr expr))) 'ExprSyntaxExc expr)
                   (string-append (symbol->csharp-string (cadr expr)) " = " (expr-translater (caddr expr) CALL #f)))
                  ((1++)
                   (Error! (and (= (length expr) 2) (symbol? (cadr expr))) 'ExprSyntaxExc expr)
                   (string-append (symbol->csharp-string (cadr expr)) "++ "))
                  ((++1 incf)
                   (Error! (and (= (length expr) 2) (symbol? (cadr expr))) 'ExprSyntaxExc expr)
                   (string-append " ++" (symbol->csharp-string (cadr expr))))
                  ((1--)
                   (Error! (and (= (length expr) 2) (symbol? (cadr expr))) 'ExprSyntaxExc expr)
                   (string-append (symbol->csharp-string (cadr expr)) "-- "))
                  ((--1 decf)
                   (Error! (and (= (length expr) 2) (symbol? (cadr expr))) 'ExprSyntaxExc expr)
                   (string-append " --" (symbol->csharp-string (cadr expr))))
                  ((->)
                   (Error! (>= (length expr) 3) 'ExprSyntaxExc expr)
                   (string-join (map (λ (t)
                                       (expr-translater t #f #f))
                                     (cdr expr))
                                "."))
                  ((make-object)
                   (cond
                     ((= (length expr) 2)
                      (string-append "new " (expr-translater (cadr expr) #f #f) "()"))
                     ((> (length expr) 2)
                      (string-append
                       "new "
                       (expr-translater (cadr expr) #f #f)
                       "("
                       (string-join (map (λ (t)
                                           (expr-translater t #f #f))
                                         (cddr expr))
                                    ", ")
                       ")"))
                     (else (throw-exception  'ExprSyntaxExc expr))))
                  ((type)
                   (Error! (= (length expr) 3) 'ExprSyntaxExc expr)
                   (string-append "(" (expr-translater (cadr expr) #f #f) ")"
                                  (expr-translater (caddr expr) #f #f)))
                  (else #f)))
               (else #f))])
      (if r (return-result (string-append (if ret? "return " "") r)) #f)))

  (define (different-expr-translater expr call? ret? return-result)
    (if (and (pair? expr) (list? expr))
        (return-result
         (case (car expr)
           ((tfn);????????????????????????
            (Error! (>= (length expr) 2) 'ExprSyntaxExc expr)
            (string-append "Func<"
                           (string-join (map (λ (t)
                                               (expr-translater t #f #f))
                                             (cdr expr))
                                        ", ")
                           ">"))
           ((t_)
            (Error! (> (length expr) 2) 'ExprSyntaxExc expr)
            (string-append (if (symbol? (cadr expr))
                               (symbol->csharp-string (cadr expr))
                               (throw-exception  'ExprSyntaxExc expr))
                           "<"
                           (string-join (map (λ (t)
                                               (expr-translater t #f #f))
                                             (cddr expr))
                                        ", ")
                           ">"))
           ((define)
            (Error (or call? ret?) 'ExprSyntaxExc expr)
            (Error! (and (= (length expr) 3) (symbol? (cadr expr)) (typed-symbol (cadr expr)))
                    'DefineSyntaxExc expr)
            (let ([tl (typed-symbol->list (typed-symbol (cadr expr)))])
              (string-append (caddr tl)
                             " "
                             (cadr tl)
                             " "
                             (car tl)
                             " = "
                             (if (and (pair? (caddr expr)) (list? (caddr expr))
                                      (typed-symbol (caaddr expr))
                                      (memq (tSymbol-symb (typed-symbol (caaddr expr))) '(fn λ)))
                                 (string-append "null;\n" (car tl) " = ")
                                 "")
                             (expr-translater (caddr expr) CALL #f))))
           ((return)
            (Error! (= (length expr) 2) 'ExprSyntaxExc expr)
            (expr-translater (cadr expr) #f RET))
           ((code-block then else)
            (Error (and (null? (cdr expr)) call?) 'ExprSyntaxExc expr)
            (let ([e (if ret? (make-ret (cdr expr)) (cdr expr))])
              (string-append "{\n"
                             (string-append* (map (λ (exp)
                                                    (string-append (expr-translater exp #f #f) ";\n"))
                                                  e))
                             "}\n")))
           ((if)
            (Error! (= (length expr) 4) 'ExprSyntaxExc expr)
            (string-append (if call?
                               (string-append "("
                                              (expr-translater (cadr expr) CALL #f)
                                              " ? ")
                               (string-append "if ("
                                              (expr-translater (cadr expr) CALL #f)
                                              ") {"))
                           (expr-translater (caddr expr) call? ret?)
                           (if call? " : " ";}\nelse {\n")
                           (expr-translater (cadddr expr) call? ret?)
                           (if call? ")" ";}")))
           ((when)
            (Error (and (< (length expr) 3) call?) 'ExprSyntaxExc expr)
            (string-append "if ("
                           (expr-translater (cadr expr) CALL #f)
                           ") "
                           (expr-translater (cons 'code-block (cddr expr)) #f ret?)))
           ((unless)
            (Error (and (< (length expr) 3) call?) 'ExprSyntaxExc expr)
            (expr-translater (list 'when (list 'not (cadr expr)) (cddr expr)) #f ret?))
           ((while)
            (Error! (>= (length expr) 3) 'ExprSyntaxExc expr)
            (Error call? 'ExprSyntaxExc (list 'CALL expr))
            (string-append "while ("
                           (expr-translater (cadr expr) CALL #f)
                           ")\n{\n"
                           (expr-translater (cons 'code-block (cddr expr)) #f #f)
                           "}\n"))
           ((cond)
            (Error! (>= (length expr) 2) 'ExprSyntaxExc expr)
            (expr-translater (let ((rexp (reverse (cdr expr))))
                               (do ((expr1 (cdr rexp) (cdr expr1))
                                    (i (length expr) (- i 1))
                                    (res (if (eq? (caar rexp) 'else)
                                             (cons 'code-block (cdar rexp))
                                             (list 'if (caar rexp) (cons 'code-block (cdar rexp))))
                                         (list 'if
                                               (if (eq? (caar expr1) 'else) #t (caar expr1))
                                               (cons 'code-block (cdar expr1)) res)))
                                 ((<= i 2) res)))
                             call? ret?))
           (else
            (string-append
             (if ret? "return " "")
             (let ([tsymb (typed-symbol (car expr))])
               (if tsymb
                   (case (tSymbol-symb tsymb)
                     ((fn λ)
                      (Error (< (length expr) 3) 'ClosureSyntaxExc expr)
                      (let ([params (cond
                                      ((null? (cadr expr)) '())
                                      ((list? (cadr expr))
                                       (map (λ(s)
                                              (Error! (and (symbol? s) (typed-symbol s)) 'ClosureSyntaxExc expr)
                                              (typed-symbol s))
                                            (cadr expr))))])
                        (string-append "((Func<"
                                       (string-append* (map (λ(s)
                                                              (string-append (cadr (typed-symbol->list s)) ", "))
                                                            params))
                                       (cadr (typed-symbol->list tsymb))
                                       ">)(("
                                       (string-join (map (λ(s) (car (typed-symbol->list s)))
                                                         params)
                                                    ", ")
                                       ") => "
                                       (expr-translater (cons 'code-block (cddr expr)) #f RET)
                                       "))")))
                     (else (throw-exception  'ExprSyntaxExc expr)))
                   (string-append (cond
                                    ((symbol? (car expr)) (symbol->csharp-string (car expr)))
                                    ((list? (car expr)) (expr-translater (car expr) #f #f))
                                    (else (throw-exception  'ExprSyntaxExc expr)))
                                  "("
                                  (string-join (map (λ (t)
                                                      (expr-translater t CALL #f))
                                                    (cdr expr))
                                               ", ")
                                  ")")))))))
        #f))

  (define (class-translater lst return-result)
    (if (and (pair? lst) (list? lst) (eq? (car lst) 'class) (>= (length lst) 2)
             (or (symbol? (cadr lst)) (and (pair? (cadr lst)) (list? (cadr lst)))))
        (let* ([tsymb (if (symbol? (cadr lst))
                          (typed-symbol (cadr lst))
                          (typed-symbol (caadr lst)))]
               [tl (if tsymb
                       (typed-symbol->list tsymb)
                       (list (symbol->csharp-string (if (symbol? (cadr lst)) (cadr lst) (caadr lst)))))]
               [parents (if (pair? (cadr lst)) (cdadr lst) null)])
          (return-result
           (string-append (if tsymb (caddr tl) "")
                          " class "
                          (car tl)
                          (if (pair? parents)
                              (string-append ": " (string-join (map (λ(p) (expr-translater p #f #f))
                                                                    parents)
                                                               ", "))
                              "")
                          "\n{\n"
                          (string-append*
                           (map (λ(e)
                                  (Error! (and (pair? e) (list? e)) 'ClassSyntaxExc lst)
                                  (case (car e)
                                    ((constructor)
                                     (Error! (and (pair? (cadr e)) (list? (cadr e)) (typed-symbol (caadr e)))
                                             'ConstructorSyntaxExc e)
                                     (let ([name (typed-symbol->list (typed-symbol (caadr e)))])
                                       (string-append (caddr name) " " (car name) " ("
                                                      (string-join (map (λ(s)
                                                                          (Error! (and (symbol? s) (typed-symbol s))
                                                                                  'ConstructorSyntaxExc e)
                                                                          (let ([t (typed-symbol->list (typed-symbol s))])
                                                                            (string-append (cadr t) " " (car t))))
                                                                        (cdadr e))
                                                                   ", ")
                                                      ") "
                                                      (expr-translater (cons 'code-block (cddr e)) #f #f))))
                                    ((method)
                                     (Error! (and (pair? (cadr e)) (list? (cadr e)) (typed-symbol (caadr e)))
                                             'MethodSyntaxExc e)
                                     (let ([name (typed-symbol->list (typed-symbol (caadr e)))])
                                       (string-append (caddr name) " " (cadr name) " " (car name)
                                                      " ("
                                                      (string-join (map (λ(s)
                                                                          (Error! (and (symbol? s) (typed-symbol s))
                                                                                  'MethodSyntaxExc e)
                                                                          (let ([t (typed-symbol->list (typed-symbol s))])
                                                                            (string-append (cadr t) " " (car t))))
                                                                        (cdadr e))
                                                                   ", ")
                                                      ") "
                                                      (expr-translater (cons 'code-block (cddr e)) #f
                                                                       (if (string=? (cadr name) "void") #f RET)))))
                                    ((define)
                                     (Error! (and (= (length e) 3) (symbol? (cadr e)) (typed-symbol (cadr e)))
                                             'DefineSyntaxExc e)
                                     (let ([tl (typed-symbol->list (typed-symbol (cadr e)))])
                                       (string-append (caddr tl)
                                                      " "
                                                      (cadr tl)
                                                      " "
                                                      (car tl)
                                                      " = "
                                                      (expr-translater (caddr e) CALL #f)
                                                      ";\n")))
                                    (else (throw-exception 'ClassSyntaxExc lst))))
                                (cddr lst)))
                          "\n}\n")))
        #f))

  (define (using-translater lst return-result)
    (if (and (pair? lst) (list? lst) (pair? (cdr lst)) (eq? (car lst) 'using))
        (return-result
         (string-append* (map (λ(s)
                                (Error! (and (symbol? s) (not (typed-symbol s))) 'UsingSyntaxExc lst)
                                (string-append "using " (symbol->csharp-string s) ";\n"))
                              (cdr lst))))
        #f))

  (define (namespace-translater lst return-result)
    (if (and (pair? lst) (list? lst) (eq? (car lst) 'namespace) (>= (length lst) 2)
             (symbol? (cadr lst)) (not (typed-symbol (cadr lst))))
        (return-result
         (string-append "namespace " (symbol->csharp-string (cadr lst))
                        "\n{\n"
                        (string-append* (map second-level-translater
                                             (cddr lst)))
                        "\n}\n"))
        #f))

  (define expr-translaters `[(simple-expr-translater . ,simple-expr-translater)
                             (different-expr-translater . ,different-expr-translater)])

  (define second-level-translaters `[(using-translater . ,using-translater)
                                     (class-translater . ,class-translater)])

  (define top-level-translaters `[(using-translater . ,using-translater)
                                  (namespace-translater . ,namespace-translater)])

  (define (add-expr-translater name translater)
    (set! expr-translaters (cons (cons name translater) expr-translaters)))

  (define (remove-expr-translater name)
    (when (assoc name expr-translaters)
      (set! expr-translaters (remove name expr-translaters
                                     (λ(a b) (equal? a (car b)))))))

  (define (expr-translater expr call? ret?)
    (call/cc (λ (return)
               (for-each (λ (f) ((cdr f) expr call? ret? return))
                         expr-translaters)
               (throw-exception 'ExprSyntaxExc expr))))

  (define (top-level-translater lst)
    (call/cc (λ (return)
               (for-each (λ (f) ((cdr f) lst return))
                         top-level-translaters)
               (throw-exception 'SyntaxExc))))

  (define (second-level-translater lst)
    (call/cc (λ (return)
               (for-each (λ (f) ((cdr f) lst return))
                         second-level-translaters)
               (throw-exception 'NameSpaceSyntaxExc lst))))

  (define (safe->csharp lst)
    (Error! (and (pair? lst) (list? lst)) 'SyntaxExc)
    (string-append* (map (λ(l) (top-level-translater l)) lst)))

  (define (safe-file->csharp-file fname)
    (Error! (string? fname) 'GlobalErrorExc "~a is not string." fname)
    (let ([iport (open-input-file fname)]
          [oport (open-output-file (string-append fname ".cs"))])
      (display "using System;\n" oport)
      (let loop ((exp (read iport)))
        (unless (eof-object? exp)
          (display (top-level-translater exp) oport)
          (loop (read iport))))
      (close-input-port iport)
      (close-output-port oport)))

  (define (safe-files->csharp-files . fnames)
    (for-each safe-file->csharp-file fnames)))
