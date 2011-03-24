;  This progam is a Another-Mutable-Forth interpreter.
;
;  Copyright (c) 2010-2011  Mikhail Mosienko  <netluxe@gmail.com>
;
;  Permission is hereby granted, free of charge, to any person obtaining a copy
;  of this software and associated documentation files (the "Software"), to deal
;  in the Software without restriction, including without limitation the rights
;  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;  copies of the Software, and to permit persons to whom the Software is
;  furnished to do so, subject to the following conditions:
;
;  The above copyright notice and this permission notice shall be included in
;  all copies or substantial portions of the Software.
;
;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;  THE SOFTWARE.
;

(define call/cc call-with-current-continuation)
(define 4th-quit #f)
(define 4th-exception #f)

(define (list->number lst)
  (string->number (list->string lst)))

(define (list->symbol lst)
  (string->symbol (list->string lst)))

(define (readln)
  (let ((c (read-char (car input-port))))
    (cond
      ((eof-object? c)       '())
      ((char=? c #\newline)  '())
      (else (cons c (readln))))))

(define (clist-downcase clist)
  (map char-downcase clist))

;(define (ilist-downcase ilist)
;  (clist->ilist (map char-downcase (ilist->clist ilist))))

(define (clist->ilist clist)
  (map char->integer clist))

(define (ilist->clist ilist) ; ERROR!!!
  (map (lambda (int)
         (if (integer? int)
             (integer->char int)
             (rtm-error 'set! 'WrongTypeExc)))
       ilist))

(define (next-word)
  (if (pair? (car input-chars))
      (letrec ((clear-ws
                (lambda(l)
                  (if (pair? l)
                      (if (char-whitespace? (integer->char (car l)))
                          (clear-ws (cdr l))
                          l)
                      l)))
               (getword
                (lambda(l)
                  (let loop ((l (clear-ws l)))
                    (cond
                      ((null? l)
                       (begin (set-car! input-chars l) '()))
                      ((char-whitespace? (integer->char (car l)))
                       (begin (set-car! input-chars (cdr l)) '()))
                      (else
                       (cons (car l) (loop (cdr l)))))))))
        (getword (car input-chars)))
      '()))

(define (4th-reader)
  (let ((l (readln)))
    (if (pair? l)
        l
        (4th-reader))))

(define (4th-repl)
  (if compilation?
      (display "**] ")
      (display "] "))
  (set-car! input-chars (clist->ilist (4th-reader)))
  (let loop ()
   (let ((x (clist-downcase (ilist->clist (next-word)))))
     (cond
       ((null? x) )
       (else
        (let* ((symb (list->symbol x))
               (wrd (voc-word? symb))
               (num (list->number x)))
          (set! current-word symb)
          (cond
            (wrd
               (if (and compilation?
                        (not (word-immediate? (word-flags wrd))))
                   (voc-add-code (car wrd))
                   (if (and (not compilation?)
                            (word-compile? (word-flags wrd)))
                       (rtm-error 'set! 'CompileOnlyExc symb)
                       (eval-word (car wrd)))))
            (num
               (if compilation?
                   (voc-add-code num)
                   (stack 'push num)))
            (else
               (rtm-error 'set! 'UndefinedExc symb)))
          (loop))))))
  (newline)
  (4th-repl))

(define (4th)
  (display "Another-Mutable-Forth interpreter, Version 0 Alfa 8\n")
  (display "Copyright (c) 2010  Mikhail Mosienko  <cnet@land.ru>\n\n")
  (4th-init)
  (let ((bye (call/cc (lambda (exit-code)
                        (set! 4th-quit exit-code) #f))))
    (if (number? bye)
        (begin
          (display "Код выхода: ") (write bye))
        (begin
          (call/cc
           (lambda (exc) (set! 4th-exception exc)))
          (4th-repl)))))

(define (make-stack-obj)
  (let ((stk '()) (err #f))
    (lambda (cmd . args)
      (case  cmd
        ((empty?)
           (null? stk))
        ((length)
           (length stk))
        ((error?)
           err)
        ((push)
           (for-each (lambda (arg)
               (set! stk (cons arg stk)))
             args))
        ((pushl)
           (for-each (lambda (arg)
               (set! stk (cons arg stk)))
             (car args)))
        ((pop)
           (do ((i 1 (+ i 1)))
               ((or (> i (car args))
                    err))
             (if (null? stk)
                 (set! err 'UnderflowExc)
                 (set! stk (cdr stk)))))
        ((pop-val)
           (if (null? stk)
               (set! err 'UnderflowExc)
               (let ((val (car stk)))
                 (set! stk (cdr stk))
                 val)))
        ((top)
           (if (null? stk)
               (set! err 'UnderflowExc)
               (car stk)))
        ((top-1)
           (cond
             ((null? stk)       (set! err 'UnderflowExc))
             ((null? (cdr stk)) (set! err 'UnderflowExc))
             (else (cadr stk))))
        ((top-2)
           (cond
             ((null? stk)        (set! err 'UnderflowExc))
             ((null? (cdr stk))  (set! err 'UnderflowExc))
             ((null? (cddr stk)) (set! err 'UnderflowExc))
             (else (caddr stk))))
        ((top-3)
           (cond
             ((null? stk)         (set! err 'UnderflowExc))
             ((null? (cdr stk))   (set! err 'UnderflowExc))
             ((null? (cddr stk))  (set! err 'UnderflowExc))
             ((null? (cdddr stk)) (set! err 'UnderflowExc))
             (else (cadddr stk))))
        ((print)
           (for-each (lambda (arg) (begin (write arg) (display " ")))
             stk))
        ((reverse-print)
           (for-each (lambda (arg) (begin (write arg) (display " ")))
             (reverse stk)))
        ((reset)
           (set! stk '()) (set! err #f))
        (else (set! err 'DevelopExc))))))

(define (make-error-obj)
  (let ((errmsg
         '((DevelopExc              . "Ошибка на совести разработчика системы")
           (AbortExc                . "")
           (UnderflowExc            . "Обратное переполнение стека")
           (UndefinedExc            . " - cлово не определено")
           (NoNameExc               . "Отсутствует имя слова")
           (EvlUnderflowExc         . "Обратное переполнение стека вычисляемых слов")
           (EvledUnderflowExc       . "Обратное переполнение стека вычисленных слов")
           (SsUnderflowExc          . "Обратное переполнение дополнительного стека")
           (EsUnderflowExc          . "Обратное переполнение стека исключений")
           (RsUnderflowExc          . "Обратное переполнение стека возвратов")
           ;(EsMistakeValExc         . "Неверное значение в стеке исключений")
           (DivisionByZeroExc       . "Деление на ноль")
           (WrongTypeExc            . "Неправильный тип")
           (WrongIndexExc           . "Неверное значение индекса")
           (WrongPWrdExc            . "Неверное значение адресного слова")
           (SListToNumberExc        . "Ошибка преобразавания списка slist в число")
           (ReqPositiveOperandExc   . "Необходим положительный операнд")
           (MismatchTypesExc        . "Несоответствие типов")
           (UnknownWrdTypeExc       . "Неизвестный тип слова")
           ;(ChangeTypeKernelWrdExc  . "Запрещено изменять тип у kernel-... слов")
           (UnknownTypeExc          . " - неизвестный тип данных")
           (CompileOnlyExc          . " - интерпретация слова только для компиляции"))))
    (lambda (cmd . args)
      (case  cmd
        ((set!)
         (newline)
         (display "Остановка на слове: ")
         (display current-word)
         (newline)
         (display "Исключение: ")
         (if (not (null? (cdr args)))
             (display (cadr args)))
         (display (cdr (assq (car args) errmsg)))
         (newline)
         (stack        'reset)
         (sub-stack    'reset)
         (evl-stack    'reset)
         (evled-stack  'reset)
         (ret-stack    'reset)
         (exc-stack    'reset)
         (set! compilation? #f)
         (set! vocabulary (car voc-link))
         (4th-exception))))))

(define rtm-error    #f) ; объект для обработки исключений
(define stack        #f) ; стек
(define sub-stack    #f) ; дополнительный стек
(define ret-stack    #f) ; стек возвтатов к первоначальному состоянию eval-стеков
(define exc-stack    #f) ; стек исключений
(define evl-stack    #f) ; стек вычисляемых слов
(define evled-stack  #f) ; стек вычисленных слов
(define vocabulary   #f) ; словарь
(define voc-link     #f) ; псевдо-указатель на словарь
(define compilation? #f) ; флаг состояния компиляция-вычисление
(define current-word #f) ; последнее считанное слово
(define input-chars  #f) ; текстовый буфер
(define input-port   #f) ; in-порт
(define output-port  #f) ; out-порт


(define (gen-stack-obj-exc stack-obj exc)
  (let ((err (stack-obj 'error?)))
    (if err
        (cond
          ((eq? err 'UnderflowExc) (rtm-error 'set! exc))
          (err (rtm-error 'set! 'DevelopExc))))))

(define (stack-error?)
  (gen-stack-obj-exc  stack 'UnderflowExc))

(define (sub-stack-error?)
  (gen-stack-obj-exc  sub-stack 'SsUnderflowExc))

(define (exc-stack-error?)
  (gen-stack-obj-exc  exc-stack 'EsUnderflowExc))

(define (ret-stack-error?)
  (gen-stack-obj-exc  ret-stack 'RsUnderflowExc))

(define (evl-stack-error?)
  (gen-stack-obj-exc  evl-stack 'EvlUnderflowExc))

(define (evled-stack-error?)
  (gen-stack-obj-exc  evled-stack 'EvledUnderflowExc))

; Структура словарной статьи
; (Инфо:pair  Флаги:list  Тело:list)
; Структура Инфо
; (Имя:symbol . НомерЗаписи:integer)

(define (voc-add lst)
  (set! vocabulary (cons lst vocabulary)))

(define (init-voc-add lst)
  (set! vocabulary (cons (cons
                          (cons (car lst)
                                (if (null? vocabulary)
                                     0
                                     (+ 1 (cdaar vocabulary))))
                          (cdr lst))
                         vocabulary))
  (set-car! voc-link vocabulary))

; voc-add-empty - добавляет в словарь пустое common слово
(define (voc-add-empty wrd)
  (voc-add (list (cons wrd
                       (if (null? vocabulary)
                           0
                           (+ 1 (cdaar vocabulary))))
                 '(common) '())))

(define (voc-add-codes lst)
  (set-car! (cddar vocabulary) (append (caddar vocabulary) lst)))

(define (voc-add-code wrd)
  (voc-add-codes (list wrd)))

(define (voc-change-code pos val)
  (set-car! (list-tail (word-body (last-word)) pos) val))

(define (last-word) (car vocabulary))
(define word-body caddr)

(define (voc-get-body wrd)
  (word-body (assq wrd vocabulary)))

(define (voc-set-flag flag)
  (if (flags>word-type (word-flags (caar voc-link)))
      (set-car! (cdaar voc-link) (cons flag (cdaar voc-link)))
      (rtm-error 'set! 'UnknownWrdTypeExc)))

(define (voc-set-type type)
  (if (word-type? type)
      (set-car! (cdaar voc-link) (list type))
      (rtm-error 'set! 'UnknownWrdTypeExc)))

(define (voc-word? wrd)
  (call/cc
   (lambda(r)
     (for-each (lambda(x)
                 (if (eq? (caar x) wrd)
                     (r x)))
               (car voc-link))
     #f)))

(define (find-pword? pwrd)
  (list-ref (car voc-link) (- (cdaaar voc-link) (cdr pwrd))))

(define (word-type? type)
  (memq type '(common kernel variable constant)))

(define (flags>word-type flags)
  (call/cc
   (lambda (return)
     (for-each (lambda(f)
                 (let ((val (memq f '(common kernel variable constant))))
                   (if val
                       (return (car val)))))
               flags))))

(define (word-immediate? flags)
  (if (list? flags)
      (memq 'immediate flags)
      #f))

(define (word-compile? flags)
  (if (list? flags)
      (memq 'compile flags)
      #f))

(define (word-kernel? flags)
  (if (list? flags)
      (memq 'kernel flags)
      #f))

(define (code-word? pair-wrd)
  (if (pair? pair-wrd)
      (if (and (symbol? (car pair-wrd))
               (integer? (cdr pair-wrd)))
          #t #f)
      #f))

(define (store-eval-stacks)
  (ret-stack 'push (cons (evled-stack 'length) (evl-stack 'length))))

(define (restore-eval-stacks)
  (let ((ret (ret-stack 'pop-val)))
    (cond
      ((ret-stack 'error?)
       (ret-stack-error?))
      ((and (pair? ret)
            (not (list? ret)))
       (if (and (integer? (car ret))
                (integer? (cdr ret)))
           (begin
             (evled-stack 'pop (- (evled-stack 'length) (car ret)))
             (evl-stack 'pop (- (evl-stack 'length) (cdr ret))))
           (rtm-error 'set! 'WrongTypeExc)))
      (else (rtm-error 'set! 'WrongTypeExc)))))

(define (gencode lst)
  (let ((l (map (lambda(el)
                  (cond
                    ((symbol? el) (car (voc-word? el)))
                    ((number? el) el)
                    ((list? el)
                     (if (eq? (car el) 'quote)
                         (cadr el)
                         el))))
                 lst)))
    (append l (list (car (voc-word? 'rets))))))
; decode - возвращает форматированное тело пользовательского слова
;          в виде списка
(define (decode wrd)
  (if (not (word-kernel? (word-flags (voc-word? wrd))))
      (let ((l (map (lambda(el)
                     (if (pair? el)
                         (if (eq? (car el) 'quote)
                             (cadr el)
                             (if (list? el)
                                 el (car el)))
                         el))
                    (word-body (voc-word? wrd)))))
        l)))

(define (voc-print)
  (write vocabulary))

(define (evl-push-body body)
  (evl-stack 'pushl (reverse body)))

(define (restore-evl n)
  (do ((i 1 (+ i 1)))
      ((> i n) )
      (begin
        (evl-stack 'push (evled-stack 'pop-val))
        (evled-stack-error?))))

(define (store-evl n)
  (do ((i 1 (+ i 1)))
      ((> i n) )
      (begin
        (evled-stack 'push (evl-stack 'pop-val))
        (evl-stack-error?))))

(define word-flags cadr)
; eval-word - вычисляет найденное сово в формате (name . position)
(define (eval-word wrd)
  ;(if (pair? wrd)
  ;    (begin
  ;      (display "Trace: ")(write (car wrd)) (newline))
  ;    (begin
  ;      (display "Trace: ")(write wrd) (newline)))
  (cond
    ((number? wrd)
     (stack 'push wrd))
    ((pair? wrd)
     (if (code-word? wrd)
         (let ((wrd-rec (find-pword? wrd)))
           (case (flags>word-type (word-flags wrd-rec))
             ((kernel)
              ((word-body wrd-rec)))
             ((common)
              (begin
                (store-eval-stacks)
                (evl-push-body (word-body wrd-rec))
                (evaluator)))
             ((variable)
              (begin
                (stack 'push (word-body wrd-rec))))
             ((constant)
              (begin
                (stack 'push (car (word-body wrd-rec)))))))
         (rtm-error 'set! 'UnknownTypeExc wrd)))
    (else
      (rtm-error 'set! 'UnknownTypeExc wrd))))
; evaluator - главный вычислительный цикл
(define (evaluator)
  (if (not (evl-stack 'empty?))
      (let ((wrd (evl-stack 'pop-val)))
        (evled-stack 'push wrd)
        (eval-word wrd)
        (evaluator))))

(define (4th-init-variables)
  (set! rtm-error    (make-error-obj))
  (set! stack        (make-stack-obj))
  (set! sub-stack    (make-stack-obj))
  (set! ret-stack    (make-stack-obj))
  (set! exc-stack    (make-stack-obj))
  (set! evl-stack    (make-stack-obj))
  (set! evled-stack  (make-stack-obj))
  (set! vocabulary   '())
  (set! voc-link     '(()))
  (set! compilation? #f)
  (set! current-word 'Undefined)
  (set! input-chars  '(()))
  (set! input-port   (list (current-input-port)))
  (set! output-port  (list (current-output-port))))
;;
;; 4th-init - загружает словарь
;;
(define (4th-init)
  (4th-init-variables) ; инициализация главных переменных

  (init-voc-add
   (list '|+| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (number? t)
                 (number? t-1))
            (stack 'push (+ t-1 t)))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|-| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (number? t)
                 (number? t-1))
            (stack 'push (- t-1 t)))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|*| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (number? t)
                 (number? t-1))
            (stack 'push (* t-1 t)))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|/| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (number? t)
                 (number? t-1))
            (if (= t 0)
                (rtm-error 'set! 'DivisionByZeroExc)
                (stack 'push (/ t-1 t))))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|div| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (integer? t)
                 (integer? t-1))
            (if (= t 0)
                (rtm-error 'set! 'DivisionByZeroExc)
                (stack 'push (quotient t-1 t))))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|mod| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (integer? t)
                 (integer? t-1))
            (if (= t 0)
                (rtm-error 'set! 'DivisionByZeroExc)
                (stack 'push (modulo t-1 t))))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|rem| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (integer? t)
                 (integer? t-1))
            (if (= t 0)
                (rtm-error 'set! 'DivisionByZeroExc)
                (stack 'push (remainder t-1 t))))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|>| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (number? t)
                 (number? t-1))
            (stack 'push (if (> t-1 t) -1 0)))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|<| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (number? t)
                 (number? t-1))
            (stack 'push (if (< t-1 t) -1 0)))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|=| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (number? t)
                 (number? t-1))
            (stack 'push (if (= t-1 t) -1 0)))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|or| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (number? t)
                 (number? t-1))
            (stack 'push (if (or (not (zero? t-1))
                                 (not (zero? t))) -1 0)))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|and| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((and (number? t)
                 (number? t-1))
            (stack 'push (if (and (not (zero? t-1))
                                  (not (zero? t))) -1 0)))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|eq| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (stack 'push (if (equal? t-1 t) -1 0)))))))
  (init-voc-add
   (list '|not| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((number? t)
            (stack 'push (if (zero? t) -1 0)))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|floor| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((number? t)
            (stack 'push (floor t)))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|ceiling| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((number? t)
            (stack 'push (ceiling t)))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|truncate| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((number? t)
            (stack 'push (truncate t)))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|round| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((number? t)
            (stack 'push (round t)))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|complex?| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((complex? t)
            (stack 'push -1))
           (else (stack 'push 0)))))))
  (init-voc-add
   (list '|real?| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((real? t)
            (stack 'push -1))
           (else (stack 'push 0)))))))
  (init-voc-add
   (list '|rational?| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((rational? t)
            (stack 'push -1))
           (else (stack 'push 0)))))))
  (init-voc-add
   (list '|integer?| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((integer? t)
            (stack 'push -1))
           (else (stack 'push 0)))))))
  (init-voc-add
   (list '|number?| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((number? t)
            (stack 'push -1))
           (else (stack 'push 0)))))))
  (init-voc-add
   (list '|number>slist| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((number? t)
            (stack 'push (clist->ilist (string->list (number->string t)))))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|slist>number| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((list? t)
            (let ((num (string->number (list->string (ilist->clist t)))))
              (if num
                  (stack 'push num)
                  (rtm-error 'set! 'SListToNumberExc))))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|drop| '(kernel)
       (lambda()
         (stack 'pop 1)
         (stack-error?))))
  (init-voc-add
   (list '|dup| '(kernel)
       (lambda()
         (stack 'push (stack 'top))
         (stack-error?))))
  (init-voc-add
   (list '|?dup| '(kernel)
     (lambda()
       (let ((t (stack 'top)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((number? t)
            (if (not (zero? t))
                (stack 'push t)))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|swap| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (stack 'push t t-1))))))
  (init-voc-add
   (list '|over| '(kernel)
       (lambda()
         (stack 'push (stack 'top-1))
         (stack-error?))))
  (init-voc-add
   (list '|>ss| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (sub-stack 'push t))))))
  (init-voc-add
   (list '|ss>| '(kernel)
     (lambda()
       (let ((t (sub-stack 'pop-val)))
         (if (sub-stack 'error?)
             (sub-stack-error?)
             (stack 'push t))))))
  (init-voc-add
   (list '|ss@| '(kernel)
     (lambda()
       (let ((t (sub-stack 'top)))
         (if (sub-stack 'error?)
             (sub-stack-error?)
             (stack 'push t))))))
  (init-voc-add
   (list '|>es| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (exc-stack 'push t))))))
  (init-voc-add
   (list '|es>| '(kernel)
     (lambda()
       (let ((t (exc-stack 'pop-val)))
         (if (exc-stack 'error?)
             (exc-stack-error?)
             (stack 'push t))))))
  (init-voc-add
   (list '|es@| '(kernel)
     (lambda()
       (let ((t (exc-stack 'top)))
         (if (exc-stack 'error?)
             (exc-stack-error?)
             (stack 'push t))))))
  (init-voc-add
   (list '|>rs| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (ret-stack 'push t))))))
  (init-voc-add
   (list '|rs>| '(kernel)
     (lambda()
       (let ((t (ret-stack 'pop-val)))
         (if (ret-stack 'error?)
             (ret-stack-error?)
             (stack 'push t))))))
  (init-voc-add
   (list '|rs@| '(kernel)
     (lambda()
       (let ((t (ret-stack 'top)))
         (if (ret-stack 'error?)
             (ret-stack-error?)
             (stack 'push t))))))
  (init-voc-add
   (list '|@| '(kernel)
     (lambda()
       (let ((var (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((pair? var)     (stack 'push (car var)))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|$| '(kernel)
     (lambda()
       (let ((var (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((pair? var)     (stack 'push (cdr var)))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|!| '(kernel)
     (lambda()
       (let ((var (stack 'pop-val)) (data (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((pair? var)     (set-car! var data))
           (else            (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|emit| '(kernel)
     (lambda()
       (let ((val (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((integer? val)  (display (integer->char val)))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|.| '(kernel)
     (lambda()
       (let ((val (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (begin
               (write val) (display " ")))))))
  (init-voc-add
   (list '|.s| '(kernel)
       (lambda()
         (display "<") (write (stack 'length)) (display "> ")
         (stack 'reverse-print))))
  (init-voc-add
   (list '|lit| '(kernel)
     (lambda()
       (let ((val (evl-stack 'pop-val)))
         (if (evl-stack 'error?)
             (evl-stack-error?)
             (begin
               (stack 'push val)
               (evled-stack 'push val)))))))
  (init-voc-add
   (list '|[| '(kernel immediate)
       (lambda() (set! compilation? #f))))
  (init-voc-add
   (list '|]| '(kernel immediate)
       (lambda() (set! compilation? #t))))
  (init-voc-add
   (list '|rets| '(kernel)
       (lambda() (restore-eval-stacks))))
  (init-voc-add
   (list '|drops-evl| '(kernel)
     (lambda()
       (let ((n (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((integer? n)
            (evl-stack 'pop n)
            (evl-stack-error?))
           (else
            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|drops-evled| '(kernel)
     (lambda()
       (let ((n (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((integer? n)
            (evled-stack 'pop n)
            (evled-stack-error?))
           (else
            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|depth| '(kernel)
       (lambda() (stack 'push (stack 'length)))))
  (init-voc-add
   (list '|sub-depth| '(kernel)
       (lambda() (stack 'push (sub-stack 'length)))))
  (init-voc-add
   (list '|evled-depth| '(kernel)
       (lambda() (stack 'push (evled-stack 'length)))))
  (init-voc-add
   (list '|evl-depth| '(kernel)
       (lambda() (stack 'push (evl-stack 'length)))))
  (init-voc-add
   (list '|ret-depth| '(kernel)
       (lambda() (stack 'push (ret-stack 'length)))))
  (init-voc-add
   (list '|exc-depth| '(kernel)
       (lambda() (stack 'push (exc-stack 'length)))))
  (init-voc-add
   (list '|restore-evl| '(kernel)
     (lambda()
       (let ((n (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((integer? n)    (restore-evl n))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|store-evl| '(kernel)
     (lambda()
       (let ((n (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((integer? n)    (store-evl n))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|drops-evl-evled| '(kernel)
     (lambda()
       (let ((n-evl (stack 'pop-val)) (n-evled (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((and (integer? n-evl)
                 (integer? n-evled))
            (evl-stack 'pop n)
            (evl-stack-error?)
            (evled-stack 'pop n)
            (evled-stack-error?))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|>lw| '(kernel)
     (lambda()
       (let ((val (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (voc-add-code val))))))
  (init-voc-add
   (list '|change-lw| '(kernel)
     (lambda()
       (let ((pos (stack 'pop-val)) (val (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((integer? pos)  (voc-change-code pos val))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|length-lw| '(kernel)
     (lambda()
       (stack 'push (length (word-body (last-word)))))))
  (init-voc-add
   (list '|lit-word| '(kernel)
     (lambda()
       (let ((val (list->symbol (clist-downcase (ilist->clist (next-word))))))
         (if (eq? val '||)
             (rtm-error 'set! 'NoNameExc)
             (stack 'push val))))))
  (init-voc-add
   (list '|list-word| '(kernel)
       (lambda() (stack 'push (next-word)))))
  (init-voc-add
   (list '|def-empty| '(kernel)
     (lambda()
       (let ((wrd (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((symbol? wrd)   (voc-add-empty wrd))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|set-type| '(kernel)
     (lambda()
       (let ((type (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((symbol? type)  (voc-set-type type))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|set-flag| '(kernel)
     (lambda()
       (let ((flag (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((symbol? flag)  (voc-set-flag flag))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|nil| '(kernel)
      (lambda() (stack 'push '()))))
  (init-voc-add
   (list '|cons| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (stack 'push (cons t-1 t)))))))
  (init-voc-add
   (list '|append| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)) (t-1 (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((and (list? t)
                 (list? t-1))
            (stack 'push (append t-1 t)))
           (else (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|length| '(kernel)
     (lambda()
       (let ((l (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((list? l)       (stack 'push (length l)))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|reverse| '(kernel)
     (lambda()
       (let ((l (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((list? l)       (stack 'push (reverse l)))
           (else            (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|ref| '(kernel)
     (lambda()
       (let ((k (stack 'pop-val)) (l (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (if (and (pair? l)
                      (list? l)
                      (integer? k))
                 (if (>= k 0)
                     (if (> (length l) k)
                         (stack 'push (list-ref l k))
                         (rtm-error 'set! 'WrongIndexExc))
                     (rtm-error 'set! 'ReqPositiveOperandExc))
                 (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|tail| '(kernel)
     (lambda()
       (let ((k (stack 'pop-val)) (l (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (if (and (pair? l)
                      (list? l)
                      (integer? k))
                 (if (>= k 0)
                     (if (>= (length l) k)
                         (stack 'push (list-tail l k))
                         (rtm-error 'set! 'WrongIndexExc))
                     (rtm-error 'set! 'ReqPositiveOperandExc))
                 (rtm-error 'set! 'MismatchTypesExc)))))))
  (init-voc-add
   (list '|list?| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((list? t)
            (stack 'push -1))
           (else (stack 'push 0)))))))
  (init-voc-add
   (list '|pair?| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((pair? t)
            (stack 'push -1))
           (else (stack 'push 0)))))))
  (init-voc-add
   (list '|open:port>| '(kernel)
     (lambda()
       (let ((slist (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((list? slist)
            (let ((ifile (list->string (ilist->clist slist))))
              (stack 'push (open-input-file ifile))))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|close:port>| '(kernel)
     (lambda()
       (let ((iport (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((input-port? iport)
            (close-input-port iport))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|open:>port| '(kernel)
     (lambda()
       (let ((slist (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((list? slist)
            (let ((ofile (list->string (ilist->clist slist))))
              (stack 'push (open-output-file ofile))))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|close:>port| '(kernel)
     (lambda()
       (let ((oport (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((output-port? oport)
            (close-output-port oport))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|port>char| '(kernel)
     (lambda()
       (let ((iport (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((input-port? iport)
            (let ((char (read-char iport)))
              (if (eof-object? char)
                  (stack 'push 'eof)
                  (stack 'push (char->integer char)))))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|char>port| '(kernel)
     (lambda()
       (let ((oport (stack 'pop-val)) (ichar (stack 'pop-val)))
         (cond
           ((stack 'error?) (stack-error?))
           ((and (output-port? oport)
                 (integer? ichar))
            (write-char (integer->char ichar) oport))
           (else (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|port>?| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((input-port? t)
            (stack 'push -1))
           (else (stack 'push 0)))))))
  (init-voc-add
   (list '|>port?| '(kernel)
     (lambda()
       (let ((t (stack 'pop-val)))
         (cond
           ((stack 'error?)
            (stack-error?))
           ((output-port? t)
            (stack 'push -1))
           (else (stack 'push 0)))))))
  (init-voc-add
   (list '|get-def| '(kernel)
     (lambda()
       (let ((pw (stack 'pop-val)))
         (if (stack 'error?)
             (stack-error?)
             (if (and (pair? pw)
                      (not (list? pw)))
                 (let ((def (assoc pw (car voc-link))))
                   (if def
                       (stack 'push def)
                       (rtm-error 'set! 'WrongPWrdExc)))
                 (rtm-error 'set! 'WrongTypeExc)))))))
  (init-voc-add
   (list '|bye| '(kernel)
      (lambda() (4th-quit 0))))
  (init-voc-add
   `(|return| '(kernel compile)
      (lambda() (restore-eval-stacks))))
  (init-voc-add
   `(|quit| '(kernel compile)
     (lambda()
       (let() (stack       'reset)
              (sub-stack   'reset)
              (evl-stack   'reset)
              (evled-stack 'reset)
              (ret-stack   'reset)
              (exc-stack   'reset)
              (set! compilation? #f)
              (set! vocabulary (car voc-link))
              (4th-exception)))))
  (init-voc-add
   (list '|rtm-error1| '(kernel)
      (lambda() (rtm-error 'set! (stack 'pop-val)))))
  (init-voc-add
   (list '|rtm-error2| '(kernel)
      (lambda() (rtm-error 'set! (stack 'pop-val) (stack 'pop-val)))))
  (init-voc-add
   (list '|vocabulary| '(kernel)
      (lambda() (stack 'push (list vocabulary)))))
  (init-voc-add
   (list '|voc-link| '(kernel)
      (lambda() (stack 'push voc-link))))
  (init-voc-add
   (list '|in>| '(kernel)
      (lambda() (stack 'push input-chars))))
  (init-voc-add
   (list '|main-port>| '(kernel)
      (lambda() (stack 'push input-port))))
  (init-voc-add
   (list '|>main-port| '(kernel)
      (lambda() (stack 'push output-port))))
  (init-voc-add
   (list '|**if**| '(kernel)
     (lambda()
       (let ((pos (stack 'pop-val)) (val (stack 'pop-val)))
         (cond
           ((stack 'error?)      (stack-error?))
           ((not (integer? val)) (rtm-error 'set! 'MismatchTypesExc))
           ((zero? val)          (store-evl pos)))))))
  (init-voc-add
   (list '|**throw**| '(kernel)
     (lambda()
       (let ((exc (stack 'pop-val))
             (rs-pos (exc-stack 'pop-val))
             (evled-depth (exc-stack 'pop-val)))
         (cond
           ((stack 'error?)     (stack-error?))
           ((exc-stack 'error?) (exc-stack-error?))
           ((or (not (integer? rs-pos))
                (not (integer? evled-depth)))
            (rtm-error 'set! 'MismatchTypesExc))
           ((or (<= rs-pos 0)
                (<= evled-depth 0)
                (< (ret-stack 'length) rs-pos))
            (rtm-error 'set! 'MismatchTypesExc))
           ((= rs-pos (ret-stack 'length))
            (store-evl (- evled-depth (evled-stack 'length)))
            (sub-stack 'push exc))
           (else
            (ret-stack 'pop (- (ret-stack 'length) (+ rs-pos 1)))
            (ret-stack-error?)
            (restore-eval-stacks)
            (store-evl (- evled-depth (evled-stack 'length)))
            (sub-stack 'push exc)))))))
  (init-voc-add
   `(|1+| (common)
       ,(gencode '(1 +))))
  (init-voc-add
   `(|2+| (common)
       ,(gencode '(2 +))))
  (init-voc-add
   `(|1-| (common)
       ,(gencode '(1 -))))
  (init-voc-add
   `(|2-| (common)
       ,(gencode '(2 -))))
  (init-voc-add
   `(|2*| (common)
       ,(gencode '(2 *))))
  (init-voc-add
   `(|2/| (common)
       ,(gencode '(2 /))))
  (init-voc-add
   `(|*/| (common)
       ,(gencode '(>ss * ss> /))))
  (init-voc-add
   `(|negate| (common)
       ,(gencode '(-1 *))))
  (init-voc-add
   `(|0=| (common)
       ,(gencode '(0 =))))
  (init-voc-add
   `(|0>| (common)
       ,(gencode '(0 >))))
  (init-voc-add
   `(|0<| (common)
       ,(gencode '(0 <))))
  (init-voc-add
   `(|<>| (common)
       ,(gencode '(= not))))
  (init-voc-add
   `(|0<>| (common)
       ,(gencode '(0 <>))))
  (init-voc-add
   '(|true| (constant)
       (-1)))
  (init-voc-add
   '(|false| (constant)
       (0)))
  (init-voc-add
   `(|rot| (common)
       ,(gencode '(>ss swap ss> swap))))
  (init-voc-add
   `(|2dup| (common)
       ,(gencode '(over over))))
  (init-voc-add
   `(|2drop| (common)
       ,(gencode '(drop drop))))
  (init-voc-add
   `(|<=| (common)
       ,(gencode '(2dup < rot rot = or))))
  (init-voc-add
   `(|>=| (common)
       ,(gencode '(2dup > rot rot = or))))
  (init-voc-add
   `(|>2ss| (common)
       ,(gencode '(>ss >ss))))
  (init-voc-add
   `(|2ss>| (common)
       ,(gencode '(ss> ss>))))
  (init-voc-add
   `(|over-ss@| (common)
       ,(gencode '(ss> ss@ swap >ss))))
  (init-voc-add
   `(|ss.| (common)
       ,(gencode '(ss@ \. over-ss@ \.))))
  (init-voc-add
   `(|cr| (common)
       ,(gencode '(10 emit))))
  (init-voc-add
   `(|space| (common)
       ,(gencode '(32 emit))))
  (init-voc-add
   `(|car| (common)
       ,(gencode '(@))))
  (init-voc-add
   `(|cdr| (common)
       ,(gencode '($))))
  (init-voc-add
   `(|abort| (common)
       ,(gencode '(lit 'AbortExc rtm-error2))))
  (init-voc-add
   `(|if| (common compile immediate)
       ,(gencode '(0 >lw
                   lit **if** >lw
                   length-lw ))))
  (init-voc-add
   `(|then| (common compile immediate)
       ,(gencode '(dup length-lw swap
                   - swap 2- change-lw))))
  (init-voc-add
   `(|else| (common compile immediate)
       ,(gencode '(0 >lw
                   lit store-evl >lw
                   then
                   length-lw))))
  (init-voc-add
   `(|do| (common compile immediate)
       ,(gencode '(lit swap >lw  lit >2ss >lw
                   length-lw >ss
                   ; точка возврата
                   lit over-ss@ >lw
                   lit dup >lw lit 0> >lw
                   lit ss@ >lw lit swap >lw
                   if  lit > >lw
                   else lit <= >lw
                   then
                   if))))
  (init-voc-add
   `(|?do| (common compile immediate)
       ,(gencode '(do))))
  (init-voc-add
   `(|i| (common compile immediate)
       ,(gencode '(lit ss@ >lw))))
  (init-voc-add
   `(|j| (common compile)
       ,(gencode '(2ss> ss@ rot rot >2ss))))
  (init-voc-add
   `(|loop| (common compile immediate)
       ,(gencode '(lit ss> >lw  lit 1+ >lw
                   lit >ss >lw
                   ss> length-lw 2+ swap - >lw
                   lit restore-evl >lw  then
                   lit 2ss> >lw  lit 2drop >lw))))
  (init-voc-add
   `(|+loop| (common compile immediate)
       ,(gencode '(lit ss> >lw  lit + >lw
                   lit >ss >lw
                   ss> length-lw 2+ swap - >lw
                   lit restore-evl >lw  then
                   lit 2ss> >lw  lit 2drop >lw))))
  (init-voc-add
   `(|leave| (common compile immediate)
       ,(gencode '(0 >lw
                   length-lw  ss@ 1+  - >lw
                   lit restore-evl >lw))))
  (init-voc-add
   `(|begin| (common compile immediate)
       ,(gencode '(length-lw))))
  (init-voc-add
   `(|until| (common compile immediate)
       ,(gencode '(lit not >lw
                   if length-lw 2+
                      rot - >lw
                      lit restore-evl >lw
                   then))))
  (init-voc-add
   `(|while| (common compile immediate)
       ,(gencode '(if))))
  (init-voc-add
   `(|repeat| (common compile immediate)
       ,(gencode '(length-lw 2+
                   rot - >lw
                   lit restore-evl >lw
                   then))))
  (init-voc-add
   `(|throw| (common compile immediate)
       ,(gencode '(true >lw  lit >ss >lw
                   lit lit >lw lit-word >lw
                   lit **throw** >lw))))
  (init-voc-add
   `(|try| (common compile immediate)
       ,(gencode '(lit ret-depth >lw  lit evled-depth >lw
                   0 >lw  lit + >lw   length-lw
                   lit >es >lw  lit >es >lw))))
  (init-voc-add
   `(|catch| (common compile immediate)
       ,(gencode '(lit es> >lw  lit es> >lw
                   lit 2drop >lw
                   0 >lw
                   lit store-evl >lw
                   dup length-lw 2+ swap
                   - swap 2- change-lw
                   length-lw))))
  (init-voc-add
   `(|tried| (common compile immediate)
       ,(gencode '(lit 2ss> >lw
                    if lit exc-depth >lw
                     if true >lw  lit >ss >lw
                        lit **throw** >lw
                     else lit abort >lw
                     then
                    else lit drop >lw
                    then
                   then))))
  (init-voc-add
   `(|iferr| (common compile immediate)
       ,(gencode '(lit lit >lw  lit-word >lw
                   lit ss@ >lw  lit eq >lw
                   if
                   lit 2ss> >lw  lit not >lw
                   lit >2ss >lw))))
  (init-voc-add
   `(|ifeq| (common compile immediate)
       ,(gencode '(lit lit >lw  lit-word >lw
                   lit eq >lw  if))))
  (init-voc-add
   `(|immediate| (common)
       ,(gencode '(lit 'immediate set-flag))))
  (init-voc-add
   `(|compile| (common)
       ,(gencode '(lit 'compile set-flag))))
  (init-voc-add
   `(|'| (common immediate)
       ,(gencode '(lit-word))))
  (init-voc-add
   `(|type| (common) ; def type  0 do dup i ref emit loop drop ;
       ,(gencode '(0 swap  >2ss over-ss@ dup 0> ss@ swap
                   3 **if** > 1 store-evl <= 9 **if** dup i
                   ref emit ss> 1+ >ss 22 restore-evl 2ss>
                   2drop drop))))
  (init-voc-add
   `(|stype| (common)
       ,(gencode '(dup length type))))
  (init-voc-add
   `(|char>| (common)
       ,(gencode '(in> @ dup @ swap $ in> !))))
  (init-voc-add
   `(|[char]| (common compile immediate) ; def [char]  begin char> dup 32 = while drop repeat >lw ;
       ,(gencode '(char> dup 32 = 3 **if** drop 9 restore-evl >lw))))
  ; def s"
  ;     lit lit >lw nil
  ;     begin in> @
  ;      lit [ nil >lw ] eq
  ;      if reverse >lw return
  ;      else char> dup [char] " =
  ;       if drop reverse >lw return
  ;       else swap cons
  ;       then
  ;      then
  ;     false
  ;     until ;
  (init-voc-add
   `(|s"| (common compile immediate)
       ,(gencode '(lit lit >lw nil in> @ lit '() eq
                   5 **if** reverse >lw return 14 store-evl
                   char> dup 34 = 6 **if** drop reverse >lw
                   return 2 store-evl swap cons false not 2
                   **if** 32 restore-evl))))
  (init-voc-add
   `(|."| (common compile immediate)
       ,(gencode '(s\" lit stype >lw))))
  (init-voc-add
   `(|list| (common)  ; def list  nil swap 0 do cons loop ;
       ,(gencode '(nil swap 0 swap >2ss over-ss@ dup 0> ss@ swap
                   3 **if** > 1 store-evl <= 6 **if** cons ss> 1+
                   >ss 19 restore-evl 2ss> 2drop))))
  (init-voc-add
   `(|{| (common)
       ,(gencode '(depth >ss))))
  (init-voc-add
   `(|}| (common)
       ,(gencode '(depth ss> - list))))
  ; def (
  ;     begin in> @
  ;      lit [ nil >lw ] eq
  ;      if true
  ;      else char> [char] ) =
  ;      then
  ;     until ;
  (init-voc-add
   `(|(| (common immediate)
       ,(gencode '(in> @ lit '() eq 3 **if** true 3 store-evl
                   char> 41 = not 2 **if** 18 restore-evl))))
  (init-voc-add
   `(|?| (common)
       ,(gencode '(@ \.))))
  (init-voc-add
   `(|variable| (common)
       ,(gencode '(lit-word def-empty  0 >lw
                   vocabulary @ voc-link !
                   lit 'variable set-type))))
  (init-voc-add
   `(|constant| (common)
       ,(gencode '(lit-word def-empty >lw
                   vocabulary @ voc-link !
                   lit 'constant set-type))))
  (init-voc-add
   `(|recursive| (common immediate)
       ,(gencode '(vocabulary @ @ @ >lw))))
  (init-voc-add
   `(|def| (common)
       ,(gencode '(lit-word def-empty \]))))
  (init-voc-add
   `(|:| (common)
       ,(gencode '(def))))
  (init-voc-add
   `(|;| (common immediate)
       ,(gencode '(lit rets >lw
                   vocabulary @ voc-link ! \[))))
  )

(4th)

;
; Tests
;

; (ilist->clist (clist->ilist (string->list "aDG, gfjtj fg")))
; def 4d 2drop 2drop .s ;
; (ret-stack 'print)

; def crash 100 . recursive ; crash

; list-word aBnm,THP stype

; def do? ?dup if . cr then ; 10 do?
; def do? dup if . cr else .s then .s ; 10 do?

; def do? 10 begin dup . 1- dup not until . ; do?
; def do? 10 begin 1- dup while dup . repeat . ; do?

; def table cr 11 1 do 11 1 do i j * . loop cr loop ; table
; def do? 10 0 do i . cr loop ; do?
; def do? 10 0 do i . cr 2 +loop ; do?
; def do? 10 0 do i 5 - ?dup if . cr else leave then loop ; do?

; def test1 try 1 2 throw my-exc 3 4 5 catch .s tried ; test1
; def err  dup 2 = if throw my-exc then ;
; def test2 try -1 err -2 err 3 4 5 catch .s tried ; test2
; def test3 try 1 2 test2 3 4 5 catch .s tried ; test3
; def test4 try 1 2 throw my-exc 3 4 5 catch iferr my-exc .s then tried ; test4
; (decode 'test)

; : fact
;   dup 0= if drop 1 return then
;   dup 1- recursive * ;

