;  Simple .INI file parser in R5RS.
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

(define (list->symbol lst)
  (string->symbol (list->string lst)))

(define (list->number lst)
  (string->number (list->string lst)))

(define (conf-parser char-lst)
  (call-with-current-continuation
    (lambda (err-exc)
      (if (pair? char-lst)
          (letrec
            ((tail '())
             (clear-ws ;удаляет разделители
              (lambda(l)
                (if (pair? l)
                    (if (char-whitespace? (car l))
                        (clear-ws (cdr l))
                        l)
                    l)))
             (char-line ;возвращает считанную строку
              (lambda(l)
                (if (pair? l)
                    (if (memq (car l) '(#\return #\newline))
                        (begin
                          (set! tail (cdr l)) '())
                        (cons (car l) (char-line (cdr l))))
                    (begin
                          (set! tail '()) l))))
             (local-item ;обрабатывает <id>=<value>
              (lambda()
                (let ((item (list->symbol (token tail))))
                  (if (eq? item '||)
                     '()
                     (case item
                       ((sound)
                        (let* ((op (clear-ws tail))
                               (val (list->symbol (token (cdr op)))))
                          (if (and (eq? (car op) #\=)
                                   (or (eq? val 'true)
                                       (eq? val 'false)))
                              (cons (cons 'sound (if (eq? val 'true) #t #f))
                                    (local-item))
                              (err-exc #f))))
                       ((volume)
                        (let* ((op (clear-ws tail))
                               (val (list->number (token (cdr op)))))
                          (if (and (eq? (car op) #\=) val)
                              (cons (cons 'volume val)
                                    (local-item))
                           (err-exc #f))))
                       (else (err-exc #f)))))))
             (global-item ;обрабатывает [<global-id>]
              (lambda(l)
                (if (eq? (car l) #\[)
                    (let ((tok (token (cdr l)))
                          (end (clear-ws tail)))
                      (if (eq? (car end) #\])
                          (begin
                            (set! tail (cdr end))
                            (list->symbol tok))
                          (err-exc #f)))
                    (err-exc #f))))
             (token ;возвращает разрешенный <id> или number
              (lambda (l)
                (let loop ((l (clear-ws l)))
                  (if (pair? l)
                      (if (char? (car l))
                          (cond
                            ((or (char-alphabetic? (car l))
                                 (char-numeric? (car l))
                                 (memq (car l) '(#\- #\_ #\.)))
                            (cons (char-downcase (car l)) (loop (cdr l))))
                            (else (set! tail l) '()))
                          (err-exc #f))
                  (begin
                    (set! tail '()) l))))))
            (let* ((l (clear-ws char-lst))
                   (r
                     (if (eq? (car l) #\[)
                         (case (global-item l) ;обрабатываем найденный [<global-id>]
                           ((patch)
                            (cons (cons 'patch (list->string (char-line (clear-ws tail))))
                                  (conf-parser tail)))
                           ((log)
                            (cons (cons 'log (list->string (char-line (clear-ws tail))))
                                  (conf-parser tail)))
                           ((audio)
                            (cons (cons 'audio (local-item))
                                  (conf-parser tail)))
                           (else (err-exc #f)))
                         (err-exc #f))))
              (if (list? r) r #f)))
          char-lst))))

(define (text-file->char-lst fname)
  (let ((iport (open-input-file fname)))
    (let loop ((c (read-char iport)))
        (if (eof-object? c)
            (begin (close-input-port iport) '())
            (cons c (loop (read-char iport)))))))

(define (parse-conf-file fname)
  (conf-parser (text-file->char-lst fname)))

; test
;(conf-parser (string->list "[patch] \r ghhfbn \r\n [ log] dfg ghfg jrtyu\n [audio] sound=true volume=5"))
;(text-file->char-lst "c:\\conf.cfg")
;(conf-parser (text-file->char-lst "c:\\conf.cfg"))
(parse-conf-file "c:\\conf.cfg")

; conf.cfg
;
; [patch]
; C:\111\
; [log]
; c:\111\log.txt
; [audio]
; sound=true
; volume=5
;