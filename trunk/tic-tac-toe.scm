;  tic-tac-toe.ss
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

(define (tic-tac-toe)
  (letrec ((area
            '((0-0 . #f) (0-1 . #f) (0-2 . #f)
                         (1-0 . #f) (1-1 . #f) (1-2 . #f)
                         (2-0 . #f) (2-1 . #f) (2-2 . #f)))
           (figure?
            (lambda(f)
              (if (or (eq? f 0)
                      (eq? f 'x)) #t #f)))
           (position?
            (lambda(p)
              (let ((pos (assq p area)))
                (if (and pos
                         (not (cdr pos))) p #f))))
           (clear-area
            (lambda()
              (for-each
               (lambda(p)
                 (set-cdr! p #f))
               area)))
           (user-figure #f)
           (cpu-figure #f)
           (end-game?
            (lambda(v)
              (cond
                ((and (eq? (cdr (assq '0-0 area)) v)
                      (eq? (cdr (assq '0-1 area)) v)
                      (eq? (cdr (assq '0-2 area)) v)) #t)
                ((and (eq? (cdr (assq '1-0 area)) v)
                      (eq? (cdr (assq '1-1 area)) v)
                      (eq? (cdr (assq '1-2 area)) v)) #t)
                ((and (eq? (cdr (assq '2-0 area)) v)
                      (eq? (cdr (assq '2-1 area)) v)
                      (eq? (cdr (assq '2-2 area)) v)) #t)
                ((and (eq? (cdr (assq '0-0 area)) v)
                      (eq? (cdr (assq '1-0 area)) v)
                      (eq? (cdr (assq '2-0 area)) v)) #t)
                ((and (eq? (cdr (assq '0-1 area)) v)
                      (eq? (cdr (assq '1-1 area)) v)
                      (eq? (cdr (assq '2-1 area)) v)) #t)
                ((and (eq? (cdr (assq '0-2 area)) v)
                      (eq? (cdr (assq '1-2 area)) v)
                      (eq? (cdr (assq '2-2 area)) v)) #t)
                ((and (eq? (cdr (assq '0-0 area)) v)
                      (eq? (cdr (assq '1-1 area)) v)
                      (eq? (cdr (assq '2-2 area)) v)) #t)
                ((and (eq? (cdr (assq '0-2 area)) v)
                      (eq? (cdr (assq '1-1 area)) v)
                      (eq? (cdr (assq '2-0 area)) v)) #t)
                (else #f))))
           (set-user-figure
            (lambda()
              (display "Выберите крестик или нолик (x или 0): ")
              (let ((f (read)))
                (if (figure? f)
                    (set! user-figure f)
                    (begin
                      (newline)
                      (set-user-figure))))))
           (set-cpu-figure
            (lambda()
              (if (eq? user-figure 'x)
                  (set! cpu-figure 0)
                  (set! cpu-figure 'x))))
           (get-move
            (lambda()
              (display "Ваш ход: ")
              (let ((p (read)))
                (if (position? p)
                    (set-cdr! (assq p area) user-figure)
                    (begin
                      (newline)
                      (get-move))))))
           (print-cage
            (lambda()
              (let ((get-value
                     (lambda(p)
                       (let ((val (cdr (assq p area))))
                         (if val val #\space)))))
                (display "  0 1 2\n")
                (display "0 ") (display (get-value '0-0)) (display "|")
                (display (get-value '0-1)) (display "|") (display (get-value '0-2))
                (newline)
                (display " -------") (newline)
                (display "1 ") (display (get-value '1-0)) (display "|")
                (display (get-value '1-1)) (display "|") (display (get-value '1-2))
                (newline)
                (display " -------") (newline)
                (display "2 ") (display (get-value '2-0)) (display "|")
                (display (get-value '2-1)) (display "|") (display (get-value '2-2))
                (newline))))
           (find-move
            (lambda()
              (let ((lines
                     (list
                      (cons
                       (list '0-0 '0-1 '0-2)
                       (list (cdr (assq '0-0 area)) (cdr (assq '0-1 area)) (cdr (assq '0-2 area))))
                      (cons
                       (list '1-0 '1-1 '1-2)
                       (list (cdr (assq '1-0 area)) (cdr (assq '1-1 area)) (cdr (assq '1-2 area))))
                      (cons
                       (list '2-0 '2-1 '2-2)
                       (list (cdr (assq '2-0 area)) (cdr (assq '2-1 area)) (cdr (assq '2-2 area))))
                      (cons
                       (list '0-0 '1-0 '2-0)
                       (list (cdr (assq '0-0 area)) (cdr (assq '1-0 area)) (cdr (assq '2-0 area))))
                      (cons
                       (list '0-1 '1-1 '2-1)
                       (list (cdr (assq '0-1 area)) (cdr (assq '1-1 area)) (cdr (assq '2-1 area))))
                      (cons
                       (list '0-2 '1-2 '2-2)
                       (list (cdr (assq '0-2 area)) (cdr (assq '1-2 area)) (cdr (assq '2-2 area))))
                      (cons
                       (list '0-0 '1-1 '2-2)
                       (list (cdr (assq '0-0 area)) (cdr (assq '1-1 area)) (cdr (assq '2-2 area))))
                      (cons
                       (list '2-0 '1-1 '0-2)
                       (list (cdr (assq '2-0 area)) (cdr (assq '1-1 area)) (cdr (assq '0-2 area)))))))
                (call-with-current-continuation
                 (lambda (return)
                   ; check win positions
                   (for-each
                    (lambda(l)
                      (cond
                        ((equal? (cdr l) `(,cpu-figure ,cpu-figure #f))
                         (return (caddr (car l))))
                        ((equal? (cdr l) `(,cpu-figure #f ,cpu-figure))
                         (return (cadr (car l))))
                        ((equal? (cdr l) `(#f ,cpu-figure ,cpu-figure))
                         (return (car (car l))))))
                    lines)
                   ; check user positions
                   (for-each
                    (lambda(l)
                      (cond
                        ((equal? (cdr l) `(,user-figure ,user-figure #f))
                         (return (caddr (car l))))
                        ((equal? (cdr l) `(,user-figure #f ,user-figure))
                         (return (cadr (car l))))
                        ((equal? (cdr l) `(#f ,user-figure ,user-figure))
                         (return (car (car l))))))
                    lines)
                   (if (equal? (cdr (assoc '(0-0 0-1 0-2) lines)) `( #f ,user-figure #f))
                       (cond
                         ((equal? (cdr (assoc '(0-0 1-0 2-0) lines)) `( #f ,user-figure #f))
                          (return '0-0))
                         ((equal? (cdr (assoc '(0-2 1-2 2-2) lines)) `( #f ,user-figure #f))
                          (return '0-2))))
                   (if (equal? (cdr (assoc '(0-0 1-0 2-0) lines)) `( #f ,user-figure #f))
                       (cond
                         ((equal? (cdr (assoc '(0-0 0-1 0-2) lines)) `( #f ,user-figure #f))
                          (return '0-0))
                         ((equal? (cdr (assoc '(2-0 2-1 2-2) lines)) `( #f ,user-figure #f))
                          (return '2-0))))
                   (if (equal? (cdr (assoc '(2-0 2-1 2-2) lines)) `( #f ,user-figure #f))
                       (cond
                         ((equal? (cdr (assoc '(0-2 1-2 2-2) lines)) `( #f ,user-figure #f))
                          (return '2-2))
                         ((equal? (cdr (assoc '(0-0 1-0 2-0) lines)) `( #f ,user-figure #f))
                          (return '2-0))))
                   (if (equal? (cdr (assoc '(0-2 1-2 2-2) lines)) `( #f ,user-figure #f))
                       (cond
                         ((equal? (cdr (assoc '(2-0 2-1 2-2) lines)) `( #f ,user-figure #f))
                          (return '2-2))
                         ((equal? (cdr (assoc '(0-0 0-1 0-2) lines)) `( #f ,user-figure #f))
                          (return '0-2))))
                   ; check second positions
                   (for-each
                    (lambda(l)
                      (cond
                        ((equal? (cdr l) `(,cpu-figure #f #f))
                         (return (caddr (car l))))
                        ((equal? (cdr l) `(#f #f ,cpu-figure))
                         (return (car (car l))))
                        ((equal? (cdr l) `(#f ,cpu-figure #f))
                         (return (car (car l))))))
                    lines)
                   ; find empty position
                   (if (not (cdr (assq '1-1 area)))
                       (return '1-1))
                   (for-each
                    (lambda(l)
                      (if (not (cdr l))
                          (return (car l))))
                    area)
                   #f)))))
           (game
            (lambda()
              (call-with-current-continuation
               (lambda (return)
                 (if (or (end-game? cpu-figure)
                         (end-game? user-figure))
                     (return))
                 (if (eq? user-figure 'x)
                     (begin
                       (print-cage)
                       (get-move)
                       (if (end-game? user-figure)
                           (return)
                           (let ((m (find-move)))
                             (if m
                                 (set-cdr! (assq m area) cpu-figure)
                                 (return)))))
                     (begin
                       (set-cdr! (assq (find-move) area) cpu-figure)
                       (print-cage)
                       (if (or (end-game? cpu-figure)
                               (not (find-move)))
                           (return)
                           (get-move))))
                 (game)))))
           )
    (clear-area)
    (set-user-figure)
    (set-cpu-figure)
    (display "Ваш ход имеет вид {№ строки}-{№ столбца}\nНапример, 1-2, 1-1, 2-0.\n")
    (game)
    (display "------------------------\n")
    (print-cage)
    (cond
      ((end-game? cpu-figure)
       (display "Вы проиграли!"))
      ((end-game? user-figure)
       (display "ВЫ ПОБЕДИЛИ!"))
      (else
       (display "НИЧЬЯ!")))))

;(tic-tac-toe)