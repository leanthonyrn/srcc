#| 10.08.2010 11:45

Simple Guestbook in Racket and MySQL.

Copyright (c) 2010 Mikhail Mosienko <netluxe@gmail.com>

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

#lang racket

(require web-server/http/bindings
         (prefix-in mysql: (planet jaz/mysql:1:6)))

(provide interface-version
         start)

(define interface-version 'stateless)

;; Настройки пользователя бд
(define gdb-name "имя бд")
(define gdb-host "localhost") ; Хост бд, по умолчанию localhost
(define gdb-port 3306)
(define gdb-user "логин")
(define gdb-password "пароль")

(define max-posts-page 5) ; Количесво постов на странице
(define max-name-length 22) ; Длина имени
(define max-email-length 16) ; Длина email
(define max-msg-length 512) ; Длина сообщения

(define debug-mode #f) ; При отладки установить #t

;; Структура поста
(struct post (id name email msg))

;; Возвращает список постов для текущей страницы
(define (gb-posts gdb-connect
                  page
                  all-posts-count
                  max-posts-page)
  (let* [(start (- (add1 all-posts-count) (* page max-posts-page)))
         (end (- all-posts-count (* (sub1 page) max-posts-page)))
         (rows (mysql:result-set-rows
                (mysql:query
                 #:connection gdb-connect
                 (format "SELECT Id, Name, Email, Msg FROM Posts WHERE Id BETWEEN ~a AND ~a"
                         start end))))]
    (if (null? rows)
        '()
        (map (lambda (row)
               (let [(id (vector-ref row 0))
                     (name (vector-ref row 1))
                     (email (vector-ref row 2))
                     (msg (vector-ref row 3))]
                 (post id name email msg)))
             (reverse rows)))))

;; Возвращает количество постов
(define (gb-posts-count gdb-connect)
  (vector-ref (car (mysql:result-set-rows
                    (mysql:query #:connection gdb-connect "SELECT COUNT(Id) FROM Posts")))
              0))

;; Добавляет пост в бд
(define (add-gb-post gdb-connect
                     name
                     email
                     msg
                     max-name-length
                     max-email-length
                     max-msg-length)
  (let* [(del-apostrophe
          (lambda(txt max-length)
            (regexp-replace*
             #rx"'"
             (if ((string-length txt) . > . max-length)
                 (substring txt 0 max-length)
                 txt)
             "&'")))
         (format-name
          (lambda(txt max-length)
            (let [(s (string-join
                      (regexp-match*
                       #rx"[!-~а-яА-Я]+"
                       (del-apostrophe txt max-length))
                      " "))]
              (if (string=? s "") #f s))))
         (format-email
          (lambda(txt max-length)
            (string-append*
             (regexp-match*
              #rx"[!-~а-яА-Я]+"
              (del-apostrophe txt max-length)))))
         (format-msg
          (lambda(txt max-length)
            (let [(s (del-apostrophe msg max-msg-length))]
              (if (regexp-match? #rx"[!-~а-яА-Я]+" s) s #f))))
         (name (format-name name max-name-length))
         (email (format-email email max-email-length))
         (msg (format-msg msg max-msg-length))]
    (if (name . and . msg)
        (mysql:query
         #:connection gdb-connect
         (format "INSERT INTO Posts (Name, Email, Msg) VALUES ('~a', '~a', '~a')"
                 name email msg))
        #f)))

;; Устанавливает соединение с сервером бд, и возвращает идентификатор открытого соединения.
(define (connect-gdb gdb-name
                     host
                     port
                     login
                     password)
  (let ([gdb-connect (mysql:connect host port login password)])
    (mysql:query (format "USE ~a" gdb-name))
    (mysql:query "CREATE TABLE IF NOT EXISTS Posts (Id INTEGER AUTO_INCREMENT PRIMARY KEY, Name TEXT, Email TEXT, Msg TEXT)")
    gdb-connect))

;; Закрывает соединение с сервером бд
(define (close-gdb gdb-connect)
  (mysql:close-connection! gdb-connect))

;; Создает страницу
(define (render-guestbook-page posts
                               all-posts-count
                               max-posts-page
                               page)
  (let* [(pages-count (if (exact-integer? (/ all-posts-count max-posts-page))
                          (/ all-posts-count max-posts-page)
                          (add1 (quotient all-posts-count max-posts-page))))
         (parse-text
          (lambda(str)
            (append* (map (lambda(s) `(,s (br))) ; А как же <pre>?
                          (regexp-split #rx"\n+" str)))))
         (pages-switcher
          (lambda(page
                  pages-count
                  switch-pages-count)
            (let [(switcher-pages-count
                   (if (> page
                          (* (quotient pages-count switch-pages-count)
                             switch-pages-count))
                       (modulo pages-count switch-pages-count)
                       switch-pages-count))]
              `("Страница: "
                ,@(if (page . > . switch-pages-count)
                      `((a ((href ,(string-append
                                    "guestbook.rkt?page="
                                    (number->string
                                     (* (quotient (sub1 page) switch-pages-count)
                                        switch-pages-count)))))
                           "<<") " ")
                      '(""))
                ,@(append*
                   (build-list
                    switcher-pages-count
                    (lambda(n)
                      (let [(curr-page
                             (lambda (n)
                               (+ (add1 n)
                                  (* (quotient (sub1 page) switch-pages-count)
                                     switch-pages-count))))]
                        `(,(if ((curr-page n) . = . page)
                               (number->string page)
                               `(a ((href ,(string-append
                                            "guestbook.rkt?page="
                                            (number->string (curr-page n)))))
                                   ,(number->string (curr-page n))))
                          ,(if ((add1 n) . = . switcher-pages-count) "" ", "))))))
                ,@(if (< (* (add1 (quotient (sub1 page) switch-pages-count))
                            switch-pages-count)
                         pages-count)
                      `(" " (a ((href ,(string-append
                                        "guestbook.rkt?page="
                                        (number->string
                                         (add1 (* (add1 (quotient (sub1 page) switch-pages-count))
                                                  switch-pages-count))))))
                               ">>"))
                      '(""))))))
         (render-post
          (lambda (pst)
            `(div ((class "post"))
                  (table ((class "msg-table") (width "100%"))
                         (tr ((class "msg-table row"))
                             (td ((class "msg-table head") (colspan "2"))
                                 (p ((class "msg-table head left-side"))
                                    (b ((class "msg-table head left-side id"))
                                       ,(string-append "#"
                                                       (number->string (post-id pst))
                                                       " "))
                                    (b ,(post-name pst))
                                    ,@(if (string=? (post-email pst) "")
                                          '("")
                                          `(" (" (a ((href ,(string-append
                                                             "mailto:"
                                                             (post-email pst))))
                                                    ,(post-email pst))
                                                 ")")))))
                         (tr ((class "msg-table row"))
                             (td ((class "msg-table body"))
                                 ,@(parse-text (post-msg pst))))))))
         (render-posts
          (lambda(posts)
            `(div ,@(map render-post posts))))]
    `(html (head (title "Гостевая")
                 (meta ((http-equiv "content-type")
                        (content "text/html; charset=UTF-8"))))
           (style ((type "text/css"))
                  "BODY {color: black; background: white; } "
                  ".main-table { } "
                  ".post { border-bottom: 10px solid white; } "
                  ".pages { background-color: beige; } "
                  ".msg-table { border-collapse: collapse; } "
                  ".msg-table.row { border: 2px solid white; } "
                  ".msg-table.head { background-color: gainsboro; } "
                  ".msg-table.head.left-side { text-align: left; } "
                  ".msg-table.head.left-side.id { color: teal; } "
                  ".msg-table.body { background-color: lightyellow; }")
           (body
            (table ((class "main-table") (align "center"))
                   (tr (td ((align "center")) (h1 "Гостевая")))
                   (tr (td (form ((method "post"))
                                 (table ((align "left"))
                                        (tr (th ((align "right")) "Имя:")
                                            (td (input ((name "name")
                                                        (maxlength "22")))))
                                        (tr (th ((align "right")) "Email:")
                                            (td (input ((name "email")
                                                        (maxlength "16")))))
                                        (tr (th ((align "right")) "Сообщение:")
                                            (td (textarea ((name "msg")
                                                           (rows "5")
                                                           (cols "50")) "")))
                                        (tr (td)
                                            (td (input ((type "submit")
                                                        (value "Добавить")))))))))
                   (tr (td ((class "pages") (align "right"))
                           ,@(pages-switcher page pages-count 5)))
                   (tr (td ,(render-posts posts))))))))

;; Проверяет корректность поста
(define (is-post? bindings)
  (and (exists-binding? 'name bindings)
       (exists-binding? 'email bindings)
       (exists-binding? 'msg bindings)
       (not (string=? (extract-binding/single 'name bindings) ""))
       (not (string=? (extract-binding/single 'msg bindings) ""))))

;; Вычисляет номер текущей страницы
(define (gb-page bindings posts-count max-posts-page)
  (if (exists-binding? 'page bindings)
      (let [(n (string->number (extract-binding/single 'page bindings)))]
        (if (exact-positive-integer? n)
            (let [(pages-count
                   (if (exact-integer? (/ posts-count max-posts-page))
                       (/ posts-count max-posts-page)
                       (add1 (quotient posts-count max-posts-page))))]
              (if (n . <= . pages-count) n 1))
            1))
      1))

(define (refresh-guestbook)
  '(html (head (meta ((http-equiv "refresh")
                      (content "0;URL=guestbook.rkt?page=1")))))) ; необходимо исправить значение URL

(define (render-error-page exc)
  (if debug-mode
      exc
      `(html (head (title "Ошибка")
                   (meta ((http-equiv "content-type")
                          (content "text/html; charset=UTF-8"))))
             (body (b "Произошла непредвиденная ошибка!")))))

(define (start req)
  (with-handlers ([any/c render-error-page])
    (let [(gdb-connect (connect-gdb gdb-name gdb-host gdb-port gdb-user gdb-password))
          (bindings (request-bindings req))]
      (if (is-post? bindings)
          [begin
            (add-gb-post gdb-connect
                         (extract-binding/single 'name bindings)
                         (extract-binding/single 'email bindings)
                         (extract-binding/single 'msg bindings)
                         max-name-length
                         max-email-length
                         max-msg-length)
            (refresh-guestbook)]
          (let* [(posts-count (gb-posts-count gdb-connect))
                 (page (gb-page bindings posts-count max-posts-page))
                 (html-guestbook-page
                  (render-guestbook-page (gb-posts gdb-connect page posts-count max-posts-page)
                                         posts-count
                                         max-posts-page
                                         page))]
            (close-gdb gdb-connect)
            html-guestbook-page)))))
