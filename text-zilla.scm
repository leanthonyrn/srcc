;  This progam is a simple text-downloader.
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

(define (text-downloader url #!key save-as)
  (call/cc
    (lambda (return)
      (let* ((url-separate
               (lambda(u)
                 (let* ((ul (string->list u))
                        (h  (let loop ((l ul))
                              (cond
                                ((null? l) l)
                                ((eq? (car l) #\/) '())
                                (else (cons (car l) (loop (cdr l)))))))
                        (fp (memq #\/ ul)))
                   (if (eq? (car ul) #\/)
                       (return #f)
                       (cons (list->string h)
                             (if fp (list->string fp) "/"))))))
             (pair-url (if (string? url)
                           (url-separate url)
                           (return #f)))
             (host-port (open-tcp-client
                         (list server-address: (car pair-url)
                               port-number: 80)))
             (fport  (if (string? save-as)
                         (if (zero? (string-length save-as))
                             (current-output-port)
                             (open-output-file save-as))
                         (current-output-port))))
        (display
          (string-append
            "GET " (cdr pair-url) " HTTP/1.0\n"
            "Host: " (car pair-url) "\n"
            "User-Agent: FuckZilla\n"
            "\n")
          host-port)
        (force-output host-port)
        (let ((header
                (let ((s ""))
                  (do ((str (read-line host-port) (read-line host-port)))
                      ((cond
                         ((eof-object? str) #t)
                         ((string=? str "\r") #t)
                         (else #f))  )
                      (set! s (string-append s str)))
                      s)))
          (do ((str (read-line host-port) (read-line host-port)))
            ((eof-object? str) )
            (begin
              (display str fport)))
          (close-output-port fport)
          (close-port host-port)
          (display header))))))


;(text-downloader "www.lisp.ru/forums.php?m=posts&q=206&n=last#bottom" save-as: "c:\\index.html")
;(text-downloader "www.lisp.ru/forums.php" save-as: "c:\\forums.html")