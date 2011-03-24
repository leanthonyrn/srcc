#|
Simple TCP/IP Port Scanner v0.1

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

#lang racket

(provide tcp-open-ports)

(define (tcp-open-ports host from to 
                        #:max-threads [max-threads 50]
                        #:wait-sec [wait-sec 2])
  (when (> from to)
    (set! to from))
  (when (< max-threads 10)
    (set! max-threads 10))
  ;(when (> max-threads 100)
  ;  (set! max-threads 100))
  (when (<= wait-sec 0)
    (set! wait-sec 0.5))
  (letrec 
      ([cust (make-custodian)]
       [result null]
       [get-port
        (let ([port (sub1 from)]
              [sem (make-semaphore 1)])
          (lambda()
            (semaphore-wait sem)
            (set! port (add1 port))
            (semaphore-post sem)
            port))]
       [work
        (lambda()
          (let ([port (get-port)])
            (when (<= port to)
              (let ([thd 
                     (thread 
                      (lambda() 
                        (with-handlers ([exn:fail:network? void])
                          (let-values ([(in out) 
                                        (tcp-connect host port)])
                            (close-output-port out)
                            (close-input-port in)
                            (set! result (cons port result))))))])
                (unless (sync/timeout wait-sec thd)
                  (kill-thread thd))
                (work)))))]
       [threads 
        (parameterize ([current-custodian cust])
          (custodian-limit-memory cust (* 2 1024 1024))
          (build-vector
           (if (> max-threads 
                  (add1 (- to from)))
               (add1 (- to from))
               max-threads)
           (lambda(n) (thread work))))])
    (do() [(zero? (vector-count thread-running? threads))
           (custodian-shutdown-all cust)
           (sort result <)]
      (sleep 0.1))))

;(time (tcp-open-ports "81.177.159.170" 41000 42000 #:max-threads 100 #:wait-sec 1))