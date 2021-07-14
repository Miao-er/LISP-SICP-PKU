#lang racket

(define exit #f)

(define cont-list '())
(define len (read))


(define (set-cont-list n)
  (set! cont-list
        (map (lambda (i) (call i) mycc)
               ((lambda (x)
                 (define (iter y lst)
                   (if (= y 0)
                       lst
                       (iter (- y 1) (cons y lst)) ))
                 (iter x '())) n))))
(define mycc #f)
(define (call n)
  (if (call/cc (lambda (i) (set! mycc i) #t))
      (void)
      (print n)))
(define (print n)
  (if (= n 0)
      (void)
      (begin
        (displayln n)
        (print (- n 1)))))
               
(define (show n)
  (define (show-helper l n)
    (if (= n 0)
        (if (continuation? (car l))
            ((car l) #f)
            (displayln "error"))
        (show-helper (cdr l) (- n 1))))
  (show-helper cont-list (- n 1)))

(define (main)
  (set-cont-list len)
  (define k (read))
  (if (eq? k eof)
      (void)
      (show k)))

(main)