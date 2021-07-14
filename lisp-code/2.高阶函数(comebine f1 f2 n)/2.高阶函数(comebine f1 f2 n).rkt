#lang racket

(define (square x) (* x x))
(define (inc x) (+ x 1))
(define (db x) (* x 2))

(define (combine f1 f2 n)
(define (fun) (lambda (x) (f1 (f2 x)))) 
(define (count n) 
	(if(= n 0)
	(lambda (x) x)
	(if(= n 1)
		(lambda (x) ((fun) x))
		(lambda (x) ((fun) ((count (- n 1)) x))))))
(count n))
((combine square inc 1) 2)
((combine square inc 2) 3)
((combine db inc 3) 2)
((combine inc inc 4) 3)

(display "********") (newline)

(define (myloop)
  (let ((n (read))
        (x (read)))
    (if (eq? n eof)
        (void)
        (begin (display ((combine inc square n) x)) 
               (newline) (myloop)))))

(myloop)