#lang racket
(define (f a1 a2 a3 a4 a5)
  (+ a1 (* 4 a2) (* 5 a3) (* a4 a4 (- 2)) (* a5 a5 a5)))
(define (fact n)
  (define (fact-iter a1 a2 a3 a4 a5 count)
    (if (= count n)
        a1
        (fact-iter (f a1 a2 a3 a4 a5) a1 a2 a3 a4 (+ 1 count))))
  (if (< n 5)
      1
   (fact-iter 1 1 1 1 1 4)))
(define (input x)
  (if (eq? x eof)
      (void)
      (begin (display (fact x))(newline) (input (read)))))
(input (read))