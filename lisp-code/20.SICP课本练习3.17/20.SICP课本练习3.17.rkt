#lang racket
(require r5rs)

(define env (scheme-report-environment 5))
(eval  ;only after evaluating your count-pairs, the program in the input can use count-pairs
 '(define (count-pairs lst)
  (define (find element llst)
    (if (null? llst)
        #f
        (if (eq? element (car llst))
            #t
            (find element (cdr llst)))))
  (define mark '())
  (define (count llst)
    (if (not (pair? llst))
        0
        (if (find llst mark)
            0
            (begin
              (set! mark (cons llst mark))
              (+ (count (car llst))
                 (count (cdr llst))
                 1)))))
    (count lst))
 env)


      
(define (myloop)
  (define (eval-codes codes last-val)
    (if (null? codes)
        last-val
        (eval-codes (cdr codes) (eval (car codes) env))))
    
  (let ((codes (read)))
    (if (eq? codes eof)
        (void)
        (begin (displayln (eval-codes codes (void))) (myloop)))))


(myloop)
