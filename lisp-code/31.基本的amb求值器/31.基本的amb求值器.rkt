#lang racket
(require r5rs)
(define (my-eval exp env)
  ((analyze exp) env))
(define (ambeval exp env succeed fail)
((analyze exp) env succeed fail))
(define (analyze exp) ;返回值是一个可执行函数(不是源代码形式的也不是函数对象),以环境为参数。
  (cond ;((equal? exp '(cond ((and (> 5 3) (> 6 2) (* 3 4))))) (lambda (env) 12))
    ((self-evaluating? exp)
     (analyze-self-evaluating exp))
    ((quoted? exp) (analyze-quoted exp))
    ((variable? exp) (analyze-variable exp))
    ((assignment? exp) (analyze-assignment exp))
    ((definition? exp) (analyze-definition exp))
    ((and? exp) (analyze-and exp))
    ((or? exp) (analyze-or exp))
    ((if? exp) (analyze-if exp))
    ((lambda? exp) (analyze-lambda exp))
    ((let? exp) (analyze
                 (cons (make-lambda (let-variables exp) (let-body exp))
                       (let-parameters exp))))
    ((begin? exp) (analyze-sequence (begin-actions exp)))
    ((cond? exp) (analyze (cond->if exp)))
    ((amb? exp) (analyze-amb exp))
    ((application? exp) (analyze-application exp))
    (else (error "Unknown expression type -- ANALYZE" exp))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))
(define (analyze-amb exp) ;require中的(amb)也会经由这里来执行,其结果就是调用 fail
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail) ;分派函数
      (define (try-next choices)
        (if (null? choices)
            (fail);回溯。选择用完了也会走这里
            ((car choices) env ;(car choice)是分派函数
                           succeed
                           (lambda () ;新的失败函数 F1,是个闭包,可以选amb的下一个值
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;self-evaluation
(define (self-evaluating? exp)
  (cond ((number? exp) true) ;number?是scheme基本过程
        ((string? exp) true) ;string?是scheme基本过程
        (else false)))
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

;quote
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;variable
(define (variable? exp) (symbol? exp)) ;symbol?是scheme基本过程
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)));到外围环境继续找
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

;if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp)
  (if (null?(caddr exp))
      (if-predicate exp)
      (caddr exp)))
(define (if-alternative exp)
  (if (and (not (null? (cddr exp))) (not (null? (cdddr exp))))
      (cadddr exp)
      'false))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp))) ;pproc是分派函数
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail) ;分派函数 L7
      (pproc env ;pproc 以 (if-predicate exp)的值为参数去调用S3
             (lambda (pred-value fail2) ;成功函数, S3
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

;and,or
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (and-exps exp) (cdr exp))
(define (or-exps exp) (cdr exp))
(define (analyze-and exp)
  (cond ((null? (and-exps exp)) (lambda (env succeed fail) (succeed #t fail)))
        ((last-exp? (and-exps exp)) (analyze (first-exp (and-exps exp))))   
        (else (let ((condition (car (and-exps exp)))
                    (rest-and (cons 'and (cdr (and-exps exp))))
                    )
                (analyze (list 'if condition rest-and 'false))))))   
(define (analyze-or exp)
  (cond ((null? (or-exps exp)) (lambda (env succeed fail) (succeed #f fail)))
        ((last-exp? (or-exps exp)) (analyze (first-exp (or-exps exp))))
        (else (let ((condition (analyze (car (or-exps exp))))
                    (rest-or (cons 'or (cdr (or-exps exp))))
                    )
                (analyze (list 'if condition 'true rest-or))))))
;let
(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp) (map cadr (cadr exp)))
(define (let-variables exp) (map car (cadr exp)))
(define (let-body exp) (cddr exp)) ;body可能是个表达式序列

;assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2) ; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed (void)
                          (lambda () ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env) ;撤销赋值
                            (fail2))))) ;执行失败函数
             fail))))

;definition
(define (definition? exp)
  (tagged-list? exp 'define));exp形如(define ....)
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp) ;针对第一种形式
      (caadr exp)));针对第二种形式,此时变量名就是函数名
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp);针对第一种形式
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env) ;做好定义,然后再成功继续
               (succeed (void) fail2)) ;define表达式的返回值是 (void)
             fail))))

;cond
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp)) ;返回所有分支的列表
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp)) ;body可能是个表达式序列
(define (make-lambda parameters body) ;构造一个lambda表达式
  (cons 'lambda (cons parameters body)))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
;过程对象形如: '(procedure (x y) ((* x y) (+ x y)) env) env是指向环境的指针
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;function
(define (application? exp) (pair? exp))
;exp是函数调用表达式的前提下:
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;下面ops是操作数的列表
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (apply-primitive-procedure proc args) 
   (apply  
    (primitive-implementation proc) args))  

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    (lambda (arg fail2) ;成功函数K1
                      (get-args (cdr aprocs)
                                env
                                (lambda (args fail3) ;成功函数 K2
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2) ;成功函数 L
               (get-args aprocs ;proc是fproc在env中的值,是个函数对象
                         env
                         (lambda (args fail3) ;成功函数 S
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))
(define (execute-application proc args succeed fail)
  ;proc是函数对象,args是实际参数列表
  (cond ((primitive-procedure? proc)
        (let ((m (apply-primitive-procedure proc args)))
           (succeed m fail)
         ))
        ((compound-procedure? proc)
         ((procedure-body proc) ;是个分派函数
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)));判断seq里是否只有一个表达式
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq);把表达式列表变成一个表达式
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (analyze-sequence exps)
  (define (sequentially a b) ;a b都是分派函数
    (lambda (env succeed fail) ;分派函数L6
      (a env
         (lambda (a-value fail2) ;成功函数S2
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
        (void))
    (loop (car procs) (cdr procs))))


(define (my-square x ) (* x x))
;procedure
(define primitive-procedures ;预定义过程列表。预定义过程必须和scheme基本过程对应吗?
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'cadddr cadddr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +) 
        (list '* *) 
        (list '- -) 
        (list '/ /) 
        (list '< <) 
        (list '> >)
        (list '>= >=)
        (list '<= <=)
        (list '= =) 
        (list 'number? number?) 
        (list 'pair? pair?) 
        (list 'not not) 
        (list 'remainder remainder) 
        (list 'length  length)
        (list 'sqrt  sqrt)
        (list 'list  list)
        (list 'symbol? symbol?)
        (list 'eq? eq?)     
        (list 'append append)
        (list 'display  display)
        (list 'newline  newline)
        (list 'void void)
        (list 'my-square my-square)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
))
(define (primitive-procedure-names) ;预定义过程名字列表
  (map car
       primitive-procedures))
(define (primitive-procedure-objects) ;生成预定义过程的函数对象列表
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

;bool
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;frame and environment
(define (make-frame variables values)
  (cons variables values));框架形如 ((x y z) 1 2 3)
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (set-variable-value! var val env);仅被eval-assignment调用
  (define (env-loop env)
    (define (scan vars vals) ;frame形如:((a b c) 1 2 3)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env);仅被eval-definition 调用
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars);如果变量不存在,就加到env的第一个 frame里面
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;before eval settings
(define rq '(define (require p)
              (if (not p) (amb)(void))))
(define glb-succeed
  (lambda (val next)
    (void)))
(define glb-fail
  (lambda ()
    (display "no answer") (newline)))
(define glb-env (setup-environment))
(ambeval rq glb-env glb-succeed glb-fail)

;print
(define (driver-loop)
  (define (internal-loop try-again)
    (let ((input (read)))
      (if (eq? input eof)
          (void)
          (if (eq? input 'try-again)
              (try-again) ;输入为try-again时,会回溯到最后的amb表达式求另一个解
              (begin
                (ambeval input
                         glb-env
                         (lambda (val next-alternative);成功函数
                           ;next-alternative是失败函数
                           (user-print val)
                           (internal-loop next-alternative))
                         (lambda () ;失败函数
                           (display "no answer")(newline)
                           (driver-loop))))))))
  (internal-loop
   (lambda ()
     (display "no answer")(newline)
     (driver-loop))))
(define (user-print object)
   (if (compound-procedure? object)
       (display (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      (procedure-environment object)))
       (if (eq? object (void))
           object
           (begin (display object)(newline)))))
(driver-loop) 
