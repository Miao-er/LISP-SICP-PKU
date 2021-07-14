#lang racket
(require r5rs)
(define (my-eval exp env)
  (cond  ((eq? exp '()) (void))
         ((self-evaluating? exp) exp);自求值表达式
         ((variable? exp) (lookup-variable-value exp env))
         ((quoted? exp) (text-of-quotation exp));单引号表达式
         ((assignment? exp) (eval-assignment exp env));赋值语句
         ((definition? exp) (eval-definition exp env));特殊形式define
         ((if? exp) (eval-if exp env))
         ((and? exp) (eval-and (and-exps exp) env))
         ((or? exp) (eval-or (or-exps exp) env))
         ((while? exp) (run-while exp env))
         ((break? exp) 'break)
         ((switch? exp) (my-eval (switch->cond exp) env))
         ((for? exp) (my-eval (for->while exp) env))
         ((lambda? exp)
          (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env));生成过程对象
         ((begin? exp)
          (eval-sequence (begin-actions exp) env))
         ((cond? exp) (my-eval (cond->if exp) env));cond转换为if
         ((let? exp) (my-apply
                      (my-eval (make-lambda (let-variables exp) (let-body exp)) env)
                      (list-of-values (let-parameters exp) env)))
         ((application? exp);除了上面各种情况之外的,都认为是函数调用表达式
          (my-apply (my-eval (operator exp) env)
                    (list-of-values (operands exp) env)))
         (else
          (error "Unknown expression type -- EVAL" exp))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply (primitive-implementation procedure) arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        ;每执行一次函数调用,哪怕是递归的,都要新建一个环境,记录本次函数调用的参数的值
        (else
         (error "unkonwn procedure type -- APPLY" procedure))))

(define (self-evaluating? exp)
  (cond ((number? exp) true) ;number?是scheme基本过程
        ((string? exp) true) ;string?是scheme基本过程
        (else false)))

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

(define (quoted? exp)
  (tagged-list? exp 'quote))
;输入中的单引号开头的表达式会被Racket自动转换成 (quote ...)列表形式
;输入 'ba Racket的(read)读进来的是 (quote a)
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

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
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (my-eval (definition-value exp) env)
    env))

;assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (my-eval (assignment-value exp) env)
                       env))

;lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp)) ;body可能是个表达式序列
(define (make-lambda parameters body) ;构造一个lambda表达式
  (cons 'lambda (cons parameters body)))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
;过程对象形如: '(procedure (x y) ((* x y) (+ x y)) env) env是指向环境的指针
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;let
(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp) (map cadr (cadr exp)))
(define (let-variables exp) (map car (cadr exp)))
(define (let-body exp) (cddr exp)) ;body可能是个表达式序列


;if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (eval-if exp env)
  (if (true? (my-eval (if-predicate exp) env))
      (my-eval (if-consequent exp) env)
      (my-eval (if-alternative exp) env)))

;and,or
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (and-exps exp) (cdr exp))
(define (or-exps exp) (cdr exp))
(define (eval-and exp env)
  (cond ((null? exp) #t)
        ((last-exp? (first-exp exp)) (my-eval (first-exp exp) env))
        ((true? (my-eval (first-exp exp) env)) (eval-and (rest-exps exp) env))
        (else #f)))
(define (eval-or exp env)
  (cond ((null? exp) #f)
        ((last-exp? (first-exp exp)) (my-eval (first-exp exp) env))
        ((false? (my-eval (first-exp exp) env)) (eval-or (rest-exps exp) env))
        (else #t)))
                                                  
;while
(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))
(define (run-while exp env)
  (let ((predicate (while-predicate exp))
        (body (while-body exp)))
    (define (iter env)
      (if (true? (my-eval predicate env))
            (if (eq? (eval-sequence body env) 'break)
                (void)
                (iter env))
          (void)))
      (iter env)))

;for
(define (for? exp)  (tagged-list? exp 'for))
(define (for-init exp) (cadr exp))
(define (for-predicate exp) (caddr exp))
(define (for-step exp) (cadddr exp))
(define (for-body exp) (cddddr exp))
(define (for->while exp) (list 'begin
                               (for-init exp)
                               (cons 'while
                                     (cons (for-predicate exp)
                                               (append (for-body exp) (list (for-step exp)))))))

;break
(define (break? exp) (tagged-list? exp 'break))

;switch
(define (switch? exp) (tagged-list? exp 'switch))
(define (switch-body exp) (map
                           (lambda (x)
                             (if (eq? (car x) 'default)
                                 (cons 'else (cdr x))
                                 (cons (list '= (cadr exp) (car x)) (cdr x))))
                           (cddr exp)))
(define (switch->cond exp) (cons 'cond (switch-body exp)))
        
;begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
;下面seq是一个列表,每个元素都是exp
(define (last-exp? seq) (null? (cdr seq)));判断seq里是否只有一个表达式
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq);把表达式列表变成一个表达式
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (my-eval (first-exp exps) env))
        (else
         (if (eq? (my-eval (first-exp exps) env) 'break)
             'break
             (eval-sequence (rest-exps exps) env)))))

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

;function
(define (application? exp) (pair? exp))
;exp是函数调用表达式的前提下:
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;下面ops是操作数的列表
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

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
        (list 'displayln displayln)
        (list 'newline  newline)
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

(define (list-of-values exps env)  
  (if (no-operands? exps)
      '()
      (cons (my-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

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
(define glb-env (setup-environment)) ;初始的全局环境

;print
(define (driver-loop) 
  (let ((input (read))) 
    (if (eq? input eof)
        (void)
        (let ((output (my-eval input glb-env)))
          (user-print output)
          (driver-loop)))))
(define (user-print object)
   (if (compound-procedure? object)
       (display (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
       (if (eq? object (void))
           object
           (begin (display object)(newline)))))
(driver-loop) 