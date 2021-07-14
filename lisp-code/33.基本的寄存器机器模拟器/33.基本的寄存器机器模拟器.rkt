#lang racket
(require r5rs)

(define (make-machine register-names ops controller-text)
  ;参数分别为寄存器列表、操作列表和指令序列
  (let ((machine (make-new-machine)));machine开始是空机器
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine)) ;安装指令序列
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc)) ;名字 'pc 'flag没用
        ; (make-register的结果是个闭包,内部有状态contents,放着寄存器的值。
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()));机器的指令序列
    (let ((the-ops ;操作列表
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name) ;添加一个寄存器表项。表项是(名字 寄存器)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name);根据名字查找寄存器
        (let ((val (assoc name register-table)))
          (if val
              (cadr val) ;返回结果是个闭包,代表寄存器。 val形如 ('pc pc)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)));insts是指令序列后缀
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                ;(car insts)就是指令序列后缀中的第一条指令
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute)) ;pc指向执行指令序列的开头,从pc开始执行
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ;安装分析完成后的指令序列seq,在make-machine中用到
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations) ;make-machine中用
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start)) ;让机器开始运行
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (get-register machine reg-name)
  ;返回值是个register,就是个闭包,里面只有一个 content状态变量
  ((machine 'get-register) reg-name))

;register
(define (make-register name) ;name没用
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))
(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

;stack
(define (make-stack)
  (let ((s '())) ;s就是栈
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '()) 'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK" message))))
    dispatch))
(define (pop stack)
  (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

;assemble
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels) ;LBD
                    (update-insts! insts labels machine)
                    insts)))
(define (extract-labels text receive) ;只在assemble中被调用
  (if (null? text)
      (receive '() '());
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst) ;为true说明是label,如 'gcd-done
                              (receive insts
                                       (cons (make-label-entry next-inst
                                                               insts)
                                             labels))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))
(define (update-insts! insts labels machine)
  ;为insts中的每条指令添加可执行过程, insts本来是文本形式的指令序列
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)));拿到上面这些东西的指针
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst ;为每条指令加上分析后得到的可执行过程
        (make-execution-procedure ;分析指令的结果是产生一个可执行过程
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts))) ;返回分析后的指令序列

;instruction
(define (make-instruction text)
  ;text是指令的文字形式。本函数创建一个只有文字形式的指令
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))
;参数inst形如: ((test (op =) (reg b) (const 0)))
;执行之后指令变成了一个序对(非列表),形如:((test (op =) (reg b) (const 0)) . <#procedureXXX>)

;label
(define (make-label-entry label-name insts)
  (cons label-name insts))
;insts是机器里指令序列的一个后缀(从指令序列的开头或中间开始,直到结尾的子序列)
;返回值是一个label-entry,形如(L1 (assign...) (test ....) (branch ...)...)
;label-entry的cdr部分就是指令序列的一个后缀, L1是标号label-name
(define (lookup-label labels label-name)
  ;labels里每个元素都是 label-entry
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))
;返回值就是一个指令序列后缀。表示从label-name开始往后的所有指令

;execution
(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                      inst))))

;primitive evalution
(define (make-primitive-exp exp machine labels) ;返回对基本表达式求值的函数
  ;primitive exp形如: (reg b) 或 (const 3) 或 (label thing-done)
  ;返回值一定是个过程,执行该过程,得到exp的值。如果exp是个标号,则返回该标号代表的指令序列后缀
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels ;lookup-label的返回值是一个指令序列后缀
                              (label-exp-label exp))))
           (lambda () insts))) ;
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r)))) ;该lbd返回寄存器的值
        (else (error "Unknown expression type -- ASSEMBLE" exp))))
(define (tagged-list? exp tag)
  (eq? (car exp) tag))
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp)) ;取寄存器名
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp)) ;取常数
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp)) ;取标号名

;operations
;形如:(('+ +) ('< <) (initialize-stack (lambda () (stack 'initialize))))
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        ;op是操作所对应的可执行过程
        (aprocs
         (map (lambda (e) ;e形如: (reg a)、 (const 3)、 (lable done)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))
(define (lookup-prim symbol operations);查找操作对应的可执行过程
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val) ;返回操作所对应的可执行过程
        (error "Unknown operation -- ASSEMBLE" symbol))))


;assign execution
(define (make-assign inst machine labels operations pc)
  ;inst 形如: (assign n (reg b)) n是寄存器名
  (let ((target ;target是寄存器,相当于 n
         (get-register machine (assign-reg-name inst))) ;取得n
        (value-exp (assign-value-exp inst))) ;value-exp 形如 ((reg b))
    (let ((value-proc ; (value-proc)是寄存器应被赋予的值
           (if (operation-exp? value-exp)
               ;value-exp形如((op rem))则是 operation-exp
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp ;此时(car value-exp)形如 (reg b)
                (car value-exp) machine labels))))
      (lambda () ;assign指令对应的可执行过程
        (set-contents! target (value-proc))
        ;set-contents!设置寄存器target的值为 (value-proc)
        (advance-pc pc)))));程序计数器向前推进即pc.content=(cdr pc.content)
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))
(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;test execution
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    ;inst 形如 (test (op =) (reg n) (const 1))
    (if (operation-exp? condition)
        ;condition 形如 ((op =) (reg n) (const 1))
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

;branch execution
(define (make-branch inst machine labels flag pc)
  ;branch 形如 (branch (label base-case))
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts ;insts是指令序列后缀
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;goto execution
(define (make-goto inst machine labels pc)
  ;inst形如(goto (reg continue))或 (goto (label fact-loop))
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;save restore
(define (make-save inst machine stack pc) ;inst形如 (save n)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc) ;inst形如 (restore n)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction)) ; stack-instruction形如 (restore n)

;perform execution
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst) (cdr inst))

(define (set-regs-value machine regs)
  (if (null? regs)
      (void)
      (begin (set-register-contents! machine (car (car regs)) (cadr (car regs)))
             (set-regs-value machine (cdr regs)))))

(define (get-regs-value machine regs)
  (if (null? regs)
      (newline)
      (begin (display (get-register-contents machine (car regs)))
             (display " ")
             (get-regs-value machine (cdr regs)))))

(define primitive-list
  (list
   (list 'remainder remainder)
   (list '= =)
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '/ /)
   (list '> >)
   (list '< <)
   (list 'eq? eq?)))
   
(define (driver cur-machine)
  (let ((input (read)))
    (if (eq? input eof)
        (void)
        (if (eq? (car input) 'make-machine)
            (let ((reg-list (cadr input))
                  (op-list (caddr input))
                  (inst-list (cadddr input)))
              (set! cur-machine (make-machine
                                 (cadr reg-list)
                                 (map (lambda (op)
                                        (list (cadadr op)
                                              (let ((val (assoc (caddr op) primitive-list)))
                                                (if val
                                                    (cadr val)
                                                    (error "can not find this op" (cadadr op)))
                                                )))
                                      (cdr op-list))
                                 (cadr inst-list)))
              (displayln "a new machine")
              (driver cur-machine))
            (begin
              (set-regs-value cur-machine input)
              (let ((output-reg (read)))
                (start cur-machine)
                (get-regs-value cur-machine output-reg)
                (driver cur-machine)))))))
(driver 'empty)
            
            
        
    
    
          
    