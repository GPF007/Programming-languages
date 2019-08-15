#Racket#
##week 1##
Racket是一种动态函数式编程语言。由括号（）作为主要语法syntax。  

+ 定义变量与函数

	(define a 3)  
	(define (f a) (+ a 2))  
	(define f (lambda a) (- a 3)))

所有运算均有函数来表示（operand e1 e2 ... e2）

* 一些常用函数（关键词）
  
| 函数        | 描述           | 例子  |
| ------------- |:-------------:| -----:|
| null      | 返回一个空list | null |
| cons      | 连接list     |   (cons 2 null) |
| car  | 得到list中第一个元素      |  (car lst) |
| cdr  | 得到list中的后面| (cdr some-list)|
| null? | 判断一个list是否为空 | (null? some-list)|

* 动态类型  
  cond相当于switch操作 
 
		(cond [test1 exp1]
			  [test2 exp2]
	          ...)

* 局部变量绑定  

		(let ([x1 e1]
			 [x2 e2]
			 '''))
			e)

let可以替换成let* letrec。let中的环境为局部绑定语句块之前，let*包括语句块之内；letrec允许定义递归函数。

* 可变变量  
		（set! b a)  
b的值赋给a  
可用mcons代替cons连接可变变量。 

* 延迟执行(thunk)  

	(define (f x) (lambda () x+1))


只有在调用f时才会求值，传递f过程不会求值。

* 流  
流相当于一个无线长的list (1 2 3 4...)

		(define powers-of-two
		(letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
		(lambda () (f 2)))) 

* 备忘录  
可以使用memo存储之前计算出的结果，避免重复计算。

		(define fibonacci
		(letrec([memo null]
		[f (lambda (x)
		(let ([ans (assoc x memo)])
		(if ans
		(cdr ans)
		(let ([new-ans (if (or (= x 1) (= x 2))
		1
		(+ (f (- x 1))
		(f (- x 2))))])
		(begin
		(set! memo (cons (cons x new-ans) memo))
		new-ans)))))])
		f))


* 宏定义  

		(define-syntax my-if
		(syntax-rules (then else)
		[(my-if e1 then e2 else e3)
		(if e1 e2 e3)]))

不像C/C++语言中的宏直接替换代码，支持闭包求值等。


