
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;(low,high,stride) -> list()
;;return [low ,high] which interval is stride
(define (sequence low high stride)
  (cond [(> low high) (list)]
        [#t (cons low (sequence (+ low stride) high stride))]))

;string list*string ->string list
(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs))

;int list*int -> int
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: emptynumber")]
        [#t (letrec ([f (lambda(xs i) (if (= i 0)
                                       (car xs)
                                       (f (cdr xs) (- i 1))))])
              (f xs (remainder n (length xs))))]))

;;test-function
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

;string*n ->list
(define (stream-for-n-steps s n)
  (letrec([f (lambda(ss i) (if (> i n)
                            (list)
                            (cons (car (ss)) (f (cdr(ss)) (+ i 1)))))])
    (f s 1)))

;funny stream
(define funny-number-stream
  (letrec([f (lambda (x) (cons (if (= (remainder x 5) 0)
                                   (- 0 x)
                                   x)
                               (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
;create stream
(define dan-then-dog
  (letrec([f (lambda (x) (cons (if (= (remainder x 2) 1)
                                   "dan.jpg" "dog.jpg")
                               (lambda () (f (+ x 1)))))])
    (lambda() (f 1))))


;stream->stream
(define (stream-add-zero s)
  (letrec([f (lambda(ss) (cons  
                        (cons 0 (car (ss)))
                        (lambda () (f (cdr (ss))))))])
    (lambda()(f s))))
                                   
;;test add-zero
(define dan-zero (stream-add-zero dan-then-dog))

;8 cycle-lists
;list*list ->stream
(define (cycle-lists xs ys)
  (letrec([f (lambda(i) (cons (cons (list-nth-mod xs i)
                              (list-nth-mod ys i))
                              (lambda() (f (+ i 1)))))])
    (lambda() (f 0))))
                              
;9 vector-assoc  
;v*vector -> #f or pair
(define (vector-assoc v vec)
  (letrec([f ( lambda(i) (if (= i (vector-length vec))
                             #f
                             (let([el (vector-ref vec i)])
                               ( cond [(not (pair? el)) (f (+ i 1))]
                                      [(equal? v (car el)) el]
                                      [#t (f (+ i 1))]))))])
    (f 0)))

;10 cached-assoc
(define (cached-assoc xs n)
  (letrec([memo (make-vector n #f)]
          [pos 0]
          [f (lambda(v)
               (let ([ans (vector-assoc v memo)])
                 (if ans
                     ans
                     (let ([new-ans (assoc v xs)])
                       (if new-ans
                           (begin
                             (vector-set! memo pos new-ans)
                             (set! pos (remainder (+ pos 1) n))
                             new-ans)
                           #f)))))])
    f))

;11 defien macro (while-less e1 do e2)
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([e e1])
         (letrec ([loop (lambda ()
                          (if (<= e e2)
                              #t
                              (loop)))])
           (loop)))]))

(define a 2)
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
                                     
                                 
          
