#lang racket
(define (my-if x y z) (if x (y) (z)))

(define (fact x)
  (my-if (= x 0)
         (lambda () 1)
         (lambda () (* x (fact(- x 1))))))