;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate  to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist(cdr xs)))))

    
                            
  

;; CHANGE (put your solutions here)

;; Problem 2
(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist(apair-e2 xs)))))

;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e)
         e]
        [(aunit? e)
         e]
        [(closure? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)(int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "> arguments aren't both real")))]
        [(fun? e)
         (closure env e)]
        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
                [new-env (cons(cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "fst argument is not an apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "snd argument is not an apair")))]
        [(isaunit? e)
         (let*([e1 (isaunit-e e)]
               [v (eval-under-env e1 env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([f (closure-fun v1)]
                     [old-env (closure-env v1)]
                     [name-opt (fun-nameopt f)]
                     [formal (fun-formal f)]
                     [body (fun-body f)]
                     [add-env1 (cons formal v2)]
                     [add-env2 (cons name-opt (closure old-env f))]             
                     [new-env (if (equal? name-opt #f)
                                  (cons add-env1 old-env)
                                  (cons add-env1 (cons add-env2 old-env)))])
                 (eval-under-env body new-env))
               (error "first argument of call is not a closure")))]    
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))
  
;;standart answer
;(defien (mlet* bs e2)
;  (cond [(null? bs) e2]
  ;      [#t (mlet (car (car bs)) (cdr (car bs))
 ;                 (mlet* (cdr bs) e2))]))

(define (mlet* lstlst e2)
  (letrec([f (lambda(xs) (if (null? xs)
                          e2
                          (let([x (car xs)]
                               [xss (cdr xs)])
                            (mlet (car x) (cdr x) (f xss) ))))])
    (f lstlst)))
                               

(define (ifeq e1 e2 e3 e4) 
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

;; Problem 4

(define addition
  (fun "addition" "x" (add (int 5) (var "x"))))

(define addition2
  (fun "additions" "x" (ifgreater (var "x") (int 3) (add (int 1) (var "x"))
                                  (add (int 2 ) (var "x")))))
(define a2 addition2)

(define a addition)
(define additions
  (fun "additions" "xs" (ifgreater (isaunit (var "xs"))
                                   (int 0)
                                   (aunit)
                                   (apair (add (fst (var "xs")) (int 1))
                                          (call (var "additions")
                                                (snd (var "xs")))))))

(define as additions)

(define m-map
         (lambda (xs)
           (lambda (f)
             (if (null? xs)
                 null
                 (cons (f (car xs)) ((m-map (cdr xs)) f))))))
(define m-map2
  (lambda (f)
    (lambda (xs)
      (if (null? xs)
          null
          (cons (f (car xs)) ((m-map2 f) (cdr xs)))))))
(define lst (list 1 2 3))
(define (mult x)
  (* x 2))
     
 
(define mupl-map
  (fun "mupl-map" "f" (fun "#f" "xs"
                           (ifgreater (isaunit (var "xs"))
                                      (int 0)
                                      (aunit)
                                      (apair(call (var "f") (fst (var "xs")))
                                            (call (call (var "mupl-map") (var "f")) (snd (var "xs")))))))) 
(define m1 (call mupl-map (fun #f "x" (add (var "x") (int 7)))))

;(define  mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "#f" "i" (fun "#f" "xs"
                           (call (call (var "map") (fun "#f" "x" (add (var "x") (var "i")))) (var "xs"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
