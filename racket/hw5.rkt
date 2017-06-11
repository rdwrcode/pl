;; Programming Languages, Homework 5
;; no any mutations such as set!, set-mcar, and so on

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
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value;
;; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
;; 1a
(define (racketlist->mupllist x)
  (letrec ([args x]
           [f (lambda (xs)
                (if (null? xs)
                    (aunit)
                    (apair (car xs) (f (cdr xs)))))])
    (f args)))

;; testing
(define tx (racketlist->mupllist (list (int 3) (int 4))))
(define ty (apair (int 3) (apair (int 4) (aunit))))
(equal? tx ty)

;; 1b
(define (mupllist->racketlist x)
  (letrec ([f (lambda (xs)
                (if (apair? xs)
                    (append (list (apair-e1 xs)) (f (apair-e2 xs)))
                    null))])
    (f x)))

;; Problem 2

(define mycall (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1)))

;; lookup a variable in an environment
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
        ;; CHANGE add more cases here
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater compared with non-number")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(aunit? e)
         (aunit)]
        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "MUPL snd applied to non-pair")))]
        [(mlet? e)
         (let ([varname (mlet-var e)]
               [varvalue (mlet-e e)]
               [mlet-body (mlet-body e)])
           (eval-under-env mlet-body (list (cons varname varvalue) env)))]
        ;; closure is an internal data structure
        ;; if we see a closure, it is a value in MUPL, we should not evaluate it.
        ;; just return it back.
        [(closure? e)
         (let ([env-c (closure-env e)]
               [fun-c (closure-fun e)])
           (closure env-c fun-c))]
        ;; fun is evaluated to a closure
        [(fun? e)
         (let ([f-name (fun-nameopt e)]
               [f-arg (fun-formal e)]
               [f-body (fun-body e)])
           (if (equal? f-name #f)
               (closure (list (cons "funarg" f-arg)
                              env)
                        f-body)
               (closure (list (cons "funarg" f-arg)
                              (cons "funname" f-name)
                              env)
                        f-body)))]
        ;; it is time to evaluate all the closures recursively
        ;; this is the main point to have call
        [(call? e)
         (let ([funexp (eval-under-env (call-funexp e) env)]
               [actual (eval-under-env (call-actual e) env)])
           (if (closure? funexp)
               (let ([c-env (closure-env funexp)]
                     [c-fun (closure-fun funexp)])
                 (begin
                   (letrec ([new-c (eval-under-env c-fun c-env)]
                         [vname (envlookup (closure-env new-c) "funarg")]
                         [newenv (list (cons vname actual) null)]
                         [fbody (closure-fun new-c)])
                     (eval-under-env fbody newenv))))
               (error "MUPL call non-closure")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(call? e)
         (let ([funexp (eval-under-env (call-funexp e) env)]
               [actual (eval-under-env (call-actual e) env)])
           (if (closure? funexp)
               (eval-under-env funexp env)
               (error "MUPL call non-closure")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (aunit? (eval-exp e1)) e2 e3))

;; helper function
;; acc serves as env for eval-under-env
(define (evallist lst acc)
  (if (null? lst)
      acc
      (evallist (cdr lst) (list (cons (car (car lst))
                                      (eval-under-env (cdr (car lst)) acc))
                                acc))))

(define (mlet* lstlst e2)
  (envlookup (evallist lstlst null) (var-string e2)))

;; TODO ifeq requires more touch, _x, _y as variable names
;;
  
(define (ifeq e1 e2 e3 e4)
  (let ([v1 (if (int? (eval-exp e1)) (int-num e1) (error "ifeq e1 applied to non-int"))]
        [v2 (if (int? (eval-exp e2)) (int-num e2) (error "ifeq e2 applied to non-int"))])
    (if (= v1 v2)
        e3
        e4)))

;; Problem 4
;; multiple-element list
;; single-element list
;; empty list

(define mupl-list
  (lambda (f lst acc)
    (let ([fname (fun-nameopt f)]
          [fargs (fun-formal f)]
          [fbody (fun-body f)])
      (if (aunit? lst)
          acc
          (mupl-list (apair-e2 lst) (apair (f (apair-e1 lst)) acc))))))

(define (mupl-list2 f lst acc)
  (let ([fname (fun-nameopt f)]
        [fargs (fun-formal f)]
        [fbody (fun-body f)])
    (if (aunit? lst)
        acc
        (mupl-list2 f
                    (apair-e2 lst)
                    (apair (call (fun fname fargs fbody) (apair-e1 lst)) acc)))))

(define mupl-map1
  (lambda (f)
    (lambda (lst)
      (mupl-list f lst (aunit)))))

(define mupl-map2
  (lambda (f)
    (lambda (lst)
      (let ([fname (fun-nameopt f)]
            [fargs (fun-formal f)]
            [fbody (fun-body f)])
        (if (aunit? (apair-e1 lst))
            (error "end")
            (call (fun fname fargs fbody) (apair-e1 lst)))))))

(define mupl-map
  (lambda (f)
    (lambda (lst)
      (let ([fname (fun-nameopt f)]
            [fargs (fun-formal f)]
            [fbody (fun-body f)])
        (if (aunit? (apair-e1 lst))
            (fun fname fargs (cons fbody (aunit)))
            (fun fname fargs (cons fbody (
        
(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

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
