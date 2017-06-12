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
  (if (null? x)
      (aunit)
      (apair (car x) (racketlist->mupllist (cdr x)))))

;; 1b
(define (mupllist->racketlist x)
  (if (aunit? x)
      null
      (cons (apair-e1 x) (mupllist->racketlist (apair-e2 x)))))

;; Problem 2

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
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater compared with non-number")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
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
        ;; correct a mistake here:
        ;; use cons instead of list
        [(mlet? e)
         (let ([var-name (mlet-var e)]
               [var-value (eval-under-env (mlet-e e) env)] ; have to evaluate the value
               [mbody (mlet-body e)])
           (eval-under-env mbody
                           (cons (cons var-name var-value) env)))]
        ;; closure is an internal data structure
        ;; if we see a closure, it is a value in MUPL, we should not evaluate it.
        ;; just return it back.
        [(closure? e) e]
        ;; fun is evaluated to a closure
        [(fun? e) (closure env e)]
        ;; it is time to evaluate all the closures recursively
        ;; this is the main point to have call
        [(call? e)
         (let ([funexp (eval-under-env (call-funexp e) env)]
               [actual (eval-under-env (call-actual e) env)])
           (if (closure? funexp)
               (letrec ([c-env (closure-env funexp)]
                        [c-fun (closure-fun funexp)]
                        [f-name (fun-nameopt c-fun)]
                        [f-arg (fun-formal c-fun)]
                        [f-body (fun-body c-fun)]
                        [env1 (cons (cons f-arg actual) c-env)]
                        [env2 (if f-name
                                  (cons (cons f-name funexp) env1) ; use funexp instead of c-fun
                                  env1)])
                 (eval-under-env f-body env2))
               (error "MUPL call applied to non-closure")))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

; use eval-exp
;(define (ifaunit e1 e2 e3)
;  (if (aunit? (eval-exp e1))
;      (eval-exp e2)
;      (eval-exp e3)))

;; the subexpression of isaunit is evaluated
;; if an aunit, the result is (int 1) else (int 0)
;; if e1 an aunit, (int 1) is greater than (int 0), evaluate e2, or evaluate e3
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;; get rid of envlookup
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let* ([item (car lstlst)]
             [name (car item)]
             [exp (cdr item)])
        (mlet name exp (mlet* (cdr lstlst) e2)))))


;; TODO
;; assume all MUPL expressions has no MUPL variable _x or _y
;; what does it imply?
(define (ifeq1 e1 e2 e3 e4)
  (let ([v1 (let ([t1 (eval-exp e1)])
              (if (int? t1) (int-num t1) (error "MUPL ifeq e1 applied to non-int")))]
        [v2 (let ([t2 (eval-exp e2)])
              (if (int? t2) (int-num t2) (error "MUPL ifeq e2 applied to non-int")))])
    (if (= v1 v2)
        e3
        e4)))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))
  
;; Problem 4
;; multiple-element list
;; single-element list
;; empty list
;; the enlightenment is to use fun defined in MUPL instead of function or lambda in Racket.
;;
(define mupl-map
  (fun #f "mfun"
       (fun "f-map" "mlst"
            (ifaunit (var "mlst")
                     (aunit)
                     (apair (call (var "mfun") (fst (var "mlst")))
                            (call (var "f-map") (snd (var "mlst"))))))))
        
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map")
                   (fun #f "x" (add (var "i") (var "x")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (define (free-vars e)
    (cond [(var? e) (set (var-string e))]
          [(int? e) (set)] ; value
          [(add? e) (set-union (free-vars (add-e1 e) (free-vars (add-e2 e))))]
          [(ifgreater? e) (set-union (free-vars (ifgreater-e1 e))
                                     (free-vars (ifgreater-e2 e))
                                     (free-vars (ifgreater-e3 e))
                                     (free-vars (ifgreater-e4 e)))] 
          ; already bound, not free
          [(mlet? e) (set-remove (free-vars (mlet-body e))
                                 (mlet-var e))]
          [(fun? e) (set-remove (set-remove (free-vars (fun-body e))
                                            (fun-nameopt e))
                                (fun-formal e))]
          ; 
          [(call? e) (set-union (free-vars (call-funexp e))
                                (free-vars (call-actual e)))]
          [(apair? e) (set-union (free-vars (apair-e1 e))
                                 (free-vars (apair-e2 e)))]
          [(fst? e) (free-vars (fst-e e))]
          [(snd? e) (free-vars (snd-e e))]
          [(aunit? e) (set)] ; already value
          [(isaunit? e) (free-vars (isaunit-e e))]))
  (cond [(add? e) (add (compute-free-vars (add-e1 e))
                       (compute-free-vars (add-e2 e)))]
        [(ifgreater? e) (ifgreater (compute-free-vars (ifgreater-e1 e))
                                   (compute-free-vars (ifgreater-e2 e))
                                   (compute-free-vars (ifgreater-e3 e))
                                   (compute-free-vars (ifgreater-e4 e)))]
        [(fun? e) (fun-challenge (fun-nameopt e)
                                 (fun-formal e)
                                 (compute-free-vars (fun-body e))
                                 (free-vars e))]
        [(call? e) (call (compute-free-vars (call-funexp e))
                         (compute-free-vars (call-actual e)))]
        [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e))
                         (compute-free-vars (mlet-body e)))]
        [(apair? e) (apair (compute-free-vars (apair-e1 e))
                           (compute-free-vars (apair-e2 e)))]
        [(fst? e) (apair (compute-free-vars (fst-e e)))]
        [(snd? e) (apair (compute-free-vars (snd-e e)))]
        [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
        [#t e]))
                 
;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (define (extend-env s v env)
    (cons (cons s v) env))
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (int (+ (int-num v1) (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(apair? e)
         (let ([v1 (eval-under-env-c (apair-e1 e) env)]
               [v2 (eval-under-env-c (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env-c (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env-c (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([v (eval-under-env-c (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        [(fun-challenge? e)
         (closure (filter (lambda (binding)
                            (set-member? (fun-challenge-freevars e) (car binding)))
                          env)
                  e)]
        [(call? e)
         (let ([funexp (eval-under-env-c (call-funexp e) env)]
               [actual (eval-under-env-c (call-actual e) env)])
           (if (closure? funexp)
               (letrec ([c-env (closure-env funexp)]
                        [c-fun (closure-fun funexp)]
                        [f-name (fun-challenge-nameopt c-fun)]
                        [f-arg (fun-challenge-formal c-fun)]
                        [f-body (fun-challenge-body c-fun)]
                        [env1 (cons (cons f-arg actual) c-env)]
                        [env2 (if f-name
                                  (cons (cons f-name funexp) env1)
                                  env1)])
                 (eval-under-env-c f-body env2))
               (error "MUPL call applied to non-closure")))]
        [(mlet? e)
         (let ([mbody (mlet-body e)]
               [varname (mlet-var e)]
               [varvalue (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c mbody (cons (varname varvalue) env)))]
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
