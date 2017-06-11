#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5.rkt")

;; mupl-map test
(define mfun (fun #f "x" (add (var "x") (int 7))))
(define mfactual (int 1))
(define mlist0 (aunit))
(define mlist1 (apair (int 1) (aunit)))
(define mlist2 (apair (int 3) (apair (int 1) (aunit))))

;; call evaluates its first and second arguments to values
;; the first is a closure, or error
;; evaluate the function body in the closure
;; 1 if not anonymous, bind the function name to the closure
;; 2 bind the function argument name to the second subexpression.
;;
;; recursively
;;
(define mcall (call (closure '() mfun) mfactual))

(define mpfun (call mupl-map mfun))

(define ans1 (apair (int 8) (aunit)))

(define firstcall (call mupl-map mfun))
(define secondcall (call (firstcall mlist2)))

;;(eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))

