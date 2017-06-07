
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1 - sequence vs. range
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2 - use map and append
;; (append (list 1 2) (list 3 4)) -> '(1 2 3 4)
;; (map (lambda (number)
;;         (+ 1 number))
;;       '(1 2 3 4))

(define (string-append-map xs suffix)
  (map (lambda (x)
         (string-append x suffix)) xs))

;; 3 -
;;
;; (list-tail (list 1 2 3 4) 2)
;; '(3 4)
;;
;; (remainder 11 2)
;; 1
;;
;; (length (list 1 2 3 4)) ; > 4
;; 
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

;; 4 -
(define (stream-for-n-steps s n)
  (if (< n 0)
      (error "stream-for-n-steps: negative number")
      (letrec ([f (lambda (stream total acc ans)
                    (let ([pr (stream)])
                      (if (< acc total)
                          (f (cdr pr) total (+ acc 1) (append ans (list (car pr))))
                          ans)))])
        (f s n 0 null))))

;; 5 -
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; 6 -
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 2))
                    (cons "dog.jpg" (lambda () (f (+ x 1))))
                    (cons "dan.jpg" (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; 7 - 
(define (stream-add-zero s)
  (letrec ([f (lambda (x)
                (let ([pr (x)])
                  (cons (cons 0 (car pr)) (lambda () (f x)))))])
    (lambda () (f s))))
                         
;; 8 -
;; from a list to generate a stream by cycling through itself
;; then merge two streams into one
;; start count from 0
;;
(define (cycle-lists xs ys)
  (letrec ([m (length xs)]
           [n (length ys)]
           [f (lambda (x) (cons (cons (list-ref xs (remainder x m))
                                      (list-ref ys (remainder x n)))
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;; 9 -
;; assoc
;> (define xs (list (cons 1 2) (cons 3 4) (cons 5 6)))
;> (assoc 1 xs) ; '(1 . 2)
;> (assoc 4 xs) ; #f
;
;; vector or array
;; use vector-ref, vector-length, equal?
;;
(define (vector-assoc v vec)
  (letrec ([n (vector-length vec)]
           [f (lambda (v vec x)
                (if (< x n)
                    (if (equal? v (car (vector-ref vec x)))
                        (vector-ref vec x)
                        (f v vec (+ x 1)))
                    #f))])
    (f v vec 0)))

;; 10 -
;; use memo to store all the results to make query fast
;;
;; Assume n is positive!
;;
(define (cached-assoc xs n)
  (letrec ([cache (build-vector n (lambda (x) (if (< x n)
                                                  (cons x #f)
                                                  #f)))] ; start with empty array
           [next-pos 0] ; next position to replace, circular cache?
           [f (lambda (x)
                (let ([ans (vector-assoc x cache)])
                  (if ans ; a hit in the cache
                      ans
                      (let ([new-ans (assoc x xs)])
                        (if new-ans
                            (begin
                              (vector-set! cache next-pos new-ans)
                              (if (< next-pos n)
                                  (set! next-pos (lambda (x) (+ x 1)))
                                  (set! next-pos (lambda () (0))))
                              new-ans)
                            #f)))))])
    f))

;; 11 -
;;
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([x e1])
       (if (< e2 x)
           (begin
             e2
             #t)
           #f))]))

  

      
                          
                          




