#lang racket

(provide (all-defined-out))

(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

;(my-mult 0 (lambda () (slow-add 3 4)))

;(my-mult 1 (lambda () (slow-add 3 4)))

;(my-mult 20 (lambda () (slow-add 3 4)))

;; these are OK
;(my-mult 0 (let ([x (slow-add 3 4)]) (lambda () x)))
;(my-mult 1 (let ([x (slow-add 3 4)]) (lambda () x)))
;(my-mult 2 (let ([x (slow-add 3 4)]) (lambda () x)))


;; pass a think you do not need to evaluate it and get the result immediately
(define (my-delay th)
  (mcons #f th))

;; #f, 
(define (my-force p)
  (if (mcar p)
    (mcdr p)
    (begin (set-mcar! p #t)
           (set-mcdr! p ((mcdr p)))
           (mcdr p))))

;; better way to call my-mult
;(my-mult 10 (let ([p (my-delay (lambda () (slow-add 3 4)))])
;             (lambda () (my-force p))))


;(define ones-bad (cons 1 ones-bad))
; ones-bad is not defined when evaluated as argument eagerly

;(define ones-bad (lambda () (cons 1 (ones-bad))))
; here ones-bad is thunk but (ones-bad) is not the thunk!!

;(define xs (list (cons 1 2) (cons 3 4) (cons 5 6)))
;(assoc 3 xs) ; `(3 . 4)
;(assoc 6 xs) ; #f

