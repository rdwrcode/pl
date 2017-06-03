#lang racket

(provide (all-defined-out))

; pair
(define pr (cons 1 (cons #t "hi")))
; list: nested pair ends with null
(define lst (cons 1 (cons #t (cons "hi" null))))

(list? pr) ; #f
(and (list? lst) (pair? pr)) ; #t
(list? null) ; #t

; mcons for mutable pairs
; cons cells are immutable, biggest change from Scheme
(define x (cons 14 null))

(define y x)

(set! x (cons 42 null))

(define mpr (mcons 1 (mcons #t "hi")))

(mcar mpr)
(mcdr mpr)

; a proper list cannot be made of mcons cells
; mcons, mcar, mcdr,
; mpair?
; set-mcar!, set-mcdr!

; in ML, Racket, Java, C:
; - function arguments are eager (call-by-value),
; evaluated once before calling the function
; - conditional branches are not eager
;
(define (factorial-normal x)
  (if (= x 0)
      1
      (* x (factorial-normal (- x 1)))))
; a zero-argument function used to delay evaluation is called thunk.
; thunk the expression
; to delay evaluation: put the expression in a function
; e
; => (lambda ()  e)
; => (e)
(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)] ; y-thunk is a function, here return the result
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

; use thunk and mutable pairs to implement promise
(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

;
(my-mult 0 (let ([p (my-delay (lambda () (slow-add 3 4)))])
(lambda () (my-force p))))

(my-mult 1 (let ([p (my-delay (lambda () (slow-add 3 4)))])
(lambda () (my-force p))))

(my-mult 50 (let ([p (my-delay (lambda () (slow-add 3 4)))])
(lambda () (my-force p))))

; a stream is a thunk, when called returns a pair.

(define ones (lambda () (cons 1 ones)))
(car (ones))
(car ((cdr (ones))))

(define (f x) (cons x (lambda () (f (+ x 1)))))
(define nats (lambda () (f 1)))
