#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Task 1.
(define (sequence low high stride)
  (if (> low high) null
      (cons low (sequence (+ low stride) high stride))))

; Task 2.
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; Task 3.
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
             (car (list-tail xs i)))]))

; Task 4.
(define (stream-for-n-steps s n)
  (if (= n 0) null
      (cons (car (s))
            (stream-for-n-steps (cdr (s)) (- n 1)))))

; Task 5.
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- x) (lambda () (f (+ 1 x))))
                    (cons x (lambda () (f (+ 1 x))))))])
    (lambda () (f 1))))

; Task 6.
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (string=? x "dog.jpg")
                    (cons "dan.jpg" (lambda () (f "dan.jpg")))
                    (cons "dog.jpg" (lambda () (f "dog.jpg")))))])
    (lambda () (f "dog.jpg"))))

; Task 7.
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

; Task 8.
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
                      (lambda () (f (+ 1 n)))))])
    (lambda () (f 0))))

; Task 9.
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (> n (- (vector-length vec) 1))
                    #f
                    (if (pair? (vector-ref vec n))
                        (if (equal? (car (vector-ref vec n)) v)
                            (vector-ref vec n)
                            (f (+ n 1)))
                        (f (+ n 1)))))])
    (f 0)))

; Task 10.
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [index 0])
    (lambda (v)
      (if (vector-assoc v cache)
          (vector-assoc v cache)
          (let ([cur-val (assoc v xs)])
            (begin
              (vector-set! cache index cur-val)
              (set! index
                    (remainder (+ index 1) n))
              (vector-assoc v cache)))))))

; Task  11 (Challenge)

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([x e1] [y e2])
       (letrec ([fe (lambda (e)
                (if (< e x)
                    (begin (let ([f e2]) (fe f)))
                    #t))])
       (fe y)))]))