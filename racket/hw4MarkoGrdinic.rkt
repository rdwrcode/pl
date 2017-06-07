#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (letrec ([loop (λ (i acc)
                   (if (<= i high)
                       (loop (+ i stride) (cons i acc))
                       (reverse acc)))])
    (loop low '())
    ))

(define (string-append-map xs suffix)
  (map (λ (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (match xs
    [_ #:when (< n 0) (error "list-nth-mod: negative number")]
    ['() (error "list-nth-mod: empty list")]
    [_ (list-ref xs (remainder n (length xs)))]))

(define (stream-for-n-steps s n)
  (if (> n 0)
      (match-let ([(cons v s) (s)])
        (cons v (stream-for-n-steps s (- n 1))))
      '()))

(define funny-number-stream
  (let* ([is_funny (λ (x) (= 0 (remainder x 5)))]
         [funny (λ (x) (if (is_funny x) (- x) x))])
    (letrec ([f (λ (x) (cons (funny x) (λ () (f (+ x 1)))))])
      (λ () (f 1)))))

(define dan-then-dog
  (let ([show (λ (x) (if (equal? x #t) "dan.jpg" "dog.jpg"))])
    (letrec ([f (λ (x) (cons (show x) (λ () (f (equal? x #f)))))])
      (λ () (f #t)))))

(define (stream-add-zero s)
  (λ ()
    (match-let ([(cons a s) (s)])
      (cons (cons 0 a) (stream-add-zero s)))))

(define (cycle-lists xs ys)
  (letrec ([f (λ (i)
                (let ([x (list-nth-mod xs i)]
                      [y (list-nth-mod ys i)])
                  (cons (cons x y) (λ () (f (+ i 1))))))])
    (λ () (f 0))))

(define (vector-assoc v vec)
  (let ([opt-car (λ (x v else)
                   (match x
                     [(cons a b) #:when (equal? a v) x]
                     [_ (else)]))])
    (letrec ([loop (λ (i)
                     (if (< i (vector-length vec))
                         (opt-car (vector-ref vec i) v (λ () (loop (+ i 1))))
                         #f))])
      (loop 0))))

(define (cached-assoc xs n)
  (let* ([vector-nth-mod_set (λ (xs n v)
                               (begin
                                 (vector-set! xs (remainder n (vector-length xs)) v)
                                 v))]
         [memo (make-vector n #f)]
         [memo_i 0]
         [memo_i (λ () 
                   (begin
                     (set! memo_i (+ 1 memo_i))
                     memo_i))])
    (λ (v)
      (let ([x (vector-assoc v memo)])
        (match x
          [#f (vector-nth-mod_set memo (memo_i) (assoc v xs))]
          [_ x])))))
