
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
;; (length (list 1 2 3 4))
;; 4
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))



