#lang web-server/insta

; port 63393
(define (start request)
  (response/xexpr
   '(html
     (head (title "my testing"))
     (body (h1 "hello")))))

