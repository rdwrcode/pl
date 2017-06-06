#lang slideshow

; #<procedure:circle>, the identifier circle is bound to a function,
; aka. "procedure"
;

(define c (circle 10))
(define r (rectangle 10 20))
(hc-append c r)
(hc-append 20 c r c)

(define (square n)
  (filled-rectangle n n))

(square 20)

; local binding with define
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(four (circle 20))

; let can be used in any expression position
; it binds many identifier at once, instead of requiring a separare define
; for each identifier.
; a let form binds many identifier at the same time,
; so the bindings cannot refer to each other.
;
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(checker (colorize (square 20) "red")
         (colorize (square 20) "black"))

; let* allows later bindings to use earlier bindings.
;
(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

(checkerboard (square 10))

; function as values
(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))

(series circle)

(series square)

(series (lambda (size) (checkerboard (square size))))

; a define form for a function is really a shorthand for a simple define
; using lambda as the value
(define test-series
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))

(test-series square)

; racket is lexical scoped language
; whenever an identifier is used as an expression,
; something in the textual environment of the expression determines
; the identifier’s binding
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))

(rgb-series circle)

(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))

(series (rgb-maker square))

; lists
(list "red" "green" "blue")
(list (colorize (circle 10) "red") (colorize (square 10) "green"))

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))

(rainbow (square 10))
(apply vc-append (rainbow (square 10)))

; modules
(require pict/flash)
(filled-flash 40 40)

; macros
(require slideshow/code)
(code (circle 10))

; let
; bind any number of local variables
; at the same time
; before the let-expression
; except that the body can use all the local variables
(define (silly-double x)
  (let ([x (+ x 3)] ;; x is shadowed, it is evaluated from the argument
        [y (+ x 2)]) ;; y uses x which is evaluated from the argument too.
    (+ x y -5)))

; let*
; evaluated in the environment produced from previous bindings

(define (silly-double2 x)
  (let* ([x (+ x 3)] ;; x is evaluated from the argument
         [y (+ x 2)]) ;; y uses x, x is evaluated from previous binding
    (+ x y -8)))


; letrec
; the expressions are evaluated in the environment that includes all the bindings
; when mutual recursion is needed

(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda (z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))

;
(define (silly-mod2 x)
  (letrec
      ([even? (lambda (x) (if (zero? x) #t (odd? (- x 1))))]
       [odd? (lambda (x) (if (zero? x) #f (even? (- x 1))))])
    (if (even? x) 0 1)))

(define (silly-mod x)
  (define (even? x) (if (zero? x) #t (odd? (- x 1))))
  (define (odd? x) (if (zero? x) #f (even? (- x 1))))
  (if (even? x) 0 1))




