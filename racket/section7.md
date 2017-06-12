# static vs dynamic typing
recognize advantages and disadvantages of ML and racket

## Racket's perspective
syntax aside, because of type-checking, a lot of code in ML would be rejected
but it would not be rejected by Racket (though runtime exception for sure if buggy). So can we think ML as a well-defined subset of Racket?

```
(define (g x) (+ x x))

(define (f y) (+ y (car y))
(define (h z) (g (cons z 2)))

(define (f x) (if (> x 0) #t (list 1 2))) ; OK in Racket, but ML can return one-type
(define xs (list 1 #t "hi"))
(define y (f (car xs)))
```  

## ML's perspective
Racket is a "one big datetype" ML.
All values has theType. But struct in Racket has no way to be put under theType.
```
datatype theType = Int of int
	| String of string
	| Pair of theType * theType
	| Fun of theType -> theType
	| ...
```

## what is static checking?
type system: approach and purpos

purpose of type system is to prevent something before it runs
language design includes deciding what is checked and how.

static checking/dynamic checking are just tow points on a continuum.
* keystroke - typo
* __compile time__ 
* link time
* __run time__
* later

## what is type-checking?
correctness
soundness: never accepts a program that does X (no false negatives) and 
completeness: never rejects a program that no matter what input it is run with, will not do X (no false positives)

incompleteness in ML
```
fun f1 x = 4 div "hi"
fun f2 x = if true then 0 else 4 div "hi"

fun f3 x = if x then 0 else 4 div "hi"
val x = f3 true

fun f4 x = if x <= abs x then 0 else 4 div "hi"

fun f5 x = 4 div x
val y = f5 (if true then 1 else "hi"
```
almost anything you might like to check statically is undecidable:
* any static checker cannot do all of (1) always terminate (2) be sound (3) be complete
* this is a mathematical theorem!

## weak typing (C/C++) vs strong typing
the program passes static checking but when run can set the computer on fire!
- easy implementation of language
- performance
- lower level
neither static or dynamic checks, if it happens, the program is still allowed to run.

Racket is not weakly typed.
Cons cells can build anything
Anything not #f is true
...

runtime semantics of the primitive

