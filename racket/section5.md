# Section5: basics

## 
- [X] based on scheme, but some non-compitable changes
- [X] Empty list: null, cons, car (head) cdr (tail), null? (list e1...en)

## 
- [ ] syntax and parentheses: a term
- [ ] an atom, #t, #f, 34, "hi", null, 4.0, x, ...
- [ ] a special form: define, lambda, if (macro to define your own)
- [ ] a sequence of terms in parens: (t1 t2 ... tn)
- [ ] if t1 a special form, semantics of sequence is special
- [ ] else a function call

## 
- [ ] DrRacket [] ()
- [ ] text to a parsed tree: atoms are leaves, leaves are atoms which are not special forms
- [ ] (e) call e with zero arguments

##
- [ ] 
```
      (cond [e1a e1b]
            [e2a e2b]
            ...
            [eNa eNb]) eNa should be #t
```
if not #f, then all true

## local binding

## Delayed evaluation
how to delay evaluation: put the expression in a function and don't call it.
```
(define (my-if x y z)
  (if x (y) (z)))

(define (fact n)
  (my-if (= n 0)
    (lambda () 1)
    (lambda () (* n (fact (- n 1))))))
```
A zero-argument function used to delay evaluation is called a thunk.
```
expression e, evaluate it and get the result
(lambda () e), a function is evaluated when called
(e), evaluate e to some thunk and then call the thunk
```

```
(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
    (mcdr p)
    (begin (set-mcar! p #t)
           (set-mcar! p ((mcdr p)))
           (mcdr p))))
```

## streams: idiom
a stream = a thunk that called returns a pair
```
(define ones (lambda () (cons 1 ones)))
```

## memoization: idiom
if a function has no side-effect and does not read mutable memory, npo point to calculating it twice.
thunk, promises do not take arguments.

## macros
macro definition: how to transform some new syntax into different syntax in the source language.
macro system: a language (or part of a larger language) for defining macros.
macro expansion: the process of rewriting the syntax for each macro use.

tokenization, parenthesization, and scope.

```
(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))
```

variables, macros, and hygiene
the following functions are equivalent to each other.
```
(define (dbl x) (+ x x))
(define (dbl x) (* 2 x))
```
the following macros are not equivalent.
```
(define-syntax dbl 
  (syntax-rules () 
    [(dbl x) (+ x x)]))

(define-syntax dbl 
  (syntax-rules () 
    [(dbl x) (* 2 x)]))
```
If you use the following code with the macros above,
```
(dbl (begin (print "hi") 42))
```
Better macro
```
(define-syntax dbl 
  (syntax-rules () 
    [(dbl x) 
     (let ([y x]) ; evaluate x only once!
          (+ y y)]))
```
Local variable is separated from local variable in the macro, even if both has the same name.
Secretly renames local variables in macros with fresh names.



