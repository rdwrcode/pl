# data structure with Racket

- [ ] dynamically typed. use ```struct``` for abstraction and modularity.
- [ ] how to write a program in one programming language that is an implementation of another programming language
- [ ] compiler vs interpreter

## symbols
```'foo``` use ```eq?``` to compare. 
 
## struct
```
(struct foo (bar baz quux) #:transparent)
(struct card (suit rank) #:transparent #:mutable)

(struct const (int) #:transparent)
```
better style and more concise
more abstract for module system and contract system in Racket.

* a function can not introduce multiple bindings
* neither functions nor macros can create a new kind of data

## dealing with variables and environment
List of pairs (strings and values)
closure: code part + its environment
free variables: not bound in closure

## hw5
hw5 is challenging. 
value is always evaluated to the value itself. 
binding is to construct the Racket pair: 
* name in Racket string, and 
* value in MUPL expression (evaluated)
 
The difficulty is two-folds, easy said hard done.
* when to use MUPL or Racket and,
* when to evaluate MUPL expression or not

