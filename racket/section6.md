# data structure with Racket

- [ ] dynamically typed. use ```struct``` for abstraction and modularity.
- [ ] how to write a program in one programming language that is an implementation of another programming language
- [ ] compiler vs interpreter
- [ ] hw5 is challenge. The difficulty is two-folds: when to use MUPL or racket and evaluate MUPL expression or not

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

