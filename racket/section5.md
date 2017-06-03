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

