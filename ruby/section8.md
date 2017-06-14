# Ruby (pure OO + dynamic typing)

- [ ] Ruby's blocks are almost function closures
- [ ] subclassing
- [ ] inheritance
- [ ] method overriding
- [ ] OOP vs FP: multiple inheritance, Ruby's mixins and double dispatch, Java-style interface
- [ ] subtyping

## ruby
dynamic typed. object. class-based. irb (interactive ruby command)

| features | dynamically typed | statically typed |
| --- |:---:|:---:|
| functional | Racket | SML |
| object-oriented | Smalltalk, Ruby | Java, etc. |

* all values are references to objects
* objects communicates via method calls, also knowns as messages
* each object has its own (prviate) state, accessible by its methods
* every object is an instance of a class
* an object's class determines the object's behavior
* three visibilities for methods in Ruby: private (object), protected (class), public
* nil is the empty object with nil? method. 
* both nil and false are false in Ruby.
* reflection: 3.methods, 3.class, instance_methods
* class is dynamic

## ruby array class
* any number of other objects indexed by number
* standard collection
* ```a = [2, 3, 4, 5]```index staring from 0. if not, nil returned.
* index can be negative
* the size of array can be changed dynamically
* element of the array can be different types
* pop and push method available to the array, stack
* shift and unshift, queue
* b = a, alias; c = a + [], new array instead of alias
* almost closure: each, map, any?, all?, inject (like reduce), collect, select (like filter)

## block: yield and proc: call
```
a = [3, 5, 7, 9]
b = a.map {|x| x + 1} # new array: [4, 6, 8, 10]
i = b.count { |x| x >= 6 } # 3

c = a.map { |x| (lambda { |y| x >= y }) }
c[2].call 17
```

## hashes and range
```
(1..100)
```

