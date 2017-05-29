Notes on section 4 of Part A (last section)

## Goals: 
* ML type inference: give every binding/expression a type 
* mutual recursion, 
* module system for encapsulation and abstraction, 
* equivalent functions, and functional programming

## ML
* staticallly typed
* implicitly typed

## type inference
* determine types of bindings in order (except for mutual recursion)
```
fun compose (f, g) = fn x => f (g x);
val compose = fn : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b
```
* value restriction: a variable-binding can have a polymorphic type only if the expression is a variable or value. 

* more difficult without polymorphism. more difficult with subtyping.

## mutual recursion
* and keyword
```
fun f1 p1 = e1
and f2 p2 = e2
and f3 p3 = e3
```
the same applies to datatype.

* state-machine example
```
fun macth xs = (* [1, 2, 1, 2] *) 
  let fun s_need_one xs = 
        case xs of
          [] => true
        | 1::xs' => s_need_two xs'
        | _ => false
      and s_need_two xs =
        case xs of 
          [] => false
        | 2::xs' => s_need_one xs'
        | _ => false
  in
    s_need_one xs
  end;
```

## modules
* modules: for namespace management
```
structure MyModule = struct bindings end
```
ModuleName.bindingName like List.foldl or String.toUpper.

* modules: open
```
open MyModule;
```
* modules: signatures and hiding things
A signatue is a type for module. public or private through signature
```
signature SIGNAME =  
sig types-for-bindings end

structure MyModule :> SIGNAME = 
struct bindings end
```

* Library spec (property) and invariants



