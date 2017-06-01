# Fundamental concepts of Programming Languages
in SML/NJ, Racket and Ruby

notes on the coursera course, [Programming Languages-Part A,B,C](https://www.coursera.org/learn/programming-languages/)

To be a decent programmer, you have to know all the following aspects around the programming languages:
- [ ] syntax: how to write the code
- [ ] semantics: evaluation rules
- [ ] idioms: typical patterns to use certain language features
- [ ] libraries: such as standard lib to access files, basic data structures
- [ ] tools: implementations of REPL, debugger, formatter, etc.

Programming is to solve problems.
- [ ] Divide and conquer: if the problem is too big, chop it into smaller pieces so that each could be solved and later combine the solutions to each smaller problems.
- [ ] One aspect of Functional programming is to think recursively. Different paradigm is to phrase the same problem differently.  

## wk1: introduce the course and setup SML and GNU Emacs
no homework

## wk2: start with SML/NJ
example code along with the video in code/section1
Sample answer hw1sample.sml is very good. 

* notes on SML: type, syntax, semantics
ML variable bindings and expressions, shadowing, functions, pairs and tuples, lists, let for local bindings, options, booleans and comparison operations.
Benefits for no mutation.
```
(* this is a comment *)
"this is a string"
negative number can be written in either 0-7 or ~7
= is used to test equality. 
no assignment but binding.
if-then-else is a complete expression, but if-then is NOT.
```

* are you covering all cases?
```
fun is_smaller (x: int, y: int) =
  if x < y
  then true
  else false;
```
To compare two integers are straightforward like the above. To compare two dates in the format of tuple ```(year, month, day)``` is not.

My first version of function ```is_older```:
```
fun is_older_bad (date1: (int*int*int), date2: (int*int*int)) =
  if (#1 date1) >= (#1 date2)
  then false
  else
      if (#2 date1) >= (#2 date2)
      then false
      else
	  if (#3 date1) >= (#3 date2)
	  then true
	  else false;
```

The final version of function ```is_older```. This covers all the cases (branches).
```
fun is_older (date1: (int*int*int), date2: (int*int*int)) =
  if (#1 date1) < (#1 date2)
  then true
  else
      (if (#1 date1) = (#1 date2)
       then
	   if (#2 date1) < (#2 date2)
	   then true
	   else
	       (if (#2 date1) = (#2 date2)
		then if (#3 date1) < (#3 date2)
		     then true
		     else false
		else false)			      
      else false);
```
But there are better ways to write ```is_older```.

## wk3: compound types and pattern matching 2017/05/18
example code in code/section2

Note: no recursive call in the anonymous function with SML.


