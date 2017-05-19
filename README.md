# Fundamental concepts of Programming Languages
in SML/NJ, Racket and Ruby

notes on the coursera course, [Programming Languages-Part A,B,C](https://www.coursera.org/learn/programming-languages/)

## wk1 introduction of the course and setup SML
no homework

## wk2 start with SML/NJ
hw1
* are you covering all possible cases?
```
fun is_smaller (x: int, y: int) =
  if x < y
  then true
  else false;
```
To compare two integers are straightforward, but to compare two dates in the format of tuple (year, month, day) is not.

My first version of ```is_older```:
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

The final version of ```is_older```. This covers all the cases.
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


