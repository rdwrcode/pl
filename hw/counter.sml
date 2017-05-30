val count : int ref = ref 0;

fun factorial n =
  if n < 1
  then 1
  else (count := !count + 1; n * factorial (n-1));

(* run factorial, then check !count *)
(*
- use "counter.sml";
[opening counter.sml]
val count = ref 0 : int ref
val factorial = fn : int -> int
val it = () : unit
- factorial 10;
val it = 3628800 : int
- !count;
val it = 10 : int
- factorial 5;
val it = 120 : int
- !count;
val it = 15 : int
- 
 *)
			 
