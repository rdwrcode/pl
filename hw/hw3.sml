(* Coursera Programming Languages, Homework 3, Provided Code *)
(* solutions by Hailin 2017/05/26 *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* 1 *)
fun only_capitals (xs: string list) =
  List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs;

(* 2 *)
fun longest_string1 (xs: string list) =
  List.foldl (fn (s,x) => if String.size s > String.size x then s else x) "" xs;

(* 3 *)
fun longest_string2 (xs: string list) =
  List.foldl (fn (s,x) => if String.size x > String.size s then x else s) "" xs;

(* 4
 * longesr_string_helper is curried 
 *)
fun longest_string_helper f xs =
  List.foldl (fn (s,x) => if f(String.size s,String.size x) then s else x) "" xs;

(* why this anonymous won't work?
val longest_string_helper =
  fn f => fn xs => List.foldl (fn (x, y) => if f(x, y) then x else y) "" xs
*)
			      
fun longest_string3 (xs: string list) =
  longest_string_helper (fn (s,x) => s > x) xs;

fun longest_string4 (xs: string list) =
  longest_string_helper (fn (s,x) => s >= x) xs;


	      
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
