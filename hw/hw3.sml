(* Coursera Programming Languages, Homework 3, Provided Code *)
(* solutions by Hailin 2017/05/29 *)

(* 1: assumption: all strings in the list have at least 1 character *)
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

fun longest_capitalized1 (xs: string list) =
  case only_capitals(xs) of
      [] => ""
    | x => longest_string1(x);

val longest_capitalized = longest_string1 o only_capitals;

fun s2l s =
  let fun aux (s, n, acc) =
	if n > 0
	then aux(s, n-1, String.str(String.sub(s, n-1))::acc)
	else acc
  in
      aux(s, size s, [])
  end;

fun s2l_rev s =
  let fun aux (s, n, acc) =
	if n < size s
	then aux(s, n+1, String.str(String.sub(s, n))::acc)
	else acc
  in
      aux(s, 0, [])
  end;
      	  
val rev_string = String.concat o s2l_rev;

val rev_string2 = String.concat o List.rev o s2l;

(**** 7-8 ****)
exception NoAnswer

fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => if isSome (f x)
		then valOf (f x)
		else first_answer f xs';

fun all_answers f xs =
  let fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => if isSome (f x)
		      then aux(xs', acc @ (valOf (f x)))
		      else aux(xs', acc)
  in
      case aux(xs, []) of
	  [] => NONE
	| ys => SOME ys
  end;

(**** 9-12 ****)
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

(* 9 *)
	
val counter : int ref = ref 0;
fun count x = (counter := !counter + 1; !counter);

(*
fun count xs =
  case xs of
      [] => 0
    | _::xs' => 1+count(xs');
*)
fun nothing s = if Char.isUpper(String.sub(s, 0)) then 1 else 0; 

val count_wildcards = g count nothing;


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** solution to challenge problem ****)

