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
(*	
val counter : int ref = ref 0;
fun count x = (counter := !counter + 1; !counter);
*)
fun count x = let val counter: int ref = ref 0
	      in
		  (counter := !counter + 1; !counter)
	      end;

fun str_size s = String.size(s);

fun str_num s = let val num: int ref = ref 0
		in
		    (num := !num + 1; !num)
		end;
  
val count_wildcards = g count (fn x => 0);

val count_wild_and_variable_lengths = g count str_size;

fun count_some_var (s, p) = g (fn x => 0) str_num p;

(* 10 *)
fun extract_pat pat =
  let
      fun getStr p =
	let fun aux pat acc =
	      case pat of
		  Variable x => x :: acc
		| TupleP ps => List.foldl (fn (p1,i) => (aux p1 acc) @ i) acc ps
		| ConstructorP(_,p2) => (aux p2 acc) @ acc
		| _ => acc
	in
	    aux p []
	end
  in
      getStr (pat)
  end;

fun check_pat pat =
  let
      fun getStr p =
	let fun aux pat acc =
	      case pat of
		  Variable x => x :: acc
		| TupleP ps => List.foldl (fn (p1,i) => (aux p1 acc) @ i) acc ps
		| ConstructorP(_,p2) => (aux p2 acc) @ acc
		| _ => acc
	in
	    aux p []
	end		   
      fun isRepeated xs =
	case xs of
	    [] => true
	  | x::[] => true
	  | x::y::[] => x <> y
	  | x::y::z::zs' => (x<>y) andalso (y<>z) andalso isRepeated(zs')
  in
      isRepeated(getStr(pat))
  end;

(**** 11 ****)
fun isMatch (value, pattern) =
  let fun check v p acc =
	case (v, p) of
	    (_, Wildcard) => acc
	  | (_, Variable s) => acc
	  | (Unit, UnitP) => acc
	  | (Const i, ConstP j) => if i=j then acc else false
	  | (Tuple vx, TupleP px) =>
	    if List.length(vx)=List.length(px)
	    then true
	    else false
	  | (Constructor(s1,v2), ConstructorP(s2,p2)) => if s1=s2 then true andalso (check v2 p2 acc) andalso acc else false		     
	  | (_,_) => false andalso acc
  in
      check value pattern true
  end;
			 
fun match (value, pattern) =
  if isMatch(value, pattern) then SOME [] else NONE;
  

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** solution to challenge problem ****)

