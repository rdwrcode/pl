(* Coursera Programming Languages, Homework 3, Provided Code *)

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

(*val g = fn : (unit -> int) -> (string -> int) -> pattern -> int *)
(* f1 = fn : unit -> int; f2 = fn : string -> int *)
(* There is only one value belongs to unit type: (). Therefore fn : unit -> int means
   this function takes nothing but a () and returns an int *)					    
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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)



(* Homework 3
   Author: Xiyu Fu
   Contact: xiyu.fu@student.kuleuven.be
   Date: 2017/4/22
 *)

(*1. only_capitals(string list ) returns a string list with strings that start with capital letters
  List.filter is in curry form*)

fun only_capitals( str_lst ) =
  List.filter(fn a => Char.isUpper(String.sub(a, 0))) str_lst

(* 2. longest_string1(string list) returns the longest string in the list *)

fun longest_string1( str_lst ) =
  foldl (fn (s1, s2) => if String.size(s1) > String.size(s2) then s1 else s2 ) "" str_lst

(* 3. longest_string2 (string list) returns the longest string in the list, in case of 
   a tie, returns the one closer to the end of list *)

fun longest_string2( str_lst ) =
  foldl (fn (s1,s2) => if String.size(s1) >= String.size(s2) then s1 else s2) "" str_lst

(* 4. *)

fun longest_string_helper f str_lst =
  foldl (fn (s1, s2) => if f(String.size(s1), String.size(s2))
			then s1 else s2)
	"" str_lst

val longest_string3 = (longest_string_helper (fn (a, b) => a > b ))	
	
val longest_string4 = (longest_string_helper (fn (a, b) => a >= b))


(* 5. longest_capitalized (string list) returns the longest string that has a capital letter 
   at the beginning *)			  

val longest_capitalized = longest_string1 o only_capitals

(* 6. rev_string(string)  returns the string that is the same characters in reverse order.
   using explode string->char list; implode char list->string and rev 'a list->'a list *)

fun rev_string( str ) = (String.implode o List.rev o String.explode) str

(* 7. first_answer  ('a -> 'b option) -> 'a list -> 'b *)							   

fun first_answer f lst =
  case lst of
      [] => raise NoAnswer
    | top::rest => case f(top) of
		       SOME b => b
		     | NONE => first_answer f rest

					    
(* 8. all_answers ('a -> 'b list option) -> 'a list -> 'b list option *)				  

fun all_answers f lst =
  let fun accumulator (acc, a_lst) =
      case a_lst of
	  [] => SOME acc
	| top_a::rest => case f(top_a) of
			     NONE => NONE
			   | SOME b_lst => accumulator(acc@b_lst, rest)
  in
      accumulator ([], lst)
  end
      
(* 9.a count_wildcards(pattern) returns how many Wildcard patterns it contains.*)

fun count_wildcards ( p ) =
  g (fn () => 1) (fn a => 0) p

(* 9.b count_wild_and_variable_lengths(pattern) returns the number of Wildcard patterns it
   contains plus the sum of the string lengths of all the variables in the variable 
   patterns it contains. *)

fun count_wild_and_variable_lengths ( p ) =
  g (fn() => 1) (fn a => String.size(a)) p

(* 9.c count_some_var(string, pattern) returns the number of times the string appears as a
   variable in the pattern. *)

fun count_some_var ( str, p ) =
  g (fn () => 0) (fn s => if s = str then 1 else 0) p

(* 10.  check_pat that takes a pattern and returns true if and only if all the variables
        appearing in the pattern are distinct from each other *)

fun check_pat ( p ) =
  let fun get_lst ( ps ) =
	case ps of
	    Variable s => [s]
	  | TupleP p_lst => List.foldl (fn (p1, lst1) => get_lst(p1)@lst1) [] p_lst
	  | ConstructorP (_, p1) => get_lst(p1)
	  | _ => [] 
      fun check_repeat (str_lst:string list) =
	case str_lst of
	    [] => true
	  | top::rest => if List.exists (fn x => x = top) rest
			 then false
			 else check_repeat (rest)
  in
      (check_repeat o get_lst) p
  end
      
(* 11. match(valu * pattern) returns a (string * valu) list option *)			      
(* patten matching (v,p) is a better choice compared with nested pattern matching. *)
      
fun match ( v, p ) =
  case p of
      Wildcard => SOME []
    | Variable s => SOME [(s, v)]
    | UnitP => if v = Unit then SOME [] else NONE
    | ConstP cp => (case v of
		       Const cv => if cv = cp then SOME [] else NONE
		     | _ => NONE)
    | TupleP ps => (case v of
			Tuple vs => if List.length(ps) = List.length(vs) then all_answers match (ListPair.zip(vs,ps)) else NONE
		      | _ => NONE)
    | ConstructorP (s1, p1) => (case v of
				    Constructor (s2, v1) => if s1 = s2 then match(v1,p1) else NONE
				  | _ => NONE)
    
(* 12. first_match valu pattern list returns a (string * valu) list option *)			      

fun first_match v p_lst =
  SOME (first_answer (fn p => match (v, p)) p_lst)
(*This is an example of closure. The anonymous function takes data v with it *)
  handle NoAnswer => NONE

(* 13. Challenge problem. Couldn't understand what it is *)

			 
