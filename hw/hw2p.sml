(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* The end result need two pieces of information:
   1) true or false, if the string is in the list or not;
   2) if true, remove the string from the list and return the remaining list
   3-1) all_except_option("string", ["string"]) should return SOME []
   3-2) all_except_option("string", ["string", "str"]) should return SOME ["str"]
   3-3) all_except_option("str", ["string"]) should return NONE
 *)
fun all_except_option (s, xs) =
  let fun compare (sl, acc) =
	case sl of
	    [] => acc
	  | x::x' => compare(x', same_string(s, x) orelse acc)
      fun remaining (sl, acc) =
	case sl of
	    [] => acc
	  | x::x' => if same_string(s, x)
		     then remaining(x', acc)
		     else remaining(x', x::acc)
  in
      if compare(xs, false)
      then SOME(remaining(xs, []))
      else NONE
  end;

fun get_substitutions1 (xslist, s) =
  case xslist of
      [] => []
    | l::xslist' =>
      let val r = all_except_option(s, l)
      in
	  if isSome r
	  then valOf(r) @ get_substitutions1(xslist', s)
	  else get_substitutions1(xslist', s)
      end;

fun get_substitutions2 (xslist, s) =
  let fun check (xs, acc) =
	case xs of
	    [] => acc
	  | l::xs' => let val r = all_except_option(s, l)
		     in
			 if isSome r
			 then check(xs', valOf(r) @ acc)
			 else check(xs', acc)
		     end
  in
      check(xslist, [])
  end;

fun similar_names (xslist, namerecord) =
  let fun get_firstname fullname =
	case fullname of
	    {first, middle, last} => first
      fun generate_similar (xs, fullname) =
	case fullname of
	    {first, middle, last} =>
	    case xs of
		[] => []
	      | x::xs' => {first=x, middle=middle, last=last}::generate_similar(xs', fullname)
  in
      let val firstname = get_firstname namerecord
	  val firstlist = get_substitutions2(xslist, firstname)
      in
	  namerecord::generate_similar(firstlist, namerecord)
      end
  end;
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
exception NotEnoughCards
	      
fun card_color ((suit, rank)) =
  case suit of
       Diamonds => "red"
     | Hearts => "red"
     | _ => "black";

fun card_value ((suit, rank)) =
  case rank of 
      Ace => 11
    | Jack => 10
    | Queen => 10
    | King => 10
    | x => x;

(* TODO where to raise the exception *)
fun remove_card (cs, c, e) =
  case cs of
      [] => []
    | x::cs' =>
      let val num = 0
	  if c = x
	  then num + 1
	  else num
      in
	  if c = x andalso num = 0
	  then remove_card(cs', c, e)
	  else x::remove_card(cs', c, e)
      end;

fun all_same_color (cs) =
  case cs of
      [] => true
    | x::cs' => 
		    [] => true
		  | y::ds => card_color(x) = card_color(y) andalso all_same_color(ds);


				
						 
      
  

	  
			     
			    
		    
		  
