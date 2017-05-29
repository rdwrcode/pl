(* Dan Grossman, Coursera PL, HW2 Provided Code *)
(* Solution added by Hailin *)

(*
officiate: Your function returns an incorrect result when the game should end due to the sum of cards in the player's hand exceeding the goal. [incorrect answer]
officiate: the game ends with no more move and the held-cards are of different colors. [incorrect answer]
officiate EXCEPTIONS: Your function does not throw a correct exception when the player attempts to discard card not in their hand. [NO RAISED EXCEPTION]
3a tests failed to run (most likely caused by an incorrect function signature or unimplemented function in the submission)
3b tests failed to run (most likely caused by an incorrect function signature or unimplemented function in the submission)
Used illegal functions in the following problems: ['get_substitutions1', 'get_substitutions2', 'similar_names'] 
 *)


(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* The end result need two pieces of information:
   1) true or false, if the string is in the list or not;
   2) if true, remove the string from the list and return the remaining list *)
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
	      | x::xs' => generate_similar(xs', fullname) @ [{first=x, middle=middle, last=last}]
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
exception GameOver of card list

fun card_color1 card =
  case card of
       (Diamonds, _) => Red
     | (Hearts, _) => Red
     | _ => Black;

fun card_value1 card =
  case card of 
      (_, Ace) => 11
    | (_, Num x) => x  
    | _ => 10;

fun card_color (suit, rank) =
  case suit of
       Diamonds => Red
     | Hearts => Red
     | _ => Black;

fun card_value (suit, rank) =
  case rank of 
      Ace => 11
    | Num x => x  
    | _ => 10;

(* remove the first c in the list cs *)
fun remove_card (cs, c, e) =
  let
      fun hasCard xs =
	case xs of
	    [] => false
	  | x::xs' => c = x orelse hasCard(xs')
      fun removeFirst (xs, num) =
	case xs of
	    [] => []
	  | x::xs' => if x = c andalso num = 0
		      then removeFirst(xs', num+1)
		      else x::removeFirst(xs', num)
  in
      if hasCard(cs)
      then removeFirst(cs, 0)
      else raise e
  end;

fun all_same_color cx =
  case cx of
      [] => true
    | x::[] => true
    | x::y::[] => card_color x = card_color y
    | x::y::z::zx' => let val colorx = card_color x
			  val colory = card_color y
			  val colorz = card_color z
		      in
			  colorx = colory andalso
			  colory = colorz andalso
			  all_same_color(zx')
		      end;

fun sum_cards cx =
  let
      fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', card_value(x)+acc)
  in
      aux(cx, 0)
  end;

fun score (cx, goal) =
  let
      val sum = sum_cards(cx)
      val pre_score =
	if sum > goal
	then 3 * (sum - goal)
	else goal -sum
  in
      if all_same_color(cx)
      then pre_score div 2
      else pre_score
  end;				

fun officiate (cards, moves, goal) =
  let
      fun heldcards (c, m, hl) =
	case m of
	    Draw => c::hl
	  | Discard x => remove_card(hl, x, IllegalMove)
      fun isDraw m =
	case m of
	    Draw => true
	  | _ => false
      fun play (cl, ml, hl) =
	case ml of
	    [] => hl (* no more moves *)
	  | m::mx' => case cl of
			     [] => hl (* no more cards *)
			   | c::cx' => let val hnow = heldcards(c, m, hl)
				       in
					   if sum_cards(hnow) <= goal
					   then
					       (if isDraw m
					       then play(cx', mx', hnow)
					       else play(c::cx', mx', hnow))
					   else
					       hnow
				       end
  in
      score(play(cards, moves, []), goal)
  end;

(* challenge problems *)
fun card_value_ace1 ((suit, rank)) =
  case rank of 
      Ace => 1
    | Num x => x  
    | _ => 10;

fun sum_cards_ace1 cx =
  let
      fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', card_value_ace1(x)+acc)
  in
      aux(cx, 0)
  end;

fun score_challenge (cx, goal) =
  let
      val sum = sum_cards(cx)
      val sum_ace1 = sum_cards_ace1(cx)		   
      val pre_score =
	  if sum > goal
	  then 3 * (sum - goal)
	  else goal - sum	 
      val pre_score_ace1 =
	  if sum_ace1 > goal
	  then 3 * (sum_ace1 - goal)
	  else goal - sum_ace1 
  in
      if pre_score < pre_score_ace1
      then
	  if all_same_color(cx)
	  then Int.div(pre_score, 2)
	  else pre_score
      else
	  if all_same_color(cx)
	  then Int.div(pre_score_ace1, 2)
	  else pre_score_ace1
  end;