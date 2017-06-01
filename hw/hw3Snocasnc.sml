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
val only_capitals = List.filter ( Char.isUpper o (fn s => String.sub(s,0)) )
				
val longest_string1 = List.foldl ( fn ( x , y ) => if ( String.size(x) > String.size(y) ) then x else y ) ""
			    
val longest_string2 = List.foldl ( fn ( x , y ) => if ( String.size(x) < String.size(y) ) then y else x ) ""
			    
fun longest_string_helper f = List.foldl ( fn ( x , y ) => if f( String.size(x) , String.size(y) ) then x else y ) ""
				    
val longest_string3 = longest_string_helper ( fn ( x , y ) => x > y )
					    
val longest_string4 = longest_string_helper ( fn ( x , y ) => x >= y )
					    
val longest_capitalized = longest_string1 o only_capitals
						
val rev_string = String.implode o rev o String.explode
				     
fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => case f(x) of
		    NONE => first_answer f xs'
		  | SOME v => v
				  
fun all_answers f xs =
  let fun helper ( acc , xs ) =
	case xs of
	    [] => SOME acc
	  | x::xs'  => case f(x) of
			   NONE => NONE
			 | SOME lst => helper( acc @ lst , xs' )	  
  in helper( [] , xs )
  end
      
val count_wildcards = g ( fn () => 1 ) ( fn _ => 0)
			
val count_wild_and_variable_lengths = g ( fn () => 1 ) String.size

fun count_some_var ( s , p ) = g ( fn () => 0 ) ( fn x => if x=s then 1 else 0 ) p
				 
val check_pat =
  let fun helper1 p =
	case p of
	    Variable x => [x]
	 | TupleP ps => List.foldl ( fn ( p , xs ) => helper1(p) @ xs ) [] ps 
	 | ConstructorP ( _ , p ) => helper1(p)
	 | _ => []
      fun helper2 xs =
	case xs of
	    [] => true
	  | x::xs' => if List.exists ( fn y => x=y ) xs' then false else helper2(xs')
  in helper2 o helper1 
  end

fun match ( v , p ) =
  case ( v , p ) of
      ( _ , Wildcard ) => SOME []
    | ( v , Variable s ) => SOME [( s , v )]
    | ( Unit , UnitP ) => SOME [] 
    | ( Const x , ConstP y ) => if x=y then SOME [] else NONE
    | ( Tuple vs , TupleP ps ) => if List.length(vs)=List.length(ps) then all_answers match ( ListPair.zip(vs , ps) ) else NONE
    | ( Constructor ( s1 , v ) , ConstructorP ( s2 , p ) ) => if s1=s2 then match( v , p ) else NONE
    | _ => NONE

fun first_match v ps =
  SOME ( first_answer ( fn x => match ( v , x ) ) ps ) handle NoAnswer => NONE  
	    
 
