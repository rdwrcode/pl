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

val only_capitals =
    List.filter (fn y => Char.isUpper(String.sub(y, 0)));

val longest_string1 =
    foldl (fn (x, y) => if String.size x > String.size y then x else y) "";

val longest_string2 =
    foldl (fn (x, y) => if String.size x >= String.size y then x else y) "";

fun longest_string_helper f xs=
  foldl (fn (x, y) => if f (String.size x, String.size y) then x else y) "" xs;

val longest_string3 = longest_string_helper (fn (x, y) => x > y);

val longest_string4 = longest_string_helper (fn (x, y) => x >= y);

val longest_capitalized = longest_string1 o only_capitals;

val rev_string = implode o List.rev o explode;

fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' => case f x of
		    NONE => first_answer f xs'
		  | SOME v => v;

fun all_answers f xs =
  let
      fun aux acc rem =
	case rem of
	    [] => SOME acc
	  | x::rem' => case f x of
			   NONE => NONE
			 | SOME v => aux (acc@v) rem';
  in
      aux [] xs
  end;

val count_wildcards = g (fn _ => 1) (fn _ => 0);

val count_wild_and_variable_lengths = g (fn _ => 1) (fn str => String.size str);

fun count_some_var(name: string, p: pattern) =
  g (fn _ => 0) (fn str => if str = name then 1 else 0) p;

fun check_pat(p: pattern)  =
  let
      fun names(p: pattern) =
	  case p of
	      Variable x => [x]
	    | TupleP ps  => List.foldl (fn (p, i) => (names p) @ i) [] ps
	    | ConstructorP (_, p) => names p
	    | _ => [];
      fun distinct(xs: string list) =
	case xs of
	    [] => true
	  | x::xs' => if List.exists (fn y => x = y) xs' then false else distinct xs';
  in
      distinct (names p)
  end;

fun match(v: valu, p: pattern) =
  case (p, v) of
      (Wildcard, _) => SOME []
    | (Variable s, _) => SOME [(s, v)]
    | (UnitP, Unit) => SOME []
    | (ConstP i, Const vi) => if i = vi then SOME [] else NONE
    | (TupleP ps, Tuple pv) => if List.length(pv) <> List.length(ps) then NONE
			       else all_answers match (ListPair.zip(pv, ps))
    | (ConstructorP (s1, p), Constructor (s2, v)) => if s1 = s2 then match(v, p) else NONE
    | _ => NONE;

fun first_match v ps =
  let fun curry f x y = f (x, y)
  in
      SOME (first_answer ((curry match) v) ps) handle NoAnswer => NONE
  end;
(*
fun typecheck_patterns(typs: (string * string * typ) list, ps: pattern list) =
  let
      fun isAcceptedTyp baseTyp testeeTyp =
	case (baseTyp, testeeTyp) of
	    (Anything, _) => true
	  | (UnitT, UnitT) => true
	  | (IntT, IntT) => true
	  | (TupleT bS, TupleT tS) => if List.length(bS) <> List.length(tS) then false
				      else List.all (fn (bItem, tItem) => isAcceptedTyp bItem tItem) (ListPair.zip(bS, tS))
	  |  (Datatype s1, Datatype s2) => s1 = s2
	  | _ => false;
      fun analyze_type(p: pattern) =
	(case p of
	    Wildcard => SOME Anything
	  | Variable s => SOME Anything
	  | UnitP => SOME UnitT
	  | ConstP i => SOME IntT
	  | TupleP ps => SOME (TupleT (map (fn p: pattern => valOf (analyze_type p)) ps) )
	  | ConstructorP (s1, p) => let
	      val matchTyp = valOf (List.find (fn x => s1 = #1 x) typs);
	      val consP = valOf (analyze_type p);
	  in
	      if isAcceptedTyp (#3 matchTyp) consP then SOME (Datatype (#2 matchTyp)) else NONE
	  end
	) handle Option => NONE;
      
      exception JoinFail;
      
      fun join_typ(t1: typ, t2: typ) =
	case (t1, t2) of
	    (Anything, t2) => t2
	  | (t1, Anything) => t1
	  | (UnitT, UnitT) => UnitT
	  | (IntT, IntT) => IntT
	  | (TupleT s1, TupleT s2) => if List.length(s1) <> List.length(s2) then raise JoinFail
				      else TupleT (map (fn (t1: typ, t2: typ) => join_typ(t1, t2)) (ListPair.zip(s1, s2)))
	  | (Datatype s1, Datatype s2) => if s1 <> s2 then raise JoinFail
					  else Datatype s1
	  | _ => raise JoinFail;				
	    
      fun get_typ(ps: pattern list, cur: typ) =
	case ps of
	    [] => SOME cur
	  | p::ps' => let
	      val pTyp = analyze_type(p);
	  in
	      if isSome pTyp then get_typ(ps', join_typ(valOf pTyp, cur)) handle JoinFail => NONE else NONE
	  end;
  in
      get_typ(ps, Anything)
  end;
*)

fun typecheck_patterns(typs: (string * string * typ) list, ps: pattern list) =
  let
      
      exception JoinFail;
      
      fun join_typ(t1: typ, t2: typ) =
	case (t1, t2) of
	    (Anything, t2) => t2
	  | (t1, Anything) => t1
	  | (UnitT, UnitT) => UnitT
	  | (IntT, IntT) => IntT
	  | (TupleT s1, TupleT s2) => if List.length(s1) <> List.length(s2) then raise JoinFail
				      else TupleT (map (fn (t1: typ, t2: typ) => join_typ(t1, t2)) (ListPair.zip(s1, s2)))
	  | (Datatype s1, Datatype s2) => if s1 <> s2 then raise JoinFail
					  else Datatype s1
	  | _ => raise JoinFail;				
	    
      fun analyze_type(p: pattern) =
	(case p of
	    Wildcard => SOME Anything
	  | Variable s => SOME Anything
	  | UnitP => SOME UnitT
	  | ConstP i => SOME IntT
	  | TupleP ps => SOME (TupleT (map (fn p: pattern => valOf (analyze_type p)) ps) )
	  | ConstructorP (s1, p) => let
	      val matchTyp = valOf (List.find (fn x => s1 = #1 x) typs);
	      val consP = valOf (analyze_type p);
	      val type_matched = (join_typ((#3 matchTyp), consP); true) handle JoinFail => false;
	  in
	      if type_matched then SOME (Datatype (#2 matchTyp)) else NONE
	  end
	) handle Option => NONE;

      fun get_typ(ps: pattern list, cur: typ) =
	case ps of
	    [] => SOME cur
	  | p::ps' => let
	      val pTyp = analyze_type(p);
	  in
	      if isSome pTyp then get_typ(ps', join_typ(valOf pTyp, cur)) handle JoinFail => NONE else NONE
	  end;
  in
      get_typ(ps, Anything)
  end;
