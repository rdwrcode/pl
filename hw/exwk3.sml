(* nested *)
exception ListLengthMismatch;
	      
fun zip list_triple =
  case list_triple of
      ([], [], []) => []
    | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip(tl1, tl2, tl3)
    | _ => raise ListLengthMismatch;

fun unzip lst =
  case lst of
      [] => ([], [], [])
    | (a, b, c)::tl => let val (l1, l2, l3) = unzip tl
		       in
			   (a::l1, b::l2, c::l3)
		       end;

(* nested pattern matching *)
fun nondecreasing xs =
  case xs of
      [] => true
    | x::[] => true (* one element list *)
    | head::neck::rest => head <= neck andalso nondecreasing (neck::rest);

datatype sgn = P | Z | N;
(* multiplication sign *)
fun multsign (x1, x2) =
  let fun sign x = if x=0 then Z else if x > 0 then P else N
  in
      case (sign x1, sign x2) of
	  (Z, _) => Z
	| (_, Z) => Z
	| (P, N) => N
	| (N, P) => N
	| _ => P
  end;

(* wild cards are good style *)
fun len xs =
  case xs of
      [] => 0
    | _::xs' => 1 + len xs';

(* function patterns *)
datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp;
fun eval e =
  case e of
      Constant i => i
    | Negate e2 => ~ (eval e2)
    | Add (e1, e2) => (eval e1) + (eval e2)
    | Multiply (e1, e2) => (eval e1) * (eval e2);

(* function patterns *)
(*
 fun f p = e
   | f p2 = e2 
*)
(*
fun eval2 (Constant i) => i
    | eval2 (Negate e2) => ~ (eval2 e2)
    | eval2 (Add (e1, e2)) => (eval2 e1) + (eval2 e2)
    | eval2 (Multiply (e1, e2)) => (eval2 e1) * (eval2 e2);
*)

fun append ([], ys) = ys
  | append (x::xs, ys) = x::append(xs, ys);

(*
 * exception
 *)
fun hd xs =
  case xs of
      [] => raise List.Empty
    | x::_ => x;

exception MyUndesirableCondition;
exception MyOtherException of int * int;

fun mydiv (x, y) =
  if y = 0
  then raise MyUndesirableCondition
  else x div y;

fun maxlist (xs, ex) =
  case xs of
      [] => raise ex
    | x::[] => x
    | x::xs' => Int.max(x, maxlist(xs', ex));

val x = maxlist([3, 4, 5], MyUndesirableCondition)
	handle MyUndesirableCondition => 42;
val w = maxlist([], MyUndesirableCondition)
	handle MyUndesirableCondition => 42;

(* e1 handle exn => e2 *)

(* tail recursion *)
fun fact n =
  if n = 0
  then 1
  else n * fact(n -1);

val x = fact 3;
(*
 * 
  fact 3 
=> 
  fact 3: 3*
  fact 2
=> 
  fact 3: 3*
  fact 2: 2*
  fact 1
=> 
  fact 3: 3*
  fact 2: 2*
  fact 1: 1*
  fact 0
=> 
  fact 3: 3*
  fact 2: 2*
  fact 1: 1*
  fact 0 = 1
=> 
  fact 3: 3*
  fact 2: 2*
  fact 1 = 1*1
=> 
  fact 3: 3*
  fact 2 = 2*1
=> 
  fact 3 = 3*2

 *)

fun fact2 n =
  let fun aux (n, acc) =
	if n = 0
	then acc
	else aux(n-1, n*acc)
  in
      aux(n, 1)
  end;

(*
  fact 3
=>
  aux(3, 1)
=>
  aux(2, 3)
=> 
  aux(1, 6)
=> 
  aux(0, 6)
*)

fun rev xs =
  case xs of
      [] => []
    | x::xs' => (rev xs') @ [x];
(* append operator @ always copy *)

fun rev2 xs =
  let fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', x::acc)
  in
      aux(xs, [])
  end;
