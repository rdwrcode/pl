(* how to approach a problem recursively
 * make it simpler iteratively
 * what is the big deal between recursion and loop?
 * 
 *)

(* sum of a list of integers *)
fun sum1 xs =
  case xs of
      [] => 0
    | x::xs' => x + sum1 xs';

(* tail recursion of sum *)
fun sum2 xs =
  let fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', x+acc)
  in
      aux(xs, 0)
  end;

(* reverse a list *)
fun reverse1 xs =
  case xs of
      [] => []
    | x::xs' => (reverse1 xs') @ [x];

fun reverse2 xs =
  let fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', x::acc)
  in
      aux(xs, [])
  end;

(* factorial, assume 0 or positive integer as the input *)
fun fac1 (n: int) =
  if n = 0
  then 1
  else
      n * fac1(n-1);

(* tail recursion of factorial *)
fun fac2 n =
  let fun aux (n, acc) =
	if n = 0
	then acc
	else aux(n-1, n*acc)
  in
      aux(n, 1)
  end;

(* use pattern matching *)
fun fac3 n =
  case n of
      0 => 1
    | _ => n*fac3(n-1);
