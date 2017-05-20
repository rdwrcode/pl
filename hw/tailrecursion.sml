(* tail recursion *)
fun fact n =
  if n = 0
  then 1
  else n*fact(n-1);

val x = fact 3;

fun factorial n =
  let fun aux (n, acc) =
	if n = 0
	then acc
	else aux(n-1, acc*n)
  in
      aux(n, 1)
  end;

val y = factorial 3;

(* sum of the list *)
fun sum xs =
  case xs of
      [] => 0
    | x::xs' => x + sum xs';

(* tail recursion *)
fun sum_of_list xs =
  let fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', x+acc)
  in
      aux(xs, 0)
  end;

(* reverse a list, slower if big list *)
fun rev xs =
  case xs of
      [] => []
    | x:xs' => (rev xs') @ [x];

(* tail recursion *)
fun reverse_list xs =
  let fun aux (xs, acc) =
	case xs of
	    [] => acc
	  | x::xs' => aux(xs', x::acc)
  in
      aux(xs, [])
  end;
