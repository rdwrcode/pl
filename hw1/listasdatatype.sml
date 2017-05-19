fun sum_list xs =
  case xs of
      [] => 0
    | x::xs' => x + sum_list xs'

fun append (xs, ys) =
  case xs of
      [] => ys
    | x::xs' => x :: append(xs', ys)

fun mystery x =
  case x of
      [] => true
    | x::xs => false

fun sum_triple triple =
  case triple of
      (x, y, z) => x + y + z

fun full_name r =
  case r of {first=x, middle=y, last=z} => x ^ " " ^ y ^ " " ^ z

fun sum_triple_v2 triple =
  let val (x, y, z) = triple
  in
      x + y + z
  end

fun full_name_v2 r =
  let val {first=x, middle=y, last=z} = r
  in
      x ^ " " ^ y ^ " " ^ z
  end

fun sum_triple_best (x, y, z) = x + y + z

fun full_name {first=x, middle=y, last=z} = x ^ " " ^ y ^ " " ^ z

fun strange x =
  case x of
      (1, b, c) => #2 x + 10
    | (a, b, c) => a * c

(* int*`a*int -> int *)
fun partial_sum (x, y, z) = x + z

				    
fun partial_name {first=x, middle=y, last=z} =
  x ^ " " ^ z

(* ''a * ''a -> string *)
fun same_thing (x, y) =
  if x=y then "yes" else "no";

(* int -> string *)
fun is_three x =
  if x=3 then "yes" else "no";

fun nondecreasing xs = (* int list -> bool *)
  case xs of
      [] => true
    | _::[] => true
    | head::(neck::rest) => head <= neck
			    andalso nondecreaing (neck::rest);
datatype sgn = P | N | Z;

fun multisign (x1, x2) =
  let fun sign x =
	if x = 0
	then Z else if x > 0
	then P
	else N
  in
      case (sign x1, sign x2) of
	  (Z, _) => Z
	| (_,Z) => Z
	| (P,P) => P
	| (N,N) => P
	| _ => N
  end;

fun length xs =
  case xs of
      [] => 0
    | _::xs' => 1 + length xs';

			   
						 
