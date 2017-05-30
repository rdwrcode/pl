fun f x = x + x;
val y = 2;
fun g x = y * x;
(* both f and g are equivalent *)

fun s (f, x) = (f x) + (f x);
fun t (f, x) = y * (f x);
(* both s and t are not equivalent *)
(* because function f may have side-effects *)

(* both s and t are equivalent if f has no side-effect *)
s (fn i => (print "hi\n"; i), 7);

fun u x = x andalso w x;
fun v x = if x then w x else false;
(* both u and v are equivalent *)

(* equivalent but performance differs greatly *)
fun max1 xs =
  case xs of
      [] => raise Empty
    | x::[] => x
    | x::xs' =>
      let val y = max1 xs'
      in
	  if x > y then x else y
      end;

(* slower *)
fun max2 xs =
  case xs of
      [] => raise Empty
    | x::[] => x
    | x::xs' =>
      if x > max xs' then x else max xs';

(* Programming language equivalence *)
(* asymptotic equivalence: algorithm, runtime, efficiency with large inputs *)
(* system equivalence *)
