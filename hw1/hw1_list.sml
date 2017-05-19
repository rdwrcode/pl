(* date is a tuple of type int*int*int  *)
(* year/month/day  *)
(* year > 0 *)
(* month: [1, 12] *)
(* day: [1, 31] *)
fun genDateTuple (y: int, m: int, d: int) =
  (y, m, d);

fun genDateList (y: int, m: int, d: int) =
  [y, m, d];

fun is_smaller (x: int, y: int) =
  if x < y
  then true
  else false;

fun is_older (d1: int list, d2: int list) =
  if hd d1 < hd d2
  then true
  else if hd (tl d1) < hd (tl d2)
  then true
  else if hd (tl (tl d1)) < hd (tl (tl d2))
  then true
  else false;



