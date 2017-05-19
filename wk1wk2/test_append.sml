(* the situation is that you can't tell if it is an alias or identical copy *)
(* ys is returne as an alias, then means the new list could be affected
   if ys is mutated somewhere else.
 *)
(* in ML it is OK *)
(* but in languages with mutable data, that may become an issue. *)
fun append (xs: int list, ys: int list) =
  if null xs
  then ys
  else hd (xs) :: append (tl (xs), ys);
