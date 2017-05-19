(* the power of let expression and nested functions *)
(* version 1 *)
fun count (from: int, to: int) =
  if from = to
  then to::[]
  else from :: count(from+1, to);

fun countup_from1_v1 (x: int) =
  count(1, x);

(* version 2 *)
fun countup_from1_v2 (x: int) =
  let fun count (from: int, to: int) =
	if from = to
	then to::[]
	else from :: count(from+1, to)
  in
      count(1, x)
  end;

(* version 3 - final *)
(* remove the to argument, x is in the outer environment 
   of the local function count, it is available to the count *)
fun countup_from1_v3 (x: int) =
  let fun count (from: int) =
	if from = x
	then x::[]
	else from :: count(from+1)
  in
      count(1)
  end;
