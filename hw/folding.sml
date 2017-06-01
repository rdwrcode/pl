fun sum xs =
  case xs of
      [] => 0
    | x::xs' => x + sum xs';

fun concat xs =
  case xs of
      [] => ""
    | x::xs' => x ^ (concat xs);

fun sum2 acc xs =
  case xs of
      [] => acc
    | x::xs' => sum2 (acc+x) xs';

fun concat2 acc xs =
  case xs of
      [] => acc
    | x::xs' => concat2 (acc^x) xs';

fun myfoldl f acc xs =
  case xs of
      [] => acc
    | x::xs' => myfoldl f (f(x, acc)) xs';

fun sum3 xs = myfoldl (fn (x, acc) => acc+x ) 0 xs;

fun concat3 xs = myfoldl (fn (x, acc) => acc^x ) "" xs;

fun myfoldr f acc xs =
  case xs of
      [] => acc
    | x::xs' => f(x, (myfoldr f acc xs'));

val sum4 = myfoldl (fn (x,a) => x+a) 0;

val concat4 = myfoldr (fn (x,a) => x^a) "";

val sum5 = myfoldl Int.+ 0;

val concat5 = myfoldr String.^ "";

fun length xs = myfoldl (fn (_, a) => a+1) 0 xs;

fun rev xs = myfoldl List.:: [] xs;

fun map f xs = myfoldr (fn (x, a) => (f x)::a) [] xs;

fun app f xs = myfoldr (fn (x, _) => f x) () xs;

fun filter f xs = myfoldr (fn (x, a) => if f x then x::a else a) [] xs;

(* List.foldl *)

fun isEven i = if (i mod 2) = 0 then true else false;

fun isOdd i = if (i mod 2) = 1 then true else false;

(*
exception LengthNotMatch
fun merge (xs, ys) = if List.length(xs)=List.length(ys)
		     then List.foldl (fn ((x, y), i) => (i+x+y))
				 0
				 ListPair.zip(xs, ys)
		     else raise LengthNotMatch;
val test1 = merge ([1, 3, 5], [2, 4, 6]) = 21;

*)

fun mysum xs = foldl (fn (x,i) => x+i) 0 xs;
val test2 = mysum [1, 3, 5] = 9;

fun mlist xs ys = ListPair.zip(xs, ys);
val test3 = (mlist [1, 3, 5] [2, 4, 6]) = [(1,2),(3,4),(5,6)];

fun sumall xs = List.foldl (fn ((x, y),i) => (x+y+i)) 0 xs;
val test4 = sumall (mlist [1, 3, 5] [2, 4, 6]) = 21;

fun testzip xs = List.foldl
	       (fn ((x, y), i) => (isOdd(x) andalso isEven(y) andalso i))
	       true
	       xs;

val test5 = testzip (mlist [1, 3, 5] [2, 4, 6]) = true;

val test6 = testzip (mlist [1, 4, 5] [2, 4, 6]) = false;

(* foldl *)
fun sum1 xs =
  case xs of
      [] => 0
    | x::xs' => x + sum1 xs';

fun sum2 [] => 0
  | sum2 (x::xs') => x + sum2 xs';

fun sum3 xs = foldl (fn (x, acc) => x + acc) 0 xs;

val test10 = sum1 [1, 2, 3, 4] = 10;

val test11 = sum2 [1, 3, 5, 7] = 16;

val test12 = sum3 [1, 3, 5, 7] = 16;

			
