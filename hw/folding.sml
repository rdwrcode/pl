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



