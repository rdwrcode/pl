(* fp:
 * avoid mutation
 * function as first class
 * style: encouraging recursion
 *)
fun double x = 2 * x;

fun incr x = x + 1;

val a_tuple = (double, incr, double(incr(3)));

(* higher order function *)
fun increment_n_times_lame (n, x) =
  if n = 0
  then x
  else 1 + increment_n_times_lame(n-1, x);

fun double_n_times_lame (n, x) =
  if n = 0
  then x
  else 2*double_n_times_lame(n-1, x);

fun nth_tail_lame (n, xs) =
  if n = 0
  then xs
  else tl (nth_tail_lame (n-1, xs));

(* val n_times = fn : ('a -> 'a) * int * 'a -> 'a *)
fun n_times (f, n, x) =
  if n = 0
  then x
  else f (n_times(f, n-1, x));

fun increment x = x + 1;

fun double x = 2 * x;

val x1 = n_times(double, 4, 7); (* instantiate 'a with int *)
val x2 = n_times(increment, 4, 7); (* instantiate 'a with int *)

(* polymorphic types and functions as arguments *)
(*
 * - fun function1 x = 5;
   val function1 = fn : 'a -> int
   - fun function2 x = "c";
   val function2 = fn : 'a -> string
   - fun function3 x = true;
   val function3 = fn : 'a -> bool
   - fun function4 x = x;
   val function4 = fn : 'a -> 'a 
 *)
(* hof are often polymorphic, but not always *)
fun times_until_zero (f, x) =
  if x = 0
  then 0
  else 1 + times_until_zero(f, f x);

(* some polymorphic functions are not higher-order functions *)
fun len xs =
  case xs of
      [] => 0
    | x::xs' => 1 + len xs';

(* better with _ *)
fun len xs =
  case xs of
      [] => 0
    | _::xs' => 1 + len xs';

(* 'a - alpha, any type is polymorphic *)				 

(* anonymous functions *)
fun triple x = 3 * x;
fun triple_n_times (n, x) = n_times(triple, n, x);
(* it can written with anonymous function *)
fun triple_n_times2 (n, x) =
  let fun triples x = 3 * x
  in
      n_times(triples, n, x)
  end;

(* it can be improved *)
fun triple_n_times3 (n, x) =
  n_times(let fun triple x = 3*x in triple end, n, x);

fun triple_n_times3 (n, x) =
  n_times((fn x => 3*x), n, x); (* expression not binding *)
(* fun is a function binding *)
(* cannot use an anonymous function for a recursive function *)
(*
 * fun triple x = 3 * x
 * val triple = fn x => 3 * x 
 *)
(* unnecessary function wrapping *)
fun nth_tail (n, xs) = n_times((fn y => tl y), n, xs);

(* this is better *)
fun nth_tail (n, xs) = n_times(tl, n, xs);
(*
if x then true else false;
(fn x => f x);
*)
    
(* List.rev *)
(* map and filter *)
fun mymap (f, xs) =
  case xs of
      [] => []
    | x::xs' => (f x)::mymap(f, xs');

val x1 = mymap((fn x => x + 1), [2, 3, 4]);
val x2 = mymap(hd, [[1, 2], [3, 4], [5, 6]]);

fun myfilter (f, xs) =
  case xs of
      [] => []
    | x::xs' => if f x
		then x::(myfilter (f, xs'))
		else myfilter(f, xs');

fun is_even v =
  (v mod 2 = 0);

fun all_even xs = myfilter(is_even, xs);

(* take one function as an argument to another function *)
(* 
  can pass several functions as arguments 
  can put functions in data structures
  can return functions as results
  can write higher-order functions that traverse your own data structures
*)

fun double_or_triple f =
  if f 7
  then fn x => 2*x
  else fn x => 3*x;

(* lexical scope *)
(* where the functions was defined (created), not where it was called *)
(* a function has two parts: code and its environment *)

fun f1 y =
  let val x = y + 1
  in
      fn z => x + y + z
  end;

fun f2 g =
  let val x = 3
  in
      g 2
  end;

(* why lexical scope? 
 * 1. function meaning does not depend on varaible names used.
 * 2. function can be type-checked and reasoned about where defined.
 * 3. closures can be easily store the data they need.
 *
 *)
(*
  lexical scope: use environment where function is defined
  dynamic scope: use environment where function is called 
 *)

fun allGreater (xs, n) = myfilter((fn x => x > n), xs);
fun nonNegatives xs = allGreater(xs, ~1);

(* val greaterThanX = fn : int -> int -> bool *)
fun greaterThanX x = fn y => y > x;

fun nonNegatives2 xs = myfilter(greaterThanX ~1, xs);

(*
 * a function body is not evaluated until the function is called.
 * String.size
 *)

fun allShorterThan (xs, s) =
  let
      val i = String.size s
  in
      myfilter(fn x => String.size x < i, xs)
  end;
(* e1; e2 *)
(* print "!" *)

(* fold, reduce, inject, etc. *)
(* why iterators again?
 * not built-in, just a programming pattern
 * separation of concerns 
 *)
(* fold(f, acc, [x1, x2, x3])
 * f(f(f(f(acc, x1), x2), x3), x4)
 *)
fun myfold (f, acc, xs) =
  case xs of
      [] => acc
    | x::xs' => myfold(f, f(acc, x), xs');

fun f1 xs = myfold((fn (x, y) => x+y), 0, xs);
(* sum of list *)
fun f2 xs = myfold((fn (x, y) => x andalso y >=0), true, xs);
(* are all elements non-negative? *)

fun f3 (xs, lo, hi) =
  myfold((fn (x, y) =>
	     x + (if y >= lo andalso y <= hi then 1 else 0)),
	 0, xs);

(* iterators made better *)
(*
 * because of closures and lexical scope, functions like map, filter, fold
 * are more powerful. 
 *)

(* compose 
 * ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
 *)
fun mycompose (f, g) =
  fn x => f(g x);

fun sqrt_of_abs i = Math.sqrt(Real.fromInt(abs i));
val sqrt_of_abs2 = Math.sqrt o Real.fromInt o abs;

(* o *)
infix !>
fun x !> f = f x;

fun sorted3_tuple (x, y, z) = z >= y andalso y >= x;
val t1 = sorted3_tuple (7, 9, 11);

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x;
val t2 = ((sorted3 7) 9) 11;

(* syntax sugar *)
val t3 = sorted3 7 9 11;

fun sorted3_nice x y z = z >= y andalso y >= x;
val t4 = sorted3_nice 7 9 11;

fun myfold2 f acc xs =
  case xs of
      [] => acc
    | x::xs' => myfold2 f (f(acc, x)) xs';

(* curry, closure, function in and function out: higher order function *)

			



	       




