(* higher order function *)
fun curry f x y = f (x, y);
fun uncurry f (x, y) = f x y;
fun compose (f, g) x = f (g x);

fun twice f x = f (f x);

(* base function to play *)
fun foo (a, x) = a*x + 10;
fun bar a x = a*x+20;

val curriedFoo = curry foo;
val composedFooBar = curriedFoo 5 o bar 1;
val uncurriedBar = uncurry bar;

val addTwo = twice(fn x => x + 1);

val test1 = composedFooBar 4 = 130;
val test2 = uncurriedBar (3, 4) = 32;
val test3 = bar 3 4 = 32;
val test4 = foo (3, 4) = 22;
val test5 = addTwo 5 = 7;
