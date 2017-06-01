fun sum1 xs =
  case xs of
      [] => 0
    | x::xs' => x + sum1 xs';

fun sum2 [] = 0
  | sum2 (x::xs) = x + sum2 xs;

fun sum3 xs = List.foldl (fn (x, acc) => x + acc) 0 xs;

val test10 = sum1 [1, 2, 3, 4] = 10;

val test11 = sum2 [1, 3, 5, 7] = 16;

val test12 = sum3 [1, 3, 5, 7] = 16;
