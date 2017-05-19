fun swap (pr: int*bool) =
  (#2 pr, #1 pr);

fun sum_two_pairs (pr1: int*int, pr2: int*int) =
  (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2);

fun sum_list(xs: int list) =
  if (null xs)
  then 0
  else (hd xs) + sum_list(tl xs);

fun product_list(xs: int list) =
  if null xs
  then 1
  else (hd xs)*product_list(tl xs);

