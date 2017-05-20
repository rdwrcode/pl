(* date is a tuple of type int*int*int  *)
(* (year,month,day)  *)
(* year > 0 *)
(* month: [1, 12] *)
(* day: [1, 31] *)
(* assume 1 year = 365 *)

fun is_older (date1: (int*int*int), date2: (int*int*int)) =
  if (#1 date1) < (#1 date2)
  then true
  else
      (if (#1 date1) = (#1 date2)
       then
	   if (#2 date1) < (#2 date2)
	   then true
	   else
	       (if (#2 date1) = (#2 date2)
		then if (#3 date1) < (#3 date2)
		     then true
		     else false
		else false)			      
      else false);

fun number_in_month (dates: (int*int*int) list, month: int) =
  if null dates
  then 0
  else if #2 (hd dates) = month
  then 1 + number_in_month(tl dates, month)
  else 0 + number_in_month(tl dates, month);

fun number_in_months (dates: (int*int*int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months));

fun dates_in_month (dates: (int*int*int) list, month: int) =
  if null dates
  then []
  else if #2 (hd dates) = month
  then (hd dates)::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month);

(* note: list append operator @ *)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd months)@dates_in_months(dates, tl months);

fun get_nth (sl: string list, n: int) =
  if n = 1
  then hd sl
  else get_nth(tl sl, n-1);

val monthnames = ["January","February", "March", "April", "May", "June",
		  "July", "August", "September", "October", "November", "December"];

fun name_of_month (month: int) =
  get_nth(monthnames, month);

fun date_to_string (date: (int*int*int)) =
  name_of_month(#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date);

(* return n such that the first n elements in the list added is less than sum, 
  but when n+1th element is added to the sum or more *)
fun sum_of_list (l: int list) =
  if null l
  then 0
  else (hd l) + sum_of_list(tl l);

fun sum_n_elements (l: int list, n: int) =
  if n = 1
  then hd l
  else (hd l)+sum_n_elements(tl l, n-1);

fun length_of_list (l: int list) =
  if null l
  then 0
  else 1 + length_of_list(tl l);

(* two-step solution *)
fun number_before_reaching_sum (sum: int, l: int list) =
  let fun sum_first_n(n: int, xs: int list) =
	if n = 1
	then hd xs
	else (hd xs) + sum_first_n(n-1, tl xs)
      fun compare (step: int, xs: int list) =
	if sum_first_n(step, xs) >= sum
	then step-1
	else compare(step+1, xs)		  
  in
      if null l
      then 0
      else compare(1, l)
		  
  end;

(* 9: assume not leap year  *)
fun what_month (day: int) =
  let val days_in_months = [31, 28, 31, 30, 31, 30,
			    31, 31, 30, 31, 30, 31]
  in
      1+number_before_reaching_sum(day, days_in_months)
  end;

fun month_range (day1: int, day2: int) =
  let fun days (from: int, to: int) =
	let val current = from
	in
	    if current > to	  
	    then []
	    else current::days(from+1, to)
	end
      fun map_day_to_month (days: int list) =
	if null days
	then []
	else what_month(hd days)::map_day_to_month(tl days)
  in
      map_day_to_month(days(day1, day2))
  end;

(* 11 *)
fun oldest (dates: (int*int*int) list) =
  if null dates
  then NONE
  else
      let
	  val tl_ans = oldest(tl dates)
	  fun oldest_nonempty (xs: (int*int*int) list) =
	    if null (tl xs)
	    then hd xs
	    else let val tl_ans = oldest_nonempty(tl xs)
		 in
		     if is_older(hd xs, tl_ans)
		     then hd xs
		     else tl_ans
		 end
      in
	  SOME (oldest_nonempty dates)
      end;

(* 12 challenge problem *)
(* TODO *)
fun is_same_date (d1: (int*int*int), d2: (int*int*int)) =
  (#1 d1 = #1 d2) andalso (#2 d1 = #2 d2) andalso (#3 d1 = #3 d2)

fun is_date_in_list (d: (int*int*int), dx: (int*int*int) list) =
  if is_same_date(d, hd dx)
  then true
  else is_date_in_list(d, tl dx)
		      
fun remove_duplicate (dx: (int*int*int) list) =
  if null dx
  then []
  else let val new_list = []
       in
	   if null (tl dx)
	   then
	       if is_date_in_list(hd dx, tl dx)
	       then remove_duplicate(tl dx)   
	       else (hd dx)::new_list
	   else
	       [hd dx]			 
       end
	   
fun number_in_months_challenge (dates: (int*int*int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months));

fun dates_in_months_challenge (dates: (int*int*int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd months)@dates_in_months(dates, tl months);

