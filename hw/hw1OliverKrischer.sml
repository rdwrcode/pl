(* Programming Languages, Part A *)
(* Homework1 solution by OKrischer *)

(* checks if first argument is a date that comes before the second argument *)
(* is_older = fn : (int * int * int) * (int * int * int) -> bool *)
fun is_older (date1 : int * int * int, date2 : int * int * int) =
  if #1 date1 < #1 date2 then true
  else if #1 date1 = #1 date2 andalso #2 date1 < #2 date2 then true
  else if #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2 then true
  else false

(* returns how many dates in the list are in the given month *)
(* number_in_month = fn : (int * int * int) list * int -> int *)
fun number_in_month (dates : (int * int * int) list, month : int) =
  if null dates then 0
  else if #2 (hd dates) = month then 1 + number_in_month (tl dates, month)
  else number_in_month (tl dates, month)

(* returns how many dates in the list are in any of the given months *)
(* number_in_months = fn : (int * int * int) list * int list -> int *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null months then 0
  else number_in_month (dates, hd months) + number_in_months (dates, tl months)

(* returns all given dates that are in the given month as a list *)
(* dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates then []
  else if #2 (hd dates) = month then (hd dates) :: dates_in_month (tl dates, month)
  else dates_in_month (tl dates, month)

(* returns all given dates that are in one of the given months as a list *)
(* dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null months then []
  else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

(* returns the nth element of a list of strings *)
(* get_nth = fn : string list * int -> string *)
fun get_nth (strings: string list, n: int) =
  let fun nth (strings: string list, count: int) =
    if null strings orelse n < count then "exception"
    else if n = count then hd strings
    else nth (tl strings, count + 1)
  in
    nth (strings, 1)
  end

(* returns a date string of the form "January 20, 2013" *)
(* date_to_string = fn : int * int * int -> string *)
fun date_to_string (year: int, month: int, day: int) =
  let val month_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth (month_list, month) ^ " " ^ Int.toString day ^ ", " ^ Int.toString year
  end

(* return an int n such that the first n elements of the list add to less than sum *)
(* number_before_reaching_sum = fn : int * int list -> int *)
fun number_before_reaching_sum (sum: int, xs: int list) =
  let fun add (xs: int list, added: int, count: int) =
    if null xs then 0
    else if (hd xs + added) >= sum then count
    else add (tl xs, added + hd xs, count + 1)
  in
    add (xs, 0, 0)
  end

(* returns the month for any given day in a year *)
(* what_month = fn : int -> int *)
fun what_month (day: int) =
  let val days_per_month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
    number_before_reaching_sum (day, days_per_month) + 1
  end

(* returns an int list with the month of all days within a range of days *)
(* month_range = fn : int * int -> int list *)
fun month_range (day1: int, day2: int) =
  let fun range (from: int, to: int) =
    if from > to then []
    else if from = to then [what_month(to)]
    else what_month(from) :: range (from + 1, to)
  in
    range (day1, day2)
  end
  

(* evaluates to NONE if the list is empty or to SOME of the oldest date *)
(* oldest = fn : (int * int * int) list -> (int * int * int) option *)
fun oldest (dates: (int * int * int) list) =
  if null dates then NONE
  else 
    let fun inner_oldest (dates: (int * int * int) list, oldest_date: (int * int * int)) =
          if null (tl dates) then 
            if is_older (hd dates, oldest_date) then SOME (hd dates) else SOME oldest_date
          else if is_older (hd dates, oldest_date) then inner_oldest (tl dates, hd dates)
          else inner_oldest (tl dates, oldest_date)
    in
      inner_oldest (dates, hd dates)
    end
