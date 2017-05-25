fun is_older (d1: int*int*int, d2: int*int*int) =
  (#1 d2) > (#1 d1) andalso (#2 d2) > (#2 d1) andalso (#3 d2) > (#3 d1)

fun number_in_month (xs: (int * int * int) list, month: int) =
  let fun counter(lt: (int * int * int) list, count: int) =
    if null lt
    then count
    else if (#2 (hd(lt))) = month
    then counter(tl(lt), count + 1)
    else counter(tl(lt), count)
  in
    counter(xs, 0)
  end

fun number_in_months (xs: (int * int * int) list, months: int list) =
  let fun counter(count: int, month_lst: int list) =
    if null month_lst
    then count
    else counter(count + number_in_month(xs, hd(month_lst)), tl(month_lst))
  in
    counter(0, months)
  end

fun dates_in_month(xs: (int * int * int) list, month: int) =
  let fun counter(lst: (int * int * int) list, dates: (int * int * int) list) =
    if null lst
    then rev dates
    else if (#2 (hd(lst))) = month
    then counter(tl(lst), (hd(lst))::dates)
    else counter(tl(lst), dates)
  in
    counter(xs, [])
  end

fun dates_in_months(xs: (int * int * int) list, months: int list) =
  let fun counter(month_lst: int list, dates: (int * int * int) list) =
    if null month_lst
    then rev dates
    else counter(tl(month_lst), (dates_in_month(xs, hd(month_lst)) @ dates))
  in
    counter(months, [])
  end

fun get_nth (words: string list, index: int) =
  let fun counter(word_lst: string list, count: int) =
    if count = index
    then hd(word_lst)
    else counter(tl(word_lst), count + 1)
  in
    counter(words, 1)
  end

fun date_to_string (date: int * int * int) =
  get_nth(["January", "February", "March",
	   "April", "May", "June",
	   "July", "August", "September",
	   "October", "November",
	   "December"], (#2 date)) ^ " " ^ Int.toString((#3 date)) ^ ", " ^ Int.toString((#1 date))

fun number_before_reaching_sum (total: int, numbers: int list) =
  let fun counter (xs: int list, sum: int, count: int) =
    if sum >= total
    then count		   
    else counter(tl(xs), sum + hd(xs), count + 1)
  in
    counter(tl(numbers), hd(numbers), 0)
  end
	   
fun what_month (day: int) =
  number_before_reaching_sum(day, [31,28,31,30,31,30,31,31,30,31,30,31]) + 1

fun month_range (day1: int, day2: int) =
  if day2 < day1
  then []
  else
    let fun builder (xs: int list) =
      if (day2 - day1) = length(xs)	    
      then rev(what_month(day2)::xs)
      else if length(xs) >= 1
      then builder((what_month(day1) + 1)::xs)
      else builder(what_month(day1)::xs)
    in
      builder([])
    end

fun oldest(dates: (int*int*int) list) =
  if null dates
  then NONE
  else
    let fun oldest_date (xs: (int*int*int) list, date: (int*int*int)) =
      if null xs
      then SOME date
      else if (#1 (hd(xs))) < (#1 date)
      then oldest_date(tl(xs),hd(xs))
      else if (#1 (hd(xs))) = (#1 date) andalso (#2 (hd(xs))) < (#2 date)
      then oldest_date(tl(xs),hd(xs))
      else if (#2 (hd(xs))) = (#2 date) andalso (#3 (hd(xs))) <= (#3 date)		 
      then oldest_date(tl(xs),hd(xs))
      else oldest_date(tl(xs),date)  
    in
      oldest_date(tl(dates),hd(dates))
    end
