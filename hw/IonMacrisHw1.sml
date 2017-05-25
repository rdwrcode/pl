
(*1. Compare two dates, if first date1 < date2 => true else return false*)
fun is_older(date1: (int * int* int), date2: (int * int* int)) =
  let
      (*We convert a date to days and compare the resulting number*)
      (*offsets - list with the offsets for days of year for a month, e.g. for March the offset is 31 + 28 = 59, April: 31 + 28 + 31 = 90 and son on ...*)
      val offsets = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
      val date1_days = #1 date1 * 365 + List.nth(offsets, (#2 date1) - 1) + #3 date1
      val date2_days = #1 date2 * 365 + List.nth(offsets, (#2 date2) - 1) + #3 date2
  in
      if date1_days < date2_days then
	  true
      else
	  false
  end

(* Check if a date has a month*)
fun month_in_date(date: (int * int * int), month: int) =
  if #2 date = month then
      true
  else
      false
	  
(*2. Check how many times a month is present in a list of dates*)
fun number_in_month(dates: (int * int * int) list, month: int) =
  if null dates then
      0
  else if null (tl dates) andalso month_in_date(hd dates, month) then
      1
  else
      let
	  (*Save the value of the recursion*)
	  val summ = number_in_month(tl dates, month)
      in
	  (*Accumulate the values*)
	  if month_in_date(hd dates, month) then
	      1 + summ
	  else
	      summ
      end

	  
(*3. Check how many times each month from the months list is present in the list of dates and sum the numbers*)
fun number_in_months(dates: (int * int * int) list, months: int list) =
  if null dates orelse null months then
      0
  else
      let
	  (*Save the value of the recursion*)
	  val summ = number_in_months(dates, tl months)
      in
	  (*Accumulate the values*)
	  summ + number_in_month(dates, hd months)
      end
	  
(*4. Get the list of all dates that have a specific month*)
fun dates_in_month(dates: (int * int * int) list, month: int) =
  if null dates then
      []
  else
      let
	  (*Store the intermediary values in order to not do recursion in recursion*)
	  val tl_dates = dates_in_month(tl dates, month)
      in
	  if #2(hd dates) = month then
	      (hd dates) :: tl_dates
	  else
	      tl_dates
      end

(*5. Get the list of all dates that have any of the months*)
fun dates_in_months(dates: (int * int * int) list, months: int list) =
  if null dates orelse null months then
      []
  else
      let
	  val tl_dates = dates_in_months(dates, tl months)
      in
	  dates_in_month(dates, hd months) @ tl_dates
      end

	  
	  
(*6. Get the n-th element from a list of strings
     The function returns an empty string when the list is empty or when there is no match.
     A more robust solution will use an Optional!!!
 *)
fun get_nth(tokens: string list, position: int) =
  let
      (*Private, helper functions that defines the starting point of the search in list*)
      fun get_nth_starting_from(tokens: string list, position: int, current_position: int) =
	if null tokens then
	    ""
	else if current_position = position then
	    hd tokens
	else
	    get_nth_starting_from(tl tokens, position, current_position + 1)
  in
      get_nth_starting_from(tokens, position, 1)
  end

(*7. Return a string representation of a given  date*)
fun date_to_string(date: (int * int * int)) =
  let
      (*Private list of months names, used to get a string representation for a given month number*)
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end


(*8. We assume sum is positive and numbers contains only positive numbers.
     We accumulate the numbers from the numbers list until their sum is less than "sum".
     If there are not enough numbers to satisfy the constraint a NoSolution exception is raised!
*)
fun number_before_reaching_sum(sum: int, numbers: int list) =
  let
      fun no_before_reaching_sum(sum: int, numbers: int list, current_sum: int, last_number: int) =
	let
	    exception NoSolution;
	in
	    if null numbers andalso current_sum < sum then
		raise NoSolution
	    else if current_sum + (hd numbers) >= sum then
		if last_number >= 0 then
		    last_number
		else
		    raise NoSolution
	    else
		no_before_reaching_sum(sum, tl numbers, current_sum + (hd numbers), last_number + 1)  
	end
  in
      no_before_reaching_sum(sum, numbers, 0, 0)
  end

(*9. Given a day from 1 to 365 return the month of the year.*)
fun what_month(dayOfYear: int) =
  let
      val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
      number_before_reaching_sum(dayOfYear, days) + 1
  end

      
(*10 Given two days of the year, day1 and day2, from 1 to 365. Return a list with [m1, m2, .. mn]
     where m1 = what_month(day1) and mn = what_month(day2)
*)
fun month_range(day1: int, day2: int) =
  if day1 > day2 then
      []
  else
      what_month(day1) :: month_range(day1 + 1, day2)

				   
(*11. Get the oldest date from a list of dates."*)	  
fun oldest(dates: (int * int * int) list) =
  if null dates then
      NONE
  else
      let
	  (*Helper functions that treats the case of a non-empty list of dates*)
	  fun oldest_helper(xs: (int * int * int) list) =
	    if null (tl xs) then
		hd xs
	    else
		let
		    val tl_min = oldest_helper(tl xs)
		in
		    if is_older(hd xs, tl_min) then
			hd xs
		    else
			tl_min
		end
      in
	  SOME(oldest_helper dates)
      end

	  
