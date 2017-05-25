(* function 1 *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
  if #1 date1 < #1 date2
  then true
  else if #1 date1 = #1 date2 andalso #2 date1 < #2 date2
  then true
  else if #1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2
  then true
  else false	   
(* function 2 *)
fun number_in_month (dates : (int*int*int) list, month : int) =
  if null dates
  then 0
  else if #2 (hd dates) = month
  then 1 + number_in_month(tl (dates), month)
  else number_in_month(tl (dates), month)    
(* function 3 *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates,hd months) + number_in_months(dates,tl months)
(* function 4 *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
  if null dates
  then []
  else if #2 (hd dates) = month
  then (hd dates) :: dates_in_month(tl (dates), month)
  else dates_in_month(tl (dates), month)
(* function 5 *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months)	@ dates_in_months(dates, tl months)   
(* function 6 *)  
fun get_nth (list : string list, loc : int) =
  let fun get_loc (list : string list,count : int) =
	if count = loc
	then hd list
	else get_loc (tl list,count + 1)	
  in
      get_loc (list, 1)
  end
(* function 7 *)
fun date_to_string (date : int*int*int) =
  let val eng_months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth (eng_months, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
  end
(* function 8 *)
fun number_before_reaching_sum (sum : int, list : int list) =
  let fun less_than_sum (sum : int, count :int, list : int list) =
	if sum - hd list > 0 
	then less_than_sum (sum - hd list, count + 1,tl list)
	else count
  in
      less_than_sum (sum, 0, list)
  end		    
(* function 9 *)  
fun what_month (day : int) =
  let val days_of_month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum (day,days_of_month) + 1
  end				 
(* function 10 *)
fun month_range (day1 : int, day2 : int) =
  if day2 < day1
  then []
  else what_month day1 :: month_range(day1 + 1,day2) 	   
(* function 11 *)
fun oldest (dates : (int*int*int) list) =
  if null dates
  then NONE
  else let
      fun oldest_nonempty (dates : (int*int*int) list) =
	if null (tl dates)
	then hd dates
	else let val tl_ans = oldest_nonempty(tl dates)
	     in
		 if is_older (hd dates, tl_ans)
		 then hd dates
		 else tl_ans
	     end		  
 	in
	    SOME (oldest_nonempty dates)
	end	 
      
