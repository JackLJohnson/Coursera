(* Inputs:  Two dates in the format (year/month/day) 
   Outputs: True if first date is older than second date, else False *)
fun is_older (date1 : (int*int*int), date2 : (int*int*int)) = 
  if #1 date1 < #1 date2
  then true
  else if #1 date1 > #1 date2
  then false
  else if #2 date1 < #2 date2
  then true
  else if #2 date1 > #2 date2
  then false
  else if #3 date1 < #3 date2
  then true
  else if #3 date1 > #3 date2
  then false
  else false

(* Inputs:  A list of dates, a month
   Outputs: Number of dates with the given month *)
fun number_in_month (dates : (int*int*int) list, month : int) =
  if null dates 
  then 0
  else if #2 (hd dates) = month 
  then 1 + number_in_month(tl dates, month)
  else number_in_month(tl dates, month)

(* Inputs:  A list of dates, A list of months
   Outputs: Number of dates where the month is in the given list of months *)
fun number_in_months(dates : (int*int*int) list, months : int list) = 
  if null dates orelse null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months) 

(* Inputs:  A list of dates, a month
   Outputs: List of dates with the given month *)
fun dates_in_month(dates : (int*int*int) list, month : int) = 
  if null dates
  then []
  else if #2 (hd dates) = month
  then hd dates::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

(* Inputs:  A list of dates, A list of months
   Outputs: List of dates where the month is in the given list of months *)
fun dates_in_months(dates : (int*int*int) list, months : int list) = 
  if null dates orelse null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months) 

(* Inputs:  A list of strings, An integer i
   Outputs: Returns the ith string in the list *)
fun get_nth(strs : string list, i : int) = 
  if i = 1
  then hd strs
  else get_nth(tl strs, i-1)

(* Inputs: A date  
   Outputs: A string which represents the given date *)
fun date_to_string(x : (int*int*int)) = 
  let
    val dates = ["January", "February", "March", "April", "May", "June", 
                  "July", "August", "September", "October", "November", "December"]
    val year  = Int.toString(#1 x)
    val month = get_nth(dates, #2 x)
    val day   = Int.toString(#3 x)
  in
    month ^ " " ^ day ^ ", " ^ year
  end

(* Inputs:  An integer sum, A list of integers
   Outputs: An integer i where the sum of values up to i in the given list is less than the given sum *)
fun number_before_reaching_sum(sum : int, nums : int list) = 
  if sum <= hd nums
  then 0
  else 1 + number_before_reaching_sum(sum - hd nums, tl nums)

(* Inputs:  An integer day
   Outputs: An integer i which corresponds to the month that the day is in *)
fun what_month(day : int) = 
  let
    val days_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_of_month) + 1
  end

(* Inputs:  Two integers day1 and day2
   Outputs: A list of integers corresponding to the months, from day1 to day2 *)
fun month_range(day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1+1, day2)

(* Inputs:  A list of dates
   Outputs: An option SOME d where d is the oldest date in the given list *)
fun oldest(dates : (int*int*int) list) = 
  if null dates
  then NONE
  else if null (tl dates)
  then SOME(hd dates)
  else if is_older(hd dates, hd (tl dates))
  then oldest(hd dates::tl (tl dates))
  else oldest(hd (tl dates)::tl (tl dates))
                                    
(* Inputs:  A date d 
   Outputs: True if d is a reasonable date, else False *)
fun reasonable_date(date : (int*int*int)) = 
  let
    val year = #1 date
    val month = #2 date
    val day = #3 date
    val leapyear = if (year mod 400 = 0 orelse year mod 4 = 0) andalso year mod 100 <> 0
                   then true 
                   else false                                         
    val days_of_month = if leapyear 
                        then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
                        else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    if year < 1
    then false
    else if month < 1 orelse month > 12 
    then false
    else if day < 1 orelse day > List.nth(days_of_month, month - 1)
    then false
    else true
  end
              
    
