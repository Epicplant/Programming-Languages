(* CSE 341, Homework 2, Provided Code *)

(* Use these functions to extract parts of a date *)
let fst3 (x,_,_) = x (* gets the first element of a triple *)
let snd3 (_,x,_) = x (* gets the second element of a triple *)
let thd3 (_,_,x) = x (* gets the third element of a triple *)

(* TODO: Complete the 12 function bindings described in the
   assignment. For the first two problems, we have given you the
   *correct* first line and an *incorrect* function body. *)

  (* TODO: actually implement is_older *)
(* 1 *)
let is_older ((date1 : int * int * int), (date2 : int * int * int)) =
  if thd3(date1) != thd3(date2)
    
    then thd3(date1) < thd3(date2)
  
  else if snd3(date1) != snd3(date2)
    
    then snd3(date1) < snd3(date2) 
  
  else if fst3(date1) != thd3(date2)
    
    then fst3(date1) < fst3(date2)

  else 
     false
  
  
(*2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.*)
(* 2 *)
    let rec number_in_month ((dates: (int * int * int) list), (month : int)) =
      if dates = []
        then (* base case *) 0

      else if snd3(List.hd(dates)) = month
       
        then 1 + number_in_month(List.tl(dates), month)
      
      else

        number_in_month(List.tl(dates), month)
    

(* TODO: continue for problem 3 and onward here *)

let rec number_in_months ((dates: (int * int *int) list), (months : (int) list)) =
  if months = []
    
    then (* base case *) 0
  
  else
    number_in_month(dates, List.hd(months)) + number_in_months(dates, List.tl(months))

let rec dates_in_month ((dates : (int * int * int) list), (month : int)) =

	if dates = []
	  then []
  
  else if snd3(List.hd(dates)) = month
	
    then 
      List.hd(dates) :: dates_in_month(List.tl(dates), month)

  else 
	    dates_in_month(List.tl(dates), month)

let rec dates_in_months((dates : (int * int * int) list), (months : int list)) =
if months = []
  then []
else 
  dates_in_month(dates, List.hd(months)) @ dates_in_months(dates, List.tl(months))

let rec get_nth ((words : (string) list), (n : int)) =
  if n <= 1
    then List.hd(words)
  else
      get_nth(List.tl(words), n-1)

let months : string list = ["January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October";
"November"; "December"]

let string_of_date(date : (int * int * int)) = get_nth(months, snd3(date)) ^ "-" ^ string_of_int(fst3(date))
 ^ "-" ^ string_of_int(thd3(date))


let rec number_before_reaching_sum((sum : int), (list : int list)) = 
  if (sum - List.hd(list)) <= 0 
      then 0 
    else
    1 + number_before_reaching_sum(sum - List.hd(list), List.tl(list))

let month_day_num : int list = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31]

let what_month((day : int)) = get_nth(months, 1+number_before_reaching_sum(day, month_day_num))

(* Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1;m2;...;mn] where m1 is the month of day1, m2 is the month of day1+1, . . . , and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1 > day2.*)


let rec month_range((dayOne : int), (dayTwo : int)) =
  if dayOne > dayTwo
    then []
  else
    1+number_before_reaching_sum(dayOne, month_day_num) :: month_range(dayOne+1, dayTwo)

(*Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to None if the list has no dates else Some d where the date d is the oldest date in the list.*)


let rec oldest(dates : (int * int * int) list) =
if dates = [] 
  then None
else if List.tl(dates) = [] then
   Some(List.hd(dates))
else 
  let oldest_date = oldest(List.tl(dates)) in
      if is_older(List.hd(dates), Option.get(oldest_date)) then
        Some(List.hd(dates))
      else
        oldest_date



(*Write a function cumulative_sum that takes a list of numbers and returns a list of the partial sums
of these numbers. For example, cumulative_sum [12;27;13] = [12;39;52]. Hint: Use a helper
function that takes two arguments.*)

let cumulative_sum((numbers : int list)) = 
  let rec looper((num : int), (nums : int list)) = 
    if nums = []
      then []
    else 
      num+List.hd(nums) :: looper(num+List.hd(nums), List.tl(nums))
  in looper(0, numbers)