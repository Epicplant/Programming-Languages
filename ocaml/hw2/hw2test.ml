open Hw2

(* 1 *)

(* the testing framework checks that each test returns true *)
let%test _ = is_older((15, 1, 2021), (1, 4, 2022))
let%test _ = is_older((1, 1, 500), (1, 4, 3000))
let%test _ = is_older((15, 1, 2021), (12, 12, 6000))


(* we expect is_older to return false here, so we put a "not" around
   it so that the test framework will check that "not" returns true. *)
let%test _ = not (is_older((1, 2, 3), (3, 2, 1)))
let%test _ = not (is_older((20, 6, 18), (1, 1, 0)))
  (* this test actually passes on the starter code, which always
     returns false. once you actually implement is_older, the test
     will actually be testing something :) *)

(* TODO: more tests for problem 1 here, probably... *)

(* 2 *)

(* here we want to check that number_in_month returns 1 on this input.
   we use "=" so that the whole test returns true when it passes. *)
let%test _ = number_in_month([(15, 1, 2021); (1, 4, 2022)], 1) = 1
let%test _ = number_in_month([(15, 1, 2021); (1, 4, 2022)], 6) = 0
let%test _ = number_in_month([(3, 4, 1980); (4, 4, 500); (4, 4, 3000)], 4) = 3

(* TODO: more tests for problem 2 here, probably... *)

(* TODO: continue with tests for problem 3 and onward here *)

(*Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem.*)

let%test _ = number_in_months([(3, 6, 1980); (4, 4, 500); (4, 5, 3000)], [6; 4; 5]) = 3
let%test _ = number_in_months([(3, 5, 1980); (4, 1, 500); (4, 5, 3000)], [6; 4; 5]) = 2
let%test _ = number_in_months([(3, 5, 1980); (4, 1, 500); (4, 5, 3000)], [7; 8; 9]) = 0
let%test _ = number_in_months([(3, 5, 1980); (4, 1, 500); (4, 5, 3000)], []) = 0

(*Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given.*)

let%test _ = dates_in_month([(1, 5, 500); (7, 8, 1800); (20, 8, 2020)], 8) = [(7, 8, 1800); (20, 8, 2020)]
let%test _ = dates_in_month([(1, 5, 500); (2, 8, 1500); (20, 8, 2020)], 5) = [(1, 5, 500)]
let%test _ = dates_in_month([(1, 5, 500); (2, 8, 1500); (20, 8, 2020)], 0) = []

(*Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and OCaml’s list-append operator (@)*)

let%test _ = dates_in_months([(1, 5, 500); (7, 4, 1800); (20, 8, 2020)], [4; 5]) = [(7,4, 1800); (1, 5, 500)]
let%test _ = dates_in_months([(1, 5, 500); (7, 4, 1800); (20, 8, 2020)], [4; 5; 8]) = 
[(7, 4, 1800);  (1, 5, 500); (20, 8, 2020)]
let%test _ = dates_in_months([(1, 5, 500); (7, 4, 1800); (20, 8, 2020)], []) = []

(*Write a function get_nth that takes a list of strings and a positive int n and returns the n
th element of the list where the head of the list is 1st. Do not worry about the case where the list has too few
elements: your function may apply List.hd or List.tl to the empty list in this case, which is okay.*)
let%test _ = get_nth(["Hi"; "my"; "name"; "is"; "Chris"], 4) = "is"
let%test _ = get_nth(["Hi"; "my"; "name"; "is"; "Chris"], 2) = "my"
let%test _ = get_nth(["Hi"; "my"; "name"; "is"; "Chris"], 1) = "Hi"
let%test _ = get_nth(["Hi"; "my"; "name"; "is"; "Chris"], 0) = "Hi"

(*Write a function string_of_date that takes a date and returns a string of the form September-10-2015
(for example). Use the operator ^ for concatenating strings and the library function string_of_int
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, use
hyphens exactly as in the example and use English month names: January, February, March, April,
May, June, July, August, September, October, November, December.*)

let%test _ = string_of_date(20, 1, 2020) = "January-20-2020"
let%test _ = string_of_date(12, 12, 1980) = "December-12-1980"
let%test _ = string_of_date(10, 9, 2015) = "September-10-2015"

(*Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case.*)

let%test _ = number_before_reaching_sum(2, [0; 1; 2]) = 2
let%test _ = number_before_reaching_sum(1, [0; 1; 2]) = 1
let%test _ = number_before_reaching_sum(2, [25; 50; 75]) = 0
let%test _ = number_before_reaching_sum(100, [25; 25; 25; 25]) = 3

(*Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem.*)

let%test _ = what_month(5) = "January"
let%test _ = what_month(365) = "December"
let%test _ = what_month(330) = "November"

(* Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1;m2;...;mn] where m1 is the month of day1, m2 is the month of day1+1, . . . , and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1 > day2.*)

let%test _ = month_range(1, 12) = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1]
let%test _ = month_range(1, 32) = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 2]
let%test _ = month_range(40, 1) = []
let%test _ = month_range(50, 63) =  [2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3]

(*Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to None if the list has no dates else Some d where the date d is the oldest date in the list.*)

let%test _ = oldest([(1, 5, 2018); (4, 6, 2020); (20, 8, 1500)]) = Some((20, 8, 1500))
let%test _ = oldest([(10, 9, 1400); (24, 11, 1800)]) = Some((10, 9, 1400))
let%test _ = oldest([]) = None

(*Write a function cumulative_sum that takes a list of numbers and returns a list of the partial sums
of these numbers. For example, cumulative_sum [12;27;13] = [12;39;52]. Hint: Use a helper
function that takes two arguments*)


let%test _ = cumulative_sum([12; 27; 13]) = [12; 39; 52]
let%test _ = cumulative_sum([20; 50; 80]) = [20; 70; 150]
let%test _ = cumulative_sum([0; 0; 0]) = [0; 0; 0]
let%test _ = cumulative_sum([]) = []