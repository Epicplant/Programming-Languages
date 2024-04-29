open Hw6types

(**** Implement the following functions, remembering the "important
      note on function bindings" in the assignment write-up ****)

exception Unimplemented of string

(* 1 *)
let only_lowercase = List.filter (fun x -> Char.lowercase_ascii(x.[0]) = x.[0]) 

(*10+10) Write a function only_lowercase that takes a string list and returns a string list that
has only the strings in the argument that start with a lowercase letter. Assume all strings have at least
1 character. Use List.filter, Char.lowercase_ascii, and string index access (str.[pos]) to make
a 1-2 line solution.*)

(* 2 *)
let longest_string1 = List.fold_left (fun acc x ->
    if(String.length(acc) >= String.length(x))
      then acc
    else x) ""

(*(5+5) Write a function longest_string1 that takes a string list and returns the longest string
in the list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning
of the list. Use List.fold_left, String.length, and no recursion (other than the fact that the
implementation of List.fold_left is recursive).*)


(* 3 *)
let longest_string2 = List.fold_left (fun acc x ->
    if(String.length(acc) > String.length(x))
      then acc
    else x) ""

(*(5+5) Write a function longest_string2 that is exactly like longest_string1 except in the case of
ties it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use List.fold_left and String.length*)

(* 4 *)
let longest_string_helper f =
  List.fold_left (fun acc x ->
    let int_one = String.length(acc) in
    let int_two = String.length(x) in
  if(f int_one int_two)
    then acc
  else x) ""
    
    
    
(*
  
• longest_string_helper has type (int -> int -> bool) -> string list -> string (notice
the currying). This function will look a lot like longest_string1 and longest_string2 but is
more general because it takes a function as an argument.

• If longest_string_helper is passed a function that behaves like > (so it returns true exactly
when its first argument is strictly greater than its second), then the function returned has the
same behavior as longest_string1.

*)

let longest_string3 =
  longest_string_helper(fun acc x -> (acc >= x))



let longest_string4 =
  longest_string_helper(fun acc x -> (acc > x))

(* 

• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
  same behavior as longest_string2.

• longest_string3 and longest_string4 are bound to the result of calls to longest_string_helper.

*)

(* 5 *)
let longest_lowercase =  longest_string1 % only_lowercase

(*
  (10+10) Write a function longest_lowercase that takes a string list and returns the longest string
  in the list that begins with a lowercase letter, or "" if there are no such strings. Assume all strings
  have at least 1 character. Use the % operator from the starter code for composing functions. Resolve
  ties like in problem 2.
*)


(* 6 *)


let caps_no_X_string =  String.concat "" % String.split_on_char 'X' % String.uppercase_ascii


(*
  (10+10) Write a function caps_no_X_string that takes a string and returns the string that is like
  the input except every letter is capitalized and every “x” or “X” is removed (e.g., “aBxXXxDdx”
  becomes “ABDD”). Use the % operator and 3 library functions in the String module. Browse the
  module documentation to find the most useful functions.
*)


(* 7 *)
let rec first_answer f xs =
  match xs with
  | [] -> raise NoAnswer
  | y :: ys ->
    match f y with
    | None -> first_answer f ys
    | Some v ->  v

(*

  (20+20) Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the
  2 arguments are curried). The first argument should be applied to elements of the second argument
  in order until the first time it returns Some v for some v and then v is the result of the call to
  first_answer. If the first argument returns None for all list elements, then first_answer should
  raise the exception NoAnswer. Hints: Sample solution is 7 lines and does nothing fancy.
*)


(* 8 *)
let all_answers f xs =
  let rec looper args acc =
    match args with
    | [] -> Some acc  
    | y::ys -> 
      match f y with
      | None -> None  
      | Some v -> looper ys (v @ acc)
  in looper xs []



(*
  (15+15) Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
  (notice the 2 arguments are curried). The first argument should be applied to elements of the second
  argument. If it returns None for any element, then the result for all_answers is None. Else the
  calls to the first argument will have produced Some lst1, Some lst2, ... Some lstn and the result of
  all_answers is Some lst where lst is lst1, lst2, ..., lstn appended together. 
  
  (Your solution can return these lists appended in any order.) Hints: The sample solution is 10 lines. It uses a helper
  function with an accumulator and uses @. Note all_answers f [] should evaluate to Some []
*)
