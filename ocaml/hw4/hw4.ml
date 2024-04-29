(* CSE 341, HW4 Provided Code *)

(* json.ml contains the main datatype definition we will use
   throughout the assignment. You will want to look over that file
   before starting. *)
open Json

(* These come from the parsed_*_bus.ml.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (~500 reports),
   respectively with the data that you will need to implement
   your homework.
*)
open Json_structures.Parsed_complete_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_small_bus

(* provided helper function that deduplicates a list *)
let dedup xs = List.sort_uniq compare xs

(* provided helper function that sorts a given list *)
let sort xs = List.sort compare xs

(* Here is a provided helper function to convert a float to a string.
   OCaml's string_of_float is not quite RFC-compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job how we want. *)
let json_string_of_float f =
  Printf.sprintf "%g" f

(* 1 *)
let make_silly_json i =
  let rec looper (i, acc) =
    let t = float_of_int(i) in
    match i with
     | 0 -> List.rev(acc)
     | i ->  looper(i-1, Object[("n", Num t); ("b", True)] :: acc)
  in
 let x = looper(i, [])
in
  Array x

(*Write a function make_silly_json that takes an int i and returns a json. The result should
represent a JSON array of JSON objects where every object in the array has two fields, "n" and "b".
Every object’s "b" field should hold true (i.e., True). The first object in the array should have a "n"
field holding the JSON number i.0 (in other words, the integer i converted to a floating point JSON
number), the next object should have an "n" field holding (i − 1).0 and so on where the last object in
the array has an "n" field holding 1.0. Sample solution is less than 10 lines. Hints: There’s a function
in OCaml’s standard library called float_of_int that converts an integer to a float. You’ll want a
helper function that does most of the work*)


(* 2 *)
let rec concat_with (sep, ss) =
  let rec looper (sep, ss, acc) =
    match ss with
  | [] -> acc
  | ss::[] -> looper(sep, [], acc ^ ss)
  | ss::sss -> looper(sep, sss, acc ^ ss ^ sep) 
  in
 looper(sep, ss, "")


  (*Write a function concat_with that takes a separator string and a list of strings, and returns
the string that consists of all the strings in the list concatenated together, separated by the separator.
The separator should be only between elements, not at the beginning or end. Use OCaml’s ^ operator
for concatenation (e.g., "hello" ^ "world" evaluates to "helloworld"). Sample solution is 5 lines.*)
  

(* 3 *)
let quote_string s =
  "\"" ^ s ^ "\""

  
  let temp = string_of_float(1.0)

  let temp2 = string_of_float(1.1234)

  let temp3 = string_of_int(int_of_float(1.0))


(* 4 *)
let rec string_of_json j =
    match j with 
    | Num e -> json_string_of_float(e)
    | True -> "true"
    | False -> "false"
    | String e -> quote_string(e) 
    | Null -> "null"
    | Array l ->
      let rec arraylooper (l) =
          match l with
          |[] -> []
          |x::xs -> string_of_json(x) :: arraylooper(xs)
          
      in "[" ^ concat_with(", ", arraylooper(l)) ^ "]"
      | Object r -> 
        let rec objectlooper(r) =
            match r with
            |[] -> []
            |(p,q)::xs -> (quote_string(p) ^ " : " ^ string_of_json(q)) :: objectlooper(xs)      
              
            in "{" ^ concat_with(", ", objectlooper(r)) ^ "}"
  
let json_obj   = Object [("foo", json_pi); ("bar", json_array); ("ok", True)]
(*"{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}"*)

(*Write a function string_of_json that converts a json into the proper string encoding in terms
of the syntax described on the first page of this assignment. The two previous problems are both
helpful, but you will also want local helper functions for processing arrays and objects (hint: in both
cases, create a string list that you then pass to concat_with). In the Num case, use the provided
json_string_of_float function. Sample solution is 25 lines.*)


(* 5 *)
let rec take (n,xs) =
  let rec looper (n, xs, acc) =
    match n with
    | 0 -> List.rev(acc)
    | _ ->
      match xs with
      | [] -> List.rev(acc)
      | xs::xss -> looper(n-1, xss, xs :: acc)
  in looper(n, xs, [])

(*Write a function take of type int * ’a list -> ’a list that takes an int called n and a list
called xs and returns the first n elements of xs in the same order as xs. You may assume that n is
non-negative and does not exceed the length of xs. Sample solution is about 5 lines.*)

(* 6 *)
let rec firsts xs =

  let rec looper(sx, acc) =
    match sx with
    | [] -> List.rev(acc)
    | sx::sxs ->
     let (p, q) = sx in
     looper(sxs, p :: acc)
  in looper(xs, [])

(*Write a function firsts of type (’a * ’b) list -> ’a list that takes a list of pairs and
returns a list of all the first components of the pairs in the same order as the argument list. Sample
solution is about 4 lines.*)

let testone = take(4, firsts [(1,2); (3,4); (5,6); (7, 8)])
let testtwo = firsts(take(4, [(1,2); (3,4); (5,6); (7, 8)]))



(* 7 *)
(* They are always going to evaluate to the same value since, regardless of whether take or firsts
   is used the quickest, the initial value in the pairs are always going to stay the same (regardless
   of whether some values are excluded by takes or not). Additionally, since n
   is also the same in both cases, they will both evaluate to the same answer since takes simply grabs the earliest
   values in the list regardless of whether they are pairs or not (meaning they will always grab the same first values in pairs
   with the only differences being firsts (take (n, xs)) will still have their second values for longer).
   In regards to which would be faster to evaluate, firsts (take (n, xs)) would be quicker to evaluate
   since firsts doesn't have to modify as many values as in take(4, firsts (n ,firsts xs)) since take
   shortens the list before calling firsts*)

(*Write a comment in your file after your definition of firsts answering the following questions.
Suppose xs has type (int * int) list, and let n be an integer between 0 and the length of xs
(inclusive), and consider the expressions firsts (take (n, xs)) and take (n, firsts xs). Either
(1) write one sentence explaining in informal but precise English why these two expressions always
evaluate to the same value; or (2) give example values of xs and n such that the two expressions
evaluate to different values. Regardless of whether you decide option (1) or option (2) is correct, also
write one sentence explaining which of the two expressions above might be faster to evaluate and why.*)


(* 8 *)
let rec assoc (k, xs) =
  match xs with
  | [] -> None
  | temp :: temps ->
      let (p, q) = temp in 
      if p = k then
       Some q else
          assoc(k, temps)

  (*Write a function assoc of type ’a * (’a * ’b) list -> ’b option that takes two arguments
k and xs. It should return Some v1 if (k1,v1) is the pair in the list closest to the beginning of the list
for which k and k1 are equal. If there is no such pair, assoc returns None. Sample solution is a few
lines. (Note: There’s a function with similar functionality in the OCaml standard library, but calling
it requires features we haven’t covered yet. Do not use that function or you will not receive credit.)*)

(* 9 *)
let dot (j, f) =
  match j with
  | Object r -> assoc(f, r)
  | _ -> None

(* Write a function dot that takes a json (call it j) and a string (call it f) and returns a
json option. If j is an object that has a field named f, then return Some v where v is the contents
of that field. If j is not an object or does not contain a field f, then return None. Sample solution is 4
short lines thanks to an earlier problem*)

(* 10 *)
let rec dots (j, f) =
  match f with
  [] -> None
  | x::[] -> dot(j, x)
  | x::xs ->
      match dot(j, x) with
      |None -> None
      |Some r -> dots(r, xs)
 
     

  (*Write a function dots that takes a json called j and a string list called fs that represents
an access path, or in other words, a list of field names. The function dots returns a json option by
recursively accessing the fields in fs, starting at the beginning of the list. If any of the field accesses
occur on non-objects, or to fields that do not exist, return None. Otherwise, return Some v where v is
the value of the field “pointed to” by the access path. (Hint: Use recursion on fs plus your solution
to the previous problem.) Sample solution is about 7 lines.*)

(* 11 *)
let one_fields j =
  match j with 
  | Object r -> 
  let rec looper (x, acc) =
      match x with
      |[] -> acc
      |x::xs ->
        let (p, q) = x in
         looper(xs, p :: acc)
  in looper(r, [])
  | _ -> []
  

  (*Write a function one_fields that takes a json and returns a string list. If the argument is
an object, then return a list holding all of its field names (not field contents). Else return the empty
list. Use a tail-recursive, locally defined helper function. The list you return can be in any order, but
it is probably easiest to have the results in reverse order from how they*)

(* 12 *)
let no_repeats xs = (List.length(dedup(xs)) = List.length(xs))
  (* Write a function no_repeats that takes a string list and returns a bool that is true if and
only if no string appears more than once in the input. Do not (!) use any explicit recursion. Rather,
use provided helper function dedup (which returns its argument without duplicates) together with
standard library function List.length to complete this problem in one line.*)

let nest = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("foo",True);
                                     ("foo",True)]);
                         ("c",True)];
                  Object []]

(* 13 *)
let rec recursive_no_field_repeats j =
  match j with 
  | Array l ->
    let rec arraylooper (l) =
        match l with
        |[] -> true
        |x::xs -> 
        recursive_no_field_repeats(x) && arraylooper(xs)
    in arraylooper(l)
  | Object kvl -> 
    let rec objectlooper(kvl) =
        match kvl with
        |[] -> true
        |(_,v)::kvls ->       
           no_repeats(one_fields(j)) && objectlooper(kvls) && recursive_no_field_repeats(v)
        in objectlooper(kvl)
  | _ -> true
   

(*Write a function recursive_no_field_repeats that takes a json and returns a bool that is
true if and only if no object anywhere “inside” (arbitrarily nested) the json argument has repeated field
names. (Notice the proper answer for a json value like False is true. Also note that it is not relevant
that different objects may have field names in common.) In addition to using some of your previous
functions, you will want two locally defined helper functions for processing the elements of a JSON
array and the contents of a JSON object’s fields. By defining these helper functions locally, rather
than at the top level, they can call recursive_no_field_repeats in addition to calling themselves
recursively. Sample solution is about 15 lines.*)

(* 14 *)
let count_occurrences (xs, e) = 
  match xs with 
  | [] -> []
  | r::rs ->    
  let rec looper(xs, acc, current, count, e) =
        match xs with
        | [] -> (current, count) :: acc
        | t::ts -> 
           if (t = current) 
            then looper(ts, acc, current, count+1, e)
          else if(t < current)
            then raise e
          else
            looper(ts, (current, count) :: acc, t, 1, e)
in looper (xs, [], r, 0, e)

(*Write a function count_occurrences of type string list * exn -> (string * int) list.
If the string list argument is sorted (using OCaml’s built-in comparison operator, <), then the
function should return a list where each string is paired with the number of times it occurs. (The
order in the output list does not matter.) If the list is not sorted, then raise the exn argument. Your
implementation should make a single pass over the string list argument, primarily using a tail 
recursive helper function. You will want the helper function to take a few arguments, including the
“current” string and its “current” count. Sample solution is about 12 lines.*)

(* 15 *)
let rec string_values_for_access_path (fs, js) =
  let rec looper(fs, js, acc) =
    match js with
    | [] -> List.rev(acc)
    | x::xs ->
      match dots(x, fs) with
      | None -> looper(fs, xs, acc)
      | Some t -> 
        match t with
        | String r -> looper(fs, xs, r :: acc)
        | _ -> looper(fs, xs, acc)
in looper(fs, js, [])


(*Write a function string_values_for_access_path of type
(string list) * (json list) -> string list
(the parentheses in this type are optional, so the REPL won’t print them). For any object in the
json list that has a field available via the given “access path” (string list), and has contents that
are a JSON string (e.g., String "hi") put the contents string (e.g., "hi") in the output list (order does
not matter; the output should have duplicates when appropriate). Sample solution is 6 lines thanks to
dots.
*)


  (*Write a function dots that takes a json called j and a string list called fs that represents
an access path, or in other words, a list of field names. The function dots returns a json option by
recursively accessing the fields in fs, starting at the beginning of the list. If any of the field accesses
occur on non-objects, or to fields that do not exist, return None. Otherwise, return Some v where v is
the value of the field “pointed to” by the access path. (Hint: Use recursion on fs plus your solution
to the previous problem.) Sample solution is about 7 lines.*)




(* 16 *)
let rec filter_access_path_value (fs, v, js) =
  let rec looper(fs, v, js, acc) =
    match js with
    |[] -> List.rev(acc)
    |x::xs ->
      match dots(x, fs) with
      | None -> looper(fs, v, xs, acc)
      | Some r -> 
        match r with
        | String t ->
            if(t = v)
              then looper(fs, v, xs, x :: acc)
            else 
          looper(fs, v, xs, acc)
        | _ -> looper(fs, v, xs, acc)
       in looper(fs, v, js, [])
 



  (* Write a function filter_access_path_value of type
string list * string * json list -> json list.
The output should be a subset of the third argument, containing exactly those elements of the input
list that have a field available via the given access path, and that field’s contents are a JSON string
equal to the second argument. Sample solution uses dots and is less than 10 lines.*)

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }



(* Some of the bus data uses latitude and longitude positions to describe the location of vehicles in
real time. To narrow our focus onto a particular geographical area, we will use the record types rect
and point, which represent a rectangle and a point, respectively. The types are defined in the starter
code, but copied here for completeness.
type rect = { min_latitude: float; max_latitude: float;
min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }
Write a function in_rect of type rect * point -> bool that determines whether a given point falls
inside a given rectangle (inclusive). Solution is two lines and uses a lot of conjunction (&&).*)

let pgascse =
  { latitude = 47.653221;
    longitude = -122.305708 }

(* 17 *)
let in_rect (r, p) = 
  (r.min_latitude <= p.latitude) && (r.max_latitude >= p.latitude)
                     && (r.min_longitude <= p.longitude) && (r.max_longitude >= p.longitude)
  
(* 18 *)
let point_of_json j =
  match (dot(j, "latitude"), dot(j, "longitude")) with
    | (Some t1, Some t2) -> 
      (match (t1, t2) with
      | (Num r1, Num r2) -> 
        let point = Some {latitude = r1; longitude = r2}
        in point
      | _ -> None)
    | (_, _) -> None 



  (*Write a function point_of_json of type json -> point option. If the argument is a json
object that contains fields named "latitude" and "longitude", both of which are json numbers,
then point_of_json returns Some p where p is the point represented by these coordinates. Otherwise,
it returns None. Solution is 5 lines and uses dot and nested patterns.*)

(* Write a function dot that takes a json (call it j) and a string (call it f) and returns a
json option. If j is an object that has a field named f, then return Some v where v is the contents
of that field. If j is not an object or does not contain a field f, then return None. Sample solution is 4
short lines thanks to an earlier problem*)

let json_pgascse = Object [("latitude", Num 47.653221); ("longitude", Num (-122.305708))]


(* 19 *)
let rec filter_access_path_in_rect (fs, r, js) =
  let rec looper(fs, r, js, acc) =
    match js with
    | [] -> List.rev(acc)
    | x::xs ->
      let v = dots(x, fs) in
      match v with
      |None -> looper(fs, r, xs, acc)
      |Some p ->
        match point_of_json(p) with
        |None -> looper(fs, r, xs, acc)
        |Some z ->
          if in_rect(r, z)
            then looper(fs, r, xs, x :: acc)
        else
          looper(fs, r, xs, acc)


  in looper(fs, r, js, [])

(*Write a function filter_access_path_in_rect of type
string list * rect * json list -> json list.
The output should be a subset of the third argument, containing exactly those elements of the input list
that (1) have a field available via the given access path, (2) that field’s contents are a JSON object that
can be converted to a point via point_of_json, and (3) the resulting point is within the rectangle
specified by the second argument. Sample solution is less than 15 lines.*)

(* 20 *)
(* In regards to similarities, there are quite a few until you reach the bottom half of the function. For example,
in the first half, they 1) both iterate through json lists, 2) both use dots to find some value within a json object if it exists,
3) both use tail-recursion, and 4) both return json lists that are reversed. When it comes to refactoring the functions
to use a more general function, once we get past the calling dots is when the functions diverge. As a result, we could separate the first
part into it's own helper function, and then choose wether we want to use filter_access_path_in_rect or filter_access_path_value by
adding an argument to the more generalized function (there are other ways to do this such as pattern matching but this is just an example).
As a result, this would limit the need to rewrite code but would also allow us to pick which filter_access fucntion we want to use.
On a scale of 1 to 10 how annoyed I am.... probably a 6.*)




(* The definition of U district and the functions to calculate a
   histogram. Use these to create the bindings as requested by the
   handout.

   Notice that our implementation of histogram uses *your* definition
   of count_occurrences.
*)

exception SortIsBroken

(* The definition of the U district for purposes of this assignment :) *)
let u_district =
  { min_latitude  =  47.648637;
    min_longitude = -122.322099;
    max_latitude  =  47.661176;
    max_longitude = -122.301019
  }

(* Creates a histogram for the given list of strings.
   Returns a tuple in which the first element is
   a string, and the second is the number of times that string
   is found. *)
let histogram xs =
  let sorted_xs = List.sort (fun a b -> compare a b) xs in
  let counts = count_occurrences (sorted_xs, SortIsBroken) in
  let compare_counts (s1, n1) (s2, n2) =
    if n1 = n2 then compare s1 s2 else compare n1 n2
  in
  List.rev (List.sort compare_counts counts)

let histogram_for_access_path (fs, js) =
  histogram (string_values_for_access_path (fs,js))

(* This binding is commented out (because it calls dot, which is not yet implemented ).
   Uncomment it when you are ready to do part 3.
*)


let complete_bus_positions_list =
  match (dot (complete_bus_positions, "entity")) with
  | Some (Array xs) -> xs
  | _ -> failwith "complete_bus_positions_list"


exception Unimplemented


(* TODO: fill out these bindings as described in the PDF. *)
let route_histogram = histogram_for_access_path(["vehicle"; "trip"; "route_num"], complete_bus_positions_list)

(*(3+0) Bind to the variable route_histogram a histogram (using histogram_for_access_path) of the
objects in complete_bus_positions_list based on the "route_num" field. (Hint: since this field is
nested inside several objects, you need to look at the data to figure out the rest of the access path that
comes before this field.)
*)
let top_three_routes    = take(3, firsts(route_histogram))

(* Bind to the variable top_three_routes a list containing the three most frequently appearing
route numbers in route_histogram. (Hint: use take and another function from part 2.)*)


let buses_in_ud         = filter_access_path_in_rect(["vehicle"; "position"], u_district, complete_bus_positions_list)

(*Bind to the variable buses_in_ud a list containing all records referring to buses in the U
district. For the purposes of this problem, the U district is defined by the rectangle bound to the
variable u_district in the provided code.
*)


let ud_route_histogram  = histogram_for_access_path(["vehicle"; "trip"; "route_num"], buses_in_ud)



(* Bind to the variable ud_route_histogram a histogram of the objects in buses_in_ud based on
the "route_num" field, as in problem 21.*)

let top_three_ud_routes =  take(3, firsts(ud_route_histogram))


(*Bind to the variable top_three_ud_routes a list containing the three most frequently appearing
route numbers in ud_route_histogram, as in problem 22*)
let all_fourty_fours    = filter_access_path_value(["vehicle"; "trip"; "route_num"], "44", complete_bus_positions_list)
(*Bind to the variable all_fourty_fours a list containing all records from complete_bus_positions
that refer to a vehicle whose (suitably nested, as in problem 21) route_num field is "44".*)
