(* CSE 341, Homework 4 Tests *)
open Hw4
open Hw4challenge
open Json

(* This file provides a list of basic tests for your homework.
 * You will surely want to add more! These tests do not guarantee that your code
 * is correct.
 *)

(* 1 *)
let%test _ =
  make_silly_json 2
  =
  Array
    [Object [("n", Num 2.); ("b", True)];
     Object [("n", Num 1.); ("b", True)]]

     let%test _ =
     make_silly_json 3
     =
     Array
       [Object [("n", Num 3.); ("b", True)];
         Object [("n", Num 2.); ("b", True)];
        Object [("n", Num 1.); ("b", True)]]

        
        let%test _ =
        make_silly_json 4
        =
        Array
          [Object [("n", Num 4.); ("b", True)];
            Object [("n", Num 3.); ("b", True)];
            Object [("n", Num 2.); ("b", True)];
           Object [("n", Num 1.); ("b", True)]]
   
           


     
(* 2 *)
let%test _ = concat_with (";", ["1"; "2"]) = "1;2"
let%test _ = concat_with (";", []) = ""
let%test _ = concat_with ("-", ["Hello"; "my"; "name"; "is"; "Chris"]) = "Hello-my-name-is-Chris"

(* 3 *)
let%test _ = quote_string "hello" = "\"hello\""

let%test _ = quote_string "Tell me what you want what you really really want" = "\"Tell me what you want what you really really want\""
let%test _ = quote_string "Not all who wander are lost" = "\"Not all who wander are lost\""


(* 4 *)
let%test _ = string_of_json json_obj = "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}"
let%test _ = string_of_json (Array[Object [("test", Num 2.0); ("free", Num 3.0)]]) = "[{\"test\" : 2, \"free\" : 3}]"
let%test _ = string_of_json (Array[Object [("test", Num 2.0); ("free", Num 3.0)];
 Object [("test", Num 2.0); ("free", Num 3.0)]]) = "[{\"test\" : 2, \"free\" : 3}, {\"test\" : 2, \"free\" : 3}]"
let%test _ = string_of_json (Array[]) = "[]"
let%test _ = string_of_json (Object [("test", True); ("free", False)]) = "{\"test\" : true, \"free\" : false}"
(* 5 *)
let%test _ = take (2, [4; 5; 6; 7]) = [4; 5]
let%test _ = take (3, ["hey"; "hi"; "yo"; "sup"]) = ["hey"; "hi"; "yo"]
let%test _ = take (0, [true; false; true; false]) = []
let%test _ = take (4, [true; false; true; false]) = [true; false; true; false]


(* 6 *)
let%test _ = firsts [(1,2); (3,4)] = [1; 3]
let%test _ = firsts [("1",2); ("2",4)] = ["1"; "2"]
let%test _ = firsts [(false, true); (true, false)] = [false; true]
let%test _ = firsts [] = []

(** Don't forget to write a comment for problem 7 in your other file! **)

(* 8 *)
let%test _ = assoc ("foo", [("bar",17);("foo",19)]) = Some 19
let%test _ = assoc ("bar", [("bar",17);("foo",19)]) = Some 17
let%test _ = assoc (5, [(5,17);(5,19)]) = Some 17
let%test _ = assoc ("zoom", [("bar",17);("foo",19)]) = None

(* 9 *)
let%test _ = dot (json_obj, "ok") = Some True
let%test _ = dot((Object [("test", True); ("free", False)]), "free") = Some False
let%test _ = dot((Array []), "hey") = None

(* 10 *)
let%test _ = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"]) = Some (String "gotcha")
let%test _ = dots (Object [("f", Object [("g", String "gotcha")])], ["f"]) = Some (Object [("g", String "gotcha")])
let%test _ = dots (Object [("f", String "hey")], ["f"; "g"]) = None

(* 11 *)
let%test _ = one_fields json_obj = List.rev ["foo";"bar";"ok"]
let%test _ = one_fields (Array[Object["test", String "hi"]]) = []
let%test _ = one_fields((Object [("f", Object [("g", String "gotcha")])])) = List.rev ["f"]
let%test _ = one_fields(Object["test", String "hi"]) = List.rev["test"]

(* 12 *)
let%test _ = not (no_repeats ["foo";"bar";"foo"])
let%test _ = no_repeats ["foo";"bar";"zoom"]
let%test _ = not (no_repeats ["foo";"bar";"zoom"; "room"; "boom"; "bar"])
let%test _ = no_repeats []

(* 13 *)
let nest = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("foo",True);
                                     ("foo",True)]);
                         ("c",True)];
                  Object []]
let%test _ = not (recursive_no_field_repeats nest)
let nest2 = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("bar",True);
                                     ("foo",True)]);
                         ("c",True)];
                  Object []]
let%test _ = recursive_no_field_repeats nest2

let nest3 = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("bar",True);
                                     ("foo", Object[
                                              ("test", Num 2.0);
                                              ("test", Num 3.0)
                                              ])]);
                         ("c",True)];
                  Object []]
                  let%test _ = not (recursive_no_field_repeats nest3)
                  let%test _ = recursive_no_field_repeats (Array [])

let%test _ = recursive_no_field_repeats (Array [Object []])

let%test _ = not(recursive_no_field_repeats (Array [Object [("a", True); ("a", False)]]))

let%test _ = not(recursive_no_field_repeats (Array [Object [("a", True); ("b", False)]; Object [("b", True); ("b", False)]]))

let%test _ = recursive_no_field_repeats (Array [Object [("a", True); ("b", False)]; Object [("a", True); ("b", False)]])

let%test _ = not(recursive_no_field_repeats (Array [Object [("a", True); ("b", False)]; Object [("a", True); ("b", Object [("b", True); ("b", False)])]]))


(* 14 *)
(* Any order is allowed by the specification, so it's ok to fail this
   test because of a different order. You can edit this test to match
   your implementation's order. *)
let%test _ = count_occurrences (["a"; "a"; "b"], (Failure "")) = [("b",1); ("a",2)]

let%test _ = count_occurrences (["a"; "a"; "a"; "b"; "b"; "c"; "c"; "c"], (Failure "")) = [("c", 3); ("b",2); ("a",3)]

(* test to see that an exception is thrown when the input list is not sorted *)
let%test _ = try count_occurrences (["b"; "a"; "b"], (Failure "")) = []
             with Failure _ -> true


(* test to see that an exception is thrown when the input list is not sorted *)
let%test _ = try count_occurrences (["something"; "is"; "definitely"; "out"; "of"; "order"], (Failure "")) = []
             with Failure _ -> true


let%test _ = count_occurrences (["a"; "a"; "a"; "b"; "b"; "c"; "c"; "c"; "d"; "d"; "d"; "d"; "d"], (Failure ""))
 = [("d", 5); ("c", 3); ("b",2); ("a",3)]

 let%test _ = count_occurrences ([], (Failure "")) = []


(* 15 *)
let%test _ =
  string_values_for_access_path (
      ["x"; "y"],
      [Object [("a", True);("x", Object [("y", String "foo")])];
       Object [("x", Object [("y", String "bar")]); ("b", True)]]
  ) = ["foo";"bar"]

  let%test _ =
  string_values_for_access_path (
      ["x"; "y"],
      []
  ) = []

  let%test _ =
  string_values_for_access_path (
      ["x"; "y"; "z"],
      [Object [("a", True);("x", Object [("y", String "foo")])];
       Object [("x", Object [("y", String "bar")]); ("b", True)]]
  ) = []

  let%test _ =
  string_values_for_access_path (
      ["x"; "y"; "z"],
      [Object [("a", True);("x", Object [("y", Object ["z" ,String "foo"])])];
       Object [("x", Object [("y", String "bar")]); ("b", True)]]
  ) = ["foo"]

(* 16 *)
let%test _ =
  filter_access_path_value (
      ["x"; "y"],
      "foo",
      [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
       Object [("x", Object [("y", String "foo")]); ("z", String "baz")];
       Object [("x", String "a")];
       Object []]
  ) = [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
       Object [("x", Object [("y", String "foo")]); ("z", String "baz")]]


let%test _ =
  filter_access_path_value (
      ["1"; "2"; "3"],
      "foo",
      [Object [("1", Object [("2", Object[("bumpOne", String "foo");
       ("bumpTwo", String "foo"); ("3", String "foo")])]); ("z", String "bar")]]
  ) = [Object [("1", Object [("2", Object[("bumpOne", String "foo");
   ("bumpTwo", String "foo"); ("3", String "foo")])]); ("z", String "bar")]]

  let%test _ =
  filter_access_path_value (
      ["1"; "2"; "3"; "4"],
      "foo",
      [Object [("1", Object [("2", Object[("bumpOne", String "foo"); ("bumpTwo", String "foo");
       ("3", String "foo")])]); ("z", String "bar")]]
  ) = []

  let pgascse =
  { latitude = 47.653221;
    longitude = -122.305708 }

let newRect =
  {
    min_latitude = 25.0;
    max_latitude = 50.0;
    
    min_longitude = 25.0;
    max_longitude = 50.0
  }


(* 17 *)
let%test _ = in_rect (u_district, pgascse)

let%test _ = in_rect (newRect, {latitude = 26.0; longitude = 26.0})
let%test _ = not(in_rect (newRect, {latitude = 60.0; longitude = 26.0}))
let%test _ = not(in_rect (newRect, {latitude = 26.0; longitude = 60.0}))
let%test _ = not(in_rect (newRect, {latitude = 24.0; longitude = 26.0}))
let%test _ = not(in_rect (newRect, {latitude = 26.0; longitude = 24.0}))




let json_pgascse = Object [("latitude", Num 47.653221); ("longitude", Num (-122.305708))]

(* 18 *)
let%test _ = point_of_json json_pgascse = Some pgascse


let json_point_one = Object [("latitude", Num 47.653221); ("longitude", String "hi")]

let%test _ = point_of_json json_point_one = None

let json_point_two = Object [("latitude", String "hi"); ("longitude", String "hi")]

let%test _ = point_of_json json_point_two = None

let json_point_three = Object [("latitude", String "hi"); ("longitude", Num 47.653221)]

let%test _ = point_of_json json_point_three = None

let json_point_three = Object [("test", Num 47.653221); ("test2", Num 47.653221)]

let%test _ = point_of_json json_point_three = None


let json_point_three = Object [("latitude", Num 200.0); ("longitude", Num 200.0)]

let%test _ = point_of_json json_point_three = Some { latitude = 200.0;
longitude = 200.0}

(* 19 *)
let%test _ =
  filter_access_path_in_rect (
      ["x"; "y"],
      u_district,
      [Object [("x", Object [("y", json_pgascse)])]; Object []]
  ) = [Object [("x", Object [("y", json_pgascse)])]]

  let newRect =
    {
      min_latitude = 25.0;
      max_latitude = 50.0;
      
      min_longitude = 25.0;
      max_longitude = 50.0
    }

    let json_point = Object [("latitude", Num 47.653221); ("longitude", Num 47.0)]

    let%test _ =
    filter_access_path_in_rect (
        ["x"; "y"],
        newRect,
        [Object [("x", Object [("y", json_point)])]; Object []]
    ) =  [Object [("x", Object [("y", json_point)])]]
 
  
    let%test _ =
    filter_access_path_in_rect (
        ["x"; "y"; "z"],
        newRect,
        [Object [("x", Object [("y", json_point)])]; Object []]
    ) =  []
 

    let%test _ =
    filter_access_path_in_rect (
        ["x"; "y"],
        u_district,
        [Object [("x", Object [("y", json_pgascse)])]; Object []]
    ) = [Object [("x", Object [("y", json_pgascse)])]]
  
    let%test _ =
    filter_access_path_in_rect (
        ["x"; "y"; "z"],
        newRect,
        [Object [("x", Object [("y", Object [("z", json_point)])])]; Object [("x", Object [("y", Object [("z", json_point)])])]]
    ) =  [Object [("x", Object [("y", Object [("z", json_point)])])]; Object [("x", Object [("y", Object [("z", json_point)])])]]
 

    let%test _ =
    filter_access_path_in_rect (
        ["1"; "2"; "3"],
        newRect,
        [Object [("1", Object [("2", Object [("3", json_point)])])]; Object [("x", Object [("y", Object [("z", json_point)])])]]
    ) =  [Object [("1", Object [("2", Object [("3", json_point)])])]]
 

    let%test _ =
    filter_access_path_in_rect (
        ["hi"; "future"; "grader"],
        u_district,
        [Object [("hi", Object [("future", Object [("grader", json_point)])])]; Object [("hi", Object [("future", Object [("grader", json_pgascse)])])]]
    ) =  [Object [("hi", Object [("future", Object [("grader", json_pgascse)])])]]
 

    let%test _ =
    filter_access_path_in_rect (
        ["hi"; "future"; "grader"],
        u_district,
        []
    ) =  []



(** Don't forget to write a comment for problem 20 in your other file! **)

(* You do not need to test your solutions to Problems 21-26. *)


(* Challenge problems *)

(* Uncomment these tests if you do the challenge problems. *)

(*
(* C1 *)
let%test _ = consume_string_literal (char_list_of_string "\"foo\" : true") = ("foo", [' '; ':'; ' '; 't'; 'r'; 'u'; 'e'])

(* C2 *)
let%test _ = consume_keyword (char_list_of_string "false foo") = (FalseTok, [' '; 'f'; 'o'; 'o'])

(* C3 *)
let%test _ = tokenize_char_list (char_list_of_string "{ \"foo\" : 3.14, \"bar\" : [true, false] }")
             = [LBrace; StringLit "foo"; Colon; NumLit "3.14"; Comma; StringLit "bar";
                Colon; LBracket; TrueTok; Comma; FalseTok; RBracket; RBrace]

(* C4 *)
(* You should write a test for C4 here. *)


(* C5 *)
let%test _ = parse_string [StringLit "foo"; FalseTok] = ("foo", [FalseTok])

(* C6 *)
let%test _ = expect (Colon, [Colon; FalseTok]) = [FalseTok]

(* C6 through C10 *)
let%test _ = parse_json (tokenize "{ \"foo\" : null, \"bar\" : [true, false] }")
             = (Object [("foo", Null); ("bar", Array [True; False])], [])

(* C11 *)

*)
