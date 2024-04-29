open Hw6types
open Hw6

(*
We have provided one correct (but usually very boring) test for each
problem. You should add more.

The provided tests are commented out because they all fail at the
moment. Uncomment them as you go.
*)

(* 1 *)

let%test _ = only_lowercase ["hello"; "Plus"; "rip"] = ["hello"; "rip"]
let%test _ = only_lowercase ["Am"; "I"; "nope"] = ["nope"]
let%test _ = only_lowercase [] = []
let%test _ = only_lowercase ["Am"] = []

(* 2 *)
let%test _ = longest_string1 [] = ""
let%test _ = longest_string1 ["1"; "12"; "123"; "12"; "1"] = "123"
let%test _ = longest_string1 ["1"; "12"; "132"; "12"; "1"; "123"] = "132"
let%test _ = longest_string1 ["1"; "12"; "123"; "12"; "1234"; ""; "12345"] = "12345"


(* 3 *)

let%test _ = longest_string2 [] = ""
let%test _ = longest_string2 ["1"; "12"; "123"; "12"; "1"] = "123"
let%test _ = longest_string2 ["1"; "12"; "132"; "12"; "123"] = "123"
let%test _ = longest_string2 ["1"; "12"; "123"; "12"; "1234"; ""; "124"] = "1234"


(* 4 *)

(*Do I need more of these tests?*)
let%test _ = longest_string_helper (>) [] = ""
let%test _ = longest_string_helper (>=) [] = ""
let%test _ = longest_string_helper (>=) ["1"; "12"; "123"; "12"; "1"] = "123"


let%test _ = longest_string3 [] = ""
let%test _ = longest_string3 ["1"; "12"; "123"; "12"; "1"] = "123"
let%test _ = longest_string3 ["1"; "12"; "132"; "12"; "1"; "123"] = "132"
let%test _ = longest_string3 ["1"; "12"; "123"; "12"; "1234"; ""; "12345"] = "12345"



let%test _ = longest_string4 [] = ""
let%test _ = longest_string4 ["1"; "12"; "123"; "12"; "1"] = "123"
let%test _ = longest_string4 ["1"; "12"; "132"; "12"; "123"] = "123"
let%test _ = longest_string4 ["1"; "12"; "123"; "12"; "1234"; ""; "124"] = "1234"

(* 5 *)


let%test _ = longest_lowercase [] = ""
let%test _ = longest_lowercase ["Hello"; "my"; "name"; "is"] = "name"
let%test _ = longest_lowercase ["hello"; "my"; "name"; "is"] = "hello"
let%test _ = longest_lowercase ["hello"; "my"; "name"; "is"; "christopher"] = "christopher"
let%test _ = longest_lowercase ["hello"; "my"; "name"; "is"; "Christopher"] = "hello"

(* 6 *)

let%test _ = caps_no_X_string "" = ""
let%test _ = caps_no_X_string "abcXxXdef" = "ABCDEF"
let%test _ = caps_no_X_string "xXXxaBcXxXdEfXxxX" = "ABCDEF"

(* 7 *)

(*Question: how to write an error test case? Is it supposed to be some 10 or just 10?*)
let%test _ = first_answer (fun x -> Some (x * 10)) [1; 2; 3] = 10

(*
I expect to see an error here. This is simply an error test:

let test = first_answer (fun x -> 
  if(x > 5)
  then Some (x * 10)
  else None
) [1; 2; 3]

*)

let%test _ = first_answer (fun x -> 
  if(x >= 2)
  then Some (x + x + x)
else None) [1; 2; 3] = 6


(* 8 *)

let%test _ = all_answers (fun _ -> failwith "impossible") [] = Some []

let only_lowercaseTest xs = Some(List.filter (fun x -> Char.lowercase_ascii(x.[0]) = x.[0]) xs)
let tester xs = Some(List.filter (fun x -> x >= 6) xs)
let testerTwo xs = None

let%test _ = all_answers only_lowercaseTest [["hello"; "test"; "yo"]; ["I"; "EXIST"; "TO"; "FAIL"]] = Some ["hello"; "test"; "yo"] 
let%test _ = all_answers tester [[4; 5; 6]; [7; 8; 9]] = Some [7; 8; 9; 6] 
let%test _ = all_answers testerTwo [[4; 5; 6]; [7; 8; 9]] = None 