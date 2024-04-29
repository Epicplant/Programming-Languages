open Ast
open Errors

type entry =
  | VariableEntry of expr
  | FunctionEntry of function_binding * dynamic_env
  | StructEntry of struct_binding
[@@deriving show]
and dynamic_env = (string * entry) list
let entry_of_string = show_entry

let rec lookup (dynenv, name) =
  match dynenv with
  | [] -> None
  | (x, entry) :: dynenv ->
     if x = name
     then Some entry
     else lookup (dynenv, name)

(* ignore this until working on part 2 *)
let rec interpret_pattern (pattern, value): (string * entry) list option =
  match pattern, value with
  
  | IntLitPattern n, IntLit num  -> begin
    if n = num
      then Some []
    else None

    end
  | BoolLitPattern b, BoolLit boo -> begin
    if b = boo
      then Some []
     else None


      end
  | NilPattern, Nil ->  Some []
  | SymbolPattern s, Symbol s2 -> begin
    
      if s = s2
        then Some []
      else None  
    end  
    

  | VariablePattern v, x -> Some [(v, (VariableEntry x))]
  | StructPattern (namePat, patterns), StructConstructor(nameCon, expressions) -> begin
    if(namePat <> nameCon)
      then None
  else if (List.length(patterns) <> List.length(expressions))
    then None
  else
    let rec looper (pi, vi, acc) =
      match pi with
      | [] -> List.rev(acc)
      | x :: xs -> 
        match vi with
        | [] -> List.rev(acc)
        | y :: ys ->
          match interpret_pattern(x, y) with
          | Some s -> looper (xs, ys, s @ acc)
          | None -> looper(xs, ys, acc)
 in

 let newEnv =  looper(patterns, expressions, []) in

 if(List.length(newEnv) < List.length(patterns))
  then None
else

    Some(newEnv)

  end
  | WildcardPattern, _ -> Some []
  | ConsPattern (p1, p2), Cons (v1, v2) -> begin
     match interpret_pattern (p1, v1), interpret_pattern (p2, v2) with
     | Some l1, Some l2 -> Some (l1 @ l2)
     | _ -> None
    end
  
  (* TODO: add cases for other kinds of patterns here *)

  | _ -> None

let rec interpret_expression (dynenv, e) =
  (* helper function to interpret a list of expressions into a list of values *)
  let rec interpret_list (dynenv, es) =
    match es with
    | [] -> []
    | e :: es -> interpret_expression (dynenv, e) :: interpret_list (dynenv, es)
  in
  match e with
  | IntLit _ | BoolLit _ | Nil | StructConstructor _ -> e
  | Variable x -> begin
      match lookup (dynenv, x) with
      | None -> raise (RuntimeError ("Unbound variable " ^ x))
      | Some (VariableEntry value) -> value
      | Some e -> raise (RuntimeError ("Expected name " ^ x ^ " to refer to a variable, but got something else: " ^ entry_of_string e))
    end
  | Plus (e1, e2) -> begin
      match interpret_expression (dynenv, e1), interpret_expression (dynenv, e2) with
      | IntLit n1, IntLit n2 -> IntLit (n1 + n2)
      | IntLit _, v2 -> raise (RuntimeError ("Plus applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Plus applied to non-integer " ^ string_of_expr v1))
    end
  | Minus (e1, e2) -> begin
      match interpret_expression (dynenv, e1), interpret_expression (dynenv, e2) with
      | IntLit n1, IntLit n2 -> IntLit (n1 - n2)
      | IntLit _, v2 -> raise (RuntimeError ("Minus applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Minus applied to non-integer " ^ string_of_expr v1))
    end
  | Times (e1, e2) -> begin
      match interpret_expression (dynenv, e1), interpret_expression (dynenv, e2) with
      | IntLit n1, IntLit n2 -> IntLit (n1 * n2)
      | IntLit _, v2 -> raise (RuntimeError ("Times applied to non-integer " ^ string_of_expr v2))
      | v1, _ -> raise (RuntimeError ("Times applied to non-integer " ^ string_of_expr v1))
    end

  | Equals (e1, e2) -> begin
      match interpret_expression (dynenv, e1), interpret_expression (dynenv, e2) with
        | n1, n2 -> BoolLit (n1 = n2)
      end
  | If (e1, e2, e3) -> begin
      match interpret_expression (dynenv, e1) with
      | BoolLit false -> interpret_expression (dynenv, e3)
      | _ -> interpret_expression (dynenv, e2)
    end
  (* TODO: add case for let expressions here *)
  | Let (e1, e2, e3) -> begin
      match (e1, e2, e3) with
      | ((varname : string), (vardef : expr), (body : expr)) -> 
        (let newenv =  (varname, (VariableEntry vardef )) :: dynenv in
         interpret_expression(newenv, body))
      | _ -> raise (RuntimeError ("Let expressions require a string and two expressions"))
  end
  | Cons (e1, e2) ->
     let v1 = interpret_expression (dynenv, e1) in
     let v2 = interpret_expression (dynenv, e2) in
     Cons (v1, v2)
  | IsNil e -> begin
     match interpret_expression (dynenv, e) with
     | Nil -> BoolLit true
     | _ -> BoolLit false
    end
  | IsCons e -> begin
     match interpret_expression (dynenv, e) with
     | Cons _ -> BoolLit true
     | _ -> BoolLit false
    end
  | Car e -> begin
     match interpret_expression (dynenv, e) with
     | Cons (v1, _)  -> v1
     | v -> raise (RuntimeError("car applied to non-cons " ^ string_of_expr v))
    end
  | Cdr e -> begin
     match interpret_expression (dynenv, e) with
     | Cons (_, v2)  -> v2
     | v -> raise (RuntimeError("car applied to non-cons " ^ string_of_expr v))
    end

  | StructPredicate (s, e) -> begin

    match interpret_expression (dynenv, e) with
    | StructConstructor(s', vs) -> BoolLit (s = s')
    | _ -> BoolLit false

    (*Consider a struct predicate expression StructPredicate(s, e) where s
stands for any string and e stands for any expression AST. Evaluate e in
the current dynamic environment to a value v. If v is of the form
StructConstructor(s', vs) and s and s' are equal as strings, return
true. In all other cases, return false.*)

    end

  | StructAccess ((s : string), (i : int), (e : expr)) -> begin

    match interpret_expression (dynenv, e) with
    | StructConstructor(s', vs) ->
        let rec looper(count, lister) : expr =
          match lister with
          | [] -> raise (RuntimeError("shouldn't happen"))
          | temp::temps ->
            if (count = 0)
            then temp
            else  looper(count-1, temps)
        in
        if ((s = s') && (i < (List.length vs)))
          then 
            looper(i, vs)
          else 
            raise (RuntimeError("Either the strings don't match or index is longer than the length of the list."))

    | _ -> raise (RuntimeError("Not a StructConstructor"))

    (*Consider a struct access expression StructAccess(s, i, e) where s stands
for any string, i stands for any (OCaml) integer, and e stands for any
expression AST. Evaluate e in the current dynamic environment to a value
v. If v is of the form StructConstructor(s', vs) and s and s' are
equal as strings and i is less than the length of vs, then return the
ith element of vs. In all other cases, signal an error.*)

  end

  | Call (fun_name, arg_exprs) -> begin
      let callenv = dynenv in
      match lookup (callenv, fun_name) with
      | None -> raise (RuntimeError ("Unbound function " ^ fun_name))
      | Some ((FunctionEntry (fb, defenv)) as entry) ->
         (* This provided line handles recursion differently than (but
            equivalent to) HW3! When you build the environment to evaluate the
            function body, start from this defenv. *)
            let argnames = fb.param_names in
            let body = fb.body in
            if (List.length(argnames) != List.length(arg_exprs))
          then 
            raise (RuntimeError("Incorrect number of arguments"))
         else
            let defenv = (fun_name, entry) :: defenv in
           
            let rec arglooper (args, acc) =
              (match args with
              | [] -> List.rev(acc)
              | x::xs -> arglooper(xs, interpret_expression(dynenv, x) :: acc)) in 
            let vals = arglooper(arg_exprs, []) in
            let rec bindinglooper (names, values, acc) =
              (match names with
                | [] -> List.rev(acc)
                | x :: xs -> (match values with
                                | [] -> List.rev(acc)
                                | y :: ys -> bindinglooper(xs, ys, (x, VariableEntry (y)) :: acc)  
                            )
              )
          in 
           let newenv = defenv @ bindinglooper(argnames, vals, [])  in
          interpret_expression(newenv, body)
        
      | Some (StructEntry sb) ->
         (* ignore this until part 2 *)

         if((List.length sb.field_names) = (List.length arg_exprs))
          then          
            
            let rec arglooper (args, acc) =
            (match args with
            | [] -> List.rev(acc)
            | x::xs -> arglooper(xs, interpret_expression(dynenv, x) :: acc)) in 
            let vals = arglooper(arg_exprs, []) in

            StructConstructor(fun_name, vals)

         else
          raise (RuntimeError("Incorrect number of arguments"))          


        (*Otherwise, if f maps to a struct entry (whose name
        is the same as f), evaluate each element of args in left-to-right order in
        callenv. Call the resulting sequence of values vals. Return
        StructConstructor(f, vals).*)

    | Some e -> raise (RuntimeError ("Expected name " ^ fun_name ^ " to refer to a function or struct, but got something else: " ^ entry_of_string e))
    end
  | Cond clauses ->
     let rec loop clauses =
       match clauses with
       | [] -> raise (RuntimeError("cond failure: no clauses left"))
       | (predicate, body) :: clauses ->
          if(BoolLit true = interpret_expression(dynenv, predicate))
            then interpret_expression(dynenv, body)
        else
          loop(clauses)
      
      in
     loop clauses
  | Symbol _ -> e

  (* TODO: add cases for the other "internal" expressions here *)
  (* TODO: add case for match expressions here *)
  | Match (e, clauses) -> begin

    let v = interpret_expression(dynenv, e) in

    let rec loop arguments =
      match arguments with
      | [] -> raise (RuntimeError("match failure: no clauses left"))
      | ((pi : pattern), bi) :: clausers ->

          match interpret_pattern(pi, v) with
          | Some args ->
            let newenv = args @ dynenv in
            interpret_expression(newenv, bi)
          | _ -> loop(clausers)
     
     in
     loop(clauses)
  end

  

let interpret_binding (dynenv, b) =
  match b with
  | VarBinding (x, e) ->
     let v = interpret_expression (dynenv, e) in
     Printf.printf "%s = %s\n%!" x (string_of_expr v);
     (x, VariableEntry v) :: dynenv
  | TopLevelExpr e ->
     let v = interpret_expression (dynenv, e) in
     print_endline (string_of_expr v);
     dynenv
  | FunctionBinding fb ->
     Printf.printf "%s is defined\n%!" fb.name;
     (fb.name, FunctionEntry (fb, dynenv)) :: dynenv
  (* TODO: implement test bindings here *)
  | TestBinding tb ->
    let temp = interpret_expression(dynenv, tb) in
      (match temp with
      | BoolLit true -> dynenv
      | _ -> raise (RuntimeError("No such value exists"))
      
      )
  | StructBinding sb ->
     (* TODO: uncomment the comment on the next line and replace the "..." with
        a mapping for the structs name to a StructEntry containing sb. *)
     let dynenv =
      
      (sb.name, StructEntry(sb)) :: dynenv in

 


     (* TODO: create struct predicate function here *)

     let func =  {name = (sb.name ^ "?"); param_names = [("x" : string)];
      body = Ast.StructPredicate(sb.name, (Ast.Variable("x")))} in
     let dynenv = ((sb.name ^ "?"), FunctionEntry(func, dynenv)) :: dynenv in


      (*Back in the StructBinding case for interpret_binding, generate a
function binding for the struct's predicate function.

Hint: The function's definition is described in LANGUAGE.md "as if by the
binding ..." with a binding that is not valid user-facing syntax because it
uses StructPredicate. So, you cannot use the parser to generate this
binding, and you must instead construct it manually using the AST
constructors for function bindings, StructPredicate, and variables.*)


     (* TODO: uncomment this when ready to do accessor functions *)
       
     let fun_entry_for_accessor (idx, field_name): string * entry =
      (sb.name ^ "-" ^ field_name, 
      FunctionEntry({
        name = (sb.name ^ "-" ^ field_name);
        param_names = [("x" : string)];
        body = Ast.StructAccess(sb.name, idx, (Ast.Variable("x")))},
        dynenv)
      ) 
     in
 

     let rec fun_entry_accessor_loop (idx, field_names) =
       match field_names with
       | [] -> []
       | f :: field_names -> fun_entry_for_accessor (idx, f) :: fun_entry_accessor_loop (idx+1, field_names)
     in
     
     let dynenv = fun_entry_accessor_loop (0, sb.field_names) @ dynenv in

     
(*(define (s? x) StructPredicate(s, x))*)     
(*(define (s-f x) StructAccess(s, i, x)) 
for each field f in fs, s-f maps to a function defined as if by the
binding (define (s-f x) StructAccess(s, i, x)) where i is the index of
the field f in the sequence fs.*)

     dynenv

(* the semantics of a whole program (sequence of bindings) *)
let rec interpret_bindings (dynenv, bs) =
  match bs with
  | [] -> dynenv
  | b :: bs ->
     interpret_bindings (interpret_binding (dynenv, b), bs)

(* starting from dynenv, first interpret the list of bindings in order. then, in
   the resulting dynamic environment, interpret the expression and return its
   value *)
let interpret_expression_after_bindings (dynenv, bindings, expr) =
  interpret_expression (interpret_bindings (dynenv, bindings), expr)
