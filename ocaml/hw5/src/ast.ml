open Errors

(* IGNORE the first 50ish lines (up to "type expr = ...") until working on part 2! *)
type pattern =
  | WildcardPattern
  | ConsPattern of pattern * pattern
  | IntLitPattern of int
  | BoolLitPattern of bool
  | NilPattern
  | SymbolPattern of string
  | VariablePattern of string
  | StructPattern of string * pattern list
(* TODO: add more patterns here *)
[@@deriving show]
let string_of_pattern = show_pattern

let rec vars_of_pattern ((pat : pattern), acc) = 
   match pat with
   | VariablePattern t -> (t :: acc)
   | ConsPattern (p1, p2) -> vars_of_pattern(p1, acc) @ vars_of_pattern(p2, acc)
   | StructPattern (name, patternList) -> 
      let rec looper (lister, returner) =
        match lister with
        | [] -> List.rev(returner)
        | x::xs ->  (vars_of_pattern(x, returner) @ looper(xs,  returner))
      in 
      looper(patternList, []) @ acc
   | _ -> []


let rec pattern_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
     try
       let n = int_of_string sym in
           IntLitPattern(n)
     with
       Failure _ ->
       match sym with
       | "_" -> WildcardPattern
       | "true" -> BoolLitPattern(true)
       | "false" -> BoolLitPattern(false)
       | "nil" -> NilPattern

       (* TODO: add other cases here for "false" and "nil" *)
       | _ ->
          if String.get sym 0 = '\'' (* if the string starts with an apostrophe *)
          then let sym_without_apostrophe = String.sub sym 1 (String.length sym - 1)
               in SymbolPattern(sym_without_apostrophe)
          else VariablePattern(sym)
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected pattern but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "cons", [p1; p2] -> ConsPattern (pattern_of_pst p1, pattern_of_pst p2)
     | Pst.Symbol s, ps ->
       
      let rec looper (patterns, acc) =
        match patterns with
        | [] -> List.rev(acc)
        | x :: xs -> looper(xs, pattern_of_pst(x) :: acc)
      in
      StructPattern(s, looper(ps, []))

     | _ -> raise (AbstractSyntaxError ("Expected pattern, but got " ^ Pst.string_of_pst p))

let pattern_of_string s =
  s
  |> Pstparser.pst_of_string
  |> pattern_of_pst

type expr =
  | IntLit of int
  | BoolLit of bool
  | Variable of string
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Equals of expr * expr
  | If of expr * expr * expr
(* TODO: add constructor for let expressions here *)
  | Let of string * expr * expr
  | Nil
  | Cons of expr * expr
  | IsNil of expr
  | IsCons of expr
  | Car of expr
  | Cdr of expr
  | Call of string * expr list

  (* More constructors for Trefoil v3 below. Ignore during Part 1. *)
  | Symbol of string
  | Cond of (expr * expr) list
  | StructConstructor of string * expr list  (* internal AST node; not written by Trefoil programmer *)
  (* TODO: add other "internal" expression ASTs here *)
  | StructPredicate of string * expr
  | StructAccess of string * int * expr
  (* TODO: add match expression constructor to the expr type here *)
  | Match of expr * (pattern * expr) list

(*Match expression: a node with head match and at least one argument.
The first argument is an expression.
Any remaining arguments are match clauses.
A match clause is a node with exactly two children.
The first child is a pattern.
If the pattern contains reuses variable patterns with the same name more
than once, it is an abstract syntax error.
The second child is an expression.
Example: The body of this function is a match expression with two clauses.*)



[@@deriving show]
let string_of_expr = show_expr

let has_duplicates (l: string list) =
  let sorted = List.sort compare l in
  let rec loop xs =
    match xs with
    | [] -> false
    | [_] -> false
    | x :: y :: zs -> x = y || loop (y :: zs)
  in
  loop sorted



  
let rec expr_of_pst p =
  match p with
  | Pst.Symbol sym -> begin
     try
       IntLit (int_of_string sym)
     with
       Failure _ ->
       match sym with
       | "true" -> BoolLit true
       | "false" -> BoolLit false
       | "nil" -> Nil
       | _ ->
          if String.get sym 0 = '\''
          then Symbol (String.sub sym 1 (String.length sym - 1))
          else Variable sym
    end
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected expression but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Node _, _ -> raise (AbstractSyntaxError ("Expression forms must start with a symbol, but got " ^ Pst.string_of_pst head))
     | Pst.Symbol "+", [left; right] -> Plus (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "+", _ -> raise (AbstractSyntaxError ("operator + expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "-", [left; right] -> Minus (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "-", _ -> raise (AbstractSyntaxError ("operator - expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "*", [left; right] -> Times (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "*", _ -> raise (AbstractSyntaxError ("operator * expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "=", [left; right] -> Equals (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "=", _ -> raise (AbstractSyntaxError ("operator = expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "if", [branch; thn; els] -> If (expr_of_pst branch, expr_of_pst thn, expr_of_pst els)
     | Pst.Symbol "if", _ -> raise (AbstractSyntaxError ("'if' special form expects 3 args but got " ^ Pst.string_of_pst p))
     (* TODO: add cases for let expressions here *)
     | Pst.Symbol "let", [left; right] ->
        (match left with
        | Pst.Node [Pst.Node[leftTwo; rightTwo;]] -> Let (Pst.string_of_pst leftTwo, expr_of_pst rightTwo, expr_of_pst right)
        | _ -> 
          raise (AbstractSyntaxError ("'let' expects 2 args for variable binding but got " ^ Pst.string_of_pst left))
        ) 
      |Pst.Symbol "let", _ -> raise (AbstractSyntaxError ("'let' expects 2 args but got " ^ Pst.string_of_pst p))
      
      
     | Pst.Symbol "cons", [left; right] -> Cons (expr_of_pst left, expr_of_pst right)
     | Pst.Symbol "cons", _ -> raise (AbstractSyntaxError ("cons expects 2 args but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "nil?", [arg] -> IsNil (expr_of_pst arg)
     | Pst.Symbol "nil?", _ -> raise (AbstractSyntaxError ("nil? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cons?", [arg] -> IsCons (expr_of_pst arg)
     | Pst.Symbol "cons?", _ -> raise (AbstractSyntaxError ("cons? expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "car", [arg] -> Car (expr_of_pst arg)
     | Pst.Symbol "car", _ -> raise (AbstractSyntaxError ("car expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cdr", [arg] -> Cdr (expr_of_pst arg)
     | Pst.Symbol "cdr", _ -> raise (AbstractSyntaxError ("cdr expects 1 arg but got " ^ Pst.string_of_pst p))
     | Pst.Symbol "cond", clauses ->
        (* converts a list of cond clauses in PST syntax into their abstract
           syntax as a list of pairs of expressions. *)
        let rec clause_loop (clauses: Pst.pst list): (expr * expr) list =
          match clauses with
          | [] -> []
          | Pst.Node [e1; e2] :: xs ->
             (* TODO: replace "[]" below with code to parse a cond clause.
                - Hint: convert e1 and e2 to expressions, pair them up, and cons
                  them onto the recursive call on xs *)
                let expressionOne = expr_of_pst(e1) in
                let expressionTwo = expr_of_pst(e2) in
             
                 (expressionOne, expressionTwo) :: clause_loop(xs)
          | x :: _ -> raise (AbstractSyntaxError("Malformed 'cond' clause: " ^ Pst.string_of_pst x))
        in
        Cond (clause_loop clauses)



     (* TODO: add parsing for match expressions here *)
     | Pst.Symbol "match", expr :: (clauses : Pst.pst list) ->
      
     let rec clause_loop (clauses: Pst.pst list): (pattern * expr) list =
      match clauses with
      | [] -> []
      | Pst.Node [e1; e2] :: xs ->
            let expressionOne = pattern_of_pst(e1) in
            let expressionTwo = expr_of_pst(e2) in

          let boolean = has_duplicates(vars_of_pattern(expressionOne, [])) in
            
          if(boolean)
            then  raise (AbstractSyntaxError("There can be no variable name duplicates")) 
          else
            (expressionOne, expressionTwo) :: clause_loop(xs)
       | x :: _ -> raise (AbstractSyntaxError("Malformed 'match' clause: " ^ Pst.string_of_pst x)) in

          Match(expr_of_pst(expr), clause_loop(clauses))


     (* Otherwise, if it doesn't match any of the above, it's a function call. *)
     | Pst.Symbol f, args ->
        let rec args_loop args =
          match args with
          | [] -> []
          | arg :: args -> expr_of_pst arg :: args_loop args
        in
        Call (f, args_loop args)

let expr_of_string s =
  s
  |> Pstparser.pst_of_string
  |> expr_of_pst

(* checks that all the psts are symbols, and if so, convert to a list of
   strings. if not, throw AbstractSyntaxError. *)
let rec check_symbols (msg, pst_list) =
  match pst_list with
  | [] -> []
  | Pst.Symbol name :: xs -> name :: check_symbols (msg, xs)
  | p :: _ -> raise (AbstractSyntaxError(msg ^ " must be symbol but got " ^ Pst.string_of_pst p))

type function_binding = { name: string; param_names: string list; body: expr }
[@@deriving show]

type struct_binding = { name: string; field_names: string list }
[@@deriving show]

type binding =
   | VarBinding of string * expr
   | TopLevelExpr of expr
   | FunctionBinding of function_binding
   (* TODO: add a constructor for test bindings here *)
   | TestBinding of expr
   | StructBinding of struct_binding
[@@deriving show]
let string_of_binding = show_binding

let binding_of_pst p =
  match p with
  | Pst.Symbol _ -> TopLevelExpr (expr_of_pst p)
  | Pst.Node [] -> raise (AbstractSyntaxError "Expected binding but got '()'")
  | Pst.Node (head :: args) ->
     match head, args with
     | Pst.Symbol "define", [Pst.Symbol lhs_var; rhs] -> VarBinding (lhs_var, expr_of_pst rhs)
     | Pst.Symbol "define", [Pst.Node (Pst.Symbol name :: param_names); rhs] ->
        let param_names = check_symbols ("Function parameter", param_names) in
        if has_duplicates (name :: param_names)
        then raise (AbstractSyntaxError("Function binding reuses a symbol multiple times as a function name or parameter name"));
        FunctionBinding {name; param_names; body = expr_of_pst rhs}
     | Pst.Symbol "define", _ -> raise (AbstractSyntaxError("This definition is malformed " ^ Pst.string_of_pst p))
     (* TODO: parse test bindings here *)
     | Pst.Symbol "test", [rhs] -> TestBinding(expr_of_pst rhs)
     | Pst.Symbol "test", _ ->  raise (AbstractSyntaxError("'test' bindings must have only one argument that is an expression"))
     | Pst.Symbol "struct", Pst.Symbol name :: field_names ->
        (* note: a struct with a field of the same name as the struct itself is allowed *)
        
        
        let fielder = check_symbols(name, field_names) in
        if has_duplicates(fielder)
           then raise (AbstractSyntaxError("All struct argument names must be distinct"))
        else
           StructBinding {name; field_names = fielder}


       (*type struct_binding = { name: string; field_names: string list }*)


     | Pst.Symbol "struct", _ -> raise (AbstractSyntaxError("'struct' bindings must at least one argument, but got none"))

     | Pst.Node _, _ -> raise (AbstractSyntaxError("Expected binding to start with a symbol but got " ^ Pst.string_of_pst p))
     | _ -> TopLevelExpr (expr_of_pst p)

let binding_of_string s =
  s
  |> Pstparser.pst_of_string
  |> binding_of_pst

let bindings_of_string s =
  let p = Pstparser.pstparser_of_string s in
  let rec parse_binding_list () =
    match Pstparser.parse_pst p with
    | None -> []
    | Some pst ->
       binding_of_pst pst :: parse_binding_list ()
  in
  parse_binding_list ()
