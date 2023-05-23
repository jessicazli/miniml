(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
  | FloatNegate
  | Sin 
  | Cos 
  | Tan
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Divide 
  | FloatPlus
  | FloatMinus
  | FloatTimes
  | FloatDivide 
  | Power
  | Equals
  | LessThan
  | GreaterThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Float of float                       (* floats *)
  | Bool of bool                         (* booleans *)
  | String of string                     (* strings *)
  | Unit                                 (* units *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var v -> SS.singleton v
  | Num _ | Bool _ | Float _ | String _ | Unit | Raise | Unassigned -> SS.empty
  | Unop (_, e) -> free_vars e
  | Binop (_, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) -> 
    SS.union (free_vars e1) (free_vars e2) |> SS.union (free_vars e3)
  | Fun (v, e) -> SS.remove v (free_vars e)
  | Let (v, e1, e2) -> SS.union (free_vars e1) (SS.remove v (free_vars e2))
  | Letrec (v, e1, e2) -> SS.remove v (SS.union (free_vars e1) (free_vars e2))
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)

(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no other variable names
   use the prefix "var". (Otherwise, they might accidentally be the
   same as a generated variable name.) *)

let gensym : string -> string =
  let suffix = ref 0 in
  fun str -> let symbol = str ^ string_of_int !suffix in
              suffix := !suffix + 1;
              symbol ;;

let new_varname () : varid =
  gensym "var" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  let rec sub_this (exp : expr) : expr =
  match exp with
  | Var v -> if v = var_name then repl else exp
  | Num _ | Bool _ | Float _ | String _ | Unit | Raise | Unassigned -> exp
  | Unop (u, e) -> Unop (u, sub_this e)
  | Binop (b, e1, e2) -> 
    Binop (b, sub_this e1, sub_this e2)
  | Conditional (e1, e2, e3) -> 
    Conditional (sub_this e1, sub_this e2, sub_this e3)
  | Fun (v, e) -> 
    if v = var_name then exp 
    else if SS.mem v (free_vars repl) then 
      let z = new_varname () in
      Fun (z, sub_this (subst v (Var z) e))
    else Fun (v, sub_this e)
  | Let (v, e1, e2) -> 
    if v = var_name then Let (v, sub_this e1, e2)
    else if SS.mem v (free_vars repl) then 
      let z = new_varname () in
      Let (z, sub_this e1, sub_this (subst v (Var z) e2))
    else Let (v, sub_this e1, sub_this e2)
  | Letrec (v, e1, e2) -> 
    if v = var_name then Letrec (v, sub_this e1, e2)
    else if SS.mem v (free_vars repl) then 
      let z = new_varname () in
      Letrec (z, sub_this e1, sub_this (subst v (Var z) e2))
    else Letrec (v, sub_this e1, sub_this e2)
  | App (e1, e2) -> App (sub_this e1, sub_this e2) in
  sub_this exp ;;
  
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var v -> v
  | Num n -> string_of_int n
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Unit -> "()"
  | Unop (u, e) ->
    let e_str = exp_to_concrete_string e in
    let u_str =
      match u with
      | Negate -> "~-" 
      | FloatNegate -> "~-."
      | Sin -> "sin "
      | Cos -> "cos "
      | Tan -> "tan " in
    u_str ^ e_str
  | Binop (b, e1, e2) ->
    let e1_str, e2_str = exp_to_concrete_string e1, 
                         exp_to_concrete_string e2 in
    let b_str =
      match b with
      | Plus -> " + "
      | Minus -> " - "
      | Times -> " * "
      | Divide -> " / "
      | FloatPlus -> " +. "
      | FloatMinus -> " -. "
      | FloatTimes -> " *. "
      | FloatDivide -> " /. "
      | Power -> " ** "
      | Equals -> " = "
      | LessThan -> " < " 
      | GreaterThan -> " > " in
    e1_str ^ b_str ^ e2_str
  | Conditional (e1, e2, e3) ->
    let e1_str, e2_str, e3_str = exp_to_concrete_string e1, 
                                 exp_to_concrete_string e2,
                                 exp_to_concrete_string e3 in 
    "if " ^ e1_str ^ " then " ^ e2_str ^ " else " ^ e3_str
  | Fun (v, e) -> 
    let e_str = exp_to_concrete_string e in
    "fun " ^ v ^ " -> " ^ e_str
  | Let (v, e1, e2) -> 
    let e1_str, e2_str = exp_to_concrete_string e1, 
                         exp_to_concrete_string e2 in
    "let " ^ v ^ " = " ^ e1_str ^ " in " ^ e2_str
  | Letrec (v, e1, e2) -> 
    let e1_str, e2_str = exp_to_concrete_string e1, 
                         exp_to_concrete_string e2 in
    "let rec " ^ v ^ " = " ^ e1_str ^ " in " ^ e2_str
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (e1, e2) -> 
    let e1_str, e2_str = exp_to_concrete_string e1, 
                         exp_to_concrete_string e2 in
    e1_str ^ " " ^ e2_str

     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var(\"" ^ v ^ "\")"
  | Num n -> "Num(" ^ string_of_int n ^ ")"
  | Float f -> "Float(" ^ string_of_float f ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | String s -> "String(\"" ^ s ^ "\")"
  | Unit -> "Unit"
  | Unop (u, e) -> 
    let e_str = exp_to_abstract_string e in
    let u_str =
      match u with
      | Negate -> "Negate" 
      | FloatNegate -> "FloatNegate"
      | Sin -> "Sin"
      | Cos -> "Cos"
      | Tan -> "Tan" in
    "Unop(" ^ u_str ^ ", " ^ e_str ^ ")"
  | Binop (b, e1, e2) -> 
    let e1_str, e2_str = exp_to_abstract_string e1, 
                          exp_to_abstract_string e2 in
    let b_str =
      match b with
      | Plus -> "Plus"
      | Minus -> "Minus"
      | Times -> "Times"
      | Divide -> "Divide"
      | FloatPlus -> "FloatPlus"
      | FloatMinus -> "FloatMinus"
      | FloatTimes -> "FloatTimes"
      | FloatDivide -> "FloatDivide"
      | Power -> "Power"
      | Equals -> "Equals"
      | LessThan -> "LessThan" 
      | GreaterThan -> "GreaterThan" in
    "Binop(" ^ b_str ^ ", " ^ e1_str ^ ", " ^ e2_str ^ ")"
  | Conditional (e1, e2, e3) -> 
    let e1_str, e2_str, e3_str = exp_to_abstract_string e1, 
                                  exp_to_abstract_string e2,
                                  exp_to_abstract_string e3 in 
    "Conditional(" ^ e1_str ^ ", " ^ e2_str ^ ", " ^ e3_str ^ ")"
  | Fun (v, e) -> 
    let e_str = exp_to_abstract_string e in
    "Fun(\"" ^ v ^ "\", " ^ e_str ^ ")"  
  | Let (v, e1, e2) -> 
    let e1_str, e2_str = exp_to_abstract_string e1, 
                          exp_to_abstract_string e2 in
    "Let(\"" ^ v ^ "\", " ^ e1_str ^ ", " ^ e2_str ^ ")"
  | Letrec (v, e1, e2) -> 
    let e1_str, e2_str = exp_to_abstract_string e1, 
                          exp_to_abstract_string e2 in
    "Letrec(\"" ^ v ^ "\", " ^ e1_str ^ ", " ^ e2_str ^ ")"
  | Raise -> "raise"
  | Unassigned -> "unassigned"
  | App (e1, e2) -> 
    let e1_str, e2_str = exp_to_abstract_string e1, 
                          exp_to_abstract_string e2 in
    "App(" ^ e1_str ^ ", " ^ e2_str ^ ")"