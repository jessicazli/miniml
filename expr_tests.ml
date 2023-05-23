(*
                         CS 51 Final Project
                             Expr Testing
 *)

open CS51Utils ;;
open Absbook ;;
open Expr ;;

let e1 = Var "x" ;;
let e2 = Num 7 ;;
let e3 = Bool true ;;
let e4 = Unop (Negate, Var "x") ;;
let e5 = Binop (Power, Var "x", Num 4) ;;
let e6 = Conditional (Binop (Equals, Var "x", Num 0), Num 1, 
         Binop (Times, Var "x", App (Var "f", Binop (Minus, Var "x", Num 1))));; 
let e7 = Fun ("x", Binop (Plus, Var "x", Var "y")) ;;
let e8 = Let ("f", Fun ("x", Var "x"), App (App (Var "f", Var "f"), Num 3)) ;;
let e9 = Letrec ("f", Fun ("x", Num 5), App (Bool false, Num 3)) ;;
let e10 = Raise ;;
let e11 = Unassigned ;;
let e12 = App (Unop (FloatNegate, Float 3.), Var "y") ;;

let exp_to_abstract_string_test () = 
    unit_test (exp_to_abstract_string e1 = "Var(x)")
    "exp_to_abstract_string e1" ;
    unit_test (exp_to_abstract_string e2 = "Num(7)")
    "exp_to_abstract_string e2" ;
    unit_test (exp_to_abstract_string e3 = "Bool(true)")
    "exp_to_abstract_string e3" ;
    unit_test (exp_to_abstract_string e4 = "Unop(Negate, Var(x))")
    "exp_to_abstract_string e4" ;
    unit_test (exp_to_abstract_string e5 = "Binop(Power, Var(x), Num(4))")
    "exp_to_abstract_string e5" ;
    unit_test (exp_to_abstract_string e6 = 
    "Conditional(Binop(Equals, Var(x), Num(0)), Num(1), Binop(Times, Var(x), App(Var(f), Binop(Minus, Var(x), Num(1)))))")
    "exp_to_abstract_string e6" ;
    unit_test (exp_to_abstract_string e7 = "Fun(x, Binop(Plus, Var(x), Var(y)))")
    "exp_to_abstract_string e7" ;
    unit_test (exp_to_abstract_string e8 = 
    "Let(f, Fun(x, Var(x)), App(App(Var(f), Var(f)), Num(3)))")
    "exp_to_abstract_string e8" ;
    unit_test (exp_to_abstract_string e9 = 
    "Letrec(f, Fun(x, Num(5)), App(Bool(false), Num(3)))")
    "exp_to_abstract_string e9" ;
    unit_test (exp_to_abstract_string e10 = "raise")
    "exp_to_abstract_string e10" ;
    unit_test (exp_to_abstract_string e11 = "unassigned")
    "exp_to_abstract_string e11" ;
    unit_test (exp_to_abstract_string e12 = "App(Unop(FloatNegate, Float(3.)), Var(y))")
    "exp_to_abstract_string e12" ;;

let free_vars_test () =
    unit_test (same_vars (free_vars e1) (vars_of_list ["x"])) 
    "free_vars e1";
    unit_test (same_vars (free_vars e2) (vars_of_list [])) 
    "free_vars e2";
    unit_test (same_vars (free_vars e3) (vars_of_list [])) 
    "free_vars e3";
    unit_test (same_vars (free_vars e4) (vars_of_list ["x"])) 
    "free_vars e4";
    unit_test (same_vars (free_vars e5) (vars_of_list ["x"])) 
    "free_vars e5";
    unit_test (same_vars (free_vars e6) (vars_of_list ["f"; "x"])) 
    "free_vars e6";
    unit_test (same_vars (free_vars e7) (vars_of_list ["y"])) 
    "free_vars e7";
    unit_test (same_vars (free_vars e8) (vars_of_list [])) 
    "free_vars e8";
    unit_test (same_vars (free_vars e9) (vars_of_list [])) 
    "free_vars e9";
    unit_test (same_vars (free_vars e10) (vars_of_list [])) 
    "free_vars e10";
    unit_test (same_vars (free_vars e11) (vars_of_list [])) 
    "free_vars e11";
    unit_test (same_vars (free_vars e12) (vars_of_list ["y"])) 
    "free_vars e12";;

let subst_test () = 
    unit_test (subst "x" (Num 1) e1 = Num 1) 
    "subst e1";
    unit_test (subst "x" (Num 1) e2 = e2) 
    "subst e2";
    unit_test (subst "x" (Num 1) e3 = e3) 
    "subst e3";
    unit_test (subst "x" (Var "y") e4 = Unop (Negate, Var "y")) 
    "subst e4";
    unit_test (subst "x" (Num 1) e5 = Binop (Power, Num 1, Num 4)) 
    "subst e5";
    unit_test (subst "y" (Num 1) e6 = Conditional (Binop (Equals, Var "x", 
    Num 0), Num 1, Binop (Times, Var "x", App (Var "f", Binop (Minus, Var "x", 
    Num 1))))) "subst e6";
    unit_test (subst "x" (Var "y") e7 = 
    Fun ("x", Binop (Plus, Var "x", Var "y"))) "subst e7";
    unit_test (subst "x" (Num 1) e8 = Let ("f", Fun ("x", Var "x"), 
    App (App (Var "f", Var "f"), Num 3))) "subst e8";
    unit_test (subst "y" (Num 7) e9 = Letrec ("f", Fun ("x", Num 5), 
    App (Bool false, Num 3))) "subst e9";
    unit_test (subst "x" (Num 7) e10 = e10) 
    "subst e10";
    unit_test (subst "x" (Num 7) e11 = e11) 
    "subst e11";
    unit_test (subst "y" (Num 1) e12 = App (Unop (FloatNegate, Float 3.), Num 1)) 
    "subst e12";;

let test_all () =
    exp_to_abstract_string_test () ;
    free_vars_test () ;
    subst_test () ;;
    
let _ = test_all () ;;