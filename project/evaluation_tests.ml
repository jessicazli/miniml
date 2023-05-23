(*
                         CS 51 Final Project
                         Evaluation Testing
 *)

open CS51Utils ;;
open Absbook ;;
open Expr ;;
open Evaluation ;;

let e1 = Var "x" ;;
let e2 = Num 3 ;;
let e3 = Float 2. ;;
let e4 = String "hello" ;;
let e5 = Unit ;;
let e6 = Unop (Sin, Float 0.)
let e7 = Unop (Negate, Var "x") ;;
let e8 = Binop (Power, Num 2, Num 4) ;;
let e9 = Conditional (Bool false, Num 1, Float 5.);; 
let e10 = Fun ("x", Binop (Plus, Var "x", Var "y")) ;;
let e11 = Let ("f", Fun ("x", Var "x"), App (App (Var "f", Var "f"), Num 3)) ;;
let e12 = Letrec ("f", Fun ("x", Num 5), App (Bool false, Num 3)) ;;
let e13 = Raise ;;
let e14 = Unassigned ;;
let e15 = App (Unop (FloatNegate, Float 3.), Var "y") ;;

let diff1 = Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), Var("y"))), 
            Let("x", Num(2), App(Var("f"), Num(3))))) ;;
let diff2 = Let("x", Float(1.), Let("f", Fun("y", Fun("z", Binop(FloatPlus, 
            Var("z"), Binop(FloatDivide, Var("x"), Var("y"))))), Let("y", 
            Float(2.), App(App(Var("f"), Float(1.)), Float(5.))))) ;;


let empty_env = Env.empty () ;;
let example_env = Env.extend empty_env "x" (ref (Env.Val (Num 1))) ;;

let env_module_test () =
  unit_test (Env.close e1 example_env = Closure (e1, example_env))
  "env_module close" ;
  unit_test (Env.lookup example_env "x" = Env.Val (Num 1))
  "env_module lookup" ;
  unit_test (Env.env_to_string example_env = "{x -> 1}")
  "env_module env_to_string" ;;

let eval_test (eval : expr -> Env.env -> Env.value) = 
  unit_test (eval e2 empty_env = Env.Val (Num (3))) 
  "eval num" ;
  unit_test (eval e3 empty_env = Env.Val (Float (2.))) 
  "eval float" ;
  unit_test (eval e4 empty_env = Env.Val (String ("hello"))) 
  "eval string" ;
  unit_test (eval e5 empty_env = Env.Val Unit)
  "eval unit" ;
  unit_test (eval e6 empty_env = Env.Val (Float (0.)))
  "eval unop sin" ;
  unit_test (eval e8 empty_env = Env.Val (Num (16)))
  "eval binop power" ;
  unit_test (eval e9 empty_env = Env.Val (Float 5.))
  "eval conditional" ;;

let eval_s_test () =
  eval_test eval_s ;
  unit_test (eval_s diff1 empty_env = Env.Val (Num (4))) "eval_s diff1" ;
  unit_test (eval_s diff2 empty_env = Env.Val (Float (6.))) "eval_s diff2" ;;

let eval_d_test () =
  eval_test eval_d ;
  unit_test (eval_d diff1 empty_env = Env.Val (Num (5))) "eval_d diff1";
  unit_test (eval_d diff2 empty_env = Env.Val (Float (5.5))) "eval_d diff2" ;;

let eval_l_test () =
  eval_test eval_l ;
  unit_test (eval_l diff1 empty_env = Env.Val (Num (4))) "eval_l diff1" ;
  unit_test (eval_l diff2 empty_env = Env.Val (Float (6.))) "eval_l diff2" ;;

let test_all () =
  env_module_test () ;
  eval_s_test () ;;
  eval_d_test () ;
  eval_l_test () ;;
  
let _ = test_all () ;;