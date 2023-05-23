(*
                         CS 51 Final Project
                           MiniML -- Parser
*)
                  
%{
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEGATE FLOATNEGATE
%token SIN COS TAN
%token PLUS FLOATPLUS MINUS FLOATMINUS
%token TIMES FLOATTIMES DIVIDE FLOATDIVIDE
%token POWER
%token LESSTHAN EQUALS GREATERTHAN
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token <float> FLOAT
%token <string> STRING
%token UNIT
%token TRUE FALSE

%nonassoc IF
%left LESSTHAN EQUALS GREATERTHAN
%left PLUS FLOATPLUS MINUS FLOATMINUS
%left TIMES FLOATTIMES DIVIDE FLOATDIVIDE
%right POWER
%nonassoc NEGATE FLOATNEGATE
%nonassoc SIN COS TAN

%start input
%type <Expr.expr> input

(* Grammar follows *)
%%
input:  exp EOF                 { $1 }

exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

expnoapp: INT                   { Num $1 }
        | FLOAT                 { Float $1 }
        | STRING                { String $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp FLOATPLUS exp     { Binop(FloatPlus, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp FLOATMINUS exp    { Binop(FloatMinus, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp FLOATTIMES exp    { Binop(FloatTimes, $1, $3) }
        | exp DIVIDE exp        { Binop(Divide, $1, $3) }
        | exp FLOATDIVIDE exp   { Binop(FloatDivide, $1, $3) }
        | exp POWER exp         { Binop(Power, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | exp GREATERTHAN exp   { Binop(GreaterThan, $1, $3) }
        | NEGATE exp            { Unop(Negate, $2) }
        | FLOATNEGATE exp       { Unop(FloatNegate, $2) }
        | SIN exp               { Unop(Sin, $2) }
        | COS exp               { Unop(Cos, $2) }
        | TAN exp               { Unop(Tan, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp   { Fun($2, $4) } 
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }
        | UNIT                  { Unit }
;

%%
