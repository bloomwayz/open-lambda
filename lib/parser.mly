%{
open Syntax
open Expr

%}

%token TRUE FALSE
%token <int> INT
%token <string> ID
%token FUN LET REC IN
%token IF THEN ELSE
%token EQ
%token PLUS MINUS AST
%token LPAREN RPAREN
%token RARROW
%token QUOTE COMMA RUN
%token EOF

%nonassoc IN
%nonassoc ELSE
%nonassoc RARROW
%left     EQ
%left     PLUS MINUS
%left     AST
%nonassoc COMMA
%nonassoc QUOTE
%nonassoc RUN

%start <Expr.t> prog
%type <Expr.t> expr
%%

prog:
    | expr; EOF { $1 }
expr:
    | apply { $1 }
    | FUN; param = ID; RARROW; body = expr                   { Fn (param, body) }
    | LET; x = ID; EQ; e1 = expr; IN; e2 = expr              { Let (x, e1, e2) }
    | LET; REC; f = ID; x = ID; EQ; e1 = expr; IN; e2 = expr { Rec (f, x, e1, e2) }
    | IF; pred = expr; THEN; con = expr; ELSE; alt = expr    { If (pred, con, alt) }
    | left = expr; op = bop; right = expr                    { Bop (op, left, right) }
    | QUOTE; e = expr                                        { Box e }
    | COMMA; e = expr                                        { Unbox e }
    | RUN; e = expr                                          { Eval e }
%inline bop:
    | EQ    { Eq }
    | PLUS  { Plus }
    | MINUS { Minus }
    | AST   { Times }
apply:
    | atom                { $1 }
    | f = apply; x = atom { App (f, x) }
atom:
    | TRUE                     { Const (Bool true) }
    | FALSE                    { Const (Bool false) }
    | n = INT                  { Const (Int n) }
    | x = ID                   { Var x }
    | LPAREN; e = expr; RPAREN { e }