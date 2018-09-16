/******************************************************************************/
/*                                                                            */
/*                Compiler for the Ristretto programming language             */
/*                                                                            */
/*                                Basile Pesin                                */
/*                                                                            */
/* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  */
/*                    GNU General Public License v3.0                         */
/******************************************************************************/

%{
  open Ast;;
%}

%token EOF SEMICOL COMMA RARROW
%token <int> INT
%token <int64> LONG
%token <float> FLOAT
%token <float> DOUBLE
%token <string> STRING
%token <bool> BOOL
%token <string> IDENT
%token LPAREN RPAREN LCURLY RCURLY
%token LET EQUAL FUN
%token IF THEN ELSE
%token PRINT

%token ADD SUB MULT DIV
%token EQEQ NEQ LESS LESSEQ GREATER GREATEREQ
%token AND OR NOT

%left AND OR
%left EQEQ NEQ LESS LESSEQ GREATER GREATEREQ
%left ADD SUB
%left MULT DIV
%right NOT

%start program
%type <Ast.stmt> statement
%type <Ast.stmt list> statements
%type <Ast.program> program
%type <Ast.expr> expr
%type <(string * string) list> args
%type <Ast.expr list> exprs
%%
program:
  statements EOF { $1 }
;

statements:
    statement SEMICOL statements { $1::$3 }
  | expr { [Ast.Return $1] } // Last, returned expression
;

statement:
  | expr { Ast.VoidExpr $1 }
  | LET IDENT EQUAL expr { Ast.Let ($2, $4) }
  | PRINT expr { Print ($2) }
  | FUN IDENT LPAREN args RPAREN RARROW IDENT block { Ast.Function ($2, $4, $7, $8) }
  | FUN IDENT LPAREN RPAREN RARROW IDENT block { Ast.Function ($2, [], $6, $7) }
;

block:
  | LCURLY statements RCURLY { $2 }
;

expr:
  | INT { Ast.Int $1 }
  | LONG { Ast.Long $1 }
  | FLOAT { Ast.Float $1 }
  | DOUBLE { Ast.Double $1 }
  | STRING { Ast.String $1 }
  | BOOL { Ast.Bool $1 }
  | LPAREN RPAREN { Ast.Unit }
  | IDENT { Ast.EVar $1 }

  | LPAREN expr RPAREN { $2 }

  | expr ADD expr { Ast.BinOp (Add, $1, $3) }
  | expr SUB expr { Ast.BinOp (Sub, $1, $3) }
  | expr MULT expr { Ast.BinOp (Mult, $1, $3) }
  | expr DIV expr { Ast.BinOp (Div, $1, $3) }
  | SUB expr { Ast.UnOp (Neg, $2) }

  | expr EQEQ expr { Ast.BinOp (Eqeq, $1, $3) }
  | expr NEQ expr { Ast.BinOp (Neq, $1, $3) }
  | expr LESS expr { Ast.BinOp (Less, $1, $3) }
  | expr LESSEQ expr { Ast.BinOp (LessEq, $1, $3) }
  | expr GREATER expr { Ast.BinOp (Greater, $1, $3) }
  | expr GREATEREQ expr { Ast.BinOp (GreaterEq, $1, $3) }

  | expr AND expr { Ast.BinOp (And, $1, $3) }
  | expr OR expr { Ast.BinOp (Or, $1, $3) }
  | NOT expr { Ast.UnOp (Not, $2) }

  | IF expr THEN block ELSE block { If ($2, $4, $6) }

  | IDENT LPAREN exprs RPAREN { Ast.Funcall ($1, $3) }
  | IDENT LPAREN RPAREN { Ast.Funcall ($1, []) }
;

args:
    IDENT IDENT COMMA args { ($2, $1)::$4 }
  | IDENT IDENT { [($2, $1)] }
;

exprs:
  | expr COMMA exprs { $1::$3 }
  | expr { [$1] }
;
