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
  open Statement;;
  open Expression;;
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <string> IDENT
%token LET
%token EQUAL
%token EOF SEMICOL
%start program
%type <Statement.stmt> statement
%type <Statement.stmt list> statements
%type <Statement.program> program
%type <Expression.expr> expr
%%
program:
  statements EOF { $1 }
;

statements:
    statement SEMICOL statements { $1::$3 }
  | expr { [Statement.VoidExpr $1] } // Last, returned expression
;

statement:
  | expr { Statement.VoidExpr $1 }
  | LET IDENT EQUAL expr { Statement.Let ($2, $4) }
;

expr:
  | INT { Expression.Int $1 }
  | FLOAT { Expression.Float $1 }
  | STRING { Expression.String $1 }
  | IDENT { Expression.VarCall $1 }
;
