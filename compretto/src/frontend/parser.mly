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

%token EOF SEMICOL
%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <bool> BOOL
%token <string> IDENT
%token LET EQUAL
%token LPAREN RPAREN

%token ADD SUB MULT DIV
%token EQEQ NEQ LESS LESSEQ GREATER GREATEREQ
%token AND OR NOT

%left AND OR
%left EQEQ NEQ LESS LESSEQ GREATER GREATEREQ
%left ADD SUB
%left MULT DIV
%right NOT

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
  | BOOL { Expression.Bool $1 }
  | IDENT { Expression.EVar $1 }

  | LPAREN expr RPAREN { $2 }

  | expr ADD expr { Expression.BinOp (Add, $1, $3) }
  | expr SUB expr { Expression.BinOp (Sub, $1, $3) }
  | expr MULT expr { Expression.BinOp (Mult, $1, $3) }
  | expr DIV expr { Expression.BinOp (Div, $1, $3) }
  | SUB expr { Expression.UnOp (Neg, $2) }

  | expr EQEQ expr { Expression.BinOp (Eqeq, $1, $3) }
  | expr NEQ expr { Expression.BinOp (Neq, $1, $3) }
  | expr LESS expr { Expression.BinOp (Less, $1, $3) }
  | expr LESSEQ expr { Expression.BinOp (LessEq, $1, $3) }
  | expr GREATER expr { Expression.BinOp (Greater, $1, $3) }
  | expr GREATEREQ expr { Expression.BinOp (GreaterEq, $1, $3) }

  | expr AND expr { Expression.BinOp (And, $1, $3) }
  | expr OR expr { Expression.BinOp (Or, $1, $3) }
  | NOT expr { Expression.UnOp (Not, $2) }
;
