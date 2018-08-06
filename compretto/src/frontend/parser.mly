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

%token ADD SUB TIMES DIV
%token EQEQ NEQ LESS LESSEQ GREATER GREATEREQ
%token AND OR NOT

%left AND OR
%left EQEQ NEQ LESS LESSEQ GREATER GREATEREQ
%left ADD SUB
%left TIMES DIV
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
  | IDENT { Expression.VarCall $1 }

  | LPAREN expr RPAREN { $2 }

  | expr ADD expr { Expression.BinOp (Primitives.add, $1, $3) }
  | expr SUB expr { Expression.BinOp (Primitives.sub, $1, $3) }
  | expr TIMES expr { Expression.BinOp (Primitives.times, $1, $3) }
  | expr DIV expr { Expression.BinOp (Primitives.div, $1, $3) }
  | SUB expr { Expression.UnOp (Primitives.neg, $2) }

  | expr EQEQ expr { Expression.BinOp (Primitives.eqeq, $1, $3) }
  | expr NEQ expr { Expression.BinOp (Primitives.neq, $1, $3) }
  | expr LESS expr { Expression.BinOp (Primitives.less, $1, $3) }
  | expr LESSEQ expr { Expression.BinOp (Primitives.lessEq, $1, $3) }
  | expr GREATER expr { Expression.BinOp (Primitives.greater, $1, $3) }
  | expr GREATEREQ expr { Expression.BinOp (Primitives.greaterEq, $1, $3) }

  | expr AND expr { Expression.BinOp (Primitives.boolAnd, $1, $3) }
  | expr OR expr { Expression.BinOp (Primitives.boolOr, $1, $3) }
  | NOT expr { Expression.UnOp (Primitives.boolNot, $2) }
;
