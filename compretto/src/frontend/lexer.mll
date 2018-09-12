(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

{
open Parser
exception Eof
}

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | "\n" { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }

  | "//"[^ '\n']* { token lexbuf } (* Single line comments *)
  | "/*" { comment lexbuf } (* Start of a multi-line comment *)

  | ";" { SEMICOL } | "," { COMMA }
  | "(" { LPAREN } | ")" { RPAREN }
  | "{" { LCURLY } | "}" { RCURLY }
  | "->" { RARROW }

  | ['0'-'9']+ as num { INT(int_of_string num) }
  | ['0'-'9']+'l' as num { LONG(Int64.of_string (String.sub num 0 ((String.length num)-1))) }
  | ['0'-'9']+"."['0'-'9']*'f' as num { FLOAT(float_of_string (String.sub num 0 ((String.length num)-1))) }
  | ['0'-'9']+"."['0'-'9']* as num { DOUBLE(float_of_string num) }
  | "\""[^'"' '\n']*"\"" as str { STRING(String.sub str 1 ((String.length str)-2)) }
  | "true" { BOOL(true) } | "false" { BOOL(false) }

  | "+" { ADD } | "-" { SUB } | "*" { MULT } | "/" { DIV }
  | "==" { EQEQ } | "!=" { NEQ } | "<" { LESS } | "<=" { LESSEQ } | ">" { GREATER } | ">=" { GREATEREQ }
  | "&&" { AND } | "||" { OR } | "not" { NOT }

  | "let" { LET } | "=" { EQUAL } | "fun" { FUN }
  | "if" { IF } | "then" { THEN } | "else" { ELSE }
  | "print" { PRINT }
  | ['a'-'z''A'-'Z']+ as ident { IDENT(ident) }

and comment = parse
  | "*/" { token lexbuf } (* End of a multi-line comment *)
  | "\n" { Lexing.new_line lexbuf; comment lexbuf }
  | _ { comment lexbuf }
