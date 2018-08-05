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
  | ";" { SEMICOL }
  | "//"[^ '\n']* { token lexbuf } (* Single line comments *)
  | "/*" { comment lexbuf } (* Start of a multi-line comment *)
  | eof { EOF }

  | "-"?['0'-'9']+ as num { INT(int_of_string num) }
  | "-"?['0'-'9']+"."['0'-'9']* as num { FLOAT(float_of_string num)}
  | "\""[^'"' '\n']*"\"" as str { STRING(String.sub str 1 ((String.length str)-2)) }

  | "let" { LET }
  | "=" { EQUAL }
  | ['a'-'z''A'-'Z']+ as ident { IDENT(ident) }

and comment = parse
  | "*/" { token lexbuf } (* End of a multi-line comment *)
  | "\n" { Lexing.new_line lexbuf; comment lexbuf }
  | _ { comment lexbuf }
