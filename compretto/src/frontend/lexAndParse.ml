(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)


open Lexing

exception LexError of string
exception ParseError of string

let error msg start finish  =
    Printf.sprintf "(line %d: char %d..%d): %s" start.pos_lnum
          (start.pos_cnum -start.pos_bol) (finish.pos_cnum - finish.pos_bol) msg

let lex_error lexbuf = raise (LexError (error (lexeme lexbuf) (lexeme_start_p lexbuf) (lexeme_start_p lexbuf)))

let parse_error lexbuf = raise (ParseError (error "Syntax error" (lexeme_start_p lexbuf) (lexeme_start_p lexbuf)))


(** Lexes and parses the input file, and returns the AST **)
let lexAndParse ic =
  let lexbuf = Lexing.from_channel ic in
  try
    Parser.program Lexer.token lexbuf
  with Failure _ -> lex_error lexbuf
     | Parsing.Parse_error -> parse_error lexbuf
