(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open Exceptions

(* Main module, containing the main function *)

(** Lexes and parses the input file, and returns the AST **)
let lexAndParse ic =
  let lexbuf = Lexing.from_channel ic in
  try
    Parser.program Lexer.token lexbuf
  with Failure _ -> lex_error lexbuf
     | Parsing.Parse_error -> parse_error lexbuf

let main filename =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let ast = lexAndParse ic in
  Statement.print_program ast (* TODO *)

let _ =
  if Array.length Sys.argv <> 2
  then print_endline "Usage : compretto <filename>"
  else main Sys.argv.(1)
