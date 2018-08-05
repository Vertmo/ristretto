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

(** Main module, containing the main function *)

let main filename =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let ast = LexAndParse.lexAndParse ic in
  Statement.print_program_with_types ast (* TODO *)

let _ =
  if Array.length Sys.argv <> 2
  then print_endline "Usage : compretto <filename>"
  else main Sys.argv.(1)
