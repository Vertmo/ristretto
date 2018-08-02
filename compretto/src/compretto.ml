(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(* Main module, containing the main function *)

let lexAndParse ic =
  () (* TODO *)

let _ =
  if Array.length Sys.argv <> 2
  then print_endline "Usage : compretto <filename>"
  else (
    let filename = Sys.argv.(1) in
    let ic = open_in filename in
    let _ast = lexAndParse ic in
    () (* TODO *)
  )
