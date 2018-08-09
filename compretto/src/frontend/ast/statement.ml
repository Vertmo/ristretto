(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open Printf
open Expression

type stmt = VoidExpr of expr (** Expression in the wind *)
          | Let of string * expr (** Declaring a var and assigning it *)
          | Return of expr (** Return a value at the end of a function / toplevel *)

type program = stmt list

(** Printing *)
let rec print_statement stmt = match stmt with
  | VoidExpr e -> print_expression e
  | Let (s, e) -> printf "let %s = " s; print_expression e
  | Return e -> print_string "return "; print_expression e

and print_program ast = List.iter (fun s -> print_statement s; print_endline ";") ast
