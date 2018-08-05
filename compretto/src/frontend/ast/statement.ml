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

type program = stmt list

let rec print_statement stmt = match stmt with
  | VoidExpr e -> print_expression e
  | Let (s, e) -> printf "let %s = " s; print_expression e

and print_program ast = match List.rev ast with
  | [] -> ()
  | h::q -> List.iter (fun s -> print_statement s; print_endline ";") (List.rev q);
    print_statement h; print_newline ()
