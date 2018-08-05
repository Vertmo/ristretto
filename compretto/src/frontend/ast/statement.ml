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

(** Printing *)
let rec print_statement stmt = match stmt with
  | VoidExpr e -> print_expression e
  | Let (s, e) -> printf "let %s = " s; print_expression e

and print_program ast = List.iter (fun s -> print_statement s; print_endline ";") ast

(** Type checking *)
let rec check_types stmt env = match stmt with
  | VoidExpr e -> ignore (Expression.check_type e env); env
  | Let (s, e) -> (s, Expression.check_type e env)::env

and check_program_types ast = List.fold_left (fun env stmt -> check_types stmt env) [] ast

(** Printing with types *)
let rec print_statement_with_types stmt env = match stmt with
  | VoidExpr e -> print_expression_with_type e env
  | Let (s, e) -> printf "let %s = " s; print_expression_with_type e env

and print_program_with_types ast =
  ignore (List.fold_left (fun env stmt -> let env = check_types stmt env in
                           print_statement_with_types stmt env;
                           print_endline ";";
                           env) [] ast)
