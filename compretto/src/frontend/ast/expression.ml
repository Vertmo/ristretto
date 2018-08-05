(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open Types

type expr = Int of int (** Primitive integer *)
          | Float of float (** Primitive floating number *)
          | String of string (** Primitive string *)
          | Bool of bool (** Primitive boolean *)
          | VarCall of string (** Getting value of a var *)

(** Printing *)
let print_expression expr = match expr with
  | Int i -> print_int i
  | Float f -> print_float f
  | String s -> Printf.printf "\"%s\"" s
  | Bool b -> if b then print_string "true" else print_string "false"
  | VarCall ident -> print_string ident

(** Type checking *)
let check_type expr env = match expr with
  | Int _ -> Types.Int
  | Float _ -> Types.Float
  | String _ -> Types.String
  | Bool _ -> Types.Bool
  | VarCall ident -> snd (List.find (fun (i,t) -> i = ident) env)

(** Printing with types *)
let print_expression_with_type expr env = print_expression expr; Printf.printf ": %s" (Types.to_string (check_type expr env))
