(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open Primitives

type expr = Int of int (** Primitive integer *)
          | Float of float (** Primitive floating number *)
          | String of string (** Primitive string *)
          | Bool of bool (** Primitive boolean *)
          | VarCall of string (** Getting value of a var *)
          | UnOp of unaryOp * expr (** Unary operator *)
          | BinOp of binaryOp * expr * expr (** Binary operator *)

(** Printing *)
let rec print_expression expr = match expr with
  | Int i -> print_int i
  | Float f -> print_float f
  | String s -> Printf.printf "\"%s\"" s
  | Bool b -> if b then print_string "true" else print_string "false"
  | VarCall ident -> print_string ident
  | UnOp (p, e) -> print_string "("; print_string (unSymbol p); print_expression e; print_string ")"
  | BinOp (p, e1, e2) -> print_string "("; print_expression e1; Printf.printf " %s " (binSymbol p); print_expression e2; print_string ")"

