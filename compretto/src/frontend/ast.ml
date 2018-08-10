(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** AST module *)

open Printf
open Primitives

(** Expression *)
type expr = Int of int (** Primitive integer *)
          | Float of float (** Primitive floating number *)
          | String of string (** Primitive string *)
          | Bool of bool (** Primitive boolean *)
          | EVar of string (** Getting value of a var *)
          | UnOp of unaryOp * expr (** Unary operator *)
          | BinOp of binaryOp * expr * expr (** Binary operator *)
          | If of expr * program * program (** if then else *)

(** Statement *)
and stmt = VoidExpr of expr (** Expression in the wind *)
          | Let of string * expr (** Declaring a var and assigning it *)
          | Return of expr (** Return a value at the end of a function / toplevel *)

(** Program (statement list) *)
and program = stmt list

(** Print an expression *)
let rec print_expression expr = match expr with
  | Int i -> print_int i
  | Float f -> print_float f
  | String s -> Printf.printf "\"%s\"" s
  | Bool b -> if b then print_string "true" else print_string "false"
  | EVar ident -> print_string ident
  | UnOp (p, e) -> print_string "("; print_string (un_symbol p); print_expression e; print_string ")"
  | BinOp (p, e1, e2) -> print_string "("; print_expression e1; Printf.printf " %s " (bin_symbol p); print_expression e2; print_string ")"
  | If (cond, th, el) -> print_string "if ("; print_expression cond; print_string ") then {\n";
    print_program th; print_string "} else {\n"; print_program el; print_string "}"

(** Print a statement *)
and print_statement stmt = match stmt with
  | VoidExpr e -> print_expression e
  | Let (s, e) -> printf "let %s = " s; print_expression e
  | Return e -> print_string "return "; print_expression e

(** Print a whole program *)
and print_program ast = List.iter (fun s -> print_statement s; print_endline ";") ast
