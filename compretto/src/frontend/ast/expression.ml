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
          | BinOp of Primitives.binOp * expr * expr (** Binary operator *)

(** Printing *)
let rec print_expression expr = match expr with
  | Int i -> print_int i
  | Float f -> print_float f
  | String s -> Printf.printf "\"%s\"" s
  | Bool b -> if b then print_string "true" else print_string "false"
  | VarCall ident -> print_string ident
  | BinOp (p, e1, e2) -> print_expression e1; Printf.printf " %s " p.symbol; print_expression e2

(** Type checking *)
let rec check_type expr env = match expr with
  | Int _ -> Types.Int
  | Float _ -> Types.Float
  | String _ -> Types.String
  | Bool _ -> Types.Bool
  | VarCall ident -> snd (List.find (fun (i,t) -> i = ident) env)
  | BinOp (p, e1, e2) -> let t1 = check_type e1 env and t2 = check_type e2 env in
    if t1 <> t2 then raise (NotTheSameTypeError ("Types "^(to_string t1)^" and "^(to_string t2)^" are not the same"))
    else if not (List.mem t1 p.inputTypes) then raise (UnexpectedTypeError ((to_string t1)^" cannot be used here"))
    else t1

(** Printing with types *)
let print_expression_with_type expr env = print_expression expr; Printf.printf ": %s" (Types.to_string (check_type expr env))
