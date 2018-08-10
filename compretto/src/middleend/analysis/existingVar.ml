(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Check that a var has been declared before using *)

open Ast

exception UnboundVarError of string

let rec check_exist_expr expr env = match expr with
  | Int _ | Float _ | String _ | Bool _ -> ()
  | EVar ident -> if not (List.exists (fun i -> ident = i) env) then raise (UnboundVarError ident)
  | UnOp (_, e) -> check_exist_expr e env
  | BinOp (_, e1, e2) -> check_exist_expr e1 env
  | If (e, th, el) -> check_exist_expr e env; check_exist_program th env; check_exist_program el env

and check_exist_stmt stmt env = match stmt with
  | VoidExpr e -> check_exist_expr e env; env
  | Let (s, e) -> check_exist_expr e env; s::env
  | Return e -> check_exist_expr e env; env

and check_exist_program ast env = ignore (List.fold_left (fun env stmt -> check_exist_stmt stmt env) env ast)
