(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Expand the ast to a kast *)

open Primitives
open Ast
open TypeChecking
open Kast

(** Expand to a k-expression *)
let rec expand_expr expr env = match expr with
  | Int i -> KInt i | Float f -> KFloat f | Bool b -> KBool b | String s -> KString s
  | EVar ident -> KEVar (ident, (snd (List.find (fun (s, t) -> s = ident) env)))
  | UnOp (p, e) -> KCall ((un_symbol p), [expand_expr e env], (check_expr_types expr env))
  | BinOp (p, e1, e2) -> KCall ((bin_symbol p), [expand_expr e1 env; expand_expr e2 env], (check_expr_types expr env))
  | If (cond, th, el) -> KIf ((expand_expr cond env), (expand_program th env), (expand_program el env), (check_expr_types expr env))

(** Expand to a k-statement *)
and expand_stmt stmt env = match stmt with
  | VoidExpr e -> KVoidExpr (expand_expr e env)
  | Let (s, e) -> KLet (s, expand_expr e env)
  | Return e -> KReturn (expand_expr e env)
  | Print e -> KPrint (expand_expr e env)

(** Expand a program *)
and expand_program ast env = List.map (fun s -> expand_stmt s (snd (check_program_types ast env))) ast
