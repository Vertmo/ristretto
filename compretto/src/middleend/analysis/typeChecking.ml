(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Type checking analysis *)

open Ast
open Types
open Primitives

exception NotTheSameTypeError of string
exception UnexpectedTypeError of string

(** Type checking of expression *)
let rec check_expr_types expr env = match expr with
  | Ast.Int _ -> Int
  | Ast.Float _ -> Float
  | Ast.String _ -> String
  | Ast.Bool _ -> Bool
  | EVar ident -> snd (List.find (fun (i,t) -> i = ident) env)
  | UnOp (p, e) -> let t = check_expr_types e env in
    if not (List.mem t (un_input_types p)) then raise (UnexpectedTypeError ((to_string t)^" cannot be used here"))
    else un_output_type p t
  | BinOp (p, e1, e2) -> let t1 = check_expr_types e1 env and t2 = check_expr_types e2 env in
    if t1 <> t2 then raise (NotTheSameTypeError ("Types "^(to_string t1)^" and "^(to_string t2)^" are not the same"))
    else if not (List.mem t1 (bin_input_types p)) then raise (UnexpectedTypeError ((to_string t1)^" cannot be used here"))
    else bin_output_type p t1 t2
  | If (cond, th, el) ->
    if (check_expr_types cond env) != Bool then raise (UnexpectedTypeError "Expression in if must be of type Bool")
    else (
      let t1 = fst (check_program_types th env) and t2 = fst (check_program_types el env) in
      if t1 <> t2 then raise (NotTheSameTypeError ("Types "^(to_string t1)^" and "^(to_string t2)^" are not the same")) else t1
    )

(** Type checking of statement *)
and check_stmt_types stmt env = match stmt with
  | VoidExpr e -> ignore (check_expr_types e env); env
  | Let (s, e) -> (s, check_expr_types e env)::env
  | Return e -> ignore (check_expr_types e env); env

and check_program_types ast env =
  let env = List.fold_left (fun env stmt -> check_stmt_types stmt env) env ast in
  match List.nth ast (List.length ast - 1) with
  | Return e -> (check_expr_types e env, env)
  | _ -> raise (Failure "Unexpected statement")

let check_types ast = ignore (check_program_types ast [])

(** Printing an expression with types *)
let print_expression_with_type expr env = print_expression expr; Printf.printf ": %s" (Types.to_string (check_expr_types expr env))

(** Printing statement with types *)
let rec print_statement_with_types stmt env = match stmt with
  | VoidExpr e -> print_expression_with_type e env
  | Let (s, e) -> Printf.printf "let %s = " s; print_expression_with_type e env
  | Return e -> print_string "return "; print_expression_with_type e env

and print_program_with_types ast =
  ignore (List.fold_left (fun env stmt -> let env = check_stmt_types stmt env in
                           print_statement_with_types stmt env;
                           print_endline ";";
                           env) [] ast)
