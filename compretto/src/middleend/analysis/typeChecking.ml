(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open Statement
open Expression
open Types
open Primitives

(** Type checking analysis *)

exception NotTheSameTypeError of string
exception UnexpectedTypeError of string

(** Type checking of expression *)
let rec check_type expr env = match expr with
  | Expression.Int _ -> Int
  | Expression.Float _ -> Float
  | Expression.String _ -> String
  | Expression.Bool _ -> Bool
  | VarCall ident -> snd (List.find (fun (i,t) -> i = ident) env)
  | UnOp (p, e) -> let t = check_type e env in
    if not (List.mem t (unInputTypes p)) then raise (UnexpectedTypeError ((to_string t)^" cannot be used here"))
    else (match (unOutputType p) with | Same -> t | Different t -> t)
  | BinOp (p, e1, e2) -> let t1 = check_type e1 env and t2 = check_type e2 env in
    if t1 <> t2 then raise (NotTheSameTypeError ("Types "^(to_string t1)^" and "^(to_string t2)^" are not the same"))
    else if not (List.mem t1 (binInputTypes p)) then raise (UnexpectedTypeError ((to_string t1)^" cannot be used here"))
    else (match (binOutputType p) with | Same -> t1 | Different t -> t)

(** Printing an expression with types *)
let print_expression_with_type expr env = Expression.print_expression expr; Printf.printf ": %s" (Types.to_string (check_type expr env))

(** Type checking of statement *)
let rec check_types stmt env = match stmt with
  | VoidExpr e -> ignore (check_type e env); env
  | Let (s, e) -> (s, check_type e env)::env

and check_program_types ast = List.fold_left (fun env stmt -> check_types stmt env) [] ast

(** Printing statement with types *)
let rec print_statement_with_types stmt env = match stmt with
  | VoidExpr e -> print_expression_with_type e env
  | Let (s, e) -> Printf.printf "let %s = " s; print_expression_with_type e env

and print_program_with_types ast =
  ignore (List.fold_left (fun env stmt -> let env = check_types stmt env in
                           print_statement_with_types stmt env;
                           print_endline ";";
                           env) [] ast)
