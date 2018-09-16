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

open Printf
open Ast
open Types
open Primitives

exception NotTheSameTypeError of string
exception UnexpectedTypeError of string

let find_from_table cpTable toFind =
  (snd (List.find (fun (s, _) -> s = toFind) cpTable))

(** Type checking of expression *)
let rec check_expr_types expr env = match expr with
  | Ast.Int _ -> Int
  | Ast.Long _ -> Long
  | Ast.Float _ -> Float
  | Ast.Double _ -> Double
  | Ast.String _ -> String
  | Ast.Bool _ -> Bool
  | Ast.Unit -> Unit
  | EVar ident -> snd (List.find (fun (i,t) -> i = ident) env)
  | UnOp (p, e) -> let t = check_expr_types e env in
    if not (List.mem t (un_input_types p)) then raise (UnexpectedTypeError ((string_of_type t)^" cannot be used here"))
    else un_output_type p t
  | BinOp (p, e1, e2) -> let t1 = check_expr_types e1 env and t2 = check_expr_types e2 env in
    if t1 <> t2 then raise (NotTheSameTypeError ("Types "^(string_of_type t1)^" and "^(string_of_type t2)^" are not the same"))
    else if not (List.mem t1 (bin_input_types p)) then raise (UnexpectedTypeError ((string_of_type t1)^" cannot be used here"))
    else bin_output_type p t1 t2
  | If (cond, th, el) ->
    if (check_expr_types cond env) != Bool then raise (UnexpectedTypeError "Expression in if must be of type Bool")
    else (
      let t1 = fst (check_program_types th env) and t2 = fst (check_program_types el env) in
      if t1 <> t2 then raise (NotTheSameTypeError ("Types "^(string_of_type t1)^" and "^(string_of_type t2)^" are not the same"));
      t1
    )
  | Funcall (ident, exprs) -> try
      let t = snd
          (List.find
             (fun (s,t) ->
                s = ident && (match t with
                    | Fun (inputT, _) when inputT = (List.map (fun e -> check_expr_types e env) exprs) -> true
                    | _ -> false))
             env) in match t with
      | Fun (inputT, r) -> r
      | t -> raise (UnexpectedTypeError ("Type of "^ident^" should be fun [?] -> ? not "^(string_of_type t)))
    with Not_found -> raise (UnexpectedTypeError ("No declared function with name "^ident^" and input type ["^
                                                  (String.concat ","
                                                     (List.map (fun e -> string_of_type (check_expr_types e env)) exprs))^"]"))

(** Type checking of statement *)
and check_stmt_types stmt env = match stmt with
  | VoidExpr e -> ignore (check_expr_types e env); env
  | Let (s, e) -> (s, check_expr_types e env)::env
  | Return e -> ignore (check_expr_types e env); env
  | Print e -> (match (check_expr_types e env) with
    | Int | Long | Float | Double | String | Bool -> env
    | t -> raise (UnexpectedTypeError ("type "^(string_of_type t)^" cannot be printed")))
  | Function (ident, args, srt, body) ->
    let tArgs = List.map (fun (s, ts) ->
        (s, (* Name of the argument *)
         try let t = (type_of_string ts) in t (* Type of the argument *)
         with Invalid_argument _ -> raise (UnexpectedTypeError (ts^" is not a type")))) args in
    let rt = try (type_of_string srt) (* Check the given return type *)
      with Invalid_argument _ -> raise (UnexpectedTypeError (srt^" is not a type")) in
    let funType = Fun (List.map snd tArgs, rt) in
    let crt = fst (check_program_types body ((ident, funType)::(tArgs@env))) in (* Check that the args are used correctly inside the body *)
    if crt <> rt then raise (UnexpectedTypeError (sprintf "Return type of function %s should be %s not %s" ident (string_of_type rt) (string_of_type crt)));
    (ident, funType)::env
  | ForeignFun (ident, argsT, retT, _) ->
    let tArgs = List.map (fun s -> try type_of_string s
                           with Invalid_argument _ -> raise (UnexpectedTypeError (s^" is not a type"))) argsT in
    let rt = try type_of_string retT
      with Invalid_argument _ -> raise (UnexpectedTypeError (retT^" is not a type")) in
    (ident, Fun (tArgs, rt))::env

and check_program_types ast env =
  let env = List.fold_left (fun env stmt -> check_stmt_types stmt env) env ast in
  match List.nth ast (List.length ast - 1) with
  | Return e -> (check_expr_types e env, env)
  | _ -> raise (Failure "Unexpected statement at the end of the program, should be Return")

let check_types ast = ignore (check_program_types ast [])

(** Printing an expression with types *)
let print_expression_with_type expr env = print_expression expr; Printf.printf ": %s" (Types.string_of_type (check_expr_types expr env))

(** Print args of a function with types *)
let print_args_with_types a t =
  let rec p_a_w_t_aux a t = match (a, t) with
    | ([], []) -> ()
    | ([a], [t]) -> printf "%s:%s" a (string_of_type t)
    | (h1::t1, h2::t2) -> printf "%s:%s, " h1 (string_of_type h2); p_a_w_t_aux t1 t2
    | _ -> invalid_arg "print_args_with_types"
  in print_string "("; p_a_w_t_aux a t; print_string ")"

(** Printing statement with types *)
let rec print_statement_with_types stmt env = match stmt with
  | VoidExpr e -> print_expression_with_type e env
  | Let (s, e) -> printf "let %s = " s; print_expression_with_type e env
  | Return e -> print_string "return "; print_expression_with_type e env
  | Print e -> print_string "print "; print_expression_with_type e env
  | Function (ident, args, _, b) ->
      let t = find_from_table env ident in
      let args = fst (List.split args) in
      (match t with
       | Fun (i, r) -> (printf "fun %s" ident; print_args_with_types args i; printf " -> %s {\n" (string_of_type r);
                        print_program_with_types b ((List.combine args i)@env); print_string "}")
       | _ -> raise (UnexpectedTypeError ("Type of "^ident^" should be fun [?] -> ? not "^(string_of_type t))))
  | ForeignFun _ -> print_statement stmt

and print_program_with_types ast env =
  ignore (List.fold_left (fun env stmt -> let env = check_stmt_types stmt env in
                           print_statement_with_types stmt env;
                           print_endline ";";
                           env) env ast)
