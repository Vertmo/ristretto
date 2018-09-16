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
  | Int i -> KInt i | Long l -> KLong l | Float f -> KFloat f | Double d -> KDouble d | Bool b -> KBool b | String s -> KString s | Unit -> KUnit
  | EVar ident -> KEVar (ident, (snd (List.find (fun (s, t) -> s = ident) env)))
  | UnOp (p, e) -> KCall ((un_symbol p), [expand_expr e env], (check_expr_types expr env))
  | BinOp (p, e1, e2) -> KCall ((bin_symbol p), [expand_expr e1 env; expand_expr e2 env], (check_expr_types expr env))
  | If (cond, th, el) -> KIf ((expand_expr cond env), (expand_program th env), (expand_program el env), (check_expr_types expr env))
  | Funcall (ident, es) -> KCall (ident, List.map (fun e -> expand_expr e env) es, (check_expr_types expr env))

(** Expand to a k-statement *)
and expand_stmt stmt env = match stmt with
  | VoidExpr e -> KVoidExpr (expand_expr e env)
  | Let (s, e) -> KLet (s, expand_expr e env)
  | Return e -> KReturn (expand_expr e env)
  | Print e -> KPrint (expand_expr e env)
  | Function (ident, args, _, body) ->
    let t = find_from_table (check_stmt_types stmt env) ident in
    (match t with
     | Fun (it, rt) -> KFunction (ident, List.map fst args, (* Adding the free variables from the environment *)
                                  (List.fold_left (fun l (s, t) ->
                                       if List.exists (fun (s2, _) -> s2 = s) l || (* Unique names *)
                                          List.exists (fun (s2, _) -> s2 = s) args (* Name not in the args *)
                                       then l else (s, t)::l) []
                                      (List.filter (fun (_, t) -> match t with (* We don't put functions there *)
                                           | Types.Fun _ -> false
                                           | _ -> true) env)),
                                  expand_program body ((ident, t)::(List.combine (fst (List.split args)) it)@env),
                                  t)
     | _ -> invalid_arg "expand_stmt")
  | ForeignFun (ident, _, _, sfcn) ->
    let t = find_from_table (check_stmt_types stmt env) ident in
    (match t with
     | Fun _ -> let fcn = String.split_on_char '.' sfcn in (match fcn with
         | fcn::q -> (match List.rev q with
             | methodN::fieldP -> KForeign (ident, fcn, List.rev fieldP, methodN, t)
             | [] -> raise (Failure ("fcn "^sfcn^" is imcomplete")))
         | [] -> raise (Failure ("fcn "^sfcn^" is incomplete")))
     | _ -> invalid_arg "expand_stmt")

(** Expand a program *)
and expand_program ast env = (* let env = snd (check_program_types ast env) in List.map (fun s -> expand_stmt s env) ast *)
  fst (List.fold_left (fun (kast, env) stmt -> let env = check_stmt_types stmt env in
                        (kast@[(expand_stmt stmt env)], env)) ([], env) ast)
