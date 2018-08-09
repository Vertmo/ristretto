(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open Opcodes
open JavaPrims
open KStatement
open KExpression

let findFromTable cpTable toFind =
  (snd (List.find (fun (s, _) -> s = toFind) cpTable))

(** Compile a k-expression *)
let rec compile_expr kexpr cpPT cpFT cpCT env = match kexpr with
  | KInt i -> [LDC (findFromTable cpCT kexpr)]
  | KFloat i -> [LDC (findFromTable cpCT kexpr)]
  | KString s -> [LDC (findFromTable cpCT kexpr)]
  | KBool b -> if b then [ICONST_1] else [ICONST_0]
  | KEVar (v, t) -> (match t with
      | Int | Bool -> [ILOAD (findFromTable env v)]
      | Float -> [FLOAD (findFromTable env v)]
      | String -> [ALOAD (findFromTable env v)])

(** Compile a k-statement *)
and compile_stmt kstmt cpPT cpFT cpCT env = match kstmt with
  | KVoidExpr kexpr -> ((compile_expr kexpr cpPT cpFT cpCT env)@[POP], env)
  | KLet (s, kexpr) -> ((compile_expr kexpr cpPT cpFT cpCT env)@(match get_type kexpr with
      | Int | Bool -> [ISTORE (List.length env + 1)]
      | Float -> [FSTORE (List.length env + 1)]
      | String -> [ASTORE (List.length env + 1)]), (s, List.length env + 1)::env)

and compile_program kast cpPT cpFT cpCT env = (* TODO return *)
  List.fold_left (fun (b, env) kstmt -> let (newB, env) = (compile_stmt kstmt cpPT cpFT cpCT env) in (b@newB, env)) ([], env) kast

(** Generate bytecode from the kast *)
let generate_bytecode kast cpPrimsTable cpFieldsTable cpConstsTable =
  fst (compile_program kast cpPrimsTable cpFieldsTable cpConstsTable [])
