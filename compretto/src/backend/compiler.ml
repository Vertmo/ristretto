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
let rec compile_expr kexpr cpPT cpFT cpCT = match kexpr with
  | KInt i -> [LDC (findFromTable cpCT kexpr)]
  | KFloat i -> [LDC (findFromTable cpCT kexpr)]
  | KBool b -> if b then [ICONST_1] else [ICONST_0]

(** Compile a k-statement *)
and compile_stmt kstmt cpPT cpFT cpCT = match kstmt with
  | KVoidExpr kexpr -> (compile_expr kexpr cpPT cpFT cpCT)@[POP]

and compile_program kast cpPT cpFT cpCT = (* TODO return *)
  List.fold_left (fun b kstmt -> b@(compile_stmt kstmt cpPT cpFT cpCT)) [] kast

(** Generate bytecode from the kast *)
let generate_bytecode kast cpPrimsTable cpFieldsTable cpConstsTable =
  compile_program kast cpPrimsTable cpFieldsTable cpConstsTable
