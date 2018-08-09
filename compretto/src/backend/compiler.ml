(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Compile the kast *)

open Opcodes
open JavaPrims
open KStatement
open KExpression

let find_from_table cpTable toFind =
  (snd (List.find (fun (s, _) -> s = toFind) cpTable))

(** Compile a k-expression *)
let rec compile_expr kexpr cpPT cpFT cpCT env isTL = match kexpr with
  | KInt i -> [LDC (find_from_table cpCT kexpr)]
  | KFloat i -> [LDC (find_from_table cpCT kexpr)]
  | KString s -> [LDC (find_from_table cpCT kexpr)]
  | KBool b -> if b then [ICONST_1] else [ICONST_0]
  | KEVar (v, t) -> (match t with
      | Int | Bool -> [ILOAD (find_from_table env v)]
      | Float -> [FLOAD (find_from_table env v)]
      | String -> [ALOAD (find_from_table env v)])
  | KCall (s, kes, _) ->
    if (List.mem s Primitives.all_prims_symbols)
    then List.fold_left (fun b ke -> b@(compile_expr ke cpPT cpFT cpCT env isTL)) [] kes
         @(CompilePrims.compile_prim s (get_type (List.hd kes)))
    else raise (Failure "Functions not implemented yet")

(** Compile a k-statement *)
and compile_stmt kstmt cpPT cpFT cpCT env isTL = match kstmt with
  | KVoidExpr kexpr -> ((compile_expr kexpr cpPT cpFT cpCT env isTL)@[POP], env)
  | KLet (s, kexpr) -> ((compile_expr kexpr cpPT cpFT cpCT env isTL)@(match get_type kexpr with
      | Int | Bool -> [ISTORE (List.length env + 1)]
      | Float -> [FSTORE (List.length env + 1)]
      | String -> [ASTORE (List.length env + 1)]), (s, List.length env + 1)::env)
  | KReturn kexpr ->
    ((if isTL
  (* if we're in toplevel (not a function), we print the result, else we return *)
      then (compile_print kexpr cpPT cpFT cpCT env isTL)
      else (compile_expr kexpr cpPT cpFT cpCT env isTL)@[POP] (* TODO *)),
     env)

(** Compile a KPrint *)
and compile_print kexpr cpPT cpFT cpCT env isTL =
  (GETSTATIC (find_from_table cpFT "out"))::
  (compile_expr kexpr cpPT cpFT cpCT env isTL)@
  [INVOKEVIRTUAL (find_from_table cpPT (match get_type kexpr with
       | Int -> PrintlnInt
       | Float -> PrintlnFloat
       | String -> PrintlnString
       | Bool -> PrintlnBool
     ))]

and compile_program kast cpPT cpFT cpCT env isTopLevel = (* TODO return *)
  List.fold_left (fun (b, env) kstmt -> let (newB, env) = (compile_stmt kstmt cpPT cpFT cpCT env isTopLevel) in (b@newB, env)) ([], env) kast

(** Generate bytecode from the kast *)
let generate_bytecode kast cpPrimsTable cpFieldsTable cpConstsTable =
  fst (compile_program kast cpPrimsTable cpFieldsTable cpConstsTable [] true)
