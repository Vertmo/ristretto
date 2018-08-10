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
open Kast

type returnCtxt = TopLevel | Block | If

let find_from_table cpTable toFind =
  (snd (List.find (fun (s, _) -> s = toFind) cpTable))

(** Compile a k-expression *)
let rec compile_expr kexpr cpPT cpFT cpCT env rCtxt = match kexpr with
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
    then List.fold_left (fun b ke -> b@(compile_expr ke cpPT cpFT cpCT env rCtxt)) [] kes
         @(CompilePrims.compile_prim s (get_type (List.hd kes)))
    else raise (Failure "Functions not implemented yet")
  | KIf (cond, th, el, t) ->
    let thB = fst (compile_program th cpPT cpFT cpCT env If)
    and elB = fst (compile_program el cpPT cpFT cpCT env If) in
    (compile_expr cond cpPT cpFT cpCT env rCtxt)@
    [IFEQ ((bytecode_length thB) + 6)]@thB@
    [GOTO ((bytecode_length elB) + 3)]@elB

(** Compile a k-statement *)
and compile_stmt kstmt cpPT cpFT cpCT env rCtxt = match kstmt with
  | KVoidExpr kexpr -> ((compile_expr kexpr cpPT cpFT cpCT env rCtxt)@[POP], env)
  | KLet (s, kexpr) -> ((compile_expr kexpr cpPT cpFT cpCT env rCtxt)@(match get_type kexpr with
      | Int | Bool -> [ISTORE (List.length env + 1)]
      | Float -> [FSTORE (List.length env + 1)]
      | String -> [ASTORE (List.length env + 1)]), (s, List.length env + 1)::env)
  | KReturn kexpr ->
    ((match rCtxt with
        (* if we're in toplevel (not a function), we print the result *)
        | TopLevel -> compile_print kexpr cpPT cpFT cpCT env rCtxt
        (* if we're in a if block we keep the result *)
        | If -> (compile_expr kexpr cpPT cpFT cpCT env rCtxt)
        (* if we're in a normal block we simply pop *)
        | Block -> (compile_expr kexpr cpPT cpFT cpCT env rCtxt)@[POP]),
     env)

(** Compile a KPrint *)
and compile_print kexpr cpPT cpFT cpCT env rCtxt =
  (GETSTATIC (find_from_table cpFT "out"))::
  (compile_expr kexpr cpPT cpFT cpCT env rCtxt)@
  [INVOKEVIRTUAL (find_from_table cpPT (match get_type kexpr with
       | Int -> PrintlnInt
       | Float -> PrintlnFloat
       | String -> PrintlnString
       | Bool -> PrintlnBool
     ))]

and compile_program kast cpPT cpFT cpCT env rCtxt =
  List.fold_left (fun (b, env) kstmt -> let (newB, env) = (compile_stmt kstmt cpPT cpFT cpCT env rCtxt) in (b@newB, env)) ([], env) kast

(** Generate bytecode from the kast *)
let generate_bytecode kast cpPrimsTable cpFieldsTable cpConstsTable =
  fst (compile_program kast cpPrimsTable cpFieldsTable cpConstsTable [] TopLevel)
