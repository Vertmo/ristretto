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

type returnCtxt = TopLevel | Function | If

let find_from_table cpTable toFind =
  (snd (List.find (fun (s, _) -> s = toFind) cpTable))

(** Compile a k-expression *)
let rec compile_expr kexpr cpPT cpFT cpCT env rCtxt bcLen = match kexpr with
  | KInt i -> [LDC (find_from_table cpCT kexpr)]
  | KFloat i -> [LDC (find_from_table cpCT kexpr)]
  | KString s -> [LDC (find_from_table cpCT kexpr)]
  | KBool b -> if b then [ICONST_1] else [ICONST_0]
  | KEVar (v, t) -> (match t with
      | Int | Bool -> [ILOAD (find_from_table env v)]
      | Float -> [FLOAD (find_from_table env v)]
      | String -> [ALOAD (find_from_table env v)]
      | _ -> invalid_arg "compile_expr")
  | KCall (s, kes, _) ->
    if (List.mem s Primitives.all_prims_symbols)
    then List.fold_left (fun b ke -> b@(compile_expr ke cpPT cpFT cpCT env rCtxt (bcLen + (bytecode_length b)))) [] kes (* Compile parameters *)
         @(CompilePrims.compile_prim s (get_type (List.hd kes)))
    else (
      let index = find_from_table env s in
      let paramsB = (List.fold_left (fun b ke -> b@(compile_expr ke cpPT cpFT cpCT env rCtxt (bcLen + (bytecode_length b)))) [] kes) in (* Compile parameters *)
      paramsB@[GOTO (index-bcLen-(bytecode_length paramsB)-3)] (* Jump TODO gerer les stack_map et stocker la returnAdress *)
    )
  | KIf (cond, th, el, t) ->
    let thB = fst (compile_program th cpPT cpFT cpCT env If bcLen) in
    let elB = fst (compile_program el cpPT cpFT cpCT env If (bcLen + (bytecode_length thB) + 6)) in
    (compile_expr cond cpPT cpFT cpCT env rCtxt bcLen)@
    [IFEQ ((bytecode_length thB) + 6)]@thB@
    [GOTO ((bytecode_length elB) + 3)]@elB
  | KClosure _ -> [] (* TODO *)

(** Compile a k-statement *)
and compile_stmt kstmt cpPT cpFT cpCT env rCtxt bcLen = match kstmt with
  | KVoidExpr kexpr -> ((compile_expr kexpr cpPT cpFT cpCT env rCtxt bcLen)@[POP], env)
  | KLet (s, kexpr) -> (match get_type kexpr with
      | Int | Bool -> ((compile_expr kexpr cpPT cpFT cpCT env rCtxt bcLen)@[ISTORE (List.length env + 1)], (s, List.length env + 1)::env)
      | Float -> ((compile_expr kexpr cpPT cpFT cpCT env rCtxt bcLen)@[FSTORE (List.length env + 1)], (s, List.length env + 1)::env)
      | String -> ((compile_expr kexpr cpPT cpFT cpCT env rCtxt bcLen)@[ASTORE (List.length env + 1)], (s, List.length env + 1)::env)
      | Fun _ -> ((compile_expr kexpr cpPT cpFT cpCT env rCtxt bcLen), (s, bcLen)::env))
  | KReturn kexpr ->
    ((match rCtxt with
        (* if we're in toplevel (not a function), we print the result *)
        | TopLevel -> List.fold_left
                        (fun b ke -> b@(compile_print ke cpPT cpFT cpCT env rCtxt (bcLen + (bytecode_length b)))) []
                        [KString "=============================================";
                         kexpr;
                         KString "============================================="]
        (* if we're in a if block we keep the result *)
        | If -> (compile_expr kexpr cpPT cpFT cpCT env rCtxt bcLen)
        (* if we're in a function we pop and ret TODO *)
        | Function -> (compile_expr kexpr cpPT cpFT cpCT env rCtxt bcLen)@[POP]),
     env)
  | KPrint kexpr -> (compile_print kexpr cpPT cpFT cpCT env rCtxt bcLen, env)

(** Compile a KPrint *)
and compile_print kexpr cpPT cpFT cpCT env rCtxt bcLen =
  (GETSTATIC (find_from_table cpFT "out"))::
  (compile_expr kexpr cpPT cpFT cpCT env rCtxt bcLen)@
  [INVOKEVIRTUAL (find_from_table cpPT (match get_type kexpr with
       | Int -> PrintlnInt
       | Float -> PrintlnFloat
       | String -> PrintlnString
       | Bool -> PrintlnBool
       | _ -> invalid_arg "compile_print"
     ))]

and compile_program kast cpPT cpFT cpCT env rCtxt bcLen =
  List.fold_left (fun (b, env) kstmt -> let (newB, env) = (compile_stmt kstmt cpPT cpFT cpCT env rCtxt (bcLen + (bytecode_length b))) in (b@newB, env)) ([], env) kast

(** Generate bytecode from the kast *)
let generate_bytecode kast cpPrimsTable cpFieldsTable cpConstsTable =
  fst (compile_program kast cpPrimsTable cpFieldsTable cpConstsTable [] TopLevel 0)
