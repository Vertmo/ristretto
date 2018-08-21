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

open Utils
open Opcodes
open JavaPrims
open Kast
open Types

type returnCtxt = TopLevel | Function of types | If

(** Environment *)
type env = {
  cpPT: (JavaPrims.javaPrim * int) list; (** Java primitives *)
  cpFT: (string * int) list; (** Java fields *)
  cpCT: (kexpr * int) list; (** Constants *)
  cpMT : (kstmt * int) list; (** Methods *)
  vars: (string * int) list; (** Declared variables *)
}

(** Add a variable to the environment *)
let add_var env s index =
  {
    cpPT = env.cpPT; cpFT = env.cpFT; cpCT = env.cpCT; cpMT = env.cpMT;
    vars = ((s, index)::env.vars)
  }

let add_var_let env s = add_var env s (List.length env.vars + 1)

(** Get variable from the environment, and load it according to it's type *)
let get_var_from_env v t vars = (try (match t with
      | Int | Bool -> [ILOAD (find_from_table vars v)]
      | Float -> [FLOAD (find_from_table vars v)]
      | String -> [ALOAD (find_from_table vars v)]
      | _ -> invalid_arg "compile_expr") with Not_found -> raise (Failure ("EVar not found : "^v)))

(** Compile a k-expression *)
let rec compile_expr kexpr env rCtxt bcLen = match kexpr with
  | KInt i -> [LDC (find_from_table env.cpCT kexpr)]
  | KFloat i -> [LDC (find_from_table env.cpCT kexpr)]
  | KString s -> [LDC (find_from_table env.cpCT kexpr)]
  | KBool b -> if b then [ICONST_1] else [ICONST_0]
  | KEVar (v, t) -> get_var_from_env v t env.vars
  | KCall (s, kes, _) ->
    if (List.mem s Primitives.all_prims_symbols)
    then (List.fold_left (fun b ke -> b@(compile_expr ke env rCtxt (bcLen + (bytecode_length b)))) [] kes)@ (* Compile parameters *)
         (CompilePrims.compile_prim s (get_type (List.hd kes)))
    else (List.fold_left (fun b ke -> b@(compile_expr ke env rCtxt (bcLen + (bytecode_length b)))) [] kes)@ (* Compile parameters *)
         [INVOKESTATIC (try find_from_table env.vars s
                         with Not_found -> raise (Failure ("Method not found in vars : "^s)))] (* invoke method *)
  | KIf (cond, th, el, t) ->
    let thB = fst (compile_program th env If bcLen) in
    let elB = fst (compile_program el env If (bcLen + (bytecode_length thB) + 6)) in
    (compile_expr cond env rCtxt bcLen)@
    [IFEQ ((bytecode_length thB) + 6)]@thB@
    [GOTO ((bytecode_length elB) + 3)]@elB

(** Compile a k-statement *)
and compile_stmt kstmt env rCtxt bcLen = match kstmt with
  | KVoidExpr kexpr -> ((compile_expr kexpr env rCtxt bcLen)@[POP], env)
  | KLet (s, kexpr) -> (match get_type kexpr with
      | Int | Bool -> ((compile_expr kexpr env rCtxt bcLen)@[ISTORE (List.length env.vars + 1)], add_var_let env s)
      | Float -> ((compile_expr kexpr env rCtxt bcLen)@[FSTORE (List.length env.vars + 1)], add_var_let env s)
      | String -> ((compile_expr kexpr env rCtxt bcLen)@[ASTORE (List.length env.vars + 1)], add_var_let env s)
      | _ -> invalid_arg "compile_stmt : unexpected type for let")
  | KReturn kexpr ->
    ((match rCtxt with
        (* if we're in toplevel (not a function), we print the result *)
        | TopLevel -> (List.fold_left
                        (fun b ke -> b@(compile_print ke env rCtxt (bcLen + (bytecode_length b)))) []
                        [KString "=============================================";
                         kexpr;
                         KString "============================================="])@[RETURN]
        (* if we're in a if block we keep the result *)
        | If -> (compile_expr kexpr env rCtxt bcLen)
        (* if we're in a function we return the value *)
        | Function t -> (compile_expr kexpr env rCtxt bcLen)@[match t with
            | Int | Bool -> IRETURN
            | Float -> FRETURN
            | String -> ARETURN
            | Fun _ -> invalid_arg "compile_stmt"]),
     env)
  | KPrint kexpr -> (compile_print kexpr env rCtxt bcLen, env)
  | KFunction (ident, _, fv, _, t) ->
    let bc = List.concat
        (List.map (fun (s, tv) ->
             (get_var_from_env s tv env.vars)@
             [PUTSTATIC (find_from_table env.cpFT (Printf.sprintf "%s#%s#%s" ident (descriptor_of_type t) s))]) fv) in
    (bc, add_var env ident (try find_from_table env.cpMT kstmt
                            with Not_found -> raise (Failure ("Method not found in cpMT : "^ident)))) (* adding the function to the env was handled previously *)

(** Compile a KPrint *)
and compile_print kexpr env rCtxt bcLen =
  (GETSTATIC (find_from_table env.cpFT "out"))::
  (compile_expr kexpr env rCtxt bcLen)@
  [INVOKEVIRTUAL (find_from_table env.cpPT (match get_type kexpr with
       | Int -> PrintlnInt
       | Float -> PrintlnFloat
       | String -> PrintlnString
       | Bool -> PrintlnBool
       | _ -> invalid_arg "compile_print"
     ))]

and compile_program kast env rCtxt bcLen =
  List.fold_left (fun (b, env) kstmt -> let (newB, env) = (compile_stmt kstmt env rCtxt (bcLen + (bytecode_length b))) in (b@newB, env)) ([], env) kast

(** Generate bytecode from the kast *)
let generate_bytecode kast cpPrimsTable cpFieldsTable cpConstsTable cpMethodsTable =
  fst (compile_program kast {
      cpPT = cpPrimsTable;
      cpFT = cpFieldsTable;
      cpCT = cpConstsTable;
      cpMT = cpMethodsTable;
      vars = [];
    } TopLevel 0)
