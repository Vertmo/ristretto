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
  cpMT : (kstmt * (string * int)) list; (** Methods *)
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
      | Long -> [LLOAD (find_from_table vars v - 1)]
      | Float -> [FLOAD (find_from_table vars v)]
      | Double -> [DLOAD (find_from_table vars v - 1)]
      | String -> [ALOAD (find_from_table vars v)]
      | Fun _ | Unit -> invalid_arg "compile_expr") with Not_found -> raise (Failure ("EVar not found : "^v)))

(** Compile a k-expression *)
let rec compile_expr kexpr env rCtxt bcLen = match kexpr with
  | KInt _ | KFloat _ | KString _ -> let index = find_from_table env.cpCT kexpr in
    if index < 255 then [LDC index] else [LDC_W index]
  | KLong _ | KDouble _ -> [LDC2_W (find_from_table env.cpCT kexpr)]
  | KBool b -> if b then [ICONST_1] else [ICONST_0]
  | KUnit -> []
  | KEVar (v, t) -> get_var_from_env v t env.vars
  | KCall (s, kes, t) ->
    if (List.mem s Primitives.all_prims_symbols)
    then
      (* Compile parameters *)
      (List.fold_left (fun b ke -> b@(compile_expr ke env rCtxt (bcLen + (bytecode_length b)))) [] kes)@
      (* Compile primitive *)
      (CompilePrims.compile_prim s (get_type (List.hd kes)))
    else
      let descriptor = descriptor_of_type (Fun ((List.map (fun e -> get_type e) kes), t)) in
      (* Compile parameters *)
      (List.fold_left (fun b ke -> b@(compile_expr ke env rCtxt (bcLen + (bytecode_length b)))) [] kes)@
      (* Call to function *)
      [INVOKESTATIC (try find_from_table env.vars (s^"#"^descriptor)
                     with Not_found -> raise (Failure ("Method not found in vars : "^s^"#"^descriptor)))] (* invoke method *)
  | KIf (cond, th, el, t) ->
    let thB = fst (compile_program th env If bcLen) in
    let elB = fst (compile_program el env If (bcLen + (bytecode_length thB) + 6)) in
    (compile_expr cond env rCtxt bcLen)@
    [IFEQ ((bytecode_length thB) + 6)]@thB@
    [GOTO ((bytecode_length elB) + 3)]@elB

(** Compile a k-statement *)
and compile_stmt kstmt env rCtxt bcLen = match kstmt with
  | KVoidExpr kexpr -> ((compile_expr kexpr env rCtxt bcLen)@
                        (match get_type kexpr with
                         | Unit -> []
                         | _ -> [POP]), env)
  | KLet (s, kexpr) -> (match get_type kexpr with
      | Int | Bool -> ((compile_expr kexpr env rCtxt bcLen)@[ISTORE (List.length env.vars + 1)], add_var_let env s)
      | Long -> ((compile_expr kexpr env rCtxt bcLen)@[LSTORE (List.length env.vars + 1)], add_var_let (add_var_let env s) s)
      | Float -> ((compile_expr kexpr env rCtxt bcLen)@[FSTORE (List.length env.vars + 1)], add_var_let env s)
      | Double -> ((compile_expr kexpr env rCtxt bcLen)@[DSTORE (List.length env.vars + 1)], add_var_let (add_var_let env s) s)
      | String -> ((compile_expr kexpr env rCtxt bcLen)@[ASTORE (List.length env.vars + 1)], add_var_let env s)
      | Fun _ | Unit -> invalid_arg "compile_stmt : unexpected type for let")
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
            | Long -> LRETURN
            | Float -> FRETURN
            | Double -> DRETURN
            | String -> ARETURN
            | Unit -> RETURN
            | Fun _ -> invalid_arg "compile_stmt"]),
     env)
  | KPrint kexpr -> (compile_print kexpr env rCtxt bcLen, env)
  | KFunction (name, _, fv, _, t) ->
    let (ident, index) = (try find_from_table env.cpMT kstmt
                          with Not_found -> raise (Failure ("Method not found in cpMT : "^name))) in
    let bc = List.concat
        (List.map (fun (s, tv) ->
             (get_var_from_env s tv env.vars)@
             [PUTSTATIC (try find_from_table env.cpFT (Printf.sprintf "%s#%s" ident s)
                         with Not_found -> raise (Failure ("Field not found in cpFT : "^(Printf.sprintf "%s#%s" ident s))))]) fv) in
    (bc, add_var env (name^"#"^(descriptor_of_type t)) index) (* adding the function to the env was handled previously *)
  | KForeign (name, _, _, _, t) ->
    let (ident, index) = (try find_from_table env.cpMT kstmt
                          with Not_found -> raise (Failure ("Method not found in cpMT : "^name))) in
    ([], add_var env (name^"#"^(descriptor_of_type t)) index)

(** Compile a KPrint *)
and compile_print kexpr env rCtxt bcLen =
  (GETSTATIC (find_from_table env.cpFT "out"))::
  (compile_expr kexpr env rCtxt bcLen)@
  (match get_type kexpr with
       | Int -> [INVOKEVIRTUAL (find_from_table env.cpPT PrintlnInt)]
       | Long -> [INVOKEVIRTUAL (find_from_table env.cpPT PrintlnLong)]
       | Float -> [INVOKEVIRTUAL (find_from_table env.cpPT PrintlnFloat)]
       | Double -> [INVOKEVIRTUAL (find_from_table env.cpPT PrintlnDouble)]
       | String -> [INVOKEVIRTUAL (find_from_table env.cpPT PrintlnString)]
       | Bool -> [INVOKEVIRTUAL (find_from_table env.cpPT PrintlnBool)]
       | Unit -> (compile_expr (KString "End of program") env rCtxt bcLen)@[INVOKEVIRTUAL (find_from_table env.cpPT PrintlnString)]
       | _ -> invalid_arg "compile_print"
     )

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
