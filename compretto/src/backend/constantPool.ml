(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open Utils
open JavaPrims
open KExpression
open KStatement

(** Generate the constant pool informations *)

(** Possible tags *)
type tag = Class | Fieldref | Methodref | InterfaceMethodref | String | Integer | Float | Long | Double | NameAndType | Utf8 | MethodHandle | MethodType | InvokeDynamic | Module | Package

let int_of_tag t = match t with
  | Class -> 7
  | Fieldref -> 9
  | Methodref -> 10
  | InterfaceMethodref -> 11
  | String -> 8
  | Integer -> 3
  | Float -> 4
  | Long -> 5
  | Double -> 6
  | NameAndType -> 12
  | Utf8 -> 1
  | MethodHandle -> 15
  | MethodType -> 16
  | InvokeDynamic -> 18
  | Module -> 19
  | Package -> 20

(** Information about an entry of the constant pool *)
type cpinfo = {
  tag: tag;
  info: int list;
}

(** Information about a field *)
type field = {
  name: string;
  class_name: string;
  descriptor: string;
}

(** Get constant_pool from ast *)
let make_constant_pool classname =
  (* Class_info *)
  let constantPool = [
    {tag = Class; info = Utils.u2_of_int 2};
    (* Class name *)
    {tag = Utf8;
     info = (Utils.u2_of_int (String.length classname))@(Utils.u1list_of_string classname)}] in

  (* Super class (Object) *)
  let objectString = "java/lang/Object" in
  let constantPool = constantPool@[
      {tag = Class; info = Utils.u2_of_int 4};
      (* Class name *)
      {tag = Utf8;
       info = (Utils.u2_of_int (String.length objectString))@(Utils.u1list_of_string objectString)}] in

  (* Code attribute *)
  let codeString = "Code" in
  let constantPool = constantPool@[{
      tag = Utf8;
      info = (Utils.u2_of_int (String.length codeString))@(Utils.u1list_of_string codeString)
    }] in
  constantPool

(** Add necessary java primitives to the constant_pool *)
let add_java_prims constantPool =
  List.fold_left (fun (cp, t) p ->
      let newCp = cp@[
          (* Class *)
          {
            tag = Class;
            info = (u2_of_int (List.length cp + 2));
          };
          (* Class name *)
          {
            tag = Utf8;
            info = (u2_of_int (String.length (class_name p)))@(u1list_of_string (class_name p));
          };
          (* Name and type *)
          {
            tag = NameAndType;
            info = (u2_of_int (List.length cp + 4))@(u2_of_int (List.length cp + 5));
          };
          (* Name *)
          {
            tag = Utf8;
            info = (u2_of_int (String.length (name p)))@(u1list_of_string (name p));
          };
          (* Descriptor *)
          {
            tag = Utf8;
            info = (u2_of_int (String.length (descriptor p)))@(u1list_of_string (descriptor p));
          };
          (* Methodref *)
          {
            tag = Methodref;
            info = (u2_of_int (List.length cp + 1))@(u2_of_int (List.length cp + 3));
          }
        ] in
      (newCp, (p, List.length newCp)::t))
    (constantPool, []) allJavaPrims

(** Add necessary fields to the constant_pool *)
let add_java_fields constantPool =
  List.fold_left (fun (cp, t) f ->
      let newCp = cp@[
          (* Class *)
          {
            tag = Class;
            info = (u2_of_int (List.length cp + 2));
          };
          (* Class name *)
          {
            tag = Utf8;
            info = (u2_of_int (String.length f.class_name))@(u1list_of_string (f.class_name));
          };
          (* Name and type *)
          {
            tag = NameAndType;
            info = (u2_of_int (List.length cp + 4))@(u2_of_int (List.length cp + 5));
          };
          (* Name *)
          {
            tag = Utf8;
            info = (u2_of_int (String.length f.name))@(u1list_of_string f.name);
          };
          (* Descriptor *)
          {
            tag = Utf8;
            info = (u2_of_int (String.length f.descriptor))@(u1list_of_string f.descriptor);
          };
          (* Fieldref *)
          {
            tag = Fieldref;
            info = (u2_of_int (List.length cp + 1))@(u2_of_int (List.length cp + 3))
          }
        ] in
      (newCp, (f.name, List.length newCp)::t))
    (constantPool, []) [{name="out";class_name="java/lang/System";descriptor="Ljava/io/PrintStream;";}]

let make_int_const i = { tag = Integer; info = u4_of_int i }

let make_float_const f = { tag = Float; info = u4_of_float f }

(** Add kast constants to constant_pool *)
let add_kast_constants constantPool kast =
  let rec find_consts_expr constantPool kexpr = match kexpr with
    | KInt i -> (constantPool@[make_int_const i], [(kexpr, (List.length constantPool + 1))])
    | KFloat f -> (constantPool@[make_float_const f], [(kexpr, (List.length constantPool + 1))])
    | _ -> (constantPool, [])

  and find_consts_stmt constantPool kstmt = match kstmt with
    | KVoidExpr ke -> find_consts_expr constantPool ke
    | KLet (_, ke) -> find_consts_expr constantPool ke

  and find_consts_program constantPool kast =
    List.fold_left (fun (cp, t) kstmt ->
        let (newCp, newT) = find_consts_stmt cp kstmt in
        (newCp, t@newT)) (constantPool, []) kast in
  find_consts_program constantPool kast

(** Print size of the constant pool *)
let print_constant_pool_count file constantPool =
  Utils.print_u2 file (Utils.u2_of_int (List.length constantPool + 1))

(** Print one info *)
let print_cpinfo file cpInfo =
  output_byte file (int_of_tag cpInfo.tag); List.iter (fun i -> output_byte file i) cpInfo.info

(** Print the whole constant pool *)
let print_constant_pool file constantPool =
  List.iter (print_cpinfo file) constantPool
