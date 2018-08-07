(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

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

(** Get constant_pool from ast *)
let get_constant_pool ast classname =
  (* Class_info *)
  let constantPool = [
    {tag = Class; info = Utils.u2_of_int 2};
    (* Class name *)
    {tag = Utf8;
     info = (Utils.u2_of_int (String.length classname))@(Utils.u1list_of_string classname)}] in

  (* Super class (Object) *)
  let objectString = "java/lang/Object" in
  let constantPool = constantPool@
                     [
                       {tag = Class; info = Utils.u2_of_int 4};
                       (* Class name *)
                       {tag = Utf8;
                       info = (Utils.u2_of_int (String.length objectString))@(Utils.u1list_of_string objectString)}] in
  (* TODO constant in the KAST *)
  constantPool

(** Print size of the constant pool *)
let print_constant_pool_count file constantPool =
  List.iter (fun i -> output_byte file i) (Utils.u2_of_int (List.length constantPool + 1))

(** Print one info *)
let print_cpinfo file cpinfo =
  output_byte file (int_of_tag cpinfo.tag); List.iter (fun i -> output_byte file i) cpinfo.info

(** Print the whole constant pool *)
let print_constant_pool file constantPool =
  List.iter (print_cpinfo file) constantPool
