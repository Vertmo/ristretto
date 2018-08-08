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
open Opcodes
open JavaPrims
open ConstantPool

(** Methods info *)

type attribute_info = {
  attribute_name_index: u2;
  attribute_length: u4;
  info: int list
}

(** Max stack and max locals *)
let max_code_attributes =
  (u2_of_int 256)@ (* max_stack *)
  (u2_of_int 256) (* max_locals *)

(** exception_table_length (0) *)
let exception_table_length = (u2_of_int 0)

let attributes_count = (u2_of_int 0)

(** Make code for the init method *)
let make_init_code cpPrimsTable =
  let code = (code ALOAD_0)@
             (code (INVOKESPECIAL (snd (List.find (fun (p, _) -> (p = ObjectInit)) cpPrimsTable))))@
             (code RETURN) in
  (u4_of_int (List.length code))@code

(** Class initialization *)
let make_init_code_attribute cpPrimsTable =
  let info = max_code_attributes@(make_init_code cpPrimsTable)@(exception_table_length)@(attributes_count) in
  {
    attribute_name_index = u2_of_int 5;
    attribute_length = u4_of_int (List.length info);
    info = info;
  }

(** Make code for the main method *)
let make_main_code kast cpPrimsTable =
  let code = List.concat (List.map code (Compiler.generate_bytecode kast cpPrimsTable))@(code RETURN) in
  (u4_of_int (List.length code))@code

(** Main method *)
let make_main_code_attribute kast cpPrimsTable =
  let info = max_code_attributes@(make_main_code kast cpPrimsTable)@(exception_table_length)@(attributes_count) in
  {
    attribute_name_index = u2_of_int 5; (* Code string is in position 5*)
    attribute_length = u4_of_int (List.length info);
    info = info;
  }

type method_info = {
  access_flags: u2;
  name_index: u2;
  descriptor_index: u2;
  attributes_count: u2;
  attributes: attribute_info list;
}

(** Make the methods (only one : main) *)
let make_methods kast constantPool cpPrimsTable =
  (* init *)
  let initName = "<init>" in
  let constantPool = constantPool@[{
      tag = Utf8;
      info = (u2_of_int (String.length initName))@(u1list_of_string initName)
    }] in
  let name_index = List.length constantPool in
  let descriptor = "()V" in
  let constantPool = constantPool@[{
      tag = Utf8;
      info = (u2_of_int (String.length descriptor))@(u1list_of_string descriptor)
    }] in
  let descriptor_index = List.length constantPool in
  let init = {
    access_flags = List.map int_of_string ["0x00";"0x01"];
    name_index = u2_of_int name_index;
    descriptor_index = u2_of_int descriptor_index;
    attributes_count = u2_of_int 1;
    attributes = [make_init_code_attribute cpPrimsTable];
  } in

  (* main *)
  let mainName = "main" in
  let constantPool = constantPool@[{
      tag = Utf8;
      info = (u2_of_int (String.length mainName))@(u1list_of_string mainName)
    }] in
  let name_index = List.length constantPool in
  let descriptor = "([Ljava/lang/String;)V" in
  let constantPool = constantPool@[{
      tag = Utf8;
      info = (u2_of_int (String.length descriptor))@(u1list_of_string descriptor)
    }] in
  let descriptor_index = List.length constantPool in
  let main = {
    access_flags = List.map int_of_string ["0x00";"0x09"];
    name_index = u2_of_int name_index;
    descriptor_index = u2_of_int descriptor_index;
    attributes_count = u2_of_int 1;
    attributes = [make_main_code_attribute kast cpPrimsTable];
  } in
  (constantPool, [init; main])

(** Print methods_count (always 1 : main) *)
let print_methods_count file methods =
  print_u2 file (Utils.u2_of_int (List.length methods))

(** Print attribute info *)
let print_attribute_info file attributeInfo =
  print_u2 file attributeInfo.attribute_name_index;
  print_u4 file attributeInfo.attribute_length;
  List.iter (output_byte file) attributeInfo.info

(** Print a method *)
let print_method_info file methodInfo =
  print_u2 file methodInfo.access_flags;
  print_u2 file methodInfo.name_index;
  print_u2 file methodInfo.descriptor_index;
  print_u2 file methodInfo.attributes_count;
  List.iter (print_attribute_info file) methodInfo.attributes

(** Print all methods *)
let print_methods file methods =
  List.iter (print_method_info file) methods


