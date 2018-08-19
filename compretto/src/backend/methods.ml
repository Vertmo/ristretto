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

type attribute_info = {
  attribute_name_index: u2;
  attribute_length: u4;
  info: int list
}

type method_info = {
  access_flags: u2;
  name_index: u2;
  descriptor_index: u2;
  attributes_count: u2;
  attributes: attribute_info list;
}

(** Max stack and max locals *)
let max_code_attributes =
  (u2_of_int 1024)@ (* max_stack *)
  (u2_of_int 1024) (* max_locals *)

(** Make code attribute from a method, based on bytecode *)
let make_code_attribute bc =
  (* max_stack max_locals code_length code exception_table_length attributes_count *)
  let bcc = List.concat (List.map code bc) in
  let info = max_code_attributes@
             (u4_of_int (List.length bcc))@bcc@
             (u2_of_int 0)@(u2_of_int 0) in
  {
    attribute_name_index = u2_of_int 5; (* Always *)
    attribute_length = u4_of_int (List.length info);
    info = info;
  }

(** Add a new method to the constantPool *)
let add_method constantPool name descriptor code isStatic =
  let constantPool = constantPool@[{ (* Name *)
      tag = Utf8;
      info = (u2_of_int (String.length name))@(u1list_of_string name)
    }] in
  let nameIndex = List.length constantPool in
  let constantPool = constantPool@[{ (* Descriptor *)
      tag = Utf8;
      info = (u2_of_int (String.length descriptor))@(u1list_of_string descriptor)
    }] in
  let descriptorIndex = List.length constantPool in
  let meth = {
    access_flags = List.map int_of_string ["0x00";if isStatic then "0x09" else "0x01"];
    name_index = u2_of_int nameIndex;
    descriptor_index = u2_of_int descriptorIndex;
    attributes_count = u2_of_int 1;
    attributes = [make_code_attribute code]
  } in
  (constantPool, meth)

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
