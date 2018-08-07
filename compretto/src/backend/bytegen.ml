(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

open ConstantPool

(** Generate bytecode *)

(** Print the magic_number for class files : 0xCAFEBABE *)
let print_magic_number file =
  List.iter (fun s -> output_byte file (int_of_string s)) ["0xCA";"0xFE";"0xBA";"0xBE"]

(** Print version_number (u2 minor, u2 major. Here, targeting with java 8 or better) *)
let print_version_number file =
  List.iter (fun i -> output_byte file i) [0;0;0;52]

(** Get the class name corresponding to the file *)
let get_class_name filepath =
  let filename = List.hd (List.rev (String.split_on_char '/' filepath)) in
  List.hd (String.split_on_char '.' filename)

(** Print access_flags (always public) *)
let print_access_flags file =
  List.iter (fun s -> output_byte file (int_of_string s)) ["0x00";"0x01"]

(** Print index of Class_info in constant_pool (always 1) *)
let print_this_class file =
  List.iter (output_byte file) (Utils.u2_of_int 1)

(** Print super_class index in constant_pool (always 3) *)
let print_super_class file =
  List.iter (output_byte file) (Utils.u2_of_int 3)

(** Print interfaces_count (always 0) *)
let print_interfaces_count file =
  List.iter (output_byte file) (Utils.u2_of_int 0)

(** Print fields_count (always 0) *)
let print_fields_count file =
  List.iter (output_byte file) (Utils.u2_of_int 0)

(** Print methods_count (always 1 : main) *)
let print_methods_count file =
  List.iter (output_byte file) (Utils.u2_of_int 0)

(** Print fields_count (always 0) *)
let print_attributes_count file =
  List.iter (output_byte file) (Utils.u2_of_int 0)

(** Generates all bytecode *)
let gen_bytecode filename kast =
  let f = open_out filename in
  print_magic_number f;
  print_version_number f;

  (* Constant pool*)
  let classname = get_class_name filename in
  let constantPool = get_constant_pool kast classname in
  print_constant_pool_count f constantPool;
  print_constant_pool f constantPool;

  print_access_flags f;
  print_this_class f;
  print_super_class f;
  print_interfaces_count f;
  print_fields_count f;

  print_methods_count f;
  (* TODO print_methods *)

  print_attributes_count f;
  close_out f;
