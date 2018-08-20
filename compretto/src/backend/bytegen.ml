(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Generate bytecode *)

open Utils
open ConstantPool
open Methods
open Compiler

(** Print the magic_number for class files : 0xCAFEBABE *)
let print_magic_number file =
  List.iter (fun s -> output_byte file (int_of_string s)) ["0xCA";"0xFE";"0xBA";"0xBE"]

(** Print version_number (u2 minor, u2 major). Here, targeting java 5 or better (that way I don't have to declare stackmap frames) *)
let print_version_number file =
  List.iter (fun i -> output_byte file i) ((u2_of_int 0)@(u2_of_int 49))

(** Get the class name corresponding to the file *)
let get_class_name filepath =
  let filename = List.hd (List.rev (String.split_on_char '/' filepath)) in
  List.hd (String.split_on_char '.' filename)

(** Print access_flags (always public) *)
let print_access_flags file =
  List.iter (fun s -> output_byte file (int_of_string s)) ["0x00";"0x01"]

(** Print index of Class_info in constant_pool (always 1) *)
let print_this_class file =
  print_u2 file (Utils.u2_of_int 1)

(** Print super_class index in constant_pool (always 3) *)
let print_super_class file =
  List.iter (output_byte file) (Utils.u2_of_int 3)

(** Print interfaces_count (always 0) *)
let print_interfaces_count file =
  print_u2 file (Utils.u2_of_int 0)

(** Print fields_count (always 0) *)
let print_fields_count file =
  print_u2 file (Utils.u2_of_int 0)

(** Print fields_count (always 0) *)
let print_attributes_count file =
  print_u2 file (Utils.u2_of_int 0)

(** Generates all bytecode *)
let gen_bytecode filename kast =
  let f = open_out filename in
  print_magic_number f;
  print_version_number f;

  (* Constant pool*)
  let classname = get_class_name filename in
  let constantPool = make_constant_pool classname in
  let (constantPool, cpPrimsTable) = add_java_prims constantPool in
  let (constantPool, cpFieldsTable) = add_java_fields constantPool in
  let (constantPool, cpConstantsTable) = add_kast_constants constantPool kast in
  let (constantPool, init, _) = add_method constantPool "<init>" "()V"
      [ ALOAD_0;
        INVOKESPECIAL (find_from_table cpPrimsTable JavaPrims.ObjectInit);
        RETURN
      ] false in
  let (constantPool, methods, cpMethodsTable) = find_and_add_methods constantPool cpPrimsTable cpFieldsTable cpConstantsTable [] kast in
  let (constantPool, main, _) = add_method constantPool "main" "([Ljava/lang/String;)V"
      (generate_bytecode kast cpPrimsTable cpFieldsTable cpConstantsTable cpMethodsTable) true in
  let methods = init::main::methods in
  print_constant_pool_count f constantPool;
  print_constant_pool f constantPool;

  print_access_flags f;
  print_this_class f;
  print_super_class f;
  print_interfaces_count f;
  print_fields_count f;

  print_methods_count f methods;
  print_methods f methods;

  print_attributes_count f;
  close_out f;
