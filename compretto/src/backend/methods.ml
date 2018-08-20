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
open Types
open Kast
open Opcodes
open JavaPrims
open ConstantPool
open Compiler

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
  let constantPool = constantPool@[{
      tag = NameAndType;
      info = (u2_of_int nameIndex)@(u2_of_int descriptorIndex);
    };{
       tag = Methodref;
       (* class_index name_and_type_index *)
       info = (u2_of_int 1)@(u2_of_int (List.length constantPool + 1));
  }] in
  (constantPool, meth, List.length constantPool)

let find_and_add_methods constantPool cpPT cpFT cpCT cpMT kast =
  let rec find_kexpr kexpr constantPool methods cpMT = match kexpr with
    | KInt _ | KFloat _ | KString _ | KBool _ -> (constantPool, methods, cpMT)
    | KEVar _ -> (constantPool, methods, cpMT)
    | KCall (_, kes, _) -> List.fold_left (fun (cp, t, cpMT) ke -> find_kexpr ke cp t cpMT) (constantPool, methods, cpMT) kes
    | KIf (cond, th, el, _) -> let (constantPool, meths, cpMT) = find_kexpr cond constantPool methods cpMT in
      let (constantPool, meths, cpMT) = find_kprogram th constantPool meths cpMT in (* constants in then branch *)
      find_kprogram el constantPool meths cpMT (* constants in else branch *)

  and find_kstmt kstmt constantPool methods cpMT = match kstmt with
    | KVoidExpr ke -> find_kexpr ke constantPool methods cpMT
    | KLet (_, ke) -> find_kexpr ke constantPool methods cpMT
    | KReturn ke -> find_kexpr ke constantPool methods cpMT
    | KPrint ke -> find_kexpr ke constantPool methods cpMT
    | KFunction (ident, args, body, t) -> match t with
      | Fun (_, r) ->
        let (constantPool, methods, cpMT) = find_kprogram body constantPool methods cpMT in
        let (constantPool, m, index) = add_method constantPool ident (descriptor_of_type t)
            (fst
               (compile_program body
                  { cpPT = cpPT; cpFT = cpFT; cpCT = cpCT; cpMT = cpMT;
                    (* add_method always add 4 elements to the constantPool*)
                    vars = (ident, (List.length constantPool + 4))::(List.mapi (fun i a -> (a, i)) args);
                  } (Function r) 0))
            true in (constantPool, m::methods, (kstmt, index)::cpMT)
      | _ -> invalid_arg "find_kstmt"

  and find_kprogram kast constantPool methods cpMT =
    List.fold_left (fun (cp, meths, cpMT) kstmt -> find_kstmt kstmt cp meths cpMT) (constantPool, methods, cpMT) kast
  in find_kprogram kast constantPool [] []

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
