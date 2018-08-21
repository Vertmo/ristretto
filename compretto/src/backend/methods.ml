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

(** Generate fields and methods *)

type attribute_info = {
  attribute_name_index: u2;
  attribute_length: u4;
  info: int list
}

(** Describe a method or a field *)
type info = {
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

(** Add a new field to the constantPool *)
let add_field constantPool name descriptor =
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
  let field = {
    access_flags = List.map int_of_string ["0x00";"0x0A"];
    name_index = u2_of_int nameIndex;
    descriptor_index = u2_of_int descriptorIndex;
    attributes_count = u2_of_int 0;
    attributes = []
  } in
  let constantPool = constantPool@[{
      tag = NameAndType;
      info = (u2_of_int nameIndex)@(u2_of_int descriptorIndex);
    };{
       tag = Fieldref;
       (* class_index name_and_type_index *)
       info = (u2_of_int 1)@(u2_of_int (List.length constantPool + 1));
  }] in
  (constantPool, field, List.length constantPool)

let find_and_add_methods constantPool cpPT cpFT cpCT cpMT kast =
  let rec find_kexpr kexpr constantPool methods fields cpMT cpFT = match kexpr with
    | KInt _ | KFloat _ | KString _ | KBool _ -> (constantPool, methods, fields, cpMT, cpFT)
    | KEVar _ -> (constantPool, methods, fields, cpMT, cpFT)
    | KCall (_, kes, _) -> List.fold_left (fun (constantPool, methods, fields, cpMT, cpFT) ke ->
        find_kexpr ke constantPool methods fields cpMT cpFT) (constantPool, methods, fields, cpMT, cpFT) kes
    | KIf (cond, th, el, _) -> let (constantPool, methods, fields, cpMT, cpFT) = find_kexpr cond constantPool methods fields cpMT cpFT in
      let (constantPool, methods, fields, cpMT, cpFT) = find_kprogram th constantPool methods fields cpMT cpFT in (* methods in then branch *)
      find_kprogram el constantPool methods fields cpMT cpFT (* methods in else branch *)

  and find_kstmt kstmt constantPool methods fields cpMT cpFT = match kstmt with
    | KVoidExpr ke -> find_kexpr ke constantPool methods fields cpMT cpFT
    | KLet (_, ke) -> find_kexpr ke constantPool methods fields cpMT cpFT
    | KReturn ke -> find_kexpr ke constantPool methods fields cpMT cpFT
    | KPrint ke -> find_kexpr ke constantPool methods fields cpMT cpFT
    | KFunction (ident, args, fv, body, t) -> match t with
      | Fun (_, r) ->
        let (constantPool, methods, fields, cpMT, cpFT) = find_kprogram body constantPool methods fields cpMT cpFT in
        let (constantPool, fields, cpFT) = List.fold_left (fun (constantPool, fields, cpFT) (s, fieldT) ->
            let name = Printf.sprintf "%s#%s#%s" ident (descriptor_of_type t) s in
            let (constantPool, f, index) = add_field constantPool name (descriptor_of_type fieldT)
            in (constantPool, (f::fields), ((name, index)::cpFT)))
            (constantPool, fields, cpFT) fv in
        let (constantPool, m, indexM) = add_method constantPool ident (descriptor_of_type t)
            (List.concat (List.mapi (fun i (s, fieldT) -> (* Store free variables *)
                 [GETSTATIC (find_from_table cpFT (Printf.sprintf "%s#%s#%s" ident (descriptor_of_type t) s));
                  (match fieldT with
                   | Int | Bool -> ISTORE (i + List.length args)
                   | Float -> FSTORE (i + List.length args)
                   | String -> ASTORE (i + List.length args)
                   | _ -> raise (Failure "Bad free variable type"))]) fv)@
            (fst (* Compile body of the function *)
               (compile_program body
                  { cpPT = cpPT; cpFT = cpFT; cpCT = cpCT; cpMT = cpMT;
                    (* add_method always add 4 elements to the constantPool*)
                    vars = ((ident, (List.length constantPool + 4))::(List.mapi (fun i a -> (a, i)) args))@
                           (List.mapi (fun i s -> (s, i + List.length args)) (List.map fst fv));
                  } (Function r) 0)))
            true in
        (constantPool, m::methods, fields, (kstmt, indexM)::cpMT, cpFT)
      | _ -> invalid_arg "find_kstmt"

  and find_kprogram kast constantPool methods fields cpMT cpFT =
    List.fold_left (fun (cp, meths, fields, cpMT, cpFT) kstmt ->
        find_kstmt kstmt cp meths fields cpMT cpFT) (constantPool, methods, fields, cpMT, cpFT) kast
  in find_kprogram kast constantPool [] [] cpMT cpFT

(** Print attribute info *)
let print_attribute_info file attributeInfo =
  print_u2 file attributeInfo.attribute_name_index;
  print_u4 file attributeInfo.attribute_length;
  List.iter (output_byte file) attributeInfo.info

(** Print methods_count *)
let print_methods_count file methods =
  print_u2 file (Utils.u2_of_int (List.length methods))

(** Print a method or a field *)
let print_info file info =
  print_u2 file info.access_flags;
  print_u2 file info.name_index;
  print_u2 file info.descriptor_index;
  print_u2 file info.attributes_count;
  List.iter (print_attribute_info file) info.attributes

(** Print all methods *)
let print_methods file methods =
  List.iter (print_info file) methods

(** Print fields_count *)
let print_fields_count file fields =
  print_u2 file (Utils.u2_of_int (List.length fields))

(** Print all fields *)
let print_fields file fields =
  List.iter (print_info file) fields
