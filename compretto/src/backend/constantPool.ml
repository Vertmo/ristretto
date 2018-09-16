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

open Utils
open JavaPrims
open Kast
open Opcodes

(** Possible tags *)
type tag = Class | Fieldref | Methodref | InterfaceMethodref | String | Integer | Float | Long | Double | NameAndType | Utf8 | MethodHandle | MethodType | InvokeDynamic | Module | Package
         | Ignore (* Added tag to identify entries that shouldn't be printed (after Double or Long entry) *)

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
  | Ignore -> 0

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

(** Add an Utf8 string to the constantPool *)
let add_Utf8 constantPool cpUT string =
  try ignore (find_from_table cpUT string); (constantPool, cpUT) (* String is already in the constant pool *)
  with Not_found -> (* String is not in the constant pool *)
    let constantPool = constantPool@[{
        tag = Utf8; info = (Utils.u2_of_int (String.length string))@(Utils.u1list_of_string string)
      }] in
    (constantPool, (string, List.length constantPool)::cpUT)

(** Get constant_pool from ast *)
let make_constant_pool classname =
  (* Class name *)
  let (constantPool, cpUT) = add_Utf8 [] [] classname in
  (* Class_info *)
  let constantPool = constantPool@[{tag = Class; info = Utils.u2_of_int 1}] in

  (* Super class (Object) name *)
  let (constantPool, cpUT) = add_Utf8 constantPool cpUT "java/lang/Object" in
  (* Super class (Object) info *)
  let constantPool = constantPool@[{tag = Class; info = Utils.u2_of_int 3}] in

  (* Code attribute *)
  let (constantPool, cpUT) = add_Utf8 constantPool cpUT "Code" in
  (constantPool, cpUT)

(** Add necessary java primitives to the constant_pool *)
let add_java_prims constantPool cpUT =
  List.fold_left (fun (cp, cpUT, t) p ->
      (* Class name *)
      let (cp, cpUT) = add_Utf8 cp cpUT (class_name p) in
      (* Name *)
      let (cp, cpUT) = add_Utf8 cp cpUT (name p) in
      (* Descriptor *)
      let (cp, cpUT) = add_Utf8 cp cpUT (descriptor p) in
      (* Class *)
      let cp = cp@[{
          tag = Class;
          info = (u2_of_int (find_from_table cpUT (class_name p)));
        }] in
      (* Name and type *)
      let cp = cp@[{
          tag = NameAndType;
          info = (u2_of_int (find_from_table cpUT (name p)))@(u2_of_int (find_from_table cpUT (descriptor p)));
        }] in
      (* Methodref *)
      let cp = cp@[{
          tag = Methodref;
          info = (u2_of_int (List.length cp - 1))@(u2_of_int (List.length cp));
        }] in
      (cp, cpUT, (p, List.length cp)::t))
    (constantPool, cpUT, []) allJavaPrims

(** Add necessary fields to the constant_pool *)
let add_java_fields constantPool cpUT =
  List.fold_left (fun (cp, cpUT, t) f ->
      (* Class name *)
      let (cp, cpUT) = add_Utf8 cp cpUT (f.class_name) in
      (* Name *)
      let (cp, cpUT) = add_Utf8 cp cpUT (f.name) in
      (* Descriptor *)
      let (cp, cpUT) = add_Utf8 cp cpUT (f.descriptor) in
      (* Class *)
      let cp = cp@[{
          tag = Class;
          info = (u2_of_int (find_from_table cpUT (f.class_name)));
        }] in
      (* Name and type *)
      let cp = cp@[{
          tag = NameAndType;
          info = (u2_of_int (find_from_table cpUT (f.name)))@(u2_of_int (find_from_table cpUT (f.descriptor)));
        }] in
      (* Fieldref *)
      let cp = cp@[{
          tag = Fieldref;
          info = (u2_of_int (List.length cp - 1))@(u2_of_int (List.length cp))
        }] in
      (cp, cpUT, (f.name, List.length cp)::t))
    (constantPool, cpUT, []) [{name="out";class_name="java/lang/System";descriptor="Ljava/io/PrintStream;";}]

let make_int_const i = { tag = Integer; info = u4_of_int i }

let make_long_const l = let info = {
    tag = Long; info = u8_of_int64 l;
  } in [info; { tag = Ignore; info = [] }]

let make_float_const f = { tag = Float; info = u4_of_float f }

let make_double_const d = let info = {
    tag = Double; info = u8_of_float d;
  } in [info; { tag = Ignore; info = [] }]

let make_string_const s constantPool = [
  (* String info *)
  { tag = String; info = u2_of_int (List.length constantPool + 2)};
  (* Utf8 string *)
  {
    tag = Utf8;
    info = (u2_of_int (String.length s))@(u1list_of_string s);
  }
]

(** Add kast constants to constant_pool *)
let add_kast_constants constantPool kast =
  let rec find_kexpr kexpr constantPool cpCT =
    if List.exists (fun (e, _) -> e = kexpr) cpCT then (constantPool, cpCT)
    else match kexpr with
    | KInt i -> (constantPool@[make_int_const i], (kexpr, (List.length constantPool + 1))::cpCT)
    | KLong l -> (constantPool@(make_long_const l), (kexpr, (List.length constantPool + 1))::cpCT)
    | KFloat f -> (constantPool@[make_float_const f], (kexpr, (List.length constantPool + 1))::cpCT)
    | KDouble d -> (constantPool@(make_double_const d), (kexpr, (List.length constantPool + 1))::cpCT)
    | KString s -> (constantPool@(make_string_const s constantPool), (kexpr, (List.length constantPool + 1))::cpCT)
    | KBool _ | KUnit | KEVar _ -> (constantPool, cpCT)
    | KCall (_, kes,_) -> List.fold_left (fun (cp, t) ke -> find_kexpr ke cp t) (constantPool, cpCT) kes
    | KIf (cond, th, el, _) -> let (constantPool, cpCT) = find_kexpr cond constantPool cpCT in
      let (constantPool, cpCT) = find_kprogram th constantPool cpCT in (* constants in then branch *)
      find_kprogram el constantPool cpCT (* constants in else branch *)

  and find_kstmt kstmt constantPool cpCT = match kstmt with
    | KVoidExpr ke -> find_kexpr ke constantPool cpCT
    | KLet (_, ke) -> find_kexpr ke constantPool cpCT
    | KReturn ke -> find_kexpr ke constantPool cpCT
    | KPrint ke -> find_kexpr ke constantPool cpCT
    | KFunction (_, _, _, kast, _) -> find_kprogram kast constantPool cpCT

  and find_kprogram kast constantPool cpCT =
    List.fold_left (fun (cp, t) kstmt -> find_kstmt kstmt cp t) (constantPool, cpCT) kast in

  (* Adding the ================= and End of program strings *)
  let (constantPool, cpCT) = find_kexpr (KString "=============================================") constantPool [] in
  let (constantPool, cpCT) = find_kexpr (KString "End of program") constantPool cpCT in
  (* Adding all the constants of the program *)
  find_kprogram kast constantPool cpCT

(** Print size of the constant pool *)
let print_constant_pool_count file constantPool =
  let len = (List.length constantPool + 1) in
  if len > 65535 then raise (Failure "Constant pool too big !");
  Utils.print_u2 file (Utils.u2_of_int len)

(** Print one info *)
let print_cpinfo file cpInfo =
  if cpInfo.tag <> Ignore then output_byte file (int_of_tag cpInfo.tag); List.iter (fun i -> output_byte file i) cpInfo.info

(** Print the whole constant pool *)
let print_constant_pool file constantPool =
  List.iter (print_cpinfo file) constantPool
