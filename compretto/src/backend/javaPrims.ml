(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

(** Java primitives used by the language *)

type javaPrim = ObjectInit | PrintlnInt | PrintlnFloat | PrintlnString | PrintlnBool

let allJavaPrims = [ObjectInit;PrintlnInt;PrintlnFloat;PrintlnString;PrintlnBool]

(** Name of the primitive *)
let name prim = match prim with
  | ObjectInit -> "<init>"
  | PrintlnInt | PrintlnFloat | PrintlnString | PrintlnBool -> "println"

let class_name prim = match prim with
  | ObjectInit -> "java/lang/Object"
  | PrintlnInt | PrintlnFloat | PrintlnString | PrintlnBool -> "java/io/PrintStream"

let descriptor prim = match prim with
  | ObjectInit -> "()V"
  | PrintlnInt -> "(I)V" | PrintlnFloat -> "(F)V" | PrintlnString -> "(Ljava/lang/String;)V" | PrintlnBool -> "(Z)V"
