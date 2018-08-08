(******************************************************************************)
(*                                                                            *)
(*                Compiler for the Ristretto programming language             *)
(*                                                                            *)
(*                                Basile Pesin                                *)
(*                                                                            *)
(* Copyright 2018 Basile Pesin. This file is licensed under the terms of the  *)
(*                    GNU General Public License v3.0                         *)
(******************************************************************************)

type javaPrim = ObjectInit

(* Name of the primitive *)
let name prim = match prim with
  | ObjectInit -> "<init>"

let class_name prim = match prim with
  | ObjectInit -> "java/lang/Object"

let descriptor prim = match prim with
  | ObjectInit -> "()V"
